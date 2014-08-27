{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- base
import           Control.Concurrent   (Chan, newChan, readChan,
                                       threadDelay, writeChan)
import           System.Environment   (getArgs, getEnvironment,
                                       getExecutablePath)
import           System.Exit          (exitSuccess)
-- unix
import           System.Posix.Process (ProcessStatus, executeFile,
                                       forkProcess,
                                       getAnyProcessStatus,
                                       getProcessID)
import           System.Posix.Signals (Handler (Catch), Signal,
                                       installHandler, sigCHLD,
                                       sigHUP, sigTERM, signalProcess,
                                       softwareTermination)
import           System.Posix.Types   (ProcessID)
-- safe
import           Safe                 (headNote, readNote)
-- data-default
import           Data.Default         (Default (..))

data Args = Args { aDelay        :: Int
                 , aRunOnce      :: Bool
                 , aChildPid     :: ProcessID
                 , aCommand      :: FilePath
                 , aCommandArgs  :: [String]
                 , aTermReceived :: Bool
                 } deriving (Show)

instance Default Args where def = Args 0 True 0 "" [] False

main :: IO ()
main = do
    getArgs >>= (\a -> putStrLn ("program started with " ++ show a))
    getProcessID >>= (\a -> putStrLn ("process id: " ++ show a))
    args <- getArgs >>= parse def
    putStrLn ("program started with " ++ show args)
    pid <- runOnce args
    putStrLn "after 1st runOnce"
    let pargs = args{aChildPid = pid}
    c <- newChan
    installSignalHandlers c
    putStrLn "before loop"
    loop c pargs

loop :: Chan Signal -> Args -> IO ()
loop c a =
    readChan c >>= processSignal a >>= respawn >>= loop c

processSignal :: Args -> Signal -> IO Args
processSignal a s
   | s == sigHUP  = restart a -- when hupped
   | s == sigTERM = term a -- when termed
   | s == sigCHLD = reap a -- when child
   | otherwise = error "processSignal: unknown signal"

respawn :: Args -> IO Args
respawn a | aChildPid a > 0 = return a
          | aChildPid a == 0 && aRunOnce a =
              exitSuccess >> return a
          | aChildPid a == 0 && aTermReceived a =
              exitSuccess >> return a
          | aChildPid a == 0 = threadDelay (aDelay a * 1000000)
                              >> runOnce a
                              >>= (\p -> return a{aChildPid = p})
          | otherwise = error "respawn: negative child pid"

reap :: Args -> IO Args
reap a = do
    p <- getAnyProcessStatus True True
    putStrLn $ "getAnyProcessStatus returned: " ++ show p
    let args = a{aChildPid = removePid (aChildPid a) p}
    return args

term :: Args -> IO Args
term a | aChildPid a > 0 =
           (sendTerm . aChildPid $ a) >> return a{aTermReceived = True}
       | otherwise = return a{aTermReceived = True}

restart :: Args -> IO Args
restart a = do
    print a
    f <- getExecutablePath
    putStrLn $ "restart: file is  " ++ f
    putStrLn $ "restart: args are " ++ show args
    run f args
    return a
    where args = filter (not . (==) "")
                   [ "-d", show $ aDelay a
                   , runOnceArg $ aRunOnce a
                   , "-p", show $ aChildPid a
                   , "--", aCommand a
                   ] ++ aCommandArgs a
          runOnceArg :: Bool -> String
          runOnceArg True  = "-o"
          runOnceArg False = ""

runOnce :: Args -> IO ProcessID
runOnce a@(Args{aChildPid = 0}) =
 forkProcess $ run (aCommand a) (aCommandArgs a)
runOnce a = return . aChildPid $ a

run :: FilePath -> [String] -> IO ()
run c args = getEnvironment >>= executeFile c False args . Just

installSignalHandlers :: Chan Signal -> IO ()
installSignalHandlers c =
    mapM_ (installSignalHandler c)  [ sigHUP , sigTERM , sigCHLD ]

installSignalHandler :: Chan Signal -> Signal -> IO Handler
installSignalHandler c s =
 installHandler s (Catch $ writeChan c s)  Nothing

removePid :: ProcessID -> Maybe (ProcessID, ProcessStatus) -> ProcessID
removePid 0 _ = 0
removePid p Nothing = p
removePid p (Just (pid,_)) = if pid == p then 0 else p

sendTerm :: ProcessID -> IO ()
sendTerm = sendSignal softwareTermination

-- sendKill :: ProcessID -> IO ()
-- sendKill = sendSignal killProcess

sendSignal :: Signal -> ProcessID -> IO ()
-- sendSignal sig ps = mapM_ (signalProcess sig) ps
sendSignal = signalProcess

parse :: Args -> [String] -> IO Args
parse _ []             = usage >> exitSuccess
parse _ ["-h"]        = usage   >> exitSuccess
parse _ ["--help"]    = usage   >> exitSuccess
parse _ ["-v"]        = version >> exitSuccess
parse _ ["--version"] = version >> exitSuccess
parse a (x:xs)     =
 case x of
    "-d" ->
        parse a{ aDelay =
                     readNote "parse -d" . headNote "parse: -d " $ xs
               , aRunOnce = False
               }
              (tail xs)
    "-o" -> parse a{aRunOnce = True} xs
    "-p" ->
        parse
          a{aChildPid =
                readNote "parse -p" . headNote "parse: -p " $ xs}
          (tail xs)
    "--" -> return $
         a{ aCommand = headNote "parse: --" xs
          , aCommandArgs = tail xs
          }
    "" -> parse a xs
    _ -> usage >> exitSuccess

usage, version :: IO ()
usage   =
 putStrLn
  "Usage: svc [-vh] -d <seconds> -o -- command [command arguments]"
version = putStrLn "svc 0.1"

-- die :: IO a
-- die     = exitWith (ExitFailure 1)
-- uninstallSignalHandlers :: IO ()
-- uninstallSignalHandlers =
--     installHandler sigCHLD Default Nothing
--     >> installHandler sigUSR1 Default Nothing
--     >> installHandler sigUSR2 Default Nothing
--     >> return ()
-- https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Signals
-- signalWait :: MVar Hup -> MVar Term -> MVar Child -> IO ()
-- signalWait h t c = do
--     putStrLn "signalWait"
--     -- https://ghc.haskell.org/trac/ghc/ticket/4504
--     blockSignals reservedSignals
--     awaitSignal Nothing >> yield
-- --     putStrLn "after signalWait"
-- --     threadDelay (1 * 1000000)
-- --     yield
--     putStrLn "end signalWait"
