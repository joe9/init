{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

-- import           Control.Concurrent.MVar
-- import           Control.Exception.Extensible (SomeException (..),
--                                                bracket, finally,
--                                                fromException, throw,
--                                                try)
-- import qualified Control.Exception.Extensible as E
-- import           Control.Monad
-- import           Data.Function
-- import           Data.Maybe           (isJust)
-- import           Debug.Trace
-- import           System.Directory
-- import           System.FilePath
-- import           System.Info
-- import           System.IO
-- import           System.Posix.IO
-- import           System.Posix.Unistd
-- import           System.Process
import           Control.Concurrent
import           Data.Default
import           Safe
import           System.Environment
import           System.Exit
import           System.Posix.Process (executeFile, forkProcess,
                                       getAnyProcessStatus)
import           System.Posix.Process (ProcessStatus)
import           System.Posix.Signals
import           System.Posix.Types   (ProcessID)
import           Text.Groom

data Args = Args { aDelay        :: Int
                 , aRunOnce      :: Bool
                 , aChildPid     :: ProcessID
                 , aCommand      :: FilePath
                 , aCommandArgs  :: [String]
                 , aTermReceived :: Bool
                 } deriving (Show)

instance Default Args where def = Args 10 True 0 "" [] False

type Hup   = Bool
type Term  = Bool
type Child = Bool

main :: IO ()
main = do
    args <- (getArgs >>= parse def)
    pid <- runOnce args
    let pargs = args{aChildPid = pid}
    h <- newMVar False
    t <- newMVar False
    c <- newMVar False
    installSignalHandlers h t c
    loop h t c pargs

loop :: MVar Hup -> MVar Term -> MVar Child -> Args -> IO ()
loop h t c a =
    apply restart h a -- when hupped
    >>= apply term t  -- when termed
    >>= apply reap c  -- when child
    >>= respawn -- other checks
    >>= (\g -> signalWait >> loop h t c g)

respawn :: Args -> IO Args
respawn a | aChildPid a > 0 = return a
          | aChildPid a == 0 && aRunOnce a == True    =
              exitSuccess >> return a
          | aChildPid a == 0 && aTermReceived a == True =
              exitSuccess >> return a
          | aChildPid a == 0 = threadDelay ((aDelay a) * 1000000)
                              >> runOnce a
                              >>= (\p -> return a{aChildPid = p})
          | otherwise = error "respawn: negative child pid"

apply :: (Args -> IO Args) -> MVar Bool -> Args -> IO Args
apply f m a = swapMVar m False >>= ifTrue f a

ifTrue :: (Args -> IO Args) -> Args -> Bool -> IO Args
ifTrue f a True = f a
ifTrue _ a _    = return a

reap :: Args -> IO Args
reap a = do
    p <- getAnyProcessStatus True True
    putStrLn $ "getAnyProcessStatus returned: " ++ (groom p)
    let args = a{aChildPid = removePid (aChildPid a) p}
    return args

term :: Args -> IO Args
term a | aChildPid a > 0 =
           (sendTerm . aChildPid $ a) >> return a{aTermReceived = True}
       | otherwise = return a{aTermReceived = True}

restart :: Args -> IO Args
restart a = do
    putStrLn . show $ a
    env <- getEnvironment
    f <- getExecutablePath
    _ <- executeFile f False args . Just $ env
    return a
    where args =  [ "-d", show $ aDelay a
                   , runOnceArg $ aRunOnce a
                   , "-p", show $ aChildPid a
                   , "--", show $ aCommand a
                   ] ++ aCommandArgs a
          runOnceArg :: Bool -> String
          runOnceArg True  = "-o"
          runOnceArg False = ""

-- https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Signals
signalWait :: IO ()
signalWait = do
    putStrLn "signalWait"
    -- https://ghc.haskell.org/trac/ghc/ticket/4504
    blockSignals reservedSignals
    awaitSignal Nothing >> yield
    putStrLn "after signalWait"
    threadDelay (1 * 1000000)
    yield
    putStrLn "end signalWait"

runOnce :: Args -> IO ProcessID
runOnce a@(Args{aChildPid = 0}) =
 forkProcess $ run (aCommand a) (aCommandArgs a)
runOnce a = return . aChildPid $ a

run :: FilePath -> [String] -> IO ()
run c args = getEnvironment >>= executeFile c False args . Just

installSignalHandlers :: MVar Hup -> MVar Term -> MVar Child -> IO ()
installSignalHandlers h t c =
    mapM_ installSignalHandler  [ (sigHUP, h)
                                , (sigTERM, t)
                                , (sigCHLD, c)
                                ]

installSignalHandler :: (Signal, MVar Bool) -> IO Handler
installSignalHandler (s,m) =
 installHandler s (Catch $ toTrue m)  Nothing

toTrue :: MVar Bool -> IO ()
toTrue m = takeMVar m >> putMVar m True

removePid :: ProcessID -> Maybe (ProcessID, ProcessStatus) -> ProcessID
removePid 0 _ = 0
removePid p Nothing = p
removePid p (Just (pid,_)) = case (pid == p) of
                               True  -> 0
                               False -> p

sendTerm :: ProcessID -> IO ()
sendTerm = sendSignal softwareTermination

-- sendKill :: ProcessID -> IO ()
-- sendKill = sendSignal killProcess

sendSignal :: Signal -> ProcessID -> IO ()
-- sendSignal sig ps = mapM_ (signalProcess sig) ps
sendSignal sig = signalProcess sig

parse :: Args -> [String] -> IO Args
parse _ []             = usage >> exitSuccess
parse _ ["-h"]        = usage   >> exitSuccess
parse _ ["--help"]    = usage   >> exitSuccess
parse _ ["-v"]        = version >> exitSuccess
parse _ ["--version"] = version >> exitSuccess
parse a (x:xs)     =
 case x of
    "-d" ->
        parse a{aDelay = readNote "parse -d" . headNote "parse: -d " $ xs}
              (tail xs)
    "-o" -> parse a{aRunOnce = True} xs
    "-p" ->
        parse a{aDelay = readNote "parse -p" . headNote "parse: -p " $ xs}
              (tail xs)
    "--" -> return $
         a{ aCommand = headNote "parse: --" xs
          , aCommandArgs = tail xs
          }
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
