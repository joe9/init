{-# OPTIONS_GHC -Wall #-}

-- import           Control.Applicative
import           Control.Concurrent
-- import           Control.Concurrent.MVar
-- import           Control.Exception.Extensible (SomeException (..),
--                                                bracket, finally,
--                                                fromException, throw,
--                                                try)
-- import qualified Control.Exception.Extensible as E
-- import           Control.Monad
import           Data.Default
-- import           Data.Function
import           Data.Maybe           (isJust)
-- import           Debug.Trace
import           Safe
-- import           System.Directory
import           System.Environment
import           System.Exit
-- import           System.FilePath
-- import           System.Info
-- import           System.IO
-- import           System.Posix.IO
import           System.Posix.Process (executeFile, forkProcess,
                                       getAnyProcessStatus)
import           System.Posix.Process (ProcessStatus)
import           System.Posix.Signals
import           System.Posix.Types   (ProcessID)
-- import           System.Posix.Unistd
import           System.Process
import           Text.Groom

-- fork and wait for the child

data State = State { sReboot :: Bool
                   , sPids   :: [ProcessID]
                   } deriving (Show)

instance Default State where def = State False []

main :: IO ()
main = do
 args <- getArgs
 pids <- runOnce args
 m <- newMVar (State False pids)
 installSignalHandlers m
 putStrLn . show $ pids
 waitLoop m

runOnce :: [String] -> IO [ProcessID]
runOnce [] =
 sequence [ forkProcess $ run "/home/j/dev/scripts/init/hello" []
          ]
runOnce xs =
 return . map (\(Just x) -> x) . filter isJust . map toProcessID $ xs

run :: FilePath -> [String] -> IO ()
run c args = getEnvironment >>= executeFile c False args . Just

toProcessID :: String -> Maybe ProcessID
toProcessID ("0") = Nothing
toProcessID x = readMay x

waitLoop :: MVar State -> IO ()
waitLoop m = do
 s <- readMVar m
 waitPidsLoop (sPids s)
 waitLoop m

-- TODO read this: https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Signals
waitPidsLoop :: [ProcessID] -> IO () -- [ProcessID]
waitPidsLoop [] = exitSuccess
waitPidsLoop ps = do
 -- https://ghc.haskell.org/trac/ghc/ticket/4504
 blockSignals reservedSignals
 awaitSignal Nothing >> yield
 threadDelay (1 * 1000000)
 yield

alarm :: MVar State -> IO ()
alarm m = do
 s <- readMVar m
 sendKill . sPids $ s
 quit . sReboot $ s

quit :: Bool -> IO ()
quit False =
 putStrLn "calling shutdown"
  >> callCommand "RUNLEVEL=0 /sbin/rc shutdown"
  >> executeFile "/sbin/poweroff" False ["-f"] Nothing
quit True =
 putStrLn "calling reboot"
  >> callCommand "RUNLEVEL=6 /sbin/rc reboot"
  >> executeFile "/sbin/reboot" False ["-f"] Nothing

installAlarmSignalHandler :: MVar State -> IO ()
installAlarmSignalHandler s =
 installHandler sigALRM (Catch $ alarm s)  Nothing
 >> return ()

installSignalHandlers :: MVar State -> IO ()
installSignalHandlers s =
 installHandler sigHUP  (Catch $ hup s)  Nothing
 >> installHandler sigUSR1 (Catch $ shutdown s) Nothing
 >> installHandler sigUSR2 (Catch $ reboot s)   Nothing
 >> installHandler sigTERM (Catch $ term s)   Nothing
 >> installHandler sigCHLD (Catch $ reap s)   Nothing
 >> return ()

reap :: MVar State -> IO ()
reap m = do
 putStrLn "received CHLD"
 s <- takeMVar m
 p <- getAnyProcessStatus True True
 putStrLn $ "getAnyProcessStatus returned: " ++ (groom p)
 let state = s{sPids = removePid (sPids s) p}
 putMVar m state
 putStrLn . groom $ state

removePid :: [ProcessID] -> Maybe (ProcessID, ProcessStatus) -> [ProcessID]
removePid [] _ = []
removePid ps Nothing = ps
removePid ps (Just (pid,_)) = filter (not . (==) pid) ps

restart :: [ProcessID] -> IO ()
restart ps =
 (putStrLn . show $ ps)
  >> getEnvironment
  >>= executeFile "/home/j/dev/scripts/init/testrc" False (map show ps)
          . Just

hup :: MVar State -> IO ()
hup f = putStrLn "received HUP" >> takeMVar f >>= restart . sPids

shutdown :: MVar State -> IO ()
shutdown s =
 putStrLn "received USR1"
 >> installAlarmSignalHandler s
 >> takeMVar s
 >>= sendTerm . sPids
 >> scheduleAlarm 1
 >> return ()

reboot :: MVar State -> IO ()
reboot f = do
  putStrLn "received USR2"
  installAlarmSignalHandler f
  s <- takeMVar f
  putMVar f (s {sReboot = True})
  sendTerm . sPids $ s
  _ <- scheduleAlarm 1
  return ()

term :: MVar State -> IO ()
term f = putStrLn "received TERM" >> takeMVar f >>= sendTerm . sPids

sendTerm :: [ProcessID] -> IO ()
sendTerm = sendSignal softwareTermination

sendKill :: [ProcessID] -> IO ()
sendKill = sendSignal killProcess

sendSignal :: Signal -> [ProcessID] -> IO ()
sendSignal sig ps = mapM_ (signalProcess sig) ps

uninstallSignalHandlers :: IO ()
uninstallSignalHandlers =
    installHandler sigCHLD Default Nothing
    >> installHandler sigUSR1 Default Nothing
    >> installHandler sigUSR2 Default Nothing
    >> return ()

-- -- | spawn. Launch an external application. Specifically, it double-forks and
-- -- runs the 'String' you pass as a command to \/bin\/sh.
-- --
-- -- Note this function assumes your locale uses utf8.
-- spawn :: MonadIO m => String -> m ()
-- spawn x = spawnPID x >> return ()

-- -- | Like 'spawn', but returns the 'ProcessID' of the launched application
-- spawnPID :: MonadIO m => String -> m ProcessID
-- spawnPID x = xfork $ executeFile "/bin/sh" False ["-c", encodeString x] Nothing

-- -- | A replacement for 'forkProcess' which resets default signal handlers.
-- xfork :: MonadIO m => IO () -> m ProcessID
-- xfork x = io . forkProcess . finally nullStdin $ do
--                 uninstallSignalHandlers
--                 createSession
--                 x
--  where
--     nullStdin = do
--         fd <- openFd "/dev/null" ReadOnly Nothing defaultFileFlags
--         dupTo fd stdInput
--         closeFd fd

-- -- | Conditionally run an action, using a @Maybe a@ to decide.
-- whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
-- whenJust mg f = maybe (return ()) f mg

-- -- | A 'trace' for the 'X' monad. Logs a string to stderr. The result may
-- -- be found in your .xsession-errors file
-- trace :: MonadIO m => String -> m ()
-- trace = io . hPutStrLn stderr
