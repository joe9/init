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
import           System.Posix.Process (executeFile, forkProcess, interruptibleGetAnyProcessStatus)
import           System.Posix.Process (ProcessStatus)
import           System.Posix.Signals
import           System.Posix.Types   (ProcessID)
-- import           System.Posix.Unistd
-- import           System.Process
import           Text.Groom

-- fork and wait for the child

data Flags = Flags {
      fRestart    :: Bool
      , fShutdown :: Bool
      , fReboot   :: Bool
      , fTerm     :: Bool
}

instance Default Flags where def = Flags False False False False

main :: IO ()
main = do
 flags <- newMVar def
 args <- getArgs
 pids <- runOnce args
 installSignalHandlers flags pids
 putStrLn . groom $ pids
 waitLoop flags pids

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

waitLoop :: MVar Flags -> [ProcessID] -> IO ()
waitLoop mf ps = do
  f <- readMVar mf
  waitLoopFlagTest mf f ps

waitLoopFlagTest :: MVar Flags -> Flags -> [ProcessID] -> IO ()
waitLoopFlagTest _ f [] = exit f
waitLoopFlagTest _ (Flags {fRestart = True}) ps = restart ps
waitLoopFlagTest mf f@(Flags {fTerm = True}) ps = do
 putStrLn "waitLoopFlagTest: checking Term "
 sendTerm ps
 putMVar mf (f {fTerm = False})
 waitLoop mf ps
waitLoopFlagTest mf _ ps = do
 putStrLn "waitLoopFlagTest: looping again"
 interruptibleGetAnyProcessStatus True False >>= waitLoop mf . removePid ps

exit :: Flags -> IO ()
exit (Flags {fShutdown = True}) =
 putStrLn "calling shutdown"
--  callCommand "RUNLEVEL=0 /sbin/rc shutdown"
--   >> executeFile "/sbin/poweroff" False ["-f"] Nothing
exit (Flags {fReboot = True}) =
 putStrLn "calling reboot"
--  callCommand "RUNLEVEL=6 /sbin/rc reboot"
--   >> executeFile "/sbin/reboot" False ["-f"] Nothing
exit _ = exitSuccess

sendTerm :: [ProcessID] -> IO ()
sendTerm ps = mapM_ (signalProcess sigTERM) ps >> putStrLn "after sendTERM"


removePid :: [ProcessID] -> Maybe (ProcessID, ProcessStatus) -> [ProcessID]
removePid [] _ = []
removePid ps Nothing = ps
removePid ps (Just (pid,_)) = filter ((==) pid) ps

installSignalHandlers :: MVar Flags -> [ProcessID] -> IO ()
installSignalHandlers f ps =
 installHandler sigHUP  (Catch $ setRestart f)  Nothing
 >> installHandler sigUSR1 (Catch $ setShutdown f) Nothing
 >> installHandler sigUSR2 (Catch $ setReboot f)   Nothing
 >> installHandler sigTERM (Catch $ setTerm f ps)   Nothing
 >> return ()

restart :: [ProcessID] -> IO ()
restart ps =
 (putStrLn . groom $ ps)
  >> getEnvironment
  >>= executeFile "/home/j/dev/scripts/init/rc" False (map show ps)
          . Just

setRestart :: MVar Flags -> IO ()
setRestart mf = do
  fs <- takeMVar mf
  putMVar mf (fs {fRestart = True})
  putStrLn "received HUP"

setShutdown :: MVar Flags -> IO ()
setShutdown mf = do
  fs <- takeMVar mf
  putMVar mf (fs {fShutdown = True})
  putStrLn "received USR1"

setReboot :: MVar Flags -> IO ()
setReboot mf = do
  fs <- takeMVar mf
  putMVar mf (fs {fReboot = True})
  putStrLn "received USR2"

setTerm :: MVar Flags -> [ProcessID] -> IO ()
setTerm f ps = do
--   fs <- takeMVar f
--   putMVar f (fs {fTerm = True})
  putStrLn "received TERM"
  sendTerm ps

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
