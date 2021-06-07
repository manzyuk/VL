{-# LANGUAGE RecordWildCards #-}
import Control.Monad
import Control.Monad.State
import System.Directory
import System.Exit
import System.FilePath
import System.FilePath.Glob
import System.IO
import System.Process
import Text.Printf

data TestStat = TestStat { passed :: !Int, failed :: !Int }

initStat :: TestStat
initStat = TestStat { passed = 0, failed = 0 }

io :: MonadIO m => IO a -> m a
io = liftIO

run :: String -> [String] -> IO ()
run command arguments = do
  exitCode <- system $ unwords $ command : arguments
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure code -> do
      hPutStrLn stderr $ unlines
        [ "The following command failed:"
        , command
        , "The exit code was: " ++ show code
        ]
      exitWith exitCode

main :: IO ()
main = do
  -- NB: 'glob' returns a list of _absolute_ paths.
  vlFiles <- glob "test/vl/*.vl"

  TestStat {..} <- flip execStateT initStat $ forM_ vlFiles $ \vlFile -> do

    let name    = dropExtension vlFile
        cFile   = name <.> "c"
        outFile = name <.> "out"

    io $ run "stack" ["exec", "vl2c", vlFile, cFile]

    io $ run "gcc" [cFile, "-o", outFile]

    received <- io $
      fmap (filter (`notElem` "\n ")) $ readProcess outFile [] []
    expected <- io $
      fmap (filter (`notElem` ";\n ") . last . lines) $ readFile vlFile

    if received == expected
      then do
        io $ printf "%-50s PASSED\n" vlFile
        modify $ \stat -> stat { passed = passed stat + 1 }
      else do
        io $ printf "%-50s FAILED\n" vlFile
        io $ printf "expected: %s" expected
        io $ printf "received: %s" received
        modify $ \stat -> stat { failed = failed stat + 1 }

  printf
    "Totally %d test(s), %d passed, %d failed"
    (passed + failed) passed failed

  exitWith $ if failed == 0 then ExitSuccess else ExitFailure 1
