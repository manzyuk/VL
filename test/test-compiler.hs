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

vl2c :: FilePath
vl2c = "dist" </> "build" </> "vl2c" </> "vl2c"

main :: IO ()
main = do
  -- Check if the executable 'vl2c' exists and fail if it doesn't.
  vl2cExists <- doesFileExist vl2c
  unless vl2cExists $ do
    hPutStrLn stderr $
      printf "Can't find %s. Try running 'cabal build'." vl2c
    exitWith $ ExitFailure 1

  -- NB: 'glob' returns a list of _absolute_ paths.
  vlFiles <- glob "test/vl/*.vl"

  TestStat {..} <- flip execStateT initStat $ forM_ vlFiles $ \vlFile -> do

    let name    = dropExtension vlFile
        cFile   = name <.> "c"
        outFile = name <.> "out"

    io $ run vl2c  [vlFile, cFile]

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
