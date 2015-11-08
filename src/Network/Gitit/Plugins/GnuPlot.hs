module Network.Gitit.Plugins.GnuPlot (plugin) where

{- 
    This plugin allows you to include a gnuplot plot
    in a page like this:

    ~~~ {.gnuplot name="gnu_plot_result"}
    set title "Simple Plots" 
    plot [-10:10] sin(x),atan(x),cos(atan(x))
    ~~~

    Must be installed and in the path:
     -  gnuplot

    The generated png file will be saved in the static img directory.
    If no name is specified, a unique name will be generated from a hash
    of the file contents.
-}

import Network.Gitit.Interface
import System.Exit (ExitCode(ExitSuccess))
-- from the utf8-string package on HackageDB:
import Data.ByteString.Lazy.UTF8 (fromString)
-- from the SHA package on HackageDB:
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.FilePath ((</>))
import System.IO (openTempFile, hClose, hPutStr, Handle)
import System.IO.Error(catchIOError, ioError, isDoesNotExistError)
import System.Process (readProcessWithExitCode)
import System.Directory(getTemporaryDirectory, removeFile)
import Control.Exception (bracket)
import Control.Monad.Trans (liftIO)
import Control.Concurrent

-- Needed only for runProcessWithStrInToOutFile. TODO: Think about moving the funtion to its own module.
import System.Process (createProcess, proc, CreateProcess(..), CmdSpec(..), StdStream(..), ProcessHandle, waitForProcess)
import System.IO (withBinaryFile, hGetContents, hPutStr, hClose, hFlush, IOMode(..))
import qualified Control.Exception as C
import Control.Monad (when)


plugin :: Plugin
plugin = mkPageTransformM transformBlock

transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (_, classes, namevals) contents) | "gnuplot" `elem` classes = do
  cfg <- askConfig
  let (name, outfile) = case lookup "name" namevals of
                                Just fn -> ([Str fn], fn ++ ".png")
                                Nothing -> ([], "unnamed/" ++ uniqueName contents ++ ".png")
  liftIO $ do
    (ec, err) <- 
        withSystemTempFile "gnuplot.txt" $ \contentFilePath hf -> do
            hPutStr hf contents
            hClose hf
            runProcessWithStrInToOutFile "gnuplot" ["-e", 
                                            "set terminal pngcairo enhanced",
                                            contentFilePath] "" (staticDir cfg </> "img" </> outfile)
    
    if ec == ExitSuccess
        then return $ Para [Image name ("/img/" ++ outfile, "")]
        else error $ "gnuplot returned an error status: " ++ err
transformBlock x = return x

withSystemTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withSystemTempFile nameTpl callBack = 
    bracket
        (getTemporaryDirectory >>= \tmpDir -> openTempFile tmpDir nameTpl)
        (\(fp, hf) -> hClose hf >> rmFile fp)
        (uncurry callBack)
  where rmFile fp = removeFile fp `catchIOError ` recoverHandler
        recoverHandler e = if isDoesNotExistError e then return () else ioError e
        
-- | Generate a unique filename given the file's contents.
uniqueName :: String -> String
uniqueName = showDigest . sha1 . fromString

-- TODO: This is duplicate from PlantUML.hs. Use a shared module for reusable code.
runProcessWithStrInToOutFile :: FilePath -> [String] -> String -> FilePath -> IO (ExitCode, String)
runProcessWithStrInToOutFile cmd args input outfp = 
    withBinaryFile outfp WriteMode $ \outHdl -> do
    
    outMVar <- newEmptyMVar
    
    (Just inh, _, Just errh, pid) <- 
        createProcess (proc cmd args){ std_out = (UseHandle outHdl), std_in = CreatePipe, std_err = CreatePipe }
        
    err  <- hGetContents errh
    _ <- forkIO $ C.evaluate (length err) >> putMVar outMVar ()
    
    -- hSetBinaryMode inp False
    
    -- now write and flush any input
    when (not (null input)) $ do hPutStr inh input; hFlush inh
    hClose inh -- done with stdin
    -- wait on the output
    takeMVar outMVar
    hClose errh
    
    -- wait on the process
    ex <- waitForProcess pid
    
    return (ex, err)
