module Network.Gitit.Plugins.Ditaa (plugin) where

{- 
    This plugin allows you to include a ditaa diagram
    in a page like this:

    ~~~ {.ditaa name="ditaa_diagram1"}
        +--------+   +-------+    +-------+
        |        | --+ ditaa +--> |       |
        |  Text  |   +-------+    |diagram|
        |Document|   |!magic!|    |       |
        |     {d}|   |       |    |       |
        +---+----+   +-------+    +-------+
            :                         ^
            |       Lots of work      |
            +-------------------------+
    ~~~

    Must be installed and in the path:
     -  Java virtual machine (java)
     -  Copy "ditaa.jar" at a known location.
        See: http://ditaa.sourceforge.net/#download
     -  add ditaa.jar to the CLASSPATH environment variable.
         -  e.g. (Windows): set CLASSPATH=C:\My\AbsPath\To\Jar\Files\ditaa.jar;%CLASSPATH%
         -  e.g. (Unix): export CLASSPATH=/My/AbsPath/To/Jar/Files/ditaa.jar:%CLASSPATH%

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


plugin :: Plugin
plugin = mkPageTransformM transformBlock

transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (_, classes, namevals) contents) | "ditaa" `elem` classes = do
  cfg <- askConfig
  let (name, outfile) = case lookup "name" namevals of
                                Just fn -> ([Str fn], fn ++ ".png")
                                Nothing -> ([], uniqueName contents ++ ".png")
  liftIO $ do
    (ec, pngData, err) <- 
        withSystemTempFile "ditaa.txt" $ \contentFilePath hf -> do
            hPutStr hf contents
            hClose hf
            readProcessWithExitCode "java" ["org.stathissideris.ascii2image.core.CommandLineConverter", 
                                            "-o",
                                            contentFilePath,
                                            staticDir cfg </> "img" </> outfile] ""
    
    if ec == ExitSuccess
        then return $ Para [Image name ("/img/" ++ outfile, "")]
        else error $ "ditaa returned an error status: " ++ err
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
