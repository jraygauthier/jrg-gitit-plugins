module Network.Gitit.Plugins.HsDiagrams (plugin) where

{- 
    This plugin allows you to include a plantuml diagram
    in a page like this:

    ~~~ {.hsdiagrams name="haskell_diagram1"}
    dia = circle 1
    ~~~

    Must be installed and in the path:
     -  diagrams-builder-svg
     -  diagrams-builder-png
    The generated png file will be saved in the static img directory.
    If no name is specified, a unique name will be generated from a hash
    of the file contents.

    Any classes not handled by the plug-in are simply handled to the
    Span wrapper put around the image container.

    The first id attribute found is handled to the previously mentioned
    Span wrapper.

    The preceding two points allow a user to add some classes and an id
    to a diagram in order to control its layout using a custom css.

    ~~~ {#my-class-diagram .hsdiagrams .auto-expand }
    ..
    ~~~
    
    with a `custom.css` of:

    ..   
    span.auto-expand img {
        width: 100%;
    }
    span#my-class-diagram img {
        border: 2px solid grey;
    }

    Will give a diagram that auto-expand to its container and
    that has a 2px grey border.
    
-}

import Network.Gitit.Interface

import System.Exit (ExitCode(ExitSuccess))
-- from the utf8-string package on HackageDB:
import Data.ByteString.Lazy.UTF8 (fromString)
-- from the SHA package on HackageDB:
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.FilePath ((</>), takeDirectory, normalise)
import System.Directory(createDirectoryIfMissing)
import Control.Monad.Trans (liftIO)
import Control.Concurrent
import System.Process (readProcessWithExitCode)
import Data.List (intersperse)

-- Needed only for `withSystemTempFile`
import System.Directory(getTemporaryDirectory, removeFile)
import Control.Exception (bracket)
import System.IO (openTempFile, hClose, hPutStr, Handle)
import System.IO.Error(catchIOError, ioError, isDoesNotExistError)

plugin :: Plugin
plugin = mkPageTransformM transformBlock

transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (id, classes, namevals) contents) | "hsdiagrams" `elem` classes = do
    cfg <- askConfig
    rq <- askRequest
    --meta <- askMeta
    --ctx <- getContext
    let
        handledClasses = ["hsdiagrams"]
        unhandledClasses = filter (\e -> not (elem e handledClasses)) classes
        relUrlToCurrentDir = drop 1 . rqUri $ rq
        isRelUri :: String -> Bool
        isRelUri uri = take 1 uri /= "/"
        uriFromImg oriUri | isRelUri oriUri = relUrlToCurrentDir ++ "/" ++ oriUri
                          | otherwise       = drop 1 oriUri

        (outExt, backendName) = case lookup "type" namevals of
                                            Just ft | ft == "png" -> ('.':ft, "cairo")
                                                    | otherwise -> ('.':ft, ft)
                                            Nothing -> (".png", "cairo")
        (name, outfile) = case lookup "name" namevals of
                                    Just fn -> ([Str fn], uriFromImg fn ++ outExt)
                                    Nothing -> ([], "unnamed/" ++ 
                                                        uniqueName contents ++ outExt)

        exeName = "diagrams-builder-" ++ backendName
        handledAttrs = ["type", "name"]
        unhandledAttrs = filter (\e -> not (elem (fst e) handledAttrs)) namevals
                    
        localOutFn = normalise (staticDir cfg) </> "img" </> (normalise outfile)
        localOutDir = takeDirectory localOutFn
        
    liftIO $ do
        createDirectoryIfMissing True localOutDir
        let args = ["-w", "400", "-o", localOutFn]

        (ec, stdout, stderr) <- 
          withSystemTempFile "hsdiagrams.txt" $ \contentFilePath hf -> do
            hPutStr hf contents
            hClose hf
            readProcessWithExitCode exeName (args ++ [contentFilePath]) ""

        if ec == ExitSuccess && "" == stderr && "" == stdout
            then 
              let 
                imgInline = Image name ("/img/" ++ outfile, "")
                imgBlock = Para [Span (id, unhandledClasses, unhandledAttrs) [imgInline]]
              in
                return $ imgBlock
            else 
              let 
                errorBlock = CodeBlock (id, classes, namevals) errorMsg
                errorMsg = 
                  exeName ++ " " ++ (concat . intersperse " "  $ args) ++ " [content]\n\n" ++
                  prettyPrintErrorCode ec ++
                  prettyPrintStdStream "stderr" stderr ++ 
                  prettyPrintStdStream "stdout" stdout
              in
                return $ errorBlock
        
transformBlock x = return x

prettyPrintErrorCode ExitSuccess = ""
prettyPrintErrorCode ec =
  "error code\n" ++
  "==========\n\n" ++ show ec ++ "\n\n";

prettyPrintStdStream _ "" = ""
prettyPrintStdStream streamName content = 
  streamName ++ "\n" ++ 
  "======\n\n" ++ content ++ "\n\n";

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
