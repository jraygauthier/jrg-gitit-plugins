module Network.Gitit.Plugins.PlantUML (plugin) where

{- 
    This plugin allows you to include a plantuml diagram
    in a page like this:

    ~~~ {.plantuml name="plantuml_diagram1"}
    @startuml
    Bob -> Alice : hello
    @enduml 
    ~~~

    Must be installed and in the path:
     -  Java virtual machine (java)
     -  Copy "plantuml.jar" at a known location.
        See: http://plantuml.sourceforge.net/download.html
     -  add planuml.jar to the CLASSPATH environment variable.
         -  e.g. (Windows): set CLASSPATH=C:\My\AbsPath\To\Jar\Files\plantuml.jar;%CLASSPATH%
         -  e.g. (Unix): export CLASSPATH=/My/AbsPath/To/Jar/Files/plantuml.jar:%CLASSPATH%
     -  The "dot" executable must be in the path. 
        See: http://www.graphviz.org/Download.php
     -  We also assume an executable wrapper that will properly handle
        calls in the form `plantuml myArguments`.
    The generated png file will be saved in the static img directory.
    If no name is specified, a unique name will be generated from a hash
    of the file contents.

    Any classes not handled by the plug-in are simply handled to the
    Span wrapper put around the image container.

    The first id attribute found is handled to the previously mentioned
    Span wrapper.

    The preceding two points allow a user to add some classes and an id
    to a diagram in order to control its layout using a custom css.

    ~~~ {#my-class-diagram .plantuml .auto-expand }
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
import Data.List (intersperse)

-- Needed only for runProcessWithStrInToOutFile. TODO: Think about moving the funtion to its own module.
import System.Process (createProcess, proc, CreateProcess(..), CmdSpec(..), StdStream(..), ProcessHandle, waitForProcess)
import System.IO (withBinaryFile, hGetContents, hPutStr, hClose, hFlush, IOMode(..))
import qualified Control.Exception as C
import Control.Monad (when)

plugin :: Plugin
plugin = mkPageTransformM transformBlock

transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (id, classes, namevals) contents) | "plantuml" `elem` classes = do
    cfg <- askConfig
    rq <- askRequest
    --meta <- askMeta
    --ctx <- getContext
    let
        handledClasses = ["plantuml"]
        unhandledClasses = filter (\e -> not (elem e handledClasses)) classes
        relUrlToCurrentDir = drop 1 . rqUri $ rq
        isRelUri :: String -> Bool
        isRelUri uri = take 1 uri /= "/"
        uriFromImg oriUri | isRelUri oriUri = relUrlToCurrentDir ++ "/" ++ oriUri
                          | otherwise       = drop 1 oriUri

        (outExt, ftParams) = case lookup "type" namevals of
                                            Just ft -> ('.':ft, ["-t" ++ ft])
                                            Nothing -> (".png", [])
        (name, outfile) = case lookup "name" namevals of
                                    Just fn -> ([Str fn], uriFromImg fn ++ outExt)
                                    Nothing -> ([], "unnamed/" ++ 
                                                        uniqueName contents ++ outExt)

        exeName = "plantuml"
        args = ["-pipe"] ++ ftParams 

        handledAttrs = ["type", "name"]
        unhandledAttrs = filter (\e -> not (elem (fst e) handledAttrs)) namevals
                    
        localOutFn = normalise (staticDir cfg) </> "img" </> (normalise outfile)
        localOutDir = takeDirectory localOutFn
        
    liftIO $ do
        createDirectoryIfMissing True localOutDir 
        (ec, stderr) <- runProcessWithStrInToOutFile exeName args contents localOutFn
        
        if ec == ExitSuccess && "" == stderr
            then 
              let 
                imgInline = Image nullAttr name ("/img/" ++ outfile, "")
                imgBlock = Para [Span (id, unhandledClasses, unhandledAttrs) [imgInline]]
              in
                return $ imgBlock
            else 
              let 
                errorBlock = CodeBlock (id, classes, namevals) errorMsg
                errorMsg = 
                  "[content] | " ++ exeName ++ " " ++ (concat . intersperse " "  $ args) ++ "\n\n" ++
                  prettyPrintErrorCode ec ++
                  prettyPrintStdStream "stderr" stderr
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
        
-- | Generate a unique filename given the file's contents.
uniqueName :: String -> String
uniqueName = showDigest . sha1 . fromString
