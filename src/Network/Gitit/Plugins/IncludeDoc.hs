module Network.Gitit.Plugins.IncludeDoc (plugin) where

{- 
    This plugin allows you to include any pandoc supported document inside the
    current document.

    ~~~ {include="mydocument.page" type="markdown"}
    This whole code block will be replaced by the document.
    ~~~
    
    If no type is provided, will attempt to infer it from the specified
    included file extension (TODO). In cases where the extension is unknown, 
    will assume `markdown`. 

    The whole `Pandoc` layer is replaced by a `Div` whose attributes correspond
    to that of the original `CodeBlock` minus `include` and `type`. This should
    allow some control over styling of included blocks.

    An idea: could also by default add a `.included` class.
-}

import Network.Gitit.Interface

-- from the utf8-string package on HackageDB:
--import Data.ByteString.Lazy.UTF8 (fromString)
-- from the SHA package on HackageDB:
import System.FilePath ((</>), normalise, takeDirectory )
import System.Directory(doesFileExist)
import Control.Monad.Trans (liftIO)

import Text.Pandoc(getReader)
import Text.Pandoc.Options(ReaderOptions(..), def)

import Data.FileStore.Types(retrieve, FileStoreError(..),  FileStore (..))
import Data.List(stripPrefix)
import Control.Exception (catch)

plugin :: Plugin
plugin = mkPageTransformM transformBlock

transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (id, classes, namevals) _) | "include" `elem` map fst namevals = do
    --cfg <- askConfig
    rq <- askRequest
    fs <- askFileStore
    let
        handledAttrs = ["include", "type"]
        unhandledAttrs = filter (\e -> not (elem (fst e) handledAttrs)) namevals
                    
        userUrl = maybe "" (\x -> x) $ lookup "include" namevals

        -- TODO: Try to infer an appropriate type from the file extension.
        formSpecs = maybe "markdown" (\x -> x) $ lookup "type" namevals

        formattedUrl = urlFromFSRoot (rqUri rq) userUrl

    liftIO $ do
        fileContent <- getFileContent fs formattedUrl
        case fileContent of
            Left err -> return $ CodeBlock (id, classes, unhandledAttrs) err
            Right content -> do
                doc <- readDocFrom content formSpecs
                return $ case doc of
                    Left err -> CodeBlock (id, classes, unhandledAttrs) err
                    Right (Pandoc _ blocks) -> Div (id, classes, unhandledAttrs) blocks

transformBlock x = return x


getFileContent :: FileStore -> FilePath -> IO (Either String String)
getFileContent fs url = 
    catch 
        (retrieve fs url Nothing >>= \c -> return (Right c))
        handler
  where
    handler ::  FileStoreError -> IO (Either String String)
    handler e = return . Left $ "Could not access to: \"" ++ url ++ "\""

urlFromFSRoot requestUri ressourceUrl =
    stripPreview impl
  where relUrlToCurrentDir = drop 1 . takeDirectory $ requestUri
        impl | isRelUri ressourceUrl    = relUrlToCurrentDir ++ "/" ++ ressourceUrl
             | otherwise                = drop 1 ressourceUrl
        stripPreview ori = maybe ori (\x -> x) (stripPrefix "_preview/" ori)
        isRelUri :: String -> Bool
        isRelUri uri = take 1 uri /= "/"


readDocFrom :: FilePath -> String -> IO (Either String Pandoc)
readDocFrom fc formSpec = 
    case getReader formSpec of
        Left err -> return . Left $ "Pandoc reader error: \"" ++ err ++ "\""
        Right readFn -> do
            doc <- readFn defaultOpts fc
            return $ Right doc
  where defaultOpts :: ReaderOptions
        defaultOpts = def