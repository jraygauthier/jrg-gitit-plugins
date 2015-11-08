module Network.Gitit.Plugins.IncludeAsCodeBlock (plugin) where

{- 
    This plugin allows you to include any code file inside the current
    code block.

    ~~~ {.csv include-code="my-file.csv"}
    This will be replaced by the content of the csv file.
    ~~~
    
-}

import Network.Gitit.Interface

-- from the utf8-string package on HackageDB:
--import Data.ByteString.Lazy.UTF8 (fromString)
-- from the SHA package on HackageDB:
import System.FilePath ((</>), normalise, takeDirectory )
import System.Directory(doesFileExist)
import Control.Monad.Trans (liftIO)

import Data.FileStore.Types(retrieve, FileStoreError(..))
import Data.List(stripPrefix)
import Control.Exception (catch)

plugin :: Plugin
plugin = mkPageTransformM transformBlock

transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (id, classes, namevals) _) | "include-code" `elem` map fst namevals = do
    --cfg <- askConfig
    rq <- askRequest
    fs <- askFileStore
    let
        handledAttrs = ["include-code"]
        unhandledAttrs = filter (\e -> not (elem (fst e) handledAttrs)) namevals
                    
        userUrl = maybe "" (\x -> x) $ lookup "include-code" namevals

        formattedUrl = urlFromFSRoot (rqUri rq) userUrl

        errorHandler ::  FileStoreError -> IO (String)
        errorHandler e = return $ "Could not access to: \"" ++ formattedUrl ++ "\""

    liftIO $ do
        fileContent <- catch 
            (retrieve fs formattedUrl Nothing)
            errorHandler
        return $ CodeBlock (id, classes, unhandledAttrs) fileContent

        
transformBlock x = return x


urlFromFSRoot requestUri ressourceUrl =
    stripPreview impl
  where relUrlToCurrentDir = drop 1 . takeDirectory $ requestUri
        impl | isRelUri ressourceUrl    = relUrlToCurrentDir ++ "/" ++ ressourceUrl
             | otherwise                = drop 1 ressourceUrl
        stripPreview ori = maybe ori (\x -> x) (stripPrefix "_preview/" ori)
        isRelUri :: String -> Bool
        isRelUri uri = take 1 uri /= "/"