module XMonad.Wallpaper.Find (findImages) where

import System.Posix.Directory
import System.Posix.Files

import Control.Applicative
import Control.Monad
import Control.Exception

import Magic
import Control.Monad.State
import Data.Maybe
import Data.List

-- File recursive list

data UnixFile = RegularFile FilePath | Directory FilePath
    deriving (Show, Eq)

toUnixFile filepath = do
    exist <- fileExist filepath
    if exist
        then do
            status <- getFileStatus filepath
            return $ toUnixFile' status filepath
        else return Nothing
    where
        toUnixFile' status 
            | isRegularFile status = Just . RegularFile
            | isDirectory status   = Just . Directory
            | otherwise            = const Nothing

toFilepath (RegularFile filepath) = filepath
toFilepath (Directory filepath)   = filepath

findDir (Directory filepath) = do
    let readPaths stream = do
            path <- readDirStream stream
            if length path == 0
                then return []
                else do
                    paths <- readPaths stream
                    if head path == '.'
                        then return paths
                        else do
                            unix <- toUnixFile $ filepath ++ "/" ++ path
                            case unix of
                                Nothing    -> return paths
                                Just unix' -> return $ unix' : paths
    bracket (openDirStream filepath) closeDirStream readPaths
findDir _                    = return []
    
findDirRecursive unixPath@(Directory filepath) = do
    paths <- findDir unixPath
    subPaths <- concat <$> mapM findDirRecursive paths
    return $ paths ++ subPaths
findDirRecursive _                             = return []

-- mimetype detection

mimetype :: FilePath -> StateT Magic IO String
mimetype filepath = do
    magic <- get
    liftIO $ magicFile magic filepath

runMimetypeDetection action = do
    magic <- magicOpen [ MagicMimeType ]
    magicLoadDefault magic
    evalStateT action magic

-- find image files

isImage (RegularFile filepath) = isPrefixOf "image" <$> mimetype filepath
isImage _ = return False

{- |
Recursively search supplied paths. Files are filtered by mimetypes, which is determined by magic bits. Duplicated paths will be removed.
-}
findImages filepaths = do
    paths  <- catMaybes <$> mapM toUnixFile filepaths
    files  <- concat <$> mapM findDirRecursive paths
    images <- runMimetypeDetection $ filterM isImage files
    return $ nub $ map toFilepath images 
