{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.Directory           ( getHomeDirectory
                                  , listDirectory
                                  , createDirectory
                                  , getPermissions
                                  , getFileSize
                                  , removeDirectoryRecursive
                                  , doesDirectoryExist
                                  , removeFile
                                  , canonicalizePath
                                  , getAccessTime)

import Control.Monad.Trans.Reader ( ReaderT (..)
                                  , ask)
import Control.Monad.IO.Class     ( liftIO )

import System.IO.Error            ( mkIOError
                                  , userErrorType
                                  , doesNotExistErrorType )

import Control.Exception          ( throwIO )

import Control.Monad.Except       ( catchError )

import Data.IORef                 (  IORef
                                  , newIORef
                                  , readIORef
                                  ,  writeIORef )

import Parser                     ( FSCommand (..)
                                  , parseInputCommand)

import Utils                      ( printCurDirInConsole
                                  , printDirTree
                                  , findFileRec
                                  , dirSizeRec
                                  , dirCountRec
                                  )

import System.FilePath            ( (</>) )

import System.FilePath.Posix      ( takeExtension )


type RealFS = ReaderT (IORef FilePath) IO

main :: IO ()
main = do
  catchError (startFS) (\e -> (liftIO $ print e))


startFS :: IO ()
startFS = do
  curDirStr   <- getHomeDirectory
  curDirIORef <- newIORef curDirStr
  runReaderT fileManagerRunLoop curDirIORef

fileManagerRunLoop :: RealFS ()
fileManagerRunLoop = do
  context <- ask
  path    <- liftIO $ readIORef context
  liftIO $ printCurDirInConsole path
  newCommand <- liftIO getLine
  if (newCommand == "stop")
    then return ()
    else do
      let resCommand = parseInputCommand newCommand
      case resCommand of
        Just CommandHelp -> printHelp
        Just CommandPwd -> currentDir path
        Just CommandLs -> dirContent path
        Just (CommandLsAll        p) -> dirContentAll (path </> p)
        Just (CommandRemoveDir    p) -> removeDir     (path </> p)
        Just (CommandRemoveFile   p) -> deleteFile    (path </> p)
        Just (CommandMkdir        p) -> createDir     (path </> p)
        Just (CommandCd           p) -> changeDir     path p
        Just (CommandTouch        p) -> createFile    path p
        Just (CommandCat          p) -> printFile     path p
        Just (CommandDirInfo      p) -> directoryInfo path p
        Just (CommandFileInfo     p) -> fileInfo      path p
        Just (CommandFind         p) -> findFile      path p
        Just (CommandWrite    p1 p2) -> writeInFile   path p1 (p2 !! 0)
        _ -> liftIO $ throwIO $ mkIOError userErrorType "invalid command" Nothing (Just path)

  fileManagerRunLoop

class (Monad m) => FSAction m where
  printHelp     :: m ()
  dirContent    :: FilePath -> m ()
  dirContentAll :: FilePath -> m ()
  removeDir     :: FilePath -> m ()
  deleteFile    :: FilePath -> m ()
  currentDir    :: FilePath -> m ()
  createDir     :: FilePath -> m ()
  changeDir     :: FilePath -> String -> m ()
  findFile      :: FilePath -> String -> m ()
  createFile    :: FilePath -> String -> m ()
  printFile     :: FilePath -> String -> m ()
  fileInfo      :: FilePath -> String -> m ()
  directoryInfo :: FilePath -> String -> m ()
  writeInFile   :: FilePath -> String -> String -> m ()


instance FSAction RealFS where
  printHelp = do
    liftIO $ putStrLn "pwd\n - current directory\
 \ls\n - file in this directory\
 \ls-all [dir name] - file in tree of directories\n\
 \cd [path] - set directory\n\
 \rmd [dir name] - remove directory\n\
 \rmf [file name] - remove file\n\
 \mkdir [dir name] - make directory\n\
 \touch [file name] - make file\n\
 \cat [file name] - print file\n\
 \write [file name] [text] - write text in file\n\
 \dinfo [dir name] - info about directory\n\
 \finfo [file name] - info about file\n\
 \find [file name] - find file"

  dirContent path = do
    filesIO <- liftIO $ listDirectory path
    let res = foldl (\ex newFile -> ex ++ newFile ++ "\n") "" filesIO
    liftIO $ putStrLn res

  dirContentAll path = do
    liftIO $ printDirTree path 0
    return ()

  currentDir path = liftIO $ putStrLn path

  removeDir path = liftIO $ removeDirectoryRecursive path

  deleteFile path = liftIO $ removeFile path

  createDir path = liftIO $ createDirectory path

  changeDir path dirName = do
    newPath <- liftIO $ canonicalizePath (path </> dirName)
    curPath <- ask
    dirExists <- liftIO $ doesDirectoryExist newPath
    if (dirExists)
      then
        liftIO $ writeIORef curPath newPath
      else
        liftIO $ throwIO $ mkIOError doesNotExistErrorType "setDirectory" Nothing (Just newPath)

  createFile path fileName = do
    let emptyString = ""
    liftIO $ writeFile (path </> fileName) emptyString

  printFile path fileName = do
    fileContent <- liftIO $ readFile (path </> fileName)
    liftIO $ putStrLn fileContent

  writeInFile path fileName text = do
    liftIO $ writeFile (path </> fileName) text

  findFile path fileName = do
    liftIO $ findFileRec path fileName

  fileInfo path fileName = do
    let filePath = (path </> fileName)
    let fileExtension = takeExtension (path </> fileName)
    filePermissions <- liftIO $ getPermissions filePath
    accessTime <- liftIO $ getAccessTime filePath
    fileSize <- liftIO $ getFileSize filePath

    liftIO $ putStrLn ("file path - " ++ filePath)
    liftIO $ putStrLn ("extension - " ++ fileExtension)
    liftIO $ putStrLn ("permissons - " ++ (show filePermissions))
    liftIO $ putStrLn ("access time - " ++ (show accessTime))
    liftIO $ putStrLn ("file size - " ++ (show fileSize) ++ " bytes")

  directoryInfo path dirName = do
    let dirPath = (path </> dirName)
    dirSize         <- liftIO $ dirSizeRec     dirPath
    filePermissions <- liftIO $ getPermissions dirPath
    filesCount      <- liftIO $ dirCountRec    dirPath

    liftIO $ putStrLn ("directory path - " ++ dirPath)
    liftIO $ putStrLn ("permissons - "     ++ (show filePermissions))
    liftIO $ putStrLn ("files count - "    ++ (show filesCount))
    liftIO $ putStrLn ("size - "           ++ (show dirSize) ++ " bytes")