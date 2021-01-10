module Utils
        ( printCurDirInConsole
        , printDirTree
        , findFileRec
        , dirSizeRec
        , dirCountRec
        ) where

import Control.Monad.IO.Class     ( liftIO )
import System.FilePath            ( (</>) )
import System.Directory           ( listDirectory
                                  , doesDirectoryExist
                                  , canonicalizePath
                                  , getFileSize )
import System.IO                  ( hFlush
                                  , stdout )

printCurDirInConsole :: FilePath -> IO ()
printCurDirInConsole path = do
  liftIO $ putStr $ path ++ "> "
  hFlush stdout

printDirTree :: FilePath -> Int -> IO ()
printDirTree path padding = do
  files <- listDirectory path
  printDir path files padding

printDir :: FilePath -> [FilePath] -> Int -> IO ()
printDir path (x : []) padding = do
  newPath <- canonicalizePath (path </> x)
  putStrLn ((concat (take padding (repeat "│ "))) ++ x)
  isDir <- doesDirectoryExist newPath
  if (isDir)
    then
      printDirTree newPath (padding + 1)
    else
      return ()

printDir path (x : xs) padding = do
  newPath <- canonicalizePath (path </> x)
  putStrLn ((concat (take padding (repeat "│ "))) ++ x)
  isDir <- doesDirectoryExist newPath
  if (isDir)
    then do
      printDirTree newPath (padding + 1)
      files <- listDirectory newPath
      printDir newPath files (padding + 1)
      printDir (path) xs padding
    else
      printDir (path) xs padding
printDir _ [] _ = return ()

findFileRec :: FilePath -> FilePath -> IO ()
findFileRec path file = do
  files <- listDirectory path
  findFileFromArr path files file

findFileFromArr :: FilePath -> [FilePath] -> FilePath -> IO ()
findFileFromArr path (x : []) file = do
  newPath <- canonicalizePath (path </> x)
  isDir <- doesDirectoryExist newPath
  if (isDir)
    then do
      findFileRec newPath file
    else do
      if (x == file)
        then do
          putStrLn newPath
        else do
          return ()

findFileFromArr path (x : xs) file = do
  newPath <- canonicalizePath (path </> x)
  isDir <- doesDirectoryExist newPath
  if (isDir)
    then do
      findFileRec newPath file
      files <- listDirectory newPath
      findFileFromArr newPath files file
      findFileFromArr path xs file
    else do
      if (x == file)
        then do
          liftIO $ putStrLn newPath
          findFileFromArr path xs file
        else findFileFromArr path xs file

findFileFromArr _ [] _ = return ()

dirCountRec :: FilePath -> IO Integer
dirCountRec path = do
  isDir <- doesDirectoryExist path
  if (isDir)
    then do
      len <- length <$> (listOfDir path >>= mapM dirSizeRec)
      return $ toInteger len
    else
      return 1

dirSizeRec :: FilePath -> IO Integer
dirSizeRec path = do
  isDir <- doesDirectoryExist path
  if (isDir)
    then do
      sum <$> (listOfDir path >>= mapM dirSizeRec)
    else
      getFileSize path

listOfDir :: FilePath -> IO [FilePath]
listOfDir name = listDirectory name >>= mapM (canonicalizePath . (name </>))

--dirCountRec :: (Monad m) => FilePath -> Int -> m Integer
--dirCountRec path count = do
--  files <- liftIO $ listDirectory path
--  dirCount (map (\name -> path </> name) files) count

--dirCount :: [FilePath] -> Int -> m Integer
--dirCount = undefined
--dirCount path (x : []) padding = do
--  let isD = doesDirectoryExist (path </> x)
--  isDD <- isD
--  if (isDD)
--    then dirCountRec (path </> x) (padding + 1)
--    else return ()
--
--dirCount path (x : xs) padding = do
--  let isD = doesDirectoryExist (path </> x)
--  isDD <- isD
--  if (isDD)
--    then do
--      dirCountRec (path </> x) (padding + 1)
--      files <- listDirectory (path </> x)
--      dirCount (path </> x) files (padding + 1)
--      dirCount (path) xs padding
--    else dirCount (path) xs padding
--
--dirCount _ [] _ = return ()