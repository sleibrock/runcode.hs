-- runcode.hs main file

module Main where

import Control.Monad (forM_)
import System.Exit
import System.Process (callProcess, createProcess, proc, waitForProcess)
import System.Environment (getArgs, getEnv)



data ProgMeta = ProgMeta {
  file      :: String
  ,compiler :: String
  ,args     :: [String]
  ,runExec  :: Maybe String
  } deriving (Show, Eq)


-- Split up a string based on a char->bool predicate
splitUp :: (Char -> Bool) -> String -> [String]
splitUp p s = case dropWhile p s of
                "" -> []
                s' -> w : splitUp p s''
                      where (w, s'') = break p s'


-- Get the extension of a file string (hello.go -> go)
getExtension :: String -> String
getExtension = last . splitUp (=='.')


-- Craft a program metadata struct
-- Contains compiler information and how to run each file
makeMeta :: String -> Maybe ProgMeta
makeMeta fname = case getExtension fname of
                   "c"   -> Just $ ProgMeta fname "gcc"    [fname] $ Just "./a.out" 
                   "cpp" -> Just $ ProgMeta fname "g++"    [fname] $ Just "./a.out"
                   "go"  -> Just $ ProgMeta fname "go"     ["run", fname] Nothing
                   "rb"  -> Just $ ProgMeta fname "ruby"   [fname] Nothing
                   "py"  -> Just $ ProgMeta fname "python" [fname] Nothing 
                   _ -> Nothing


-- Code to run a process and yield the exit code within the IO monad
runProcess :: ProgMeta -> IO (Int)
runProcess pm = do
  putStrLn $ "--- Attempting to build/run " ++ (file pm) ++ " --- " 
  (i, o, e, ph) <- createProcess $ proc (compiler pm) (args pm)
  exit <- waitForProcess ph
  case exit of
    ExitFailure x -> return x
    ExitSuccess -> do
      case (runExec pm) of
        Nothing -> return 0
        Just fpath -> do
          putStrLn $ "--- Attempting to run " ++ fpath ++ " --- "
          (i', o', e', ph') <- createProcess $ proc fpath []
          exit' <- waitForProcess ph'
          case exit of
            ExitSuccess -> do
              putStrLn $ "--- Finished running " ++ fpath ++ " ---"
              return 0
  

-- Code to use on file strings to run all actions
runCode :: String -> IO ()
runCode fname = runCode' (makeMeta fname) 
  where runCode' Nothing = putStrLn $ "Error: no metadata found for file " ++ fname 
        runCode' (Just m) = do
          runProcess m
          putStrLn $ "Done processing " ++ fname
        

-- Main entrypoint
main :: IO ()
main = do
  args <- getArgs
  forM_ args runCode 
  putStrLn "Done"


-- end Main.hs
