-- runcode.hs main file

module Main where

import Control.Monad (forM_)
import System.Process (callProcess)
import System.Environment (getArgs, getEnv)



data ProgMeta = ProgMeta {
  compiler :: String
  , args :: [String]
  , runExec :: Maybe String
  } deriving (Show, Eq)


splitUp :: (Char -> Bool) -> String -> [String]
splitUp p s = case dropWhile p s of
                "" -> []
                s' -> w : splitUp p s''
                      where (w, s'') = break p s'

getExtension :: String -> String
getExtension = last . (splitUp (\c -> c == '.'))


makeMeta :: String -> Maybe ProgMeta
makeMeta fname = case getExtension fname of
                   "go"  -> Just ProgMeta { compiler = "go", args = ["run", fname], runExec = Nothing }
                   "cpp" -> Just ProgMeta { compiler = "g++", args = [fname], runExec = Just "a.out" } 
                   "rb"  -> Just ProgMeta { compiler = "ruby", args = [fname], runExec = Nothing }
                   "py"  -> Just ProgMeta { compiler = "python", args = [fname], runExec = Nothing }
                   _ -> Nothing


runCode :: String -> IO ()
runCode fname = runCode' (makeMeta fname) 
  where runCode' Nothing = putStrLn "Error" 
        runCode' (Just m) = do
          callProcess (compiler m) (args m)
          putStrLn $ "Done processing " ++ fname
        


main :: IO ()
main = do
  args <- getArgs
  forM_ args runCode 
  runCode "no_meta.f"
  putStrLn "Done"


-- end Main.hs
