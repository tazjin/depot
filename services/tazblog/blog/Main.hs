-- | Main module for the blog's web server
module Main where

import Control.Applicative ((<$>), (<*>))
import Locales (version)
import Server (runBlog)
import System.Environment (getEnv)

data MainOptions
  = MainOptions
      { blogPort :: Int,
        resourceDir :: String
        }

readOpts :: IO MainOptions
readOpts =
  MainOptions
    <$> (fmap read $ getEnv "PORT")
    <*> getEnv "RESOURCE_DIR"

main :: IO ()
main = do
  putStrLn ("TazBlog " ++ version ++ " in Haskell starting")
  opts <- readOpts
  runBlog (blogPort opts) (resourceDir opts)
