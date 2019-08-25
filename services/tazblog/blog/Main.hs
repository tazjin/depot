-- | Main module for the blog's web server
module Main where

import Control.Applicative ((<$>), (<*>))
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
  opts <- readOpts
  putStrLn ("tazblog starting on port " ++ (show $ blogPort opts))
  runBlog (blogPort opts) (resourceDir opts)
