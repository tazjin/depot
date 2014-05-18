module Main where

import           Control.Applicative          (pure, (<$>), (<*>))
import           Control.Exception            (bracket)
import           Data.Acid
import           Data.Acid.Local (createCheckpointAndClose)
import           Options

import           BlogDB                       (initialBlogState)
import           Locales                      (version)
import           Server

{- Server -}

data MainOptions = MainOptions {
  optState :: String,
  optPort  :: Int,
  optRes   :: String
}

instance Options MainOptions where
  defineOptions = pure MainOptions
    <*> simpleOption "statedir" "/var/tazblog/"
        "Directory in which the BlogState is located."
    <*> simpleOption "port" 8000
        "Port to run on. Default is 8000."
    <*> simpleOption "res" "/usr/share/tazblog/res"
        "Resources folder location."
        
main :: IO()
main = do
    putStrLn ("TazBlog " ++ version ++ " in Haskell starting")
    runCommand $ \opts args ->
      bracket (openLocalStateFrom (optState opts ++ "BlogState") initialBlogState)
              createCheckpointAndClose
              (\acid -> runBlog acid (optPort opts) (optRes opts))


