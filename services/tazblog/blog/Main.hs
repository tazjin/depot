-- | Main module for the blog's web server
module Main where

import           Control.Applicative (pure, (<*>))
import           Control.Exception   (bracket)
import           Data.Word           (Word16)
import           Locales             (version)
import           Network             (HostName, PortID (..))
import           Options
import           Server

data MainOptions = MainOptions {
  blogPort    :: Int,
  resourceDir :: String
}

instance Options MainOptions where
  defineOptions = pure MainOptions
    <*> simpleOption "blogPort" 8000
        "Port to serve the blog on. Default is 8000."
    <*> simpleOption "resourceDir" "/opt/tazblog/static"
        "Resources folder location."

main :: IO()
main = do
    putStrLn ("TazBlog " ++ version ++ " in Haskell starting")
    runCommand $ \opts _ ->
      runBlog (blogPort opts) (resourceDir opts)
