-- | Main module for the blog's web server
module Main where

import           BlogDB              (initialBlogState)
import           Control.Applicative (pure, (<$>), (<*>))
import           Control.Exception   (bracket)
import           Data.Acid
import           Data.Acid.Remote
import           Data.Word           (Word16)
import           Locales             (version)
import           Network             (HostName, PortID (..))
import           Options
import           Server

data MainOptions = MainOptions {
  dbHost      :: String,
  dbPort      :: Word16,
  blogPort    :: Int,
  resourceDir :: String
}

instance Options MainOptions where
  defineOptions = pure MainOptions
    <*> simpleOption "dbHost" "localhost"
        "Remote acid-state database host. Default is localhost"
    <*> simpleOption "dbPort" 8070
        "Remote acid-state database port. Default is 8070"
    <*> simpleOption "blogPort" 8000
        "Port to serve the blog on. Default is 8000."
    <*> simpleOption "resourceDir" "/opt/tazblog/static"
        "Resources folder location."

main :: IO()
main = do
    putStrLn ("TazBlog " ++ version ++ " in Haskell starting")
    runCommand $ \opts _ ->
      let port = PortNumber $ fromIntegral $ dbPort opts
      in openRemoteState skipAuthenticationPerform (dbHost opts) port >>=
              (\acid -> runBlog acid (blogPort opts) (resourceDir opts))


