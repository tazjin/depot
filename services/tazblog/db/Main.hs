-- | Main module for the database server
module Main where

import           BlogDB              (initialBlogState)
import           Control.Applicative (pure, (<$>), (<*>))
import           Control.Exception   (bracket)
import           Data.Acid
import           Data.Acid.Local     (createCheckpointAndClose)
import           Data.Acid.Remote
import           Data.Word
import           Network             (PortID (..))
import           Options

data DBOptions = DBOptions {
  dbPort         :: Word16,
  stateDirectory :: String
}

instance Options DBOptions where
  defineOptions = pure DBOptions
    <*> simpleOption "dbport" 8070
        "Port to serve acid-state on remotely."
    <*> simpleOption "state" "/var/tazblog/state"
        "Directory in which the acid-state is located."

main :: IO ()
main = do
    putStrLn ("Launching TazBlog database server ...")
    runCommand $ \opts args ->
      bracket (openState opts) createCheckpointAndClose
              (acidServer skipAuthenticationCheck $ getPort opts)
  where
    openState o = openLocalStateFrom (stateDirectory o) initialBlogState
    getPort = PortNumber . fromIntegral . dbPort
