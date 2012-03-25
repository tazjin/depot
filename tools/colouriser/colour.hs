{-# LANGUAGE TemplateHaskell #-}

import Control.Monad (unless)
import Language.Haskell.HsColour.Colourise (defaultColourPrefs)
import Language.Haskell.HsColour.CSS
import Options

defineOptions "MainOptions" $ do
	stringOption "optFile" "file" ""
		"Name of the .hs file. Will be used for the HTML file as well"

colorCode :: String -> String -> IO ()
colorCode input output = do
	code <- readFile input
	writeFile output $ hscolour False code

main :: IO ()
main = runCommand $ \opts args -> do
	let file = optFile opts
	unless (file == "") $
		colorCode (file ++ ".hs") (file ++ ".html")