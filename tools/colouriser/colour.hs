{-# LANGUAGE TemplateHaskell #-}

import Control.Monad (unless)
import Language.Haskell.HsColour.Colourise (defaultColourPrefs)
import Language.Haskell.HsColour.CSS
import Options

defineOptions "MainOptions" $ do
	stringOption "optFile" "file" ""
		"Name of the .hs file. Will be used for the HTML file as well"

colorCode :: String -> IO ()
colorCode input = do
	code <- readFile input
	putStr $ concat [ "<div class=\"code\">"
		     		, hscolour False code
				    , "</div>"
				    ]

main :: IO ()
main = runCommand $ \opts args -> do
	let file = optFile opts
	unless (file == "") $
		colorCode file