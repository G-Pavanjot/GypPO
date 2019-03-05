module Gen where

import Text.ParserCombinators.Parsec (parse)
import qualified Text.PrettyPrint.Leijen as L
import qualified Plat_Pretty as PP

import Parser.Game (game)

gen :: String -> IO ()
gen file = do
    f <- readFile file
    case (parse game "" f) of
        Right x -> do
            writeFile "javascript/test.js" (show (L.pretty $ PP.enit x))
            putStrLn $ "success"
        _ -> putStrLn $ "fail"
