module Parser.Game where

import Text.ParserCombinators.Parsec
import qualified Parser.Elements as EP
import Parser.Logic (logic)
import AST.Design (Game(Game), Coord, getUpgrades)
import Parser.Helper (inline, antagNames, weaponNames, upgradeNames, digit')
import Parser.Language (reserved)

game :: Parser Game
game = do
    sP <- getPosition
    g <- grid
    _ <- inline sP
    e <- EP.elements
    l <- logic (antagNames e) (weaponNames e) (upgradeNames $ getUpgrades e)
    return $ Game g e l

grid :: Parser Coord
grid = do
    reserved "grid"
    x <- digit'
    spaces
    y <- digit'
    return $ (x,y)
