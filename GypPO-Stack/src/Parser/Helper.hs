module Parser.Helper (readStr, digit', quote, line, makeColor, readNat, indented, inline,
  options, weaponNotFoundError, readCoord, readNatWithType, bossNotFoundError,
  str, antagNotFoundError,
  antagNames, weaponNames, upgradeNames) where

-- Module adapted from Helpers.hs from MAKU

import Parser.Language (reserved)
import Text.ParserCombinators.Parsec
import qualified Data.Colour.SRGB as SRGB
import Data.Colour.Names
import Data.Functor.Identity (Identity)
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (toList)

import qualified Text.Parsec.Prim as P
import AST.Design (Nat, Elements(..), Coord, Upgrades(..), Colour(..),
  getAntagName, getWeaponName, getCollectName, toNat)

-- From LogicParser of MAKU (moved to helper)
readStr :: String -> Parser String
readStr n = try $ do
  reserved n
  s <- str
  spaces
  return s

readNat :: String -> Parser Nat
readNat n = try $ do
  reserved n
  d <- digit'
  spaces
  return d

readNatWithType :: (Nat -> a) -> String -> Parser a
readNatWithType t n = try $ do
  reserved n
  d <- digit'
  spaces
  return $ t d

readCoord :: Parser Coord
readCoord = try $ do
    x <- digit'
    spaces
    y <- digit'
    return (x,y)

-- quote definition
quote :: Parser String
quote = string "\""

antagNames :: Elements -> [String]
antagNames = map getAntagName . toList . getAntags

weaponNames :: Elements -> [String]
weaponNames = map getWeaponName . getWeapons

upgradeNames :: Maybe Upgrades -> [String]
upgradeNames = maybe [] (\u -> map getCollectName (getCollectibles u))

str :: Parser String
str = try $ do
  s <- between quote quote (many alphaNum)
  spaces
  return $ s

natify :: String -> Nat
natify = toNat . read

readColor :: String -> SRGB.Colour Double
readColor color = fromMaybe (error $ "invalid color: " ++ color) $ readColourName color

makeColor :: String -> Colour
makeColor = Colour . SRGB.sRGB24show . readColor

-- get line
line :: Parser String
line = do
 ln <- manyTill anyChar newline
 spaces
 return ln

-- parse digits
digit' :: Parser Nat
digit' = do
  d <- many1 digit
  spaces
  return $ natify d

-- indent
indented :: Show tok => SourcePos -> P.ParsecT [tok] u Data.Functor.Identity.Identity [a]
indented p = (eof >> return []) <|> do
  innerPos <- getPosition
  if sameIndent p innerPos then pzero else return []

-- options
options :: [(String,a)] -> Parser a
options l = choice $ map (\(s,r) -> do { _ <- reserved s; return r }) l

-- check if columns are inline
inline :: SourcePos -> P.ParsecT [tok] u Identity [a]
inline p = do
  a <- getPosition
  if sameIndent a p then return [] else pzero

-- check for same indent
sameIndent :: SourcePos -> SourcePos -> Bool
sameIndent a b = (sourceColumn a) == (sourceColumn b)

-- Errors in DSL
notFoundError :: P.Stream s m t => String -> String -> P.ParsecT s u m a
notFoundError x a = unexpected ("--> the \"" ++ x ++ "\": \"" ++ a ++ "\" does not exist.")

weaponNotFoundError :: P.Stream s m t => String -> P.ParsecT s u m a
weaponNotFoundError = notFoundError "weapon"

bossNotFoundError :: P.Stream s m t => String -> P.ParsecT s u m a
bossNotFoundError = notFoundError "boss"

antagNotFoundError :: P.Stream s m t => String -> P.ParsecT s u m a
antagNotFoundError = notFoundError "antag"
