module Parser.Logic where

import Parser.Helper (weaponNotFoundError, readStr, inline, readCoord, readNatWithType
  , bossNotFoundError, str, options, indented, readNat, quote, antagNotFoundError)
import Parser.Language (reserved)

import AST.Design as D
import Data.List.NonEmpty (NonEmpty((:|)))
import Text.ParserCombinators.Parsec
import qualified Parser.Elements as EP

logic :: [String] -> [String] -> [String] -> Parser D.Logic
logic antags bullets upgrades = do
    sP <- getPosition
    reserved "logic"
    _ <- indented sP
    _ <- getPosition
    l <- levels antags bullets upgrades
    return $ D.Logic l

levels :: [String] -> [String] -> [String] -> Parser (NonEmpty D.Level)
levels a b u = do
    (l:ls) <- many $ level a b u
    spaces
    return $ l :| ls

level :: [String] -> [String] -> [String] -> Parser D.Level
level a b u = do
    sP <- getPosition
    reserved "level"
    _ <- indented sP
    nP <- getPosition
    cam <- camera
    _ <- inline nP
    plats <- platformList
    ac <- antagCoords nP a
    uc <- upgradeCoords nP u
    _ <- inline nP
    p <- protag b
    _ <- inline nP
    wc <- boss a <|> score <|> eol
    spaces
    return $ D.Level cam plats ac uc p wc

camera :: Parser D.CameraType
camera = do
    sP <- getPosition
    reserved "camera"
    _ <- indented sP
    ct <- options [
        ("fixed", 0 :: Integer),
        ("one-way", 1),
        ("free-scroll", 2)]
    spaces
    case ct of
        0 -> return $ D.Fixed
        1 -> do
            _ <- indented sP
            xy <- readCoord
            return $ D.OneWay xy
        2 -> do
            _ <- indented sP
            xy <- readCoord
            return $ D.FreeScroll xy

platformList :: Parser D.PlatformList
platformList = do
    sP <- getPosition
    reserved "platforms"
    _ <- indented sP
    nP <- getPosition
    pl <- platforms nP
    return $ D.PlatformList pl

platforms :: SourcePos -> Parser (NonEmpty D.Platform)
platforms p = do
    (pl:pls) <- many $ platform p
    spaces
    return $ pl :| pls

--coordX coordY size colour
platform :: SourcePos -> Parser D.Platform
platform p = do
    _ <- inline p
    xy <- readCoord
    spaces
    s <- size'
    spaces
    clr <- EP.colour'
    spaces
    return $ D.Platform xy s clr

antagCoords :: SourcePos -> [String] -> Parser (Maybe D.AntagPlacements)
antagCoords n a = try $ do
    _ <- inline n
    sP <- getPosition
    reserved "antags"
    _ <- indented sP
    nP <- getPosition
    c <- option [] (lvlAntags nP a)
    return $ Just $ D.AntagPlacements c

lvlAntags :: SourcePos -> [String] -> Parser [D.AntagPlacement]
lvlAntags p a = try $ many $ lvlAntag p a

lvlAntag :: SourcePos -> [String] -> Parser D.AntagPlacement
lvlAntag p a = try $ do
    _ <- inline p
    nP <- getPosition
    n <- between quote quote (many alphaNum)
    spaces
    xy <- readCoord
    spaces
    pd <- option Nothing (patrolData nP)
    if elem n a
    then return $ D.AntagPlacement n xy pd
    else antagNotFoundError n

patrolData :: SourcePos -> Parser (Maybe D.PatrolData)
patrolData p = try $ do
    _ <- inline p
    sP <- getPosition
    reserved "patrol"
    _ <- indented sP
    nP <- getPosition
    reserved "start"
    p1 <- readCoord
    _ <- inline nP
    reserved "end"
    p2 <- readCoord
    return $ Just $ D.PatrolData p1 p2

upgradeCoords :: SourcePos -> [String] -> Parser (Maybe D.UpgradePlacements)
upgradeCoords n a = try $ do
    _ <- inline n
    sP <- getPosition
    reserved "lvlUpgrades"
    _ <- indented sP
    nP <- getPosition
    c <- option [] (lvlUpgrades nP a)
    return $ Just $ D.UpgradePlacements c

lvlUpgrades :: SourcePos -> [String] -> Parser [D.UpgradePlacement]
lvlUpgrades p u = try $ many $ lvlUpgrade p u

lvlUpgrade :: SourcePos -> [String] -> Parser D.UpgradePlacement
lvlUpgrade p _ = try $ do
    _ <- inline p
    n <- between quote quote (many alphaNum)
    spaces
    xy <- readCoord
    spaces
    return $ D.UpgradePlacement n xy

-- parse size without the reserved keyword
size' :: Parser D.Size
size' = try $ do
  s <- options [
    ("s", D.Small),
    ("m", D.Medium),
    ("l", D.Large)]
  spaces
  return s

protag :: [String] -> Parser D.Protag
protag b = do
    sP <- getPosition
    reserved "protag"
    _ <- indented sP
    nP <- getPosition
    strt <- spawn
    _ <- inline nP
    jumpH <- jumpHeight
    _ <- inline nP
    speed <- EP.speed
    _ <- inline nP
    l <- lives
    _ <- inline nP
    h <- health
    _ <- inline nP
    clr <- EP.colour
    _ <- inline nP
    shp <- EP.shape
    wpn <- option Nothing (weaponfind nP b)
    return $ D.Protag strt jumpH speed l h clr shp wpn

spawn :: Parser (D.Nat, D.Nat)
spawn = reserved "spawn" >> readCoord

jumpHeight :: Parser D.Nat
jumpHeight = readNat "jump"

lives :: Parser D.Nat
lives = readNat "lives"

health :: Parser D.Nat
health = readNat "health"

boss :: [String] -> Parser D.WinCondition
boss a = try $ do
    reserved "boss"
    b <- str
    spaces
    if elem b a
    then return $ D.Boss b
    else bossNotFoundError b

score :: Parser D.WinCondition
score = readNatWithType D.Score "score"

eol :: Parser D.WinCondition
eol = try $ do
    reserved "eol"
    xy <- readCoord
    return $ D.EoL xy

weaponfind :: SourcePos -> [String] -> Parser (Maybe String)
weaponfind p b = try $ do
    _ <- inline p
    wpnN <- readStr "weaponname"
    if elem wpnN b
    then return $ Just wpnN
    else weaponNotFoundError wpnN
