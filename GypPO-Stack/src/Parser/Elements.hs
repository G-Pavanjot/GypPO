module Parser.Elements where

import Parser.Helper (options, weaponNotFoundError, readStr, inline, indented, readNat, makeColor
  , line, quote, digit')
import Parser.Language (reserved)

import Text.ParserCombinators.Parsec
import Data.List.NonEmpty (NonEmpty((:|)))
import AST.Design as D

elements :: Parser D.Elements
elements = do
    sP <- getPosition
    reserved "elements"
    _ <- indented sP
    nP <- getPosition
    b <- weapons
    _ <- inline nP
    (a:as) <- antags (map D.getWeaponName b)
    c <- option Nothing (upgrades (map D.getWeaponName b) nP)
    return $ D.Elements b (a :| as) c

weapons :: Parser [D.Weapon]
weapons = many weapon

weapon :: Parser D.Weapon
weapon = do
    sP <- getPosition
    reserved "weapon"
    _ <- indented sP
    nP <- getPosition
    n <- name
    _ <- inline nP
    dmg <- damage
    _ <- inline nP
    clr <- colour
    _ <- inline nP
    shp <- shape
    _ <- inline nP
    atktyp <- attacktype
    _ <- inline nP
    atk <- atkrange
    _ <- inline nP
    rof <- rateoffire
    _ <- inline nP
    targs <- targets
    return $ D.Weapon n dmg clr shp atktyp atk rof targs

antags :: [String] -> Parser [D.Antag]
antags b = many $ antag b

antag :: [String] -> Parser D.Antag
antag b = do
    sP <- getPosition
    reserved "antag"
    _ <- indented sP
    nP <- getPosition
    n <- name
    _ <- inline nP
    hlth <- health
    _ <- inline nP
    atkwpn <- weaponfind b
    _ <- inline nP
    fly <- flying
    _ <- inline nP
    mvmnt <- movement
    _ <- inline nP
    clr <- colour
    _ <- inline nP
    siz <- size
    _ <- inline nP
    shp <- shape
    _ <- inline nP
    scr <- score
    spaces
    return $ D.Antag n hlth atkwpn fly mvmnt clr siz shp scr

upgrades :: [String] -> SourcePos -> Parser (Maybe D.Upgrades)
upgrades weaponNames n = try $ do
    _ <- inline n
    sP <- getPosition
    reserved "upgrades"
    _ <- indented sP
    nP <- getPosition
    c <- option [] (collectibles nP)
    h <- option [] (healthUpgrades nP)
    l <- option [] (lifeUpgrades nP)
    w <- option [] (weaponUpgrades weaponNames nP)
    return $ Just $ D.Upgrades c h l w

collectibles :: SourcePos -> Parser [D.Collectible]
collectibles p = try $ many $ collectible p

collectible :: SourcePos -> Parser D.Collectible
collectible p = try $ do
    _ <- inline p
    reserved "collectible"
    n <- between quote quote (many alphaNum)
    spaces
    scr <- digit'
    spaces
    clr <- colour'
    spaces
    return $ D.Collectible n scr clr

healthUpgrades :: SourcePos -> Parser [D.HealthUpgrade]
healthUpgrades p = try $ many $ healthUpgrade p

healthUpgrade :: SourcePos-> Parser D.HealthUpgrade
healthUpgrade p = try $ do
    _ <- inline p
    reserved "health"
    n <- between quote quote (many alphaNum)
    spaces
    hlth <- digit'
    spaces
    clr <- colour'
    spaces
    return $ D.HealthUpgrade n hlth clr

lifeUpgrades :: SourcePos -> Parser [D.LifeUpgrade]
lifeUpgrades p = try $ many $ lifeUpgrade p

lifeUpgrade :: SourcePos -> Parser D.LifeUpgrade
lifeUpgrade p = try $ do
    _ <- inline p
    reserved "life"
    n <- between quote quote (many alphaNum)
    spaces
    lives <- digit'
    spaces
    clr <- colour'
    spaces
    return $ D.LifeUpgrade n lives clr

weaponUpgrades :: [String] -> SourcePos -> Parser [D.WeaponUpgrade]
weaponUpgrades weps p = try $ many $ weaponUpgrade weps p

weaponUpgrade :: [String] -> SourcePos -> Parser D.WeaponUpgrade
weaponUpgrade weps p = try $ do
    _ <- inline p
    reserved "weapon"
    n <- between quote quote (many alphaNum)
    spaces
    newW <- between quote quote (many alphaNum)
    spaces
    clr <- colour'
    spaces
    if elem newW weps
    then return $ D.WeaponUpgrade n newW clr
    else weaponNotFoundError newW 

attacktype :: Parser D.AttackType
attacktype = do
    sP <- getPosition
    at <- options [
        ("melee", 0 :: Integer),
        ("ranged", 1)]
    spaces
    if at == 0
        then return D.Melee
        else do
            _ <- indented sP
            nP <- getPosition
            (bspd, bptrn) <- rangeatk nP
            return $ D.Ranged bspd bptrn

rangeatk :: SourcePos -> Parser (D.Speed, D.AttackPattern)
rangeatk nP = do
    _ <- inline nP
    bspd <- speed
    _ <- inline nP
    bptrn <- bulletpattern
    return (bspd, bptrn)

rateoffire :: Parser D.Nat
rateoffire = readNat "rof"

targets :: Parser D.Targets
targets = do
    reserved "target"
    t <- options [
        ("all", D.All),
        ("weapons", D.Bullets),
        ("characters", D.Characters)]
    spaces
    return t

bulletpattern :: Parser D.AttackPattern
bulletpattern = do
    reserved "bulletpattern"
    p <- options [
        ("straight", D.Straight),
        ("arc", D.Arc),
        ("vatk", D.VAtk),
        ("homing", D.Homing)]
    spaces
    return p

score :: Parser D.Nat
score = readNat "score"

-- separate colour function without the reserved keyword
colour' :: Parser D.Colour
colour' = do
    c <- line
    return $ makeColor c

colour :: Parser D.Colour
colour = do
  reserved "colour"
  colour'

name :: Parser String
name = readStr "name"

-- parse size
size :: Parser D.Size
size = try $ do
  reserved "size"
  s <- options [
    ("s", D.Small),
    ("m", D.Medium),
    ("l", D.Large)]
  spaces
  return s

health :: Parser D.Nat
health = readNat "health"

shape :: Parser D.Shape
shape = try $ do
    reserved "shape"
    s <- options [
        ("tri", D.Triangle),
        ("square", D.Square),
        ("rectangle", D.Rectangle)]
    spaces
    return s

damage :: Parser D.Nat
damage = readNat "damage"

atkrange :: Parser D.Nat
atkrange = readNat "atkrange"

flying :: Parser Bool
flying = try $ do
    reserved "flying"
    t <- options $ [
        ("yes", True),
        ("no", False)]
    spaces
    return t

movement :: Parser D.Movement
movement = do
    sP <- getPosition
    reserved "movement"
    _ <- indented sP
    nP <- getPosition
    p <- pattern
    _ <- inline nP
    t <- track
    _ <- inline nP
    v <- vMove
    _ <- inline nP
    s <- speed
    return $ D.Movement p t v s

weaponfind :: [String] -> Parser String
weaponfind b = do
    wpnN <- readStr "weaponname"
    if elem wpnN b
    then return $ wpnN
    else weaponNotFoundError wpnN

speed :: Parser D.Speed
speed = try $ do
    reserved "speed"
    s <- options [
        ("rest", D.R),
        ("vslow", D.VS),
        ("slow", D.S),
        ("medium", D.M),
        ("fast", D.F),
        ("vfast", D.VF)]
    spaces
    return s

track :: Parser Bool
track = try $ do
    reserved "track"
    t <- options $ [
        ("yes", True),
        ("no", False)]
    spaces
    return t

vMove :: Parser Bool
vMove = try $ do
    reserved "vee"
    t <- options $ [
        ("yes", True),
        ("no", False)]
    spaces
    return t

pattern :: Parser D.MovementPattern
pattern = do
    reserved "pattern"
    p <- options [
        ("still", D.Still),
        ("patrol", D.Patrol),
        ("charge", D.Charge),
        ("random", D.Random)]
    spaces
    return p
