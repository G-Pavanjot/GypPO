module AST.Design where

import Data.List.NonEmpty (NonEmpty)

type Coord = (Nat, Nat)

-- Game data definition
data Game = Game {
  getGrid :: Coord,
  getElements :: Elements,
  getLogic :: Logic
} deriving (Show)

-- Logic data definition
data Logic = Logic {
  getLevels :: NonEmpty Level
} deriving (Show)

-- Grid data definition. Create a coordinate system
data Grid = Grid {
  getGridCoord :: Coord
} deriving (Show)

-- Level definition
data Level = Level {
    getCameraType :: CameraType,
    getPlatformList :: PlatformList,
    getLevelAntagsList :: Maybe AntagPlacements,
    getLevelUpgrades :: Maybe UpgradePlacements,
    getProtag :: Protag,
    getWinCondition :: WinCondition
} deriving (Show)

data CameraType 
  = Fixed
  | OneWay { getOWView :: Coord }
  | FreeScroll { getFSView :: Coord } deriving (Show)
  
data PlatformList = PlatformList {
    getPlatforms :: NonEmpty Platform
} deriving (Show)

 -- Absolute coords for now
data Platform = Platform {
    getPlatformCoord :: Coord,
    getPlatformSize :: Size,
    getPlatformColour :: Colour
} deriving (Show)

data AntagPlacements = AntagPlacements {
    getLevelAntags :: [AntagPlacement]
} deriving (Show)

data AntagPlacement = AntagPlacement {
    getLevelAntagName :: String,
    getAntagCoord :: Coord,
    getPatrolData :: Maybe PatrolData
} deriving (Show)

data PatrolData = PatrolData {
    getPoint1 :: Coord,
    getPoint2 :: Coord
} deriving (Show)

data UpgradePlacements = UpgradePlacements {
    getLevelUpgradePlacements :: [UpgradePlacement]
} deriving (Show)

data UpgradePlacement = UpgradePlacement {
    getLvlUpgradeName :: String,
    getUpgradeCoord :: Coord
} deriving (Show)

-- Player character definition
data Protag = Protag {
    getStart :: Coord,
    getJumpHeight :: Nat,
    getProtagSpeed :: Speed,
    getLives :: Nat,
    getHealth :: Nat,
    getColour :: Colour,
    getShape :: Shape,
    getWeapon :: Maybe String
} deriving (Show)

-- Elements of generated game
data Elements = Elements {
    getWeapons :: [Weapon],
    getAntags :: NonEmpty Antag,   
    getUpgrades :: Maybe Upgrades
} deriving (Show)

-- Enemies definition
data Antag =  Antag {
    getAntagName :: String,
    getAntagHealth :: Nat,
    getAntagWeapon :: String,
    getAntagIsFlying :: Bool,
    getAntagMovement :: Movement,
    getAntagColour :: Colour,
    getAntagSize :: Size,
    getAntagShape :: Shape,
    getAntagScore :: Nat
} deriving (Show)

data Weapon = Weapon {
    getWeaponName :: String,
    getWeaponDamage :: Nat,
    getWeaponColour :: Colour,
    getWeaponShape :: Shape,
    getWeaponType :: AttackType,
    getWeaponAtkRange :: Nat,
    getWeaponRoF :: Nat,
    getTargets :: Targets
} deriving (Show)

data Upgrades = Upgrades {
  getCollectibles :: [Collectible],
  getHealthUpgrades :: [HealthUpgrade],
  getLifeUpgrades :: [LifeUpgrade],
  getWeaponUpgrades :: [WeaponUpgrade]  
} deriving (Show)

data Collectible = Collectible {
    getCollectName :: String,
    getCollectScore :: Nat,
    getCollectColour :: Colour
} deriving (Show)

data HealthUpgrade = HealthUpgrade {
    getHealthName :: String,
    getHealthIncrease :: Nat,
    getHealthColour :: Colour
} deriving (Show)

data LifeUpgrade = LifeUpgrade {
    getLifeName :: String,
    getLifeIncrease :: Nat,
    getLifeColour :: Colour
} deriving (Show)

data WeaponUpgrade = WeaponUpgrade {
    getWeaponUpgradeName :: String,
    getNewWeaponName :: String,
    getWeaponUpgradeColour :: Colour
} deriving (Show)

data Targets
    = All --Every entity can be targeted
    | Characters -- Only protags and antags (characters) can be targeted
    | Bullets deriving (Show)   -- Only other bullets can be targets

data AttackPattern
    = Straight
    | Arc
    | VAtk
    | Homing deriving (Show)

data AttackType = Melee | Ranged {
    getBulletSpeed :: Speed,
    getBulletPattern :: AttackPattern
} deriving (Show)

-- Kill a boss, obtain a certain score, or reach the end of the level
data WinCondition = Boss String | Score Nat | EoL (Nat,Nat) deriving (Show) 

data Movement = Movement {
    getPattern :: MovementPattern,
    getTrack :: Bool,
    getV :: Bool, -- V movement is more of whether or not the entity jumps when hitting the ground while moving in its respective movement pattern (Can have patrolling antags with v movement and without)
    getSpeed :: Speed
} deriving (Show)

data Shape = Triangle | Square | Rectangle deriving (Show)

data Size = Small | Medium | Large deriving (Show)

data Speed = R | VS | S | M | F | VF deriving (Show)

data MovementPattern = Still | Patrol | Charge | Random deriving (Show)

data Colour = Colour { unClr :: String } deriving (Show)

newtype Nat = Nat { unNat :: Double } deriving (Show)

toNat :: Double -> Nat
toNat x
  | x < 0 = error "(-)ve nat"
  | otherwise = Nat x

fromNat :: Nat -> Double
fromNat (Nat x) = x
