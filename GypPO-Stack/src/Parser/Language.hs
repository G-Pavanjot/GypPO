module Parser.Language(reserved) where

import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T
import Data.Functor.Identity
import qualified Text.Parsec.Prim as P

-- language definition
def :: LanguageDef g
def = emptyDef
  {
    reservedNames   = [
      "elements",
      "attack", "atkrange", "atkpattern",
      "melee",
      "ranged",
      "damage",
      "rof",
      "bullet",
      "bulletspeed",
      "weapon",
      "protag",
      "name",
      "movement",
      "direction",
      "speed",
      "size",
      "shape",
      "colour",
      "antag", "antags",
      "pattern",
      "spawn",
      "upgrades",
      "track",
      "health",
      "x",
      "y",
      "jump",
      "none",
      "logic",
      "base",
      "level",
      "using",
      "lanes",
      "enterFrom",
      "timeline",
      "timestamp",
      "count",
      "score",
      "time",
      "boss",
      "progression",
      "grid",
      "lives",
      "iP",
      "s",
      "m",
      "f",
      "vslow", "slow", "medium", "fast", "vfast",
      "no", "yes",
      "eol",
      "tri", "square", "rectangle",
      "platform", "platforms",
      "collectible",
      "add", "mult",
      "life",
      "still",
      "patrol",
      "start", "end",
      "follow",
      "left", "right", "top", "bottom",
      "lvlUpgrades",
      "target",
      "flying",
      "vee",
      "camera",
      "fixed",
      "one-way",
      "free-scroll"
    ]
  }

reserved :: String -> P.ParsecT String u Identity ()
reserved = T.reserved $ T.makeTokenParser def
