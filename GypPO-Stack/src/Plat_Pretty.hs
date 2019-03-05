module Plat_Pretty where

import Data.List.NonEmpty (toList)
import Translation_Helpers
import AST.JS
import AST.Design

-- System libraries
import Prelude hiding (GT, or, and, div)

grid :: Coord -> JSStatement
grid (Nat x, Nat y) = statement enviro gridP (invoke' [dbl x, dbl y])

elements :: Elements -> (JSVarStatement, JSVarStatement, JSVarStatement)
elements e = (w, a, u)
    where
        w = weapons (getWeapons e)
        a = antags (toList $ getAntags e)
        u = upgrades (getUpgrades e)

weapons :: [Weapon] -> JSVarStatement
weapons w = var weaponDataNm (array $ map weapon w)

weapon :: Weapon -> JSExpression
weapon w = 
    object
    [
        field name           (exstr $ getWeaponName w),
        field damage         (dbl $ unNat $ getWeaponDamage w),
        field shape          (getShapeExpr (getWeaponShape w) 0.004 (getWeaponColour w)),
        field attack_type    (attackType $ getWeaponType w),
        field range          (dbl $ unNat $ getWeaponAtkRange w),
        field rateofFire     (dbl $ unNat $ getWeaponRoF w),
        field targets        (targetType $ getTargets w)
    ]

targetType :: Targets -> JSExpression
targetType t = case t of
  All -> entityStr
  Bullets -> weaponStr
  Characters -> character

attackType :: AttackType -> JSExpression
attackType w = case w of
  Melee -> melee
  Ranged rspd rpat -> object
    [
      field bulletSpeed      (movementSpeed $ rspd),
      field bulletPattern    (attackPattern $ rpat)
    ]

attackPattern :: AttackPattern -> JSExpression
attackPattern ap = case ap of
  Straight    -> straight
  Arc         -> arc
  VAtk        -> vatk
  Homing      -> homing

antags :: [Antag] -> JSVarStatement
antags a = var antagDataNm (array $ map antag a)

antag :: Antag -> JSExpression
antag a =
    object
        [
            field name       (exstr $ getAntagName a),
            field livesNm    (dbl 1), -- Be consistent with the other entity in game (the protagonist)
            field health     (dbl $ unNat $ getAntagHealth a),
            field weaponNm   (exstr $ getAntagWeapon a),
            field flying     (exbool $ getAntagIsFlying a),
            field movement   (antagMovement $ getAntagMovement a),
            field shape      (getShapeExpr (getAntagShape a) (antagSize $ getAntagSize a) (getAntagColour a)),
            field score      (dbl $ unNat $ getAntagScore a)
        ]

antagMovement :: Movement -> JSExpression
antagMovement m = 
  object
  [
    field pattern          (movementPattern $ getPattern m),
    field track            (exbool $ getTrack m),
    field vee              (exbool $ getV m),
    field speed            (movementSpeed $ getSpeed m)       
  ]

movementPattern :: MovementPattern -> JSExpression
movementPattern mp = case mp of
  Still   -> still
  Patrol  -> patrol
  Charge  -> charge
  Random  -> random

-- Movement speed options are based on how much of the entire game window to cover in a second (0% - 25% of the game screen)
movementSpeed :: Speed -> JSExpression
movementSpeed ms = numCells $ dbl $ case ms of
  R    -> 0.0
  VS   -> 0.05
  S    -> 0.1
  M    -> 0.15
  F    -> 0.2
  VF   -> 0.25
  where
    numCells = excond Mul (arrayVal gridExpr (dbl 0))

upgrades :: Maybe Upgrades -> JSVarStatement
upgrades = maybe noUpgrades (\u -> (var upgradeDataNm (object  [collectibles (getCollectibles u), healthUpgrades (getHealthUpgrades u),
  lifeUpgrades (getLifeUpgrades u), weaponUpgrades (getWeaponUpgrades u)])))

noUpgrades :: JSVarStatement
noUpgrades = var upgradeDataNm (object [])

collectibles :: [Collectible] -> JSObjectField
collectibles c = field collectiblesNm (array $ map collectible c)

collectible :: Collectible -> JSExpression
collectible c = 
    object
    [
        field name           (exstr $ getCollectName c),
        field score          (dbl $ unNat $ getCollectScore c),
        field colour         (exstr $ clr $ getCollectColour c)
    ]

healthUpgrades :: [HealthUpgrade] -> JSObjectField
healthUpgrades h = field healthUpgradesNm  (array $ map healthUpgrade h)

healthUpgrade :: HealthUpgrade -> JSExpression
healthUpgrade h = 
    object
    [
        field name           (exstr $ getHealthName h),
        field health         (dbl $ unNat $ getHealthIncrease h),
        field colour         (exstr $ unClr $ getHealthColour h)
    ]

lifeUpgrades :: [LifeUpgrade] -> JSObjectField
lifeUpgrades l = field lifeUpgradesNm  (array $ map lifeUpgrade l)

lifeUpgrade :: LifeUpgrade -> JSExpression
lifeUpgrade l = 
    object
    [
        field name           (exstr $ getLifeName l),
        field livesNm        (dbl $ unNat $ getLifeIncrease l),
        field colour         (exstr $ unClr $ getLifeColour l)
    ]

weaponUpgrades :: [WeaponUpgrade] -> JSObjectField
weaponUpgrades w = field weaponUpgradesNm  (array $ map weaponUpgrade w)

weaponUpgrade :: WeaponUpgrade -> JSExpression
weaponUpgrade w = 
    object
    [
        field name           (exstr $ getWeaponUpgradeName w),
        field newWeapon      (exstr $ getNewWeaponName w),
        field colour         (exstr $ unClr $ getWeaponUpgradeColour w)
    ]

logic :: Logic -> JSVarStatement
logic = levels . toList . getLevels

levels :: [Level] -> JSVarStatement
levels ls = var lvlsNm (array $ map level ls)

level :: Level -> JSExpression
level l = object [cam, pl, a, u, p, w]
    where
        cam = camera (getCameraType l)
        pl = platformList (getPlatformList l)
        a = antagList (getLevelAntagsList l)
        u = upgradeList (getLevelUpgrades l)
        p = protag (getProtag l)
        w = winCondition $ getWinCondition l

camera :: CameraType -> JSObjectField
camera ct =  field cameraNm $ case ct of
  Fixed -> fixed
  OneWay view -> object
    [
      field typeNm onewayCam,
      field viewNm (jsCoord view)
    ]
  FreeScroll view -> object
    [
      field typeNm freeCam,
      field viewNm (jsCoord view)
    ]

platformList :: PlatformList -> JSObjectField
platformList p = platforms (toList $ getPlatforms p)

platforms :: [Platform] -> JSObjectField
platforms p = field lvlplats     (array $ map platform p)

platform :: Platform -> JSExpression
platform p = 
    object
    [
        field coords         (jsCoord $ getPlatformCoord p),
        field sizeNm         (dbl $ size $ getPlatformSize p),
        field colour         (exstr $ clr clrVal)
    ]
    where
      clrVal = getPlatformColour p

antagList :: Maybe AntagPlacements -> JSObjectField
antagList = maybe noAntags (antagsP . getLevelAntags)

noAntags :: JSObjectField
noAntags = field lvlants           (object [])

antagsP :: [AntagPlacement] -> JSObjectField
antagsP a = field lvlants          (array $ map antagP a)

antagP :: AntagPlacement -> JSExpression
antagP a =
    object
    [
        field name              (exstr $ getLevelAntagName a),
        field coords            (jsCoord $ getAntagCoord a),
        field movementData      (patrolData $ getPatrolData a)
    ]

patrolData :: Maybe PatrolData -> JSExpression
patrolData = maybe (object []) (\pd -> patrolDataObj (getPoint1 pd) (getPoint2 pd))

patrolDataObj :: Coord -> Coord -> JSExpression
patrolDataObj p1 p2 =
  object
  [
    field start           (jsCoord p1),
    field end             (jsCoord p2)
  ]

upgradeList :: Maybe UpgradePlacements -> JSObjectField
upgradeList = maybe nolvlUpgrades (\a -> upgradesP (getLevelUpgradePlacements a))

nolvlUpgrades :: JSObjectField
nolvlUpgrades = field lvlupgrds (object [])

upgradesP :: [UpgradePlacement] -> JSObjectField
upgradesP c = field lvlupgrds (array $ map upgradeP c)

upgradeP :: UpgradePlacement -> JSExpression
upgradeP c =
    object
    [
        field name               (exstr $ getLvlUpgradeName c),
        field coords             (jsCoord $ getUpgradeCoord c)
    ]

protag :: Protag -> JSObjectField
protag p = fieldWithObject "protag"
    [
        ("initPos", jsCoord $ getStart p),
        ("jumpHeight", dbl $ unNat $ getJumpHeight p),
        ("speed", movementSpeed $ getProtagSpeed p),
        ("lives",   dbl $ unNat $ getLives p),
        ("health", dbl $ unNat $ getHealth p),
        ("shape",   getShapeExpr (getShape p) 0.025 (getColour p)),
        ("weapon",  protagWeapon $ getWeapon p)
    ]

protagWeapon :: Maybe String -> JSExpression
protagWeapon = maybe noWep exstr

winCondition  :: WinCondition -> JSObjectField
winCondition wc = fieldWithObject "winCondition" $ case wc of
  Boss b -> [ ("boss", exstr b) ]
  Score (Nat s) -> [ ("score", dbl s) ]
  EoL c -> [ ("eol", jsCoord c) ]

getShapeExpr :: Shape -> Double -> Colour -> JSExpression
getShapeExpr shp db clrVal =
  case shp of
    Triangle -> triangle sizeVal $ clr clrVal
    Square -> rectangle sizeVal sizeVal $ clr clrVal
    Rectangle -> rectangle sizeVal (sizeVal `mul` (dbl 0.5)) $ clr clrVal
  where
    sizeVal =  (arrayVal gridExpr (dbl 1)) `mul` (dbl db)

triangle :: JSExpression -> String -> JSExpression
triangle sL clrVal =
    object
    [
        field shapeType          (dbl 1),
        field lengthNm           sL,
        field colour             (exstr clrVal)
    ]

rectangle :: JSExpression -> JSExpression -> String -> JSExpression
rectangle h w clrVal =
    object
    [
        field shapeType          (dbl 0),
        field height             h,
        field width              w,
        field colour             (exstr clrVal)
    ]

clr :: Colour -> String
clr = unClr

-- Platform is 1,2 or 3 grid units
size :: Size -> Double
size Small = 1
size Medium = 2
size Large = 3

antagSize :: Size -> Double
antagSize siz = case siz of
  Small  -> 0.01
  Medium -> 0.025
  Large  -> 0.04

hudElements :: [(String, JSExpression)] -> JSVarStatement
hudElements elems = var hudElementsNm (array $ map hudElement elems)

hudElement :: (String, JSExpression) -> JSExpression
hudElement (text, func) =
  object
  [
    field txt           (exstr text),
    field functionNm           func
  ]

-- Create a very thin (invisible) shape for the bottom of the level
bottomShape :: JSExpression -> JSVarStatement
bottomShape w = var bttmShp (object
    [
        field shapeType          (dbl 0),
        field height             (dbl 0.00001),
        field width              w,
        field colour             (exstr "yellow")
    ])

-- Descrease the player's lives by 1, respawn the player, and destroy the "killed" playable entity
entityRespawn :: JSName -> [JSStatement]
entityRespawn varName =
  [
    statement varName setlives (invoke' [excond Sub lives (dbl 1)]),
    statement varName spawn (invoke' []),
    enviroHudUpdate,
    craftyTrigger "CameraAnimationDone"
  ]

-- 4.5 seems like a random number, but the jump speed value is the number of pixels to move per second. 
-- The jump stat is based on how many grid cells the user specified for the player to jump. 
-- Thus, if the speed was equal to that number, the jump would appear to slow in game.
getCraftyJumpSpeed :: JSExpression
getCraftyJumpSpeed =  (dbl 4.5) `mul` (getCellHeight `mul` (getField (selfExpr, "jumpHeight")))

-- Takes the crafty object's speed as expression and multiply with the cell width to get actual speed value
getCraftyMovementSpeed :: JSExpression -> JSExpression
getCraftyMovementSpeed spd = spd `mul` getCellWidth

-- In order to make any crafty entity playable, two lines of code need to be executed once the entity is created
-- The crafty component "Twoway" needs to be added to the entity which provides keyboard inputs to manipulate the entity
-- The twoway function then alters speed of the entity when they move
playable :: JSName -> [JSStatement]
playable jsname = [
  craftyAddComponent jsname [twoWayComponent, player, character],
  statement jsname twoway (invoke' [getCraftyMovementSpeed (getField (selfExpr, "speed")), getCraftyJumpSpeed]), -- number should be a variable based on speed of entity, not a fixed 100
  statement jsname bindProp (invoke' [keyDown, keyboardFunction])               
  ]

-- Keyboard function to create
-- if (e.key == Crafty.keys.SPACE) {check if the weapon can fire}
-- if (e.key == Crafty.keys.A || Crafty.keys.LEFT_ARROW) {player.facing = "left"}
-- if (e.key == Crafty.keys.D || Crafty.keys.RIGHT_ARROW) {player.facing = "right"}
keyboardFunction :: JSExpression
keyboardFunction = function "" [keyE]
  (functionBody [] [
    ifStatement (checkIfKeyPressed "SPACE") (
      functionBody [] [statement' checkIfFired (invoke' [playerExpr, getField (selfExpr, "weapon")])]) Nothing,
    ifStatement ((checkIfKeyPressed "A") `or` (checkIfKeyPressed "LEFT_ARROW")) (
      setPlayerFacing "left") Nothing,
    ifStatement ((checkIfKeyPressed "D") `or` (checkIfKeyPressed "RIGHT_ARROW")) (
      setPlayerFacing "right") Nothing
  ])

-- Requires engine function Environment.getRealCoords which outputs the actual screen coordinates based on the relative grid coordinates provided
-- This line of code is required in a lot of places so having this function saves on duplication
engineCoordinates :: [JSExpression] -> JSExpression
engineCoordinates exprs = stmtAsExpr enviro getRealCoords (invoke' exprs)

livesGT :: JSExpression
livesGT = excond GT lives $ dbl 1

destroyColliderObject :: JSName -> JSFunctionBody
destroyColliderObject varName = functionBody [] ([
  statement collider detach (invoke' []),
  craftyDestroy collider,
  craftyDestroy objName] ++
  entityRespawn varName)

updateScoreFunc :: JSName -> JSStatement
updateScoreFunc jsname = statement gameStats updateScore (invoke' [stmtAsExpr jsname getscore (invoke' [colliderNameExpr])])

updateLives :: TypeofEntity -> JSName -> JSStatement
updateLives entType varName = case entType of
  ProEntity -> ifStatement livesGT (destroyColliderObject varName) (Just $ Left $ craftyGameOver)
  AntEntity  -> ifStatement livesGT (destroyColliderObject varName) (Just $ Left $ functionBody [] 
      ([updateScoreFunc antagonists,
      craftyDestroy objName] ++ updateCheckDestroy))

checkWhoFired :: JSExpression
checkWhoFired = excond NotEq (getWhoFired objExpr) (exnm' collider)

getWhoFired :: JSExpression -> JSExpression
getWhoFired obj = getField (obj, "whoFired")

getHealthExpression :: JSName -> [JSExpression] -> JSExpression
getHealthExpression varName invExprs = stmtAsExpr varName gethealth $ invoke' invExprs

getLivesExpression :: JSName -> [JSExpression] -> JSExpression
getLivesExpression varName invExprs = stmtAsExpr varName getlives $ invoke' invExprs

-- if the colliding entity has the tag weapon, it should decrease the health by the weapon's damage value
-- Also check to make sure that the weapon isn't the same one that this entity fired in the first place
-- updateEntityHealth "Protagonist" or updateEntityHealth "antagEntity"
updateEntityHealth :: TypeofEntity -> JSName -> JSFunctionBody
updateEntityHealth entType varName = functionBody [
              var livesNm $ getLivesExpression varName [],
              var updatedHlth ((getHealthExpression varName []) `sub` (stmtAsExpr weaponName getdamage (invoke' [objNmExpr])))] 
              [
                ifStatement ((exnm' updatedHlth) `lte` (dbl 0)) (functionBody [] [updateLives entType varName]) (Just $ Left $ functionBody [] [
                  statement varName sethealth (invoke' [exnm' updatedHlth]),
                  enviroHudUpdate,
                  craftyDestroy objName])
              ]

-- Create an Antag entity for the antags needed in each level
-- This will be an instance of the antagonist defined in the elements portion of the GYPPO file 
-- This instance is created so when enemies are attacked, only that specific entity's health is updated, not every antagonist of that type
-- Example: if there are 2 of the same type of enemies, this way prevents both of them from being destroyed when only one is attacked
antagProto :: [JSVarStatement]
antagProto = 
  ([
    var antagNm (function "" [nmeNm, initP]
      (
        functionBody
        ([
          self,
          selfField "name" nme,
          var health $ getHealthExpression antagonists [nme],
          selfField "movement" $ stmtAsExpr antagonists getmovement (invoke' [nme])
        ] ++ protoLinks)[]
      )
    ),

    protoVarStmt antagNm constructor $ exnm' antagNm,
    protoVarStmt antagNm getMoveDataNm (function "" [objName] (functionBody 
      [
        self,
        var i (extern (objectHasField objName "start") (object [
          field start (engineCoordinates [getField (objExpr, "start")]),
          field end (engineCoordinates [getField (objExpr, "end")])]) objExpr),
        var movement (getField (selfExpr, "movement")),
        selfField "movement" (object 
        [
        field pattern   (new (getField (movementExpr, "pattern")) []),
        field speed     (getCraftyMovementSpeed (getField (movementExpr, "speed"))),
        field track     (getField (movementExpr, "track")),
        field targets   player,
        field vee       (getField (movementExpr, "v")),
        field dataSet   i'
        ])
      ]
      [
      returnStatement (getField (selfExpr, "movement"))
      ]
    )),

    protoVarStmt antagNm spawnNm (function "" []     (functionBody
      [
      self,
      selfField "health" $ getField (selfExpr, "initialHealth"),
      var shape (stmtAsExpr antagonists getshape (invoke' [getSelfField "name"])),
      var newEntNm (paramInvk "createEntity" [getSelfField "initialPos", shpExpr, getSelfField "flying"]),
      var wpnName  $ getSelfField "weapon",
      setWeaponTrigger,
      setField (newEnt, "entity") thisExpr,
      setField (newEnt, "movement") $ getSelfField "movement"
      ]
      [
      setNameStmt newEntNm [getSelfField "name"],
      craftyAddComponent newEntNm [antagStr, character]
      ]
    ))

  ] ++ concat protoGetterSetter)
  where
    (protoLinks, protoGetterSetter) = unzip ([protoLinkGetSet antagNm "initialHealth" healthExpr] ++ [protoLinkGetSet antagNm "health" healthExpr] ++
      [protoLinkGetSet antagNm "lives" $ getLivesExpression antagonists [nme]] ++
      [protoLinkGetSet antagNm "initialPos" initPExpr] ++
      [protoLinkGetSet antagNm "flying" $ stmtAsExpr antagonists getflying (invoke' [nme])] ++
      [protoLinkGetSet antagNm "weapon" $ stmtAsExpr antagonists getweapon (invoke' [nme])])

collisionComments :: JSVarStatement
collisionComments = comment (exstr "Collision breakdown: when an entity comes into contact with another entity,\n\
  \a separate function is called which will require the initial entity as well as the list of objects which have collided with the entity.\n\
  \This function needs to determine what entity collided with the object calling this function and respond accordingly (reduce health or destory)\n\
  \Basic Steps of this dispatch function:\n\
  \1. Recieve the information about every object colliding with the entity\n\
  \2. Determine what these other objects are (antagonists, protagonists, weapons)\n\
  \ \t 2.1 When each entity is created use the crafty function setName to link the engine entity to its respective game element\n\
  \ \t 2.2 When a collision occurs, just use the crafty function getName in order to know exactly which game element collided with the entity\n\
  \3. If the antagonist/weapons collide with the protagonist, reduce the protagonist's health based on damage done\n\
  \ \n\
  \update Antagonist health/lives based on if they are attacked by player\n\
  \Make 2 dispatch functions instead of a bunch of repeating conditionals for if the original object is a weapon or protag\n\
  \Like dispatchFunction(object, hitData) {\n\
  \ \tif (protag) {\n\
  \ \tdispatchFuncProtag(object, hitData); This will be the function coded below\n\
  \ \t} else{\n\
  \ \tdispatchFuncWeapon(object, hitData);\n\
  \ \t}\n\
\}\n\
\ Weapon collision function is the primary one which updates health/other stats when colliding with entities in the game")

-- Three lines of code found regularily in the collision functions
-- Step 1: update HUD
-- Step 2: check if the win condition has been met
-- Step 3: destroy the colliding object.
updateCheckDestroy :: [JSStatement]
updateCheckDestroy = [enviroHudUpdate,
              checkWinCond,
              craftyDestroy collider]

checkUpgradeCollision :: TypeOfUpgrades -> JSStatement
checkUpgradeCollision cc = case cc of
  Collect -> ifStatement (craftyHas collider "collectibles") (functionBody [] ([
              updateScoreFunc upgradeFuncNm] ++ updateCheckDestroy)) Nothing

  Health -> ifStatement (craftyHas collider "healthUpgrades") (functionBody [
    var updatedHlth ((getHealthExpression protagonist []) `add` (getHealthExpression upgradeFuncNm [colliderNameExpr]))] ([
              statement protagonist sethealth (invoke' [exnm "updatedHlth"])] ++ updateCheckDestroy)) Nothing

  Life -> ifStatement (craftyHas collider "lifeUpgrades") (functionBody [
    var updatedLife ((getLivesExpression protagonist []) `add` (getLivesExpression upgradeFuncNm [colliderNameExpr]))] ([
              statement protagonist setlives (invoke' [exnm "updatedLife"])] ++ updateCheckDestroy)) Nothing

  -- Remaining upgrade is setting new weapon for player (room for adding additional upgrades)
  _ -> ifStatement (craftyHas collider "weaponUpgrades") (functionBody [] ([
              statement protagonist setweapon (invoke' [stmtAsExpr upgradeFuncNm getnewWeapon (invoke' [colliderNameExpr])])] ++ updateCheckDestroy)) Nothing

protagCollision :: JSVarStatement
protagCollision =
  var dispatchFunctionPro (function "" [objName, dataSet]
    (functionBody [] [
        forLoop i dataSetExpr (functionBody (getCraftyColliderData ++ [
          var livesNm $ getLivesExpression protagonist []])
          [
            ifStatement ((craftyHas collider "Antag") `or` (craftyHas collider "Bottom")) (functionBody []
              [
                ifStatement livesGT (functionBody [] ([statement objName detach (invoke' []),
                craftyDestroy objName] ++ (entityRespawn protagonist))) (Just $ Left $ craftyGameOver)
              ]) Nothing,

            checkUpgradeCollision Collect,
            checkUpgradeCollision Health,
            checkUpgradeCollision Life,
            checkUpgradeCollision NewWeapon,

            ifStatement (craftyHas collider "EOL") (functionBody [] [checkWinCond]) Nothing         
          ])
      ]))

weaponCollision :: JSVarStatement
weaponCollision =
  var dispatchFunctionWep (function "" [objName, dataSet]
    (functionBody
      [
        var objNm (stmtAsExpr objName getName (invoke' [])),
        var targets (stmtAsExpr weaponName gettargets (invoke' [objNmExpr]))
      ]
      [
        forLoop i dataSetExpr (functionBody (getCraftyColliderData ++ [
          var colliderEntity  (getField (exnm' collider, "entity"))])
        [
          ifStatement ((craftyHas collider "Antag") `and` checkWhoFired) (updateEntityHealth AntEntity colliderEntity) (Just $ Right $
          (ifStatement ((craftyHas collider "Player") `and` checkWhoFired) (updateEntityHealth ProEntity protagonist) (Just $ Right $
          (ifStatement (((exnm' targets) `eq` weaponStr) `and` (craftyHas collider "Weapon")) (functionBody [] [
          craftyDestroy collider]) Nothing))))
        ])
      ]
    )
  )

-- Create the entity at the bottom of the level to act as an Antag which kills the player if they fall
bottomOfLevel :: JSVarStatement
bottomOfLevel =
  var createBot (function "" []
    (functionBody
    [
      var winDim (stmtAsExpr enviro getWindowDimensions (invoke' [])),
      bottomShape (getField (exnm' winDim, "w")),
      var spawnLoc (object
        [
          field (nm "x")    (dbl 0),
          field (nm "y")    (getField (exnm' winDim, "h"))
        ]),
      var bttm (paramInvk "createEntity" [exnm' spawnLoc, exnm' bttmShp, true])
    ]
    [
      craftyAddComponent bttm [bottomStr],
      setNameStmt bttm [bottomStr]
    ]
    )
  )

-- Create conditional which will place a hitbox at the end of the level if that is the way to win that level
-- This is so the engine does not need to constantly check if the player's position is at the end of the level
endOfLevelWCCond :: JSStatement
endOfLevelWCCond = ifStatement (objectHasField winCond "eol") (functionBody
        [
          var coords (engineCoordinates [getField (exnm "winCond", "eol")]),
          var cellWidth getCellWidth,
          var cellHeight getCellHeight,
          var eolHit (paramInvk "createHitbox" [coordsExpr, exnm' cellHeight, exnm' cellWidth])
        ] [
          setNameStmt eolHit [eolStr],
          craftyAddComponent eolHit [eolStr]
        ]) Nothing

getCurrentWinCondition :: JSExpression
getCurrentWinCondition = getField (stmtAsExpr levelsNm getCurrentLevel (invoke' []), "winCondition")

levelGenerator :: JSVarStatement
levelGenerator =
  var levelNm  (function "" [dataVal]
    (functionBody
    [
      self,
      var winCond getCurrentWinCondition,
      var typeNm getCurrentCameraType
    ]
    [
      statement protagonist initProp (invoke' [getField (dataValExpr, "protag")]),
      statement' platPlacer (invoke' [getField (dataValExpr, "lvlplatforms")]),
      statement' antPlacer (invoke' [getField (dataValExpr, "lvlantags")]),
      statement' upgrdPlacer (invoke' [getField (dataValExpr, "lvlupgrades")]),
      statement' createBot (invoke' []),
      statement enviro hudDisplay (invoke' [exnm' hudElementsNm]),
      endOfLevelWCCond,
      levelCamera
    ]
    )
  )

getCurrentCameraType :: JSExpression
getCurrentCameraType = getField (stmtAsExpr levelsNm getCurrentLevel (invoke' []), "camera")

levelCamera :: JSStatement
levelCamera = ifStatement (excond NotEq typeExpr fixed) (functionBody
  [
  var camWidth    $ arrayVal cameraView (dbl 0),
  var camHeight   $ arrayVal cameraView (dbl 1),
  var camFunction $ getField (typeExpr, "Type")
  ]
  [
  statement' camFunction (invoke' [player, exnm' camHeight, exnm' camWidth])
  ]) Nothing

-- Create the for loop for the antag and upgrade placer (general enough to be used for both)
-- Commonalities:
-- for loop from i in expression (forexpr)
-- Get and set name
-- Antags need shape data (Antagonists.getshape(name)) while collectibles need colour data (Upgrades.getcolour(name))
-- Get real coordinates (Environment.getRealCoords(i.coords))
-- Add component to crafty entity (for antags: 'Antag', for collectibles: 'Pickup')
-- Unique to Antags: newEnt = createEntity(..), newAntag.entity = new Antag(name)
-- Unique to Upgrades: newEnt = createPickup(..)
entityPlacer :: JSExpression -> JSName -> EntOrUpg -> String -> [JSExpression] -> JSStatement
entityPlacer forexpr shpOrclr eOru getterFunc cc =
  forLoop i forexpr (functionBody ([
      var objName (arrayVal forexpr i'),
      var nmeNm (getField (objExpr, "name")),
      var clrShp (stmtAsExpr shpOrclr (prop getterFunc) (invoke' [nme])),
      var coords (engineCoordinates [getField (objExpr, "coords")])
    ] ++ createAntagOrPickup eOru coordsExpr (exnm' clrShp))
    [
      setNameStmt newEntNm [nme],
      craftyAddComponent newEntNm cc
    ])

setWeaponTrigger :: JSVarStatement
setWeaponTrigger = setField (newEnt, "triggerFunction") (object [
  field functionNm (exnm' checkIfFired),
  field args (array [newEnt, wpnNameExpr])
  ])

-- Adds the unique lines of code for antags or upgrades
-- pattern match the data structure EntOrUpg: Ent for antags, Upg for pickup entities in game
createAntagOrPickup :: EntOrUpg -> JSExpression -> JSExpression -> [JSVarStatement]
createAntagOrPickup Ent param1 param2 = [
        var newEntNm (paramInvk "createEntity" [param1, param2, stmtAsExpr antagonists getflying (invoke' [nme])]),
        var dataVal (paramInvk "Antag" [nme, coordsExpr]),
        var wpnName  (stmtAsExpr dataVal getweapon (invoke' [])),
        setWeaponTrigger,
        setField (newEnt, "entity") dataValExpr,
        setField (newEnt, "movement") (stmtAsExpr dataVal getMovementData (invoke' [getField (objExpr, "movementData")]))]
createAntagOrPickup Upg param1 param2 = [var newEntNm (paramInvk "createPickup" [param1, param2])]

platformPlacer :: JSVarStatement
platformPlacer =
  var platPlacer (function "" [dataSet]
    (functionBody []
    [
      forLoop i dataSetExpr (functionBody [
        var dataVal (arrayVal dataSetExpr i'),
        var coords (engineCoordinates [getField (dataValExpr, "coords")])]
        [
        statement' floorNm (invoke' [coordsExpr, getField (dataValExpr, "size"), getField (dataValExpr, "colour")])
        ])
    ]
    )
  )

antagPlacer :: JSVarStatement
antagPlacer =
  var antPlacer (function "" [dataSet]
    (functionBody []
    [
      entityPlacer dataSetExpr antagonists Ent "getshape" [antagStr, character]
    ]
    )
  )

upgradePlacer :: JSVarStatement
upgradePlacer =
  var upgrdPlacer (function "" [dataSet]
    (functionBody []
    [
      entityPlacer dataSetExpr upgradeFuncNm Upg "getcolour" [pickup, stmtAsExpr upgradeFuncNm getType (invoke' [nme])]
    ]
    )
  )

checkFired :: JSVarStatement
checkFired =
  var checkIfFired (function "" [entityNm, wpnName]
    (functionBody
    [
      var rateofFire $ (dbl 1000) `div` (stmtAsExpr weaponName getrof $ invoke' [wpnNameExpr]),
      var now $ stmtAsExpr date nowP $ invoke' [],
      var lastF $ getField (entity, "lastFired"),
      var dif $ excond Sub (exnm' now) (exnm' lastF)
    ]
    [
      ifStatement (((exnm' lastF) `eq` undefinedExpr) `or` ((exnm' dif) `gte` (exnm' rateofFire))) (
        functionBody [setField (entity, "lastFired") (exnm' now)] [statement' fireWeaponNm (invoke' [entity, wpnNameExpr])]) Nothing
    ]))

getCraftyBulletSpeed :: JSExpression
getCraftyBulletSpeed = getCraftyMovementSpeed $ getField (at, "bulletSpeed")

fireWeapon :: JSVarStatement
fireWeapon =
  var fireWeaponNm (function "" [entityNm, wpnName]
    (functionBody
    [
      var shape (stmtAsExpr weaponName getshape (invoke' [wpnNameExpr])),
      var range (stmtAsExpr weaponName getrange (invoke' [wpnNameExpr])),
      var coords (object 
        [
        -- Offset the firing location so the bullet is not spawning in the entity and causing a collision with its own firing entity
        field (nm "x")    (getField (entity, "x")),
        field (nm "y")    (getField (entity, "y"))
        ]),
      var attack_type (stmtAsExpr weaponName getattack_type (invoke' [wpnNameExpr])),
      var newWeapon (paramInvk "createEntity" [coordsExpr, shpExpr, true]),
      setField (newW, "start") coordsExpr,
      setField (newW, "whoFired") entity
    ]
    [
    setNameStmt newWeapon [wpnNameExpr],
    craftyAddComponent newWeapon [weaponStr],
    ifStatement (at `eq` melee) (functionBody [
      comment (exstr "swing weapon"),
      -- Make a hitbox covering the entire area that the weapon swings in (since Crafty is not rotating the actual hitbox of the melee weapon)
      var weaponHitboxNm (paramInvk "createEntity" [coordsExpr, shpExpr, true]),
      setField (exnm "weaponHitbox", "whoFired") entity,
      setField (newW, "hitbox") (exnm' weaponHitboxNm),
      setField (newW, "h") rng, -- For melee weapons the range is how long the weapon can reach from its swinging point (how long the weapon is)
      setField (newW, "movement") (object [
        field pattern    (paramInvk "Swing" []),
        field speed      (checkFacing (dbl 400) (dbl $ -400)), --Quick enough swing for a melee weapon
        field dataSet    (dbl 0) -- Not needed for the Swing movement but this field exists to be consistent with other movement objects
        ])
      ] [
        setNameStmt weaponHitboxNm [wpnNameExpr],
        craftyAddComponent weaponHitboxNm [weaponStr],
        craftyRemoveComponent weaponHitboxNm [color],
        craftyRemoveComponent newWeapon [weaponStr],
        craftyShift weaponHitboxNm [weaponShiftx, dbl 0, checkFacing ((dbl $ -1) `mul` rng) rng, getField (entity, "h")],
        statement entityNm attach (invoke' [newW]), -- Make the melee weapon entity a child of the firing entity so they move together
        craftyShift newWeapon [weaponShiftx, weaponShifty, dbl 0, dbl 0], -- Shift the starting point from the entity to a little offset so they do not collide with each other
        statement newWeapon bindProp (invoke' [exstr "Remove", hitboxRemoveFunction]) -- When the melee weapon is removed from the game, the hitbox should also be destroyed
      ]) (Just $ Left $ (functionBody [
      var bulletPattern (getField (at, "bulletPattern")),
      setField (newW, "movement") (object [
        field pattern       (paramInvk "bulletPattern" []),
        field speed         (checkFacing ((dbl (-1)) `mul` getCraftyBulletSpeed) getCraftyBulletSpeed),
        field dataSet       (stmtAsExpr weaponName weaponMoveData (invoke' [wpnNameExpr, wpnPat, rng, newW]))
        ])][craftyShift newWeapon [dbl 0, weaponShifty, dbl 0, dbl 0]] --Shift the starting point from the entity to a little offset so they do not collide with each other (different y value shift for projectiles)
      ))
    ]
    )
  )
  where
    checkFacing = extern ((getField (entity, "facing")) `eq` (exstr "left"))
    weaponShiftx = (getField (entity, "w")) `div` (dbl 2)
    weaponShifty = (getField (entity, "h")) `div` (dbl 2)

hitboxRemoveFunction :: JSExpression
hitboxRemoveFunction = function "" [] $
  functionBody [] [craftyDestroy $ nm "newWeapon.hitbox"]

weaponMovementData :: JSExpression
weaponMovementData = function "" [wpnName, weaponType, lengthNm, newWeapon] (functionBody
    [
      var dataVal (stmtAsExpr weaponName gettargets (invoke' [wpnNameExpr]))
    ]
    [
      ifStatement (excond Eq (exnm' weaponType) homing) (functionBody [] 
        [returnStatement (object [
          field range    (exnm' lengthNm),
          field closestTarg   (stmtAsExpr weaponName getClosestTarget (invoke' [dataValExpr, newW]))])
        ]) (Just $ Left $ (functionBody [] [returnStatement (exnm' lengthNm)]))
    ])

weaponGetClosestTarg :: JSExpression
weaponGetClosestTarg = function "" [targets, entityNm] (functionBody
  [
    var allTargs (craftyGet [exnm' targets]),
    var filtered (stmtAsExpr allTargs filterP (invoke' [arrowFunction closestTarg filterTargets]))
  ]
  [
    returnStatement' shortestDist Nothing (invoke' [getWhoFired entity, exnm' filtered])
  ]
  )

filterTargets :: JSExpression
filterTargets = excond And (
  excond NotEq (exnm' closestTarg) (getWhoFired entity)) (
  excond NotEq (getWhoFired (exnm' closestTarg)) (getWhoFired entity))

getScoreExpr :: JSExpression
getScoreExpr = stmtAsExpr gameStats getscore $ invoke' []

winCondCheck :: JSVarStatement
winCondCheck =
  var winConditionCheck (function "" [objName] (functionBody
    [
      var winCond getCurrentWinCondition 
    ] [
      ifStatement (objectHasField winCond "score") (functionBody [] [
        ifStatement (getScoreExpr `gte` (getField (winExpr, "score"))) (functionBody [] [
          statement levelsNm nextLevel (invoke' [])]) Nothing]) (Just $ Right $

      ifStatement (objectHasField winCond "boss") (functionBody [] [
        ifStatement (excond Eq objExpr (getField (winExpr, "boss"))) (functionBody [] [
          statement levelsNm nextLevel (invoke' [])]) Nothing]) (Just $ Left $

      functionBody [] [
        ifStatement (excond Eq objExpr eolStr) (functionBody [] [
          statement levelsNm nextLevel (invoke' [])]) Nothing]
      )
      )
    ]
    )
  )

upgradeTypeConditional :: CraftyComponent -> [JSVarStatement] -> JSStatement
upgradeTypeConditional c upgradeLinks = ifStatement (objectHasField upgradeDataNm c) (functionBody
                                [
                                  var dataSet (getField (exnm' upgradeDataNm, c))
                                ]
                                [
                                  forLoop i dataSetExpr (
                                    functionBody
                                    ([
                                      var dataVal (arrayVal dataSetExpr i'),
                                      var nmeNm (getField (dataValExpr, "name")),
                                      selfField' (arrayVal (exnm' upgradeDataNm) nme) $ dataValExpr,
                                      selfArrayField (arrayVal (exnm' upgradeDataNm) nme) typeNm $ exstr c
                                    ] ++ upgradeLinks)
                                    [])
                                ]) Nothing

enit :: Game -> JSProgram
enit g = 
    program
    ([
        var gridNm (jsCoord $ getGrid g),
        logic (getLogic g),
        w,
        a,
        u,
        var initialize (function "" []
            (
                functionBody
                []
                [
                    statement enviro initProp (invoke' []),
                    grid $ getGrid g,
                    statement weaponName initProp (invoke' []),
                    statement antagonists initProp (invoke' []),
                    statement upgradeFuncNm initProp (invoke' []),
                    statement levelsNm initProp (invoke' [lvls]),
                    statement gameStats initProp (invoke' []),
                    statement enviro begin (invoke' [true])
                ]
            )
        ),
        -- Global statistics which will carry on between levels (such as score if applicable)
        var gameStats (object
            ([
                field initNm           (function "" [] (
                    functionBody
                    [
                      self,
                      fst (linkGetter "score" $ dbl 0)
                    ]
                    []
                    )
                ),

                field updateScoreNm    (function "" [update] (
                  functionBody
                  [
                    self,
                    selfField "score" $ (exnm' update) `add` getScoreExpr
                  ][]
                  ))
            ] ++ snd (linkGetter "score" $ dbl 0))
        ),

        var antagonists (object
            ([
                field initNm           (function "" [] (
                            functionBody 
                            [
                                self,
                                fst (linkGetter "antags" $ exnm "{}")
                            ]
                            [
                                forLoop i antagsExpr (
                                    functionBody
                                    ([
                                        var dataVal (arrayVal antagsExpr i'),
                                        var nmeNm (getField (arrayVal antagsExpr i', "name")),
                                        selfField' (arrayVal antagsExpr nme) dataValExpr
                                    ] ++ antagLinks)
                                    []
                                )
                                
                            ]
                        )
                    )
            ] ++ concat antagGetterSetter)
        ),

        var protagonist (object
            ([
                field initNm          (function "" [prtg] (
                  functionBody 
                  ([
                    self,
                    selfField "initialHealth" $ getField (exnm' prtg, "health")
                  ] ++ protagLinks)
                  [   
                    returnStatement (stmtAsExpr protagonist spawn (invoke' [])) -- Return a new game entity which is playable
                  ]
                        )
                    ),

                field spawnNm   (function "" [] (
                  functionBody
                  [
                    self,
                    selfField "health" $ getField (selfExpr, "initialHealth"),
                    var dataVal $ engineCoordinates [stmtAsExpr protagonist getinitPos (invoke' [])],
                    var shape (stmtAsExpr protagonist getshape (invoke' [])),
                    var playerNm (paramInvk "createEntity" [dataValExpr, shpExpr, false])
                  ]
                  ([
                  ] ++ playable playerNm)))

            ] ++ concat protagGetterSetter)
        ),

        var dispatchFunction (function "" [objName, dataSet] (
          functionBody []
          [
            ifStatement (craftyHas objName "Player") (functionBody [] [
              statement' dispatchFunctionPro (invoke' [objExpr, dataSetExpr])
            ]) (Just $ Right $ 

            ifStatement (craftyHas objName "Weapon") (functionBody [] [
              statement' dispatchFunctionWep (invoke' [objExpr, dataSetExpr])]) Nothing)
          ])),

        var weaponName (object
            ([
                field initNm           (function "" [] (
                            functionBody 
                            [
                                self,
                                fst (linkGetter "weapons" $ exnm "{}")
                            ]
                            [
                                forLoop i weaponsExpr (
                                    functionBody
                                    ([
                                        var dataVal (arrayVal weaponsExpr i'),
                                        var nmeNm (getField (arrayVal weaponsExpr i', "name")),
                                        selfField' (arrayVal weaponsExpr nme) dataValExpr
                                    ] ++ weaponLinks)
                                    []
                                )
                                
                            ]
                        )
                    ),

                field weaponMoveFunc  weaponMovementData,
                field closestTargFunc weaponGetClosestTarg
            ] ++ concat weaponGetterSetter)
        ),

        var upgradeFuncNm (object
            ([
                field initNm           (function "" [] (
                            functionBody 
                            [
                                self,
                                fst (linkGetter "upgrades" $ exnm "{}")
                            ]
                            [
                              upgradeTypeConditional "collectibles" upgradeLinks,
                              upgradeTypeConditional "healthUpgrades" upgradeLinks,
                              upgradeTypeConditional "lifeUpgrades" upgradeLinks,
                              upgradeTypeConditional "weaponUpgrades" upgradeLinks                               
                            ]
                        )
                    ),

                stepTwo' "Type" (arrayVal upgradesExpr nme) 
            ] ++ concat upgradeGetterSetter)
        ),
        collisionComments,
        protagCollision,
        weaponCollision,
        bottomOfLevel,
        platformPlacer,
        antagPlacer,
        upgradePlacer,
        winCondCheck,
        levelGenerator,
        checkFired,
        fireWeapon,
        hudElements [
        ("score", bind (exnm' gameStats) "getscore"),
        ("lives", bind (exnm' protagonist) "getlives"),
        ("health", bind (exnm' protagonist) "gethealth")
        ]

    ] ++ antagProto)
    [
        statement' initialize (invoke' [])
    ]

    where
        (w, a, u) = elements $ getElements g
        (protagLinks,protagGetterSetter) = unzip ([linkGetter "health" $ getField (prtgExpr, "health")] ++ [linkGetter "shape" $ getField (prtgExpr, "shape")] ++
          [linkGetter "initPos" $ getField (prtgExpr, "initPos")] ++ [linkGetter "lives" $ getField (prtgExpr, "lives")] ++
          [linkGetter "weapon" $ getField (prtgExpr, "weapon")] ++ [linkGetter "jumpHeight" $ getField (prtgExpr, "jumpHeight")] ++
          [linkGetter "speed" $ getField (prtgExpr, "speed")])

        (antagLinks,antagGetterSetter) = unzip ([linkGetter' "lives" (arrayVal antagsExpr nme) $ getField (arrayVal antagsExpr i', "lives")] ++
          [linkGetter' "shape" (arrayVal antagsExpr nme) $ getField (arrayVal antagsExpr i', "shape")] ++
          [linkGetter' "health" (arrayVal antagsExpr nme) $ getField (arrayVal antagsExpr i', "health")] ++
          [linkGetter' "score" (arrayVal antagsExpr nme) $ getField (arrayVal antagsExpr i', "score")] ++
          [linkGetter' "movement" (arrayVal antagsExpr nme) $ getField (arrayVal antagsExpr i', "movement")] ++
          [linkGetter' "flying" (arrayVal antagsExpr nme) $ getField (arrayVal antagsExpr i', "flying")] ++
          [linkGetter' "weapon" (arrayVal antagsExpr nme) $ getField (arrayVal antagsExpr i', "weapon")])

        (weaponLinks, weaponGetterSetter) = unzip ([linkGetter' "shape" (arrayVal weaponsExpr nme) $ getField (arrayVal weaponsExpr i', "shape")] ++
          [linkGetter' "damage" (arrayVal weaponsExpr nme) $ getField (arrayVal weaponsExpr i', "damage")] ++
          [linkGetter' "attack_type" (arrayVal weaponsExpr nme) $ getField (arrayVal weaponsExpr i', "attack_type")] ++
          [linkGetter' "range" (arrayVal weaponsExpr nme) $ getField (arrayVal weaponsExpr i', "range")] ++
          [linkGetter' "rate_of_fire" (arrayVal weaponsExpr nme) $ getField (arrayVal weaponsExpr i', "rate_of_fire")] ++
          [linkGetter' "targets" (arrayVal weaponsExpr nme) $ getField (arrayVal weaponsExpr i', "targets")])

        (upgradeLinks, upgradeGetterSetter) = unzip ([linkGetter' "colour" (arrayVal upgradesExpr nme) $ getField (dataValExpr, "colour")] ++
          [linkGetter' "score" (arrayVal upgradesExpr nme) $ getField (dataValExpr, "score")] ++
          [linkGetter' "health" (arrayVal upgradesExpr nme) $ getField (dataValExpr, "health")] ++
          [linkGetter' "lives" (arrayVal upgradesExpr nme) $ getField (dataValExpr, "lives")] ++
          [linkGetter' "newWeapon" (arrayVal upgradesExpr nme) $ getField (dataValExpr, "newWeapon")])

program :: [JSVarStatement] -> [JSStatement] -> JSProgram
program = JSProgram
