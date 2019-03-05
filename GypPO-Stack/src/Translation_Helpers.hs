module Translation_Helpers where

-- Module adapted from Pretty.hs from MAKU

import AST.JS
import AST.Design (Coord, unNat)

type Field = String -- When calling an object field
type CraftyComponent = String

data TypeOfUpgrades = Collect | Health | Life | NewWeapon
data TypeofEntity = ProEntity | AntEntity
data EntOrUpg = Ent| Upg

-- names of fundamental 'objects'
collider, colliderNm, objName, newValname, protagonist, levelsNm, antagonists, weaponName, upgradeFuncNm,
  enviro, crafty, winConditionCheck, bttm, eolHit, platPlacer, antPlacer, upgrdPlacer,
  createBot, camFunction, newEntNm, floorNm, fireWeaponNm, newWeapon, weaponHitboxNm,
  entityNm, gameStats, dispatchFunction, dispatchFunctionPro, dispatchFunctionWep,
  initialize, checkIfFired, antagNm, hudElementsNm, bttmShp, shapeType, height, width,
  colour, keyE, livesNm, updatedHlth, nmeNm, update, prtg, upgradeDataNm, dataVal, dataSet,
  typeNm, i, objNm, targets, clrShp, coords, wpnName, functionNm, args, cameraNm, initP,
  initNm, updateScoreNm, spawnNm, playerNm, name, shape, damage, attack_type, range, health,
  rateofFire, bulletSpeed, bulletPattern, weaponNm, flying, movement, score, pattern, track,
  vee, speed, gridNm, collectiblesNm, healthUpgradesNm, lifeUpgradesNm, weaponUpgradesNm,
  viewNm, sizeNm, movementData, start, end, lengthNm, colliderEntity, weaponDataNm, antagDataNm,
  constructor, getMoveDataNm, weaponType, updatedLife, winDim, spawnLoc, cellWidth, cellHeight,
  levelNm, camWidth, camHeight, winCond, lastF, dif, now, date, closestTarg, filtered,
  allTargs, closestTargFunc, weaponMoveFunc, lvlsNm, lvlplats, lvlants, lvlupgrds, txt :: JSName
collider = nm "collider" -- this is the crafty object
colliderNm = nm "colliderNm" -- this is the name assigned to the crafty object (collider.getName())
colliderEntity = nm "collEnt" -- All the gyppo entity information assigned to a crafty object (collider.entity)
objName = nm "obj"
newValname = nm "newVal"
newWeapon = nm "newWeapon"
entityNm = nm "entity"
protagonist = nm "Protagonist"
antagonists = nm "Antagonists"
antagNm = nm "Antag"
levelsNm = nm "Levels"
weaponName = nm "Weapons"
upgradeFuncNm = nm "Upgrades" -- Upgrades functions
gameStats = nm "GameStats"
fireWeaponNm = nm "fireWeapon"
weaponHitboxNm = nm "weaponHitbox"
checkIfFired = nm "checkIfFired"
enviro = nm "Environment"
crafty = nm "Crafty"
winConditionCheck = nm "winConditionCheck"
bttm = nm "bttm"
bttmShp = nm "bottomShp"
shapeType = nm "shapeType"
height = nm "height"
width = nm "width"
colour = nm "colour"
floorNm = nm "Floor"
eolHit = nm  "eolHit"
platPlacer = nm "platformPlacer"
antPlacer = nm "antagPlacer"
upgrdPlacer = nm "upgradePlacer"
createBot = nm "createBot" 
camFunction = nm "camFunction"
newEntNm = nm "newEnt"
dispatchFunction = nm "dispatchFunction"
dispatchFunctionPro = nm "dispatchFunctionProtag"
dispatchFunctionWep = nm "dispatchFunctionWeapon"
initialize = nm "initialize"
hudElementsNm = nm "HUDelements"
keyE = nm "e"
livesNm = nm "lives"
updatedHlth = nm "updatedHlth"
updatedLife = nm "updatedLife"
nmeNm = nm "nme"
update = nm "update"
prtg = nm "prtg"
dataVal = nm "dataVal"
dataSet = nm "dataSet"
upgradeDataNm = nm "upgrades" --upgrades data set
weaponDataNm = nm "weapons" -- weapons data set as apposed to Weapons functions (above)
antagDataNm = nm "antags" -- antags data set
typeNm = nm "Type"
i = nm "i"
objNm = nm "objNm"
targets = nm "targets"
clrShp = nm "clrShp"
coords = nm "coords"
wpnName = nm "wpnName"
functionNm = nm "function"
args = nm "args"
cameraNm = nm "camera"
initP = nm "initP" --initial position
initNm = nm "init" --init function
updateScoreNm = nm "updateScore"
spawnNm = nm "spawn"
playerNm = nm "player"
name = nm "name"
damage = nm "damage"
shape = nm "shape"
attack_type = nm "attack_type"
range = nm "range"
rateofFire = nm "rate_of_fire"
bulletSpeed = nm "bulletSpeed"
bulletPattern = nm "bulletPattern"
health = nm "health"
weaponNm = nm "weapon" -- reffering to the linked weapon to an entity rather than the functions belong to Weapons
flying = nm "flying"
movement = nm "movement"
score = nm "score"
pattern = nm "pattern"
track = nm "track"
vee = nm "v"
speed = nm "speed"
gridNm = nm "grid"
collectiblesNm = nm "collectibles"
healthUpgradesNm = nm "healthUpgrades"
lifeUpgradesNm = nm "lifeUpgrades"
weaponUpgradesNm = nm "weaponUpgrades"
viewNm = nm "view"
sizeNm = nm "size"
movementData = nm "movementData"
start = nm "start"
end = nm "end"
lengthNm = nm "length"
constructor = nm "constructor"
getMoveDataNm = nm "getMovementData"
weaponType = nm "weaponType"
winDim = nm "winDim"
spawnLoc = nm "spawnLoc"
winCond = nm "winCond"
cellWidth = nm "cellWidth"
cellHeight = nm "cellHeight"
levelNm = nm "Level"
camWidth = nm "camWidth"
camHeight = nm "camHeight"
now = nm "now"
lastF = nm "lastF"
dif = nm "dif"
date = nm "Date"
closestTarg = nm "target" -- single target for the homing weapon
filtered = nm "filtered"
allTargs = nm "allTargs"
closestTargFunc = nm "getClosestTarget"
weaponMoveFunc = nm "weaponMovementData"
lvlsNm = nm "lvls"
lvlplats = nm "lvlplatforms"
lvlants = nm "lvlantags"
lvlupgrds = nm "lvlupgrades"
txt = nm "txt"

-- names as expressions
nme, newValexpr, lives, weaponsExpr, antagsExpr, newW, newEnt, wpnNameExpr, entity,
  colliderNameExpr, selfExpr, playerExpr, thisExpr, healthExpr, initPExpr, prtgExpr,
  dataValExpr, i', objExpr, objNmExpr, dataSetExpr, shpExpr, upgradesExpr, gridExpr,
  onewayCam, freeCam, movementExpr, typeExpr, undefinedExpr, coordsExpr, at, rng,
  wpnPat, homing, shortestDist, winExpr, lvls, straight, arc, vatk, still, patrol,
  charge, random :: JSExpression
nme = exnm "nme"
newValexpr = exnm' newValname
lives = exnm "lives"
weaponsExpr = exnm "weapons"
antagsExpr = exnm "antags"
newW = exnm "newWeapon"
newEnt = exnm' newEntNm
wpnNameExpr = exnm' wpnName
entity = exnm "entity"
colliderNameExpr = exnm "colliderNm"
selfExpr = exnm "self"
playerExpr = exnm "player"
thisExpr = exnm "this"
healthExpr = exnm "health"
initPExpr = exnm' initP
prtgExpr = exnm' prtg
dataValExpr = exnm' dataVal
i' = exnm "i"
objExpr = exnm "obj"
objNmExpr = exnm' objNm -- Name of the object as expression
dataSetExpr = exnm' dataSet
shpExpr = exnm' shape
upgradesExpr = exnm "upgrades"
gridExpr = exnm' gridNm
onewayCam = exnm "oneWayCamera"
freeCam = exnm "freeCamera"
movementExpr = exnm' movement
typeExpr = exnm' typeNm
undefinedExpr = exnm "undefined"
coordsExpr = exnm' coords
at = exnm' attack_type
rng = exnm' range
wpnPat = exnm' bulletPattern
homing = exnm "Homing"
straight = exnm "Straight"
arc = exnm "Arc"
vatk = exnm "VAtk"
still = exnm "Still"
patrol = exnm "Patrol"
charge = exnm "Charge"
random = exnm "Random"
shortestDist = exnm "shortestDistance"
winExpr = exnm' winCond
lvls = exnm "lvls"

-- strings as expressions
player, character, antagStr, pickup, weaponStr, color, gameOver, entityStr, twoWayComponent,
  keyDown, eolStr, fixed, bottomStr, melee, noWep :: JSExpression
player = exstr "Player"
twoWayComponent = exstr "Twoway"
entityStr = exstr "Entity"
character = exstr "Character"
antagStr = exstr "Antag"
pickup = exstr "Pickup"
weaponStr = exstr "Weapon"
color = exstr "Color"
bottomStr = exstr "Bottom"
gameOver = exstr "GameOver"
keyDown = exstr "KeyDown" -- crafty event 
eolStr = exstr "EOL"
fixed = exstr "fixed"
melee = exstr "melee"
noWep = exstr "noweapon"

-- properties (JSRefinement)
getshape, getweapon, getName, getscore, hudUpdate, setName, setweapon, updateScore,
  hudDisplay, sethealth, setlives, detach, enterScene, initProp, shift, attach, bindProp,
  nextLevel, begin, trigger, destroy, addComponent, removeComponent, spawn, twoway,
  call, has, get, hasOwnProperty, getRealCoords, getlives, gethealth, getmovement,
  getflying, getnewWeapon, gettargets, getdamage, getMovementData, getType, getWindowDimensions,
  gridP, getCurrentLevel, getrof, nowP, getrange, getattack_type, weaponMoveData,
  getClosestTarget, filterP, getinitPos :: JSRefinement
getshape = prop "getshape"
getdamage = prop "getdamage"
getweapon = prop "getweapon"
getnewWeapon = prop "getnewWeapon"
setweapon = prop "setweapon"
gethealth = prop "gethealth" 
sethealth = prop "sethealth"
getlives = prop "getlives"
setlives = prop "setlives"
getmovement = prop "getmovement"
getflying = prop "getflying"
gettargets = prop "gettargets"
getWindowDimensions = prop "getWindowDimensions"
getName = prop "getName"
getscore = prop "getscore"
gridP = prop "grid"
hudUpdate = prop "HUDUpdate"
hudDisplay = prop "HUDDisplay"
updateScore = prop "updateScore"
enterScene = prop "enterScene"
setName = prop "setName"
detach = prop "detach"
initProp = prop "init"
shift = prop "shift"
attach = prop "attach"
bindProp = prop "bind"
nextLevel = prop "nextLevel"
begin = prop "begin"
trigger = prop "trigger"
destroy = prop "destroy"
addComponent = prop "addComponent"
removeComponent = prop "removeComponent"
spawn = prop "spawn"
twoway = prop "twoway"
call = prop "call"
has = prop "has"
get = prop "get"
hasOwnProperty = prop "hasOwnProperty"
getRealCoords = prop "getRealCoords"
getMovementData = prop "getMovementData"
weaponMoveData = prop "weaponMovementData"
getType = prop "getType"
getCurrentLevel = prop "getCurrentLevel"
getrof = prop "getrate_of_fire"
nowP = prop "now"
getrange = prop "getrange"
getattack_type = prop "getattack_type"
getClosestTarget = prop "getClosestTarget"
filterP = prop "filter"
getinitPos = prop "getinitPos"

getCellWidth, getCellHeight :: JSExpression
getCellWidth = stmtAsExpr enviro (prop "getCellWidth") (invoke' [])
getCellHeight = stmtAsExpr enviro (prop "getCellHeight") (invoke' [])

dbl :: Double -> JSExpression
dbl = JSExpressionLiteral . JSLiteralDouble . JSDouble

exbool :: Bool -> JSExpression
exbool True = true
exbool False = false

num :: Double -> JSLiteral
num = JSLiteralDouble . JSDouble

exnm :: String -> JSExpression
exnm = JSExpressionName . jsName

exnm' :: JSName -> JSExpression
exnm' = JSExpressionName

nm :: String -> JSName
nm = jsName

exstr :: String -> JSExpression
exstr = JSExpressionLiteral . JSLiteralString . str

str :: String -> JSString
str = jsString

comment :: JSExpression -> JSVarStatement
comment = JSCommentStatement

var :: JSName -> JSExpression -> JSVarStatement
var a b = JSVarStatement [JSVarDecl a (Just b)]

false :: JSExpression
false = exnm "false"

true :: JSExpression
true = exnm "true"

excond :: InfixOp -> JSExpression -> JSExpression -> JSExpression
excond = JSExpressionInfix

extern :: JSExpression -> JSExpression -> JSExpression -> JSExpression
extern = JSExpressionTernary

new :: JSExpression -> [JSExpression] -> JSExpression
new a b = JSExpressionNew a (invk b)

self :: JSVarStatement
self = var (nm "self") (exnm "this")

selfField :: Field -> JSExpression -> JSVarStatement
selfField fld e = JSSelfStatement [JSSelfDecl (nm ("self." ++ fld)) e]

selfField' :: JSExpression -> JSExpression -> JSVarStatement
selfField' fld e = JSSelfArrayStatement [JSSelfArrayDecl (nm "self") fld e]

selfArrayField :: JSExpression -> JSName -> JSExpression -> JSVarStatement
selfArrayField arrayexpr jsname e = JSSelfArrayFieldStatement [JSSelfArrayFieldDecl (nm "self") arrayexpr jsname e]

linkGetter :: Field ->JSExpression -> (JSVarStatement, [JSObjectField])
linkGetter fld e = (JSSelfStatement [JSSelfDecl (nm ("self." ++ fld)) e], [stepTwo fld,setter fld])

stepTwo ::  Field -> JSObjectField
stepTwo f = field (nm ("get" ++ f))       (function "" [] (
                        functionBody
                        [
                          self
                        ]
                        [
                          returnStatement (exnm ("self." ++ f))
                        ]))

linkGetter' :: Field -> JSExpression -> JSExpression -> (JSVarStatement, [JSObjectField])
linkGetter' fld arrayexpr e = (JSSelfArrayFieldStatement [JSSelfArrayFieldDecl (nm "self") arrayexpr (nm fld) e], [stepTwo' fld arrayexpr, setter' fld arrayexpr])

stepTwo' ::  Field -> JSExpression -> JSObjectField
stepTwo' f e = field (nm ("get" ++ f))       (function "" [nm "nme"] (
                        functionBody
                        [
                          self
                        ]
                        [
                          returnStatementArray (Just (nm "self")) e (nm f)
                        ]))

setter :: Field -> JSObjectField
setter f = field (nm ("set" ++ f))          (function "" [newValname] (
                        functionBody
                        [
                          self,
                          selfField f newValexpr
                        ]
                        [
                        ]))

setter' :: Field -> JSExpression -> JSObjectField
setter' f e = field (nm ("set" ++ f))          (function "" [nm "nme", newValname] (
                        functionBody
                        [
                          self,
                          JSSelfArrayFieldStatement [JSSelfArrayFieldDecl (nm "self") e (nm f) newValexpr]
                        ]
                        [
                        ]))

functionBody :: [JSVarStatement] -> [JSStatement] -> JSFunctionBody
functionBody = JSFunctionBody

function :: String -> [JSName] -> JSFunctionBody -> JSExpression
function strName parms body = JSExpressionLiteral $ JSLiteralFunction $ JSFunctionLiteral (Just $ nm strName) parms body

arrowFunction :: JSName -> JSExpression -> JSExpression
arrowFunction = JSExpressionArrow

prop :: String -> JSRefinement
prop = JSProperty . nm

invk :: [JSExpression] -> JSInvocation
invk = JSInvocation

invoke' :: [JSExpression] -> JSRValue
invoke' = JSRVInvoke . pure . invk

paramInvk :: String -> [JSExpression] -> JSExpression
paramInvk a b = new (exnm a) b

-- paramInvk without the "new" keyword
paramInvk' :: String -> [JSExpression] -> JSExpression
paramInvk' a b = JSExpressionInvocation (exnm a) (invk b)

protoLinkGetSet :: JSName -> Field -> JSExpression -> (JSVarStatement, [JSVarStatement])
protoLinkGetSet name1 fld e = (JSSelfStatement [JSSelfDecl (nm ("self." ++ fld)) e], [protoGetter name1 fld, protoSetter name1 fld])

protoGetter :: JSName -> Field -> JSVarStatement
protoGetter n1 f = JSProtoVarStatement [JSProtoDecl n1 (nm ("get" ++ f)) (function "" [] (
  functionBody
  [
    self
  ]
  [
    returnStatement (exnm ("self." ++ f))
  ]))]

protoSetter :: JSName -> Field -> JSVarStatement
protoSetter n1 f = JSProtoVarStatement [JSProtoDecl n1 (nm ("set" ++ f)) (function "" [newValname] (
  functionBody
  [
    self,
    selfField f newValexpr
  ]
  []))]

protoVarStmt :: JSName -> JSName -> JSExpression -> JSVarStatement
protoVarStmt name1 name2 expr = JSProtoVarStatement [JSProtoDecl name1 name2 expr]

array :: [JSExpression] -> JSExpression
array = JSExpressionLiteral . JSLiteralArray . JSArrayLiteral

arrayVal :: JSExpression -> JSExpression -> JSExpression
arrayVal arr index = JSExpressionArray $ JSArrayVal arr index

forLoop :: JSName -> JSExpression -> JSFunctionBody -> JSStatement
forLoop a b c = JSStatementFor Nothing $ ForStatementInStyle a b c

ifStatement :: JSExpression -> JSFunctionBody -> (Maybe (Either JSFunctionBody JSStatement)) -> JSStatement
ifStatement cond thn els = JSStatementIf $ JSIfStatement cond thn els

caseAndDisrupt :: JSExpression -> [JSStatement] -> JSCaseAndDisruptive
caseAndDisrupt expr stmt = JSCaseAndDisruptive (JSCaseClause expr stmt) (Break $ JSBreakStatement Nothing)

switchCaseStatement :: JSExpression -> [JSCaseAndDisruptive] -> [JSStatement] -> JSStatement
switchCaseStatement expr cases dflt = JSStatementSwitch Nothing $ JSSwitchStatement expr cases dflt

jsCoord :: Coord -> JSExpression
jsCoord (x, y) = array [dbl (unNat x), dbl (unNat y)]

statement :: JSName -> JSRefinement -> JSRValue -> JSStatement
statement jsname ref val = JSStatementExpression $ JSESApply (pure $ JSLValue jsname [([], ref)]) val

statement' :: JSName -> JSRValue -> JSStatement
statement' jsname val = JSStatementExpression $ JSESApply (pure $ JSLValue jsname []) val

-- Same as statement code above except need the function call returned as an expression (to be stored in a variable) rather than as a statement
stmtAsExpr :: JSName -> JSRefinement -> JSRValue -> JSExpression
stmtAsExpr a b c = JSStmtAsExpression $ JSEApply (pure $ JSLValue a [([], b)]) c

break :: JSStatement
break = JSStatementDisruptive $ Break $ JSBreakStatement Nothing

returnStatement :: JSExpression -> JSStatement
returnStatement = JSStatementDisruptive . Return . JSReturnStatement . Just

returnStatement' :: JSExpression -> (Maybe JSRefinement) -> JSRValue -> JSStatement
returnStatement' a b c = case b of
  Nothing -> JSStatementDisruptive $ Return $ JSRSApply (pure $ JSRLValue a []) c
  Just prp -> JSStatementDisruptive $ Return $ JSRSApply (pure $ JSRLValue a [([], prp)]) c

returnStatementArray :: (Maybe JSName) -> JSExpression -> JSName -> JSStatement
returnStatementArray slf expr fld = JSStatementDisruptive $ Return $ JSArrayReturn slf expr fld

fieldWithObject :: String -> [(String, JSExpression)] -> JSObjectField
fieldWithObject nameOfField parms = field (nm nameOfField) ( object $ fields parms) 

fields :: [(String, JSExpression)] -> [JSObjectField]
fields = map (\(x,y) -> field (nm x) y)

field :: JSName -> JSExpression -> JSObjectField
field a b = JSObjectField (Left a) b

getField :: (JSExpression, Field) -> JSExpression
getField (obj, fld) = JSExpressionField obj (prop fld)

setField :: (JSExpression, Field) -> JSExpression -> JSVarStatement
setField (obj, fld) e = JSFieldVarStatement [JSFieldDecl obj (prop fld) (Just e)]

object :: [JSObjectField] -> JSExpression
object = JSExpressionLiteral . JSLiteralObject . JSObjectLiteral

-- Expression.Refinement.bind(Expression)
bind :: JSExpression -> String -> JSExpression
bind a b = JSStmtAsExpression $ JSBindApply (getField (a,b)) bindProp (invoke' [a])

-- Since this line of code needs to be created many times, function creates the code which checks if the crafty entity has a specific component
craftyHas :: JSName -> CraftyComponent -> JSExpression
craftyHas obj cc = stmtAsExpr obj has (invoke' [exstr cc])

-- Generate crafty js statement to trigger a crafty event
craftyTrigger :: String -> JSStatement
craftyTrigger event = statement crafty trigger (invoke' [exstr event])

craftyGet :: [JSExpression] -> JSExpression
craftyGet comp = JSStmtAsExpression $ JSEApply (pure $ JSLValue crafty [([invk comp], get)]) (invoke' [])

craftyDestroy :: JSName -> JSStatement
craftyDestroy jsname = statement jsname destroy (invoke' [])

-- Line of code created so often, replaced with a function
-- Adds the JSExpressions (strings) as components to the crafty object created
-- name.addComponent(exprs)
craftyAddComponent :: JSName -> [JSExpression] -> JSStatement
craftyAddComponent n exprs = statement n addComponent (invoke' exprs)

craftyRemoveComponent :: JSName -> [JSExpression] -> JSStatement
craftyRemoveComponent n exprs = statement n removeComponent (invoke' exprs)

-- Check if the object has the field f (obj.hasOwnProperty(field))
objectHasField :: JSName -> Field -> JSExpression
objectHasField obj f = stmtAsExpr obj hasOwnProperty (invoke' [exstr f])

-- Check if current key pressed is the key we are checking for
checkIfKeyPressed :: String -> JSExpression
checkIfKeyPressed key = excond Eq (getField (exnm' keyE, "key")) $ getField (getField (exnm' crafty, "keys"), key)

setPlayerFacing :: String -> JSFunctionBody
setPlayerFacing direction = functionBody [setField (playerExpr, "facing") $ exstr direction] []

getSelfField :: Field -> JSExpression
getSelfField fld = getField (selfExpr, fld)

getCraftyColliderData :: [JSVarStatement]
getCraftyColliderData = [
  var collider (getField (arrayVal dataSetExpr i', "obj")),
  var colliderNm (stmtAsExpr collider getName (invoke' []))
  ]

checkWinCond :: JSStatement
checkWinCond = statement' winConditionCheck $ invoke' [colliderNameExpr]

cameraView :: JSExpression
cameraView = getField (typeExpr, "view")

setNameStmt :: JSName -> [JSExpression] -> JSStatement
setNameStmt jsname invExprs = statement jsname setName $ invoke' invExprs

craftyShift :: JSName -> [JSExpression] -> JSStatement
craftyShift jsname exprs = statement jsname shift $ invoke' exprs

craftyGameOver :: JSFunctionBody
craftyGameOver = functionBody [] [statement crafty enterScene $ invoke' [gameOver]]

enviroHudUpdate :: JSStatement
enviroHudUpdate = statement enviro hudUpdate (invoke' [])

or :: JSExpression -> JSExpression -> JSExpression
or a b = excond Or a b

and :: JSExpression -> JSExpression -> JSExpression
and a b = excond And a b

mul :: JSExpression -> JSExpression -> JSExpression
mul a b = excond Mul a b

div :: JSExpression -> JSExpression -> JSExpression
div a b = excond Div a b

sub :: JSExpression -> JSExpression -> JSExpression
sub a b = excond Sub a b

add :: JSExpression -> JSExpression -> JSExpression
add a b = excond Add a b

lte :: JSExpression -> JSExpression -> JSExpression
lte a b = excond LTE a b

eq :: JSExpression -> JSExpression -> JSExpression
eq a b = excond Eq a b

gte :: JSExpression -> JSExpression -> JSExpression
gte a b = excond GTE a b