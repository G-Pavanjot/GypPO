-- Module adapted from https://github.com/sseefried/js-good-parts

module AST.JS (
  JSString, JSName,
  unJSString, unJSName,
  jsString, jsName,

  -- * Data types
  JSDouble(..), JSFloat(..),
  JSVarStatement(..), JSVarDecl(..), JSFieldDecl(..), JSProtoDecl(..), JSStatement(..), JSSelfDecl(..), JSSelfArrayDecl(..), JSSelfArrayFieldDecl(..),
  JSExpressionStatement(..), JSLValue(..), JSRLValue(..), JSRValue(..), JSExpression(..), JSSwitchStatement(..), JSDisruptiveStatement(..),
  JSInvocation(..), JSRefinement(..), JSForStatement(..), JSIfStatement(..), JSReturnStatement(..), JSBreakStatement(..), JSArrayVal(..),
  JSGetField(..), JSLiteral(..), JSObjectLiteral(..), JSObjectField(..), JSArrayLiteral(..), JSCaseAndDisruptive(..), JSCaseClause(..),
  JSFunctionLiteral(..), JSFunctionBody(..), JSProgram(..),
  InfixOp(..), JSExpressionStmt(..)
) where

import Text.PrettyPrint.Leijen
import Prelude hiding ((<$>),GT,LT)
import Data.List.NonEmpty (NonEmpty, toList)

data JSName = JSName { unJSName :: String }

jsName :: String -> JSName
jsName = JSName

data JSString = JSString { unJSString :: String }

jsString :: String -> JSString
jsString = JSString

newtype JSDouble = JSDouble Double
newtype JSFloat = JSFloat Float

data JSVarStatement
  = JSVarStatement [JSVarDecl]
  | JSSelfStatement [JSSelfDecl]
  | JSSelfArrayStatement [JSSelfArrayDecl]
  | JSSelfArrayFieldStatement [JSSelfArrayFieldDecl]
  | JSProtoVarStatement [JSProtoDecl]
  | JSFieldVarStatement [JSFieldDecl]
  | JSCommentStatement       JSExpression
data JSVarDecl = JSVarDecl JSName (Maybe JSExpression)

-- Very similar to JSVarDecl but does not need the "var" keyword in the generated JS (self.name = antag.name)
data JSSelfDecl = JSSelfDecl JSName JSExpression

-- Similar to JSSelfDecl but instead of self.field, the field is an array (example: self.antags[name] = antag)
data JSSelfArrayDecl = JSSelfArrayDecl JSName JSExpression JSExpression

-- Similar to JSSelfDecl but instead of self.field, its self.array[x].field (example: self.antags[name].name = antag.name)
data JSSelfArrayFieldDecl = JSSelfArrayFieldDecl JSName JSExpression JSName JSExpression

-- To create JSVarStatement of the type (JSName.prototype.JSName = expression)
data JSProtoDecl = JSProtoDecl JSName JSName JSExpression

-- set the field of an object as a variable statement (obj.field = expr)
data JSFieldDecl = JSFieldDecl JSExpression JSRefinement (Maybe JSExpression)

data JSStatement
  = JSStatementExpression   JSExpressionStatement
  | JSStatementFor          (Maybe JSName) JSForStatement
  | JSStatementDisruptive   JSDisruptiveStatement
  | JSStatementIf           JSIfStatement
  | JSStatementSwitch       (Maybe JSName) JSSwitchStatement

data JSDisruptiveStatement
  = Break     JSBreakStatement
  | Return    JSReturnStatement

data JSExpressionStatement
  = JSESApply (NonEmpty JSLValue) JSRValue

data JSLValue = JSLValue JSName [([JSInvocation], JSRefinement)]

data JSRValue
  = JSRVInvoke    (NonEmpty JSInvocation)
  | JSRVRefinement

data JSExpression
  = JSExpressionLiteral    JSLiteral
  | JSExpressionName       JSName
  | JSExpressionInvocation JSExpression     JSInvocation
  | JSExpressionRefinement JSExpression     JSRefinement
  | JSExpressionNew        JSExpression     JSInvocation
  | JSExpressionArray      JSArrayVal
  | JSExpressionField      JSExpression     JSRefinement
  | JSExpressionInfix      InfixOp          JSExpression     JSExpression
  | JSExpressionTernary    JSExpression     JSExpression     JSExpression -- Expression? Expression : Expression
  | JSExpressionArrow      JSName           JSExpression
  | JSStmtAsExpression     JSExpressionStmt

data JSExpressionStmt
  = JSEApply (NonEmpty JSLValue) JSRValue
  | JSBindApply JSExpression JSRefinement JSRValue

data InfixOp
  = Mul  -- *
  | Div  -- /
  | Mod  -- %
  | Add  -- +
  | Sub  -- -
  | GTE  -- >=
  | LTE  -- <=
  | GT   -- >
  | LT   -- <
  | Eq   -- ==
  | NotEq-- !=
  | Or   -- ||
  | And  -- &&

data JSInvocation = JSInvocation [JSExpression]

data JSRefinement
  = JSProperty JSName
  | JSSubscript JSExpression

data JSLiteral
  = JSLiteralDouble   JSDouble
  | JSLiteralFloat    JSFloat
  | JSLiteralString   JSString
  | JSLiteralObject   JSObjectLiteral
  | JSLiteralArray    JSArrayLiteral
  | JSLiteralFunction JSFunctionLiteral

data JSObjectLiteral = JSObjectLiteral [JSObjectField]
data JSObjectField  = JSObjectField (Either JSName JSString) JSExpression
data JSArrayLiteral = JSArrayLiteral [JSExpression]
data JSFunctionLiteral = JSFunctionLiteral (Maybe JSName) [JSName] JSFunctionBody
data JSFunctionBody = JSFunctionBody [JSVarStatement] [JSStatement]
data JSProgram = JSProgram [JSVarStatement] [JSStatement]

data JSForStatement
  = ForStatementCStyle
       (Maybe JSExpressionStatement)
       (Maybe JSExpression)
       (Maybe JSExpressionStatement)
       JSFunctionBody
    | ForStatementInStyle
       JSName
       JSExpression
       JSFunctionBody

-- Examples:
-- if expression then statements
-- if expression then statements else statements
-- if expression then statements else (if statement)
data JSIfStatement = JSIfStatement JSExpression
                                        JSFunctionBody
                                        (Maybe (Either JSFunctionBody JSStatement))

-- Switch (expr) {cases:..}
data JSSwitchStatement
  = JSSwitchStmtSingleCase JSExpression JSCaseClause -- Single case
    | JSSwitchStatement JSExpression 
                            [JSCaseAndDisruptive] -- Case clauses 
                            [JSStatement] -- Default statements

-- Case x: 
--    y = 2; 
--    break;
data JSCaseAndDisruptive = JSCaseAndDisruptive JSCaseClause JSDisruptiveStatement

data JSCaseClause = JSCaseClause JSExpression [JSStatement]                      

data JSReturnStatement
  = JSReturnStatement (Maybe JSExpression)
  | JSRSApply (NonEmpty JSRLValue) JSRValue
  | JSArrayReturn (Maybe JSName) JSExpression JSName

data JSBreakStatement = JSBreakStatement (Maybe JSName)

data JSRLValue = JSRLValue JSExpression [([JSInvocation], JSRefinement)]

data JSArrayVal = JSArrayVal JSExpression JSExpression

data JSGetField = JSGetField JSName JSExpression

instance Pretty JSString where
  pretty s = char '\'' <> text (unJSString s) <> char '\''

instance Pretty JSName where
  pretty = text . unJSName

sepWith :: Pretty a => Doc -> [a] -> Doc
sepWith s = encloseSep empty empty s . map pretty

sepWith' :: Pretty a => Doc -> NonEmpty a -> Doc
sepWith' s = encloseSep empty empty s . map pretty . toList

endWith :: Pretty a => Doc -> [a] -> Doc
endWith s xs = sepWith s xs <> s

params :: Pretty a => [a] -> Doc
params x = parens $ hcat $ prettifyList x

prettifyList :: Pretty a => [a] -> [Doc]
prettifyList [] = []
prettifyList [x] = [pretty x]
prettifyList (x:xs) = [pretty x <> comma <+> empty] ++ prettifyList xs

---------------------------

instance Pretty JSDouble where
  pretty (JSDouble x) = pretty x

instance Pretty JSFloat where
  pretty (JSFloat x) = pretty x

instance Pretty JSVarStatement where
  pretty (JSVarStatement x) = (hcat $ prettifyList x) <> semi
  pretty (JSSelfStatement x) = (hcat $ prettifyList x) <> semi
  pretty (JSSelfArrayStatement x) = (hcat $ prettifyList x) <> semi
  pretty (JSSelfArrayFieldStatement x) = (hcat $ prettifyList x) <> semi
  pretty (JSProtoVarStatement x) = (hcat $ prettifyList x) <> semi
  pretty (JSFieldVarStatement x) = (hcat $ prettifyList x) <> semi
  pretty (JSCommentStatement cmnt) = text "/*" <+> pretty cmnt <+> text "*/"

instance Pretty JSVarDecl where
  pretty (JSVarDecl x Nothing)    = text "var" <+> pretty x
  pretty (JSVarDecl x (Just e)) = text "var" <+> pretty x <+> text "=" <+> pretty e

instance Pretty JSSelfDecl where
  pretty (JSSelfDecl x e) = pretty x <+> text "=" <+> pretty e

instance Pretty JSSelfArrayDecl where
  pretty (JSSelfArrayDecl x f e) = pretty x <+> text "." <+> pretty f <+> text "=" <+> pretty e 

instance Pretty JSSelfArrayFieldDecl where
  pretty (JSSelfArrayFieldDecl x a f e) = pretty x <+> text "." <+> pretty a <+>  text "." <+> pretty f <+> text "=" <+> pretty e 

instance Pretty JSProtoDecl where
  pretty (JSProtoDecl f s e) = pretty f <+> text ".prototype." <+> pretty s <+> text "=" <+> pretty e

instance Pretty JSFieldDecl where
  pretty (JSFieldDecl o f Nothing)  = pretty o <> pretty f
  pretty (JSFieldDecl o f (Just e)) = pretty o <> pretty f <+> text "=" <+> pretty e

instance Pretty JSStatement where
  pretty stmt = case stmt of
    (JSStatementExpression  es)  -> pretty es
    (JSStatementDisruptive dis) -> pretty dis
    (JSStatementFor lbl fs) -> pp lbl fs
    (JSStatementIf is)      -> pretty is
    (JSStatementSwitch lbl sw)  -> pp lbl sw 
    where
      pp :: Pretty a => Maybe JSName -> a -> Doc
      pp (Just label) doc = pretty label <> colon <+> pretty doc
      pp Nothing doc = pretty doc

instance Pretty JSDisruptiveStatement where
  pretty dis = case dis of
    (Break br) -> pretty br
    (Return ret) -> pretty ret

instance Pretty JSExpressionStatement where
  pretty (JSESApply lvalues rvalue) =
    sepWith' (space <> text "=" <> space) lvalues <> pretty rvalue <> semi

instance Pretty JSExpressionStmt where
  pretty (JSEApply lvalues rvalue) =
    sepWith' (space <> text "=" <> space) lvalues <> pretty rvalue
  pretty (JSBindApply expr refine rvalue) = pretty expr <> pretty refine <> pretty rvalue

instance Pretty JSLValue              where
  pretty (JSLValue name invsAndRefines) = pretty name <> (hcat . map ppIR $ invsAndRefines)
    where
      ppIR (invs, refine) = (hcat . map pretty $ invs) <> pretty refine

instance Pretty JSRValue              where
  pretty rvalue = case rvalue of
    JSRVInvoke invs -> hcat . toList . fmap pretty $ invs
    JSRVRefinement -> text ""

instance Pretty JSExpression where
  pretty e = case e of
    JSExpressionLiteral literal      -> pretty literal
    JSExpressionName name            -> pretty name
    JSExpressionInvocation x i       -> pretty x <> pretty i
    JSExpressionRefinement x r       -> pretty x <> pretty r
    JSExpressionNew x i              -> text "new" <+> pretty x <> pretty i
    JSExpressionArray arr            -> pretty arr
    JSExpressionField name fld       -> pretty name <> pretty fld
    JSExpressionInfix infx e1 e2     -> pretty e1 <+> text (infixOpSymbol(infx)) <+> pretty e2
    JSExpressionTernary cond thn els -> pretty cond <+> char '?' <+> pretty thn <+> colon <+> pretty els
    JSExpressionArrow param stmts    -> pretty param <+> text "=>" <+> pretty stmts
    JSStmtAsExpression se            -> pretty se

instance Pretty JSInvocation          where
  pretty (JSInvocation x)  = params x

instance Pretty JSRefinement          where
  pretty (JSProperty a) = char '.' <> pretty a
  pretty (JSSubscript e)   = char '[' <> pretty e <> char ']'

instance Pretty JSLiteral             where
  pretty lit = case lit of
    JSLiteralDouble x   -> pretty x
    JSLiteralFloat x    -> pretty x
    JSLiteralString s   -> pretty s
    JSLiteralObject o   -> pretty o
    JSLiteralArray  a   -> text "[" <+> pretty a <+> text "]"
    JSLiteralFunction f -> pretty f

instance Pretty JSObjectLiteral       where
  pretty (JSObjectLiteral f) =
    lbrace <$>
    indent 2 (sepWith comma (map pretty f)) <$>
    rbrace

instance Pretty JSObjectField         where
  pretty (JSObjectField eitherNameString e) = ppEitherNameString <> colon <+> pretty e
    where ppEitherNameString = case eitherNameString of
                 Left name -> pretty name
                 Right s   -> pretty s

instance Pretty JSArrayLiteral        where
  pretty (JSArrayLiteral es) = sepWith (comma <+> empty) es

instance Pretty JSFunctionLiteral     where
  pretty (JSFunctionLiteral mbName p body) =
    text "function" `join` (params p) <+> pretty body
      where join = case mbName of
                     Just name -> (\a b -> a <+> pretty name <> b)
                     Nothing   -> (<>)

instance Pretty JSFunctionBody        where
  pretty (JSFunctionBody varStmts stmts) =
    lbrace <$>
    indent 2 (sepWith empty (map pretty varStmts ++ map pretty stmts)) <$>
    rbrace

instance Pretty JSProgram where
  pretty (JSProgram varStmts stmts) = vcat (map pretty varStmts ++ map pretty stmts)

instance Pretty JSForStatement where
  pretty (ForStatementCStyle init_ cond incr stmts) =
    text "for" <+> parens (pretty init_ <> semi <+> pretty cond <> semi <+>
                           pretty incr) <+> pretty stmts
  pretty (ForStatementInStyle nm exp' stmts) =
    text "for" <+> parens (pretty nm <+> text "in" <+> pretty exp') <+> pretty stmts

instance Pretty JSSwitchStatement where
  pretty (JSSwitchStmtSingleCase cond caseC) =
    text "switch" <+> parens (pretty cond) <+> lbrace <$> pretty caseC <$> rbrace
  pretty (JSSwitchStatement cond cases dflt) =
    text "switch" <+> parens (pretty cond) <+> lbrace <$>
      indent 2 (vcat (fmap pretty $ cases) <$> (text "default:" <$> indent 2 (endWith semi dflt))) <$> rbrace

instance Pretty JSCaseAndDisruptive where
  pretty (JSCaseAndDisruptive caseC disrupt) = pretty caseC <$> pretty disrupt

instance Pretty JSCaseClause where
  pretty (JSCaseClause exprs stmts) =
    text "case" <+> pretty exprs <> colon <+> endWith semi stmts

instance Pretty JSIfStatement where
  pretty (JSIfStatement cond thenStmts elseOrIf) =
    text "if" <+> parens (pretty cond) <+> pretty thenStmts <+> eOI
    where
      eOI = case elseOrIf of
        Nothing               -> empty
        Just (Left elseStmts) -> text "else" <+> pretty elseStmts
        Just (Right ifStmt)   -> pretty ifStmt

instance Pretty JSArrayVal where
  pretty (JSArrayVal array index) =
    pretty array <+> lbracket <$> pretty index <$> rbracket

instance Pretty JSBreakStatement where
  pretty (JSBreakStatement maybExp) = case maybExp of
    Nothing -> text "break;"
    Just exprs -> text "break" <+> pretty exprs <> semi 

instance Pretty JSReturnStatement where
  pretty rs = case rs of
    (JSRSApply lvalues rvalues) -> sepWith' (space <> text "=" <> space) lvalues <> pretty rvalues <> semi
    (JSReturnStatement e) -> case e of
                                Nothing  -> text "return;"
                                Just expr -> text "return" <+> pretty expr <> semi
    (JSArrayReturn s e f) -> case s of
                                Nothing   -> text "return" <+> pretty e <+> text "." <+> pretty f <> semi
                                Just slf  -> text "return" <+> pretty slf <+> text "." <+> pretty e <+> text "." <+> pretty f <> semi

instance Pretty JSRLValue              where
  pretty (JSRLValue name invsAndRefines) = text "return" <+> pretty name <> (hcat . map ppIR $ invsAndRefines)
    where
      ppIR (invs, refine) = (hcat . map pretty $ invs) <> pretty refine

infixOpSymbol :: InfixOp -> String
infixOpSymbol op = case op of
  Mul   -> "*"
  Div   -> "/"
  Mod   -> "%"
  Add   -> "+"
  Sub   -> "-"
  GTE   -> ">="
  LTE   -> "<="
  GT    -> ">"
  LT    -> "<"
  Eq    -> "=="
  NotEq -> "!="
  Or    -> "||"
  And   -> "&&"
