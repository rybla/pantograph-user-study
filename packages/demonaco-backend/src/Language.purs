module Language where

import Prelude

import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Expr ((%*), (%))
import Data.Expr as Expr
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))
import Debug (trace, traceM)
import Grammar as Grammar
import Text.Pretty (pretty)
import Util as Util

data DataType
    = Bool
    | Int

derive instance Generic DataType _
instance Show DataType where show x = genericShow x
instance Eq DataType where eq x = genericEq x
instance Ord DataType where compare x y = genericCompare x y
instance Expr.IsExprLabel DataType

data SortLabel
  -- Judgements
  = TermSort {-Ctx-} {-Type-}
  | TypeSort {-Type-}
  -- Contexts
  | CtxConsSort String {-Type-} {-Ctx-}
  | CtxNilSort
  -- Types
  | DataType DataType
  | Arrow {-Type-} {-Type-}
  | List {-Type-}

appendSpaced :: String -> String -> String
appendSpaced str1 str2 | String.null str1 = str2
appendSpaced str1 str2 | String.null str2 = str1
appendSpaced str1 str2 = str1 <> " " <> str2

infixr 5 appendSpaced as <+>

surround :: String -> String -> String -> String
surround left right str = left <> str <> right
parens = surround "(" ")"

printSortImpl :: (SortLabel /\ Array String) -> String
printSortImpl (TermSort /\ [gamma, ty]) = "Term" <+> parens gamma <+> ty
printSortImpl (TypeSort /\ [t]) = "Type" <+> parens t
--printSortImpl (CtxConsSort x /\ [ty, "∅"]) = x <> ":" <> ty
printSortImpl (CtxConsSort x /\ [ty, gamma]) = x <> ":" <> ty <> ", " <> gamma
printSortImpl (CtxNilSort /\ []) = "∅"
printSortImpl (DataType ty /\ []) = show ty
printSortImpl (Arrow  /\ [a, b]) = parens $ a <> " -> " <> b
printSortImpl (List  /\ [t]) = "List" <+> t
printSortImpl _ = Util.bug "printSortImpl"

printSort :: Sort -> String
printSort ((Expr.MInj s) % kids) = printSortImpl (s /\ (map printSort kids))
printSort (Expr.MV x % _) = pretty x

derive instance Generic SortLabel _
instance Show SortLabel where show x = genericShow x
instance Eq SortLabel where eq x = genericEq x
instance Ord SortLabel where compare x y = genericCompare x y
instance Expr.IsExprLabel SortLabel

type Sort = Grammar.Sort SortLabel

dataTypeSort :: String -> DataType
dataTypeSort = case _ of
    "Int" -> Int
    "Bool" -> Bool
    _ -> Util.bug "dataTypeSort error"

constantType :: String -> Maybe Sort {-type-}
constantType = case _ of
    "true" -> pure $ DataType Bool %* []
    "false" -> pure $ DataType Bool %* []
    "not" -> pure $ Arrow %* [DataType Bool %* [], DataType Bool %* []]
    _ -> Nothing

infixTypes :: String -> {left :: Sort, right :: Sort, output :: Sort}
infixTypes op =
    let int = DataType Int %* [] in
    let bool = DataType Bool %* [] in
    case op of
    "+" -> {left : int, right : int, output : int}
    "-" -> {left : int, right : int, output : int}
    "*" -> {left : int, right : int, output : int}
    "/" -> {left : int, right : int, output : int}
    "%" -> {left : int, right : int, output : int}
    "^" -> {left : int, right : int, output : int}
    "<" -> {left : int, right : int, output : bool}
    ">" -> {left : int, right : int, output : bool}
    "<=" -> {left : int, right : int, output : bool}
    ">=" -> {left : int, right : int, output : bool}
    "&&" -> {left : bool, right : bool, output : bool}
    "||" -> {left : bool, right : bool, output : bool}
    _ -> Util.bug "infixTypes error"

{-
In order to deal with named variables, we have a partial sort. This is a sort (with metavariables) that is known about the
term up to that point in typechecking. It is guaranteed to at least have whatever information comes from the parent (and its parents recursively)
Which includes the broad structure of the context.

A property that this function sould have is that it should respect substitutiono on "partialSort".
-}


{-
TODO: I don't think I need the concept of rule metavars. I think each can just be a fresh metavar. It doesn't matter where it comes from.
-}

findInCtx :: Sort -> String -> Maybe Sort
findInCtx sort name =
    let impl :: Sort -> Maybe Sort
        impl (Expr.MInj (CtxConsSort name') % [ty, gamma]) =
            if name == name' then pure ty else impl gamma
        impl sort = Nothing
    in
    case sort of
    (Expr.MInj TermSort) % [ctx, _ty] -> impl ctx
    _ -> Util.bug ("bug in findInCtx: sort not right " <> printSort sort)


data ErrorMsg = UnificationError Sort Sort | HoleMessage Sort | UnboundVariable String

language :: Sort -> String -> String -> String -> Either ErrorMsg (Grammar.Rule SortLabel)
language partialSort label dataa dataa2 = case label of

  "TOP" -> pure $ Grammar.makeRule ["x"] \[x] ->
    [x]
    /\ -------
    x

  "PARENTERM" -> pure $ Grammar.makeRule ["x"] \[x] ->
    [x]
    /\ -------
    x

  "PARENTYPE" -> pure $ Grammar.makeRule ["x"] \[x] ->
    [x]
    /\ -------
    x

  "FUN" -> pure $ Grammar.makeRule ["gamma", "a", "b"] \[gamma, a, b] ->
    [ TypeSort %* [a]
    , TermSort %* [CtxConsSort dataa %* [a, gamma], b] ]
    /\ --------
    ( TermSort %* [gamma, Arrow %* [a, b]])

  "FUN_NOANN" -> pure $ Grammar.makeRule ["gamma", "a", "b"] \[gamma, a, b] ->
    [ TermSort %* [CtxConsSort dataa %* [a, gamma], b] ]
    /\ --------
    ( TermSort %* [gamma, Arrow %* [a, b]])
  
  "IF" -> pure $ Grammar.makeRule ["gamma", "ty"] \[gamma, ty] ->
    [ TermSort %* [gamma, DataType Bool %* []]
      , TermSort %* [gamma, ty]
      , TermSort %* [gamma, ty]]
    /\ -------
    ( TermSort %* [gamma, ty] )

  "APP" -> pure $ Grammar.makeRule ["gamma", "a", "b"] \[gamma, a, b] ->
    [ TermSort %* [gamma, Arrow %* [a, b]]
    , TermSort %* [gamma, a] ]
    /\ --------
    ( TermSort %* [gamma, b] )

  "HOLE" -> pure $ Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
    [ TypeSort %* [ty] ]
    /\ --------
    ( TermSort %* [gamma, ty] )

  "LET" -> pure $ Grammar.makeRule ["a", "b", "gamma"] \[a, b, gamma] ->
    [ TypeSort %* [a]
    , TermSort %* [CtxConsSort dataa %* [a, gamma], a]
    , TermSort %* [CtxConsSort dataa %* [a, gamma], b] ]
    /\ --------
    ( TermSort %* [gamma, b])

  "LET_NOANN" -> pure $ Grammar.makeRule ["a", "b", "gamma"] \[a, b, gamma] ->
    [ TermSort %* [CtxConsSort dataa %* [a, gamma], a]
    , TermSort %* [CtxConsSort dataa %* [a, gamma], b] ]
    /\ --------
    ( TermSort %* [gamma, b])

--  TypeHole -> pure $ Grammar.makeRule ["type"] \[ty] ->
--    [ ]
--    /\ --------
--    ( TypeSort %* [ty] )

--  "BASE_TYPE" -> pure $ Grammar.makeRule [] \[] ->
--    []
--    /\ --------
--    ( TypeSort %* [DataType (dataTypeSort dataa) %* []] )

  "BOOL" -> pure $ Grammar.makeRule [] \[] ->
    []
    /\ --------
    ( TypeSort %* [DataType Bool %* []] )

  "INT" -> pure $ Grammar.makeRule [] \[] ->
    []
    /\ --------
    ( TypeSort %* [DataType Int %* []] )

  "LIST" -> pure $ Grammar.makeRule ["type"] \[ty] ->
    [TypeSort %* [ty]]
    /\ -------
    ( TypeSort %* [List %* [ty]] )

  "ARROW_TYPE" -> pure $ Grammar.makeRule ["a", "b"] \[a, b] ->
    [TypeSort %* [a], TypeSort %* [b]]
    /\ --------
    ( TypeSort %* [Arrow %* [a, b]] )

  "IF" -> pure $ Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
      [ TermSort %* [gamma, DataType Bool %* []]
      , TermSort %* [gamma, ty]
      , TermSort %* [gamma, ty] ]
      /\ -------
      ( TermSort %* [gamma, ty] )

  "NAME" | Just ty <- constantType dataa -> pure $ Grammar.makeRule ["gamma"] \[gamma] ->
    []
    /\ -------
    (TermSort %* [gamma, ty])

  "INTEGER" -> pure $ Grammar.makeRule ["gamma"] \[gamma] ->
    []
    /\ -------
    ( TermSort %* [gamma, DataType Int %* []])

  "INFIX_EXPRESSION" ->
    let {left, right, output} = infixTypes dataa in
    pure $ Grammar.makeRule ["gamma"] \[gamma] ->
    [ TermSort %* [gamma, left], TermSort %* [gamma, right] ]
    /\ -------
    ( TermSort %* [gamma, output])

  "EQUALS" -> pure $ Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
    [ TermSort %* [gamma, ty], TermSort %* [gamma, ty] ]
    /\ -------
    ( TermSort %* [gamma, DataType Bool %* []])

  "NAME" | dataa == "nil" -> pure $ Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
    []
    /\ -------
    ( TermSort %* [gamma, List %* [ty]])

  "NAME" | dataa == "cons" -> pure $ Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
    []
    /\ -------
    ( TermSort %* [gamma, Arrow %* [ty, Arrow %* [List %* [ty], List %* [ty]]]])

  "NAME" | dataa == "head" -> pure $ Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
    []
    /\ -------
    ( TermSort %* [gamma, Arrow %* [List %* [ty], ty]])

  "NAME" | dataa == "tail" -> pure $ Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
    []
    /\ -------
    ( TermSort %* [gamma, Arrow %* [List %* [ty], List %* [ty]]])

  "NAME" | dataa == "index" -> pure $ Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
    []
    /\ -------
    ( TermSort %* [gamma, Arrow %* [List %* [ty], Arrow %* [DataType Int %* [], ty]]])

  "NAME" | dataa == "length" -> pure $ Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
    []
    /\ -------
    ( TermSort %* [gamma, Arrow %* [List %* [ty], DataType Int %* []]])

  "NAME" | dataa == "append" -> pure $ Grammar.makeRule ["gamma", "type"] \[gamma, ty] ->
    []
    /\ -------
    ( TermSort %* [gamma, Arrow %* [List %* [ty], Arrow %* [List %* [ty], List %* [ty]]]])

  "NAME" ->
            case findInCtx partialSort dataa of
                Nothing -> Left (UnboundVariable dataa)
                Just ty ->
                    pure $ Grammar.makeRule
                    ["gamma"]
                    \[gamma] -> []
                                /\ -----
                                ( TermSort %* [gamma, ty])

  "MATCH" -> pure $ Grammar.makeRule ["gamma", "type", "outTy"] \[gamma, ty, outTy] ->
    [ TermSort %* [gamma, List %* [ty]]
    , TermSort %* [gamma, outTy]
    , TermSort %* [CtxConsSort dataa %* [ty, CtxConsSort dataa2 %* [List %* [ty], gamma]], outTy]]
    /\ -------
    ( TermSort %* [gamma, outTy])

--  Comment -> pure $ Grammar.makeRule ["c", "gamma", "type"] \[c, g, t] ->
--    [ TypeOfLabel SortString %* [c]
--    , TermSort %* [g, t] ]
--    /\ --------
--    ( TermSort %* [g, t] )
  _ -> Util.bug ("language constructor not found: " <> label)

