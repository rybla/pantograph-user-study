module Typecheck
  ( TypecheckError
  , typecheckTop
  ) where

import Foreign
import Prelude
import Data.Array as Array
import Data.String as String
import Debug (trace)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.State (State)
import Grammar as Grammar
import Language
import Data.Expr as Expr
import Data.Expr ((%*), (%))
import Data.Traversable (sequence)
import Unification
import Control.Monad.Writer.Trans (WriterT)
import Util
import Data.List (List(..), (:))
import Data.List as List
import Data.Either (Either(..))
import Data.Map as Map

type TypecheckError
  = { severity :: String, msg :: String, start_pos :: Pos, end_pos :: Pos }

putErrorsOnHoles :: Ast -> Array TypecheckError
putErrorsOnHoles {label, dataa, dataa2, kids, start_pos, end_pos} =
    let kids' = Array.concat (map (putErrorsOnHoles <<< ast2IsAst) kids) in
    if label == "HOLE" then
        [ {
            severity: "warning"
            , msg: "There'a a hole in your program!!!!"
            , start_pos: start_pos { offset = start_pos.offset + 1.0 }
            , end_pos: end_pos { offset = end_pos.offset + 1.0 }
            }
        ] <> kids'
        else kids'

test :: Ast -> String
test {label, dataa, dataa2, kids, start_pos, end_pos} =
    ("(" <> label <> " [" <> dataa <> "]" <> "[" <> dataa2 <> "]" <> (String.joinWith " " (map (test <<< ast2IsAst) kids)) <> ")")


{-
TODO:
Instead of this being in ExceptT, it should accumulate a bunch of errors.
-}
--inferFImpl :: Sort -> Ast ->  WriterT Unit (State (Grammar.SortSub SortLabel)) Unit
--inferFImpl partialSort {label, dataa, kids, start_pos, end_pos} = do
--    let Grammar.Rule kidSorts parentSort = language partialSort label dataa
--    _ <- unifyFImpl parentSort partialSort
--    _ <- sequence (Array.zipWith inferFImpl kidSorts (map ast2IsAst kids))
----    _ <- sequence $ Array.zipWith (\parentBottom kidTop -> unifyFImpl parentBottom (derivTermSort kidTop)) (kidSorts l) kids
--    pure unit

type Error = {
    start_pos :: Pos
    , end_pos :: Pos
    , msg :: ErrorMsg
}

inferImpl :: Stateful (List Error) -> Stateful (Sub SortLabel) -> Sort -> Ast -> Unit
inferImpl errors sub partialSort {label, dataa, dataa2, kids, start_pos, end_pos} =
    let addErrorMsg msg = errors.set ({start_pos, end_pos, msg} : errors.get unit) in
    case language (Expr.subMetaExprPartially (flattenSub (sub.get unit)) partialSort) label dataa dataa2 of
        Left msg -> addErrorMsg msg
        Right (Grammar.Rule kidSorts parentSort) ->
            let _ = case unify sub parentSort partialSort of
                    Left err -> addErrorMsg (UnificationError parentSort partialSort)
                    Right res ->
                        if label == "HOLE" then
                            addErrorMsg (HoleMessage parentSort)
                        else unit
            in
            let _ = Array.zipWith (inferImpl errors sub) kidSorts (map ast2IsAst kids) in
            unit

printType :: Sort -> String
printType (Expr.MInj TermSort % [gamma, ty]) = printSort ty
printType sort = bug ("printType: wasn't of right form: " <> printSort sort)

typecheckTop :: Ast -> Array TypecheckError
typecheckTop ast =
    trace ("Typechecking ast: " <> test ast) \_ ->
--    trace (test ast) \_ ->
--    putErrorsOnHoles ast
    let errors = stateful Nil in
    let sub = stateful Map.empty in
    let topSort = Expr.MInj TermSort % [Expr.MInj CtxNilSort % [], Grammar.freshMetaVarSort "topty"] in
    let _ = inferImpl errors sub topSort ast in
    let printError {start_pos, end_pos, msg} =
            let applySubs = Expr.subMetaExprPartially (flattenSub (sub.get unit)) in
            case msg of
                UnificationError a b -> {
                       severity: "error"
                       , msg: ("Type error. Actual type: " <> printType (applySubs a) <> "   Expected type: " <> printType (applySubs b))
                       , start_pos: start_pos { offset = start_pos.offset + 1.0 }
                       , end_pos: end_pos { offset = end_pos.offset + 1.0 }
                   }
                UnboundVariable name -> {
                       severity: "error"
                       , msg: ("Unbound variable: " <> name)
                       , start_pos: start_pos { offset = start_pos.offset + 1.0 }
                       , end_pos: end_pos { offset = end_pos.offset + 1.0 }
                   }
                HoleMessage t -> {
                       severity: "warning"
                       , msg: ("Hole with type: " <> printType (applySubs t))
                       , start_pos: start_pos { offset = start_pos.offset + 1.0 }
                       , end_pos: end_pos { offset = end_pos.offset + 1.0 }
                   }
    in
    map printError (Array.fromFoldable (errors.get unit))
