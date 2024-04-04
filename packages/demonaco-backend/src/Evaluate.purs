module Evaluate where

import Data.Expr
import Data.Tuple.Nested
import Foreign
import MaybeJs
import Prelude
import Prelude

import Data.Either (Either(..))
import Data.Either as Either
import Data.Int (pow, fromString)
import Data.Lazy (Lazy, defer, force)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Debug as Debug
import Effect.Exception.Unsafe as Exception
import Util as Util
import Debug (trace, traceM)
import Data.Map as Map
import Data.Map (Map)
import Data.Array as Array

-- NOTE: you can throw an catchable exceptions using Exception.unsafeThrow
-- evaluate :: Ast -> Ast
-- evaluate _ = Exception.unsafeThrow "unimplemented: evaluate"

evaluate :: Ast -> String
evaluate dterm =
    let res = realCatchException Left Right (\_ -> eval Map.empty dterm) in
    case res of
    Right (Left error) -> case error of
        HoleError -> "Error: hole"
        BoundaryError -> "Error: type boundary"
        FreeVarError -> "Error: unbound variable"
    Right (Right res) -> printValue res
    Left error -> "Error: infinite loop"




data Value = IntVal Int | BoolVal Boolean | ListVal (List Value) | FunVal (Value -> Either Error Value)

eqValue :: Value -> Value -> Boolean
eqValue v1 v2 = case v1 /\ v2 of
    IntVal x /\ IntVal y -> x == y
    BoolVal x /\ BoolVal y -> x == y
    ListVal x /\ ListVal y -> List.all (\x -> x) (List.zipWith eqValue x y)
    _ /\ _ -> false

assertValInt :: Value -> Int
assertValInt = case _ of
    IntVal x -> x
    _ -> Util.bug "assertValint failed"
assertValBool :: Value -> Boolean
assertValBool = case _ of
    BoolVal x -> x
    _ -> Util.bug "assertValint failed"
assertValList :: Value -> (List Value)
assertValList = case _ of
    ListVal x -> x
    _ -> Util.bug "assertValint failed"
assertValFun :: Value -> (Value -> Either Error Value)
assertValFun = case _ of
    FunVal x -> x
    _ -> Util.bug "assertValint failed"

data Error = HoleError | BoundaryError | FreeVarError

--   = { label :: String
--     , dataa :: String
--     , dataa2 :: String
--     , kids :: Array Ast2 -- purescript doesn't allow recursive records.
--     , start_pos :: Pos
--     , end_pos :: Pos
--   }
eval :: (Map String (Lazy (Either Error Value))) -> Ast -> Either Error Value
-- eval env ((Grammar.DerivLabel r _) % kids) =
eval env {label, dataa, dataa2, kids} =
    let children = (map ast2IsAst kids) in
    case label /\ children of
        --   Zero /\ [] -> force $ Util.fromJust' "eval Zero case" $ List.head env
        --   Suc /\ [x] -> eval (Util.fromJust' "eval suc" (List.tail env)) x
          "TOP" /\ [prog] -> eval env prog
          "PARENTERM" /\ [t] -> eval env t
          "FUN" /\ [_ty, t] -> pure $ FunVal (\x -> eval (Map.insert dataa (pure (Right x)) env) t)
          "FUN_NOANN" /\ [t] -> pure $ FunVal (\x -> eval (Map.insert dataa (pure (Right x)) env) t)
          "LET" /\ [_ty, def, body] -> do
            let vDef = eval (Map.insert dataa (defer \_ -> vDef) env) def
            eval (Map.insert dataa (pure vDef) env) body
          "LET_NOANN" /\ [def, body] -> do
            let vDef = eval (Map.insert dataa (defer \_ -> vDef) env) def
            eval (Map.insert dataa (pure vDef) env) body
          "APP" /\ [t1, t2] -> do
            v1 <- eval env t1
            v2 <- eval env t2
            assertValFun v1 v2
          "NAME" /\ [] | Just res <- evalConst dataa -> pure $ res
          "NAME" /\ [] | dataa == "nil" -> pure $ ListVal Nil
          "NAME" /\ [] | dataa == "cons" -> pure $ FunVal (\x -> pure (FunVal (\xs -> pure (ListVal (x : assertValList xs)))))
          "NAME" /\ [] | dataa == "length" -> pure $ FunVal (\xs -> pure (IntVal (List.length (assertValList xs))))
          "NAME" /\ [] | dataa == "append" -> pure $ FunVal (\xs -> pure (FunVal (\ys -> pure (ListVal (assertValList xs <> assertValList ys)))))
          "NAME" /\ [] | dataa == "head" -> pure $ FunVal (\xs -> pure (Util.fromJust (List.head (assertValList xs))))
          "NAME" /\ [] | dataa == "tail" -> pure $ FunVal (\xs -> pure (ListVal (Util.fromJust (List.tail (assertValList xs)))))
          "NAME" /\ [] | dataa == "index" -> pure $ FunVal (\xs -> pure (FunVal (\n -> pure (Util.fromJust (List.index (assertValList xs) (assertValInt n))))))
          "NAME" /\ [] -> force (Util.fromJust (Map.lookup dataa env))
          -- TODO: Deal with the cases where its a constant
          "HOLE" /\ [] -> Left HoleError
          -- TODO: Add If
          "IF" /\ [cond, thenn, elsee] -> do
            vCond <- eval env cond
            if (assertValBool vCond) then eval env thenn else eval env elsee
          "INFIX_EXPRESSION" /\ [t1, t2] -> do
            v1 <- eval env t1
            v2 <- eval env t2
            pure $ evalInfix dataa v1 v2
          "EQUALS" /\ [t1, t2] -> do
            v1 <- eval env t1
            v2 <- eval env t2
            pure $ BoolVal (eqValue v1 v2)
          "MATCH" /\ [li, nilCase, consCase] -> do
            vLi <- eval env li
            case assertValList vLi of
                Nil -> eval env nilCase
                v : vs -> eval (Map.insert dataa (pure (Right v)) (Map.insert dataa2 (pure (Right (ListVal vs))) env)) consCase
          "INTEGER" /\ _ -> pure (IntVal (Util.fromJust (fromString dataa)))
          "NOT" /\ [b] -> do
            vB <- eval env b
            pure $ BoolVal (not (assertValBool vB))
          _ -> Util.bug ("eval case fail: label was " <> label <> " num children was " <> show (Array.length children))
eval _ _ = Util.bug "eval case shouldn't happen"

evalConst :: String -> Maybe Value
evalConst = case _ of
    "true" -> pure $ BoolVal true
    "false" -> pure $ BoolVal false
    -- "not" -> pure $ FunVal (\b -> pure (BoolVal (not (assertValBool b))))
    _ -> Nothing

evalInfix :: String -> (Value -> Value -> Value)
evalInfix op = case op of
    "+" -> \x y -> IntVal (assertValInt x + assertValInt y)
    "-" -> \x y -> IntVal (assertValInt x - assertValInt y)
    "*" -> \x y -> IntVal (assertValInt x * assertValInt y)
    "/" -> \x y -> IntVal (assertValInt x / assertValInt y)
    "%" -> \x y -> IntVal (mod (assertValInt x) (assertValInt y))
    "^" -> \x y -> IntVal (pow (assertValInt x) (assertValInt y))
    "<" -> \x y -> BoolVal (assertValInt x < assertValInt y)
    ">" -> \x y -> BoolVal (assertValInt x > assertValInt y)
    "<=" -> \x y -> BoolVal (assertValInt x <= assertValInt y)
    ">=" -> \x y -> BoolVal (assertValInt x >= assertValInt y)
    "&&" -> \x y -> BoolVal (assertValBool x && assertValBool y)
    "||" -> \x y -> BoolVal (assertValBool x || assertValBool y)
    _ -> Util.bug ("evalInfix case not found: " <> op)

printValue :: Value -> String
printValue val = case val of
    BoolVal x -> show x
    IntVal x -> show x
    ListVal x -> List.foldr (\x xs -> "(cons " <> (printValue x) <> " " <> xs <> ")") "nil" x
    FunVal _ -> "<function>"