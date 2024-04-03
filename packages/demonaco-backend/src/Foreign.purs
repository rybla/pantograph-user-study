module Foreign where

import Prelude
import Unsafe.Coerce (unsafeCoerce)

type Pos
  = { overallPos :: Number
    , line :: Number
    , offset :: Number
    }

type Ast2 = {}

type Ast
  = { label :: String
    , dataa :: String
    , dataa2 :: String
    , kids :: Array Ast2 -- purescript doesn't allow recursive records.
    , start_pos :: Pos
    , end_pos :: Pos
  }

ast2IsAst :: Ast2 -> Ast
ast2IsAst x = unsafeCoerce x