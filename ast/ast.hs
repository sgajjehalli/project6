module Ast where

-- Definition of the Expr data type
data Expr
    = Plus Expr Expr     -- Addition
    | Minus Expr Expr    -- Subtraction
    | Times Expr Expr    -- Multiplication
    | Div Expr Expr      -- Division
    | Literal Float      -- Literal value
    deriving (Show, Eq)

-- Evaluates an expression
eval :: Expr -> Float
eval (Literal x) = x
eval (Plus e1 e2) = eval e1 + eval e2
eval (Minus e1 e2) = eval e1 - eval e2
eval (Times e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 / eval e2

-- Checks structural and semantic equality of two expressions
equals :: Expr -> Expr -> Bool
equals (Literal x) (Literal y) = x == y
equals (Plus e1 e2) (Plus e3 e4) = equals e1 e3 && equals e2 e4
equals (Minus e1 e2) (Minus e3 e4) = equals e1 e3 && equals e2 e4
equals (Times e1 e2) (Times e3 e4) = equals e1 e3 && equals e2 e4
equals (Div e1 e2) (Div e3 e4) = equals e1 e3 && equals e2 e4
equals _ _ = False

-- Test expressions (revised to be Expr types)
test1 :: Expr
test1 = Plus (Literal 3.0) (Literal 2.0)

test2 :: Expr
test2 = Plus (Literal 3.0) (Div (Literal 1.0) (Literal 2.0))

test3 :: Expr
test3 = Plus (Times (Literal 3.0) (Literal 5.0)) (Div (Literal 1.0) (Literal 2.0))
