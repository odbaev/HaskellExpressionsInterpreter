{-# LANGUAGE TypeOperators #-}

import Control.Applicative
import Control.Monad.Identity

import Debug.SimpleReflect
import Debug.Reflect


instance Show m => Show (Const m a) where
  show (Const x) = "Const " ++ addParens (show x)
    where addParens s = if ' ' `elem` s then parens s else s

instance Show a => Show (Identity a) where
  show (Identity x) = "Identity " ++ addParens (show x)
    where addParens s@(x:_) = if x /= '(' && ' ' `elem` s then parens s else s

-- (+)
(.+) :: (Num a, Show a) => a ~> a ~> a
(.+) = makeBinOp "+" (+)

-- (-)
(.-) :: (Num a, Show a) => a ~> a ~> a
(.-) = makeBinOp "-" (-)

-- (*)
(.*) :: (Num a, Show a) => a ~> a ~> a
(.*) = makeBinOp "*" (*)

-- (/)
(./) :: (Fractional a, Show a) => a ~> a ~> a
(./) = makeBinOp "/" (/)

-- (^)
(.^) :: (Num a, Show a, Integral b) => a ~> b ~> a
(.^) = makeBinOp "^" (^)

-- (^^)
(.^^) :: (Fractional a, Show a, Integral b) => a ~> b ~> a
(.^^) = makeBinOp "^^" (^^)

-- (**)
(.**) :: (Floating a, Show a) => a ~> a ~> a
(.**) = makeBinOp "**" (**)

-- (<)
(.<) :: (Ord a, Show a) => a ~> a ~> Bool
(.<) = makeBinOp "<" (<)

-- (>)
(.>) :: (Ord a, Show a) => a ~> a ~> Bool
(.>) = makeBinOp ">" (>)

-- (<=)
(.<=) :: (Ord a, Show a) => a ~> a ~> Bool
(.<=) = makeBinOp "<=" (<=)

-- (>=)
(.>=) :: (Ord a, Show a) => a ~> a ~> Bool
(.>=) = makeBinOp ">=" (>=)

-- (==)
(.==) :: (Eq a, Show a) => a ~> a ~> Bool
(.==) = makeBinOp "==" (==)

-- (/=)
(./=) :: (Eq a, Show a) => a ~> a ~> Bool
(./=) = makeBinOp "/=" (/=)

-- (&&)
(.&&) ::  Bool ~> Bool ~> Bool
(.&&) = makeBinOp "&&" (&&)

-- (||)
(.||) ::  Bool ~> Bool ~> Bool
(.||) = makeBinOp "||" (||)

-- (++)
(.++) :: (Show a) => [a] ~> [a] ~> [a]
(.++) = makeBinOp "++" (++)

-- (:)
(.:) :: (Show a) => a ~> [a] ~> [a]
(.:) = makeBinOp ":" (:)

-- (!!)
(.!!) :: (Show a) => [a] ~> Int ~> a
(.!!) = makeBinOp "!!" (!!)

sequenceA :: (Show a, Show (f a), Show (f [a]), Show (f ([a] ~> [a])), Applicative f) => [f a] -> Ap (f [a])
sequenceA [] = pure'' []
sequenceA (x:xs) = Val ap' :$ ((.:) -$- x) :$ sequenceA xs

traverse :: (Show a, Show b, Show (f b), Show (f [b]), Show (f ([b] ~> [b])), Applicative f) => (a -> f b) -> [a] -> Ap (f [b])
traverse _ [] = pure'' []
traverse f (x:xs) = Val ap' :$ (Val fmap' :$ Val (.:) :$ fx) :$ traverse f xs
  where fx = Val (Fn "f" f) :$ Val x


main :: IO ()
main = mapM_ print . reductions $ 