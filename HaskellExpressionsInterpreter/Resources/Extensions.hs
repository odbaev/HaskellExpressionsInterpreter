{-# LANGUAGE TypeOperators #-}

import Control.Applicative
import Data.Functor.Identity
import Data.Traversable

import Debug.SimpleReflect
import Debug.Reflect


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

sequenceA' :: (Show a, Show (f a), Show (f [a]), Show (f ([a] ~> [a])), Applicative f) => [f a] -> Ap (f [a])
sequenceA' [] = pure'' []
sequenceA' (x:xs) = Val ap' :$ ((.:) -$- x) :$ sequenceA' xs

traverse' :: (Show a, Show b, Show (f b), Show (f [b]), Show (f ([b] ~> [b])), Applicative f) => (a -> f b) -> [a] -> Ap (f [b])
traverse' _ [] = pure'' []
traverse' f (x:xs) = Val ap' :$ (Val fmap' :$ Val (.:) :$ fx) :$ traverse' f xs
  where fx = Val (Fn "f" f) :$ Val x