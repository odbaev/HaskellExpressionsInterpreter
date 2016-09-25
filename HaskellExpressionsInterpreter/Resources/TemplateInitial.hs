import Control.Applicative
import Control.Monad.Identity
import Data.Traversable
import Debug.SimpleReflect


parens :: String -> String
parens s = "(" ++ s ++ ")"

instance Show m => Show (Const m a) where
  show (Const x) = "Const " ++ addParens (show x)
    where addParens s = if ' ' `elem` s then parens s else s

instance Show a => Show (Identity a) where
  show (Identity x) = "Identity " ++ addParens (show x)
    where addParens s@(x:_) = if x /= '(' && ' ' `elem` s then parens s else s


main :: IO ()
main = print $ 