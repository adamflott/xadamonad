module XMonad.Wallpaper.Expand (expand) where

import Control.Monad.State
import Data.List
import Data.Char

import System.Posix.Env
import Data.Maybe
import Control.Applicative

data AST = Variable String | Literal String
    deriving (Show)

isExpr a = isAlphaNum a || a == '_'

literal str =
    let (a, b) = break (== '$') str in (Literal a, b)

variable ('{':as) =
    let (a, b) = break (== '}') as in (Variable a, tail b)
variable as = 
    let (a, b) = break (not . isExpr) as in (Variable a, b)

parse []       = []
parse ('$':as) =
    let (a, b) = variable as in a : parse b
parse as = 
    let (a, b) = literal as in a : parse b

interpolate (Variable var) = maybe "" id <$> getEnv var
interpolate (Literal str) = return str

expand :: String -> IO String
{- |
Expand string using environment variables, shell syntax are supported.
Examples:

>>> epxand "$HOME/Pictures"
"/home/user/Pictures"

>>> expand "${HOME}ABC"
"/home/userABC"
-}
expand str = do
    let ast = parse str
    concat <$> mapM interpolate ast
