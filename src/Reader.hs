module Reader where

import qualified Control.Applicative (Applicative, pure, (<*>))
import qualified Control.Monad (Monad, (>>=))
import System.Environment (getEnv, getArgs)
import Data.List (intercalate)


data Reader r a = Reader (r -> a)


withEnv :: Reader r a -> r -> a
withEnv (Reader f) e = f e


instance Functor (Reader r) where
    -- f :: a -> b
    -- m :: Reader r a
    -- m' :: a
    -- GOAL :: Reader r b
    fmap f m = Reader (\e -> 
                    let m' = m `withEnv` e
                    in f m')


instance Applicative (Reader r) where
    -- x :: a
    -- GOAL :: Reader r a
    pure x = Reader (\e -> x)

    -- f :: Reader r (a -> b)
    -- m :: Reader r a
    -- f' :: a -> b
    -- m' :: a
    -- GOAL :: Reader r b
    f <*> m = Reader (\e -> 
                    let f' = f `withEnv` e
                        m' = m `withEnv` e
                    in f' m')


instance Monad (Reader r) where
    -- m :: Reader r a
    -- f :: a -> Reader r b
    -- m' :: a
    -- GOAL :: Reader r b
    m >>= f = Reader (\e ->
                let m' = m `withEnv` e
                in f m' `withEnv` e)


data Ctx = Ctx {
          _user :: String
        , _home :: String
        , _args :: [String]
    }


user :: Reader Ctx String
user = Reader _user

home :: Reader Ctx String
home = Reader _home

args :: Reader Ctx [String]
args = Reader _args

runWithCtx :: Reader Ctx a -> IO a
runWithCtx c = do
    userVal <- getEnv "USER"
    homeVal <- getEnv "HOME"
    argsVal <- getArgs

    let ctx = Ctx {
          _user = userVal
        , _home = homeVal
        , _args = argsVal

    }

    pure (c `withEnv` ctx)


computation :: Reader Ctx String
computation = do
    userVal <- user
    homeVal <- home
    argsVal <- args

    let argsString = intercalate "\n" argsVal

    pure ("Hello, " ++ userVal ++ "!\n" ++
          "Your home directory is " ++ homeVal ++ "\n" ++
          "and you called this program with the following args:\n" ++
          argsString)

