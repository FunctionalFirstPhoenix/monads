module State where

import qualified Control.Applicative (Applicative, pure, (<*>))
import qualified Control.Monad (Monad, (>>=))
import System.Environment (getEnv, getArgs)
import Data.List (intercalate)

data State s a = State (s -> (a, s))

evalState :: State s a -> s -> a
evalState (State f) s = 
    let (x, s') = f s
    in x

execState :: State s a -> s -> s
execState (State f) s = 
    let (x, s') = f s
    in s'

instance Functor (State s) where
    -- f :: a -> b
    -- m :: State s a
    -- GOAL :: State s b
    fmap f m = State (\s -> 
                    let m' = evalState m s
                        s' = execState m s
                    in (f m', s'))

instance Applicative (State s) where
    -- x :: a
    -- GOAL :: State s a
    pure x = State (\s -> (x, s))

    -- f :: State s (a -> b)
    -- m :: State s a
    -- f' :: a -> b
    -- m' :: a
    -- GOAL :: State s b
    f <*> m = State (\s -> 
                    let f'  = evalState f s
                        s'  = execState f s
                        m'  = evalState m s'
                        s'' = execState m s'

                    in (f' m', s''))


instance Monad (State s) where
    -- m :: State s a
    -- f :: a -> State s b
    -- m' :: a
    -- b :: State s b
    -- GOAL :: State s b
    m >>= f = State (\s -> 
                let m'  = evalState m s
                    s'  = execState m s
                    b   = f m'
                    r   = evalState b s'
                    s'' = execState b s'
                in (r, s''))


