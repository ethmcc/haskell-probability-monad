-- A GHC 7.10- compatible rewrite of the Prob monad example from 
-- LYAH's "For a Few Monads More" chapter. 
import Data.Ratio
import Data.List (all)
import Control.Monad
import Data.List

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

-- luckily in LYAH, Prob is already given a Functor instance
-- implementation: 
instance Functor Prob where 
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x, p)) xs
-- Note that if we weren't defining >>= in terms of fmap, we could 
-- simply use fmap = liftM in our functor implementation. However, 
-- it's best practice to define fmap instead, so good! 

-- our custom version of ``join``
flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs,p) = map (\(x,r) -> (x, p*r)) innerxs

-- Here's where we diverge from LYAH's text:
-- since GHC 7.10 and the adoption of the Functor Applicative Monad 
-- proposal, Monads are a subclass of Applicative. But have no worry, 
-- the Applicative instance can be derived from your Monad code quite 
-- easily: 
instance Monad Prob where
    -- we need to move the ``return`` definition into the Applicative's
    -- implementation of ``pure``
    m >>= f = flatten (fmap f m)

instance Applicative Prob where
    pure x = Prob [(x, 1%1)] -- here's where we moved ``return``
    (<*>) = ap 
-- You might be wondering why GHC can't figure out (<*>) = ap by itself. 
-- Well, actually it can, but as of right now it will give a warning 
-- message if you don't either implement (<*>) or liftA2. 

-- The rest remains unchanged. 
data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads, 1%2), (Tails, 1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1%10), (Tails, 9%10)]

flipThree :: Prob Bool
flipThree = do
    a <- coin
    b <- coin
    c <- loadedCoin
    return (all (==Tails) [a,b,c])

-- Here's one approach to the exercise given at the end of the chapter
boolifyProb :: Prob Bool -> Prob Bool
boolifyProb (Prob xs) = Prob [(False, probFalse), (True, probTrue)] where
    probFalse = sum [p | (x, p) <- xs, x == False]
    probTrue = sum [p | (x, p) <- xs, x == True]

-- Or we could do it generically
condenseProb :: (Eq a) => Prob a -> Prob a
condenseProb (Prob xs) = Prob $ nub [(event, sumProbs event) | (event, p) <- xs] where
    sumProbs event = sum [ p | (x, p) <- xs, x == event ]