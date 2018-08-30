module Main where

import           Data.Bifunctor (first, second)
import qualified Data.Map.Strict as M

type Probability = Double

newtype Dist a = Dist
  { unDist :: [(a, Probability)]
  } deriving (Show, Eq)

instance Functor Dist where
  fmap g = Dist . fmap (first g) . unDist

instance Applicative Dist where
  pure x = Dist [(x, 1)]
  (Dist ps) <*> (Dist qs) =
    Dist $ do
      (f, p) <- ps
      (x, q) <- qs
      return (f x, p * q)

instance Monad Dist where
  return = pure
  Dist ps >>= f =
    Dist $ do
      (x, p) <- ps
      l <- (unDist . f) x
      return $ second (* p) l

unique :: (Ord a) => Dist a -> Dist a
unique (Dist []) = Dist []
unique (Dist ps) = Dist $ M.toList $ M.fromListWith (+) ps

randomWalk :: Num a => a -> Dist a
randomWalk x = Dist [(x + 1, 0.5), (x - 1, 0.5)]

uniform :: Dist Integer
uniform = Dist $ zip [1 ..] [0.25, 0.25, 0.25, 0.25]

main :: IO ()
main = putStrLn "Hello, almost-surely world!"
