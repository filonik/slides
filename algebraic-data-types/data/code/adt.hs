{-# LANGUAGE EmptyCase, TypeOperators #-}

import Prelude hiding (curry, uncurry)
import qualified Prelude as P

infixl 5 <~>
infixl 6 :+:
infixl 7 :*:
infixl 8 :^:


--data a :*: b = Product a b
--  deriving (Eq, Ord, Show)

--data a :+: b = Left a | Right b
--  deriving (Eq, Ord, Show)


-- data Either a b = Left a | Right b
newtype a :+: b = Sum { unSum :: Either a b } deriving (Eq, Ord, Show)

inl :: a -> (a :+: b)
inl = Sum . Left

inr :: b -> (a :+: b)
inr = Sum . Right


-- data (a, b) = (a, b)
newtype a :*: b = Product { unProduct :: (a,b) } deriving (Eq, Ord, Show)

exl :: (a :*: b) -> a
exl = fst . unProduct

exr :: (a :*: b) -> b
exr = snd . unProduct


newtype a :^: b = Exponential { unExponential :: a -> b }


data Unit = Unit deriving (Eq, Ord, Show)

plausible :: a -> Unit
plausible = \x -> Unit

data Void

absurd :: Void -> a
absurd = \x -> case x of {}

-- Examples

type One    = Unit
type Two    = Unit :+: One
type Three  = Unit :+: Two

-- data Bool = True | False


-- Isomorphisms

data a <~> b = Iso { from :: a -> b, to :: b -> a }

isoBT :: Bool <~> Two
isoBT = Iso f g where
  f :: Bool -> Two
  g :: Two -> Bool
  f x = case x of { True -> (Sum (Left Unit)); False -> (Sum (Right Unit)) }
  g x = case x of { (Sum (Left Unit)) -> True; (Sum (Right Unit)) -> False }


data Fruit = Apple | Orange
  deriving (Eq, Ord, Show)

boolToFruit :: Bool -> Fruit
boolToFruit x = case x of { True -> Apple; False -> Orange }

fruitToBool :: Fruit -> Bool
fruitToBool x = case x of { Apple -> True; Orange -> False }

isoBF :: Bool <~> Fruit
isoBF = Iso boolToFruit fruitToBool


-- Product and Sum Laws 

-- Identity of Addition
addIdFrom :: (Void :+: a) -> a
addIdFrom = (\x -> case x of { Left x -> absurd x; Right x -> id x }) . unSum 

addIdTo :: a -> (Void :+: a)
addIdTo = Sum . (\x -> Right x)

addId :: (Void :+: a) <~> a
addId = Iso addIdFrom addIdTo


-- Identity of Multiplication
mulIdFrom :: (Unit :*: a) -> a
mulIdFrom = (\(Unit, x) -> x) . unProduct

mulIdTo :: a -> (Unit :*: a)
mulIdTo = Product . \x -> (Unit, x)

mulId :: (Unit :*: a) <~> a
mulId = Iso mulIdFrom mulIdTo

-- Associativity of Addition
--addAssoc :: (a :+: (b :+: c)) <~> ((a :+: b) :+: c)


-- Associativity of Multiplication
--mulAssoc :: (a :*: (b :*: c)) <~> ((a :*: b) :*: c)


-- Commutativity of Addition
addComm :: (a :+: b) <~> (b :+: a)
addComm = Iso f f where
  f :: (a :+: b) -> (b :+: a)
  f (Sum (Left x)) = Sum (Right x)
  f (Sum (Right y)) = Sum (Left y)


-- Commutativity of Multiplication
mulComm :: (a :*: b) <~> (b :*: a)
mulComm = Iso f f where
  f :: (a :*: b) -> (b :*: a)
  f (Product (x, y)) = Product (y, x)


-- Exponent Laws
curry :: ((a :*: b) -> c) -> (a -> b -> c)
curry f = P.curry (f . Product)

uncurry :: (a -> b -> c) -> ((a :*: b) -> c)
uncurry f = (P.uncurry f) . unProduct

powerRule :: ((a :*: b) -> c) <~> (a -> (b -> c))
powerRule = Iso curry uncurry


-- Data Types

type Increment a = Unit :+: a

fromMaybe :: Maybe a -> Increment a
fromMaybe = Sum . (\x -> case x of { Nothing -> Left Unit; (Just y) -> Right y })

toMaybe :: Increment a -> Maybe a
toMaybe = (\x -> case x of { (Left Unit) -> Nothing; (Right y) -> Just y }) . unSum

isoMaybe :: Maybe a <~> Increment a
isoMaybe = Iso fromMaybe toMaybe

-- Examples:
-- from isoMaybe Nothing
-- from isoMaybe (Just 1)


newtype List a = List (Unit :+: (a :*: List a)) deriving (Eq, Ord, Show)

fromList :: [a] -> List a
fromList []     = List (inl Unit)
fromList (x:xs) = List (inr (Product (x, fromList xs)))

toList :: List a -> [a]
toList (List (Sum (Left Unit)))               = []
toList (List (Sum (Right (Product (x, xs))))) = (x:toList xs)

isoList :: [a] <~> List a
isoList = Iso fromList toList

-- Examples:
-- from isoList []
-- from isoList [1,2,3]



newtype Tree a = Tree (Unit :+: ((Tree a) :*: a :*: (Tree a))) deriving (Eq, Ord, Show)

