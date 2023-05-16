{-# LANGUAGE TypeFamilies #-}
type Append :: forall a. [a] -> [a] -> [a]  -- kind signature
type family Append xs ys where              -- header
  Append '[]    ys = ys                     -- clause 1
  Append (x:xs) ys = x : Append xs ys       -- clause 2

type Not :: Bool -> Bool
type family Not a where
  Not 'True = 'False
  Not 'False = 'True

type FromMaybe :: a -> Maybe a -> a
type family FromMaybe d x where
  FromMaybe d 'Nothing = d
  FromMaybe _ ('Just x) = x

type Fst :: (a, b) -> a
type family Fst t where
  Fst '(x, _) = x
