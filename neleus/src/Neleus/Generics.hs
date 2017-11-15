{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE EmptyCase              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
-- | TBW
module Neleus.Generics (
    -- * Product
    NP (..),
    toNP,
    fromNP,
    IsProduct,
    -- * Sum
    NS (..),
    toNS,
    fromNS,
    IsSum,
    -- * Names
    HasDatatypeName (..),
    -- * Other
    I (..),
    All,
    ) where

import Data.Bifunctor (first)
import Generics.SOP   (All, I (..), NP (..), NS (..))
import GHC.Generics

-------------------------------------------------------------------------------
-- Product
-------------------------------------------------------------------------------

-- | Product types are "records".
type IsProduct a xs = (Generic a, IsProduct' (Rep a) xs)

toNP :: IsProduct a xs => a -> NP I xs
toNP = toNP' . from

fromNP :: IsProduct a xs => NP I xs -> a
fromNP = to . fromNP'

class IsProduct' (f :: k -> *) (xs :: [*]) | f -> xs where
    toNP'   :: f a -> NP I xs
    fromNP' :: NP I xs -> f a

instance IsProduct' U1 '[] where
    toNP' _ = Nil
    fromNP' _ = U1

instance IsProduct' f xs => IsProduct' (M1 _m _c f) xs where
    toNP'   = toNP' . unM1
    fromNP' = M1 . fromNP'

instance IsProduct' (K1 d a) '[a] where
    toNP' (K1 x)         = I x :* Nil
    fromNP' (I x :* Nil) = K1 x

instance (IsProduct' f xs, IsProduct' g ys, NPAppend xs ys zs) => IsProduct' (f :*: g) zs where
    toNP' (f :*: g) = npAppend (toNP' f) (toNP' g)
    fromNP' zs      = case npSplit zs of
        (xs, ys) -> fromNP' xs :*: fromNP' ys

-------------------------------------------------------------------------------
-- Sum
-------------------------------------------------------------------------------

-- | Sum types are of special form, constructors with single value, e.g.
--
-- @
-- data MySum = OptI Int | OptB Bool | OptC Char
-- @
--
type IsSum a xs = (Generic a, IsSum' (Rep a) xs)

toNS :: IsSum a xs => a -> NS I xs
toNS = toNS' . from

fromNS :: IsSum a xs => NS I xs -> a
fromNS = to . fromNS'

class IsSum' (f :: k -> *) (xs :: [*]) | f -> xs where
    toNS'   :: f a -> NS I xs
    fromNS' :: NS I xs -> f a

instance IsSum' f xs => IsSum' (M1 _m _c f) xs where
    toNS'   = toNS' . unM1
    fromNS' = M1 . fromNS'

instance IsSum' (K1 d a) '[a] where
    toNS' (K1 x)      = Z (I x)
    fromNS' (Z (I x)) = K1 x
    fromNS' (S x)     = case x of {}

instance (IsSum' f xs, IsSum' g ys, NSAppend xs ys zs) => IsSum' (f :+: g) zs where
    toNS' (L1 f) = nsAppend (Left (toNS' f) :: NSEither I xs ys)
    toNS' (R1 g) = nsAppend (Right (toNS' g) :: NSEither I xs ys)

    fromNS' = either (L1 . fromNS') (R1 . fromNS') . nsSplit

-------------------------------------------------------------------------------
-- List append utilities
-------------------------------------------------------------------------------

class NPAppend xs ys zs | xs ys -> zs where
    npAppend :: NP f xs -> NP f ys -> NP f zs

    npSplit :: NP f zs -> (NP f xs, NP f ys)

instance ys ~ zs => NPAppend '[] ys zs where
    npAppend _ ys = ys

    npSplit zs = (Nil, zs)

instance NPAppend xs ys zs => NPAppend (x ': xs) ys (x ': zs) where
    npAppend (x :* xs) ys = x :* npAppend xs ys

    npSplit (x :* zs) = case npSplit zs of
        (xs, ys) -> (x :* xs, ys)


type NSEither f xs ys = Either (NS f xs) (NS f ys)

class NSAppend xs ys zs | xs ys -> zs where
    nsAppend :: Either (NS f xs) (NS f ys) -> NS f zs

    nsSplit :: NS f zs -> Either (NS f xs) (NS f ys)

instance ys ~ zs => NSAppend '[] ys zs where
    nsAppend (Right ys) = ys
    nsAppend (Left xs) = case xs of {}

    nsSplit = Right

instance NSAppend xs ys zs => NSAppend (x ': xs) ys (x ': zs) where
    nsAppend :: forall f. Either (NS f (x ': xs)) (NS f ys) -> NS f (x ': zs)
    nsAppend (Left (Z x))  = Z x
    nsAppend (Left (S xs)) = S (nsAppend (Left xs  :: NSEither f xs ys))
    nsAppend (Right ys)    = S (nsAppend (Right ys :: NSEither f xs ys))

    nsSplit :: forall f. NS f (x ': zs) -> Either (NS f (x ': xs)) (NS f ys)
    nsSplit (Z z)  = Left (Z z)
    nsSplit (S zs) = first S (nsSplit zs :: Either (NS f xs) (NS f ys))

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

class HasDatatypeName a where
    typeName :: proxy a -> String

instance (Generic a, Datatype d, Rep a ~ D1 d x) => HasDatatypeName a where
    typeName _ = datatypeName p
      where
        p :: InfoProxy d x a
        p = InfoProxy

-- | A proxy to access Generics 'Datatype'
data InfoProxy (c :: k') (f ::  k -> *) (x :: k) = InfoProxy
