{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | ...
module Neleus.Schema (
    -- * Class
    ASN1 (..),
    -- * Basis schemas
    octetString,
    integer,
    bool,
    -- * Combinators
    named',
    namedNewtype,
    tagged,
    sequence,
    taggedSequence,
    choice,
    option,
    option',
    -- ** Fields
    required,
    required',
    optional,
    optional',
    defaulted,
    defaulted',
    -- * Schema
    Schema (..),
    FieldSchema (..),
    OptionSchema (..),
    SchemaName,
    FieldName,
    OptionName,
    Iso (..),
    ) where

import Data.Set       (Set)
import GHC.Generics
import Prelude ()
import Prelude.Compat hiding (sequence)

import qualified Data.ByteString as BS

import Neleus.Generics
import Neleus.Types

data Iso a b = Iso
    { isoTo   :: a -> b
    , isoFrom :: b -> a
    }

type SchemaName = String

-- | Schema.
data Schema a where
    -- primitive
    SNull        :: Schema ()
    SBool        :: Schema Bool
    SInteger     :: Schema Integer
    SOctetString :: Schema BS.ByteString
    -- alias
    SNamed       :: SchemaName -> Iso a b -> Schema a -> Schema b
    -- combinators
    SSetOf       :: Ord a => Schema a -> Schema (Set a)
    SSequenceOf  :: Schema a -> Schema [a]
    SSequence    :: NP FieldSchema xs -> Schema (NP I xs)
    SChoice      :: NP OptionSchema xs -> Schema (NS I xs)
    STagged      :: Explicitness -> Class -> Tag -> Schema a -> Schema a
    -- magic
    SAny         :: Schema ASN1Value

type FieldName = String

data FieldSchema a where
    Req :: FieldName -> Schema a -> FieldSchema a
    Opt :: FieldName -> Schema a -> FieldSchema (Maybe a)
    Def :: FieldName -> a ->  Schema a -> FieldSchema a

type OptionName = String

data OptionSchema a = SOption OptionName (Schema a)

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

class ASN1 a where
    schema :: Schema a

-- | >>> prettySchema (schema :: Schema BS.ByteString)
-- OCTET STRING
instance ASN1 BS.ByteString where
    schema = octetString

-- | >>> prettySchema (schema :: Schema Integer)
-- INTEGER
instance ASN1 Integer where
    schema = integer

-- | >>> prettySchema (schema :: Schema ASN1Value)
-- ANY
instance ASN1 ASN1Value where
    schema = value

-- | >>> prettySchema (schema :: Schema Bool)
-- BOOLEAN
instance ASN1 Bool where
    schema = bool

-- | >>> prettySchema (schema :: Schema ())
-- NULL
instance ASN1 () where
    schema = SNull

-- | >>> prettySchema (schema :: Schema [Integer])
-- SEQUENCE OF INTEGER
instance ASN1 a => ASN1 [a] where
    schema = SSequenceOf schema

-- | >>> prettySchema (schema :: Schema (Set Bool))
-- SET OF BOOLEAN
instance (ASN1 a, Ord a) => ASN1 (Set a) where
    schema = SSetOf schema

-------------------------------------------------------------------------------
-- Basic Schemas
-------------------------------------------------------------------------------

bool :: Schema Bool
bool = SBool

octetString :: Schema BS.ByteString
octetString = SOctetString

integer :: Schema Integer
integer = SInteger

value :: Schema ASN1Value
value = SAny

-------------------------------------------------------------------------------
-- Fields
-------------------------------------------------------------------------------

required :: ASN1 a => FieldName -> FieldSchema a
required n = Req n schema

optional :: ASN1 a => FieldName -> FieldSchema (Maybe a)
optional n = Opt n schema

defaulted :: ASN1 a => FieldName -> a -> FieldSchema a
defaulted n v = Def n v schema

required' :: FieldName -> Schema a -> FieldSchema a
required' = Req

optional' :: FieldName -> Schema a -> FieldSchema (Maybe a)
optional' = Opt

defaulted' :: FieldName -> a -> Schema a -> FieldSchema a
defaulted' = Def

-------------------------------------------------------------------------------
-- Sequence
-------------------------------------------------------------------------------

sequence :: forall a xs. (IsProduct a xs, HasDatatypeName a) => NP FieldSchema xs -> Schema a
sequence = SNamed (typeName ([] :: [a])) (Iso fromNP toNP) . SSequence

-- | Implicitly tagged @SEQUENCE@.
taggedSequence
    :: forall a xs. (IsProduct a xs, HasDatatypeName a)
    => Class -> Tag
    -> NP FieldSchema xs -> Schema a
taggedSequence cls tag
    = SNamed (typeName ([] :: [a])) (Iso fromNP toNP)
    . STagged Implicit cls tag
    . SSequence

-------------------------------------------------------------------------------
-- Choice
-------------------------------------------------------------------------------

choice :: forall a xs. (IsSum a xs, HasDatatypeName a) => NP OptionSchema xs -> Schema a
choice = SNamed (typeName ([] :: [a])) (Iso fromNS toNS) . SChoice

option :: ASN1 a => OptionName -> OptionSchema a
option n = option' n schema

option' :: OptionName -> Schema a -> OptionSchema a
option' = SOption

-------------------------------------------------------------------------------
-- Newtype
-------------------------------------------------------------------------------

-- | A variant of 'named', which works out details by using 'Generic'.
namedNewtype :: forall a b d c s. (ASN1 a, Generic b, HasDatatypeName b, Rep b ~ D1 d (C1 c (S1 s (Rec0 a)))) => Schema b
namedNewtype = SNamed (typeName ([] :: [b])) (Iso build match) schema where
    build :: a -> b
    build = to . M1 . M1  . M1 . K1

    match :: b -> a
    match = unK1 . unM1 . unM1 . unM1 . from

named' :: forall a b. (HasDatatypeName b) => (a -> b) -> (b -> a) -> Schema a -> Schema b
named' f g = SNamed (typeName ([] :: [b])) (Iso f g)

-------------------------------------------------------------------------------
-- Tagging
-------------------------------------------------------------------------------

-- | Implicit tagging.
tagged :: Class -> Tag -> Schema a -> Schema a
tagged = STagged Implicit

-- $setup
-- >>> import Neleus.Pretty
