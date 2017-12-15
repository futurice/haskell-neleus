{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs     #-}
module Neleus.Encode where

import Prelude ()
import Prelude.Compat

import Data.Bits      ((.|.))
import Data.Foldable  (toList)
import Data.List      (genericLength)
import Data.Semigroup

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Builder     as B
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.Fin                    as Fin

import Neleus.Generics
import Neleus.Integer
import Neleus.Schema
import Neleus.Types

-------------------------------------------------------------------------------
-- B
-------------------------------------------------------------------------------

-- | A wrapper around 'Builder' which tracks the length.
data B = B !Word B.Builder

bLen :: B -> Word
bLen (B n _) = n

instance Semigroup B where
    B l b <> B l' b' = B (l + l') (b <> b')

instance Monoid B where
    mempty = B 0 mempty
    mappend = (<>)

-- | Identifier and data
data IB = IB !Identifier !B

ibToB :: IB -> B
ibToB (IB (Identifier cls pc tag) b) = identifierB cls pc tag <> b

-------------------------------------------------------------------------------
-- Encode
-------------------------------------------------------------------------------

encode
    :: Schema a        -- ^ schema
    -> a               -- ^ value
    -> LBS.ByteString  -- ^ result is a stream
encode s x =  case encodeB s x of
    B _ bldr -> B.toLazyByteString bldr

encodeB :: Schema a -> a -> B
encodeB s x = ibToB (encodeA s x)

encodeA :: Schema a -> a -> IB
encodeA SAny           x  = asn1B x
encodeA SInteger       i  = IB
    (Identifier UniversalC Primitive 0x02)
    (finiteB (bLen b) <> b)
  where
    b = integerB i
encodeA SOctetString   bs = IB
    (Identifier UniversalC Primitive 0x04)
    (finiteB l <> B l (B.byteString bs))
  where
    l = fromIntegral (BS.length bs)
encodeA SBool b = IB
    (Identifier UniversalC Primitive 0x01)
    (finiteB 1 <> B 1 (B.word8 $ if b then 1 else 0))
encodeA SNull _= IB
    (Identifier UniversalC Primitive 0x05)
    (finiteB 0)

encodeA (SNamed _ i s) x  = encodeA s (isoFrom i x)
encodeA (STagged Explicit cls tag s) x = IB
    (Identifier cls Constructed tag)
    (encodeB s x)
encodeA (STagged Implicit cls tag s) x = case encodeA s x of
    IB (Identifier _ pc _) b -> IB (Identifier cls pc tag) b

encodeA (SSequenceOf s) xs = IB
    (Identifier UniversalC Constructed 0x10)
    (finiteB (bLen b) <> b)
  where
    b = foldMap (encodeB s) xs

encodeA (SSetOf s) xs = IB
    (Identifier UniversalC Constructed 0x11)
    (finiteB (bLen b) <> b)
  where
    b = foldMap (encodeB s) xs

encodeA (SEnumeration _) j = IB
    (Identifier UniversalC Primitive 0x0A)
    (integerB $ toInteger $ Fin.toNatural j)

encodeA (SSequence fs) xs = IB
    (Identifier UniversalC Constructed 0x10)
    (finiteB (bLen b) <> b)
  where
    b = go fs xs

    go :: NP FieldSchema xs -> NP I xs -> B
    go Nil Nil = mempty
    go (Req _ s   :* fs') (I x        :* xs') = encodeB s x <> go fs' xs'
    go (Def _ _ s :* fs') (I x        :* xs') = encodeB s x <> go fs' xs'
    go (Opt _ _   :* fs') (I Nothing  :* xs') = go fs' xs'
    go (Opt _ s   :* fs') (I (Just x) :* xs') = encodeB s x <> go fs' xs'

encodeA (SChoice os) xs = go os xs
  where
    go :: NP OptionSchema xs -> NS I xs -> IB
    go Nil x = case x of {}
    go (SOption _ s :* _)   (Z (I x)) = encodeA s x
    go (_           :* os') (S xs')   = go os' xs'

-------------------------------------------------------------------------------
-- Value
-------------------------------------------------------------------------------

asn1B :: ASN1Value -> IB
asn1B EOC =
    IB (Identifier UniversalC Primitive 0x00) (finiteB 0)
asn1B (Int i) =
    IB (Identifier UniversalC Primitive 0x02) (finiteB (bLen b) <> b)
  where
    b = integerB i
asn1B (OctetString bs) =
    IB (Identifier UniversalC Primitive 0x04) (finiteB l <> B l (B.byteString bs))
  where
    l = fromIntegral (BS.length bs)
asn1B (Enum i) =
    IB (Identifier UniversalC Primitive 0x0A) (finiteB (bLen b) <> b)
  where
    b = integerB i
asn1B (Sequence xs) =
    IB (Identifier UniversalC Constructed 0x10) (finiteB (bLen b) <> b)
  where
    b = foldMap (ibToB . asn1B) xs
asn1B (Application tag x) =
    nonUniversalB ApplicationC tag x
asn1B x = error (show x)

nonUniversalB :: Class -> Tag -> BS -> IB
nonUniversalB cls tag (BS bs) = IB
    (Identifier cls Primitive tag)
    (finiteB l <> B l (B.byteString bs))
  where
    l = fromIntegral (BS.length bs)
nonUniversalB cls tag (Value xs) = IB
    (Identifier cls Constructed tag)
    (finiteB (bLen b))
  where
    b = foldMap (ibToB . asn1B) xs

-------------------------------------------------------------------------------
-- Integer
-------------------------------------------------------------------------------

integerB :: Integer -> B
integerB i = B (genericLength bs) (foldMap B.word8 bs)
  where
    bs = toList (encodeInteger i)

-------------------------------------------------------------------------------
-- Identifier
-------------------------------------------------------------------------------

identifierB :: Class -> PC -> Tag -> B
identifierB cls pc tag =
    B 1 (B.word8 (cls' .|. pc' .|. tag'))
  where
    cls' = case cls of
        UniversalC   -> 0x00
        ApplicationC -> 0x40
        ContextC     -> 0x80
        PrivateC     -> 0xc0
    pc' = case pc of
        Primitive   -> 0x00
        Constructed -> 0x20
    tag' | tag < 31 = fromIntegral tag
         | otherwise = error ">= tags not supported"

-------------------------------------------------------------------------------
-- Length
-------------------------------------------------------------------------------

finiteB :: Word -> B
finiteB n
    | n < 0x80  = B 1 (B.word8 (fromIntegral n))
    | otherwise = error "lengthB" -- TODO!!!

indefiniteB :: B
indefiniteB = B 1 (B.word8 0x80)

lengthB :: Length -> B
lengthB = maybe indefiniteB finiteB
