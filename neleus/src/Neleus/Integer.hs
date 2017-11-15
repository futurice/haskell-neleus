-- | __8.3__ Encoding of an integer value.
--
-- Integers are encoded in two's complement binary.
module Neleus.Integer where

import Data.Int           (Int8)
import Data.List          (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word          (Word8)

import qualified Data.List.NonEmpty as NE

-- | Encode integer.
--
-- prop> xs == decodeInteger (encodeInteger xs)
encodeInteger :: Integer -> NonEmpty Word8
encodeInteger = NE.reverse . go where
    go n | -128 <= n && n <= 127 = pure (fromInteger n)
         | otherwise = case divMod n 256 of
              (d, m) -> NE.cons (fromInteger m) (go d)

-- | This doesn't check for encoding minimality.
-- i.e. that the bits of the first byte and bit 8 of the second byte
-- aren't all zeros or all ones.
decodeInteger :: NonEmpty Word8 -> Integer
decodeInteger (w :| ws) = foldl' (\x y -> x * 256 + fromIntegral y) w' ws where
    w' = fromIntegral (fromIntegral w :: Int8)
