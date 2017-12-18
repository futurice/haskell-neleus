{-# LANGUAGE GADTs         #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}
module Neleus.Decode (
    decode,
    decodeSingle,
    Error,
    ) where

-- http://luca.ntop.org/Teaching/Appunti/asn1.html

import Data.List.NonEmpty (NonEmpty (..))
import Prelude ()
import Prelude.Compat     hiding (sequence)

import qualified Data.ByteString as BS

import Neleus.DER
import Neleus.Generics
import Neleus.Integer
import Neleus.Schema
import Neleus.Types

import Debug.Trace

-- | For now, errors are just strings.
type Error = String

decode
    :: (s -> DER s)          -- ^ a way to consume stream
    -> Schema a              -- ^ schema
    -> s                     -- ^ stream
    -> Either Error (a, s)   -- ^ result is either an error, or value and rest of the stream.
decode step sch s = case step s of
    End                   -> Left "unexpected end-of-input"
    Err err               -> Left err
    Fin ident contents s' ->
        (,s') <$> decodeA True ident contents sch

decodeSingle :: Schema a -> BS.ByteString -> Either Error a
decodeSingle sch contents = do
    (x, s) <- decode unfix sch (fullDER contents)
    case s of
        Fix End         -> Right x
        Fix (Err err)   -> Left err
        Fix (Fin _ _ _) -> Left "uparsed input"

-- Decode single value, __don't check tags__.
decodeA :: Bool -> Identifier -> BS.ByteString -> Schema a -> Either Error a
decodeA chk ident@(Identifier _cls pc _tag) contents sch = case sch of
    -- Named schemas are just an isomorphism
    SNamed _ (Iso f _) sch' ->
        f <$> decodeA chk ident contents sch'

    -- 8.2 Encoding of a boolean value
    SBool -> do
        requireClassTag chk ident UniversalC 0x01

        -- 8.2.1 boolean should be primitive
        requirePrimitive pc
        -- should consist of a single octet
        requireBool (len == 1) "Bool: shall consist of a single octet"
        let w = BS.head contents
        -- 8.2.2 False encoded as 0, True as everything else
        return $ w /= 0

    -- 8.3 Encoding of an integer value
    SInteger -> do
        requireClassTag chk ident UniversalC 0x02

        -- 8.3.1 integer should be primitive
        requirePrimitive pc
        -- 8.3.1 should be one or more octets
        requireBool (len >= 1) "Integer: Shall consist >=1 octets"

        -- Note: this enforces len >= 1 as well
        case BS.unpack contents of
            (w:ws) -> Right (decodeInteger (w :| ws))
            []     -> Left "panic! len >= 1, but empty contents"

    -- 8.7 Encoding of an octetstring value
    SOctetString -> do
        requireClassTag chk ident UniversalC 0x04

        -- DER 10.2: Primitive form should be used
        requirePrimitive pc

        Right contents

    -- 8.8 Encoding of a null value
    SNull -> do
        requireClassTag chk ident UniversalC 0x05

        -- 8.8.1 null should be primitive
        requirePrimitive pc
        -- 8.8.2 should not contain any octets
        requireBool (len == 0) "End-of-contents content's should be absent"

        return ()

    -- 8.9 Encoding of a sequence value
    SSequence fs -> do
        requireClassTag chk ident UniversalC 0x10

        -- 8.9.1 sequence should be constructed
        requireConstructed pc

        decodeF fs (fullDER contents)

    -- 8.13 Encoding of a choice value
    SChoice os -> decodeO chk ident contents "Choice don't match" os

    -- 8.14 Encoding of a tagged type
    STagged Implicit cls' tag' sch' -> do
        requireClassTag chk ident cls' tag'

        decodeA False ident contents sch'

    STagged Explicit cls' tag' sch' -> do
        requireClassTag chk ident cls' tag'

        -- 8.14.3 Explicit tags should be constructed
        requireConstructed pc

        decodeSingle sch' contents

    -- 8.15 Encoding of an open type
    SAny -> asn1Parser (Fix (Fin ident contents (Fix End)))

    {- TODO
    (SSetOf _)
    (SSequenceOf _)
    (SEnumeration _)
    -}
  where
    len = BS.length contents

-- TODO: non-deterministic match
decodeF :: NP FieldSchema xs -> Fix DER -> Either Error (NP I xs)
decodeF _ (Fix (Err err))  = Left err
decodeF Nil (Fix End)      = Right Nil
decodeF Nil _              = Left "not fully consumed SEQUENCE"
decodeF (f :* fs) s@(Fix End) = case f of
    Req name _ -> Left $ "missing fields in SEQUENCE, e.g." ++ name
    Opt _ _ -> do
        xs <- decodeF fs s
        pure (I Nothing :* xs)
    Def _ v _ -> do
        xs <- decodeF fs s
        pure (I v :* xs)
decodeF (f :* fs) (Fix (Fin ident contents s)) = case f of
    Req _name sch -> do
        x <- decodeA True ident contents sch
        xs <- decodeF fs s
        pure (I x :* xs)
    {- TODO: Opt Def #-}

-- | TODO: At the moment, this is dummy and tries all options in order.
decodeO :: Bool -> Identifier -> BS.ByteString -> [Char] -> NP OptionSchema xs -> Either Error (NS I xs)
decodeO _chk _ident _contents err Nil = Left err
decodeO  chk  ident  contents err (SOption _name sch :* schs) =
    case decodeA chk ident contents sch of
        Right x -> Right (Z (I x))
        Left e  -> S <$> decodeO chk ident contents (err ++ ";" ++ e) schs

-------------------------------------------------------------------------------
-- Guards
-------------------------------------------------------------------------------

requireClassTag :: Bool -> Identifier -> Class -> Tag -> Either Error ()
requireClassTag False _ _ _ = pure ()
requireClassTag True (Identifier cls _ tag) cls' tag'
    | cls /= cls' = Left $ "Classes don't match: " ++ show (cls, cls')
    | tag /= tag' = Left $ "Tags don't match: " ++ show (tag, tag')
    | otherwise = pure ()

requireBool :: Bool -> String -> Either Error ()
requireBool True _    = Right ()
requireBool False err = Left err

requirePrimitive :: PC -> Either Error ()
requirePrimitive Primitive   = Right ()
requirePrimitive Constructed = Left "Primitive encoding expected"

requireConstructed :: PC -> Either Error ()
requireConstructed Constructed = Right ()
requireConstructed Primitive   = Left "Constructed encoding expected"

-------------------------------------------------------------------------------
-- Value
-------------------------------------------------------------------------------

asn1Parser :: Fix DER -> Either Error ASN1Value
asn1Parser s = case asn1Parser' s of
    Left err                   -> Left err
    Right (_, Fix (Err err))   -> Left err
    Right (_, Fix (Fin _ _ _)) -> Left "unconsumed input"
    Right (x, Fix End)         -> Right x

asn1Parser' :: Fix DER -> Either Error (ASN1Value, Fix DER)
asn1Parser' (Fix (Err err)) = Left err
asn1Parser' (Fix End)       = Left "unexpected end-of-input"
asn1Parser' (Fix (Fin ident@(Identifier cls pc tag) contents s)) = do
    (,s) <$> case cls of
        UniversalC -> case traceShow ident tag of
            0x02 -> Int <$> decodeA False ident contents schema
            0x01 -> Bool <$> decodeA False ident contents schema
            0x04 -> OctetString <$> decodeA False ident contents schema
            0x0A -> Enum <$> decodeA False ident contents schema
            0x10 -> Sequence <$> iter (fullDER contents)
{-
            0x00 -> ConsumeEmpty EOC
            0x01 -> ConsumeBool Bool
            0x05 -> ConsumeEmpty Null
            0x0A -> ConsumeInt Enum
            0x10 -> ConsumeConstructed (SequenceOf Sequence asn1Parser)
-}
            _    -> Left $ "Unimplemented tag" ++ show ident
        ApplicationC -> nonUniversalParser pc contents $ Application tag
        ContextC     -> nonUniversalParser pc contents $ Context tag
        PrivateC     -> nonUniversalParser pc contents $ Private tag

iter :: Fix DER -> Either Error [ASN1Value]
iter (Fix End) = Right []
iter s0        = do
    (x, s1) <- asn1Parser' s0
    xs <- iter s1
    pure (x : xs)

-- | non universal types are encoded similarly.
--
-- X.680 says that there is no difference between non-universal tags.
-- It's a matter of choice and style
-- Three classes are there for historical reasons.
--
-- See note in X.680 (07/2002) 8.3
nonUniversalParser :: PC -> BS.ByteString -> (BS -> ASN1Value) -> Either Error ASN1Value
nonUniversalParser Constructed s f = f . Value <$> iter (fullDER s)
nonUniversalParser Primitive s f = Right (f (BS s))
