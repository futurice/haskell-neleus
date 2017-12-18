{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}
module Neleus.Decode (
    decode,
    decodeSingle,
    Error,
    ) where

-- http://luca.ntop.org/Teaching/Appunti/asn1.html

import Data.Foldable      (toList)
import Data.Functor.Alt   (Alt (..))
import Data.List          (intercalate, nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup     (Semigroup (..))
import Data.Vec.Lazy      (Vec (..))
import Prelude ()
import Prelude.Compat     hiding (sequence)

import qualified Data.ByteString    as BS
import qualified Data.Fin           as Fin
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as Set

import Neleus.DER
import Neleus.Generics
import Neleus.Integer
import Neleus.Schema
import Neleus.Types

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

        -- 8.2.1 boolean must be primitive
        requirePrimitive pc
        -- must consist of a single octet
        requireBool (len == 1) "Bool: shall consist of a single octet"
        let w = BS.head contents
        -- 8.2.2 False encoded as 0, True as everything else
        return $ w /= 0

    -- 8.3 Encoding of an integer value
    SInteger -> do
        requireClassTag chk ident UniversalC 0x02

        -- 8.3.1 integer must be primitive
        requirePrimitive pc
        -- 8.3.1 must be one or more octets
        requireBool (len >= 1) "Integer: Shall consist >=1 octets"

        -- Note: this enforces len >= 1 as well
        case BS.unpack contents of
            (w:ws) -> Right (decodeInteger (w :| ws))
            []     -> Left "panic! len >= 1, but empty contents"

    -- 8.4 Encoding of an enumerated value
    SEnumeration vs -> do
        requireClassTag chk ident UniversalC 0x0A

        -- 8.3.1 integer must be primitive
        requirePrimitive pc
        -- 8.3.1 must be one or more octets
        requireBool (len >= 1) "Integer: Shall consist >=1 octets"

        -- Note: this enforces len >= 1 as well
        i <- case BS.unpack contents of
            (w:ws) -> Right (decodeInteger (w :| ws))
            []     -> Left "panic! len >= 1, but empty contents"

        decodeEnum (fromInteger i) vs

    -- 8.7 Encoding of an octetstring value
    SOctetString -> do
        requireClassTag chk ident UniversalC 0x04

        -- DER 10.2: Primitive form must be used
        requirePrimitive pc

        Right contents

    -- 8.8 Encoding of a null value
    SNull -> do
        requireClassTag chk ident UniversalC 0x05

        -- 8.8.1 null must be primitive
        requirePrimitive pc
        -- 8.8.2 must not contain any octets
        requireBool (len == 0) "End-of-contents content's must be absent"

        return ()

    -- 8.9 Encoding of a sequence value
    SSequence fs -> do
        requireClassTag chk ident UniversalC 0x10

        -- 8.9.1 sequence must be constructed
        requireConstructed pc

        decodeF fs (fullDER contents)

    -- 8.10 Encoding of a sequence-of value
    SSequenceOf s -> do
        requireClassTag chk ident UniversalC 0x10

        -- 8.10.1 sequence-of must be constructed
        requireConstructed pc

        decodeSeqOf s (fullDER contents)

    -- 8.12 Encoding of a set-of value
    SSetOf s -> do
        requireClassTag chk ident UniversalC 0x11

        -- 8.10.1 sequence-of must be constructed
        requireConstructed pc

        Set.fromList <$> decodeSeqOf s (fullDER contents)

    -- 8.13 Encoding of a choice value
    SChoice os -> decodeO chk ident contents "Choice don't match" os

    -- 8.14 Encoding of a tagged type
    STagged Implicit cls' tag' sch' -> do
        requireClassTag chk ident cls' tag'

        decodeA False ident contents sch'

    STagged Explicit cls' tag' sch' -> do
        requireClassTag chk ident cls' tag'

        -- 8.14.3 Explicit tags must be constructed
        requireConstructed pc

        decodeSingle sch' contents

    -- 8.15 Encoding of an open type
    SAny -> asn1Parser (Fix (Fin ident contents (Fix End)))
  where
    len = BS.length contents

decodeSeqOf :: Schema a -> Fix DER -> Either Error [a]
decodeSeqOf sch = go where
    go (Fix (Err err))              = Left err
    go (Fix End)                    = Right []
    go (Fix (Fin ident contents s)) = do
        x <- decodeA True ident contents sch
        xs <- go s
        pure (x : xs)

decodeEnum :: Int -> Vec n EnumSchema -> Either Error (Fin.Fin n)
decodeEnum i = go where
    go :: Vec m EnumSchema -> Either Error (Fin.Fin m)
    go VNil = Left $ "unknown enum value " ++ show i
    go (EnumSchema _ x ::: es)
        | x == i    = Right Fin.Z
        | otherwise = Fin.S <$> go es

-- | TODO: At the moment, this is dummy and tries all options in order.
decodeO :: Bool -> Identifier -> BS.ByteString -> [Char] -> NP OptionSchema xs -> Either Error (NS I xs)
decodeO _chk _ident _contents err Nil = Left err
decodeO  chk  ident  contents err (SOption _name sch :* schs) =
    case decodeA chk ident contents sch of
        Right x -> Right (Z (I x))
        Left e  -> S <$> decodeO chk ident contents (err ++ ";" ++ e) schs

-------------------------------------------------------------------------------
-- Sequence
-------------------------------------------------------------------------------

-- TODO: non-deterministic match
decodeF :: NP FieldSchema xs -> Fix DER -> Either Error (NP I xs)
decodeF fs s = case decodeF' fs s of
    Success (x :| []) -> Right x
    Success _         -> Left "Ambiguous parse"
    Failure es        ->
        Left $ "Failed SEQUENCE parse, encountered errors: " ++ intercalate "; " (nub (map snd (toList es)))
{-

decodeF (f :* fs) (Fix (Fin ident contents s)) = case f of
    Req _name sch -> do
        x <- decodeA True ident contents sch
        xs <- decodeF fs s
        pure (I x :* xs)
    {- TODO: Opt Def #-}
-}

decodeF' :: NP FieldSchema xs -> Fix DER -> NEValidation Error (NP I xs)
decodeF' _         (Fix (Err err)) = nevError err
decodeF' Nil       (Fix End)       = pure Nil
decodeF' Nil       _               = nevError' "there is leftover"
decodeF' (f :* fs) s@(Fix End)     = case f of
    Req name _ -> nevError' $ "missing fields, e.g." ++ name
    Opt _ _    -> (I Nothing :*) <$> nevFromEither (decodeF fs s)
    Def _ v _  -> (I v :*)       <$> nevFromEither (decodeF fs s)
decodeF' (f :* fs) s'@(Fix (Fin ident contents s)) = case f of
    Req _name sch ->
        let x  = nevFromEither $ decodeA True ident contents sch
            xs = decodeF' fs s
        in (\y ys -> I y :* ys) <$> x <*> xs
    Opt _name sch ->
        let x   = nevFromEither $ decodeA True ident contents sch
            xs  = decodeF' fs s
            xs' = decodeF' fs s'
        in (\y ys -> I (Just y) :* ys) <$> x <*> xs
            <!> (I Nothing :*) <$> xs'
    Def _name v sch ->
        let x   = nevFromEither $ decodeA True ident contents sch
            xs  = decodeF' fs s
            xs' = decodeF' fs s'
        in (\y ys -> I y :* ys) <$> x <*> xs
            <!> (I v :*) <$> xs'

data NEValidation e a
    = Failure (NonEmpty (Bool, e))
    | Success (NonEmpty a)
  deriving (Functor)

nevError :: e -> NEValidation e a
nevError e = Failure $ (True, e) :| []

nevError' :: e -> NEValidation e a
nevError' e = Failure $ (False, e) :| []

nevFromEither :: Either e a -> NEValidation e a
nevFromEither = either nevError pure

instance Applicative (NEValidation e)  where
    pure = Success . pure

    Failure as <*> Failure bs = combineFailure as bs
    Success _  <*> Failure es = Failure es
    Failure es <*> Success _  = Failure es
    Success fs <*> Success xs = Success (fs <*> xs)

instance Alt (NEValidation e) where
    Failure as    <!> Failure bs   = combineFailure as bs
    Failure _     <!> bs@Success{} = bs
    as@Success {} <!> Failure _    = as
    Success as    <!> Success bs   = Success (as <> bs)

combineFailure :: NonEmpty (Bool, e) -> NonEmpty (Bool, e) -> NEValidation e a
combineFailure as bs = Failure $ case NE.filter fst es' of
        e : es -> e :| es
        _      -> es'
      where
        es' = as <> bs


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
        UniversalC -> case tag of
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
