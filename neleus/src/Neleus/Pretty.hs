{-# LANGUAGE GADTs #-}
module Neleus.Pretty (prettySchema) where

import Control.Monad             (unless)
import Control.Monad.Trans.State (State, execState, get, modify', put)
import Data.Set                  (Set)
import Prelude ()
import Prelude.Compat
import Text.PrettyPrint          ((<+>))

import qualified Data.Set         as Set
import qualified Text.PrettyPrint as PP

import Neleus.Generics
import Neleus.Schema
import Neleus.Types

prettySchema :: Schema a -> PP.Doc
prettySchema sch = case execState action st0 of
    St _ _ doc -> doc
  where
    action = do
        p <- schemaP sch
        case p of
            PNamed _ -> pure ()
            _        -> write (ppPSchema p)
        loop

    loop = do
        s <- get
        case sQueue s of
            [] -> pure ()
            ((n, m) : xs) -> do
                put $ s { sQueue = xs }
                p <- m
                write (PP.text n <+> PP.text "::=" <+> ppPSchema p)

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data PSchema
    = PNull
    | PBool
    | PInteger
    | POctetString
    | PNamed SchemaName
    | PTagged Explicitness Class Tag PSchema
    | PSequence [(FieldName, PSchema, PFieldMod)]
    | PChoice [(OptionName, PSchema)]
    | PSetOf PSchema
    | PSequenceOf PSchema
    | PAny

data PFieldMod = PReq | POpt | PDef

-------------------------------------------------------------------------------
-- St
-------------------------------------------------------------------------------

type PM = State St

-- State of pp printing
data St = St
    { sNames :: Set SchemaName
    , sQueue :: [(SchemaName, PM PSchema)]
    , sDoc   :: PP.Doc
    }

st0 :: St
st0 = St mempty mempty PP.empty

write :: PP.Doc -> PM ()
write doc = modify' $ \s -> s
    { sDoc = sDoc s PP.$$ doc
    }

-------------------------------------------------------------------------------
-- Comm
-------------------------------------------------------------------------------

namedSchema :: SchemaName -> Schema a -> PM PSchema
namedSchema n sch = do
    s <- get
    unless (n `Set.member` sNames s) $ put $ s
        { sQueue = sQueue s ++ [(n, schemaP sch)]
        , sNames = Set.insert n $ sNames s
        }
    pure (PNamed n)

schemaP :: Schema a -> PM PSchema
schemaP SNull                     = pure PNull
schemaP SBool                     = pure PBool
schemaP SInteger                  = pure PInteger
schemaP SOctetString              = pure POctetString
schemaP (SNamed n _ sch)          = namedSchema n sch
schemaP (SSetOf sch)              = PSetOf <$> schemaP sch
schemaP (SSequenceOf sch)         = PSequenceOf <$> schemaP sch
schemaP (SSequence fs)            = PSequence <$> fSchemaP fs
schemaP (SChoice fs)              = PChoice <$> oSchemaP fs
schemaP (STagged exc cls tag sch) = PTagged exc cls tag <$> schemaP sch
schemaP SAny                      = pure PAny

fSchemaP :: NP FieldSchema a -> PM [(FieldName, PSchema, PFieldMod)]
fSchemaP Nil = pure []
fSchemaP (f :* fs) = (:) <$> pp f <*> fSchemaP fs
  where
    pp (Req n sch)   = (\p -> (n, p, PReq)) <$> schemaP sch
    pp (Opt n sch)   = (\p -> (n, p, POpt)) <$> schemaP sch
    pp (Def n _ sch) = (\p -> (n, p, PDef)) <$> schemaP sch

oSchemaP :: NP OptionSchema a -> PM [(OptionName, PSchema)]
oSchemaP Nil = pure []
oSchemaP (SOption n sch :* fs) = mk <$> schemaP sch <*> oSchemaP fs
  where
    mk p xs = (n,p) : xs

ppPSchema :: PSchema -> PP.Doc
ppPSchema PNull           = PP.text "NULL"
ppPSchema PBool           = PP.text "BOOLEAN"
ppPSchema PInteger        = PP.text "INTEGER"
ppPSchema POctetString    = PP.text "OCTET STRING"
ppPSchema (PNamed n)      = PP.text n
ppPSchema (PSetOf p)      = PP.text "SET OF" <+> ppPSchema p
ppPSchema (PSequenceOf p) = PP.text "SEQUENCE OF" <+> ppPSchema p
ppPSchema (PSequence fs)            =
    PP.text "SEQUENCE" <+> PP.braces (PP.vcat $ map pp fs)
  where
    pp (f, p, m) = PP.text f <+> ppPSchema p <+> case m of
        PReq -> PP.empty
        POpt -> PP.text "OPTIONAL"
        PDef -> PP.text "DEFAULT ?"
ppPSchema (PChoice fs) =
    PP.text "CHOICE" <+> PP.braces (PP.vcat $ map pp fs)
  where
    pp ("...", PAny) = PP.text "..."
    pp (f, p)        = PP.text f <+> ppPSchema p

ppPSchema (PTagged exc cls tag sch) =
    PP.brackets (cls' <+> tag') <+> exc' <+> sch'
  where
    exc' = case exc of
        Explicit -> PP.text "EXPLICIT"
        Implicit -> PP.empty
    cls' = case cls of
        UniversalC   -> PP.text "UNIVERSAL"
        ApplicationC -> PP.text "APPLICATION"
        ContextC     -> PP.empty
        PrivateC     -> PP.text "PRIVATE"
    tag' = PP.integer (fromIntegral tag)
    sch' = ppPSchema sch
ppPSchema PAny         = PP.text "ANY"
