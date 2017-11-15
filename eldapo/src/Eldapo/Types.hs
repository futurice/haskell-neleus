{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
module Eldapo.Types where

import Data.Set     (Set)
import GHC.Generics (Generic)
import Neleus

import qualified Data.ByteString as BS

-- | >>> prettySchema (schema :: Schema LDAPMessage)
-- LDAPMessage ::= SEQUENCE {messageID INTEGER
--                           protocolOp CHOICE {bindRequest BindRequest
--                                              bindResponse BindResponse
--                                              unbindRequest UnbindRequest
--                                              searchRequest SearchRequest
--                                              ...}
--                           controls [0] SEQUENCE OF Control OPTIONAL}
data LDAPMessage = LDAPMessage
    { lmId       :: MessageID
    , lmOp       :: NS I
        '[ BindRequest
        , BindResponse
        , UnbindRequest
        , SearchRequest
        , ASN1Value
        ]
    , lmControls :: Maybe Controls
    }
  deriving (Show, Generic)

instance ASN1 LDAPMessage where
    schema = Neleus.sequence $
        required "messageID" :*
        required' "protocolOp" opts :*
        optional' "controls" (tagged ContextC 0 schema) :*
        Nil
      where
        opts = SChoice $
            option "bindRequest" :*
            option "bindResponse" :*
            option "unbindRequest" :*
            option "searchRequest" :*
            option "..." :*
            Nil

-- | @
-- MessageID ::= INTEGER (0 .. maxInt)
--
-- maxInt INTEGER ::= 2147483647 -- (2^^31 - 1)
-- @
type MessageID = Integer -- Int32

-- | >>> prettySchema (schema :: Schema LDAPString)
-- LDAPString ::= OCTET STRING
newtype LDAPString = LDAPString OctetString deriving (Show, Eq, Ord, Generic)
instance ASN1 LDAPString where schema = namedNewtype

-- | >>> prettySchema (schema :: Schema LDAPOID)
-- LDAPOID ::= OCTET STRING
newtype LDAPOID = LDAPOID OctetString deriving (Show, Eq, Ord, Generic)
instance ASN1 LDAPOID where schema = namedNewtype

-- | >>> prettySchema (schema :: Schema LDAPDN)
-- LDAPDN ::= OCTET STRING
newtype LDAPDN = LDAPDN OctetString deriving (Show, Eq, Ord, Generic)
instance ASN1 LDAPDN where schema = namedNewtype

type AttributeDescription = LDAPString
type AssertionValue = OctetString

data AttributeValueAssertion = AttributeValueAssertion
    { avaDesc :: AttributeDescription
    , avaValue :: AssertionValue
    }
  deriving (Eq, Ord, Show, Generic)

instance ASN1 AttributeValueAssertion where
    schema = Neleus.sequence $
        required "avaDesc" :*
        required  "avaValue" :*
        Nil

-- | @
-- Controls ::= SEQUENCE OF Control
-- @
type Controls = [Control]

-- | @
-- Control ::= SEQUENCE {
--         controlType             LDAPOID,
--         criticality             BOOLEAN DEFAULT FALSE,
--         controlValue            OCTET STRING OPTIONAL }
-- @
data Control = Control
    { controlType        :: LDAPOID
    , controlCriticality :: Bool
    , controlValue       :: Maybe OctetString
    }
  deriving (Show, Generic)

instance ASN1 Control where
    schema = Neleus.sequence $
        required "controlType" :*
        defaulted "criticality" False :*
        optional "controlValue" :*
        Nil

{-
-------------------------------------------------------------------------------
-- LDAPResult
-------------------------------------------------------------------------------

-- | @
-- ENUMERATED {
-- success                      (0),
-- operationsError              (1),
-- protocolError                (2),
-- ...
-- authMethodNotSupported       (7),
-- strongAuthRequired           (8),
-- ...
-- inappropriateAuthentication  (48),
-- ... }
-- @
data ResultCode
    = RCSuccess
    | RCProtocolError
    | RCInvalidCredentials
  deriving Show

-- @
-- LDAPResult ::= SEQUENCE {
--         resultCode      ENUMERATED {
--         matchedDN       LDAPDN,
--         errorMessage    LDAPString,
--         referral        [3] Referral OPTIONAL }
-- @
-}

-- | >>> prettySchema (schema :: Schema LDAPResult)
-- LDAPResult ::= SEQUENCE {resultCode ANY
--                          matchedDN LDAPDN
--                          erroMessage LDAPString
--                          referral [3] ANY OPTIONAL}
data LDAPResult = LDAPResult
    { resCode         :: ASN1Value
    , resMatchedDN    :: LDAPDN
    , resErrorMessage :: LDAPString
    , resReferreal    :: Maybe ASN1Value
    }
  deriving (Show, Generic)

instance ASN1 LDAPResult where
    schema = Neleus.sequence $
        required "resultCode" :*
        required "matchedDN" :*
        required "erroMessage" :*
        optional' "referral" (tagged ContextC 3 schema) :*
        Nil

-------------------------------------------------------------------------------
-- Bind
-------------------------------------------------------------------------------

-- | >>> prettySchema (schema :: Schema BindRequest)
-- BindRequest ::= [APPLICATION 0] SEQUENCE {version INTEGER
--                                           name LDAPDN
--                                           authentication AuthenticationChoice}
data BindRequest = BindRequest
    { brVersion        :: Integer
    , brName           :: LDAPDN
    , brAuthentication :: AuthenticationChoice
    }
  deriving (Generic, Show)

instance ASN1 BindRequest where
    schema = Neleus.taggedSequence ApplicationC 0 $
        required "version" :*
        required "name" :*
        required "authentication" :*
        Nil

-- | @
-- AuthenticationChoice ::= CHOICE {
--         simple                  [0] OCTET STRING,
--                                  -- 1 and 2 reserved
--         sasl                    [3] SaslCredentials }
-- @
data AuthenticationChoice
    = AuthSimple BS.ByteString
    | AuthSasl SaslCredentials
  deriving (Show, Generic)

instance ASN1 AuthenticationChoice where
    schema = choice $
        option' "simpl" (tagged ContextC 0 schema) :*
        option' "sasl"  (tagged ContextC 3 schema) :*
        Nil

-- | @
-- SaslCredentials ::= SEQUENCE {
--         mechanism               LDAPString,
--         credentials             OCTET STRING OPTIONAL }
-- @
data SaslCredentials = SaslCredentials
    { scMechanism   :: LDAPString
    , scCredentials :: Maybe BS.ByteString
    }
  deriving (Show, Generic)

instance ASN1 SaslCredentials where
    schema = Neleus.sequence $
        required "mechanism" :*
        optional "credentials" :*
        Nil

-- | >>> prettySchema (schema :: Schema BindResponse)
-- BindResponse ::= SEQUENCE {resultCode ANY
--                            matchedDN LDAPDN
--                            erroMessage LDAPString
--                            referral [3] ANY OPTIONAL
--                            serverSaslCreds [7] OCTET STRING OPTIONAL}
data BindResponse = BindResponse
    { bdCode             :: ASN1Value
    , brMatchedDN        :: LDAPDN
    , brErrorMessage     :: LDAPString
    , brReferreal        :: Maybe ASN1Value
    , brServerSaslsCreds :: Maybe OctetString
    }
  deriving (Show, Generic)

instance ASN1 BindResponse where
    schema = Neleus.sequence $
        required "resultCode" :*
        required "matchedDN" :*
        required "erroMessage" :*
        optional' "referral" (tagged ContextC 3 schema) :*
        optional' "serverSaslCreds" (tagged ContextC 7 schema) :*
        Nil

-- | >>> prettySchema (schema :: Schema UnbindRequest)
-- UnbindRequest ::= [APPLICATION 2] NULL
data UnbindRequest = UnbindRequest
  deriving (Show, Generic)

instance ASN1 UnbindRequest where
    schema = named' (const UnbindRequest) (const ()) $
        tagged ApplicationC 2 (schema :: Schema ())

-------------------------------------------------------------------------------
-- Search
-------------------------------------------------------------------------------

data SearchRequest = SearchRequest
    { srBaseObject :: LDAPDN
    , scope        :: ASN1Value
    , derefAliases :: ASN1Value
    , sizeLimit    :: Integer
    , timeLimit    :: Integer
    , typesOnly    :: Bool
    , filter       :: Filter
    , attributes   :: ASN1Value
    }
  deriving (Show, Generic)

instance ASN1 SearchRequest where
    schema = taggedSequence ApplicationC 3 $
        required "baseObject" :*
        required "scope" :*
        required "derefAliases" :*
        required "sizeLimit" :*
        required "timeLimit" :*
        required "typesOnly" :*
        required "filter" :*
        required "attributes" :*
        Nil

-- | >>> prettySchema (schema :: Schema Filter)
-- Filter ::= CHOICE {and [0] SET OF Filter
--                    or [1] SET OF Filter
--                    not [2] Filter
--                    equalityMatch [3] AttributeValueAssertion
--                    ...}
data Filter
    = FilterAnd (Set Filter)
    | FilterOr (Set Filter)
    | FilterNot Filter
    | FilterEqualityMatch AttributeValueAssertion
    | FilterOther ASN1Value
  deriving (Eq, Ord, Show, Generic)

instance ASN1 Filter where
    schema = choice $
        option' "and"           (tagged ContextC 0 schema) :*
        option' "or"            (tagged ContextC 1 schema) :*
        option' "not"           (tagged ContextC 2 schema) :*
        option' "equalityMatch" (tagged ContextC 3 schema) :*
        option "..." :*
        Nil

-------------------------------------------------------------------------------
-- Extended
-------------------------------------------------------------------------------

-- ExtendedRequest ::= [APPLICATION 23] SEQUENCE {
--         requestName      [0] LDAPOID,
--         requestValue     [1] OCTET STRING OPTIONAL }
--
-- ExtendedResponse ::= [APPLICATION 24] SEQUENCE {
--         COMPONENTS OF LDAPResult,
--         responseName     [10] LDAPOID OPTIONAL,
--         response         [11] OCTET STRING OPTIONAL }
