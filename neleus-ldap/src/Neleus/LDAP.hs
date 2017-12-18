{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
module Neleus.LDAP where

import Data.Set     (Set)
import Data.String  (IsString (..))
import GHC.Generics (Generic)
import Neleus

import qualified Data.ByteString as BS

-- | >>> prettySchema (schema :: Schema LDAPMessage)
-- LDAPMessage ::= SEQUENCE {messageID INTEGER
--                           protocolOp CHOICE {bindRequest BindRequest
--                                              bindResponse BindResponse
--                                              unbindRequest UnbindRequest
--                                              searchRequest SearchRequest
--                                              searchResultEntry SearchResultEntry
--                                              searchResultDone SearchResultDone
--                                              ...}
--                           controls [0] SEQUENCE OF Control OPTIONAL}
data LDAPMessage = LDAPMessage
    { lmId       :: MessageID
    , lmOp       :: NS I
        '[ BindRequest
        , BindResponse
        , UnbindRequest
        , SearchRequest
        , SearchResultEntry
        , SearchResultDone
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
            option "searchResultEntry" :*
            option "searchResultDone" :*
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

instance IsString LDAPString where
    fromString = LDAPString . fromString

-- | >>> prettySchema (schema :: Schema LDAPOID)
-- LDAPOID ::= OCTET STRING
newtype LDAPOID = LDAPOID OctetString deriving (Show, Eq, Ord, Generic)
instance ASN1 LDAPOID where schema = namedNewtype

-- | >>> prettySchema (schema :: Schema LDAPDN)
-- LDAPDN ::= OCTET STRING
newtype LDAPDN = LDAPDN OctetString deriving (Show, Eq, Ord, Generic)
instance ASN1 LDAPDN where schema = namedNewtype

instance IsString LDAPDN where
    fromString = LDAPDN . fromString

type AttributeDescription = LDAPString
type AttributeValue = OctetString
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

-- | >>> prettySchema (schema :: Schema Control)
-- Control ::= SEQUENCE {controlType LDAPOID
--                       criticality BOOLEAN DEFAULT ?
--                       controlValue OCTET STRING OPTIONAL}
--
-- /TODO:/ default printing in schema. @criticality BOOLEAN DEFAULT TRUE@.
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

-------------------------------------------------------------------------------
-- LDAPResult
-------------------------------------------------------------------------------

-- | >>> prettySchema (schema :: Schema ResultCode)
-- ResultCode ::= ENUMERATED {success (0)
--                            operationsError (1)
--                            protocolError (2)
--                            inappropriateAuthentication (48)}
data ResultCode
    = RCSuccess
    | RCOperationsError
    | RCProtocolError
    | RCInvalidCredentials
  deriving (Show, Generic)

instance ASN1 ResultCode where
    schema = enumeration $
        enumOption "success" 0 :::
        enumOption "operationsError" 1 :::
        enumOption "protocolError" 2 :::
        enumOption "inappropriateAuthentication" 48 :::
        VNil

-- | >>> prettySchema (schema :: Schema LDAPResult)
-- LDAPResult ::= SEQUENCE {resultCode ResultCode
--                          matchedDN LDAPDN
--                          erroMessage LDAPString
--                          referral [3] ANY OPTIONAL}
data LDAPResult = LDAPResult
    { resCode         :: ResultCode
    , resMatchedDN    :: LDAPDN
    , resErrorMessage :: LDAPString
    , resReferral     :: Maybe ASN1Value
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

-- | >>> prettySchema (schema :: Schema AuthenticationChoice)
-- AuthenticationChoice ::= CHOICE {simpl [0] OCTET STRING
--                                  sasl [3] SaslCredentials}
data AuthenticationChoice
    = AuthSimple BS.ByteString
    | AuthSasl SaslCredentials
  deriving (Show, Generic)

instance ASN1 AuthenticationChoice where
    schema = choice $
        option' "simpl" (tagged ContextC 0 schema) :*
        option' "sasl"  (tagged ContextC 3 schema) :*
        Nil

-- | >>> prettySchema (schema :: Schema SaslCredentials)
-- SaslCredentials ::= SEQUENCE {mechanism LDAPString
--                               credentials OCTET STRING OPTIONAL}
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
-- BindResponse ::= SEQUENCE {resultCode ResultCode
--                            matchedDN LDAPDN
--                            erroMessage LDAPString
--                            referral [3] ANY OPTIONAL
--                            serverSaslCreds [7] OCTET STRING OPTIONAL}
data BindResponse = BindResponse
    { brCode             :: ResultCode
    , brMatchedDN        :: LDAPDN
    , brErrorMessage     :: LDAPString
    , brReferral         :: Maybe Referral
    , brServerSaslsCreds :: Maybe OctetString
    }
  deriving (Show, Generic)

-- | TODO: proper Referral type
type Referral = ASN1Value

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

-- | >>> prettySchema (schema :: Schema SearchRequest)
-- SearchRequest ::= [APPLICATION 3] SEQUENCE {baseObject LDAPDN
--                                             scope ANY
--                                             derefAliases ANY
--                                             sizeLimit INTEGER
--                                             timeLimit INTEGER
--                                             typesOnly BOOLEAN
--                                             filter Filter
--                                             attributes ANY}
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
--                    present [7] LDAPString
--                    ...}
data Filter
    = FilterAnd (Set Filter)
    | FilterOr (Set Filter)
    | FilterNot Filter
    | FilterEqualityMatch AttributeValueAssertion
    | FilterPresent AttributeDescription
    | FilterOther ASN1Value
  deriving (Eq, Ord, Show, Generic)

instance ASN1 Filter where
    schema = choice $
        option' "and"           (tagged ContextC 0 schema) :*
        option' "or"            (tagged ContextC 1 schema) :*
        option' "not"           (tagged ContextC 2 schema) :*
        option' "equalityMatch" (tagged ContextC 3 schema) :*
        option' "present"       (tagged ContextC 7 schema) :*
        option "..." :*
        Nil

-- | >>> prettySchema (schema :: Schema SearchResultEntry)
-- SearchResultEntry ::= [APPLICATION 4] SEQUENCE {objectName LDAPDN
--                                                 sreAttributes SEQUENCE OF PartialAttribute}
data SearchResultEntry = SearchResultEntry
    { sreObjectName :: LDAPDN
    , sreAttributes :: [PartialAttribute]
    }
  deriving (Show, Generic)

instance ASN1 SearchResultEntry where
    schema = taggedSequence ApplicationC 4 $
        required "objectName" :*
        required "sreAttributes" :*
        Nil

-- | >>> prettySchema (schema :: Schema PartialAttribute)
-- PartialAttribute ::= SEQUENCE {type LDAPString
--                                vals SET OF OCTET STRING}
data PartialAttribute = PartialAttribute
    { paType :: AttributeDescription
    , paVals :: Set AttributeValue
    }
  deriving (Show, Generic)

instance ASN1 PartialAttribute where
    schema = Neleus.sequence $
        required "type" :*
        required "vals" :*
        Nil

-- | >>> prettySchema (schema :: Schema SearchResultDone)
-- SearchResultDone ::= [APPLICATION 5] SEQUENCE {resultCode ResultCode
--                                                matchedDN LDAPDN
--                                                erroMessage LDAPString
--                                                referral [3] ANY OPTIONAL}
data SearchResultDone = SearchResultDone
    { srdCode             :: !ResultCode
    , srdMatchedDN        :: !LDAPDN
    , srdErrorMessage     :: !LDAPString
    , srdReferral         :: !(Maybe Referral)
    }
  deriving (Show, Generic)

instance ASN1 SearchResultDone where
    schema = taggedSequence ApplicationC 5 $
        required "resultCode" :*
        required "matchedDN" :*
        required "erroMessage" :*
        optional' "referral" (tagged ContextC 3 schema) :*
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
