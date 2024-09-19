{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE UndecidableInstances #-}

module CronMarshaller where
import qualified Data.Either.Extra as Extra
import Data.Aeson.Types (parseJSON)
import Data.Aeson
    ( (.=),
      (.:),
      FromJSON,
      ToJSON,
      decode,
      encode,
      toJSON,
      withObject,
      object )

import Data.Int (Int32)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Orville.PostgreSQL as O
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Base64 as B64 (encode, decode)
import qualified Data.Text.Encoding as TE (encodeUtf8, decodeUtf8)


-----------------------------------------------------------------------

--CRON TASK : DATE (18th September 2024)

{-

APIs

1. /cron/identifier/format (create - update - delete)
2. /create/identifier
3. /update/identifier
4. /delete//identifier

-}


newtype Identifier = Identifier T.Text deriving (Show, Eq, Generic, FromJSON, ToJSON)
newtype CronId = CronId Int32 deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Status
    = Pending T.Text
    | Success T.Text
    | Error T.Text
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Formats
    = Create T.Text
    | Update T.Text
    | Delete T.Text
    deriving (Show, Eq, Generic)



instance FromJSON Formats where
    parseJSON = withObject "Formats" $ \v -> do
      tag <- v .: "tag"
      case (tag :: String) of
        "Create" -> Create <$> v .: "value"
        "Update" -> Update <$> v .: "value"
        "Delete" -> Delete <$> v .: "value"
        _ -> fail "Invalid tag for Formats"

instance ToJSON Formats where
    toJSON (Create value) = object ["tag" .= ("Create" :: T.Text), "value" .= value]
    toJSON (Update value) = object ["tag" .= ("Update" :: T.Text), "value" .= value]
    toJSON (Delete value) = object ["tag" .= ("Delete" :: T.Text), "value" .= value]







newtype PersonName = PersonName T.Text deriving (Show, Eq, Generic , FromJSON , ToJSON)
newtype PersonAge = PersonAge Int32 deriving (Show, Eq, Generic , FromJSON , ToJSON)
newtype PersonMobile = PersonMobile Int32 deriving (Show, Eq, Generic , FromJSON , ToJSON)

data Person = Person {
  name :: PersonName ,
  age :: PersonAge ,
  mobile :: PersonMobile
} deriving (Show, Eq, Generic , FromJSON , ToJSON)


data Cron = Cron
  { cronId     :: CronId
  , format     :: Formats
  , status     :: Status
  , identifier :: Identifier
  , dataField  :: Person
  } deriving (Show, Generic, FromJSON, ToJSON)



-- Field Definitions
cronIdField :: O.FieldDefinition O.NotNull CronId
cronIdField = O.coerceField (O.integerField "id")

formatField :: O.FieldDefinition O.NotNull Formats
formatField = O.convertField jsonByteStringConversion (O.coerceField (O.jsonbField "format"))

statusField :: O.FieldDefinition O.NotNull Status
statusField = O.convertField jsonByteStringConversion ( O.coerceField (O.unboundedTextField "status"))

identifierField :: O.FieldDefinition O.NotNull Identifier
identifierField = O.convertField  jsonByteStringConversion (O.coerceField (O.unboundedTextField "identifier"))

checkDataField :: O.FieldDefinition O.NotNull Person
checkDataField = O.convertField jsonByteStringConversion (O.jsonbField "data")

-- since we are using data type to be stored in Cron in JSON format
-- to match with DB table type we are converting things into SqlType


jsonByteStringConversion :: (FromJSON a, ToJSON a) => O.SqlType T.Text -> O.SqlType a
jsonByteStringConversion =
  O.tryConvertSqlType (LT.toStrict . decodeUtf8 . encode . toJSON) (Extra.maybeToEither "Error" . decode . encodeUtf8 . LT.fromStrict)


-- Cron Marshaller
cronMarshaller :: O.SqlMarshaller Cron Cron

cronMarshaller =
  Cron
    <$> O.marshallField cronId cronIdField
    <*> O.marshallField format formatField
    <*> O.marshallField status statusField
    <*> O.marshallField identifier identifierField
    <*> O.marshallField dataField checkDataField



cronTable :: O.TableDefinition (O.HasKey CronId) Cron Cron
cronTable =
  O.mkTableDefinition
    "cron"
    (O.primaryKey cronIdField)
    cronMarshaller


--------------------------------------------------------------------------------------------------



data Address = Address {
  a_building :: T.Text ,
  a_street :: T.Text ,
  a_city :: T.Text ,
  a_state :: T.Text ,
  a_country :: T.Text
} deriving (Show , FromJSON , ToJSON , Generic)


data BankAccount = BankAccount {
  bank_name :: T.Text ,
  bank_ifsc :: T.Text ,
  bank_address :: T.Text ,
  bank_email :: T.Text ,
  account_number :: T.Text ,
  account_holder_name :: T.Text

} deriving (Show , Generic, Eq , FromJSON , ToJSON)



-- Encryption
myEncode :: T.Text -> T.Text
myEncode = TE.decodeUtf8 . B64.encode . TE.encodeUtf8

-- Decryption 
myDecode :: T.Text -> T.Text
myDecode input = case B64.decode (TE.encodeUtf8 input) of
  Right bs -> TE.decodeUtf8 bs
  Left _ -> error "Invalid Base64 data"



-- Encryption BankAccount
encryptBankAccount :: BankAccount -> BankAccount
encryptBankAccount bankAccount =
  BankAccount
    { bank_name = myEncode $ bank_name bankAccount
    , bank_ifsc = myEncode $ bank_ifsc bankAccount
    , bank_address = myEncode $ bank_address bankAccount
    , bank_email = myEncode $ bank_email bankAccount
    , account_number = myEncode $ account_number bankAccount
    , account_holder_name = myEncode $ account_holder_name bankAccount
    }

-- Decryption BankAccount
decryptBankAccount :: BankAccount -> BankAccount
decryptBankAccount bankAccount =
  BankAccount
    { bank_name = myDecode $ bank_name bankAccount
    , bank_ifsc = myDecode $ bank_ifsc bankAccount
    , bank_address = myDecode $ bank_address bankAccount
    , bank_email = myDecode $ bank_email bankAccount
    , account_number = myDecode $ account_number bankAccount
    , account_holder_name = myDecode $ account_holder_name bankAccount
    }


data Graph = Graph
  {
    g_id :: CronId ,
    g_identifier :: Identifier ,
    g_address :: Address ,
    g_bank_account :: BankAccount ,
    g_details :: Person
  } deriving (Show, Generic, FromJSON, ToJSON)



graphIdField :: O.FieldDefinition O.NotNull CronId
graphIdField = O.coerceField (O.integerField "g_id")


graphIdentifierField ::  O.FieldDefinition O.NotNull Identifier
graphIdentifierField = O.coerceField (O.unboundedTextField "g_identifier")


graphAddressField :: O.FieldDefinition O.NotNull Address
graphAddressField = O.convertField jsonByteStringConversion (O.jsonbField "g_address")


graphBankAccountField :: O.FieldDefinition O.NotNull BankAccount
graphBankAccountField = O.convertField jsonByteStringConversion $ O.coerceField $ O.unboundedTextField "g_bank_account"


graphDetailsField :: O.FieldDefinition O.NotNull Person
graphDetailsField = O.convertField jsonByteStringConversion $ O.coerceField  $ O.unboundedTextField "g_details"



--Graph Marshaller || Use marshallReadOnly
graphMarshaller :: O.SqlMarshaller Graph Graph
graphMarshaller =
  Graph
    <$> O.marshallField g_id graphIdField
    <*> O.marshallField g_identifier graphIdentifierField
    <*> O.marshallField g_address graphAddressField
    <*> O.marshallField g_bank_account graphBankAccountField
    <*> O.marshallField g_details graphDetailsField



graphTable :: O.TableDefinition (O.HasKey CronId) Graph Graph
graphTable =
  O.mkTableDefinition
    "graph"
    (O.primaryKey graphIdField)
    graphMarshaller





