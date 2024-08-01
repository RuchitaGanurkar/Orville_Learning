{-# LANGUAGE DeriveGeneric #-}



module Implementation.Models.Student where

import qualified Data.Text as T
import qualified Orville.PostgreSQL as O
import qualified Data.Int as Int
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

type StudentId = Int.Int32
type StudentName = T.Text
type StudentAge = Int.Int32

data Student = Student
  { studentId :: StudentId
  , studentName :: StudentName
  , studentAge :: StudentAge
  }
  deriving (Show)

studentIdField :: O.FieldDefinition O.NotNull StudentId
studentIdField = O.integerField "id"

studentNameField :: O.FieldDefinition O.NotNull StudentName
studentNameField = O.unboundedTextField "name"

studentAgeField :: O.FieldDefinition O.NotNull StudentAge
studentAgeField = O.integerField "age"

studentMarshaller :: O.SqlMarshaller Student Student
studentMarshaller =
  Student
    <$> O.marshallField studentId studentIdField
    <*> O.marshallField studentName studentNameField
    <*> O.marshallField studentAge studentAgeField

studentTable :: O.TableDefinition (O.HasKey StudentId) Student Student
studentTable =
  O.mkTableDefinition "student" (O.primaryKey studentIdField) studentMarshaller

data ApiStudent = ApiStudent
  { apiStudentId :: StudentId
  , apiStudentName :: StudentName
  , apiStudentAge :: StudentAge
  } deriving (Generic, Show)

instance FromJSON ApiStudent
instance ToJSON ApiStudent
