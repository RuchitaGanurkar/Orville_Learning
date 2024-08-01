{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Implementation.Routes where

import Servant
import Implementation.Handlers.StudentHandlers
import Implementation.Models.Student (ApiStudent)
import Data.Int
import qualified Data.Text as T
import Implementation.Core

type API =  "student" :> ReqBody '[JSON] ApiStudent :> Post '[JSON] NoContent
        :<|> "student" :> Capture "id" Int32 :> Get '[JSON] ApiStudent
        :<|> "student" :> Capture "name" T.Text :> Get '[JSON] ApiStudent

api :: Proxy API
api = Proxy

appServer :: ServerT API AppMonad
appServer = postStudent :<|> getStudentById :<|> getStudentByName
