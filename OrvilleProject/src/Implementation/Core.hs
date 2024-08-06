{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}


module Implementation.Core where

import Control.Monad.Reader
import Control.Monad.Except (ExceptT)
import Servant
import Implementation.Config (AppConfig)

type AppMonad = ReaderT AppConfig (ExceptT ServerError IO)

runApp :: AppConfig -> AppMonad a -> Handler a
runApp cfg appMonad = Handler $ runReaderT appMonad cfg


-- correct