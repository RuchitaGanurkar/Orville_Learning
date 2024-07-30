
{-# LANGUAGE DataKinds #-}



module Main where

import Network.Wai.Handler.Warp (run)
import Servant
import Implementation.Config (AppConfig(..), initialConfig)
import Implementation.Core (AppMonad, runApp)
import Implementation.Routes (api, appServer)
import Implementation.DB (migrateDB)

app :: AppConfig -> Application
app cfg = logStdoutDev $ serve api (hoistServer api (runApp cfg) appServer)

main :: IO ()
main = do
  appConfig <- initialConfig
  runDBMigrate appConfig
  run 3000 (app appConfig)