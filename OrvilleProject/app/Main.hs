{-# LANGUAGE OverloadedStrings #-}

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import Implementation.Routes (api, appServer)
import Implementation.Config (initializeAppConfig)
import Implementation.Core (runApp)
import Implementation.DB (runDBMigration)

app :: AppConfig -> Application
app cfg = logStdoutDev $ serve api $ hoistServer api (runApp cfg) appServer

main :: IO ()
main = do
  appConfig <- initializeAppConfig
  runApp appConfig runDBMigration
  run 3000 (app appConfig)
