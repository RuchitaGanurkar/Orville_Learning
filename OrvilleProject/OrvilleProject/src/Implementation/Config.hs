module Implementation.Config where


import qualified Orville.PostgreSQL as O

data AppConfig = AppConfig {
    myPool :: O.ConnectionPool
}
