{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A 'Store' instance which communicates with a data store over a network.
module Telescope.Source.Network where

import           Data.Proxy           (Proxy (Proxy))
import           Reflex.Dom           (MonadWidget)
import           Reflex.Dynamic       (constDyn)
import           Servant.API          ((:<|>) ((:<|>)))
import           Servant.Reflex       (BaseUrl (BasePath), client)

-- import           Servant                    ((:<|>)
-- import           Network.HTTP.Client        (defaultManagerSettings, newManager)
-- import           Servant.Client.Ghcjs       (BaseUrl (..), ClientM,
--                                              Scheme (Http), client, mkClientEnv,
--                                              runClientM)

import           Telescope.Server.API (API)

runGUI :: forall t m. MonadWidget t m => m ()
runGUI = do
  let rmAll :: _
      rmAll :<|> viewAll :<|> setAll =
        client (Proxy :: Proxy API) (Proxy :: Proxy m) (Proxy :: Proxy ()) (constDyn (BasePath "/"))
  pure ()

-- | Run client functions on a network.
-- TODO: Lift exceptions.
-- runOnNetwork :: SourceConfig -> ClientM a -> IO a
-- runOnNetwork config ops =
--   case hostPort config of
--     -- Nothing -> Left
--     Just (host, port) -> do
--       eitherEA   <- liftIO $ do
--         manager' <- newManager defaultManagerSettings
--         runClientM ops $ mkClientEnv manager' (BaseUrl Http host port "")
--       case eitherEA of
--         -- Left e  -> exception $ ConnException $ "Network error: " ++ e
        -- Right a -> pure a

-- network :: Source
-- network = Source {
--     _sourceName         = "network"
--   , _sourceRmS          = Shortcuts.rmS   viewAllS setAllS
--   , _sourceSetS         = Shortcuts.setS  viewAllS setAllS
--   , _sourceViewS        = Shortcuts.viewS viewAllS
--   , _sourceRmAllS       = rmAllS
--   , _sourceViewAllS     = viewAllS
--   , _sourceSetAllS      = setAllS
--   , _sourceEmitUpdatesS =
--       const $ const $ const $
--         print "Network.emitUpdatesS not implemented"
--   }

-- -- | A configuration to operate on a data source.
-- networkConfig :: FilePath -> String -> Int -> IO SourceConfig
-- networkConfig path host port = do
--   handlersMVar <- newMVar []
--   pure SourceConfig {
--       path     = path
    -- , source   = network
    -- , handlers = handlersMVar
    -- , hostPort = Just (host, port)
    -- }

-- rmAllS config (TableKey tkS) = void $
--   runOnNetwork config $ rmAllS'Client tkS

-- viewAllS config (TableKey tkS) = do
--   list <- runOnNetwork config $ viewAllS'Client tkS
--   pure $ Map.fromList $ map (\(rkS, v) -> (RowKey rkS, pack v)) list

-- setAllS config (TableKey tkS) all = void $ do
--   runOnNetwork config $ setAllS'Client tkS $
    -- Map.fromList $ [(rkS, unpack v) | (RowKey rkS, v) <- Map.toList all]

-- * Derived client functions.

-- rmAllS'Client :: String -> ClientM NoContent
-- rmAllS'Client :: String -> _

-- viewAllS'Client :: String -> ClientM [(String, String)]
-- viewAllS'Client :: String -> _

-- setAllS'Client :: String -> Map String String -> ClientM NoContent
-- setAllS'Client :: String -> Map String String -> _
