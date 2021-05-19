module Lib.Client.Types where

import           Network.HTTP.Client            ( newManager
                                                , defaultManagerSettings
                                                )

import           Relude.Extra                   ( firstF )
import           Control.Exception              ( catch
                                                , throwIO
                                                , try
                                                )
import           Servant.Auth.Client
import           Lib.Data.Photographer          ( Photographers )
import           Graphics.UI.Threepenny.Core
import           Servant                 hiding ( throwError )
import qualified Servant.Client                as Servant

import           Lib.Client.Error               ( ClientAppError
                                                , throwError
                                                , ClientAppException(..)
                                                , ClientAppErrorType(..)
                                                )

import qualified Control.Monad.Except          as E

import           Lib.Api.Types
import           Lib.Api


data ClientEnv (m :: Type -> Type) = ClientEnv
    { postLogin :: LoginForm -> ClientApp (Headers '[Header "Set-Cookie" Text] NoContent)
    , getPhotographers :: Token -> ClientApp Photographers
    }


type ClientAppEnv = ClientEnv ClientApp


newtype ClientApp a = ClientApp
    { unClientApp :: ReaderT ClientAppEnv UI a
    } deriving newtype ( Functor
                        , Applicative
                        , Monad
                        , MonadIO
                        , MonadReader ClientAppEnv
                        )

instance MonadUI ClientApp where
    liftUI m = ClientApp (ReaderT (const m))

instance E.MonadError ClientAppError ClientApp where
    throwError :: ClientAppError -> ClientApp a
    throwError = undefined

    catchError :: ClientApp a -> (ClientAppError -> ClientApp a) -> ClientApp a
    catchError action handler = ClientApp $ ReaderT $ \env -> do
        undefined


runClientAppAsUI :: ClientAppEnv -> ClientApp a -> UI (Either ClientAppError a)
runClientAppAsUI env = undefined -- firstF unClientAppException . try . runClientApp env


runClientApp :: ClientAppEnv -> ClientApp a -> UI a
runClientApp env = usingReaderT env . unClientApp


runClientM :: Servant.ClientEnv -> Servant.ClientM a -> ClientApp a
runClientM cenv client = do
    e <- liftIO $ Servant.runClientM client cenv
    case e of
        Left  servantErr -> throwError $ ClientError "servantErr"
        Right a          -> pure a


clients :: Servant.ClientEnv -> ClientAppEnv
clients cenv =
    let
        api = Servant.hoistClient siteAPI
                                  (runClientM cenv)
                                  (Servant.client siteAPI)
        public                          :<|> protected      = api
        postLogin                       :<|> docs           = public
        getPhotographers :<|> getPermissions = protected
    in
        ClientEnv { .. }

mkClientAppEnv :: IO ClientAppEnv
mkClientAppEnv = do
    let serverPort = 8080
    let baseUrl' = Servant.BaseUrl Servant.Http "localhost" serverPort ""
    manager' <- newManager defaultManagerSettings
    let cenv = Servant.mkClientEnv manager' baseUrl'
    pure $ clients cenv
