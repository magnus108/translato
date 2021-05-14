module Lib.Client where


import Servant.API
import Servant.Auth.Client
import Servant.Client

import Lib.Server
import qualified Lib.App                        as App
import           Lib.App                        ( AppEnv
                                                , Env(..)
                                                , InChan(..)
                                                , OutChan(..)
                                                , runApp
                                                , runAppAsIO
                                                , App(..)
                                                , throwError
                                                , AppErrorType(..)
                                                , Clients(..)
                                                , siteAPI
                                                )



runClientM' :: ClientEnv -> ClientM a -> App a
runClientM' cenv client = do
    e <- liftIO $ runClientM client cenv
    case e of
        Left  servantErr -> throwError $ ServerError "servantErr"
        Right a          -> pure a



clients :: ClientEnv -> Clients
clients cenv =
    let public :<|> protected = hoistClient siteAPI (runClientM' cenv) (client siteAPI)
        postLogin :<|> docs = public
        accessKey :<|> getPhotographers :<|> getPermissions = protected
    in
        Clients postLogin getPhotographers
