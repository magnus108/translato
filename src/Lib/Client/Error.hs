module Lib.Client.Error
    ( ClientAppError(..)
    , ClientAppErrorType(..)
    , ClientAppException(..)
    , WithError
    , throwError
    , throwOnNothing
    , throwOnNothingM
    , SourcePosition(..)
    , toSourcePosition
    )
where

import qualified Control.Monad.Except          as E
                                                ( throwError
                                                , catchError
                                                )
import           Control.Monad.Except           ( MonadError )
import           GHC.Stack                      ( SrcLoc
                                                    ( SrcLoc
                                                    , srcLocModule
                                                    , srcLocStartLine
                                                    )
                                                )

type WithError m = (MonadError ClientAppError m, HasCallStack)


-- wtf is this
instance MonadError ClientAppError ((->) a) where
    throwError e = E.throwError e
    catchError e = E.catchError e


throwError :: WithError m => ClientAppErrorType -> m a
throwError = E.throwError . ClientAppError (toSourcePosition callStack)


newtype SourcePosition = SourcePosition
    { unSourcePosition :: Text
    } deriving newtype (Show, Eq)


newtype ClientAppException = ClientAppException
    { unClientAppException :: ClientAppError
    } deriving (Show)
      deriving anyclass (Exception)


data ClientAppError = ClientAppError
    { clientAppErrorCallStack :: !SourcePosition
    , clientAppErrorType      :: !ClientAppErrorType
    } deriving (Show, Eq)


data ClientAppErrorType
    = ClientError Text
    deriving (Show, Eq)


toSourcePosition :: CallStack -> SourcePosition
toSourcePosition cs = SourcePosition showCallStack
  where
    showCallStack :: Text
    showCallStack = case getCallStack cs of
        [] -> "<unknown loc>"
        [(name, loc)] -> showLoc name loc
        (_, loc) : (callerName, _) : _ -> showLoc callerName loc

    showLoc :: String -> SrcLoc -> Text
    showLoc name SrcLoc {..} =
        toText srcLocModule <> "." <> toText name <> "#" <> show srcLocStartLine


throwOnNothing :: WithError m => ClientAppErrorType -> Maybe a -> m a
throwOnNothing err = withFrozenCallStack . maybe (throwError err) pure


throwOnNothingM :: WithError m => ClientAppErrorType -> m (Maybe a) -> m a
throwOnNothingM err action =
    withFrozenCallStack $ action >>= throwOnNothing err
