module Lib.Server.Error
    ( ServerAppError(..)
    , ServerAppErrorType(..)
    , ServerAppException(..)
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
                                                )
import           Control.Monad.Except           ( MonadError )
import           GHC.Stack                      ( SrcLoc
                                                    ( SrcLoc
                                                    , srcLocModule
                                                    , srcLocStartLine
                                                    )
                                                )

type WithError m = (MonadError ServerAppError m, HasCallStack)


throwError :: WithError m => ServerAppErrorType -> m a
throwError = E.throwError . ServerAppError (toSourcePosition callStack)


newtype SourcePosition = SourcePosition
    { unSourcePosition :: Text
    } deriving newtype (Show, Eq)


newtype ServerAppException = ServerAppException
    { unServerAppException :: ServerAppError
    } deriving (Show)
      deriving anyclass (Exception)


data ServerAppError = ServerAppError
    { serverAppErrorCallStack :: !SourcePosition
    , serverAppErrorType      :: !ServerAppErrorType
    } deriving (Show, Eq)


data ServerAppErrorType
    = ServerError Text
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


throwOnNothing :: WithError m => ServerAppErrorType -> Maybe a -> m a
throwOnNothing err = withFrozenCallStack . maybe (throwError err) pure


throwOnNothingM :: WithError m => ServerAppErrorType -> m (Maybe a) -> m a
throwOnNothingM err action =
    withFrozenCallStack $ action >>= throwOnNothing err
