module Lib.App.Error
    ( AppError(..)
    , AppErrorType(..)
    , AppException(..)
    , WithError
    , throwError
    , throwOnNothing
    , throwOnNothingM
    , SourcePosition(..)
    , toSourcePosition
    )
where

import qualified Control.Monad.Except          as E
                                                ( throwError )
import           Control.Monad.Except           ( MonadError )
import           GHC.Stack                      ( SrcLoc
                                                    ( SrcLoc
                                                    , srcLocModule
                                                    , srcLocStartLine
                                                    )
                                                )

type WithError m = (MonadError AppError m, HasCallStack)


throwError :: WithError m => AppErrorType -> m a
throwError = E.throwError . AppError (toSourcePosition callStack)


newtype SourcePosition = SourcePosition
    { unSourcePosition :: Text
    } deriving newtype (Show, Eq)


newtype AppException = AppException
    { unAppException :: AppError
    } deriving (Show)
      deriving anyclass (Exception)


data AppError = AppError
    { appErrorCallStack :: !SourcePosition
    , appErrorType      :: !AppErrorType
    } deriving (Show, Eq)


data AppErrorType
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


throwOnNothing :: WithError m => AppErrorType -> Maybe a -> m a
throwOnNothing err = withFrozenCallStack . maybe (throwError err) pure


throwOnNothingM :: WithError m => AppErrorType -> m (Maybe a) -> m a
throwOnNothingM err action =
    withFrozenCallStack $ action >>= throwOnNothing err
