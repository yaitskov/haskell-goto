{-# LANGUAGE LambdaCase #-}
module Lib (GotoEx, goto, label) where
import Control.Monad.IO.Class
import Control.Monad.Catch

newtype GotoEx = GotoEx String deriving (Show, Eq)

instance Exception GotoEx

label :: (MonadCatch m, MonadThrow m, MonadIO m) => String -> m a -> m a
label labelName action = do
  try action >>= \case
    Left e@(GotoEx lbl) ->
      if lbl == labelName
        then label labelName action
        else throwM e
    Right v -> pure v

goto :: (MonadThrow m, MonadIO m) => String -> m a
goto label = throwM $ GotoEx label
