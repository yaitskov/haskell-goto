{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE XDataKinds #-}

module Lib (GotoEx, goto, label) where
import Data.Typeable
import Control.Monad.IO.Class
import Control.Monad.Catch

-- newtype Label l = Label  String deriving (Show, Eq)

newtype GotoEx l = GotoEx l deriving (Show, Eq, Typeable)

-- instance (Show t, Eq t) => Typeable (GotoEx t)
instance (Show t, Eq t, Typeable t) => Exception (GotoEx t)

label :: (MonadCatch m, MonadThrow m, MonadIO m, Show l, Eq l, Typeable l) => l -> m a -> m a
label labelName action = do
  try action >>= \case
    Left e@(GotoEx (lbl :: l)) ->
      if lbl == labelName
        then label labelName action
        else throwM e
    Right v -> pure v

goto :: (MonadThrow m, MonadIO m, Show t, Eq t, Typeable t) => t -> m a
goto label = throwM $ GotoEx label
