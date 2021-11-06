{-# LANGUAGE LambdaCase #-}
module Main where

import Lib
import UnliftIO
import UnliftIO.IORef

main :: IO ()
main = do
  xr <- newIORef 0
  label "B" $ do
    liftIO $ putStrLn "B body"
    label "A" $ do
      liftIO $ putStrLn "A body"
      label "C" $ do
        readIORef xr >>= \case
         10 -> liftIO $ putStrLn "END"
         x -> do
           liftIO (putStrLn $ " x = " ++ show x)
           modifyIORef' xr (+1)
           if x == 5
             then goto "B"
             else goto "A"
