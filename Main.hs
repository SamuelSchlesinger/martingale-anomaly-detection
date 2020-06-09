{-# LANGUAGE TypeApplications #-}
module Main where

import Anomaly (rand, runAnomalyTWith, martingale, observe)
import Text.Read (readMaybe)
import Control.Monad.IO.Class
import Control.Monad.State

main :: IO ()
main = do
  ns <- sequence [(100 *) <$> rand | i <- [1..1000]] 
  (liftIO . print) =<< runAnomalyTWith ns 5 0.92 go
  void $ getChar
  main
  where
    go = do
      let logMartingale = get >>= liftIO . print . martingale
      sequence_ [rand >>= observe >> logMartingale | i <- [1..100]]
      liftIO $ putStrLn "--------------------------------------------------------------"
      sequence_ [(((1000 +) . (100 *)) <$> rand) >>= observe >> logMartingale | i <- [1..100]]
      liftIO $ putStrLn "--------------------------------------------------------------"
      sequence_ [((100 *) <$> rand) >>= observe >> logMartingale | i <- [1..100]]
