{-# LANGUAGE 
      DuplicateRecordFields
    , LambdaCase
    , DeriveGeneric
    , OverloadedStrings
    , BangPatterns
    , ViewPatterns
    #-}

module Main where 

import Numeric 
import Data.Char 
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Lightning 
import Control.Client 
import Control.Monad.State
import Control.Monad.Reader 
import Control.Applicative (liftA2) 
import Data.Foldable 

import Data.Graph.Inductive.Graph

import Control.Plugin
import Data.Lightning 
import Control.Client
import Data.Lightning.Generic

import Route 
import Search 
import Graph 

main = plugin manifest start app

manifest = object [
      "dynamic" .= True
    , "options" .= ([] :: [Option])
    , "rpcmethods" .= [
         RpcMethod "route" "n m [a, l]" "l routes from n to m of a" Nothing False 
       , RpcMethod "network" "" "show metrics of loaded graph" Nothing False 
       , RpcMethod "robo" "" "" Nothing False
       ]
    ]

start :: InitMonad Gra
start = createGraph


app (Just i, "robo", _) = do 
    Just (Res x _) <- lightningCli (Command "getinfo" Nothing params)
    respond x i 
    where
    nfilt = object ["version" .= True]
    params = object [] 
        

app (Just i, "candidates", v) = 
    let n = getNodeInt <$> v ^? nth 0 . _String 
    in do 
        g <- get
        release i        

app (Just i, "route", v) =
    let n = getNodeInt <$> v ^? nth 0 . _String
        m = getNodeInt <$> v ^? nth 1 . _String 
        a = maybe 100000000 fromInteger $ v ^? nth 2 . _Integer
        x = maybe 1 fromInteger $ v ^? nth 3 . _Integer
    in do
        g <- get
        case valid g n m of
            Just (n', m') -> do 
                r <- pure $ evalBy g n' m' $ getXresults x
                respond (object ["routes" .= r ]) i 
            _ -> respond (object ["invalid nodid" .= True]) i 


app (Just i, "network", _) = do 
    g <- get
    respond (object [
          "nodes" .= order g
        , "edges" .= size g
        , "capacity" .= ("todo" :: Text ) -- (evalState total (g, 0))
        ]) i 
        where
        total :: State (Gra, Msat) Msat
        total = undefined 

app (Just i, _, _) = release i
app _ = pure () 

valid g n m = case (n , m) of 
    (Just n', Just m') -> if gelem n' g && gelem m' g
                          then (Just (n', m'))
                          else Nothing   
    _ -> Nothing




