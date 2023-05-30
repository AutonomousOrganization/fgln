{-# LANGUAGE 
      DuplicateRecordFields
    , LambdaCase
    , DeriveGeneric
    , OverloadedStrings
    , BangPatterns
    , ViewPatterns
    , ScopedTypeVariables
    #-}

module Main where 

import Numeric 
import Data.Char 
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types
import Data.Aeson.Text 
import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Lightning 
import Control.Client 
import Control.Monad.State
import Control.Monad.Reader 
import Control.Applicative (liftA2) 
import Data.Foldable 
import qualified Data.Map as M
import Data.Text.Format.Numbers
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.Dominators (dom,iDom)
import qualified Data.Graph.Inductive.Query.BFS as B
import Control.Plugin
import Data.Lightning 
import Control.Client
import Data.Lightning.Generic
import Lightning.Route 
import Lightning.Search 
import Lightning.Graph 
import Lightning.Candidates
import Lightning.Fees
import Fmt

-- prin m = appendFile "/home/o/.ao/storm" $ m <> " \n" 

main = plugin manifest start app

manifest = object [
      "dynamic" .= True
    , "options" .= ([] :: [Option])
    , "rpcmethods" .= [
         RpcMethod "route" "n m [a, l]" "l routes from n to m of a" Nothing False 
       , RpcMethod "network" "" "show metrics of loaded graph" Nothing False 
       , RpcMethod "candidates" "n [l]" "" Nothing False
       , RpcMethod "deploy" "x" "" Nothing False 
       ]
    -- , "subscriptions" .= (["forward_event"] :: [Text] ) 
    ]

start :: InitMonad Gra
start = createGraph

--app (Nothing, "forward_event", 
   -- fromJSON -> Success fe@(ForwardEvent{status = "settled", ..})) = do
       -- re <- setNewFee fe

app (Just i, "candidates", v) = do 
    nodeid <- getNodeId
    let n = getNodeInt $ maybe nodeid id $ v ^? nth 0 . _String 
    let x = maybe 100000000 fromInteger $ v ^? nth 1 . _Integer
    g <- get
    respond (toJSON $ evalState collectD (suggest g n, x, [])) i 

app (Just i, "deploy", v) = 
    let x = maybe 0 fromInteger $ v ^? nth 0 . _Integer
    in do 
        g <- get
        nodeid <- getNodeId
        let lazyD = suggest g (getNodeInt nodeid)    
        let dest = evalState collectD (lazyD, x, []) 
        Just (Res v _) <- lightningCliDebug prin $ Command "multifundchannel" Nothing (object [
                  "destinations" .= dest
                , "feerate" .= ("slow" :: Text)
                , "minchannels" .= (3 * div (length dest) 4)
                ])
        respond v i 

app (Just i, "route", v) =
    let n = getNodeInt <$> v ^? nth 0 . _String
        m = getNodeInt <$> v ^? nth 1 . _String 
        a = maybe 100000000 fromInteger $ v ^? nth 2 . _Integer
        x = maybe 1 fromInteger $ v ^? nth 3 . _Integer
    in do
        g <- get
        case valid g n m of
            Just (n', m') -> do 
                r <- pure . (`runReader` (g, n', m')) $ do 
                    xrefs <- search $ getXresults x
                    ihops <- mapM getHops xrefs
                    mapM (getRoute 1000000) ihops
                respond (object ["routes" .= r ]) i 
            _ -> respond (object ["invalid nodid" .= True]) i 

app (Just i, "network", _) = do 
    g <- get
    Just (Res (Object ( (parse (.: "id")) -> ((Success nodeid)::Result Text) )) _) <- lightningCli $ Command "getinfo" Nothing (object []) 
    respond (object [
          "nodes" .= order g
        , "edges" .= size g
        , "y-levels" .= (M.map lvlPrint $ foldr (countl g) M.empty $ B.level (getNodeInt nodeid) g)
        , "sats" .= (prettyI (Just ',') $ (`div` 1000) $ ufold total 0 g)
        ]) i 
        where countl :: Gra -> (Node, Int) -> M.Map Int (Int,[Fee],[Fee]) -> M.Map Int (Int, [Fee], [Fee])  
              countl g (n, i) m = case M.lookup i m of
                Just t -> M.adjust (addNode g n) i m 
                Nothing -> M.insert i (1, [inFee g n], [outFee g n]) m 
              total ctx t = t + (sum . map (msats.snd) . lsuc' $ ctx) 
              addNode g' n' (i', f', f'') = (i'+1, inFee g' n' : f', outFee g' n' : f'')
app (Just i, _, _) = release i
app _ = pure () 

lvlPrint :: (Int, [Fee], [Fee]) -> Value
lvlPrint (i, f', f'') = object ["nodes".=i, "inFee".= (avgFee id f'), "outFee".= (avgFee id f'')]

valid g n m = case (n , m) of 
    (Just n', Just m') -> if gelem n' g && gelem m' g
                          then (Just (n', m'))
                          else Nothing   
    _ -> Nothing


getNodeId = do 
    Just (Res (Object ((parse (.: "id")) -> ((Success nodeid)::Result Text) )) _) <- 
            lightningCli $ Command "getinfo" Nothing (object []) 
    pure nodeid


