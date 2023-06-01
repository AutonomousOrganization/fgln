{-# LANGUAGE 
      DuplicateRecordFields
    , LambdaCase
    , DeriveGeneric
    , OverloadedStrings
    , BangPatterns
    , ViewPatterns
    , ScopedTypeVariables
    , FlexibleContexts
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
import qualified Data.Text as T
import Data.Lightning 
import Control.Client 
import Control.Monad.State
import GHC.Generics
import Control.Monad.Reader 
import Control.Applicative (liftA2) 
import Data.Foldable 
import qualified Data.Map as M
import Data.Text.Format.Numbers
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.Dominators (dom,iDom)
import Data.Sequence (Seq(..),(<|),(|>),(><)) 
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

--prin m = appendFile "/home/o/.ao/storm" $ m <> " \n" 
-- lcli = lightningCliDebug prin

main = plugin manifest start app

manifest = object [
      "dynamic" .= True
    , "options" .= ([] :: [Option])
    , "rpcmethods" .= [
         RpcMethod "route" "n m [a, l]" "l routes from n to m of a" Nothing False 
       , RpcMethod "network" "" "show metrics of loaded graph" Nothing False 
       , RpcMethod "candidates" "n [l]" "" Nothing False
       , RpcMethod "deploy" "x" "" Nothing False 
       , RpcMethod "payer" "x" "" Nothing False 
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
                    mapM (getRoute a) ihops
                respond (object ["routes" .= r ]) i 
            _ -> respond (object ["invalid nodid" .= True]) i 


app (Just i, "payer", v) = 
    let bolt = maybe "" id $ v ^? nth 0 . _String
    in do 
        Just (Res (fromJSON -> Success (Decode d a h)) _)
            <- lightningCli $ Command "decodepay" decodeFilter (object ["bolt11".=bolt])
        n <- getNodeId
        g <- get
        rx <- pure . (`runReader` (g, getNodeInt n, getNodeInt d)) $ do 
                    xrefs <- search $ getXresults 111
                    ihops <- mapM getHops xrefs
                    rou <- mapM (getRoute a) ihops
                    pure . filter (isCheap a) $ rou
        r2 <- sendPays h rx []
        respond (object ["resu" .= r2 ]) i 

app (Just i, "network", _) = do 
    g <- get
    nodeid <- getNodeId
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


isCheap a (r :<| _) = 
    let cost = __amount_msat r - a 
    in cost < 1000000 

-- sendPays :: _ 
sendPays _ [] re = pure re
sendPays h (r:rx) re = do 
    liftIO $ prin "sendPays go"
    lightningCliDebug prin $ Command "sendpay" Nothing (object [
              "route" .= r
            , "payment_hash" .= h
            ])
    Just wsp <- lightningCliDebug prin $ Command "waitsendpay" Nothing (object [
          "payment_hash" .= h
        , "timeout" .= (11 :: Int) 
        ])
    case wsp of 
        Res v _ -> pure (v:re)
        ErrRes g@(T.take 4 -> "fail") _ -> do 
            liftIO $ prin "view pattern success, should try next?"
            sendPays h rx ((object ["err".=g]):re)
        o -> liftIO (prin "did not view pattern") >> pure ((object ["o" .= o]):re) 
        

getNodeId = do 
    Just (Res (Object ((parse (.: "id")) -> ((Success nodeid)::Result Text) )) _) <- 
            lightningCli $ Command "getinfo" Nothing (object []) 
    pure nodeid


decodeFilter = Just $ object [
      "payee".=True
    , "amount_msat".=True
    , "payment_hash".=True
    ]
data Decode = Decode {
      payee :: Text
    , amount_msat :: Msat
    , payment_hash :: Text
    } deriving (Generic)
instance FromJSON Decode
