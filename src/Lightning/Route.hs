{-# LANGUAGE 
      DuplicateRecordFields
    , OverloadedStrings
    , DeriveGeneric 
    , ViewPatterns
    , LambdaCase
    #-} 

    -- , NamedFieldPuns 
    -- , RecordWildCards
module Lightning.Route where 

import Data.Lightning 
import Data.Graph.Inductive.Graph
import Data.Functor.Identity
import Numeric (readHex) 
import Data.Aeson
import Data.Foldable
import Data.Maybe
import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as T  
import Control.Applicative 
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Sequence as Q
import Data.Sequence (Seq(..),(<|),(|>),(><)) 
import Lightning.Search 
import Lightning.Graph
import Control.Exception

data Route = Route {
      __id :: Text 
    , channel :: Text 
    , direction :: Int
    , __amount_msat :: Msat 
    , __delay :: Int 
    , style :: Text
    } deriving (Show, Generic, Eq) 
instance ToJSON Route where
    toJSON v = genericToJSON defaultOptions{fieldLabelModifier = dropWhile (=='_')} v 

--type Way = [Route]

--type Pad = (Node, Ref, Hops) 

--type Seal = StateT Pad Sea 

--type Sea = ReaderT Gra Maybe

-- type Hops = Q.Seq Hop
-- type Hop = (Text, LNode DirectedEdge)

prin m = appendFile "/home/o/.ao/storm" $ show m <> "\n"

type HopsNodes = (Way, Q.Seq Node)

hdir :: Hop -> Int
hdir = direction
hdel :: Hop -> Int
hdel = delay

getRoute :: Msat -> HopsNodes -> Search (Q.Seq Route)
getRoute a (hs@(hx :|> h), nx :|> n) = do 
    n' <- getId n 
    let { final = Route
        n'
        (short h)
        (hdir h)
        a
        9
        "tlv"
        }
    evalStateT rou (hs, nx, Q.singleton final)
    where 
    rou :: StateT (Way, Q.Seq Node, Q.Seq Route) Search (Q.Seq Route)
    rou = get >>= \case 
        (_, Empty, r) -> pure r 
        (hs@(hx :|> h') :|> h, nx :|> n, rs@(r :<| _) ) -> do
            n' <- lift$getId n
            let nexr = Route n' 
                             (short h')
                             (hdir h')
                             (calcAmt h r) 
                             (calcDly h r)
                             "tlv"
            put (hs, nx, nexr <| rs)
            rou 

calcAmt h r = 
    let a = __amount_msat r
        baseFee = base.fees $ h
        ppmFee =  fromInteger $ div num (mil*mil)
        mil = 1000000 :: Integer 
        num = mil * toInteger a * toInteger (ppm.fees $ h)
    in a + baseFee + ppmFee
    
calcDly h r = __delay r + hdel h 

getId :: Node -> Search Text
getId n = do 
    (g,_,_) <- ask
    case lab g n of 
        Just (Vertex a) -> pure a
        Nothing         -> pure ("BAD"::Text) 
      

getHops :: Ref -> Search HopsNodes
getHops Empty = pure (Empty, Empty)
getHops r = do 
    (g,n,v) <- ask
    evalStateT bunny (n, r, (Empty, Empty))
    where
    bunny :: StateT (Node, Ref, HopsNodes) Search HopsNodes
    bunny = get >>= \case 
        (_, Empty, ww) -> pure ww
        (n, i :<| r, (w,e)) -> do 
            mayH <- lift$ getHop n i
            case mayH of 
                Just (n', h) -> put (n', r
                                          , (w |> h
                                          , e |> n')
                                          ) >> bunny 
                _ -> error "invalid REF"
            
getHop :: Node -> Int -> Search (Maybe (Node, Hop))
getHop n i = do
      oo <- outgoing n 
      pure $ oo !? i 
-- g n a r = do
--     h <- runReaderT (evalStateT cha (n, r, Empty)) g
--     -- liftIO $ prin h 
--     cho h
-- 
-- -- cha :: Seal ( Hops , Text )
-- cha = get >>= \case 
--     (n, Empty, c) -> do 
--         l' <- lift $ nodeLab n
--         pure $ (c , l')
--     
--     (n, x :<| r, c) -> do
--         hop <- lift $ match' n x
--         put (getNodeInt $ fst hop, r , c |> hop) 
--         cha 
--             
-- -- match' :: Node -> Int -> Sea Hop 
-- match' n i = do 
--     g <- ask 
--     oo <- pure $ lsuc g n 
--     l <- nodeLab n
--     d <- pure (oo !? i) 
--     return $ (l, fromJust d)  
-- 
-- -- nodeLab :: Node -> Sea Text
-- nodeLab n' = do
--     g <- ask
--     Just (Vertex l) <- pure $ lab g n'  
--     pure l
-- 
-- -- cho :: (Hops, Text) -> Maybe Way
-- cho (hops, lastNode) = evalStateT runner (hops, lastNode, Empty) 
--     where 
--     -- runner :: StateT (Hops, Text, Q.Seq Route) IO Way
--     runner = get >>= \case 
--         (Empty, _, c) -> pure $ toList c 
--         ( x :|> (l', (_, edge) ), l, c') -> do 
--             liftIO $ prin l'
--             liftIO $ prin $ ___short_channel_id edge
--             put (x, l', 
--                 (Route l (___short_channel_id edge) calcDir calcAmt calcDel "tlv") <| c')
--             runner
--             where 
--             calcDir = if readHex (show l) < readHex (show l') then 0 else 1
--             calcAmt = 0
--             calcDel = 0
--             nextLab = undefined 
--     
-- --        addHop a (cp, c)  r = Route
-- --            (cDest cp)
-- --            (shortid cp)
-- --            (liftA2 getDirect cSource cDest cp)
-- --            (getAmount a c r)
-- --            (getDelay c r)
-- --            "tlv"
-- --            : r
-- --
-- --        -- getDirect :: String -> String -> Int
-- --        getDirect a b = if readHex (show a) < readHex (show b) then 0 else 1
-- --
-- --        -- getDelay :: Channel -> [Route] -> Int
-- --        getDelay e [] = 9
-- --        getDelay e (r:_) = __delay r + cDelay e
-- --        
-- --        getAmount :: (Channel c) => Msat -> c -> [Route] -> Msat
-- --        getAmount a e [] = a
-- --        getAmount a e r =
-- --            let mil = 1000000 :: Integer
-- --                b = basefee e
-- --                p = ppmrate e
-- --                num = (mil * toInteger nextAmount * toInteger p)
-- --                denum = mil*mil
-- --                ppmfee  = fromInteger $ div num denum -- inaccurate?>
-- --                nextAmount = maximum $ map __amount_msat r
-- --            in sum [ nextAmount, b, ppmfee ]
-- --
-- 
-- 
-- 
-- 
-- --getRoute = get >>= \case 
-- --    ([], n) -> 
-- --    (r, n)  -> do 
--         
-- 
-- 
-- 
-- 
-- 
--  -- :: Ref   -- -> [ (Text, Text, DirectedEdge) ]
-- 
-- -- getChannels r n = go r n []
-- --     where
-- --     go Empty _ c = c
-- --     go (i :|> r') n' c = 
-- --         let 
-- --             node1 = lab g n'
-- --             node2 = lab g n''
-- --             (n'', hop) = (lsuc g n') !? i
-- --             hop   = 
-- --         in (node1, node2, hop) : c 
-- --         
-- -- 
-- 
-- 
-- 
-- -- createRoute :: Gra -> Msat -> Node -> Ref -> [Route]
-- -- createRoute g a n (ri <|: rx) = 
-- 
-- ---- | Route, current node, current amount, current delay
-- --type RouteBuilder = ([Route], Node, Msat, Int)
-- --
-- --buildRoute :: StateT RouteBuilder (Reader Gra) [Route]
-- --buildRoute = do 
-- --    (r, n, a, d) <- get
-- --    g <- lift ask
-- --    case match g n of 
-- --       -- (Just (_, _, Vertex nodeid, ( (!? i) -> Just (DirectedEdge{}) ), g') -> 
-- --        
-- --evalStateT (runReader buildRoute (g, a)) (r, n, [])   
-- --    where 
-- --        buildRoute :: StateT (Ref, Node, [Route]) (Reader (Gra, Msat)) [Route] 
-- --        buildRoute = do 
-- --            (gra, amt) <-ask
-- --            (r, n, c) <- get
-- --            return c 
-- --
-- ----createRoute a c = foldr (addHop a) [] $ pairUp c
-- --    where
-- --        pairUp :: [c] -> [(c,c)]
-- --        pairUp [] = []
-- --        pairUp (a:[]) = [(a,a)]
-- --        pairUp (a:b) = (a,head b) : pairUp b
-- --
-- --        -- addHop :: Msat -> (Channel, Channel) -> [Route] -> [Route]
