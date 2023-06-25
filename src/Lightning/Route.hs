{-# LANGUAGE 
      DuplicateRecordFields
    , OverloadedStrings
    , DeriveGeneric 
    , ViewPatterns
    , LambdaCase
    #-} 

module Lightning.Route where 

import Lightning.Util
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
    , __direction :: Int
    , __amount_msat :: Msat 
    , __delay :: Int 
    , style :: Text
    } deriving (Show, Generic, Eq) 
instance ToJSON Route where
    toJSON v = genericToJSON defaultOptions{fieldLabelModifier = dropWhile (=='_')} v 

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
