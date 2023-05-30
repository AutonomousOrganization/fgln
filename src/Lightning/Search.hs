{-# LANGUAGE
    LambdaCase, 
    DuplicateRecordFields, 
    TypeSynonymInstances, 
    FlexibleInstances
#-} 
module Lightning.Search where 

import Data.Graph.Inductive.Graph
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor.Identity
import qualified Data.Sequence as Q
import Data.Sequence (Seq(..),(<|),(|>),(><)) 
import Lightning.Graph 

type Search = Reader (Gra, Node, Node)  -- from / to
type Ref = Q.Seq Int
type Way = Q.Seq Hop

type Partial = (Ref, Node, Ref) 
type Complete = Ref 
type Hydrate = Either Partial (Node, Complete)
type Collect = (Ref, [Complete]) 

search :: StateT Collect Search [Complete] -> Search [Complete] 
search = (`evalStateT` (Empty, []))  


getXresults :: Int -> StateT (Ref, [Complete]) Search [Complete] 
getXresults x = do 
    (r, c) <- get
    r' <- lift $ loop r
    put (increment.chop $ r', r':c)
    if x > length c
        then getXresults x 
        else return (r':c)
 
loop :: Ref -> Search Complete
loop r = (hydrate r) >>= \case
    Left x -> do 
        loop $ nextr r x
    Right h -> do
        (fin h) >>= \case  
            Nothing -> loop $ increment r
            (Just z) -> lift $ pure z   

hydrate :: Ref -> Search Hydrate
hydrate r = do
    (_, n, _) <- ask 
    evalStateT deref (r, n, Empty) 
    where 
    deref :: StateT Partial Search Hydrate
    deref = get >>= \case 
        (Empty, n', c) -> return $ Right (n', c)
        partial@(y :<| t, n', c) -> do 
            oo <- lift $ outgoing n'
            case oo !? y of 
                Nothing     -> pure $ Left partial 
                Just (m, _) -> do 
                    put (t, m, c |> y) 
                    deref

nextr :: Ref -> Partial -> Ref 
nextr r (r', _, c)  
    | z == Q.length r = extend.increment.chop $ r
    | z == 0          = extendTo (Q.length r + 1) Empty
    | otherwise       = extendTo (Q.length r) $ increment $ Q.take z r
    where z = Q.length c

fin :: (Node, Ref) -> Search (Maybe Complete) 
fin (n, r)  = do 
    (g, _, v) <- ask 
    oo <- outgoing n
    let {
        f = dropWhile (not.(== v).fst) oo;
        all = length oo; 
        dropped = length f ;
        finI = all - dropped ;
        }
    case f of 
        [] -> pure Nothing 
        ((_, w'):_) -> pure . Just $ r |> finI 

outgoing :: Node -> Search [(Node, Hop)] 
outgoing m = do 
    (g, _, _) <- ask
    pure $ lsuc g m

increment :: Ref -> Ref
increment Empty = Q.singleton 0
increment (r :|> x) = r |> (x + 1) 
extend :: Ref -> Ref
extend r = r |> 0
chop :: Ref -> Ref
chop Empty = Empty 
chop (r :|> _) = r
extendTo :: Int -> Ref -> Ref 
extendTo x r
    | length r >= x = r 
    | otherwise = extendTo x $ extend r
        

(!?) :: (Foldable t, Eq b, Num b) => t a -> b -> Maybe a
(!?) = foldr voo (const Nothing)
    where 
    voo x r k 
        | k == 0 = Just x
        | otherwise = r $ k-1 

