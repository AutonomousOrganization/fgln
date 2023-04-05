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
import qualified Data.Sequence as Q
import Data.Sequence (Seq(..),(<|),(|>),(><)) 
import Lightning.Graph 

type Search = Reader (Gra, Node, Node)  -- from / to
type Ref = Q.Seq Int
type Way = Q.Seq Hop

type Partial = (Ref, Way, Node, Ref) 
type Complete = (Way, Node, Ref) 
type Hydrate = Either Partial Complete

evalBy g n m s = (`runReader` (g, n, m)) .  (`evalStateT` (Empty, [])) $ s 

getXresults :: Int -> StateT (Ref, [Complete]) Search [Complete] 
getXresults x = do 
    (r, c) <- get
    c'@(_, _, r') <- lift $ search r
    put (increment.chop $ r', c':c)
    if x > length c
        then getXresults x 
        else return (c':c)
 
search :: Ref -> Search Complete
search r = (hydrate r) >>= \case
    Left x -> do 
        search $ nextr r x
    Right h -> do
        (fin h) >>= \case  
            Nothing -> search $ increment r
            (Just z) -> lift $ pure z   

hydrate :: Ref -> Search Hydrate
hydrate r = do
    (_, n, _) <- ask 
    evalStateT deref (r, Empty, n, Empty) 
    where 
    deref :: StateT Partial Search Hydrate
    deref = get >>= \case 
        (Empty, w, n', c) -> return $ Right (w, n', c)
        partial@(y :<| t, w, n', c) -> do 
            oo <- lift $ outgoing n'
            case oo !? y of 
                Nothing     -> pure $ Left partial 
                Just (m, w') -> do 
                    put (t, w |> w', m, c |> y) 
                    deref

nextr :: Ref -> Partial -> Ref 
nextr r (r', _, _, c)  
    | z == Q.length r = extend.increment.chop $ r
    | z == 0          = extendTo (Q.length r + 1) Empty
    | otherwise       = extendTo (Q.length r) $ increment $ Q.take z r
    where z = Q.length c

fin :: Complete -> Search (Maybe Complete) 
fin (w, n, r)  = do 
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
        ((_, w'):_) -> pure $ Just (w |> w', v, r |> finI) 

outgoing :: Node -> Search [(Node, Hop)] 
outgoing m = do 
    (g, n, _) <- ask
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

