-- Every Node is either Red or Black (let Nil be black)
data Color = B | R deriving (Show)
data RBTree a = Nil | Node Color a (RBTree a) (RBTree a) deriving (Show)

instance Functor RBTree where
    fmap f Nil = Nil
    fmap f (Node c v l r) = Node c (f v) (fmap f l) (fmap f r)

-- balance' uses the Purely Functional red black tree implementation from Chris Okasaki
balance' :: (Ord a) => RBTree a -> RBTree a
balance' (Node B v (Node R lv (Node R llv lll llr) lr) r) = Node R lv (Node B llv lll llr) (Node B v lr r)
balance' (Node B v (Node R lv ll (Node R lrv lrl lrr)) r) = Node R lrv (Node B lv ll lrl) (Node B v lrr r)
balance' (Node B v l (Node R rv (Node R rlv rll rlr) rr)) = Node R rlv (Node B v l rll) (Node B rv rlr rr)
balance' (Node B v l (Node R rv rl (Node R rrv rrl rrr))) = Node R rv (Node B v l rl) (Node B rrv rrl rrr)


-- insertBalance is more in line with the implementation from CLRS
insertBalance :: (Ord a) => RBTree a -> RBTree a
----- Here is where the red father of a red son is the left of its parent
-- here we switch the colors of all nodes besides the red son and then insertBalance higher up
insertBalance (Node B v (Node R lv ll lr@(Node R _ _ _)) (Node R rv rl rr)) = Node R v (Node B lv ll lr) (Node B rv rl rr)
insertBalance (Node B v (Node R lv ll@(Node R _ _ _) lr) (Node R rv rl rr)) = Node R v (Node B lv ll lr) (Node B rv rl rr)
-- here is the first rotation of a double rotation
insertBalance (Node B v (Node R lv ll (Node R lrv lrl lrr)) r) = insertBalance $ Node B v (Node R lrv (Node R lv ll lrl) lrr) r 
-- here is the first single rotation or second of a double
insertBalance (Node B v (Node R lv (Node R llv lll llr) lr) r) = Node B lv (Node R llv lll llr) (Node R v lr r)
----- Here is where the red father of a red son is the right of its parent
-- here we switch the colors of all nodes besides the red son and then insertBalance higher up
insertBalance (Node B v (Node R lv ll lr) (Node R rv rl@(Node R _ _ _) rr)) = Node R v (Node B lv ll lr) (Node B rv rl rr) 
insertBalance (Node B v (Node R lv ll lr) (Node R rv rl rr@(Node R _ _ _))) = Node R v (Node B lv ll lr) (Node B rv rl rr) 
-- here is the first rotation of a double rotation
insertBalance (Node B v l (Node R rv (Node R rlv rll rlr) rr)) = insertBalance $ Node B v l (Node R rlv rll (Node R rv rlr rr)) 
-- here is the first single rot or second of a double
insertBalance (Node B v l (Node R rv rl (Node R rrv rrl rrr))) = Node B rv (Node R v l rl) (Node R rrv rrl rrr)
-- fallthrough case
insertBalance body = body

-- Change the given Node to a Black node
blacken :: (Ord a) => RBTree a -> RBTree a
blacken (Node R x y z) = Node B x y z
blacken x = x

-- Create a tree of one node
singleton :: (Ord a) => a -> RBTree a
singleton x = Node B x Nil Nil

-- The insert function
---- With the below line in GHC I can then write a type signature for put using a from insert
---- insert :: forall a. (Ord a) => a -> RBTree a -> RBTree a
insert :: (Ord a) => a -> RBTree a -> RBTree a
insert n Nil = singleton n
insert n tree = blacken . put $ tree
        where
            put Nil = Node R n Nil Nil
            put node@(Node c w y z)
                    | n < w = insertBalance (Node c w (put y) z)
                    | n > w = insertBalance (Node c w y (put z))
                    | otherwise = node
