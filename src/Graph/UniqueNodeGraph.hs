{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Graph.UniqueNodeGraph where

import Data.Functor
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Hashable (Hashable, hash, hashWithSalt)
import qualified Data.HashMap.Strict as H
import Data.Monoid ((<>))
import qualified Data.Text as T

import Debug.Trace

-- | Constraints a Node in this type of graph must have.
type NodeConstraints n = (Eq n, Hashable n, Show n)

-- | Data to be stored in nodes.
data NodeData = NodeData
  { nodeProps :: [(T.Text, T.Text)]
  , nodeLabels:: [T.Text]
  } deriving (Eq, Show)

instance Hashable NodeData where
  hashWithSalt s (NodeData ps ls) = s + hash ps + hash ls


-- | Data to be stored in relations.
data RelationData = RelationData
  { relationProps :: [(T.Text, T.Text)]
  , relationLabels:: [T.Text]
  } deriving (Eq, Show)


data UniqueNodeGraph n r = UniqueNodeGraph
  { nodeMap :: H.HashMap n Int
  , unGraph :: Gr n r
  } deriving (Eq, Show)


-- | Construct an empty unique node graph.
--
emptyGraph :: UniqueNodeGraph NodeData RelationData
emptyGraph = UniqueNodeGraph H.empty empty


-- | Construct an edge between the node at 'n1' and the node at 'n2'. The 'relationData'
-- tracks metadata about the relation - labels, properties, direction etc...
--
consEdge :: Int
         -> Int
         -> RelationData
         -> UniqueNodeGraph NodeData RelationData
         -> UniqueNodeGraph NodeData RelationData
consEdge n1 n2 relationData (UniqueNodeGraph m g) =
    UniqueNodeGraph m (insEdge (n1, n2, relationData) g)


-- | Construct a new node with data 'n' in the graph. Returns the graph and the int id
-- of the new node.
--
consNode :: NodeConstraints NodeData
         => NodeData
         -> UniqueNodeGraph NodeData RelationData
         -> (Int, UniqueNodeGraph NodeData RelationData)
consNode ndata (UniqueNodeGraph m g) = create ndata $ H.lookup ndata m
  where
    -- | Create a node in the graph if it does not already exist.
    create ndata (Just nid) = (nid, UniqueNodeGraph m (insNode (nid, ndata) g))
    create ndata Nothing =
        let [newNid] = newNodes 1 g
            m'       = H.insert ndata newNid m
        in  (newNid, UniqueNodeGraph m' (insNode (newNid, ndata) g))

extract :: UniqueNodeGraph NodeData RelationData -> Gr NodeData RelationData
extract = unGraph


-- | Convenience method to pretty print the graph.
--
pretty :: UniqueNodeGraph NodeData RelationData -> T.Text
pretty g = T.pack . prettify $ extract g
