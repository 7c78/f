module PlainText.View.Diff where

import Prelude
import Data.Array (snoc, reverse, foldl)
import Data.Array.Unsafe ((!), scanl, scanlWithIndex, last)
import Data.Foldable.Unsafe (minimumOn)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), snd)
import PlainText.Model.Node (Node, NodeId, nodeId)

data EditOperation
    = Retain NodeId
    | Update NodeId Node
    | Replace NodeId Node
    | Insert Node
    | Delete NodeId

derive instance Eq EditOperation

instance Show EditOperation where
    show (Retain n)    = "Retain " <> show n
    show (Insert n)    = "Insert " <> show (nodeId n)
    show (Update id _) = "Update " <> show id
    show (Delete n)    = "Delete " <> show n
    show (Replace n m) = "Replace " <> show n <> " " <> show (nodeId m)

diff :: Array NodeId -> Array Node -> Set NodeId -> Array EditOperation
diff oldNodes newNodes dirtyNodes =
    let oldNodes' = reverse oldNodes
        newNodes' = reverse newNodes
        entries = foldl (nextRow oldNodes' dirtyNodes) (row0 oldNodes') newNodes'
        Tuple ops _ = last entries
     in reverse ops

type Cost = Int
type Entry = Tuple (Array EditOperation) Cost

row0 :: Array NodeId -> Array Entry
row0 =
    scanl nextEntry entry0
  where
    entry0 = Tuple [] 0
    nextEntry (Tuple ops cost) nodeId =
        Tuple (ops `snoc` Delete nodeId) (cost + 1)

nextRow :: Array NodeId -> Set NodeId -> Array Entry -> Node -> Array Entry
nextRow oldNodes dirtyNodes prevRow node =
    scanlWithIndex nextEntry entry0 oldNodes
  where
    Tuple ops0 cost0 = prevRow ! 0
    entry0 = Tuple (ops0 `snoc` Insert node) (cost0 + 1)

    nextEntry i _iEntry oldNodeId | nodeId node == oldNodeId =
        let Tuple prevIEditOperations prevICost = prevRow ! i
            op = if Set.member oldNodeId dirtyNodes
                    then Update oldNodeId node
                    else Retain oldNodeId
         in Tuple (prevIEditOperations `snoc` op) prevICost
    nextEntry i (Tuple iEditOperations iCost) oldNodeId =
        let Tuple prevJEditOperations prevJCost = prevRow ! (i + 1)
            Tuple prevIEditOperations prevICost = prevRow ! i
         in minimumOn snd
                [ Tuple (iEditOperations `snoc` Delete oldNodeId) (iCost + 1)
                , Tuple (prevIEditOperations `snoc` Replace oldNodeId node) (prevICost + 1)
                , Tuple (prevJEditOperations `snoc` Insert node) (prevJCost + 1)
                ]
