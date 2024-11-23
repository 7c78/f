module PlainText.Model.Path where

import Prelude
import Data.Map as Map
import Data.Map (Map)
import Data.List (List(..), foldl, singleton, snoc, takeWhile, zip)
import Data.Array.Unsafe as Array
import Data.Tuple (fst)
import Data.Tuple.Unsafe ((&))
import Data.FoldableWithIndex (foldrWithIndex)
import PlainText.Model.Node (Node, NodeId, children, nodeId, childAt)

type Path = List Int

rootPath :: Path
rootPath = Nil

indexPaths :: Node -> Map NodeId Path
indexPaths root =
    Map.fromFoldable paths
    where
    paths = childPaths `snoc` (nodeId root & rootPath)
    childPaths = foldrWithIndex (loop rootPath) Nil (children root)
    loop parentPath index node z =
        let nodePath = parentPath `snoc` index
            e = (nodeId node) & nodePath
         in foldrWithIndex (loop nodePath) (z `snoc` e) (children node)

nodeAt :: Node -> Path -> Node
nodeAt =
    foldl step
    where
    step node i =
        children node `Array.index` i

type ResolvedPathEntry =
    { node  :: Node
    , index :: Int
    , level :: Int
    }
type ResolvedPath = List ResolvedPathEntry

resolvePath :: Node -> Path -> ResolvedPath
resolvePath node =
    _.z <<< foldl step {parent: node, level: 1, z} where
    z = singleton {node, level: 0, index: -1}
    step {parent, level, z} index =
        let node = parent `childAt` index
            e = {level, index, node}
         in { parent: node
            , level: level + 1
            , z: z `snoc` e
            }

commonPath :: ResolvedPath -> ResolvedPath -> ResolvedPath
commonPath xs ys =
     map fst
   $ takeWhile (\(x & y) -> x.index == y.index)
   $ zip xs ys
