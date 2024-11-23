module PlainText.Model.NodeOperation.Internal where

import Prelude
import Data.List (foldr)
import Data.List.Unsafe (unsnoc)
import Data.Array ((..))
import Data.Foldable.Unsafe (foldl1)
import Data.Tuple.Unsafe ((&))
import Effect.Exception.Unsafe (unsafeThrow)
import PlainText.Model.Node (Node(..), childAt, childrenFrom, childrenUntil, deleteChildAt, insertChildAt, updateChildAt, updateChildren)
import PlainText.Model.Path (Path, ResolvedPath, ResolvedPathEntry, resolvePath)

replace :: ResolvedPath -> ResolvedPathEntry -> Node
replace xs {node, index} =
    _.node $ foldr step {node, index} xs
    where
    step {node, index} {node: childNode, index: childIndex} =
        let node' = updateChildAt childIndex childNode node
         in {node: node', index}

insertNode :: Path -> Node -> Node -> Node
insertNode path node root =
    let (path' & index) = unsnoc path
        (path'' & last) = unsnoc $ resolvePath root path'
        lastNode' = insertChildAt index node last.node
        last' = last {node = lastNode'}
     in replace path'' last'

deleteNode :: Path -> Node -> Node
deleteNode path root =
    let (path' & index) = unsnoc path
        (path'' & last) = unsnoc $ resolvePath root path'
        lastNode' = deleteChildAt index last.node
        last' = last {node = lastNode'}
     in replace path'' last'

joinTexts :: Path -> Int -> Int -> Node -> Node
joinTexts path from to root =
    let (path' & last) = unsnoc $ resolvePath root path
        texts = map (last.node `childAt` _) (from .. to)
        children' =  last.node `childrenUntil` from
                  <> [foldl1 joinText texts]
                  <> last.node `childrenFrom` (to + 1)
        lastNode' = updateChildren children' last.node
        last' = last {node = lastNode'}
     in replace path' last'

joinText :: Node -> Node -> Node
joinText (Text m attrs s) (Text _ _ t) =
    Text m attrs (s <> t)
joinText _ _ =
    unsafeThrow "PlainText.Model.NodeOperation.Internal.joinText: invalid call"
