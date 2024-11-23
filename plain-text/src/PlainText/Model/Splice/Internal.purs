module PlainText.Model.Splice.Internal where

import Prelude hiding (join)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.List (List(..), (:), foldl, foldr, length, singleton, snoc)
import Data.List.Unsafe (head, last, scanr, splitAt, unsnoc)
import Data.Array (cons, snoc) as Array
import Data.Array.Unsafe (head, tail, init, last) as Array
import Data.Tuple.Unsafe (type (&), (&))
import Effect.Exception.Unsafe (unsafeThrow)
import PlainText.Model.Selection (TextOffset)
import PlainText.Model.Path (Path, ResolvedPath, ResolvedPathEntry, resolvePath, commonPath)
import PlainText.Model.Node (Node, children, childrenFrom, childrenUntil, isText, nodeId, text, textFrom, textUntil, updateChildAt, updateChildren, updateNodeId, updateText)
import PlainText.Model.Structure (isInline, isBlock, joinable)

cutFrom :: TextOffset -> ResolvedPath -> ResolvedPath
cutFrom _ Nil =
    Nil
cutFrom offset (x:Nil) =
    let t = x.node `textFrom` offset
        xnode = updateText t x.node
        x' = x{node = xnode}
     in singleton x'
cutFrom offset (x:xs) =
    let xs' = cutFrom offset xs
        child = head xs'
        after = x.node `childrenFrom` (child.index + 1)
        xnode = updateChildren (child.node `Array.cons` after) x.node
        x' = x{node = xnode}
     in x' : xs'

cutUntil :: TextOffset -> ResolvedPath -> ResolvedPath
cutUntil _ Nil =
    Nil
cutUntil offset (x:Nil) =
    let t = x.node `textUntil` offset
        xnode = updateText t x.node
        x' = x{node = xnode}
     in singleton x'
cutUntil offset (x:xs) =
    let xs' = cutUntil offset xs
        child = head xs'
        before = x.node `childrenUntil` child.index
        xnode = updateChildren (before `Array.snoc` child.node) x.node
        x' = x{node = xnode}
     in x' : xs'

joinNodes :: Node -> Node -> Array Node
joinNodes x y | joinable x y =
    if isText x then
        let text' = text x <> text y
            x' = updateText text' x
         in [x']
    else
        let xchildren = children x
            ychildren = children y
            children' =
                case xchildren, ychildren of
                    [], _ -> ychildren
                    _, [] -> xchildren
                    _, _  -> Array.init xchildren
                          <> joinNodes (Array.last xchildren) (Array.head ychildren)
                          <> Array.tail ychildren
            x' = updateChildren children' x
         in [x']
joinNodes x y =
    [x, y]

join :: ResolvedPath -> ResolvedPath -> Array Node
join (x:Nil) (y:Nil) | x.index == y.index =
    let text' = text x.node <> text y.node
        node' = updateText text' x.node
     in [node']
join (x:xs) (y:ys) | x.index == y.index =
    let children' =  Array.init (children x.node)
                  <> join xs ys
                  <> Array.tail (children y.node)
        node' = updateChildren children' x.node
     in [node']
join (x:_) (y:_) =
    joinNodes x.node y.node
join _ _ =
    unsafeThrow "PlainText.Model.Splice.join: invalid call"

joinUntil :: Int -> ResolvedPath -> ResolvedPath -> Array Node
joinUntil level (x:xs) (y:ys) | x.index == y.index && x.level < level =
    let children' =  Array.init (children x.node)
                  <> joinUntil level xs ys
                  <> Array.tail (children y.node)
        node' = updateChildren children' x.node
     in [node']
joinUntil _ (x:_) (y:_) =
    joinNodes x.node y.node
joinUntil _ _ _ =
    unsafeThrow "PlainText.Model.Splice.joinUntil: invalid call"

replace :: ResolvedPath -> ResolvedPathEntry -> ResolvedPath
replace xs z =
    scanr step z xs
    where
    step {node, index, level} {node: childNode, index: childIndex} =
        let node' = updateChildAt childIndex childNode node
         in {node: node', index, level}

appendText :: String -> ResolvedPath -> ResolvedPath
appendText "" path  = path
appendText ins path =
    let (path' & last) = unsnoc path
        text' = text last.node <> ins
        lastNode' = updateText text' last.node
        last' = last {node = lastNode'}
     in replace path' last'

appendNode :: Int -> Node -> ResolvedPath -> ResolvedPath
appendNode level ins path =
    let (before & after) = splitAt level path
        (before' & parent) = unsnoc before
        children' = children parent.node `Array.snoc` ins
        parentNode' = updateChildren children' parent.node
        parent' = parent {node = parentNode'}
        last = head after
        last' = last {node = ins, index = last.index + 1}
     in replace before' parent' `snoc` last'

spliceText :: (Path & TextOffset) -> (Path & TextOffset) -> String -> Node -> Node
spliceText (Nil & _) (Nil & _) _ root =
    root
spliceText (xs & xoffset) (ys & yoffset) ins root =
    let xs' = appendText ins
            $ cutUntil xoffset
            $ resolvePath root xs
        ys' = cutFrom yoffset
            $ resolvePath root ys
     in Array.head $ join xs' ys'

splice :: (Path & TextOffset) -> (Path & TextOffset) -> Node -> Node
splice xpoint ypoint =
    spliceText xpoint ypoint ""

splitLevels :: Int -> ResolvedPath -> ResolvedPath -> (Int & Int)
splitLevels xlevel xs ys =
    let common = map _.level $ commonPath xs ys
        (_ & levels) = splitAt xlevel common
     in if length levels == 0
           then (-1 & -1)
           else (head levels & last levels)

newIDs :: (Int & Int) -> Int -> ResolvedPath -> ResolvedPath
newIDs (startLevel & endLevel) int =
    _.z <<< foldr step {int, child: Nothing, z: Nil}
    where
    step e@{node, level} {int, child, z} =
        if level >= startLevel && level <= endLevel then
            let node' = updateNodeId int node
                node'' = case child of
                    Nothing -> node'
                    Just c  -> updateChildAt 0 c node'
                e' = e{node = node''}
             in {int: int + 1, child: Just node', z: (e' : z)}
        else
            {int, child, z: (e : z)}

inlineLevel :: ResolvedPath -> Int
inlineLevel =
    fromMaybe 0 <<< foldl step Nothing
    where
    step Nothing {node, level} | isInline node =
        Just level
    step z _ = z

spliceInline :: (Path & TextOffset) -> (Path & TextOffset) -> Node -> Node -> Node
spliceInline (xs & xoffset) (ys & yoffset) ins root =
    let xs' = resolvePath root xs
        ys' = resolvePath root ys
        xlevel = inlineLevel xs'
        xs'' = appendNode xlevel ins
             $ cutUntil xoffset xs'
        maxId = nodeId ins
        ys'' = newIDs (splitLevels xlevel xs' ys') (maxId + 1)
             $ cutFrom yoffset ys'
     in Array.head $ joinUntil xlevel xs'' ys''

blockLevel :: ResolvedPath -> Int
blockLevel =
    fromMaybe 0 <<< foldr step Nothing
    where
    step {node, level} Nothing | isBlock node =
        Just level
    step _ z = z

spliceBlock :: (Path & TextOffset) -> (Path & TextOffset) -> Node -> Node -> Node
spliceBlock (xs & xoffset) (ys & yoffset) ins root =
    let xs' = resolvePath root xs
        ys' = resolvePath root ys
        xlevel = blockLevel xs'
        xs'' = appendNode xlevel ins
             $ cutUntil xoffset xs'
        maxId = nodeId ins
        ys'' = newIDs (splitLevels xlevel xs' ys') (maxId + 1)
             $ cutFrom yoffset ys'
     in Array.head $ joinUntil xlevel xs'' ys''
