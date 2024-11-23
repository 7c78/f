module PlainText.Model.Structure where

import Prelude
import PlainText.Model.Node (ElementType(..), Node(..), isText)

isInline :: Node -> Boolean
isInline (Text _ _ _)           = true
isInline (LineBreak _)          = true
isInline (Element _ (Link _) _) = true
isInline _                      = false

isBlock :: Node -> Boolean
isBlock = not <<< isInline

allow :: Node -> Node -> Boolean
allow (Root _ _) child =
    case child of
        Element _ (Heading _) _ ->
            true
        Element _ (Paragraph _) _ ->
            true
        _otherwise ->
            false
allow (Element _ (Heading _) _) child
    | isInline child
    = true
allow (Element _ (Paragraph _) _) child
    | isInline child
    = true
allow (Element _ (Link _) _) child
    | isText child
    = true
allow _ _ = false

joinable :: Node -> Node -> Boolean
joinable (Text _ xattrs _) (Text _ yattrs _) =
    xattrs == yattrs
joinable (Element _ x _) (Element _ y _) =
    case x, y of
        Link xattrs, Link yattrs -> xattrs == yattrs
        Paragraph _, Paragraph _ -> true
        Heading _, Heading _     -> true
        Paragraph _, Heading _   -> true
        Heading _, Paragraph _   -> true
        _, _                     -> false
joinable _ _ =
    false
