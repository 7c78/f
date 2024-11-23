module PlainText.View.Node where

import Prelude hiding (append)
import Data.Foldable (foldM)
import Data.Array (snoc)
import Data.Array.Unsafe (uncons)
import Data.Tuple.Unsafe (type (&), (&))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.String as String
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Effect.Exception.Unsafe (unsafeThrow)
import FFI.DOM.Object ((.+), (:=))
import FFI.DOM.Document (createElement, createTextNode)
import FFI.DOM.Element (append, after, replaceWith, remove)
import PlainText.Model.Node (ElementType(..), Node(..), NodeId, TextAttributes, children, nodeId, text)
import PlainText.View.DOM (DOMNode, DOMPoint)
import PlainText.View.Diff (EditOperation(..), diff)

newtype ViewNode = ViewNode
    { node     :: Node
    , domNode  :: DOMNode
    , children :: Array ViewNode
    }

viewNodeId :: ViewNode -> NodeId
viewNodeId (ViewNode {node}) = nodeId node

foreign import _setDOMx :: EffectFn2 DOMNode ViewNode Unit
setDOMx :: DOMNode -> ViewNode -> Effect Unit
setDOMx = runEffectFn2 _setDOMx

foreign import emptyTextViewNode :: Node -> DOMNode -> ViewNode
foreign import textViewNode :: Node -> DOMNode -> DOMNode -> ViewNode
foreign import updateTextViewNode :: ViewNode -> Node -> ViewNode

foreign import _setDOMTextData :: EffectFn2 DOMNode String Unit
setDOMTextData :: DOMNode -> String -> Effect Unit
setDOMTextData = runEffectFn2 _setDOMTextData

foreign import _closest :: EffectFn3 DOMNode DOMNode (Node -> Boolean) ViewNode
closest :: DOMNode -> DOMNode -> (Node -> Boolean) -> Effect ViewNode
closest = runEffectFn3 _closest

foreign import _emptyText :: EffectFn1 DOMPoint ViewNode
emptyText :: DOMPoint -> Effect ViewNode
emptyText = runEffectFn1 _emptyText

foreign import domText :: ViewNode -> DOMNode

foreign import _getRenderedChildren :: EffectFn1 DOMNode (Array ViewNode)
getRenderedChildren :: DOMNode -> Effect (Array ViewNode)
getRenderedChildren = runEffectFn1 _getRenderedChildren

renderRoot :: Set NodeId -> DOMNode -> Node -> Effect ViewNode
renderRoot dirty domNode node = do
    vnode <- renderNode dirty domNode node
    setDOMx domNode vnode
    pure vnode

renderNode :: Set NodeId -> DOMNode -> Node -> Effect ViewNode
renderNode dirty domNode node = do
    oldChildren <- getRenderedChildren domNode
    let oldIDs = map viewNodeId oldChildren
        ops = diff oldIDs (children node) dirty
    patches <- patch ops oldChildren
    children <- patchNode dirty domNode node patches
    pure $ ViewNode {node, domNode, children}

type Patch = EditOperation & Maybe ViewNode & Maybe ViewNode

patch :: Array EditOperation -> Array ViewNode -> Effect (Array Patch)
patch ops oldVnodes = do
    let z = (oldVnodes & [])
    (_ & patches) <- foldM patchOp z ops
    pure patches

patchOp
    :: (Array ViewNode & Array Patch)
    -> EditOperation
    -> Effect (Array ViewNode & Array Patch)
patchOp (oldVnodes & patches) op@(Insert node) = do
    case node of
        Text _ _ s -> do
            vnode <- if s == ""
                then createEmptyTextViewNode node
                else createTextViewNode node
            let p = (op & Nothing & Just vnode)
            pure (oldVnodes & (patches `snoc` p))
        LineBreak _ -> do
            domNode <- createElement "br"
            let vnode = ViewNode {node, domNode, children: []}
            let p = (op & Nothing & Just vnode)
            pure (oldVnodes & (patches `snoc` p))
        Element _ etype _ -> do
            domNode <- createElementDOM etype
            let vnode = ViewNode {node, domNode, children: []}
            let p = (op & Nothing & Just vnode)
            pure (oldVnodes & (patches `snoc` p))
        Root _ _ ->
            unsafeThrow "patchOp: invalid call on Root"
patchOp (oldVnodes & patches) op@(Update _ node) =
    let (oldVnode@(ViewNode old) & oldVnodes') = uncons oldVnodes
     in case node of
            Text _ _ _ -> do
                case text old.node, text node of
                    "", s | String.length s > 0 -> do
                        vnode <- createTextViewNode node
                        let p = (op & Just oldVnode & Just vnode)
                        pure (oldVnodes' & patches `snoc` p)
                    s, "" | String.length s > 0 -> do
                        vnode <- createEmptyTextViewNode node
                        let p = (op & Just oldVnode & Just vnode)
                        pure (oldVnodes' & patches `snoc` p)
                    _, _ -> do
                        let vnode = updateTextViewNode oldVnode node
                            p = (op & Just oldVnode & Just vnode)
                        pure (oldVnodes' & patches `snoc` p)
            _ -> do
                let vnode = ViewNode old{node = node}
                    p = (op & Just oldVnode & Just vnode)
                pure (oldVnodes' & patches `snoc` p)
patchOp (oldVnodes & patches) op@(Retain _) = do
    let (oldVnode & oldVnodes') = uncons oldVnodes
        p = (op & Just oldVnode & Just oldVnode)
    pure (oldVnodes' & patches `snoc` p)
patchOp (oldVnodes & patches) op@(Delete _) = do
    let (oldVnode & oldVnodes') = uncons oldVnodes
        p = (op & Just oldVnode & Nothing)
    pure (oldVnodes' & patches `snoc` p)
patchOp _ _ =
    unsafeThrow "not implemented"

createTextViewNode :: Node -> Effect ViewNode
createTextViewNode node@(Text _ attrs s) = do
    (mMarkDOM & textDOM) <- createTextDOM attrs s
    let (markDOM & textDOM) = case mMarkDOM of
            Nothing -> (textDOM & textDOM)
            Just m  -> (m & textDOM)
    pure $ textViewNode node markDOM textDOM
createTextViewNode _ =
    unsafeThrow "createTextViewNode: invalid call"

createEmptyTextViewNode :: Node -> Effect ViewNode
createEmptyTextViewNode node = do
    domNode <- createElement "br"
    pure $ emptyTextViewNode node domNode

createTextDOM :: TextAttributes -> String -> Effect (Maybe DOMNode & DOMNode)
createTextDOM attrs@{italic} content | italic = do
    em <- createElement "em"
    mStrong & text <- createTextDOM attrs{italic = false} content
    case mStrong of
        Nothing     -> em `append` text
        Just strong -> em `append` strong
    pure (Just em & text)
createTextDOM attrs@{bold} content | bold = do
    strong <- createElement "strong"
    (_ & text) <- createTextDOM attrs{bold = false} content
    strong `append` text
    pure (Just strong & text)
createTextDOM _attrs content = do
    text <- createTextNode content
    pure (Nothing & text)

createElementDOM :: ElementType -> Effect DOMNode
createElementDOM (Heading {level}) =
    createElement (headingTag level)
createElementDOM (Paragraph _) =
    createElement "p"
createElementDOM (Link {href}) = do
    a <- createElement "a"
    a .+ "href":=href
    pure a

headingTag :: Int -> String
headingTag level =
    case level of
        1 -> "h1"
        2 -> "h2"
        3 -> "h3"
        4 -> "h4"
        5 -> "h5"
        6 -> "h6"
        _ -> unsafeThrow "headingTag: invalid level"

patchNode :: Set NodeId -> DOMNode -> Node -> Array Patch -> Effect (Array ViewNode)
patchNode dirty domNode _ patches = do
    {z} <- foldM (patchChild dirty domNode) {sibling: Nothing, z: []} patches
    pure z

patchChild
    :: Set NodeId
    -> DOMNode
    -> {sibling :: Maybe DOMNode, z :: Array ViewNode}
    -> Patch
    -> Effect {sibling :: Maybe DOMNode, z :: Array ViewNode}
patchChild dirty parent {sibling, z} (Insert _ & _ & Just vnode@(ViewNode {node, domNode})) = do
    vnode' <- case node of
        Element _ _ _ ->
            renderNode dirty domNode node
        _ ->
            pure vnode
    setDOMx domNode vnode'
    case sibling of
        Nothing ->
            parent `append` domNode
        Just sibling ->
            sibling `after` domNode
    let sibling' = Just domNode
        z' = z `snoc` vnode'
    pure {sibling: sibling', z: z'}
patchChild dirty _ {z} (Update _ _ & Just (ViewNode old) & Just vnode@(ViewNode {domNode, node})) = do
    if old.domNode == domNode then
        updateDOM domNode old.node node
    else
        old.domNode `replaceWith` domNode
    vnode' <- case node of
        Element _ _ _ ->
            renderNode dirty domNode node
        _ ->
            pure vnode
    setDOMx domNode vnode'
    let sibling' = Just domNode
        z' = z `snoc` vnode'
    pure {sibling: sibling', z: z'}
patchChild _ _ {z} (Retain _ & _ & Just vnode@(ViewNode {domNode})) = do
    pure {sibling: Just domNode, z: z `snoc` vnode}
patchChild _ _ z (Delete _ & Just (ViewNode old) & _) = do
    remove old.domNode
    pure z
patchChild _ _ _ _ = do
    unsafeThrow "not implemented"

updateDOM :: DOMNode -> Node -> Node -> Effect Unit
updateDOM domNode (Text _ _ s) (Text _ _ t) = do
    whenM (pure (s /= t)) do
        setDOMTextData domNode t
updateDOM _ _ _ = do
    pure unit
