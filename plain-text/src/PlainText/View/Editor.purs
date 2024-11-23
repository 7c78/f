module PlainText.View.Editor where

import Prelude hiding (apply)
import Data.Set as Set
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Unsafe ((&))
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Exception.Unsafe (unsafeThrow)
import FFI.DOM.Document (document)
import FFI.DOM.Event (addEventListener, listener, preventDefault)
import FFI.DOM.Event.InputEvent (InputType(..), inputData, inputType)
import PlainText.Model.Node (Node)
import PlainText.Model.Path (indexPaths)
import PlainText.Model.Selection (Selection(..))
import PlainText.View.DOM (DOMNode)
import PlainText.View.Node (renderRoot)
import PlainText.View.Selection (clearDOMSelection, fromSelection, getAnchor, getFocus, getSelection, isCollapsed, normalizeDOMPoint, renderDOMSelection, selectionWithin, toPoint, toPointEmptyText)
import PlainText.View.State (clearSelection, deleteBackward, deleteForward, insertLineBreak, insertParagraph, insertText, setSelection)

makeEditor :: DOMNode -> Node -> Effect Unit
makeEditor rootDOM rootNode = do
    void $ renderRoot Set.empty rootDOM rootNode
    stateRef <- Ref.new
        { root: rootNode
        , pathIndex: indexPaths rootNode
        , selection: Nothing
        }

    addEventListener rootDOM "dragstart" $ listener preventDefault
    addEventListener rootDOM "paste" $ listener preventDefault
    addEventListener rootDOM "cut" $ listener preventDefault

    addEventListener document "selectionchange" $ listener \_ -> do
        domSel <- getSelection
        selectedWithin <- selectionWithin domSel rootDOM

        whenM (pure selectedWithin) do
            state@{pathIndex} <- Ref.read stateRef

            if isCollapsed domSel then do
                let anchor = getAnchor domSel
                mAnchor <- normalizeDOMPoint anchor
                case mAnchor of
                    Just domAnchor ->
                        renderDOMSelection domSel domAnchor domAnchor
                    Nothing ->
                        pure unit
                point <- maybe (toPointEmptyText pathIndex anchor)
                               (toPoint rootDOM pathIndex)
                               mAnchor
                let sel = PointSelection point
                    newState = setSelection sel state
                Ref.write newState stateRef
            else do
                mAnchor <- normalizeDOMPoint (getAnchor domSel)
                mFocus <- normalizeDOMPoint (getFocus domSel)
                case mAnchor, mFocus of
                    Just domAnchor, Just domFocus -> do
                        renderDOMSelection domSel domAnchor domFocus
                        anchor <- toPoint rootDOM pathIndex domAnchor
                        focus <- toPoint rootDOM pathIndex domFocus
                        let sel = RangeSelection {anchor, focus}
                            newState = setSelection sel state
                        Ref.write newState stateRef
                    _, _ ->
                        unsafeThrow "not implemented"

        whenM (pure (not selectedWithin)) do
            state <- Ref.read stateRef
            let newState = clearSelection state
            Ref.write newState stateRef

    addEventListener rootDOM "beforeinput" $ listener \event -> do
        preventDefault event

        let transform = case inputType event of
                DeleteContentBackward ->
                    deleteBackward
                DeleteContentForward ->
                    deleteForward
                InsertText ->
                    insertText (inputData event)
                InsertLineBreak ->
                    insertLineBreak
                InsertParagraph ->
                    insertParagraph
                _ ->
                    unsafeThrow "not implemented"

        state <- Ref.read stateRef
        let (dirty & newState) = transform state
        Ref.write newState stateRef

        whenM (pure $ state.root /= newState.root) do
            rootViewNode' <- renderRoot dirty rootDOM newState.root

            domSel <- getSelection
            case newState.selection of
                Nothing ->
                    clearDOMSelection domSel
                Just sel -> do
                    let {anchor, focus} = fromSelection sel rootViewNode'
                    renderDOMSelection domSel anchor focus
