module FFI.DOM.Event.InputEvent
    ( InputEvent
    , InputType(..)
    , inputData, inputType
    ) where

import Data.Maybe (Maybe(..))
import Effect.Exception.Unsafe (unsafeThrow)
import FFI.Object ((!), (!?))

-- | https://developer.mozilla.org/en-US/docs/Web/API/InputEvent
foreign import data InputEvent :: Type

-- | https://developer.mozilla.org/en-US/docs/Web/API/InputEvent/inputType
data InputType
    = DeleteContent
    | DeleteContentBackward
    | DeleteContentForward
    | DeleteByCut
    | DeleteWordBackward
    | DeleteWordForward
    | DeleteSoftLineBackward
    | DeleteSoftLineForward
    | InsertText
    | InsertLineBreak
    | InsertParagraph

inputType :: InputEvent -> InputType
inputType e = itype (e ! "inputType")
    where itype "deleteContent"          = DeleteContent
          itype "deleteContentBackward"  = DeleteContentBackward
          itype "deleteContentForward"   = DeleteContentForward
          itype "deleteByCut"            = DeleteByCut
          itype "deleteWordBackward"     = DeleteWordBackward
          itype "deleteWordForward"      = DeleteWordForward
          itype "deleteSoftLineBackward" = DeleteSoftLineBackward
          itype "deleteSoftLineForward"  = DeleteSoftLineForward
          itype "insertText"             = InsertText
          itype "insertLineBreak"        = InsertLineBreak
          itype "insertParagraph"        = InsertParagraph
          itype _                        = unsafeThrow "not implemented"

-- | https://developer.mozilla.org/en-US/docs/Web/API/InputEvent/data
inputData :: InputEvent -> String
inputData e = idata (e !? "data")
    where idata Nothing  = ""
          idata (Just x) = x
