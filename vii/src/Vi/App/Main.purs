module Vi.App.Main where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import FFI.AssocArray ((:=))
import FFI.DOM.Document (getElementById)
import FFI.DOM.Event
import FFI.DOM.Object ((.+))
import Vi.Model.Word as ViWord
import Vi.Model.Word (ViWord)
import Vi.Model.InputMethod as Vi

type Typing =
    { raw :: String
    , vi  :: Maybe ViWord
    , viS :: String
    }

initTyping :: Typing
initTyping =
    { raw: ""
    , vi:  Just ViWord.nil
    , viS: ""
    }

typeChar :: String -> Typing -> Typing
typeChar c t@{ raw, vi } =
    let raw' = raw <> c
        vi' = vi >>= Vi.snocChar Vi.telex c
        viS' = fromMaybe raw' (ViWord.toString <$> vi')
    in t{ raw = raw'
        , vi = vi'
        , viS = viS'
        }

main :: Effect Unit
main = do
    t1 <- getElementById "text1"
    t <- Ref.new initTyping

    addEventListener t1 "beforeinput" $ listener \e -> do
        preventDefault e
        case inputType e of
            InsertText -> do
                let c = inputData e
                t' <- Ref.modify (typeChar c) t
                t1 .+ "value":=t'.viS
            DeleteContentBackward -> do
                log "del back"
            _ ->
                pure unit
