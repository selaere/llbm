module Main where

import Prelude

import Affjax as AJ
import Affjax.ResponseFormat (string)
import Affjax.Web as AJW
import Control.Monad.Error.Class (liftEither)
import Data.Bifunctor (lmap)
import Data.Either (note)
import Effect (Effect)
import Effect.Aff (error)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Main.Common ((∘))
import Main.Thorax (render)
import Main.Spine (File, handleAction, initialState, parseFile)

component ∷ ∀query o m. MonadAff m ⇒ H.Component query File o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction }
  }

main ∷ Effect Unit
main = HA.runHalogenAff do
  file ← AJW.get string "hist.txt"
       >>= liftEither ∘ lmap (error ∘ AJ.printError)
  datas ← liftEither $ note (error "file failed parsing") $ parseFile file.body
  body ← HA.awaitBody
  runUI component datas body
