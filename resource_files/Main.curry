--------------------------------------------------------------------------
--- This main module of a Spicey application.
--------------------------------------------------------------------------

module Main where

import HTML.Base
import WUI

import Config.ControllerMapping
import Config.RoutesData
import System.Routes
import System.Processes
import System.Spicey


dispatcher :: IO HtmlForm
dispatcher = do
  -- get url
  (url,ctrlparams) <- getControllerURL
  
  controller <- nextControllerRefInProcessOrForUrl url >>=
                maybe (displayError "Illegal URL!")
                      getController

  form <- getForm controller
  saveLastUrl (url ++ concatMap ("/"++) ctrlparams)
  return form

--- Main function: call the dispatcher
main :: IO HtmlForm
main = dispatcher
