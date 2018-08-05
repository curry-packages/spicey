--------------------------------------------------------------------------
--- This module implements some auxiliary operations to support the
--- generic implementation of the Spicey entities.
--------------------------------------------------------------------------

module System.Spicey (
  module System,
  module HTML.Base,
  module Numeric,
  Controller, applyControllerOn,
  nextController, nextControllerForData, confirmNextController,
  confirmController, transactionController,
  getControllerURL,getControllerParams, showControllerURL,
  getForm, wDateType, wBoolean, wUncheckMaybe, wFloat,
  displayError, cancelOperation,
  renderWuiForm, renderLabels,
  nextInProcessOr,
  stringToHtml, maybeStringToHtml,
  intToHtml,maybeIntToHtml, floatToHtml, maybeFloatToHtml,
  boolToHtml, maybeBoolToHtml, dateToHtml, maybeDateToHtml,
  userDefinedToHtml, maybeUserDefinedToHtml,
  spTable,
  setPageMessage, getPageMessage,
  saveLastUrl, getLastUrl, getLastUrls
  ) where

import Data.Char (isSpace,isDigit)
import Data.Global
import Numeric
import ReadShowTerm(readsQTerm)
import System.Process
import Data.Time

import Database.CDBI.Connection ( SQLResult )
import HTML.Base
import HTML.Styles.Bootstrap3
import WUI

import Config.UserProcesses
import System.Routes
import System.Processes
import System.Session
import System.Authentication

---------------- vvvv -- Framework functions -- vvvv -----------------------

-- a viewable can be turned into a representation which can be displayed
-- as interface
-- here: a representation of a HTML page
type Viewable = HtmlPage

type ViewBlock = [HtmlExp]

--- Controllers contains all logic and their result should be a Viewable.
--- if the behavior of controller should depend on URL parameters
--- (following the first name specifying the controller), one
--- can access these URL parameters by using the operation
--- Spicey.getControllerParams inside the controller.
type Controller = IO ViewBlock

--- Reads an entity for a given key and applies a controller to it.
applyControllerOn :: Maybe enkey -> (enkey -> IO en)
                  -> (en -> Controller) -> Controller
applyControllerOn Nothing _ _ = displayError "Illegal URL"
applyControllerOn (Just userkey) getuser usercontroller =
  getuser userkey >>= usercontroller

nextController :: Controller -> _ -> IO HtmlForm
nextController controller _ = do
  view <- controller
  getForm view

-- for WUIs
nextControllerForData :: (a -> Controller) -> a -> IO HtmlForm
nextControllerForData controller param = do
  view <- controller param
  getForm view

--- Call the next controller after a user confirmation.
--- The Boolean user answer is passed as an argument to the controller.
confirmNextController :: HtmlExp -> (Bool -> Controller) -> _ -> IO HtmlForm
confirmNextController question controller _ = do
  getForm [question,
           defaultButton "Yes" (nextController (controller True)),
           defaultButton "No"  (nextController (controller False))]

--- Ask the user for a confirmation and call the corresponding controller.
--- @param question - a question asked
--- @param yescontroller - the controller used if the answer is "yes"
--- @param nocontroller  - the controller used if the answer is "no"
confirmController :: [HtmlExp] -> Controller -> Controller -> Controller
confirmController question yescontroller nocontroller = do
  return $ question ++
           [par [defaultButton "Yes" (nextController yescontroller),
                 defaultButton "No"  (nextController nocontroller )]]

--- A controller to execute a transaction and proceed with a given
--- controller if the transaction succeeds. Otherwise, the
--- transaction error is shown.
--- @param trans - the transaction to be executed
--- @param controller - the controller executed in case of success
transactionController :: IO (SQLResult _) -> Controller -> Controller
transactionController trans controller = do
  transResult <- trans
  either (\error -> displayError (show error))
         (\_     -> controller)
         transResult

--- If we are in a process, execute the next process depending on
--- the provided information passed in the second argument,
--- otherwise execute the given controller (first argument).
nextInProcessOr :: Controller -> Maybe ControllerResult -> Controller
nextInProcessOr controller arg = do
  isproc <- isInProcess
  if isproc then advanceInProcess arg >> return [htxt ""] -- triggers redirect
            else controller


--------------------------------------------------------------------------
-- Operations for handling URL parameters

--- Parse the URL parameter passed to the main script. The result is a pair
--- consisting of the route and the list of parameters separated by '/'.
parseUrl :: String -> (String, [String])
parseUrl urlparam =
  let (url:ctrlparams) = splitUrl urlparam
  in  (url,ctrlparams)

--- Splits the URL parameter passed to the main script into a list of
--- strings. The strings are separated in the URL by '/'.
splitUrl :: String -> [String]
splitUrl url =
  let (ys,zs) = break (== '/') url
   in if null zs then [ys]
                 else ys : splitUrl (tail zs)

--- Gets the controller URL and the control parameters (separated by '/').
--- For instance, if the spicey script is called with the URL
--- "spicey.cgi?listEntity/arg1/arg2", this operation returns
--- ("listEntity",["arg1","arg2"]).
getControllerURL :: IO (String, [String])
getControllerURL = getUrlParameter >>= return . parseUrl

--- Gets the control parameters from the current URL.
getControllerParams :: IO [String]
getControllerParams = getUrlParameter >>= return . snd . parseUrl

--- Shows the URL corresponding to the control parameters.
--- The first argument is the URL of the controller (e.g., "listEntity")
--- and the second argument is the list of control parameters.
showControllerURL :: String -> [String] -> String
showControllerURL ctrlurl params = '?' : ctrlurl ++ concatMap ('/':) params

--------------------------------------------------------------------------
--- Standard rendering for WUI forms to edit data.
--- @param wuispec    - the associated WUI specification
--- @param initdata   - initial data to be prefilled in the form
--- @param ctrl       - the controller that handles the submission of the data
--- @param cancelctrl - the controller called if submission is cancelled
--- @param title      - the title of the WUI form
--- @param buttontag  - the text on the submit button
renderWuiForm :: WuiSpec a -> a -> (a -> Controller) -> Controller
              -> String -> String -> [HtmlExp]
renderWuiForm wuispec initdata controller cancelcontroller title buttontag =
  wuiframe hexp handler
 where
  wuiframe wuihexp hdlr =
    [h1 [htxt title],
     blockstyle "editform" [wuihexp],
     wuiHandler2button buttontag hdlr `addClass` "btn btn-primary",
     defaultButton "cancel"
                   (nextController (cancelOperation >> cancelcontroller))]

  (hexp,handler) = wuiWithErrorForm wuispec
                     initdata
                     (nextControllerForData controller)
                     (\he whdlr -> getForm (wuiframe he whdlr))

--- A WUI for manipulating CalendarTime entities.
--- It is based on a WUI for dates, i.e., the time is ignored.
wDateType :: WuiSpec ClockTime
wDateType = transformWSpec (tuple2date,date2tuple) wDate
 where
  tuple2date :: (Int, Int, Int) -> ClockTime
  tuple2date (day, month, year) =
    toClockTime (CalendarTime year month day 0 0 0 0)

  date2tuple :: ClockTime -> (Int, Int, Int)
  date2tuple ct = let CalendarTime year month day _ _ _ _ = toUTCTime ct
                  in (day, month, year)

--- A WUI for manipulating date entities.
wDate :: WuiSpec (Int, Int, Int)
wDate = wTriple (wSelectInt [1..31])
                (wSelectInt [1..12])
                (wSelectInt [1950..2050])

--- A WUI for manipulating Boolean entities. In general, this view should
--- be specialized by replacing true and false by more comprehensible strings.
wBoolean :: WuiSpec Bool
wBoolean = wSelectBool "True" "False"

--- A WUI transformer to map WUIs into WUIs for corresponding Maybe types.
wUncheckMaybe :: Eq a => a -> WuiSpec a -> WuiSpec (Maybe a)
wUncheckMaybe defval wspec =
  wMaybe (transformWSpec (not,not) (wCheckBool [htxt "No value"]))
         wspec
         defval

--- A widget for editing floating point values.
wFloat :: WuiSpec Float
wFloat = transformWSpec (readFloat, show)
            (wString `withCondition` (\s -> readMaybeFloat s /= Nothing))
 where
   readFloat s = maybe 0.0 id (readMaybeFloat s)

-- Read a float in a string.
-- Return Nothing is this is not a float string.
readMaybeFloat :: String -> Maybe Float
readMaybeFloat s =
  if all isFloatChar s
   then case readsQTerm s of
          [(x,tail)] -> if all isSpace tail then Just x else Nothing
          _  ->  Nothing
   else Nothing
 where
   isFloatChar c = isDigit c || c == '.'

--------------------------------------------------------------------------
-- Define page layout of the application.

--- The title of this application (shown in the header).
spiceyTitle :: String
spiceyTitle = "Spicey Application"

--- The home URL and brand shown at the left top of the main page.
spiceyHomeBrand :: (String, [HtmlExp])
spiceyHomeBrand = ("?", [homeIcon, htxt " Home"])

--- The standard footer of the Spicey page.
spiceyFooter :: [HtmlExp]
spiceyFooter =
  [par [htxt "powered by",
        href "http://www.informatik.uni-kiel.de/~pakcs/spicey"
             [image "images/spicey-logo.png" "Spicey"]
          `addAttr` ("target","_blank"),
        htxt "Framework"]]

--- Transforms a view into an HTML form by adding the basic page layout.
getForm :: ViewBlock -> IO HtmlForm
getForm viewblock = case viewblock of
  [HtmlText ""] ->
       return $ HtmlForm "forward to Spicey"
                  [formMetaInfo [("http-equiv","refresh"),
                                 ("content","1; url=spicey.cgi")]]
                  [par [htxt "You will be forwarded..."]]
  _ -> do
    routemenu <- getRouteMenu
    msg       <- getPageMessage
    login     <- getSessionLogin
    lasturl   <- getLastUrl
    cookie    <- sessionCookie
    return (bootstrapForm "." ["bootstrap.min","spicey"] spiceyTitle
               spiceyHomeBrand routemenu (rightTopMenu login)
               0 []  [h1 [htxt spiceyTitle]]
               (messageLine msg lasturl : viewblock ) spiceyFooter
              `addFormParam` cookie)
 where
  messageLine msg lasturl =
    if null msg
      then HtmlStruct "header" [("class","pagemessage pagemessage-empty")]
                      [htxt ("Last page: "++lasturl)]
      else HtmlStruct "header" [("class","pagemessage")] [htxt msg]

  rightTopMenu login =
    [[href "?login" (maybe [loginIcon, nbsp, htxt "Login"]
                           (\n -> [logoutIcon, nbsp, htxt "Logout"
                                  ,htxt $ " ("
                                  ,style "text-success" [userIcon]
                                  ,htxt $ " "++n++")"
                                  ])
                           login)]]

-------------------------------------------------------------------------
-- Action performed when a "cancel" button is pressed.
-- In this case, a message is shown.
cancelOperation :: IO ()
cancelOperation = do
  inproc <- isInProcess
  if inproc then removeCurrentProcess else done
  setPageMessage $ (if inproc then "Process" else "Operation") ++ " cancelled"

-- dummy-controller to display an error
displayError :: String -> Controller
displayError msg = do
  inproc <- isInProcess
  if inproc then removeCurrentProcess else done
  setPageMessage ("Error occurred!" ++
                  if inproc then " Process terminated!" else "")
  if null msg
   then return [htxt "General error (shown by function Spicey.displayError)"]
   else return [htxt msg]

-- like renderTaggedTuple from WUI Library but takes list of HtmlExp
-- instead of list of strings
renderLabels :: [[HtmlExp]] -> Rendering
renderLabels labels hexps =
  spTable (map (\(l, h) -> [l, [enlargeInput h]]) (zip labels hexps))
 where
  enlargeInput h = h `addClass` "input-xxlarge"

-- Convert standard datatype values to HTML representation
stringToHtml :: String -> HtmlExp
stringToHtml s = textstyle "type_string" s

maybeStringToHtml :: Maybe String -> HtmlExp
maybeStringToHtml s = textstyle "type_string" (maybe "" id s)

intToHtml :: Int -> HtmlExp
intToHtml i = textstyle "type_int" (show i)

maybeIntToHtml :: Maybe Int -> HtmlExp
maybeIntToHtml i = textstyle "type_int" (maybe "" show i)

floatToHtml :: Float -> HtmlExp
floatToHtml i = textstyle "type_float" (show i)

maybeFloatToHtml :: Maybe Float -> HtmlExp
maybeFloatToHtml i = textstyle "type_float" (maybe "" show i)

boolToHtml :: Bool -> HtmlExp
boolToHtml b = textstyle "type_bool" (show b)

maybeBoolToHtml :: Maybe Bool -> HtmlExp
maybeBoolToHtml b = textstyle "type_bool" (maybe "" show b)

dateToHtml :: ClockTime -> HtmlExp
dateToHtml ct = textstyle "type_calendartime" (toDayString (toUTCTime ct))

maybeDateToHtml :: Maybe ClockTime -> HtmlExp
maybeDateToHtml ct =
  textstyle "type_calendartime" (maybe "" (toDayString . toUTCTime) ct)

userDefinedToHtml :: Show a => a -> HtmlExp
userDefinedToHtml ud = textstyle "type_string" (show ud)

maybeUserDefinedToHtml :: Show a => Maybe a -> HtmlExp
maybeUserDefinedToHtml ud = textstyle "type_string" (maybe "" show ud)

--------------------------------------------------------------------------
-- Auxiliary HTML items:

--- Standard table in Spicey.
spTable :: [[[HtmlExp]]] -> HtmlExp
spTable items = table items  `addClass` "table table-hover table-condensed"

--------------------------------------------------------------------------
-- The page messages are implemented by a session store.
-- We define a global variable to store a message which is shown
-- in the next HTML page of a session.

--- Definition of the session state to store the page message (a string).
pageMessage :: Global (SessionStore String)
pageMessage = global emptySessionStore Temporary

--- Gets the page message and delete it.
getPageMessage :: IO String
getPageMessage = do
  msg <- getSessionData pageMessage
  removeSessionData pageMessage
  return (maybe "" id msg)

--- Set the page message of the current session.
setPageMessage :: String -> IO ()
setPageMessage msg = putSessionData msg pageMessage

--------------------------------------------------------------------------
-- Another example for using sessions.
-- We store the list of selected URLs into  the current session.

--- Definition of the session state to store the last URL (as a string).
lastUrls :: Global (SessionStore [String])
lastUrls = global emptySessionStore Temporary

--- Gets the list of URLs of the current session.
getLastUrls :: IO [String]
getLastUrls = getSessionData lastUrls >>= return . maybe [] id

--- Gets the last URL of the current session (or "?").
getLastUrl :: IO String
getLastUrl = do urls <- getLastUrls
                return (if null urls then "?" else head urls)

--- Saves the last URL of the current session.
saveLastUrl :: String -> IO ()
saveLastUrl url = do
  urls <- getLastUrls
  putSessionData (url:urls) lastUrls

--------------------------------------------------------------------------
