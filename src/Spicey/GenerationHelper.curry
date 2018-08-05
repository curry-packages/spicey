module Spicey.GenerationHelper where

import AbstractCurry.Types
import AbstractCurry.Build
import Data.Char
import Database.ERD
import Database.ERD.Goodies
import Data.Time

------------------------------------------------------------------------
-- lower the first character in a string
lowerFirst :: String -> String
lowerFirst (y:ys) = (toLower y) : ys
lowerFirst []     = [] -- this case should not occur, but one never knows...

-- upper the first character in a string
upperFirst :: String -> String
upperFirst (y:ys) = (toUpper y) : ys
upperFirst []     = [] -- this case should not occur, but one never knows...

------------------------------------------------------------------------
--- Converts a string into a qualified name of the module
--- "Database.CDBI.Connection".
dbconn :: String -> QName
dbconn f = ("Database.CDBI.Connection", f)

--- Converts a string into a qualified name of the module "HTML.Base".
html :: String -> QName
html f = ("HTML.Base", f)

--- Converts a string into a qualified name of the module "WUI".
wui :: String -> QName
wui f = ("WUI", f)

-- Some module names:
spiceyModule :: String
spiceyModule = "System.Spicey"

authenticationModule :: String
authenticationModule = "System.Authentication"

-- Name of generic authorization module:
authorizationModule :: String
authorizationModule = "System.Authorization"

sessionInfoModule :: String
sessionInfoModule = "System.SessionInfo"

dataModuleName :: String
dataModuleName = "Config.RoutesData"

mappingModuleName :: String
mappingModuleName = "Config.ControllerMapping"

--- Name of EntitiesToHtml module.
entitiesToHtmlModule :: String -> String
entitiesToHtmlModule erdname = "View." ++ erdname ++ "EntitiesToHtml"

bootstrapModule :: String
bootstrapModule = "HTML.Styles.Bootstrap3"

-- Name of hrefButton operation:
hrefButtonName :: QName
hrefButtonName = (bootstrapModule,"hrefButton")

relatedRelation :: String -> Relationship -> String
relatedRelation en (Relationship _ [REnd en1 _ _, REnd en2 _ _]) =
  if en==en1 then en2 else en1

relationshipsForEntityName :: String -> [Relationship] -> [Relationship]
relationshipsForEntityName ename rels = filter endsIn rels
 where
  endsIn (Relationship _ ends) = any (\ (REnd n _ _) -> ename == n) ends

-- An entity is generated (to represent many-to-many relations)
-- if all attributes are foreign keys
isGenerated :: Entity -> Bool
isGenerated (Entity _ attrs) = null (filter (not . isForeignKey) attrs)

notPKey :: Attribute -> Bool
notPKey (Attribute _ _ k _) = k /= PKey

notKey :: Attribute -> Bool
notKey (Attribute _ t _ _) =
  case t of
    (KeyDom _) -> False
    _ -> True

-- An entity is relevant for a list of attributes if the first Key attribute
-- is a key to this entity.
isRelevantForEntity :: Entity -> [Attribute] -> Bool
isRelevantForEntity (Entity ename a) (attr:attrs) =
  case attr of
    (Attribute _ (KeyDom name) _ _) -> ename == name
    _ -> isRelevantForEntity (Entity ename a) attrs
isRelevantForEntity _ [] = False

oneToOne :: Entity -> [Relationship] -> [String]
oneToOne (Entity ename _) rel =
    map (relatedRelation ename) (filter isOneToOne rel)
  where
    isOneToOne :: Relationship -> Bool
    isOneToOne relationship =
      case relationship of
        (Relationship _ [(REnd _ _ (Exactly 1)), (REnd _ _ (Exactly 1))]) -> True
        _ -> False

manyToOne :: Entity -> [Relationship] -> [String]
manyToOne (Entity ename _) rel =
    map (relatedRelation ename) (filter isManyToOne rel)
  where
    isManyToOne :: Relationship -> Bool
    isManyToOne relationship =
      case relationship of
        (Relationship _ [REnd _ _ (Exactly 1),
                         REnd relEName _ (Between _ _)])
           -> relEName == ename
        _  -> False

manyToMany :: [Entity] -> Entity -> [String]
manyToMany entities forEntity =
    map (getOtherREnd forEntity)
        (filter (\ (Entity ename attr) -> isGenerated (Entity ename attr) &&
                                          isRelevantForEntity forEntity attr)
                entities)
  where
    getOtherREnd (Entity ename _)
                 (Entity _ [(Attribute _ (KeyDom name1) _ _),
                            (Attribute _ (KeyDom name2) _ _)]) =
      if (name1 == ename) then name2 else name1

linkTableName :: String -> String -> [Entity] -> String
linkTableName ename1 ename2 entities =
    getLinkTableName (filter isGenerated entities)
  where
    getLinkTableName ((Entity name [(Attribute _ (KeyDom rel1) _ _),
                                    (Attribute _ (KeyDom rel2) _ _)]):erest) =
      if (rel1==ename1 && rel2==ename2) || (rel1==ename2 && rel2==ename1)
      then name
      else getLinkTableName erest
    getLinkTableName [] = error "linkTableName: link not found"

--- The standard type of new and list controllers.
controllerType :: CTypeExpr
controllerType = baseType (spiceyModule,"Controller")

controllerModuleName :: String -> String
controllerModuleName entityName = "Controller." ++ entityName

--- The name of the controller function for a given entity and controller
--- functionality.
controllerFunctionName :: String -> String -> QName
controllerFunctionName entityName controllerFunction =
  (controllerModuleName entityName,
   controllerFunction ++ entityName ++ "Controller")

--- The name of the transaction function for a given entity and transaction
--- functionality.
transFunctionName :: String -> String -> QName
transFunctionName entityName controllerFunction =
  (controllerModuleName entityName,
   controllerFunction ++ entityName ++ "T")


viewModuleName :: String -> String
viewModuleName entityName = "View." ++ entityName

viewFunctionName :: String -> String -> QName
viewFunctionName entityName viewFunction =
  (viewModuleName entityName, viewFunction ++ entityName ++ "View")

viewBlockType :: CTypeExpr
viewBlockType = listType (baseType (html "HtmlExp"))


attrType :: Attribute -> CTypeExpr
attrType (Attribute _ t k False) =
  case t of (IntDom _)       -> if k==PKey
                                then ctvar "Key"
                                else ctvar "Int"
            (FloatDom _)     -> ctvar "Float"
            (StringDom _ )   -> ctvar "String"
            (BoolDom _)      -> ctvar "Bool"
            (DateDom _)      -> ctvar "ClockTime"
            (UserDefined s _)-> ctvar s
            (KeyDom _)       -> ctvar "Key"
            _                -> ctvar "Int"
attrType (Attribute _ t k True) =
  case t of (IntDom _)       -> if k==PKey
                                then maybeType (ctvar "Key")
                                else maybeType (ctvar "Int")
            (FloatDom _)     -> maybeType (ctvar "Float")
            (StringDom _ )   -> ctvar "String"
            (BoolDom _)      -> maybeType (ctvar "Bool")
            (DateDom _)      -> maybeType (ctvar "ClockTime")
            (UserDefined s _)-> maybeType (ctvar s)
            (KeyDom _)       -> maybeType (ctvar "Key")
            _                -> maybeType (ctvar "Int")

--- Generates Curry expressions representing default values.
--- The first argument contains an expression that is used for
--- ClockTime attributes (it is set to the current
--- time as a default value).
attrDefaultValues :: CExpr -> [Attribute] -> [CExpr]
attrDefaultValues defaultctime attrs = map defaultValue attrs
 where
  defaultValue (Attribute _ domain _ null) = case domain of
    IntDom    Nothing  -> nothingOrDefault
    IntDom    (Just n) -> addJust (CLit (CIntc n))
    FloatDom  Nothing  -> nothingOrDefault
    FloatDom  (Just x) -> addJust (CLit (CFloatc x))
    CharDom   Nothing  -> nothingOrDefault
    CharDom   (Just c) -> addJust (CLit (CCharc c))
    StringDom Nothing  -> string2ac "" -- null string values are empty strings
    StringDom (Just s) -> string2ac s
    BoolDom   Nothing  -> nothingOrDefault
    BoolDom   (Just b) -> addJust (constF (pre (if b then "True" else "False")))
    DateDom   Nothing  -> nothingOrDefault
    DateDom   (Just (CalendarTime y mo d h m s tz))
                       -> addJust (applyF ("Time", "toClockTime")
                                    [applyF ("Time", "CalendarTime")
                                      (map (CLit . CIntc) [y,mo,d,h,m,s,tz])])
    UserDefined _ _    -> nothingOrDefault
    KeyDom _           -> nothingOrDefault
    _ -> error "GenerationHelper.attrDefaultValues: unknown domain for attribute"
   where
     nothingOrDefault = if null
                        then constF (pre "Nothing")
                        else domainDefaultValue defaultctime domain

     -- add "Just" constructor if the attribute can be null-valued:
     addJust e = if null then applyF (pre "Just") [e] else e

--- Generates Curry expressions representing a default values
--- for a given domain.
--- The first argument contains an expression that is used for
--- ClockTime attributes (it is set to the current
--- time as a default value).
domainDefaultValue :: CExpr -> Domain -> CExpr
domainDefaultValue defaultctime domain = case domain of
    IntDom    _  -> CLit (CIntc 0)
    FloatDom  _  -> CLit (CFloatc 0)
    CharDom   _  -> CLit (CCharc ' ')
    StringDom _  -> string2ac []
    BoolDom   _  -> constF (pre "False")
    DateDom   _  -> defaultctime
    UserDefined _ _ -> list2ac [] -- no support of user-defined default values
    KeyDom _    -> CLit (CIntc 0)
    _ -> error "GenerationHelper.domainDefaultValue: unknown domain"

-- Is the attribute domain a string domain?
isStringDom :: Domain -> Bool
isStringDom dom = case dom of
                   StringDom _ -> True
                   _           -> False

hasDateAttribute :: [Attribute] -> Bool
hasDateAttribute = any isDate
 where
  isDate (Attribute _ domain _ _) = case domain of
    DateDom _   -> True
    _           -> False

combinator :: Int -> QName
combinator n
 | n==0 = error "GenerationHelper.combinator: empty attribute list"
 | n==1
 = error "GenerationHelper.combinator: no combinator for list of length 1"
 | n>14      = error "GenerationHelper.combinator: attribute list too long"
 | n==2      = (wui "wPair")
 | n==3      = (wui "wTriple")
 | otherwise = (wui $ "w" ++ show n ++ "Tuple")

-- Associate to each attribute of the argument list a WUI specification
-- as an abstract Curry program
attrWidgets :: [Attribute] -> [CExpr]
attrWidgets ((Attribute _ domain _ null):attrlist) =
  (widgetFor domain null) : (attrWidgets attrlist)
attrWidgets [] = []

widgetFor :: Domain -> Bool -> CExpr
widgetFor domain null =
  case domain of
    IntDom _    -> addMaybe (constF (wui "wInt"))
    FloatDom _  -> addMaybe (constF (wui "wFloat"))
    CharDom _   -> addMaybe (constF (wui "wString"))
    StringDom _ -> if null then constF (spiceyModule,"wString")
                           else constF (wui "wRequiredString")
                   --constF (wui (if null then "wString" else "wRequiredString"))
    BoolDom _   -> addMaybe (constF (wui "wBoolean"))
    DateDom _   -> addMaybe (constF (spiceyModule, "wDateType"))
    UserDefined _ _ -> addMaybe (applyF (wui "wCheckBool")
                                        [applyF (html "htxt") [string2ac ""]])
    KeyDom _    -> addMaybe (constF (wui "wInt"))
    _ -> error "widgetFor: unknown domain for attribute"
 where
  -- adds a Maybe WUI if null values are allowed
  addMaybe e =
    if null
     then applyF (spiceyModule,"wUncheckMaybe")
            [domainDefaultValue
               (applyF ("Time", "toClockTime")
                 [applyF ("Time", "CalendarTime")
                         (map (CLit . CIntc) [2018,1,1,0,0,0,0])])
               domain, e]
     else e
