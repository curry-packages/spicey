{-# OPTIONS_CYMAKE -Wno-incomplete-patterns #-}

module Spicey.ControllerGeneration where

import Data.Char ( toLower )

import AbstractCurry.Types
import AbstractCurry.Build
import Database.ERD
import Database.ERD.Goodies

import Spicey.GenerationHelper

-- Name of entity-specific authorization module:
enauthModName :: String
enauthModName = "System.AuthorizedActions"

-- Name of module defining the default controller:
defCtrlModName :: String
defCtrlModName = "Controller.DefaultController"

-- "main"-function
generateControllersForEntity :: String -> [Entity] -> Entity -> [Relationship]
                             -> CurryProg
generateControllersForEntity erdname allEntities
                             entity@(Entity ename attrlist) relationships =
 let noKeyAttrs = filter (\a -> notKey a && notPKey a) attrlist
 in
  CurryProg
   (controllerModuleName ename)
   -- imports:
   [ timeModule
   , "HTML.Base", "HTML.Session", "HTML.WUI"
   , model erdname
   , "Config.EntityRoutes", "Config.UserProcesses"
   , sessionInfoModule, authorizationModule, enauthModName, spiceyModule
   , "System.PreludeHelpers"
   , entitiesToHtmlModule erdname
   , viewModuleName ename
   ]
   Nothing -- defaultdecl
   [] -- classdecls
   [] -- instdecls
   [newEntityType erdname entity relationships allEntities] -- typedecls
   -- functions
   (
    [
     -- controller for dispatching to various controllers:
     mainController erdname entity relationships allEntities,
     -- controller for providing a page to enter new entity data:
     --newController erdname entity relationships allEntities,
     newController erdname (Entity ename noKeyAttrs) relationships allEntities,
     newForm  erdname (Entity ename noKeyAttrs) relationships allEntities,
     newStore erdname (Entity ename noKeyAttrs) relationships allEntities,
     -- transaction for saving data in new entity:
     createTransaction erdname entity relationships allEntities,
     -- controller to show an existing record in a form to edit
     editController erdname entity relationships allEntities,
     editForm  erdname (Entity ename noKeyAttrs) relationships allEntities,
     editStore erdname (Entity ename noKeyAttrs) relationships allEntities,
     -- transaction to update a record with the given data
     updateTransaction erdname entity relationships allEntities,
     -- controller to delete an entity with the given data
     deleteController erdname entity relationships allEntities,
     -- controller to destroy an entity with the given data
     destroyController erdname entity relationships allEntities,
     -- transaction to delete an entity with the given data
     deleteTransaction erdname entity relationships allEntities,
     -- controller to list all entities:
     listController erdname entity relationships allEntities,
     -- controller to show entites:
     showController erdname entity relationships allEntities
    ] ++ 
    manyToManyAddOrRemove erdname entity (manyToMany allEntities entity)
                          allEntities ++
    --(getAll erdname entity (manyToOne entity relationships) allEntities) ++
    --(getAll erdname entity (manyToMany allEntities entity) allEntities) ++
    --(manyToManyGetRelated erdname entity (manyToMany allEntities entity) allEntities) ++
     manyToOneGetRelated erdname entity (manyToOne entity relationships)
                         allEntities relationships
   )
   [] -- opdecls


-- erdname: name of the entity-relationship-specification
-- entity: the entity to generate a controller for
type ControllerGenerator = String -> Entity -> [Relationship] -> [Entity]
                        -> CFuncDecl

-- Generates the main controller that dispatches to the various
-- subcontrollers according to the URL parameters.
mainController :: ControllerGenerator
mainController _ (Entity entityName _) _ _ =
  controllerFunction 
  ("Choose the controller for a "++entityName++
   " entity according to the URL parameter.")
  entityName "main" 0
    controllerType -- function type
    [simpleRule [] -- no arguments
      (doExpr
         [CSPat (CPVar (1,"args"))
                (constF (spiceyModule,"getControllerParams")),
          CSExpr
           (CCase CRigid (CVar (1,"args"))
            ([cBranch (listPattern [])
                      (constF (controllerFunctionName entityName "list")),
              cBranch (listPattern [stringPattern "list"])
                      (constF (controllerFunctionName entityName "list")),
              cBranch (listPattern [stringPattern "new"])
                      (constF (controllerFunctionName entityName "new"))] ++
              map applyControllerBranch ["show", "edit", "delete", "destroy"] ++
             [cBranch (CPVar (3,"_"))
                      (constF (spiceyModule, "displayUrlError"))])
          )
         ]
      )]
 where
  applyControllerBranch n = let svar = (2,"s") in
    cBranch (listPattern [stringPattern n, CPVar svar])
            (applyF (spiceyModule,"controllerOnKey")
                    [CVar svar, constF (controllerFunctionName entityName n)])

--- Generates a type alias for a "new entity" tuple type which is
--- used to create and insert new entities (without an entity key).
newEntityType :: String -> Entity -> [Relationship] -> [Entity] -> CTypeDecl
newEntityType _ (Entity entityName attrList) relationships allEntities =
  let notGeneratedAttributes = filter (\attr -> not (isForeignKey attr)
                                                && notPKey attr)
                                      attrList
      manyToManyEntities = manyToMany allEntities (Entity entityName attrList)
      manyToOneEntities  = manyToOne (Entity entityName attrList) relationships
  in CTypeSyn (newEntityTypeName entityName) Private []
       (tupleType (map attrType notGeneratedAttributes ++
                   map ctvar manyToOneEntities ++
                   map (listType . ctvar) manyToManyEntities))

------------------------------------------------------------------------------
-- generates a controller to show a form to create a new entity
-- the input is then passed to the create controller
-- only has to call the blank entry form and pass the create controller
newController :: ControllerGenerator
newController erdname (Entity entityName attrList) relationships allEntities =
  controllerFunction 
  ("Shows a form to create a new " ++ entityName ++ " entity.")
  entityName "new" 0
  controllerType -- function type
  [simpleRule [] $ -- no arguments
     applyF (pre "$")
       [applyF checkAuthorizationFunc
         [applyF (enauthModName, lowerFirst entityName ++ "OperationAllowed")
           [constF (authorizationModule,"NewEntity")]],
        CLambda [CPVar infovar] $
         doExpr (
          map 
           (\ (ename, num) ->
              CSPat (CPVar (num,"all" ++ ename ++ "s")) 
                    (applyF (model erdname,"runQ")
                            [constF (model erdname,"queryAll" ++ ename ++ "s")])
           )
           (zip (manyToOneEntities ++ manyToManyEntities) [2..]) ++
         (if hasDateAttribute attrList
            then [CSPat (CPVar ctimevar) (constF (timeModule,"getClockTime"))]
            else []) ++
         (if null manyToOneEntities
            then setParCallAndReturn
            else [CSExpr $ ifThenElseExp
                    (foldr1 (\e1 e2 -> applyF (pre "||") [e1,e2])
                            (map (\ (name, varId) -> applyF (pre "null")
                                      [CVar (varId, "all" ++ name ++ "s")])
                                 (zip manyToOneEntities [2..])))
                    (applyF (pre "return")
                       [ list2ac [applyF (html "h2")
                                   [list2ac [applyF (html "htxt")
                                               [string2ac missingError]]]]])
                    (doExpr setParCallAndReturn)]
         ))]]
 where
  manyToManyEntities = manyToMany allEntities (Entity entityName attrList)
  manyToOneEntities  = manyToOne (Entity entityName attrList) relationships
  missingError = "Some related entities are required but not yet undefined"
  infovar      = (0,"sinfo")
  ctimevar     = (1,"ctime")

  setParCall =
    applyF (wuiModule "setParWuiStore")
      [constF (controllerStoreName entityName "new"),
       tupleExpr
        ([CVar infovar] ++ 
         map (\ (ename, num) -> CVar (num, "all" ++ ename ++ "s"))
             (zip (manyToOneEntities ++ manyToManyEntities) [2 ..])),
       tupleExpr $
         attrDefaultValues (CVar ctimevar) attrList ++
         map (\ (name, varId) -> applyF (pre "head")
                                        [CVar (varId, "all" ++ name ++ "s")])
             (zip manyToOneEntities [2..]) ++
         map (\_ -> list2ac []) (zip manyToManyEntities [2..])
      ]

  setParCallAndReturn =
     [CSExpr setParCall,         
      CSExpr $ applyF (pre "return")
                 [list2ac [applyF (html "formElem")
                          [constF (controllerFormName entityName "new")]]]]

--- Generates the form definition to create a new entity.
newForm :: String -> Entity -> [Relationship] -> [Entity] -> CFuncDecl
newForm erdname entity@(Entity entityName attrlist) relationships allEntities =
  cmtfunc ("A WUI form to create a new " ++ entityName ++ " entity.\n" ++
           "The default values for the fields are stored in '" ++
           snd (controllerStoreName entityName "new") ++ "'.")
    (controllerFormName entityName "new") 0
    Public
    (emptyClassType $ applyTC (htmlModule "HtmlFormDef")
        [newTupleType entity relationships allEntities])
    [simpleRule []
      (applyF (wuiModule "pwui2FormDef")
        [string2ac $ showQName $ controllerFormName entityName "new",
         constF (controllerStoreName entityName "new"),
         wuiFun, storeFun, renderFun])]
 where
  manyToManyEntities = manyToMany allEntities (Entity entityName attrlist)
  manyToOneEntities  = manyToOne (Entity entityName attrlist) relationships
  arity1 = 1 + length manyToOneEntities + length manyToManyEntities
  listEntityURL = '?' : entityName ++ "/list"

  wuiFun =
    CLambda
      [tuplePattern
        ([CPVar (1,"_")] ++
         map (\ (name, varId) -> CPVar(varId,("possible"++name++"s")))
             (zip (manyToOneEntities++manyToManyEntities) [2..]))] $
      applyF (viewModuleName entityName, "w" ++ entityName)
        (map (\ (name, varId) -> CVar(varId,("possible"++name++"s")))
             (zip (manyToOneEntities ++ manyToManyEntities) [2..]) )

  storeFun =
    let entvar    = (1, "entity")
        newentvar = (2, "newentity")
    in
    CLambda [CPVar (0,"_"), CPVar entvar]
      (applyF checkAuthorizationFunc
         [applyF (enauthModName, lowerFirst entityName ++ "OperationAllowed")
            [constF (authorizationModule,"NewEntity")],
          CLambda [CPVar (0,"_")]
            (applyF (spiceyModule,"transactionController")
               [applyF (model erdname,"runT")
                  [applyF (transFunctionName entityName "create")
                           [CVar entvar]],
                CLambda [CPVar newentvar]
                  (doExpr
                     [CSExpr $ applyF (spiceyModule,"setPageMessage")
                                [string2ac $ "New " ++ entityName ++ " created"],
                      CSExpr $ applyF (spiceyModule,"nextInProcessOr")
                        [applyF (spiceyModule,"redirectController")
                           [applyF (spiceyModule,"showRoute") [CVar newentvar]],
                         constF (pre "Nothing")]])])])

  renderFun =
    CLambda [tuplePattern
               (map CPVar (sinfovar : map (\v -> (v,"_")) [2 .. arity1]))] $
      letExpr
        [CLocalPat
           (CPVar entvar)
           (simpleRhs (simpleTyped (constF (pre "failed"))
                                   (baseType (model erdname, entityName))))]
        (applyF (spiceyModule,"renderWUI")
          [CVar sinfovar,
           string2ac $ "Create new " ++ entityName,
           string2ac "Create",
           applyF (spiceyModule,"listRoute") [CVar entvar],
           constF (pre "()")
          ])
   where sinfovar = (1, "sinfo")
         entvar   = (2, "phantom")


--- Generates the store for WUI to create a new entity.
newStore :: String -> Entity -> [Relationship] -> [Entity] -> CFuncDecl
newStore _ entity@(Entity entityName _) relationships allEntities =
  cmtfunc "The data stored for executing the \"new entity\" WUI form."
    (controllerStoreName entityName "new") 0
    Private
    (emptyClassType $
       applyTC (sessionModule "SessionStore")
         [newTupleType entity relationships allEntities])
    [simpleRule []
      (applyF (sessionModule "sessionStore")
              [string2ac $ "new" ++ entityName ++ "Store"])]


--- Computes the tuple type of the data to be stored and manipulated
--- by the WUI to create a new entity.
newTupleType :: Entity -> [Relationship] -> [Entity] -> CTypeExpr
newTupleType (Entity entityName attrlist) relationships allEntities =
  tupleType
    [tupleType $
       [userSessionInfoType] ++
       map (\e -> listType (ctvar e))
           (manyToOneEntities ++ manyToManyEntities), -- possible values
     applyTC (wuiModule "WuiStore")
             [baseType (newEntityTypeName entityName)]]
 where
  manyToManyEntities = manyToMany allEntities (Entity entityName attrlist)
  manyToOneEntities  = manyToOne (Entity entityName attrlist) relationships


--- Generates a transaction to store a new entity.
createTransaction :: ControllerGenerator
createTransaction erdname (Entity entityName attrList)
                  relationships allEntities = stCmtFunc
  ("Transaction to persist a new " ++ entityName ++ " entity to the database.")
  (transFunctionName entityName "create")
  1 Private
    (baseType (newEntityTypeName entityName)
      ~> applyTC (dbconn "DBAction") [baseType (model erdname,entityName)])
  [simpleRule 
    [tuplePattern
      (map (\ (param, varId) -> CPVar (varId, param)) 
           (zip (parameterList ++ map lowerFirst manyToOneEntities ++
                 map (\e -> (lowerFirst e) ++ "s") manyToManyEntities)
                 [1..]))
    ] -- parameter list for controller
    (doExpr $
       CSPat (cpvar "newentity")
         (applyF (entityConstructorFunction erdname
                    (Entity entityName attrList) relationships)
                 (map (\ ((Attribute name dom key null), varId) -> 
                     if (isForeignKey (Attribute name dom key null))
                       then applyF (model erdname,
                              lowerFirst (getReferencedEntityName dom) ++ "Key")
                                   [CVar (varId,
                                       lowerFirst (getReferencedEntityName dom))]
                       else let cv = CVar (varId, lowerFirst name)
                            in if hasDefault dom && not (isStringDom dom)
                                  && not null
                                 then applyF (pre "Just") [cv]
                                 else cv)
                     (zip noPKeys [1..])
                  )) :
       map (\name -> CSExpr $
                       applyF (controllerModuleName entityName,
                              "add" ++ linkTableName entityName name allEntities)
                              [cvar (lowerFirst name ++ "s"),
                               cvar "newentity"])
           manyToManyEntities ++
       [CSExpr $ applyF (pre "return") [cvar "newentity"]])
  ]
 where
  noPKeys            = (filter notPKey attrList)
  -- foreignKeys = (filter isForeignKey attrList)
  -- notGeneratedAttributes = filter (\attr -> (not (isForeignKey attr))
  --                                          && (notPKey attr))     attrList
  parameterList      = map (\(Attribute name _ _ _) -> lowerFirst name)
                           (filter (not . isForeignKey) noPKeys)
  manyToManyEntities = manyToMany allEntities (Entity entityName attrList)
  manyToOneEntities  = manyToOne (Entity entityName attrList) relationships

------------------------------------------------------------------------------
--- Generates a controller to edit an entity.
editController :: ControllerGenerator
editController erdname (Entity entityName attrList) relationships allEntities =
  controllerFunction
    ("Shows a form to edit the given " ++ entityName ++ " entity.")
    entityName "edit" 1
    (baseType (model erdname,entityName) ~> controllerType)
    [simpleRule [CPVar pvar] -- parameterlist for controller
      (applyF (pre "$")
        [applyF checkAuthorizationFunc
          [applyF (enauthModName,lowerFirst entityName++"OperationAllowed")
            [applyF (authorizationModule,"UpdateEntity") [CVar pvar]]],
         CLambda [CPVar infovar] $
           doExpr (
            map (\ (ename, num) ->
                    CSPat (CPVar (num,"all"++ename++"s")) 
                          (applyF (model erdname,"runQ")
                             [constF (model erdname,"queryAll"++ename++"s")]))
               (zip (manyToOneEntities ++ manyToManyEntities) [1..]) ++
            map 
              (\ (ename, num) -> CSPat (CPVar (num,(lowerFirst (fst $ relationshipName entityName ename relationships))++ename)) 
                              (
                                applyF (model erdname,"runJustT") [
                                  applyF (controllerModuleName entityName,"get"++(fst $ relationshipName entityName ename relationships)++ename) [CVar pvar]
                                ]
                              )
              )
              (zip manyToOneEntities [1..]) ++
            map 
              (\ (ename, num) -> CSPat (CPVar (num,(lowerFirst (linkTableName entityName ename allEntities))++ename++"s")) 
                              (
                                applyF (model erdname,"runJustT") [
                                  applyF (controllerModuleName entityName,"get"++entityName++ename++"s") [CVar pvar]
                                ]
                              )
              )
              (zip manyToManyEntities [1..]) ++
            [CSExpr setParCall,
             CSExpr $ applyF (pre "return")
               [list2ac [applyF (html "formElem")
                           [constF (controllerFormName entityName "edit")]]]
            ])])]
 where
  manyToManyEntities = manyToMany allEntities (Entity entityName attrList)
  manyToOneEntities  = manyToOne (Entity entityName attrList) relationships
  pvar               = (0, lowerFirst entityName ++ "ToEdit")
  infovar            = (1, "sinfo")

  setParCall =
    applyF (wuiModule "setParWuiStore")
      [constF (controllerStoreName entityName "edit"),
       tupleExpr
        ([CVar infovar, CVar pvar] ++ 
         map (\ (ename, num) ->
               CVar (num,lowerFirst (fst $ relationshipName
                            entityName ename relationships)
                         ++ ename))
             (zip manyToOneEntities [1..]) ++
         map (\ (ename, num) -> CVar (num, "all"++ename++"s"))
             (zip (manyToOneEntities ++ manyToManyEntities)
                  [1..])),
       tupleExpr
        ([CVar pvar] ++ 
          (map (\ (ename, num) ->
                 CVar (num,lowerFirst (linkTableName entityName
                                       ename allEntities)
                        ++ename++"s"))
               (zip manyToManyEntities [1..])))]


--- Generates the form definition to edit an entity.
editForm :: String -> Entity -> [Relationship] -> [Entity] -> CFuncDecl
editForm erdname entity@(Entity entityName attrlist) relationships allEntities =
  cmtfunc ("A WUI form to edit a " ++ entityName ++ " entity.\n" ++
           "The default values for the fields are stored in '" ++
           snd (controllerStoreName entityName "edit") ++ "'.")
    (controllerFormName entityName "edit") 0
    Public
    (emptyClassType $ applyTC (htmlModule "HtmlFormDef")
      [editTupleType erdname entity relationships allEntities])
    [simpleRule []
      (applyF (wuiModule "pwui2FormDef")
        [string2ac $ showQName $ controllerFormName entityName "edit",
         constF (controllerStoreName entityName "edit"),
         wuiFun, storeFun, renderFun])]
 where
  manyToManyEntities = manyToMany allEntities (Entity entityName attrlist)
  manyToOneEntities  = manyToOne (Entity entityName attrlist) relationships
  arity1 = 2 + length manyToOneEntities * 2 + length manyToManyEntities

  wuiFun =
    CLambda
      [tuplePattern
        ([CPVar (1,"_"), CPVar (1, lowerFirst entityName)] ++
         map (\ (name, varId) -> CPVar(varId,("related"++name)))
             (zip manyToOneEntities [2..]) ++
         map (\ (name, varId) -> CPVar(varId,("possible"++name++"s")))
             (zip (manyToOneEntities++manyToManyEntities) [2..]))] $
      applyF (viewModuleName entityName, "w" ++ entityName ++ "Type")
        ([cvar (lowerFirst entityName)] ++
         map (\ (name, varId) -> CVar(varId,("related"++name)))
             (zip manyToOneEntities [2..]) ++
         map (\ (name, varId) -> CVar(varId,("possible"++name++"s")))
             (zip (manyToOneEntities++manyToManyEntities) [2..]) )

  storeFun =
    let evar   = (1, lowerFirst entityName ++ "ToEdit")
        entvar = (2, "entity")
    in
    CLambda [CPVar (0,"_"),
             CPAs entvar
               (tuplePattern
                  ([CPVar evar] ++ 
                   map (\i -> CPVar (i+2,"_"))
                       [1 .. length manyToManyEntities]))]
      (applyF checkAuthorizationFunc
         [applyF (enauthModName,lowerFirst entityName ++ "OperationAllowed")
            [applyF (authorizationModule,"UpdateEntity") [CVar evar]],
          CLambda [CPVar (0,"_")]
            (applyF (spiceyModule,"transactionController")
               [applyF (model erdname,"runT")
                  [applyF (transFunctionName entityName "update")
                           [CVar entvar]],
                applyF (pre "const")
                  [doExpr
                     [CSExpr $ applyF (spiceyModule,"setPageMessage")
                                      [string2ac $ entityName ++ " updated"],
                      CSExpr $ applyF (spiceyModule,"nextInProcessOr")
                        [applyF (spiceyModule,"redirectController")
                           [applyF (spiceyModule,"showRoute") [CVar evar]],
                         constF (pre "Nothing")]]]])])

  renderFun =
    CLambda [tuplePattern
               (map CPVar (sinfovar : entvar :
                           map (\v -> (v,"_")) [3 .. arity1]))] $
      applyF (spiceyModule,"renderWUI")
        [CVar sinfovar,
         string2ac $ "Edit " ++ entityName,
         string2ac "Change",
         applyF (spiceyModule,"listRoute") [CVar entvar],
         constF (pre "()")
        ]
   where sinfovar = (1, "sinfo")
         entvar   = (2, "entity")


--- Generates the store for WUI to edit an entity.
editStore :: String -> Entity -> [Relationship] -> [Entity] -> CFuncDecl
editStore erdname entity@(Entity entityName _) relationships allEntities =
  cmtfunc "The data stored for executing the edit WUI form."
    (controllerStoreName entityName "edit") 0
    Private
    (emptyClassType $
       applyTC (sessionModule "SessionStore")
         [editTupleType erdname entity relationships allEntities])
    [simpleRule []
      (applyF (sessionModule "sessionStore")
              [string2ac $ "edit" ++ entityName ++ "Store"])]

--- Computes the tuple type of the data to be stored and manipulated
--- by the WUI to edit a new entity.
editTupleType :: String -> Entity -> [Relationship] -> [Entity] -> CTypeExpr
editTupleType erdname (Entity entityName attrlist) relationships allEntities =
  tupleType
    [tupleType $
      [userSessionInfoType, baseType (model erdname, entityName)] ++
      map ctvar manyToOneEntities ++ -- defaults for n:1
      map (\e -> listType (ctvar e))
          (manyToOneEntities ++ manyToManyEntities), -- possible values
     applyTC (wuiModule "WuiStore")
      [tupleType $
         [baseType (model erdname, entityName)] ++
         map (\name -> listType (ctvar name)) manyToManyEntities]]
 where
  manyToManyEntities = manyToMany allEntities (Entity entityName attrlist)
  manyToOneEntities  = manyToOne (Entity entityName attrlist) relationships

--- Generates the transaction to update an entity.
updateTransaction :: ControllerGenerator
updateTransaction erdname (Entity entityName attrList) _ allEntities =
  stCmtFunc
    ("Transaction to persist modifications of a given " ++ entityName ++
     " entity\nto the database.")
    (transFunctionName entityName "update")
    2 Private
    (tupleType ([baseType (model erdname, entityName)] ++
                 map (\name -> listType (ctvar name)) manyToManyEntities)
      ~> applyTC (dbconn "DBAction") [unitType])
    [simpleRule 
      [tuplePattern
             ([CPVar (0, lowerFirst entityName)] ++
              (map (\ (param, varId) -> CPVar (varId, param)) 
                   (zip (map (\e -> lowerFirst e ++ "s" ++
                                    linkTableName entityName e allEntities)
                             manyToManyEntities)
                        [1..])))
      ] -- parameter list for controller
      (doExpr $
         (CSExpr (applyF (model erdname, "update" ++ entityName)
                         [cvar (lowerFirst entityName)]) :
         (concatMap (\name ->
            [CSPat (CPVar (0, "old" ++ linkTableName entityName name allEntities ++ name ++ "s"))
                   (applyF (controllerModuleName entityName,
                            "get" ++ entityName ++ name ++ "s")
                           [cvar (lowerFirst entityName)]),
             CSExpr (applyF (controllerModuleName entityName,
                             "remove" ++ linkTableName entityName name allEntities)
                            [cvar ("old"++ linkTableName entityName name allEntities ++ name ++ "s"),
                             cvar (lowerFirst entityName)])])
               manyToManyEntities
                      ) ++
         (map (\name -> CSExpr $
                  applyF (controllerModuleName entityName,
                          "add" ++ linkTableName entityName name allEntities)
                         [cvar (lowerFirst name ++ "s" ++ linkTableName entityName name allEntities),
                          cvar (lowerFirst entityName)])
              manyToManyEntities)
         ))]
 where
  manyToManyEntities = manyToMany allEntities (Entity entityName attrList)
  -- manyToOneEntities = manyToOne (Entity entityName attrList) relationships
  -- noPKeys = (filter notPKey attrList)

------------------------------------------------------------------------------
--- Generates controller to delete an entity after confirmation.
deleteController :: ControllerGenerator
deleteController erdname (Entity entityName _) _ _ =
  let entlc    = lowerFirst entityName  -- entity name in lowercase
      entvar   = (0, entlc)             -- entity parameter for controller
      sinfovar = (1, "sinfo")           -- "sinfo" parameter
  in
  controllerFunction
  ("Deletes a given "++entityName++" entity (after asking for confirmation)\n"++
   "and proceeds with the list controller.")
  entityName "delete" 1
  (baseType (model erdname, entityName) ~> controllerType)
  [simpleRule [CPVar entvar]
    (applyF (pre "$")
       [applyF checkAuthorizationFunc
         [applyF (enauthModName,entlc++"OperationAllowed")
                 [applyF (authorizationModule,"DeleteEntity") [CVar entvar]]],
        CLambda [CPVar sinfovar] $
         applyF (spiceyModule,"confirmDeletionPage")
                [CVar sinfovar,
                 applyF (pre "concat")
                   [list2ac [string2ac "Really delete entity \"",
                             applyF (entitiesToHtmlModule erdname,
                                     entlc ++ "ToShortView")
                                    [CVar entvar],
                             string2ac "\"?"]]]])]

--- Generates controller to delete an entity.
destroyController :: ControllerGenerator
destroyController erdname (Entity entityName _) _ _ =
  let entlc  = lowerFirst entityName  -- entity name in lowercase
      entvar = (0, entlc)             -- entity parameter for controller
      listEntityURL = '?' : entityName ++ "/list"
  in
  controllerFunction
  ("Deletes a given " ++ entityName ++ " entity\n" ++
   "and proceeds with the list controller.")
  entityName "destroy" 1
  (baseType (model erdname, entityName) ~> controllerType)
  [simpleRule [CPVar entvar]
    (applyF (pre "$")
       [applyF checkAuthorizationFunc
         [applyF (enauthModName,entlc++"OperationAllowed")
                 [applyF (authorizationModule,"DeleteEntity") [CVar entvar]]],
        CLambda [CPVar (0,"_")] $
         applyF (spiceyModule,"transactionController")
            [applyF (model erdname,"runT")
                    [applyF (transFunctionName entityName "delete")
                            [CVar entvar]],
             applyF (pre "const")
               [doExpr
                  [CSExpr $ applyF (spiceyModule,"setPageMessage")
                                   [string2ac $ entityName ++ " deleted"],
                   CSExpr $ applyF (spiceyModule,"redirectController")
                                   [applyF (spiceyModule,"listRoute")
                                           [CVar entvar]]]]]])]

--- Generates a transaction to delete an entity.
deleteTransaction :: ControllerGenerator
deleteTransaction erdname (Entity entityName attrList) _ allEntities =
  let manyToManyEntities = manyToMany allEntities (Entity entityName attrList)
      entlc  = lowerFirst entityName  -- entity name in lowercase
      entvar = (0, entlc)             -- entity parameter for trans.
  in
   stCmtFunc
    ("Transaction to delete a given " ++ entityName ++ " entity.")
    (transFunctionName entityName "delete")
    1 Private
    (baseType (model erdname, entityName)
      ~> applyTC (dbconn "DBAction") [unitType])
    [simpleRule 
      [CPVar entvar] -- entity parameter for controller
      (doExpr $
         concatMap (\name ->
           [CSPat (CPVar(0, "old" ++ (linkTableName entityName name allEntities) ++ name ++ "s"))
                  (applyF (controllerModuleName entityName,
                           "get" ++ entityName ++ name ++ "s")
                          [CVar entvar]),
            CSExpr (applyF (controllerModuleName entityName,
                            "remove" ++ (linkTableName entityName name allEntities))
                           [cvar ("old" ++ (linkTableName entityName name allEntities) ++ name ++ "s"),
                            CVar entvar ])]
           )
           manyToManyEntities ++
         [CSExpr $ applyF (model erdname, "delete" ++ entityName)
                          [CVar entvar]])]

------------------------------------------------------------------------------
listController :: ControllerGenerator
listController erdname (Entity entityName _) _ _ =
  controllerFunction
    ("Lists all " ++ entityName ++ " entities with buttons to show, delete,\n"++
     "or edit an entity.")
    entityName "list" 0
    controllerType
    [simpleRule [] -- no arguments
      (applyF (pre "$")
          [applyF checkAuthorizationFunc
            [applyF (enauthModName,lowerFirst entityName ++ "OperationAllowed")
              [applyF (authorizationModule,"ListEntities") []]],
           CLambda [CPVar infovar] $ doExpr
            [CSPat (CPVar entsvar)
                   (applyF (model erdname,"runQ")
                      [constF (model erdname,"queryAll" ++ entityName ++ "s")]),
             CSExpr (applyF (pre "return")
                           [applyF (viewFunctionName entityName "list")
                                   [CVar infovar, CVar entsvar]])
            ]
         ]
        )]
 where
  infovar = (0, "sinfo")
  entsvar = (1, lowerFirst entityName ++ "s")

------------------------------------------------------------------------------
showController :: ControllerGenerator
showController erdname (Entity entityName attrList) relationships allEntities =
  let manyToManyEntities = manyToMany allEntities (Entity entityName attrList)
      manyToOneEntities  = manyToOne (Entity entityName attrList) relationships
      pvar               = (0, lowerFirst entityName)
      infovar            = (1, "sinfo")
  in
    controllerFunction
    ("Shows a " ++ entityName ++ " entity.")
    entityName "show" 1
      (baseType (model erdname,entityName) ~> controllerType)
      [simpleRule 
        [CPVar pvar] -- parameterlist for controller
        (applyF (pre "$")
            [applyF checkAuthorizationFunc
              [applyF (enauthModName,lowerFirst entityName ++ "OperationAllowed")
                [applyF (authorizationModule,"ShowEntity") [CVar pvar]]],
             CLambda [CPVar infovar] $ doExpr (
               map (\ (ename, num) ->
                     CSPat (CPVar (num,lowerFirst
                                         (fst $ relationshipName entityName
                                               ename relationships) ++ ename)) 
                           (applyF (model erdname,"runJustT")
                              [applyF (controllerModuleName entityName,
                                       "get"++ fst (relationshipName
                                                entityName ename relationships)
                                            ++ ename)
                                      [CVar pvar]
                              ])
                   )
                   (zip (manyToOneEntities) [1..]) ++
               map (\ (ename, num) ->
                      CSPat (CPVar (num,lowerFirst (linkTableName entityName
                                                           ename allEntities)
                                        ++ename ++ "s"))
                            (applyF (model erdname,"runJustT")
                               [applyF (controllerModuleName entityName,
                                        "get" ++ entityName ++ ename ++ "s")
                                       [CVar pvar]])
                   )
                   (zip (manyToManyEntities) [1..]) ++
              [CSExpr (
                 applyF (pre "return")
                    [applyF (viewFunctionName entityName "show")
                       ([CVar infovar, CVar pvar] ++
                        (map (\ (ename, num) ->
                                CVar (num,lowerFirst (fst $ relationshipName
                                             entityName ename relationships)
                                           ++ ename))
                             (zip (manyToOneEntities) [1..])) ++
                        (map (\ (ename, num) ->
                               CVar (num,lowerFirst (linkTableName entityName
                                                       ename allEntities)
                                         ++ename ++ "s"))
                             (zip (manyToManyEntities) [1..])))
                    ])
              ])
            ]
          )
      ]

manyToManyAddOrRemove :: String -> Entity -> [String] -> [Entity] -> [CFuncDecl]
manyToManyAddOrRemove erdname (Entity entityName _) entities allEntities =
    (map (addOrRemoveFunction "add" "new" entityName) entities) ++
    (map (addOrRemoveFunction "remove" "delete" entityName) entities)
  where
    addOrRemoveFunction :: String -> String -> String -> String -> CFuncDecl
    addOrRemoveFunction funcPrefix dbFuncPrefix e1 e2 =      
      stCmtFunc 
      (if (funcPrefix == "add")
        then ("Associates given entities with the " ++ entityName ++ " entity.")
        else ("Removes association to the given entities with the " ++ entityName ++ " entity."))
      (controllerModuleName e1, funcPrefix ++ (linkTableName e1 e2 allEntities))
      2 
      Private
      (listType (ctvar e2) ~> ctvar e1 ~> applyTC (dbconn "DBAction")
                                                 [tupleType []])
      [simpleRule [CPVar (0, (lowerFirst e2) ++ "s"), CPVar (1, (lowerFirst e1))]
        (applyF (pre "mapM_")
           [CLambda [CPVar(2, "t")]
             (applyF (model erdname,
                      dbFuncPrefix ++ linkTableName e1 e2 allEntities)
               [applyF (model erdname, lowerFirst e1 ++ "Key")
                       [cvar (lowerFirst e1)],
                applyF (model erdname, lowerFirst e2 ++ "Key") [cvar "t"]]),
            cvar ((lowerFirst e2) ++ "s")])]

getAll :: String -> Entity -> [String] -> [Entity] -> [CFuncDecl]
getAll erdname (Entity entityName _) entities _ =
    map getAllFunction entities
  where
    getAllFunction :: String -> CFuncDecl
    getAllFunction foreignEntity =
      stCmtFunc 
      ("Gets all " ++ foreignEntity ++ " entities.")
      (controllerModuleName entityName, "getAll" ++ foreignEntity ++ "s")
      0
      Private
      (ioType (listType (ctvar foreignEntity)))
      [simpleRule []
        (applyF (model erdname,"runQ")
          [applyF (model erdname,"queryAll")
            [CLambda [CPVar(0, take 1 (lowerFirst foreignEntity) )]
                     (CLetDecl [(CLocalVars [(1,"key")])]
                        (applyF (model erdname, lowerFirst foreignEntity)
                                [cvar "key",
                                 cvar (take 1 (lowerFirst foreignEntity))]))
                    ]
            ]
       )
      ]
      
manyToManyGetRelated :: String -> Entity -> [String] -> [Entity] -> [CFuncDecl]
manyToManyGetRelated erdname (Entity entityName _) entities allEntities =      
    map getRelatedFunction entities
  where
    getRelatedFunction :: String -> CFuncDecl
    getRelatedFunction foreignEntity =
      stCmtFunc 
      ("Gets the associated " ++ foreignEntity ++ " entities for a given " ++ entityName ++ " entity.")
      (controllerModuleName entityName, "get" ++ (linkTableName entityName foreignEntity allEntities) ++ foreignEntity ++ "s")
      0
      Private
      (ctvar entityName ~> applyTC (dbconn "DBAction")
                                   [listType (ctvar foreignEntity)])
      [simpleRule [CPVar (1, (take 1 $ lowerFirst entityName) ++ foreignEntity)]
        (applyF (model erdname,"queryAll")
          [CLambda [CPVar(0, take 1 (lowerFirst foreignEntity) )] 
            (CLetDecl
               [CLocalVars [(1,(take 1 $ lowerFirst entityName) ++ "key"),
                            (2,(take 1 $ lowerFirst foreignEntity) ++ "key")]]
               (foldr (\a b -> applyF ("Dynamic", "<>") [a,b]) 
                 (applyF (model erdname, lowerFirst (linkTableName entityName foreignEntity allEntities)) [cvar ((take 1 $ lowerFirst entityName) ++ "key"), cvar ((take 1 $ lowerFirst foreignEntity) ++ "key")])
                 [
                 (applyF (model erdname, lowerFirst entityName) [cvar $ (take 1 $ lowerFirst entityName) ++ "key", cvar ((take 1 $ lowerFirst entityName) ++ foreignEntity)]),
                 (applyF (model erdname, lowerFirst foreignEntity) [cvar $ (take 1 $ lowerFirst foreignEntity) ++ "key", cvar (take 1 (lowerFirst foreignEntity))])
                 ]
               )
            )
          ]
        )
      ]

manyToOneGetRelated :: String -> Entity -> [String] -> [Entity]
                    -> [Relationship] -> [CFuncDecl]
manyToOneGetRelated erdname (Entity entityName _) entities _ relationships =      
    map getRelatedFunction entities
  where
    getRelatedFunction :: String -> CFuncDecl
    getRelatedFunction foreignEntity =
      let argvar  = (1, (take 1 $ lowerFirst entityName) ++ foreignEntity)
          rname   = fst (relationshipName entityName foreignEntity relationships)
          fkeysel = lowerFirst entityName ++ foreignEntity ++ rname ++ "Key"
      in
      stCmtFunc 
      ("Gets the associated " ++ foreignEntity ++ " entity for a given "++
       entityName ++ " entity.")
      (controllerModuleName entityName,
       "get" ++ rname ++ foreignEntity)
      0
      Private
      ((ctvar entityName) ~> applyTC (dbconn "DBAction") [ctvar foreignEntity])
      [simpleRule [CPVar argvar]
                  (applyF (model erdname,"get" ++ foreignEntity)
                          [applyF (model erdname,fkeysel) [CVar argvar]])]

relationshipName :: String -> String -> [Relationship] -> (String, String)
relationshipName e1 e2 (rel:relrest)=
  case rel of
    (Relationship name [(REnd relE1 _ _), (REnd relE2 relName _)]) ->
      if ((relE1 == e1 && relE2 == e2) || (relE1 == e2 && relE2 == e1))
        then (name, relName)
        else relationshipName e1 e2 relrest
relationshipName _ _ [] = error "relationshipName: relationship not found"

---- auxiliaries ---

displayErrorFunction :: QName
displayErrorFunction = (spiceyModule, "displayError")

entityConstructorFunction :: String -> Entity -> [Relationship] -> QName
entityConstructorFunction erdname (Entity entityName attrList) relationships =
  (model erdname, "new" ++ 
    entityName ++ (newSuffix entityName attrList relationships)
  )

-- entityName: Name of entity the controller should be generated for
-- controllerType: the function of the generated Controller, e.g. "new", "edit", "list"
-- arity
-- functionType: the type of the controller function
-- rules: the rules defining the controller
controllerFunction :: String -> String -> String -> Int -> CTypeExpr -> [CRule]
                   -> CFuncDecl
controllerFunction description entityName controllerType arity functionType
                   rules =
  stCmtFunc description (controllerFunctionName entityName controllerType) arity
          (if controllerType `elem` ["main"]
           then Public
           else Private)
          functionType rules

getReferencedEntityName :: Domain -> String
getReferencedEntityName t =
  case t of KeyDom kd -> kd
            _         -> ""
            
relatedEntityNames :: Entity -> [Relationship] -> [String]
relatedEntityNames (Entity entityName attrlist) relationships =
  map (\(Relationship _ ((REnd name1 _ _):(REnd name2 _ _):[])) -> if (name1 == entityName) then name2 else name1) (relationshipsForEntity (Entity entityName attrlist) relationships)

-- gets all relationships 
relationshipsForEntity :: Entity -> [Relationship] -> [Relationship]
relationshipsForEntity (Entity entityName _) relationships =
  filter (\(Relationship _ ((REnd name1 _ _):(REnd name2 _ _):[])) -> name1 == entityName || name2 == entityName) (filter (not . isGeneratedR) relationships)
    
------ from ERD CodeGeneration

newSuffix :: String -> [Attribute] -> [Relationship] -> String
newSuffix eName attrs rels = 
  let
    generatedRs = filter isGeneratedR rels
    exactRs  = filter isExactB  generatedRs --(i,i), i>1
    maxRs    = filter isMaxB    generatedRs --(0,i), i>1
    minMaxRs = filter isMinMaxB generatedRs --(i,j), i>0, j>i
  in
    concatMap ("With"++)
              (map attributeName (filter isForeignKey attrs)) ++
    if (length (exactRs ++ maxRs ++ minMaxRs))==0
    then ""
    else concatMap (\k->"With" ++ k ++ "Keys")
                   (map (relatedRelation eName)
                        (exactRs ++ maxRs ++ minMaxRs))
  where
    isExactB (Relationship _ [REnd _ _ _, REnd _ _ c]) =
      case c of Exactly i -> i>1
                _         -> False
    isMaxB (Relationship _ [REnd _ _ _, REnd _ _ c]) =
      case c of (Between 0 (Max i)) -> i>1
                _                   -> False
    isMinMaxB (Relationship _ [REnd _ _ _, REnd _ _ c]) =
      case c of (Between i (Max j)) -> i>0 && j>i
                _                   -> False

isGeneratedR :: Relationship -> Bool
isGeneratedR (Relationship n _) = n == ""

-- extracts the name of the relationship related to a given entity name
relatedRelation :: String -> Relationship -> String
relatedRelation en (Relationship _ [REnd en1 _ _, REnd en2 _ _]) =
  if en==en1 then en2 else en1

relationshipsForEntityName :: String -> [Relationship] -> [Relationship]
relationshipsForEntityName ename rels = filter endsIn rels
 where
  endsIn (Relationship _ ends) = any (\ (REnd n _ _) -> ename == n) ends

------------------------------------------------------------------------
-- Auxiliaries:

getUserSessionInfoFunc :: CExpr
getUserSessionInfoFunc = constF (sessionInfoModule,"getUserSessionInfo")

checkAuthorizationFunc :: QName
checkAuthorizationFunc = (authorizationModule,"checkAuthorization")

------------------------------------------------------------------------
