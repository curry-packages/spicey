module Spicey.ViewGeneration where

import AbstractCurry.Types
import AbstractCurry.Build
import Database.ERD
import Database.ERDGoodies

import Spicey.GenerationHelper

-- "main"-function
generateViewsForEntity :: String -> [Entity] -> Entity -> [Relationship]
                       -> CurryProg
generateViewsForEntity erdname allEntities
                       (Entity ename attrlist) relationships =
 let noKeyAttrs  = filter (\a -> notKey a && notPKey a) attrlist
     noPKeyAttrs = filter notPKey attrlist
  in simpleCurryProg
  (viewModuleName ename)
  ["WUI", "HTML", "Time", "Sort", "Bootstrap3Style", "Spicey", "SessionInfo",
   erdname, erdname++"EntitiesToHtml"] -- imports
  [] -- typedecls
  -- functions
  [
   wuiSpec      erdname (Entity ename noKeyAttrs) relationships allEntities,
   tuple2Entity erdname (Entity ename noPKeyAttrs) relationships allEntities,
   entity2Tuple erdname (Entity ename noPKeyAttrs) relationships allEntities,
   wuiType      erdname (Entity ename noKeyAttrs) relationships allEntities,
   blankView    erdname (Entity ename noKeyAttrs) relationships allEntities,
   createView   erdname (Entity ename noKeyAttrs) relationships allEntities,
   editView     erdname (Entity ename noKeyAttrs) relationships allEntities,
   showView     erdname (Entity ename noKeyAttrs) relationships allEntities,
   leqEntity    erdname (Entity ename noKeyAttrs) relationships allEntities,
   listView     erdname (Entity ename noKeyAttrs) relationships allEntities
  ]  
  [] -- opdecls
  
  
type ViewGenerator = String -> Entity -> [Relationship] -> [Entity] -> CFuncDecl

wuiSpec :: ViewGenerator
wuiSpec erdname (Entity entityName attrlist) relationships allEntities =
  let
    manyToManyEntities = manyToMany allEntities (Entity entityName attrlist)
    manyToOneEntities  = manyToOne (Entity entityName attrlist) relationships
    argumentCount = length attrlist + length manyToOneEntities
                    + length manyToManyEntities
  in
    stCmtFunc 
    ("The WUI specification for the entity type "++entityName++".\n"++
     if null (manyToOneEntities ++ manyToManyEntities)
     then ""
     else "It also includes fields for associated entities.")
    (viewModuleName entityName, "w"++entityName) 2 Public
    (foldr CFuncType
           (applyTC ("WUI", "WuiSpec")
               [entityInterface attrlist manyToOneEntities manyToManyEntities])
           (map (\e -> listType (ctvar e))
                (manyToOneEntities ++ manyToManyEntities))-- possible values
    )
    [simpleRule (map (\e -> CPVar (1, lowerFirst $ e ++ "List"))
                     (manyToOneEntities ++ manyToManyEntities))
       (applyF ("WUI", "withRendering") [         
            (if (argumentCount == 1) then
              head (attrWidgets attrlist)
            else
              applyF (combinator argumentCount) 
              (
                (attrWidgets attrlist) ++
                (map (\e -> applyF (wui "wSelect")
                              [constF (erdname, lowerFirst e++"ToShortView"),
                               CVar (1, lowerFirst $ e ++ "List")])
                     manyToOneEntities) ++
                (map (\e -> 
                  applyF (wui "wMultiCheckSelect")
                   [CLambda [CPVar (1, lowerFirst e)]
                      (list2ac [
                        applyF ("HTML", "htxt") [
                         applyF (erdname, lowerFirst e++"ToShortView")
                                [CVar (1, lowerFirst e)]
                         ]]),
                    CVar (1, lowerFirst $ e ++ "List")
                  ]) manyToManyEntities)
              )
            ),
            applyF ("Spicey", "renderLabels")
                   [constF (erdname++"EntitiesToHtml",
                            lowerFirst entityName++"LabelList")]
          ]
        )]


tuple2Entity :: ViewGenerator
tuple2Entity erdname (Entity entityName attrlist) relationships allEntities =
  let
    manyToManyEntities = manyToMany allEntities (Entity entityName attrlist)
    manyToOneEntities = manyToOne (Entity entityName attrlist) relationships
  in
    stCmtFunc 
    ("Transformation from data of a WUI form to entity type "++entityName++".")
    (viewModuleName entityName, "tuple2"++entityName) 2 Public
    (
     foldr CFuncType
      (if null manyToManyEntities
       then baseType (erdname, entityName)
       else tupleType ([ctvar entityName] ++
                       map (\name -> listType (ctvar name)) manyToManyEntities)
      )
      ([ctvar entityName] ++
       [entityInterface (filter notKey attrlist)
                        manyToOneEntities manyToManyEntities])
    )
    [simpleRule
      ( 
        [ CPVar (1, lowerFirst entityName ++ "ToUpdate"),
          tuplePattern
           (
            (map (\ ((Attribute name _ _ _), varId) ->
                       CPVar (varId,lowerFirst name)))
                 (zip (filter notKey attrlist) [1..]) ++
            (map (\ (name, varId) -> CPVar(varId,lowerFirst name))
                 (zip manyToOneEntities [1..])) ++
            (map (\ (name, varId) -> CPVar(varId,lowerFirst $ name++"s"))
                 (zip manyToManyEntities [1..]))
           )
        ]
      )
      (tupleExpr $
         (foldr (\ (Attribute aname domain _ _) expr ->
                   case domain of
                     (KeyDom rel) ->
                       applyF (erdname, "set"++entityName++aname)
                              [expr,
                               applyF (erdname, (lowerFirst rel)++"Key")
                                      [CVar (1, lowerFirst rel)]]
                     _ -> applyF (erdname, "set"++entityName++aname)
                                 [expr,  CVar (1, lowerFirst aname)]  )
                
                 (CVar (0, lowerFirst $ entityName++"ToUpdate"))
                 attrlist )
            : (map (\e -> cvar (lowerFirst $ e ++ "s")) manyToManyEntities))]


entity2Tuple :: ViewGenerator
entity2Tuple erdname (Entity entityName attrlist) relationships allEntities =
  let
    manyToManyEntities = manyToMany allEntities (Entity entityName attrlist)
    manyToOneEntities = manyToOne (Entity entityName attrlist) relationships
  in
    stCmtFunc
    ("Transformation from entity type "++entityName++" to a tuple\n"++
     "which can be used in WUI specifications.")
    (viewModuleName entityName, (lowerFirst entityName)++"2Tuple") 2 Public
    (
      foldr (CFuncType)
      (entityInterface (filter notKey attrlist)
                       manyToOneEntities manyToManyEntities)
      (
        (map ctvar manyToOneEntities) ++
        
        [(
          if null manyToManyEntities
          then baseType (erdname, entityName)
          else
            tupleType ([ctvar entityName] ++
                       map (\name -> listType (ctvar name)) manyToManyEntities)
        )]
      )
    )
    [simpleRule
      ( 
        (map (\ (name, varId) -> CPVar(varId,(lowerFirst name)))
             (zip manyToOneEntities [1..])) ++
        [
         tuplePattern
          (
            CPVar (1, lowerFirst entityName) :
            (map (\ (name, varId) -> CPVar(varId,(lowerFirst $ name++"s")))
                 (zip manyToManyEntities [1..]))
          )
        ]
      )
      (tupleExpr
            (map (\ (Attribute a _ _ _) ->
                     applyF (erdname, (lowerFirst entityName)++a)
                            [cvar (lowerFirst entityName)])
                  (filter notKey attrlist) ++
             map (\e -> cvar (lowerFirst e)) manyToOneEntities ++
             map (\e -> cvar (lowerFirst $ e ++ "s")) manyToManyEntities)
        )]

wuiType :: ViewGenerator
wuiType _ (Entity entityName attrlist) relationships allEntities =
  let
    manyToManyEntities = manyToMany allEntities (Entity entityName attrlist)
    manyToOneEntities  = manyToOne (Entity entityName attrlist) relationships
  in
    stCmtFunc 
    ("WUI Type for editing or creating "++entityName++" entities.\n"++
     "Includes fields for associated entities.")
    (viewModuleName entityName, "w"++entityName++"Type") 2 Public
    (
      foldr CFuncType
      (applyTC ("WUI", "WuiSpec") [
        if null manyToManyEntities
        then ctvar entityName
        else
          tupleType ([ctvar entityName] ++
                     map (\name -> listType (ctvar name)) manyToManyEntities)
      ])
      (
        [ctvar entityName] ++
        (map (\e -> ctvar e) (manyToOneEntities)) ++ -- related values
        --(map (\e -> listType (ctvar e)) manyToManyEntities) ++ -- related values
        (map (\e -> listType (ctvar e))
             (manyToOneEntities ++ manyToManyEntities))-- possible values
      )
    )
    [simpleRule
      (
        [CPVar (1, lowerFirst entityName)] ++
        (map (\e -> CPVar (1, lowerFirst e)) manyToOneEntities) ++ -- related values
        --(map (\e -> CPVar (1, lowerFirst $ e++"s")) (manyToManyEntities)) ++ -- related values
        (map (\e -> CPVar (1, lowerFirst $ e++"List"))
             (manyToOneEntities ++ manyToManyEntities))
      )
      (applyF (wui "transformWSpec") [
            tupleExpr
            [
             applyF (viewModuleName entityName, "tuple2"++entityName)
                    [cvar (lowerFirst entityName)],
             applyF (viewModuleName entityName,lowerFirst entityName++"2Tuple")
                    (map (\e -> CVar (1, lowerFirst e)) (manyToOneEntities))
            ],
            applyF (viewModuleName entityName, "w"++entityName)
                   (map (\e -> CVar (1, lowerFirst $ e ++ "List"))
                        (manyToOneEntities ++ manyToManyEntities))
          ]
        )]


createView :: ViewGenerator
createView _ (Entity entityName attrlist) relationships allEntities =
  let
    manyToManyEntities = manyToMany allEntities (Entity entityName attrlist)
    manyToOneEntities  = manyToOne (Entity entityName attrlist) relationships
    infovar            = (0, "_")
  in
    viewFunction 
      ("Supplies a WUI form to create a new "++entityName++" entity.\n"++
       "Takes default values to be prefilled in the form fields.")
      entityName "create" 3
      ( -- function type
       userSessionInfoType ~>
       (foldr CFuncType viewBlockType (
          (map attrType attrlist) ++
          (map ctvar manyToOneEntities) ++ -- defaults for n:1
          (map (\e -> listType (ctvar e)) manyToManyEntities) ++ -- defaults for n:m
          (map (\e -> listType (ctvar e)) (manyToOneEntities ++ manyToManyEntities)) ++ -- possible values
          [entityInterface attrlist manyToOneEntities manyToManyEntities
           ~> controllerType,
           controllerType]))
      )
      [simpleRule
        ( -- params
          CPVar infovar :
          (map (\ ((Attribute name _ _ _), varId) ->
                        CPVar(varId,("default"++name)))
               (zip attrlist [1..])) ++
          (map (\ (name, varId) -> CPVar(varId,("default"++name)))
               (zip manyToOneEntities [1..])) ++
          (map (\ (name, varId) -> CPVar(varId,("default"++name++"s")))
               (zip manyToManyEntities [1..])) ++
          (map (\ (name, varId) -> CPVar(varId,("possible"++name++"s")))
               (zip (manyToOneEntities++manyToManyEntities) [1..])) ++
          [CPVar (100, "controller"), CPVar (101, "cancelcontroller")]
        )
        (applyF ("Spicey","renderWuiForm")
           [applyF (viewModuleName entityName, "w"++entityName)
              (map (\ (name, varId) -> CVar (varId,("possible"++name++"s")))
                   (zip (manyToOneEntities++manyToManyEntities) [1..])),
            tupleExpr (
                    (map (\ ((Attribute name _ _ _), varId) ->
                                CVar(varId,("default"++name)))
                         (zip attrlist [1..])) ++
                    (map (\ (name, varId) -> CVar(varId,("default"++name)))
                         (zip manyToOneEntities [1..])) ++
                    (map (\ (name, varId) -> CVar(varId,"default"++name++"s"))
                         (zip manyToManyEntities [1..]))),
            CVar (100, "controller"),
            CVar (101, "cancelcontroller"),
            string2ac ("Create new "++entityName),
            string2ac "create"]
          )]
      
editView :: ViewGenerator
editView erdname (Entity entityName attrlist) relationships allEntities =
  let
    manyToManyEntities = manyToMany allEntities (Entity entityName attrlist)
    manyToOneEntities  = manyToOne (Entity entityName attrlist) relationships
    infovar            = (0, "_")
  in
    viewFunction 
      ("Supplies a WUI form to edit the given "++entityName++" entity.\n"++
       "Takes also associated entities and a list of possible associations\n"++
       "for every associated entity type.")
      entityName "edit" 3
      ( -- function type
       userSessionInfoType ~>
       (foldr CFuncType viewBlockType (
          [tupleType ([baseType (erdname, entityName)] ++
                      map (\name -> listType (ctvar name)) manyToManyEntities)
          ] ++
          (map ctvar manyToOneEntities) ++ -- defaults for n:1
          (map (\e -> listType (ctvar e))
               (manyToOneEntities ++ manyToManyEntities)) ++ -- possible values
          [tupleType ([baseType (erdname, entityName)] ++
                       map (\name -> listType (ctvar name)) manyToManyEntities)
           ~> controllerType,
           controllerType]))
      )
      [simpleRule
        ( -- params
         CPVar infovar :
         [tuplePattern
             ([CPVar (1, lowerFirst entityName)] ++
              (map (\name -> CPVar (1, lowerFirst (name++"s")))
                   manyToManyEntities)
             )] ++
          (map (\ (name, varId) -> CPVar(varId,("related"++name)))
               (zip manyToOneEntities [2..])) ++
          (map (\ (name, varId) -> CPVar(varId,("possible"++name++"s")))
               (zip (manyToOneEntities++manyToManyEntities) [2..])) ++
          [CPVar (1, "controller"), CPVar (102, "cancelcontroller")]
        )
        (applyF ("Spicey","renderWuiForm")
             [applyF (viewModuleName entityName, "w"++entityName++"Type") (
               [cvar (lowerFirst entityName)] ++
                --(map (\ (name, varId) -> CVar(varId,((lowerFirst name)++"s"))) (zip manyToManyEntities [2..])) ++
               (map (\ (name, varId) -> CVar(varId,("related"++name)))
                    (zip manyToOneEntities [2..])) ++
               (map (\ (name, varId) -> CVar(varId,("possible"++name++"s")))
                    (zip (manyToOneEntities++manyToManyEntities) [2..]))
               ),
              tupleExpr ([CVar (1, lowerFirst entityName)] ++
                         map (\name -> CVar (1, lowerFirst (name++"s")))
                             manyToManyEntities),
              CVar (1, "controller"),
              CVar (102, "cancelcontroller"),
              string2ac ("Edit "++entityName),
              string2ac "change"]
          )]

blankView :: ViewGenerator
blankView _ (Entity entityName attrlist) relationships allEntities =
  let
    manyToManyEntities = manyToMany allEntities (Entity entityName attrlist)
    manyToOneEntities  = manyToOne (Entity entityName attrlist) relationships
    withCTime          = hasCalendarTimeAttribute attrlist
    infovar            = (0, "sinfo")
  in
    viewFunction
      ("Supplies a WUI form to create a new "++entityName++" entity.\n"++
       "The fields of the entity have some default values.")
      entityName "blank" 3
      ( -- function type
       userSessionInfoType ~>
       foldr CFuncType viewBlockType (
          (if withCTime then [baseType ("Time","CalendarTime")] else []) ++
          (map (\e -> listType (ctvar e))
               (manyToOneEntities ++ manyToManyEntities)) ++ -- possible values
          [entityInterface attrlist manyToOneEntities manyToManyEntities
           ~> controllerType,
           controllerType])
      )
      [simpleRule
        ( -- params
         CPVar infovar :
         (if withCTime then [CPVar (0,"ctime")] else []) ++
         (map (\ (name, varId) -> CPVar(varId,("possible"++name++"s")))
              (zip (manyToOneEntities++manyToManyEntities) [2..])) ++
         [CPVar (1, "controller"), CPVar (2, "cancelcontroller")]
        )
        (applyF (viewFunctionName entityName "create") 
              (CVar infovar :
               (attrDefaultValues (CVar (0,"ctime")) attrlist) ++
               (map (\ (name, varId) ->
                           applyF (pre "head")
                                  [CVar (varId,("possible"++name++"s"))])
                    (zip manyToOneEntities [2..])) ++
               (map (\_ -> list2ac []) (zip manyToManyEntities [2..])) ++
               (map (\ (name, varId) -> CVar (varId,("possible"++name++"s")))
                    (zip (manyToOneEntities++manyToManyEntities) [2..])) ++
               [CVar (1, "controller"), CVar (1, "cancelcontroller")]
              )
          )]

-- Generate function to compare to entities in lexicographic order.
leqEntity :: ViewGenerator
leqEntity erdname (Entity entityName attrlist) _ _ =
  stCmtFunc
    ("Compares two "++entityName++" entities. This order is used in the list view.")
    (viewModuleName entityName, "leq" ++ entityName) 2 Private
    -- function type
    (baseType (erdname,entityName) ~> baseType (erdname,entityName) ~>boolType)
    [let ename = lowerFirst entityName
         e1 = (1,"x1")
         e2 = (2,"x2")
      in simpleRule [CPVar e1,CPVar e2]
           (applyF (pre "<=")
                   [tupleExpr (map (\ (Attribute a _ _ _) ->
                                       applyF (erdname,ename++a) [CVar e1])
                                   (filter notKey attrlist)),
                    tupleExpr (map (\ (Attribute a _ _ _) ->
                                       applyF (erdname,ename++a) [CVar e2])
                                   (filter notKey attrlist))
                   ])]


-- generate view for showing entities
showView :: ViewGenerator
showView erdname (Entity entityName attrlist) relationships allEntities =
 let manyToManyEntities = manyToMany allEntities (Entity entityName attrlist)
     manyToOneEntities  = manyToOne (Entity entityName attrlist) relationships
     infovar            = (0, "_")
     evar               = (1, lowerFirst entityName)
  in viewFunction 
      ("Supplies a view to show the details of a "++entityName++".\n")
      entityName "show" 2
      -- function type
      (userSessionInfoType ~>
       foldr CFuncType viewBlockType (
          [baseType (erdname,entityName)] ++
          (map ctvar manyToOneEntities) ++ -- defaults for n:1
          (map (\name -> listType (ctvar name)) manyToManyEntities))
      )
      [simpleRule
        ( -- parameters
          [CPVar infovar, CPVar evar] ++
          (map (\ (name, varId) -> CPVar (varId,"related"++name))
               (zip manyToOneEntities [3..])) ++
          (map (\ (name, varId) -> CPVar (varId, lowerFirst name ++ "s"))
               (zip manyToManyEntities [(length manyToOneEntities + 3)..]))
        )
        (applyF (pre "++")
              [applyF (erdname++"EntitiesToHtml",
                       lowerFirst entityName++"ToDetailsView")
                  ([CVar evar] ++
                   map (\ (name, varId) -> CVar (varId,"related"++name))
                       (zip manyToOneEntities [3..]) ++
                   map (\ (name, varId) -> CVar (varId, lowerFirst name++"s"))
                       (zip manyToManyEntities
                            [(length manyToOneEntities + 3)..])
                  ),
               list2ac [applyF hrefButtonName
                         [string2ac ("?"++entityName++"/list"),
                          list2ac [applyF ("HTML","htxt")
                            [string2ac ("back to "++entityName++" list")]]]]
              ]
            )]
      
--- Create operation for the "list entities" view.
listView :: ViewGenerator
listView erdname (Entity entityName attrlist) _ _ =
 let infovar = (0, "sinfo")
     entsvar = (1, (lowerFirst entityName)++"s")
     envar   = (2, lowerFirst entityName)
     showkey = applyF (erdname,"show"++entityName++"Key") [CVar envar]
  in
    viewFunction 
      ("Supplies a list view for a given list of "++entityName++" entities.\n"++
       "Shows also show/edit/delete buttons if the user is logged in.\n"++
       "The arguments are the session info and the list of "++entityName++
       " entities.\n")
      entityName "list" 1
      -- function type
      (userSessionInfoType ~> listType (baseType (erdname,entityName))
                           ~> viewBlockType)
      [CRule
        [CPVar infovar, CPVar entsvar]
        (CSimpleRhs
          (applyF (pre ":") [
              applyF ("HTML", "h1")
                     [list2ac [applyF ("HTML", "htxt")
                                      [string2ac $ entityName ++ " list"]]],
              list2ac [
                applyF ("Spicey", "spTable") [
                  applyF (pre "++") [
                    list2ac [
                      applyF (pre "take") [
                        CLit (CIntc (length attrlist)),
                        constF (erdname++"EntitiesToHtml",
                                lowerFirst entityName++"LabelList")
                      ]
                    ],
                    applyF (pre "map") [
                      constF (viewModuleName entityName,"list"++entityName),
                      applyF ("Sort","mergeSortBy") [
                          constF (viewModuleName entityName,"leq"++entityName),
                          CVar entsvar
                      ]
                    ]
                  ]
                ]
              ]
            ]
          )
        [CLocalFunc (stFunc
          (viewModuleName entityName, "list"++entityName) 2 Private
          (ctvar entityName ~> listType viewBlockType)
          [simpleRule [CPVar envar]
             (applyF (pre "++") [
                applyF (erdname++"EntitiesToHtml",
                        lowerFirst entityName++"ToListView")
                       [cvar $ lowerFirst entityName],
                applyF (pre "if_then_else")
                 [applyF (pre "==")
                   [applyF ("SessionInfo","userLoginOfSession") [CVar infovar],
                    constF (pre "Nothing")],
                  list2ac [],
                  list2ac
                   [list2ac
                     [applyF hrefButtonName
                       [applyF (pre "++")
                         [string2ac ("?"++entityName++"/show/"),showkey],
                        list2ac [applyF ("HTML","htxt") [string2ac "show"]]]],
                    list2ac
                     [applyF hrefButtonName
                       [applyF (pre "++")
                         [string2ac ("?"++entityName++"/edit/"),showkey],
                        list2ac [applyF ("HTML","htxt") [string2ac "edit"]]]],
                    list2ac
                     [applyF hrefButtonName
                       [applyF (pre "++")
                         [string2ac ("?"++entityName++"/delete/"),showkey],
                        list2ac [applyF ("HTML","htxt") [string2ac "delete"]]]]
                ]]
              ]
              )])
        ])
      ]
      
-- aux

-- entityName: Name of entity the view should be generated for
-- viewType: the function of the generated View, e.g. "new", "edit", "list"
-- arity
-- functionType: the type of the view function
-- rules: the rules defining the view
viewFunction :: String -> String -> String -> Int -> CTypeExpr -> [CRule]
             -> CFuncDecl
viewFunction description entityName viewType arity functionType rules =
  stCmtFunc description (viewFunctionName entityName viewType) arity
            Public functionType rules
  
entityInterface :: [Attribute] -> [String] -> [String] -> CTypeExpr
entityInterface attrlist manyToOne manyToMany = 
  tupleType (map attrType attrlist ++
             map ctvar manyToOne ++
             map (\e -> listType (ctvar e)) manyToMany)

-- Type "UserSessionInfo"
userSessionInfoType :: CTypeExpr
userSessionInfoType = baseType ("SessionInfo","UserSessionInfo")
