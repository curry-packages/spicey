module Spicey.ViewGeneration where

import AbstractCurry.Types
import AbstractCurry.Build
import Database.ERD
import Database.ERD.Goodies

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
  [ "Sort", "Time"
  , "HTML.Base", bootstrapModule, "HTML.WUI"
  , erdname
  , "Config.EntityRoutes"
  , sessionInfoModule, spiceyModule
  , entitiesToHtmlModule erdname] -- imports
  [] -- typedecls
  -- functions
  [
   wuiSpec      erdname (Entity ename noKeyAttrs) relationships allEntities,
   tuple2Entity erdname (Entity ename noPKeyAttrs) relationships allEntities,
   entity2Tuple erdname (Entity ename noPKeyAttrs) relationships allEntities,
   wuiType      erdname (Entity ename noKeyAttrs) relationships allEntities,
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
           (applyTC (wuiModule "WuiSpec")
               [entityInterface attrlist manyToOneEntities manyToManyEntities])
           (map (\e -> listType (ctvar e))
                (manyToOneEntities ++ manyToManyEntities))-- possible values
    )
    [simpleRule (map (\e -> CPVar (1, lowerFirst $ e ++ "List"))
                     (manyToOneEntities ++ manyToManyEntities))
       (applyF (wuiModule "withRendering") [         
            (if (argumentCount == 1) then
              head (attrWidgets attrlist)
            else
              applyF (combinator argumentCount) 
               ( attrWidgets attrlist ++
                 map (\e -> applyF (wuiModule "wSelect")
                              [constF (erdname, lowerFirst e++"ToShortView"),
                               CVar (1, lowerFirst $ e ++ "List")])
                     manyToOneEntities ++
                 map (\e -> 
                  applyF (wuiModule "wMultiCheckSelect")
                   [CLambda [CPVar (1, lowerFirst e)]
                      (list2ac [
                        applyF (html "htxt") [
                         applyF (erdname, lowerFirst e++"ToShortView")
                                [CVar (1, lowerFirst e)]
                         ]]),
                    CVar (1, lowerFirst $ e ++ "List")
                  ]) manyToManyEntities
               )
            ),
            applyF (spiceyModule, "renderLabels")
                   [constF (entitiesToHtmlModule erdname,
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
      (applyTC (wuiModule "WuiSpec") [
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
      (applyF (wuiModule "transformWSpec") [
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


-- Generate function to compare to entities in lexicographic order.
-- To avoid useless component comparisons, only the first five non-key
-- attributes are used for the comparison.
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
                                   (take 5 (filter notKey attrlist))),
                    tupleExpr (map (\ (Attribute a _ _ _) ->
                                       applyF (erdname,ename++a) [CVar e2])
                                   (take 5 (filter notKey attrlist)))
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
              [applyF (entitiesToHtmlModule erdname,
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
                          list2ac [applyF (html"htxt")
                            [string2ac ("back to "++entityName++" list")]]]]
              ]
            )]
      
--- Create operation for the "list entities" view.
listView :: ViewGenerator
listView erdname (Entity entityName attrlist) _ _ =
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
            applyF (html "h1")
                   [list2ac [applyF (html "htxt")
                                    [string2ac $ entityName ++ " list"]]],
            list2ac [
              applyF (spiceyModule, "spTable") [
                applyF (pre "++") [
                  list2ac [
                    applyF (pre "take") [
                      CLit (CIntc (length attrlist)),
                      constF (entitiesToHtmlModule erdname,
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
              applyF (entitiesToHtmlModule erdname,
                      lowerFirst entityName++"ToListView")
                     [cvar $ lowerFirst entityName],
              applyF (pre "if_then_else")
               [applyF (pre "==")
                 [applyF (sessionInfoModule,"userLoginOfSession")
                         [CVar infovar],
                  constF (pre "Nothing")],
                list2ac [],
                list2ac
                 [list2ac
                   [applyF hrefButtonName
                     [applyF (spiceyModule,"showRoute") [CVar envar],
                      list2ac [applyF (html "htxt") [string2ac "show"]]]],
                  list2ac
                   [applyF hrefButtonName
                     [applyF (spiceyModule,"editRoute") [CVar envar],
                      list2ac [applyF (html "htxt") [string2ac "edit"]]]],
                  list2ac
                   [applyF hrefButtonName
                     [applyF (spiceyModule,"deleteRoute") [CVar envar],
                      list2ac [applyF (html "htxt") [string2ac "delete"]]]]
              ]]
            ]
            )])
      ])
    ]
 where
  infovar = (0, "sinfo")
  entsvar = (1, (lowerFirst entityName)++"s")
  envar   = (2, lowerFirst entityName)
    
-- Auxiliaries

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
