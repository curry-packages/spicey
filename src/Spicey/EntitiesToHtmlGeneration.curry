module Spicey.EntitiesToHtmlGeneration where

import AbstractCurry.Types
import AbstractCurry.Build
import Database.ERD
import Database.ERD.Goodies

import Spicey.GenerationHelper

-- "main"-function
generateToHtml :: String -> [Entity] -> [Relationship] -> CurryProg
generateToHtml erdname allEntities relationships = simpleCurryProg
  (entitiesToHtmlModule erdname)
  [ timeModule, "HTML.Base", bootstrapModule, "HTML.WUI", "Config.EntityRoutes"
  , spiceyModule, model erdname] -- imports
  [] -- typedecls
  -- functions
  (
   foldr1 (++)
     (map (\e -> generateToHtmlForEntity erdname allEntities e relationships)
          (filter (not . Spicey.GenerationHelper.isGenerated)
                  allEntities))
  )
  []


generateToHtmlForEntity :: String -> [Entity] -> Entity -> [Relationship]
                        -> [CFuncDecl]
generateToHtmlForEntity erdname allEntities (Entity ename attrlist) relationships =
  [toListView    erdname (Entity ename (filter noKeyAttr attrlist)) relationships allEntities,
   toShortView   erdname (Entity ename (filter noKeyAttr attrlist)) relationships allEntities,
   toDetailsView erdname (Entity ename (filter noKeyAttr attrlist)) relationships allEntities,
   labelList     erdname (Entity ename (filter noKeyAttr attrlist)) relationships allEntities
  ]
 where
  noKeyAttr a = (notKey a) && (notPKey a)
  
  
type ToHtmlGenerator =
       String -> Entity -> [Relationship] -> [Entity] -> CFuncDecl

toListView :: ToHtmlGenerator
toListView erdname (Entity entityName attrlist) _ _ =
 cmtfunc
  ("The list view of a " ++ entityName ++ " entity in HTML format.\n" ++
   "This view is used in a row of a table of all entities.")
  (thisModuleName erdname, lowEntity ++ "ToListView") 2 Public
  (withHTMLContext
     (baseType (model erdname, entityName) ~> listType (listType htmlTVar)))
  [simpleRule [CPVar envar]
     (list2ac
       (map (\a@(Attribute _ domain key _) ->
              let attref = applyF (model erdname, lowEntity ++ attributeName a)
                                  [CVar envar]
              in list2ac
                  [if key == Unique
                     then applyF hrefSmallButtonName
                           [applyF (spiceyModule,"showRoute") [CVar envar],
                            list2ac [applyF (attributeToConverter a) [attref]]]
                     else applyF (attributeToConverter a) [attref]
                  ] )
            attrlist))]
 where lowEntity = lowerFirst entityName
       envar = (1, lowEntity)

toShortView :: ToHtmlGenerator
toShortView erdname (Entity entityName attrlist) _ _ =
  stCmtFunc
  ("The short view of a " ++ entityName ++ " entity as a string.\n"++
   "This view is used in menus and comments to refer to a " ++ entityName ++
   " entity.")
  (thisModuleName erdname, (lowerFirst entityName)++"ToShortView")
  2
  Public
  (baseType (model erdname, entityName) ~> stringType)
  [simpleRule [CPVar (1,eName)]
     (case attributeDomain firstKeyAttribute of
         StringDom _ -> accessFirstKeyAttribute
         _           -> applyF (pre "show") [accessFirstKeyAttribute]
     )]
  where
    eName = lowerFirst entityName

    accessFirstKeyAttribute =
      applyF (model erdname, eName ++ attributeName firstKeyAttribute)
             [cvar eName]

    firstKeyAttribute = findKeyAttribute attrlist attrlist
    
    findKeyAttribute (attr@(Attribute _ _ key _):attrList) fullList =
      if key == Unique then attr
                       else findKeyAttribute attrList fullList
    findKeyAttribute [] fullList = head fullList

toDetailsView :: ToHtmlGenerator
toDetailsView erdname (Entity entityName attrlist) relationships allEntities =
 let manyToManyEntities = manyToMany allEntities (Entity entityName attrlist)
     manyToOneEntities  = manyToOne (Entity entityName attrlist) relationships
     eName = lowerFirst entityName
     evar  = (1,eName)
  in
 cmtfunc
  ("The detailed view of a " ++ entityName ++ " entity in HTML format.\n" ++
   if null manyToOneEntities && null manyToManyEntities
     then ""
     else "It also takes associated entities for every associated entity type.")
  (thisModuleName erdname, lowerFirst entityName ++ "ToDetailsView")
  2
  Public
  -- function type
  (withHTMLContext $
     foldr CFuncType (listType htmlTVar)
           ([baseType (model erdname, entityName)] ++
            (map ctvar manyToOneEntities) ++ -- defaults for n:1
            (map (\ (name,_) -> listType (ctvar name)) manyToManyEntities)
           )
  )
  [CRule
    ( -- parameters
      [CPVar evar] ++
       map (\ (name, varId) -> CPVar (varId,"related"++name))
            (zip manyToOneEntities [2..]) ++
       map (\ ((ename,erel), i) -> CPVar (i, lowerFirst erel ++ ename ++ "s"))
           (zip manyToManyEntities [(length manyToOneEntities + 2)..])
    )
    (CSimpleRhs
       (list2ac [
          applyF (spiceyModule, "spTable") [
            applyF (pre "map")
            [
              CLambda [tuplePattern [CPVar(2, "label"), CPVar(3, "value")]]
                      (list2ac [cvar "label", cvar "value"]),
              applyF (pre "zip")
                     [constF (thisModuleName erdname, eName++"LabelList"),
                      CVar (2,"detailedView")]
            ]
          ]
        ]
      )
    [CLocalPat (CPVar (2,"detailedView"))
       (CSimpleRhs
         (list2ac
            (map (\a -> list2ac [
                          applyF (attributeToConverter a)
                              [applyF (model erdname, eName ++ attributeName a)
                                      [CVar evar]]])
                 attrlist ++
             map (\ (name,varId) ->
                    list2ac [applyF (html "htxt")
                                    [applyF (thisModuleName erdname,
                                             lowerFirst name ++ "ToShortView")
                                            [CVar (varId,"related"++name)]]])
                 (zip manyToOneEntities [2..]) ++
             map (\ ((ename,erel),varId) ->
                    list2ac
                      [applyF (html "htxt")
                         [applyF (pre "unwords")
                            [applyF (pre "map")
                               [applyF (thisModuleName erdname,
                                        lowerFirst ename ++ "ToShortView") [],
                                CVar (varId, lowerFirst erel ++ ename++"s")]]]])
                 (zip manyToManyEntities [(length manyToOneEntities + 2)..])
            )
        )
        [])
    ])
  ]

labelList :: ToHtmlGenerator
labelList erdname (Entity entityName attrlist) relationships allEntities =
  cmtfunc
   ("The labels of a " ++ entityName ++ " entity, as used in HTML tables.")
   (thisModuleName erdname, lowerFirst entityName ++ "LabelList") 2 Public
   (withHTMLContext (listType (listType htmlTVar)))
   [simpleRule []
     (list2ac (
           (map (\ (Attribute name domain _ _) ->
                  list2ac
                    [applyF (html "textstyle")
                            [string2ac ("spicey_label spicey_label_for_type_"++
                                        domainToString domain),
                             string2ac name]])
                attrlist) ++
           (map (\ (ename,erel) -> list2ac
                     [applyF (html "textstyle")
                       [string2ac "spicey_label spicey_label_for_type_relation",
                        string2ac $ erel ++ ename]])
                (map (\n -> (n,"")) manyToOneEntities ++ manyToManyEntities))
         )
      )]
 where
  manyToManyEntities = manyToMany allEntities (Entity entityName attrlist)
  manyToOneEntities = manyToOne (Entity entityName attrlist) relationships

thisModuleName :: String -> String
thisModuleName erdname = entitiesToHtmlModule erdname

attributeToConverter :: Attribute -> QName
attributeToConverter (Attribute _ domain _ isnull) =
  (spiceyModule,
   if isnull && not (isStringDom domain)
     then "maybe" ++ upperFirst (domainToString domain) ++ "ToHtml"
     else domainToString domain ++ "ToHtml")

domainToString :: Domain -> String
domainToString domain = case domain of
  IntDom    _     -> "int"
  FloatDom  _     -> "float"
  CharDom   _     -> "char"
  StringDom _     -> "string"
  BoolDom   _     -> "bool"
  DateDom   _     -> "date"
  UserDefined _ _ -> "userDefined"
  KeyDom    _     -> "key"
