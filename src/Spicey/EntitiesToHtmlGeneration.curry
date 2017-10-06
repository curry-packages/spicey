module Spicey.EntitiesToHtmlGeneration where

import AbstractCurry.Types
import AbstractCurry.Build
import Database.ERD
import Database.ERD.Goodies

import Spicey.GenerationHelper

-- "main"-function
generateToHtml :: String -> [Entity] -> [Relationship] -> CurryProg
generateToHtml erdname allEntities relationships = CurryProg
  (entitiesToHtmlModule erdname)
  ["WUI", "HTML.Base", "Time", spiceyModule, erdname] -- imports
  [] -- typedecls
  -- functions
  (
   foldr1 (++) (map (\e -> generateToHtmlForEntity erdname allEntities e relationships)
                    (filter (not . Spicey.GenerationHelper.isGenerated)
                            allEntities))
  )
  []
  
  
generateToHtmlForEntity :: String -> [Entity] -> Entity -> [Relationship] -> [CFuncDecl]
generateToHtmlForEntity erdname allEntities (Entity ename attrlist) relationships =
  [toListView    erdname (Entity ename (filter noKeyAttr attrlist)) relationships allEntities,
   toShortView   erdname (Entity ename (filter noKeyAttr attrlist)) relationships allEntities,
   toDetailsView erdname (Entity ename (filter noKeyAttr attrlist)) relationships allEntities,
   labelList     erdname (Entity ename (filter noKeyAttr attrlist)) relationships allEntities
  ]
 where
   noKeyAttr a = (notKey a) && (notPKey a)
  
  
type ToHtmlGenerator = String -> Entity -> [Relationship] -> [Entity] -> CFuncDecl

toListView :: ToHtmlGenerator
toListView erdname (Entity entityName attrlist) _ _ =
 cmtfunc
  ("The list view of a "++entityName++" entity in HTML format.\n"++
   "This view is used in a row of a table of all entities.")
  (thisModuleName erdname, (lowerFirst entityName)++"ToListView") 2 Public
  (baseType (erdname, entityName)
    ~> listType (listType (baseType (html "HtmlExp"))))
  [simpleRule [CPVar (1, lowerFirst entityName)]
     (list2ac (
            (map (\a -> list2ac [
                          applyF (attributeToConverter a) [
                            applyF (erdname, (lowerFirst entityName)++(attributeName a)) [
                              cvar (lowerFirst entityName)
                            ]
                          ]
                        ]
                 ) 
              attrlist
           )
         )
     )]

toShortView :: ToHtmlGenerator
toShortView erdname (Entity entityName attrlist) _ _ =
  cmtfunc
  ("The short view of a "++entityName++" entity as a string.\n"++
   "This view is used in menus and comments to refer to a "++entityName++" entity.")
  (thisModuleName erdname, (lowerFirst entityName)++"ToShortView")
  2
  Public
  (baseType (erdname, entityName) ~> stringType)
  [simpleRule [CPVar (1,eName)]
     (case attributeDomain firstKeyAttribute of
         StringDom _ -> accessFirstKeyAttribute
         _           -> applyF (pre "show") [accessFirstKeyAttribute]
     )]
  where
    eName = lowerFirst entityName

    accessFirstKeyAttribute =
      applyF (erdname, eName++attributeName firstKeyAttribute) [cvar eName]

    firstKeyAttribute = findKeyAttribute attrlist attrlist
    
    findKeyAttribute (attr@(Attribute _ _ key _):attrList) fullList =
      if key== Unique then attr
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
  ("The detailed view of a "++entityName++" entity in HTML format.\n"++
   if null (manyToOneEntities ++ manyToManyEntities) then "" else
   "It also takes associated entities for every associated entity type.")
  (thisModuleName erdname, (lowerFirst entityName)++"ToDetailsView")
  2
  Public
  -- function type
  (foldr CFuncType (listType (baseType (html "HtmlExp")))
         ([baseType (erdname, entityName)] ++
          (map ctvar manyToOneEntities) ++ -- defaults for n:1
          (map (\name -> listType (ctvar name)) manyToManyEntities)
         )
  )
  [CRule
    ( -- parameters
      [CPVar evar] ++
       map (\ (name, varId) -> CPVar (varId,"related"++name))
            (zip manyToOneEntities [2..]) ++
       map (\ (name, varId) -> CPVar (varId, lowerFirst name ++ "s"))
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
                                 [applyF (erdname, eName ++ attributeName a)
                                         [CVar evar]]])
                 attrlist ++
             map (\ (name,varId) ->
                    list2ac [applyF (html "htxt")
                                    [applyF (thisModuleName erdname,
                                             lowerFirst name ++ "ToShortView")
                                            [CVar (varId,"related"++name)]]])
                 (zip manyToOneEntities [2..]) ++
             map (\ (name,varId) ->
                    list2ac
                      [applyF (html "htxt")
                              [applyF (pre "unwords")
                                [applyF (pre "map")
                                  [applyF (thisModuleName erdname,
                                           lowerFirst name ++"ToShortView") [],
                                   CVar (varId,lowerFirst name++"s")]]]])
                 (zip manyToManyEntities [(length manyToOneEntities + 2)..])
            )
        )
        [])
    ])
  ]

labelList :: ToHtmlGenerator
labelList erdname (Entity entityName attrlist) relationships allEntities =
  let
    manyToManyEntities = manyToMany allEntities (Entity entityName attrlist)
    manyToOneEntities = manyToOne (Entity entityName attrlist) relationships
  in
   cmtfunc
    ("The labels of a "++entityName++" entity, as used in HTML tables.")
    (thisModuleName erdname, (lowerFirst entityName)++"LabelList") 2 Public
    (
      listType (listType (CTCons (html "HtmlExp") []))
    )
    [simpleRule []
      (list2ac (
            (map (\ (Attribute name domain _ _) ->
                   list2ac [applyF (html "textstyle")
                               [string2ac ("spicey_label spicey_label_for_type_"++
                                           domainToString domain),
                                string2ac name]])
                 attrlist) ++
            (map (\s -> list2ac [applyF (html "textstyle")
                                  [string2ac "spicey_label spicey_label_for_type_relation",
                                   string2ac s]])
                 (manyToOneEntities++manyToManyEntities))
          )
       )]

thisModuleName :: String -> String
thisModuleName erdname = entitiesToHtmlModule erdname

attributeToConverter :: Attribute -> QName
attributeToConverter (Attribute _ domain _ isnull) =
  (spiceyModule,
   if isnull && not (isStringDom domain)
     then "maybe" ++ upperFirst (domainToString domain) ++ "ToHtml"
     else domainToString domain ++ "ToHtml")

domainToString :: Domain -> String
domainToString domain =
  case domain of
    IntDom    _     -> "int"
    FloatDom  _     -> "float"
    CharDom   _     -> "char"
    StringDom _     -> "string"
    BoolDom   _     -> "bool"
    DateDom   _     -> "calendarTime"
    UserDefined _ _ -> "userDefined"
    KeyDom    _     -> "key"