{-# OPTIONS_CYMAKE -Wno-incomplete-patterns #-}

module Spicey.EntityRoutesGeneration where

import Data.Char ( toLower )

import AbstractCurry.Types
import AbstractCurry.Build
import Database.ERD
import Database.ERD.Goodies

import Spicey.GenerationHelper

-- "main"-function
generateRoutesForEntity :: String -> [Entity] -> CurryProg
generateRoutesForEntity erdname allEntities =
  CurryProg
   "Config.EntityRoutes"
   -- imports:
   [ "System.Spicey", model erdname ]
   Nothing -- defaultdecl
   [] -- classdecls
   (map (controllerInstDecl erdname) allEntities) -- instdecls
   [] -- typedecls
   -- functions
   []
   [] -- opdecls


-- Generates the instance declaration for a controller.
controllerInstDecl :: String -> Entity -> CInstanceDecl
controllerInstDecl erdname (Entity entityName _) =
  CInstance (spiceyModule,"EntityController")
    (CContext [])
    entityType
    [stFunc (spiceyModule,"controllerOnKey") 1 Private
       (stringType ~> (entityType ~> controllerType) ~> controllerType)
       [simpleRule [CPVar (2,"s")]
                   (applyF (spiceyModule,"applyControllerOn")
                           [readKey, getEntityOp])],
     stFunc (spiceyModule,"entityRoute") 2 Private
       (stringType ~> entityType ~> stringType)
       [simpleRule [CPVar rvar, CPVar entvar]
          (applyF (pre "concat")
             [list2ac
                [string2ac $ '?' : entityName ++ "/",
                 CVar rvar,
                 string2ac "/",
                 applyF (model erdname, "show" ++ entityName ++ "Key")
                        [CVar entvar]]])]]
 where
  entityType  = baseType (model erdname, entityName)
  rvar        = (1,"r")
  entvar      = (2,"ent")
  readKey     = applyF (model erdname, "read" ++ entityName ++ "Key")
                       [CVar (2,"s")]
  getEntityOp = applyF (pre ".")
                       [constF (model erdname, "runJustT"),
                        constF (model erdname, "get" ++ entityName)]

------------------------------------------------------------------------
