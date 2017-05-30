module Spicey.RouteGeneration where

import AbstractCurry.Types
import AbstractCurry.Build
import Database.ERD
import Database.ERD.Goodies

import Spicey.ControllerGeneration
import Spicey.GenerationHelper

generateRoutesForERD :: ERD -> CurryProg
generateRoutesForERD (ERD _ entities _) =
 let spiceySysCtrl = "Controller.SpiceySystemController" in
 simpleCurryProg
  mappingModuleName
  ([spiceyModule, "System.Routes", spiceySysCtrl, dataModuleName] ++
   (map (\e -> controllerModuleName (entityName e)) entities)) -- imports
  [] -- typedecls
  [
    stCmtFunc 
      ("Maps the controllers associated to URLs in module RoutesData\n"++
       "into the actual controller operations.")
      (mappingModuleName, "getController")
      1 
      Public 
      (baseType (mappingModuleName, "ControllerReference") ~> controllerType)
      [simpleRule [CPVar (1, "fktref")]
         (CCase CRigid (CVar (1, "fktref")) 
              ( [cBranch (CPComb (dataModuleName, "ProcessListController") [])
                         (constF (spiceySysCtrl, "processListController")),
                 cBranch (CPComb (dataModuleName, "LoginController") [])
                         (constF (spiceySysCtrl, "loginController"))] ++
                map branchesForEntity entities ++
                [cBranch (CPVar (2,"_"))
                  (applyF (spiceyModule, "displayError")
                          [string2ac "getController: no mapping found"])]
              )
          )]
  ] -- functions
  [] -- opdecls
  
-- startpoint controller prefixes
controllerPrefixes :: [String]
controllerPrefixes = ["List","New"]

branchesForEntity :: Entity -> (CPattern, CRhs)
branchesForEntity (Entity entityName _) =
  let controllerReference = entityName ++ "Controller"
   in cBranch (CPComb ("RoutesData", controllerReference) [])
              (constF (controllerModuleName entityName,
                       "main" ++ controllerReference))
  
generateStartpointDataForERD :: ERD -> CurryProg
generateStartpointDataForERD (ERD _ entities _) = simpleCurryProg
  dataModuleName
  [authenticationModule] -- imports
  [
    CType (dataModuleName, "ControllerReference") Public []
          ([simpleCCons (dataModuleName, "ProcessListController") Public [],
            simpleCCons (dataModuleName, "LoginController") Public []] ++
           map controllerReferencesForEntity entities) [],
    urlMatchType,
    routeType
  ] -- typedecls
  [stCmtFunc 
     ("This constant specifies the association of URLs to controllers.\n"++
      "Controllers are identified here by constants of type\n"++
      "ControllerReference. The actual mapping of these constants\n"++
      "into the controller operations is specified in the module\n"++
      "ControllerMapping.")
     (dataModuleName, "getRoutes")
     0
     Public 
     (ioType routeMappingType)
     [simpleRule []
        (CDoExpr
          [CSPat (CPVar (1,"login"))
                 (constF (authenticationModule,"getSessionLogin")),
           CSExpr $ applyF (pre "return")
             [list2ac (
               [tupleExpr
                  [string2ac "Processes",
                   applyF (dataModuleName, "Exact")
                          [string2ac "spiceyProcesses"],
                   constF (dataModuleName, "ProcessListController")]
               ] ++
               concatMap startpointsForEntity entities ++
               [tupleExpr
                  [applyF (pre "maybe")
                          [string2ac "Login",
                           applyF (pre "const") [string2ac "Logout"],
                           CVar (1,"login")],
                   applyF (dataModuleName, "Exact") [string2ac "login"],
                   constF (dataModuleName, "LoginController")],
                tupleExpr
                  [string2ac "default",
                   constF (dataModuleName, "Always"),
                   constF (dataModuleName,
                           firstEntityName entities ++ "Controller")]
               ]
             )
            ]
         ]
        )]
  ] -- functions
  [] -- opdecls
 where
  firstEntityName :: [Entity] -> String
  firstEntityName ((Entity entityName _):_) = entityName
  firstEntityName [] = error "RouteGeneration.firstEntityName: empty list arg"

  route :: String -> String -> String -> String -> CExpr
  route desc url uparam controllerDef =
    tupleExpr [string2ac desc,
               applyF (dataModuleName, "Prefix")
                      [string2ac url, string2ac uparam],
               constF (dataModuleName, controllerDef)]

  startpointsForEntity :: Entity -> [CExpr]
  startpointsForEntity (Entity entityName _) =
    map (\pre -> route (pre ++ " " ++ entityName)
                       entityName
                       (lowerFirst pre)
                       (entityName ++ "Controller"))
        controllerPrefixes
      
  urlMatchType :: CTypeDecl
  urlMatchType =
    CType (dataModuleName, "UrlMatch") Public [] [
      simpleCCons (dataModuleName, "Exact")   Public [stringType],
      simpleCCons (dataModuleName, "Prefix")  Public [stringType,stringType],
      simpleCCons (dataModuleName, "Matcher") Public [stringType ~> boolType],
      simpleCCons (dataModuleName, "Always")  Public []
    ] []
    
  routeMappingType :: CTypeExpr
  routeMappingType = listType (baseType (dataModuleName,"Route"))
  
  routeType :: CTypeDecl
  routeType =
    CTypeSyn (dataModuleName, "Route") Public []
      (tupleType [stringType,
                  baseType (dataModuleName, "UrlMatch"),
                  baseType (dataModuleName, "ControllerReference")])
    
  controllerReferencesForEntity :: Entity -> CConsDecl
  controllerReferencesForEntity (Entity entityName _) =
    simpleCCons (dataModuleName, entityName++"Controller") Public []
