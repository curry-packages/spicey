------------------------------------------------------------------------
--- This is the main file for scaffolding.
------------------------------------------------------------------------

module Spicey.Scaffolding where

import AbstractCurry.Types
import AbstractCurry.Build
import AbstractCurry.Pretty hiding(showCProg)
import Database.ERD
import Directory
import FilePath ( (</>) )
import IO
import System(system)

import ERD2Curry ( erd2cdbiWithDBandERD )
import Database.ERD.Goodies

import Spicey.ControllerGeneration
import Spicey.EntitiesToHtmlGeneration
import Spicey.GenerationHelper
import Spicey.RouteGeneration
import Spicey.Transformation
import Spicey.ViewGeneration

--- Pretty print an AbstractCurry program with name qualification on demand.
--- TODO: Currently, our naming scheme should ensure that there are no
--- name conflicts. Therefore, we omit the list of Curry modules
--- for the on-demand qualification. However, to be on the safe side,
--- one should explicitly set this list to the current module and the
--- list of its imports.
showCProg :: CurryProg -> String
showCProg = prettyCurryProg (setOnDemandQualification [] defaultOptions)

getRelationships :: ERD -> [Relationship]
getRelationships (ERD _ _ relationships) = relationships

getEntities :: ERD -> [Entity]
getEntities (ERD _ entities _) = entities

createViews :: String -> ERD -> String -> String -> IO ()
createViews _ (ERD name entities relationship) path _ =
  mapIO_ (saveView name (getEntities erdt) (getRelationships erdt))
         (filter (not . Spicey.GenerationHelper.isGenerated) (getEntities erdt))
 where
  erdt = transform (ERD name entities relationship)

  saveView :: String -> [Entity] -> [Relationship] -> Entity -> IO ()
  saveView erdname allEntities relationships (Entity ename attrlist) = do
    putStrLn ("Saving view operations in 'View."++ename++".curry'...")
    writeFile (path </> ename++".curry")
              (showCProg (generateViewsForEntity erdname allEntities
                            (Entity ename attrlist) relationships))

createControllers :: String -> ERD -> String -> String -> IO ()
createControllers _ (ERD name entities relationship) path _ = do
  mapIO_ (saveController name (getEntities erdt) (getRelationships erdt))
         (filter (not . Spicey.GenerationHelper.isGenerated) (getEntities erdt))
  putStrLn "Generating default controller authorization 'AuthorizedControllers.curry'..."
  writeFile (path </> "DefaultController.curry")
            (showCProg (generateDefaultController name entities))
  writeFile (path </> "AuthorizedControllers.curry")
            (showCProg (generateAuthorizations name entities))
 where
  erdt = transform (ERD name entities relationship)

  saveController :: String -> [Entity] -> [Relationship] -> Entity -> IO ()
  saveController erdname allEntities relationships (Entity ename attrlist) = do
    putStrLn ("Saving controllers in 'Controller."++ename++".curry'...")
    writeFile (path </> ename++".curry")
              (showCProg (generateControllersForEntity erdname allEntities
                            (Entity ename attrlist) relationships))

createAuthorizations :: String -> ERD -> String -> String -> IO ()
createAuthorizations _ (ERD name entities _) path _ = do
  let targetfile = path </> "AuthorizedActions.curry"
  putStrLn $ "Generating default action authorization '" ++ targetfile ++ "'..."
  writeFile targetfile (showCProg (generateAuthorizations name entities))

createHtmlHelpers :: String -> ERD -> String -> String -> IO ()
createHtmlHelpers _ (ERD name entities relationship) path _ =
  saveToHtml name (getEntities erdt) (getRelationships erdt)
 where
  erdt = transform (ERD name entities relationship)

  saveToHtml :: String -> [Entity] -> [Relationship] -> IO ()
  saveToHtml erdname allEntities relationships = do
    putStrLn $ "Saving 'View."++entitiesToHtmlModule erdname++".curry'..."
    fileh <- openFile (path </> erdname++"EntitiesToHtml.curry") WriteMode
    hPutStr fileh (showCProg (generateToHtml erdname allEntities relationships))
    hClose fileh

-- Uses Curry's `ertools` for ERD to Curry transformation
createModels :: String -> ERD -> String -> String -> IO ()
createModels term_path erd path db_path = do
  let erdname = erdName erd
      dbfile = if null db_path then erdname ++ ".db"
                               else db_path
  aterm_path <- getAbsolutePath term_path
  curdir <- getCurrentDirectory
  setCurrentDirectory path
  erd2cdbiWithDBandERD dbfile term_path
  setCurrentDirectory curdir

createRoutes :: String -> ERD -> String -> String -> IO ()
createRoutes _ erd path _ = do
  putStrLn $ "Saving '"++mappingModuleName++".curry'..."
  mmfileh <- openFile (path </> "ControllerMapping.curry") WriteMode
  hPutStr mmfileh (showCProg (generateRoutesForERD erd))
  hClose mmfileh  
  putStrLn $ "Saving '"++dataModuleName++".curry'..."
  dmfileh <- openFile (path </> "RoutesData.curry") WriteMode
  hPutStr dmfileh (showCProg (generateStartpointDataForERD erd))
  hClose dmfileh

------------------------------------------------------------------------
-- Generate all default authorizations.
generateAuthorizations :: String -> [Entity] -> CurryProg
generateAuthorizations erdname entities = simpleCurryProg
  enauthModName
  [authorizationModule, sessionInfoModule, erdname] -- imports
  [] -- typedecls
  -- functions
  (map operationAllowed entities)
  [] -- opdecls
 where
  operationAllowed (Entity entityName _) =
   stCmtFunc
    ("Checks whether the application of an operation to a "++entityName++"\n"++
     "entity is allowed.")
    (enauthModName, lowerFirst entityName ++ "OperationAllowed")
    1
    Public
    (applyTC (authorizationModule,"AccessType") [baseType (erdname,entityName)]
     ~> baseType (sessionInfoModule,"UserSessionInfo")
     ~> ioType (baseType (authorizationModule,"AccessResult")))
    [simpleRule [CPVar (1,"at"), CPVar (2,"_")]
     (CCase CRigid (CVar (1,"at"))
       [cBranch (CPComb (authorizationModule,"ListEntities") []) allowed,
        cBranch (CPComb (authorizationModule,"NewEntity")    []) allowed,
        cBranch (CPComb (authorizationModule,"ShowEntity")   [CPVar (3,"_")])
                allowed,
        cBranch (CPComb (authorizationModule,"DeleteEntity") [CPVar (3,"_")])
                allowed,
        cBranch (CPComb (authorizationModule,"UpdateEntity") [CPVar (3,"_")])
                allowed])]

  -- Expression implemented access allowed
  allowed = applyF (pre "return") [constF (authorizationModule,"AccessGranted")]

  -- Expression implemented access denied
  --exprDenied = applyF (pre "return")
  --                    [applyF (authorizationModule,"AccessDenied")
  --                            [string2ac "Operation not allowed!"]]

------------------------------------------------------------------------