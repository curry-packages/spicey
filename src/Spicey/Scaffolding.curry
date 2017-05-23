------------------------------------------------------------------------
--- This is the main file for scaffolding.
------------------------------------------------------------------------

module Spicey.Scaffolding where

import AbstractCurry.Types
import AbstractCurry.Build
import AbstractCurry.Pretty hiding(showCProg)
import Database.ERD
import Database.ERDGoodies
import FilePath ( (</>) )
import IO
import System(system)

import ERD2Curry ( erd2curryWithDBandERD )

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

createViewsForTerm :: String -> String -> String -> String -> IO ()
createViewsForTerm _ termpath path _ = do
  erd <- readERDTermFile termpath
  createViews path erd
    
createViews :: String -> ERD -> IO ()
createViews path (ERD name entities relationship) =
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

createControllersForTerm :: String -> String -> String -> String -> IO ()
createControllersForTerm _ termpath path _ = do
  erd <- readERDTermFile termpath
  createControllers path erd
      
createControllers :: String -> ERD -> IO ()
createControllers path (ERD name entities relationship) = do
   mapIO_ (saveController name (getEntities erdt) (getRelationships erdt))
          (filter (not . Spicey.GenerationHelper.isGenerated) (getEntities erdt))
   putStrLn "Generating default controller authorization AuthorizedControllers.curry..."
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

createHtmlHelpersForTerm :: String -> String -> String -> String -> IO ()
createHtmlHelpersForTerm _ termpath path _ = do
  erd <- readERDTermFile termpath
  createHtmlHelper path erd    
    
createHtmlHelper :: String -> ERD -> IO ()
createHtmlHelper path (ERD name entities relationship) =
    saveToHtml name (getEntities erdt) (getRelationships erdt)
 where
  erdt = transform (ERD name entities relationship)

  saveToHtml :: String -> [Entity] -> [Relationship] -> IO ()
  saveToHtml erdname allEntities relationships = do
    putStrLn $ "Saving 'View."++entitiesToHtmlModule erdname++".curry'..."
    fileh <- openFile (path </> erdname++"EntitiesToHtml.curry") WriteMode
    hPutStr fileh (showCProg (generateToHtml erdname allEntities relationships))
    hClose fileh

-- uses Curry's build-in tool for ERD to Curry transformation
createModelsForTerm :: String -> String -> String -> String -> IO ()
createModelsForTerm _ term_path path db_path = do
  erd <- readERDTermFile term_path
  let dbfile = if null db_path then erdName erd ++ ".db"
                               else db_path
  erd2curryWithDBandERD dbfile term_path
  let orgerdfile   = erdName erd ++ "_ERD.term"
      transerdfile = erdName erd ++ "_ERDT.term"
      curryfile    = erdName erd ++ ".curry"
  system $ unwords ["mv", transerdfile, curryfile, "ERDGeneric.curry", path]
  system $ unwords ["cp", term_path, path </> orgerdfile]
  done

createRoutesForTerm :: String -> String -> String -> String -> IO ()
createRoutesForTerm _ termpath path _ = do
  erd <- readERDTermFile termpath
  createRoutes path erd

createRoutes :: String -> ERD -> IO ()
createRoutes path erd = do
  putStrLn $ "Saving '"++mappingModuleName++".curry'..."
  mmfileh <- openFile (path </> "ControllerMapping.curry") WriteMode
  hPutStr mmfileh (showCProg (generateRoutesForERD erd))
  hClose mmfileh  
  putStrLn $ "Saving '"++dataModuleName++".curry'..."
  dmfileh <- openFile (path </> "RoutesData.curry") WriteMode
  hPutStr dmfileh (showCProg (generateStartpointDataForERD erd))
  hClose dmfileh

------------------------------------------------------------------------
