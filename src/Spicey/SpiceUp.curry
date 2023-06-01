------------------------------------------------------------------------------
--- Main module to generate the initial Spicey application
------------------------------------------------------------------------------

module Spicey.SpiceUp where

import Control.Monad        ( unless, when )
import Curry.Compiler.Distribution ( installDir )
import Data.List            ( isSuffixOf )
import System.Environment   ( getArgs, setEnv )

import Database.ERD         ( ERD(..), Entity(..) )
import Database.ERD.Goodies ( erdName, readERDFromProgram )
import System.Directory
import System.FilePath      ( (</>), takeFileName )
import System.Process       ( exitWith, system )

import Spicey.PackageConfig ( packagePath, packageVersion, packageLoadPath )
import Spicey.Scaffolding

systemBanner :: String
systemBanner =
  let bannerText = "Spicey Web Framework (Version " ++ packageVersion ++
                   " of 01/06/23)"
      bannerLine = take (length bannerText) (repeat '-')
   in bannerLine ++ "\n" ++ bannerText ++ "\n" ++ bannerLine

data FileMode = Exec | NoExec

setFileMode :: FileMode -> String -> IO ()
setFileMode NoExec filename = return ()
setFileMode Exec   filename = do
  system $ "chmod +x \"" ++ filename ++ "\""
  return ()


data DirTree =
   Directory String [DirTree]   -- a directory to be created
 | ResourceFile FileMode String -- a file to be copied from resource directory
 | ResourcePatchFile FileMode String (ERD -> String -> String)
    -- file to be copied from resource directory
    -- where its contents is patched by the given function
 | GeneratedFromERD (String -> ERD -> String -> String -> IO ())
   -- takes an operation to generate code from ERD specification

spiceyStructure :: String -> DirTree
spiceyStructure pkgname = 
  Directory "." [
    ResourceFile NoExec "README.md",
    ResourcePatchFile NoExec "package.json" (replacePackageName pkgname),
    ResourcePatchFile NoExec "Makefile" patchMakeFile,
    Directory "src" [
      srcfile "Main.curry",
      Directory "System" [
        srcfile $ "System" </> "Spicey.curry",
        srcfile $ "System" </> "Routes.curry",
        srcfile $ "System" </> "SessionInfo.curry",
        srcfile $ "System" </> "Authorization.curry",
        srcfile $ "System" </> "Authentication.curry",
        srcfile $ "System" </> "Processes.curry",
        GeneratedFromERD createAuthorizations ],
      Directory "View" [
        srcfile $ "View" </> "SpiceySystem.curry",
        GeneratedFromERD createViews,
        GeneratedFromERD createHtmlHelpers ],
      Directory "Controller" [
        srcfile $ "Controller" </> "SpiceySystem.curry",
        GeneratedFromERD createControllers ],
      Directory "Model" [
        GeneratedFromERD createModels ],
      Directory "Config" [
        srcfile $ "Config" </> "UserProcesses.curry",
        GeneratedFromERD createRoutes,
        GeneratedFromERD createEntityRoutes ]
    ],
    Directory "data" [
      ResourceFile NoExec $ "data" </> "htaccess"
    ],
    Directory "public" [
      ResourceFile NoExec $ "public" </> "index.html",
      Directory "bt4" [
        Directory "css" [
          bt4file $ "css" </> "bootstrap.min.css",
          bt4file $ "css" </> "spicey.css"
        ],
        Directory "js" [
          bt4file $ "js" </> "bootstrap.bundle.min.js",
          bt4file $ "js" </> "jquery.slim.min.js"
        ],
        Directory "img" [
          bt4file $ "img" </> "favicon.ico",
          bt4file $ "img" </> "spicey-logo.png",
          bt4file $ "img" </> "text.png",
          bt4file $ "img" </> "time.png",
          bt4file $ "img" </> "number.png",
          bt4file $ "img" </> "foreign.png"
        ]
      ]
    ]
  ]
 where
  srcfile f = ResourceFile NoExec $ "src" </> f
  bt4file f = ResourceFile NoExec $ "public" </> "bt4" </> f

-- Replace every occurrence of `XXXCURRYHOMEXXX` by `installDir` and
-- every occurrince of `XXXICONTROLLERXXX` by
-- `-i <controllermod1> ... -i <controllermodn>`.
patchMakeFile :: ERD -> String -> String
patchMakeFile _                      []     = []
patchMakeFile erd@(ERD _ entities _) (c:cs)
  | c=='X' && take 14 cs == "XXCURRYHOMEXXX"
  = installDir ++ patchMakeFile erd (drop 14 cs)
  | c=='X' && take 16 cs == "XXICONTROLLERXXX"
  = unwords (map (\ (Entity ename _) -> "-i Controller." ++ ename) entities) ++
    patchMakeFile erd (drop 16 cs)
  | otherwise
  = c : patchMakeFile erd cs

-- Replace every occurrence of "XXXPKGNAMEXXX" by first argument
replacePackageName :: String -> ERD -> String -> String
replacePackageName _  _   []     = []
replacePackageName pn erd (c:cs)
  | c=='X' && take 12 cs == "XXPKGNAMEXXX"
    = pn ++ replacePackageName pn erd (drop 12 cs)
  | otherwise = c : replacePackageName pn erd cs

-- checks if given path exists (file or directory) and executes
-- given action if not
ifNotExistsDo :: String -> IO () -> IO ()
ifNotExistsDo path cmd = do
  fileExists <- doesFileExist path
  dirExists  <- doesDirectoryExist path
  if fileExists || dirExists
    then putStrLn $ "Skipping '" ++ path ++ "'..."
    else cmd

--- Creates the structure of the source files of the new package.
createStructure :: String -> String -> ERD -> String -> String -> DirTree
                -> IO ()
createStructure target_path resource_dir _ _ _
                (ResourceFile fmode filename) = do
  let infile     = resource_dir </> filename
      targetfile = target_path  </> takeFileName filename
  ifNotExistsDo targetfile $ do
    putStrLn $ "Creating file '" ++ targetfile ++ "'..."
    system $ "cp \"" ++ infile ++ "\" \"" ++ targetfile ++ "\""
    setFileMode fmode targetfile

createStructure target_path resource_dir erd _ _
                (ResourcePatchFile fmode filename patchfun) = do
  let full_path = target_path </> filename
  ifNotExistsDo full_path $ do
    putStrLn ("Creating file '" ++ full_path ++ "'...")
    cnt <- readFile (resource_dir </> filename)
    let outfile = target_path </> filename
    writeFile outfile (patchfun erd cnt)
    setFileMode fmode outfile

createStructure target_path resource_dir erd erprogpath db_path
                (Directory dirname subtree) = do
  let full_path = target_path </> dirname
  ifNotExistsDo full_path $ do
    putStrLn $ "Creating directory '" ++ full_path ++ "'..."
    createDirectory full_path
  mapM_ (createStructure full_path resource_dir erd erprogpath db_path) subtree

createStructure target_path _ erd erprogpath db_path
                (GeneratedFromERD generatorFunction) =
  generatorFunction erprogpath erd target_path db_path

--- The main operation to start the scaffolding.
main :: IO ()
main = do
  putStrLn systemBanner
  args <- getArgs
  case args of
    ["-h"]     -> spiceupHelp 0
    ["--help"] -> spiceupHelp 0
    ["-?"]     -> spiceupHelp 0
    ["--db",dbfile,orgfile] -> createStructureWith orgfile dbfile
    [orgfile]               -> createStructureWith orgfile ""
    _ -> putStrLn ("Wrong arguments!\n") >> spiceupHelp 1
 where
  createStructureWith orgfile dbfile = do
    -- set CURRYPATH in order to compile ERD model (which requires Database.ERD)
    unless (null packageLoadPath) $ setEnv "CURRYPATH" packageLoadPath
    -- The directory containing the resource files of the project generator:
    let resourcedir = packagePath </> "resource_files"
    exfile <- doesFileExist orgfile
    unless exfile $ error ("File '" ++ orgfile ++ "' does not exist!")
    unless (".curry"  `isSuffixOf` orgfile ||
            ".lcurry" `isSuffixOf` orgfile) $ do
       putStrLn $ "ERROR: '" ++ orgfile ++ "' is not a Curry program file!"
       exitWith 1
    erd <- readERDFromProgram orgfile
    let pkgname = erdName erd
    createDirectoryIfMissing True pkgname
    dbpath <- getAbsolutePath $ if null dbfile then pkgname </> pkgname ++ ".db"
                                               else dbfile
    createStructure pkgname resourcedir erd orgfile dbpath
                    (spiceyStructure pkgname)
    -- save original ERD specification in src/Model directory:
    copyFile orgfile
             (pkgname </> "src" </> "Model" </> erdName erd ++ "_ERD.curry")
    putStrLn (helpText pkgname dbpath)

  helpText pkgname dbpath = unlines $
    [ take 70 (repeat '-')
    , "Source files for the application generated as Curry package '" ++
      pkgname ++ "'."
    , ""
    , "The database is stored in: " ++ dbpath
    , "If you want to store it at another place, move this file and change"
    , "the definition of 'sqliteDBFile' in 'src/Model/" ++ pkgname ++ ".curry'."
    , ""
    , "Please go into the package directory where the 'README.md' file"
    , "contains some hints how to install the generated application."
    , ""
    , "IMPORTANT NOTE:"
    , "Before you deploy your web application (by 'make deploy'),"
    , "you should define the variable WEBSERVERDIR in the Makefile!"
    ]

spiceupHelp :: Int -> IO ()
spiceupHelp ecode = putStrLn helpText >> exitWith ecode
 where
  helpText = unlines $
   [ "Usage:"
   , ""
   , "    spiceup [--db <db file>] <ERD program file>"
   , ""
   , "Parameters:"
   , "--db <db file>    : name of the SQLite3 database file (default: <ERD name>.db)"
   , "<ERD program file>: name of Curry program file containing ERD definition"
   ]

------------------------------------------------------------------------
