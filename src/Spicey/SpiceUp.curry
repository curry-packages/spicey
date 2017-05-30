-- Main module to generate the initial Spicey application

module Spicey.SpiceUp where

import Database.ERD         (ERD, readERDTermFile)
import Database.ERD.Goodies (erdName, storeERDFromProgram)
import Directory
import Distribution
import FilePath             ((</>))
import List                 (isSuffixOf, last)
import System               (system, getArgs, exitWith)

import Spicey.PackageConfig (packagePath, packageVersion)
import Spicey.Scaffolding

systemBanner :: String
systemBanner =
  let bannerText = "Spicey Web Framework (Version " ++ packageVersion ++
                   " of 30/05/17)"
      bannerLine = take (length bannerText) (repeat '-')
   in bannerLine ++ "\n" ++ bannerText ++ "\n" ++ bannerLine

data FileMode = Exec | NoExec

setFileMode :: FileMode -> String -> IO ()
setFileMode fmode filename =
  if fmode==Exec then system ("chmod +x \"" ++ filename ++ "\"") >> done
                 else done

data DirTree =
   Directory String [DirTree] -- a directory to be created
 | ResourceFile FileMode String   -- a file to be copied from resource directory
 | ResourcePatchFile FileMode String (String->String) -- file to be copied from
      -- resource directory where its contents is patched by the given function
 | GeneratedFromERD (String -> ERD -> String -> String -> IO ())
   -- takes an operation to generate code from ERD specification

spiceyStructure :: String -> DirTree
spiceyStructure pkgname = 
  Directory "." [
    ResourceFile NoExec "README.txt",
    ResourcePatchFile NoExec "package.json" (replacePackageName pkgname),
    ResourcePatchFile NoExec "Makefile" replaceCurryBin,
    Directory "src" [
      ResourceFile NoExec "Main.curry",
      Directory "System" [
        ResourceFile NoExec "Spicey.curry",
        ResourceFile NoExec "Routes.curry",
        ResourceFile NoExec "Crypto.curry",
        ResourceFile NoExec "Session.curry",
        ResourceFile NoExec "SessionInfo.curry",
        ResourceFile NoExec "Authorization.curry",
        ResourceFile NoExec "Authentication.curry",
        ResourceFile NoExec "Processes.curry" ],
      Directory "View" [
        ResourceFile NoExec "SpiceySystemView.curry",
        GeneratedFromERD createViews,
        GeneratedFromERD createHtmlHelpers ],
      Directory "Controller" [
        ResourceFile NoExec "SpiceySystemController.curry",
        GeneratedFromERD createControllers ],
      Directory "Model" [
        GeneratedFromERD createModels ],
      Directory "Config" [
        ResourceFile NoExec "UserProcesses.curry",
        GeneratedFromERD createRoutes ]
    ],
    Directory "public" [
      ResourceFile NoExec "index.html",
      ResourceFile NoExec "favicon.ico",
      Directory "css" [
        ResourceFile NoExec "bootstrap.min.css",
        ResourceFile NoExec "spicey.css"
      ],
      Directory "js" [
        ResourceFile NoExec "bootstrap.min.js",
        ResourceFile NoExec "jquery.min.js"
      ],
      Directory "fonts" [
        ResourceFile NoExec "glyphicons-halflings-regular.eot",
        ResourceFile NoExec "glyphicons-halflings-regular.svg",
        ResourceFile NoExec "glyphicons-halflings-regular.ttf",
        ResourceFile NoExec "glyphicons-halflings-regular.woff",
        ResourceFile NoExec "glyphicons-halflings-regular.woff2"
      ],
      Directory "images" [
        ResourceFile NoExec "spicey-logo.png",
        ResourceFile NoExec "text.png",
        ResourceFile NoExec "time.png",
        ResourceFile NoExec "number.png",
        ResourceFile NoExec "foreign.png"
      ]
    ]
  ]

-- Replace every occurrence of "XXXCURRYBINXXX" by installDir++"/bin"
replaceCurryBin :: String -> String
replaceCurryBin [] = []
replaceCurryBin (c:cs)
  | c=='X' && take 13 cs == "XXCURRYBINXXX"
    = installDir ++ "/bin" ++ replaceCurryBin (drop 13 cs)
  | otherwise = c : replaceCurryBin cs

-- Replace every occurrence of "XXXPKGNAMEXXX" by first argument
replacePackageName :: String -> String -> String
replacePackageName _ [] = []
replacePackageName pn (c:cs)
  | c=='X' && take 12 cs == "XXPKGNAMEXXX"
    = pn ++ replacePackageName pn (drop 12 cs)
  | otherwise = c : replacePackageName pn cs

copyFileLocal :: FileMode -> String -> String -> String -> IO ()
copyFileLocal fmode path resource_dir filename = do
  let infile  = resource_dir </> filename
  let outfile = path </> filename
  system $ "cp \"" ++ infile ++ "\" \"" ++ outfile ++ "\""
  setFileMode fmode outfile

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
  let full_path = target_path </> filename
  ifNotExistsDo full_path $ do
    putStrLn $ "Creating file '" ++ full_path ++ "'..."
    copyFileLocal fmode target_path resource_dir filename
      
createStructure target_path resource_dir _ _ _
                (ResourcePatchFile fmode filename f) = do
  let full_path = target_path </> filename
  ifNotExistsDo full_path $ do
    putStrLn ("Creating file '" ++ full_path ++ "'...")
    cnt <- readFile (resource_dir </> filename)
    let outfile = target_path </> filename
    writeFile outfile (f cnt)
    setFileMode fmode outfile

createStructure target_path resource_dir erd termfile db_path
                (Directory dirname subtree) = do
  let full_path = target_path </> dirname
  ifNotExistsDo full_path $ do
    putStrLn $ "Creating directory '"++full_path++"'..."
    createDirectory full_path
  mapIO_ (createStructure full_path resource_dir erd termfile db_path) subtree

createStructure target_path _ erd termfile db_path
                (GeneratedFromERD generatorFunction) =
  generatorFunction termfile erd target_path db_path

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
    -- The directory containing the project generator:
    let resourcedir = packagePath </> "resource_files"
    exfile <- doesFileExist orgfile
    unless exfile $ error ("File `" ++ orgfile ++ "' does not exist!")
    termfile <- if ".curry" `isSuffixOf` orgfile ||
                   ".lcurry" `isSuffixOf` orgfile
                  then storeERDFromProgram orgfile
                  else return orgfile
    erd <- readERDTermFile termfile
    let pkgname = erdName erd
    createDirectoryIfMissing True pkgname
    createStructure pkgname resourcedir erd termfile dbfile
                    (spiceyStructure pkgname)
    when (orgfile /= termfile) $ do
      -- delete generated ERD term file:
      removeFile termfile
      -- save original ERD specification in src/Model directory:
      copyFile orgfile
               (pkgname </> "src" </> "Model" </> erdName erd ++ "_ERD.curry")
    putStrLn (helpText pkgname)

  helpText pkgname = unlines $
    [ take 70 (repeat '-')
    , "Source files for the application generated as Curry package '" ++
      pkgname ++ "'."
    , ""
    , "Please go into the package directory where the 'README.txt' file"
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