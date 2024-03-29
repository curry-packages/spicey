-- CurryCheck test for Spicey web application framework

import System.Directory ( createDirectory, getCurrentDirectory
                        , setCurrentDirectory )
import System.FilePath  ( (</>) )
import System.Process   ( system )
import Test.Prop

import Spicey.PackageConfig ( packagePath, packageExecutable )

-- Generate and compile a Spicey application for a given ERD name.
compileSpiceyApplication :: String -> IO Int
compileSpiceyApplication erdname = do
  let testdir = packagePath </> "examples" </> "spicey_" ++ erdname
      spiceup = packageExecutable
  system $ "/bin/rm -rf " ++ testdir
  createDirectory testdir
  curdir <- getCurrentDirectory
  putStrLn $ "Generating Spicey application in directory '" ++ testdir ++ "'..."
  setCurrentDirectory testdir
  system $ spiceup ++ " " ++
           packagePath </> "examples" </> erdname ++ "ERD.curry"
  let makecall = "make WEBSERVERDIR=/tmp"
  putStrLn $ "Compiling Spicey application in directory '" ++ testdir ++ "'..."
  ecode <- system $ "cd " ++ erdname ++ " && " ++
                    makecall ++ " install && " ++
                    makecall ++ " CURRYOPTIONS=\":set parser -Wnone\" compile"
  setCurrentDirectory curdir
  system $ "/bin/rm -rf " ++ testdir
  return ecode

-- Compile the Blog example.
testCompileBlog :: PropIO
testCompileBlog = compileSpiceyApplication "Blog"  `returns` 0

-- Compile the Uni example.
testCompileUni :: PropIO
testCompileUni = compileSpiceyApplication "Uni"  `returns` 0
