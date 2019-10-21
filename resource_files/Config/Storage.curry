------------------------------------------------------------------------------
--- Some configurations where data is stored.
------------------------------------------------------------------------------

module Config.Storage where

import FilePath     ( (</>) )

--- Prefix a file name with the directory where global form data
--- is stored during run-time.
inDataDir :: String -> String
inDataDir filename = "data" </> filename
