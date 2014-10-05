{-# LANGUAGE TemplateHaskell, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Call.TH
-- Copyright   :  (c) Fumiaki Kinoshita 2014
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Utilities that use Template Haskell
--
-----------------------------------------------------------------------------
module Call.TH (
    loadBitmapsWith
    ) where

import Control.Applicative
import Control.Monad
import Data.Char
import qualified Call.Data.Bitmap as Bitmap
import Language.Haskell.TH
import System.Directory
import System.FilePath
import System.IO.Unsafe

-- | The type of the given 'ExpQ' must be @FilePath -> IO FilePath@
-- FIXME: This may cause name duplication if there are multiple non-alphanumeric file names.
loadBitmapsWith :: ExpQ -> FilePath -> Q [Dec]
loadBitmapsWith getFullPath path = do
    loc <- (</>path) <$> takeDirectory <$> loc_filename <$> location
    paths <- runIO $ getFileList loc
    
    sequence $ do
        p <- paths
        let name = pathToName p
        [ return $ SigD (mkName name) (ConT ''Bitmap.Bitmap)
            , funD (mkName name) [clause [] (normalB $ load name $ loc </> p) []]
            ]
    where
        load name fp = do
            runIO $ putStrLn $ "Defined: " ++ fp ++ " as `" ++ name ++ "'"

            appE (varE 'unsafePerformIO) $ uInfixE (appE getFullPath $ litE $ StringL fp)
                (varE '(>>=))
                (varE 'Bitmap.readFile)

-- | Load and define all pictures in the specified directory.
-- On base >= 4.6, file paths to actually load will be respect to the directory of the executable. Otherwise it will be based on the current directory.


getFileList :: FilePath -> IO [FilePath]
getFileList path = do
    allContents <- filter notHidden `fmap` getDirectoryContents path

    files <- filterM (doesFileExist . (path</>)) allContents
    dirs <- filterM (doesDirectoryExist . (path</>)) allContents
    fmap ((files++).concat) $ forM dirs $ \i -> map (i</>) `fmap` getFileList (path</>i)
    where
        notHidden ('.':_) = False
        notHidden _ = True

pathToName :: FilePath -> String
pathToName = ('_':) . map p where
    p c | isAlphaNum c = c
        | otherwise = '_'
