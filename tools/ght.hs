module Main where

import Control.Monad (liftM, when)
import Control.Monad.Trans (liftIO)

import Data.Default
import Data.List (intersperse, sort)

import UI.Command

import Git.Commit
import Git.Pack

-- show-prefix, show-root use these
import System.FilePath hiding (normalise)
import System.Directory
import System.Posix.Files

-- show
import System.IO (stdout)
import Codec.Compression.Zlib
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Digest.Pure.SHA (sha1, showDigest)

------------------------------------------------------------
-- show-prefix
--

ghtShowPrefix :: Command ()

ghtShowPrefix = defCmd {
       	        cmdName = "show-prefix",
                cmdHandler = ghtShowPrefixHandler,
                cmdCategory = "Reporting",
                cmdShortDesc = "Show path from top-level directory of repo"
        }

ghtShowPrefixHandler = do
	path <- liftIO findRoot
	cwd <- liftIO $ getCurrentDirectory
	canPath <- liftIO $ canonicalizePath path
	let relPath = makeRelative canPath cwd
	liftIO $ putStrLn (relPath ++ [pathSeparator])

------------------------------------------------------------
-- show-root
--

ghtShowRoot :: Command ()

ghtShowRoot = defCmd {
       	        cmdName = "show-root",
                cmdHandler = ghtShowRootHandler,
                cmdCategory = "Reporting",
                cmdShortDesc = "Show path to top-level directory of repo"
        }

ghtShowRootHandler = do
	path <- liftIO findRoot
	liftIO $ putStrLn path

------------------------------------------------------------
-- findRoot
--

gitPath :: FilePath -> IO FilePath
gitPath f = do
	root <- findRoot
	return $ root </> ".git" </> f

findRoot :: IO FilePath
findRoot = do
	mp <- liftIO $ findRoot' "."
	case mp of
		Just path -> return (path ++ [pathSeparator])
		Nothing -> error "fatal: Not a git repository (or any of the parent directories)"

findRoot' :: FilePath -> IO (Maybe FilePath)
findRoot' path = do
	b <- fileExist path
	case b of
		True -> do
			d <- dirIsRoot path
			case d of
				True -> return (Just (normalise path))
				False -> do
					let newPath = ".." </> path
					canPath <- canonicalizePath path
					canNewPath <- canonicalizePath newPath
					if (canPath == canNewPath) then
						return Nothing
						else findRoot' newPath
		False -> return Nothing
	
dirIsRoot path = do
	let dotGit = path </> ".git"
	liftIO $ fileExist dotGit
	
------------------------------------------------------------
-- normalise
--

-- NOTE: this is a modified version of normalise from filepath,
-- fixed to handle the case of a trailing dot. This version was
-- submitted via the libraries process as ticket #3975:
-- http://hackage.haskell.org/trac/ghc/ticket/3975
-- which was applied on 08 Jan 2011.

-- | Normalise a file
--
-- * \/\/ outside of the drive can be made blank
--
-- * \/ -> 'pathSeparator'
--
-- * .\/ -> \"\"
--
-- > Posix:   normalise "/file/\\test////" == "/file/\\test/"
-- > Posix:   normalise "/file/./test" == "/file/test"
-- > Posix:   normalise "/test/file/../bob/fred/" == "/test/file/../bob/fred/"
-- > Posix:   normalise "../bob/fred/" == "../bob/fred/"
-- > Posix:   normalise "./bob/fred/" == "bob/fred/"
-- > Windows: normalise "c:\\file/bob\\" == "C:\\file\\bob\\"
-- > Windows: normalise "c:\\" == "C:\\"
-- > Windows: normalise "\\\\server\\test" == "\\\\server\\test"
-- > Windows: normalise "c:/file" == "C:\\file"
-- >          normalise "." == "."
-- > Posix:   normalise "./" == "./"
-- > Posix:   normalise "./." == "./"
-- > Posix:   normalise "bob/fred/." == "bob/fred/"
normalise :: FilePath -> FilePath
normalise path = joinDrive (normaliseDrive drv) (f pth)
              ++ [pathSeparator | isDirPath pth]
    where
        (drv,pth) = splitDrive path

        isDirPath xs = lastSep xs
            || not (null xs) && last xs == '.' && lastSep (init xs)
        lastSep xs = not (null xs) && isPathSeparator (last xs)

        f = joinPath . dropDots [] . splitDirectories . propSep

        propSep (a:b:xs)
         | isPathSeparator a && isPathSeparator b = propSep (a:xs)
        propSep (a:xs)
         | isPathSeparator a = pathSeparator : propSep xs
        propSep (x:xs) = x : propSep xs
        propSep [] = []

        dropDots acc xs | all (==".") xs = ["."]
        dropDots acc xs = dropDots' acc xs

        dropDots' acc (".":xs) = dropDots' acc xs
        dropDots' acc (x:xs) = dropDots' (x:acc) xs
        dropDots' acc [] = reverse acc

--joinDrive = ++
normaliseDrive = id

------------------------------------------------------------
-- branch
--

ghtBranch = defCmd {
       	        cmdName = "branch",
                cmdHandler = ghtBranchHandler,
                cmdCategory = "Blob management",
                cmdShortDesc = "show branches",
                cmdExamples = [("Show branches available", "")]
        }

ghtBranchHandler = do
        args <- appArgs
        liftIO $ showBranches args

showBranches _ = do
	path <- gitPath $ "refs" </> "heads"
	branches <- getDirectoryContents path
	let branches' = filter (/= ".") branches
	let branches'' = filter (/= "..") branches'
	hd <- derefFile "HEAD"
	mapM_ (showBranch hd) (sort branches'')

showBranch hd b = do
	ref <- derefFile $ "refs" </> "heads" </> b
	if (ref == hd) then
		putStr "* "
		else putStr "  "
	putStrLn b

------------------------------------------------------------
-- findBlob
--

findBlob [] = findBlob ["HEAD"]

findBlob (name:_) = do
	mPath <- firstExist [name,
                            ("refs" </> name),
                            ("refs" </> "tags" </> name),
                            ("refs" </> "heads" </> name),
                            ("refs" </> "remotes" </> name),
                            ("refs" </> "remotes" </> name </> "HEAD")]
	case mPath of
		Just path -> do
			bs <- derefFile path
			return [C.unpack bs]
		Nothing -> return [name]

firstExist :: [FilePath] -> IO (Maybe FilePath)
firstExist [] = return Nothing
firstExist (f:fs) = do
	p <- gitPath f
	b <- fileExist p
	if b then return (Just f) else firstExist fs

------------------------------------------------------------
-- log
--

ghtLog = defCmd {
       	        cmdName = "log",
                cmdHandler = ghtLogHandler,
                cmdCategory = "Blob management",
                cmdShortDesc = "Show commit logs",
                cmdExamples = [("Show log of current branch", ""), ("Show log of branch feature1", "feature1")]
        }

ghtLogHandler = do
        args <- appArgs
	b <- liftIO $ findBlob args
	liftIO $ showLog b

showLog (blob:_)
	| blob == "" = return ()
	| otherwise = do
		d <- readBlob blob
		let m'pb = prettyLog blob d
		case m'pb of
			Just c -> do
				let p = C.concat [commitHeader, C.pack (blob ++ "\n"), commitPretty c]
				L.hPut stdout p
				showLog [C.unpack $ commitParent c]
			Nothing -> return ()
	where
		commitHeader = C.pack "commit "

prettyLog blob bs
	| commitHeader `L.isPrefixOf` bs = Just c
        | otherwise = Nothing
	where
		commitHeader = C.pack "commit "
		c = commitParse bs

------------------------------------------------------------
-- show-pack
--

ghtShowPack = defCmd {
       	        cmdName = "show-pack",
                cmdHandler = ghtShowPackHandler,
                cmdCategory = "Blob management",
                cmdShortDesc = "Show the raw dump of a pack",
                cmdExamples = [("Show raw contents of pack pack-abcd.pack", "abcd")]
        }

ghtShowPackHandler = do
        args <- appArgs
	b <- liftIO $ findPack args
	-- liftIO $ L.hPut stdout b
	let p = prettyPack b
	liftIO $ L.hPut stdout p

findPack (pack:_) = do
	path <- gitPath ("objects" </> "pack" </> ("pack-" ++ pack ++ ".pack"))
	b <- L.readFile path
	return b

prettyPack bs
	| packHeader `L.isPrefixOf` bs = packPretty $ packParse bs
        | otherwise = error "Not a pack"
	where
		packHeader = C.pack "PACK"

------------------------------------------------------------
-- show-raw
--

ghtShowRaw = defCmd {
       	        cmdName = "show-raw",
                cmdHandler = ghtShowRawHandler,
                cmdCategory = "Blob management",
                cmdShortDesc = "Show the raw dump of an object",
                cmdExamples = [("Show raw contents of blob deadbeef", "deadbeef"), ("Show raw contents of branch feature1", "feature1")]
        }

ghtShowRawHandler = do
        args <- appArgs
	b <- liftIO $ findBlob args
	liftIO $ showRawBlob b

showRawBlob (blob:_) = do
	d <- readBlob blob
	L.hPut stdout d

------------------------------------------------------------
-- show
--

ghtShow = defCmd {
       	        cmdName = "show",
                cmdHandler = ghtShowHandler,
                cmdCategory = "Blob management",
                cmdShortDesc = "Show an object",
                cmdExamples = [("Show contents of blob deadbeef", "deadbeef"), ("Show contents of branch feature1", "feature1")]
        }

ghtShowHandler = do
        args <- appArgs
	b <- liftIO $ findBlob args
	liftIO $ showBlob b

readBlob blob = do
        let (bH,bT) = splitAt 2 blob
        path <- gitPath ("objects" </> bH </> bT)
	b <- L.readFile path
	return (decompress b)

showBlob (blob:_) = do
	d <- readBlob blob
	let pb = prettyBlob blob d
	L.hPut stdout pb

prettyBlob blob bs
	| commitHeader `L.isPrefixOf` bs = C.concat [commitHeader, C.pack (blob ++ "\n"), commitPretty $ commitParse bs]
        | otherwise = chomp bs
	where
		commitHeader = C.pack "commit "

derefFile f = do
	path <- gitPath f
	bs <- L.readFile path
	deref bs

deref bs
	| refHeader `L.isPrefixOf` bs = derefFile refPath
        | otherwise = return (chomp bs)
        where
		refHeader = C.pack "ref: "
		refPath = C.unpack (chomp $ L.drop 5 bs)

chomp = C.takeWhile (/= '\n')

------------------------------------------------------------
-- hash-object
--

ghtHashObject = defCmd {
       	        cmdName = "hash-object",
                cmdHandler = ghtHashObjectHandler,
                cmdCategory = "Blob management",
                cmdShortDesc = "Compute object ID from a file",
                cmdExamples = [("Compute the object ID of file.c", "file.c")]
        }

ghtHashObjectHandler = do
        args <- appArgs
	liftIO $ hashFile args

hashFile [] = return ()

hashFile (path:_) = do
        b <- L.readFile path
	status <- getFileStatus path	
	let h = C.pack $ "blob " ++ (show $ fileSize status)
	let t = h `L.append` (L.singleton 0x0) `L.append` b
        putStrLn $ showHash t

showHash = showDigest . sha1

------------------------------------------------------------
-- The Application
--

ght :: Application () ()
ght = def {
	        appName = "ght",
                appVersion = "0.1",
		appAuthors = ["Conrad Parker"],
                appBugEmail = "conrad@metadecks.org",
                appShortDesc = "Trivial git inspection tools",
                appLongDesc = longDesc,
	        appCategories = ["Reporting", "Blob management"],
		appSeeAlso = ["git"],
		appProject = "Ght",
	        appCmds = [ghtShowPrefix, ghtShowRoot, ghtShow, ghtLog, ghtShowRaw, ghtShowPack, ghtHashObject, ghtBranch]
	}

longDesc = "This is a bunch of trivial routines for inspecting git repositories. It is in no way useful beyond that."

------------------------------------------------------------
-- Main
--

main :: IO ()
main = appMain ght
