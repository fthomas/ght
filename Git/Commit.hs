module Git.Commit (
	Commit(..),
	commitParse,
	commitPretty
) where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.Digest.Pure.SHA (Digest, sha1, showDigest)

type Author = String

type Date = String

data Commit = Commit {
	commitParent :: C.ByteString, --Digest,
	commitAuthor :: C.ByteString, --Person,
	commitAuthorDate :: C.ByteString, --Date,
	commitCommitter :: C.ByteString, --Person,
	commitCommitterDate :: C.ByteString, --Date,
	commitMessage :: C.ByteString
}

------------------------------------------------------------
-- commitParse
--

e = C.empty

defCommit = Commit e e e e e e

commitParse bs = commitParseLines defCommit (C.lines bs)

commitParseLines c [] = c

commitParseLines c (l:ls)
	| C.null l = c{commitMessage = C.unlines ls}
	| otherwise = commitParseLines (commitModLine c l) ls

commitModLine c l = commitMod c (C.unpack hd) bdy
	where (hd, bdy) = C.span (/= ' ') l

commitMod c hd bdy
	| hd == "commit" = c
	| hd == "parent" = c{commitParent = bdy}
	| hd == "author" = c{commitAuthor = pName, commitAuthorDate = t}
	| hd == "committer" = c{commitCommitter = pName, commitCommitterDate = t}
	| otherwise = c
	where
		(pTZ:pTime:pAs) = reverse $ C.words bdy
		pName = C.unwords $ reverse pAs
		t = C.unwords [pTime, pTZ]

------------------------------------------------------------
-- commitPretty
--

commitPretty (Commit p a ad c cd m) =
	C.unlines [
		C.concat [(C.pack "Author: "), a],
		C.concat [(C.pack "Date:   "), ad],
		C.empty,
		m
	]
