-- |ZDD.hs
--
-- Naive ZDD implementation.
--
-- Node must have IDs that point to nodes with smaller variables.
--
-- Copyright (C) 2021, 2022 Serguey Zefirov
module ZDD where

import Control.Monad
import Control.Monad.State

import Data.Bits

import qualified Data.List as List

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Word

data ID = ID Int
	deriving (Eq, Ord, Read, Show)

id0, id1 :: ID
id0 = ID 0
id1 = ID 1

data ZDDN = Empty | One | Node !Int !ID !ID -- Node var present absent
	deriving (Eq, Ord, Read, Show)

data IDP = IDP !ID !ID
	deriving (Eq, Ord, Read, Show)

data ZDDS = ZDDS
	{ zddsCounter		:: !Int
	, zddsByID		:: !(Map.Map ID ZDDN)
	, zddsByNode		:: !(Map.Map ZDDN ID)
	, zddsUnionCache
	, zddsDiffCache
	, zddsDistributeCache
	, zddsIntersectCache	:: !(Map.Map IDP ID)
	}
	deriving (Show)

emptyZDDS :: ZDDS
emptyZDDS = ZDDS
	{ zddsCounter		= 2
	, zddsByID		= Map.fromList [(id0, Empty), (id1, One)]
	, zddsByNode		= Map.fromList [(Empty, id0), (One, id1)]
	, zddsUnionCache	= Map.empty
	, zddsDiffCache		= Map.empty
	, zddsDistributeCache	= Map.empty
	, zddsIntersectCache	= Map.empty
	}

type ZDDM a = StateT ZDDS IO a

getID :: ZDDN -> ZDDM ID
getID Empty = return id0
getID One = return id1
getID node@(Node var p a)
	| p == id0 = return a
	| otherwise = do
		mbID <- Map.lookup node . zddsByNode <$> get
		case mbID of
			Just id -> return id
			Nothing -> do
				zdds <- get
				let	new = zddsCounter zdds
					newID = ID new
				when True $ do
					let check i = do
						when (i /= id0 && i /= id1) $ do
							Node v' _ _ <- fetchNode i
							when (v' >= var) $ error "variable ordering has been violated"
					check p
					check a
				put $ zdds
					{ zddsCounter = new + 1
					, zddsByID = Map.insert newID node $ zddsByID zdds
					, zddsByNode = Map.insert node newID $ zddsByNode zdds
					}
				return newID

fetchNode :: ID -> ZDDM ZDDN
fetchNode id = do
	Map.findWithDefault (error $ "unable to find node by ID, " ++ show id) id . zddsByID <$> get

mkSet :: [Int] -> ZDDM ID
mkSet elems' = loop id1 elems
	where
		elems = List.sort elems'
		loop id [] = return id
		loop id (v:vs) = do
			id <- getID (Node v id id0)
			loop id vs

member :: ID -> [Int] -> ZDDM Bool
member root set
	| root == id0 = return False
	| root == id1 = return $ null set
	| otherwise = check root $ reverse $ List.sort set
	where
		check root xs
			| root == id0 = return False -- empty set does not contain anything,
			| root == id1 = return $ null xs	-- singleton set does not contain non-singleton set.
			| otherwise = do
				node <- fetchNode root
				case (xs, node) of
					([], Node _ _ a) -> check a xs -- look at the "element is not in set" part.
					(y:ys, Node v p a)
						| v == y -> check p ys -- check whether set--of-sets that contains v also contains rest.
						| otherwise -> check a xs

cached :: (ZDDS -> Map.Map IDP ID, IDP -> ID -> ZDDS -> ZDDS)
	-> ID -> ID -> ZDDM ID -> ZDDM ID
cached (fetch, set) a b compute = do
	mbCached <- Map.lookup (IDP a b) . fetch <$> get
	case mbCached of
		Just result -> return result
		_ -> do
			r <- compute
			modify' $ set (IDP a b) r
			return r

displayAllSets' :: (Int -> Int) -> String -> ID -> ZDDM ()
displayAllSets' interpret msg root = do
	liftIO $ putStrLn $ msg ++ ": root "++show root++":"
	(sets, _) <- enumerate Map.empty root
	liftIO $ case sets of
		[] -> putStrLn $ "    <empty>"
		_ -> forM_ sets $ \set -> putStrLn $ "    " ++ show set
	where
		enumerate visited id
			| id == id0 = return ([], visited)
			| id == id1 = return ([[]], visited)
			| Just sets <- Map.lookup id visited = return (sets, visited)
			| otherwise = do
				Node var present absent <- fetchNode id
				(setsp, visitedp) <- enumerate visited present
				(setsa, visiteda) <- enumerate visitedp absent
				return (setsa ++ map (interpret var:) setsp, visiteda)

displayAllSets :: String -> ID -> ZDDM ()
displayAllSets = displayAllSets' id

union :: ID -> ID -> ZDDM ID
union a b
	| a > b = union b a
	| a == b = return a
	| a == id0 = return b
	| otherwise = cached (fetch, set) a b $ do
		if a == id1
			then do
				Node var present absent <- fetchNode b
				p1 <- union present a
				a1 <- union absent a
				getID $ Node var p1 a1
			else do
				Node vara pa aa <- fetchNode a
				Node varb pb ab <- fetchNode b
				case compare vara varb of
					EQ -> do
						pab <- union pa pb
						aab <- union aa ab
						getID $ Node vara pab aab
					GT -> do
						-- we construct pseudonode Node vara id0 b
						pab <- union pa id0
						aab <- union aa b
						getID $ Node vara pab aab
					LT -> do
						-- here pseudonode is Node varb id0 a
						pab <- union id0 pb
						aab <- union a ab
						getID $ Node varb pab aab
	where
		fetch = zddsUnionCache
		set idp id zdds = zdds { zddsUnionCache = Map.insert idp id $ zddsUnionCache zdds }

usedSet :: [ID] -> ZDDM (Set.Set ID)
usedSet roots = foldM down Set.empty roots
	where
		down visited root
			| root == id0 || root == id1 || Set.member root visited = return visited
			| otherwise = do
				Node _ p a <- fetchNode root
				onP <- down (Set.insert root visited) p
				down onP a

garbageCollect :: [ID] -> ZDDM ()
garbageCollect roots = do
	--liftIO $ putStrLn $ "gc: roots: "++show roots
	shouldKeepIDs <- Set.insert id0 . Set.insert  id1 <$> usedSet roots
	--liftIO $ putStrLn $ "gc: ids to keep: "++show shouldKeepIDs
	let	mapShouldKeepIDs = Map.fromSet (const ()) shouldKeepIDs
	shouldKeepNodes <- Map.foldr Set.insert Set.empty . flip Map.intersection mapShouldKeepIDs . zddsByID <$> get
	let	mapShouldKeepNodes = Map.fromSet (const ()) shouldKeepNodes
	modify $ \zdds -> zdds
		{ zddsByID		= flip Map.intersection mapShouldKeepIDs $ zddsByID zdds
		, zddsByNode		= flip Map.intersection mapShouldKeepNodes $ zddsByNode zdds
		, zddsUnionCache	= Map.empty
		, zddsDiffCache		= Map.empty
		, zddsDistributeCache	= Map.empty
		, zddsIntersectCache	= Map.empty
		}
	where
{-
	{ zddsCounter		= 2
	, zddsByID		= Map.fromList [(id0, Empty), (id1, One)]
	, zddsByNode		= Map.fromList [(Empty, id0), (One, id1)]
	, zddsByVar		= Map.empty
	, zddsReferredBy	= Map.empty
	, zddsUnionCache	= Map.empty
	, zddsDiffCache		= Map.empty
	, zddsDistributeCache	= Map.empty
	, zddsIntersectCache	= Map.empty
-}

runWithNodes :: Map.Map ID ZDDN -> ZDDM a -> IO a
runWithNodes nodes act = do
	flip evalStateT (error "construct  with nodes") act
