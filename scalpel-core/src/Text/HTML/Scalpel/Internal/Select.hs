{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.HTML.Scalpel.Internal.Select (
    SelectContext (..)
,   TagSpec
,   TagInfo (..)

,   select
,   tagsToSpec
,   fromAttrib
,   isTagOpen
,   isTagSelfClose
) where

import Text.HTML.Scalpel.Internal.Select.Types

import Control.Applicative ((<|>))
import Data.Maybe (catMaybes, isJust, fromJust, fromMaybe, listToMaybe)

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Tree as Tree
import qualified Data.Vector as Vector

import qualified Text.HTML.Parser as HP


type Index = Int
type Name = Maybe T.Text
type CloseOffset = Maybe Index

-- | The span of a tag in terms of the index of the opening tag and the index of
-- the closing tag. If there is no closing tag the closing tag is equal to the
-- opening tag.
data Span = Span !Int !Int

-- | A representation of the hierarchal structure of a document. Nodes of the
-- tree are spans which mark the start and end of a tag. The tree is organized
-- such that tags that appear earlier in the parsed string appear earlier in the
-- list of nodes, and that a given node is completely within the span of its
-- parent.
type TagForest = Tree.Forest Span

-- | A tag and associated precomputed meta data that is accessed in tight inner
-- loops during scraping.
data TagInfo = TagInfo {
               infoTag    :: !(HP.Token)
             , infoName   :: !Name
             , infoOffset :: !CloseOffset
             }

-- | A vector of tags and precomputed meta data. A vector is used because it
-- allows for constant time slicing and sharing memory between the slices.
type TagVector = Vector.Vector TagInfo

-- | Ephemeral meta-data that each TagSpec is tagged with. This type contains
-- information that is not intrinsic in the sub-tree that corresponds to a given
-- TagSpec.
data SelectContext = SelectContext {
                     -- | `select` generates a list of `TagSpec`s that match a
                     -- selector. This indicates the index in the result list
                     -- that this TagSpec corresponds to.
                     ctxPosition :: !Index
                     -- | True if the current context is the result of chrooting
                     -- to a sub-tree. This is used by serial scrapers to
                     -- determine how to generate zippered sibling nodes.
                   , ctxInChroot :: !Bool
                   }

-- | A structured representation of the parsed tags that provides fast element
-- look up via a vector of tags, and fast traversal via a rose tree of tags.
type TagSpec = (TagVector, TagForest, SelectContext)

-- | The 'select' function takes a 'Selectable' value and a list of
-- 'HP.Tag's and returns a list of every subsequence of the given list of
-- Tags that matches the given selector.
select :: Selector -> TagSpec -> [TagSpec]
select s tagSpec = newSpecs
    where
        (MkSelector nodes) = s
        newSpecs =
            zipWith applyPosition [0..] (selectNodes nodes tagSpec tagSpec [])
        applyPosition p (tags, f, _) = (tags, f, SelectContext p True)

-- | Creates a TagSpec from a list of tags parsed by 'HP'.
tagsToSpec :: [HP.Token] -> TagSpec
tagsToSpec tags = (vector, tree, ctx)
    where
        vector = tagsToVector tags
        tree   = vectorToTree vector
        ctx    = SelectContext 0 False

-- | Annotate each tag with the offset to the corresponding closing tag. This
-- annotating is done in O(n * log(n)).
--
-- The algorithm works on a list of tags annotated with their index. It
-- maintains a map of unclosed open tags keyed by tag name.
--
--      (1) When an open tag is encountered it is pushed onto the list keyed by
--          its name.
--
--      (2) When a closing tag is encountered the corresponding opening tag is
--          popped, the offset between the two are computed, the opening tag is
--          annotated with the offset between the two, and both are added to the
--          result set.
--
--      (3) When any other tag is encountered it is added to the result set
--          immediately.
--
--      (4) After all tags are either in the result set or the state, all
--          unclosed tags from the state are added to the result set without a
--          closing offset.
--
--      (5) The result set is then sorted by their indices.
tagsToVector :: [HP.Token] -> TagVector
tagsToVector tags = let indexed  = zip tags [0..]
                        total    = length indexed
                        unsorted = go indexed Map.empty
                        emptyVec = Vector.replicate total undefined
                     in emptyVec Vector.// unsorted
    where
        go :: [(HP.Token, Index)]
           -> Map.Map T.Text [(HP.Token, Index)]
           -> [(Index, TagInfo)]
        go [] state =
                map (\(t, i) -> (i, TagInfo t (maybeName t) Nothing))
                              $ concat
                              $ Map.elems state
            where
                maybeName t | isTagOpen t      = Just $ getTagName t
                            | isTagSelfClose t = Just $ getTagName t
                            | isTagClose t     = Just $ getTagName t
                            | otherwise        = Nothing
        go ((tag, index) : xs) state
            | isTagClose tag =
                let maybeOpen = Map.lookup tagName state >>= listToMaybe
                    state'    = Map.alter popTag tagName state
                    info      = TagInfo tag (Just tagName) Nothing
                    res       = catMaybes [
                                  Just (index, info)
                              ,   calcOffset <$> maybeOpen
                              ]
                 in res <> go xs state'
            | isTagOpen tag || isTagSelfClose tag =
                go xs (Map.alter appendTag tagName state)
            | otherwise =
                let info = TagInfo tag Nothing Nothing
                in (index, info) : go xs state
            where
                tagName = getTagName tag

                appendTag :: Maybe [(HP.Token, Index)]
                          -> Maybe [(HP.Token, Index)]
                appendTag m = ((tag, index) :) <$> (m <|> Just [])

                calcOffset :: (HP.Token, Index) -> (Index, TagInfo)
                calcOffset (t, i) =
                    let offset = index - i
                        info   = TagInfo t (Just tagName) (Just offset)
                     in offset `seq` (i, info)

                popTag :: Maybe [a] -> Maybe [a]
                popTag (Just (_ : y : xs)) = let s = y : xs in s `seq` Just s
                popTag _                   = Nothing

getTagName :: HP.Token -> Text
getTagName (HP.TagOpen name _)      = name
getTagName (HP.TagSelfClose name _) = name
getTagName (HP.TagClose name)       = name
getTagName _                        = undefined

-- | Builds a forest describing the structure of the tags within a given vector.
-- The nodes of the forest are tag spans which mark the indices within the
-- vector of an open and close pair. The tree is organized such for any node n
-- the parent of node n is the smallest span that completely encapsulates the
-- span of node n.
vectorToTree :: TagVector -> TagForest
vectorToTree tags = fixup $ forestWithin 0 (Vector.length tags)
    where
        forestWithin :: Int -> Int -> TagForest
        forestWithin !lo !hi
            | hi <= lo   = []
            | shouldSkip = forestWithin (lo + 1) hi
            | otherwise  = Tree.Node (Span lo closeIndex) subForest
                         : forestWithin (closeIndex + 1) hi
            where
                info        = tags Vector.! lo
                isOpen      = isTagOpen $ infoTag info
                isText      = isTagText $ infoTag info
                isSelfClose = isTagSelfClose $ infoTag info
                shouldSkip  = not isOpen && not isText && not isSelfClose
                closeIndex  = lo + fromMaybe 0 (infoOffset info)
                subForest   = forestWithin (lo + 1) closeIndex

        -- Lifts nodes whose closing tags lay outside their parent tags up to
        -- within a parent node that encompasses the node's entire span.
        fixup :: TagForest -> TagForest
        fixup [] = []
        fixup (Tree.Node (Span lo hi) subForest : siblings)
            = Tree.Node (Span lo hi) ok : bad
            where
                (ok, bad) = malformed (fixup siblings) $ fixup subForest

                malformed :: TagForest -- Forest to prepend bad trees on.
                          -> TagForest  -- Remaining trees to examine.
                          -> (TagForest, TagForest)
                malformed preBad [] = ([], preBad)
                malformed preBad (n@(Tree.Node (Span _ nHi) _) : ns)
                    | hi < nHi  = (ok, n : bad)
                    | otherwise = (n : ok, bad)
                    where (ok, bad) = malformed preBad ns

-- | Generates a list of 'TagSpec's that match the given list of 'SelectNode's.
-- This is is done in linear time with respect to the number of tags.
--
-- The algorithm is a simple DFS traversal of the tag forest. While traversing
-- the forest if the current SelectNode is satisfied by the current node in the
-- tree the SelectNode is popped and the current node's sub-forest is traversed
-- with the remaining SelectNodes. If there is only a single SelectNode then any
-- node encountered that satisfies the SelectNode is returned as an answer.
selectNodes ::
            [(SelectNode, SelectSettings)]
            -> TagSpec
            -> TagSpec
            -> [TagSpec]
            -> [TagSpec]
selectNodes []  _          _ acc = acc
selectNodes [_] (_, [], _) _ acc = acc
-- Now that there is only a single SelectNode to satisfy, search the remaining
-- forests and generates a TagSpec for each node that satisfies the condition.
selectNodes [n] cur@(tags, f : fs, ctx) root acc
    | MatchOk == matchResult
        = (shrunkSpec :)
        $ selectNodes [n] (tags, fs, ctx) root
        $ selectNodes [n] (tags, Tree.subForest f, ctx) root acc
    | MatchCull == matchResult
        = selectNodes [n] (tags, fs, ctx) root acc
    | otherwise
        = selectNodes [n] (tags, Tree.subForest f, ctx) root
        $ selectNodes [n] (tags, fs, ctx) root acc
    where
        Span lo hi = Tree.rootLabel f
        shrunkSpec = (
                       Vector.slice lo (hi - lo + 1) tags
                     , [fmap recenter f]
                     , ctx
                     )
        recenter (Span nLo nHi) = Span (nLo - lo) (nHi - lo)
        info = tags Vector.! lo
        matchResult = nodeMatches n info cur root
-- There are multiple SelectNodes that need to be satisfied. If the current node
-- satisfies the condition, then the current nodes sub-forest is searched for
-- matches of the remaining SelectNodes.
selectNodes (_ : _) (_, [], _) _ acc = acc
selectNodes (n : ns) cur@(tags, f : fs, ctx) root acc
    | MatchOk == matchResult
        = selectNodes ns       (tags, Tree.subForest f ++ siblings, ctx)
                               -- The new root node is the just matched node
                               -- plus the lifted siblings.
                               (tags, f : siblings, ctx)
        $ selectNodes (n : ns) (tags, fs, ctx) root acc
    | MatchCull == matchResult
        = selectNodes (n : ns) (tags, fs, ctx) root acc
    | otherwise
        = selectNodes (n : ns) (tags, Tree.subForest f, ctx) root
        $ selectNodes (n : ns) (tags, fs, ctx) root acc
    where
        Span lo hi = Tree.rootLabel f
        info = tags Vector.! lo
        matchResult = nodeMatches n info cur root

        -- In the case of a match, it is possible that there are children nested
        -- within the sibling forests that are potentially valid matches for the
        -- current node despite not being direct children. This can happen with
        -- malformed HTML, for example <a><b><c></c><a></b>. In this case <c>
        -- would be a child of <b> which would be a sibling of <a>.
        --
        -- In order to match <c> it must be lifted out of <b>'s sub forest when
        -- matching <a>.
        treeLo tree | (Span lo _) <- Tree.rootLabel tree = lo
        treeHi tree | (Span _ hi) <- Tree.rootLabel tree = hi

        liftSiblings []       acc = acc
        liftSiblings (t : ts) acc
          | lo < treeLo t && treeHi t < hi = t : liftSiblings ts acc
          | hi < treeLo t || treeHi t < lo = liftSiblings ts acc
          | otherwise                      = liftSiblings (Tree.subForest t)
                                           $ liftSiblings ts acc
        siblings = liftSiblings fs []

-- | The result of nodeMatches, can either be a match, a failure, or a failure
-- that culls all children of the current node.
data MatchResult = MatchOk | MatchFail | MatchCull
  deriving (Eq)

-- | Ands together two MatchResult values.
andMatch :: MatchResult -> MatchResult -> MatchResult
andMatch MatchOk MatchOk = MatchOk
andMatch MatchCull _     = MatchCull
andMatch _ MatchCull     = MatchCull
andMatch _ _             = MatchFail

-- | Turns a boolean value into a MatchResult.
boolMatch :: Bool -> MatchResult
boolMatch True  = MatchOk
boolMatch False = MatchFail

-- | Returns True if a tag satisfies a given SelectNode's condition.
nodeMatches ::
            (SelectNode, SelectSettings)
            -> TagInfo
            -> TagSpec
            -> TagSpec
            -> MatchResult
nodeMatches (SelectNode node preds, settings) info cur root =
    checkSettings settings cur root `andMatch` checkTag node preds info
nodeMatches (SelectAny preds      , settings) info cur root =
    checkSettings settings cur root `andMatch` checkPreds preds (infoTag info)
nodeMatches (SelectText           , settings) info cur root =
    checkSettings settings cur root `andMatch`
    boolMatch (isTagText $ infoTag info)

-- | Given a SelectSettings, the current node under consideration, and the last
-- matched node, returns true IFF the current node satisfies all of the
-- selection settings.
checkSettings :: SelectSettings -> TagSpec -> TagSpec -> MatchResult
checkSettings (SelectSettings (Just depth))
              (_, curRoot : _, _)
              (_, root, _)
  | depthOfCur < depth = MatchFail
  | depthOfCur > depth = MatchCull
  | otherwise          = MatchOk
  where
      Span curLo curHi = Tree.rootLabel curRoot
      mapTree f = map f . concatMap Tree.flatten
      depthOfCur = sum $ mapTree oneIfContainsCur root
      oneIfContainsCur (Span lo hi)
          | lo < curLo && curHi < hi = 1
          | otherwise = 0
checkSettings (SelectSettings _) _ _ = MatchOk

-- | Given a tag name and a list of attribute predicates return a function that
-- returns true if a given tag matches the supplied name and predicates.
checkTag :: T.Text -> [AttributePredicate] -> TagInfo -> MatchResult
checkTag name preds (TagInfo tag tagName _)
      =  boolMatch (
          (isTagOpen tag || isTagSelfClose tag)
        && isJust tagName
        && name == fromJust tagName
      ) `andMatch` checkPreds preds tag

-- | Returns True if a tag satisfies a list of attribute predicates.
checkPreds :: [AttributePredicate] -> HP.Token -> MatchResult
checkPreds []    tag = boolMatch
                     $ isTagOpen tag || isTagSelfClose tag || isTagText tag
checkPreds preds tag = boolMatch
                     $ (isTagOpen tag || isTagSelfClose tag) && all (`checkPred` attrs) preds
    where attrs = case tag of
                    (HP.TagOpen _ attrs) -> attrs
                    (HP.TagSelfClose _ attrs) -> attrs
                    _ -> []

isTagOpen :: HP.Token -> Bool
isTagOpen (HP.TagOpen _ _) = True; isTagOpen _ = False

isTagSelfClose :: HP.Token -> Bool
isTagSelfClose (HP.TagSelfClose _ _ ) = True; isTagSelfClose _ = False

isTagClose :: HP.Token -> Bool
isTagClose (HP.TagClose _) = True; isTagClose _ = False

isTagText :: HP.Token -> Bool
isTagText (HP.ContentText _) = True; isTagText _ = False

fromAttrib :: Text -> HP.Token -> Text
fromAttrib att tag = fromMaybe T.empty $ maybeAttrib att tag

maybeAttrib :: Text -> HP.Token -> Maybe HP.AttrValue
maybeAttrib att (HP.TagOpen _ atts) = lookup att $ map unwrap atts
  where unwrap (HP.Attr n v) = (n, v)
maybeAttrib att (HP.TagSelfClose _ atts) = lookup att $ map unwrap atts
  where unwrap (HP.Attr n v) = (n, v)

maybeAttrib _ x = error ("(" ++ show x ++ ") is not a TagOpen")
