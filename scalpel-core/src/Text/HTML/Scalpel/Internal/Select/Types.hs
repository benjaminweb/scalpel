{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_HADDOCK hide #-}

module Text.HTML.Scalpel.Internal.Select.Types (
    Selector (..)
,   AttributePredicate (..)
,   checkPred
,   AttributeName (..)
,   matchKey
,   anyAttrPredicate
,   TagName (..)
,   SelectNode (..)
,   tagSelector
,   anySelector
,   textSelector
,   toSelectNode
,   SelectSettings (..)
,   defaultSelectSettings
) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Text.HTML.Parser as HP
import Data.String (IsString, fromString)

-- | The 'AttributeName' type can be used when creating 'Selector's to specify
-- the name of an attribute of a tag.
data AttributeName = AnyAttribute | AttributeString Text

matchKey :: AttributeName -> Text -> Bool
matchKey (AttributeString s) = (T.toLower s ==)
matchKey AnyAttribute = const True

instance IsString AttributeName where
  fromString = AttributeString . T.pack

-- | An 'AttributePredicate' is a method that takes a 'HP.Attr' and
-- returns a 'Bool' indicating if the given attribute matches a predicate.
data AttributePredicate
        = MkAttributePredicate
                ([HP.Attr]
                                                    -> Bool)

checkPred :: AttributePredicate -> [HP.Attr] -> Bool
checkPred (MkAttributePredicate p) = p

-- | Creates an 'AttributePredicate' from a predicate function of a single
-- attribute that matches if any one of the attributes matches the predicate.
anyAttrPredicate :: (HP.Attr -> Bool)
                 -> AttributePredicate
anyAttrPredicate p = MkAttributePredicate $ any p

-- | 'Selector' defines a selection of an HTML DOM tree to be operated on by
-- a web scraper. The selection includes the opening tag that matches the
-- selection, all of the inner tags, and the corresponding closing tag.
newtype Selector = MkSelector [(SelectNode, SelectSettings)]

-- | 'SelectSettings' defines additional criteria for a Selector that must be
-- satisfied in addition to the SelectNode. This includes criteria that are
-- dependent on the context of the current node, for example the depth in
-- relation to the previously matched SelectNode.
data SelectSettings = SelectSettings {
  -- | The required depth of the current select node in relation to the
  -- previously matched SelectNode.
  selectSettingsDepth :: Maybe Int
} deriving Show

defaultSelectSettings :: SelectSettings
defaultSelectSettings = SelectSettings {
  selectSettingsDepth = Nothing
}

tagSelector :: Text -> Selector
tagSelector tag = MkSelector [
    (toSelectNode (TagString tag) [], defaultSelectSettings)
  ]

-- | A selector which will match any node (including tags and bare text).
anySelector :: Selector
anySelector = MkSelector [(SelectAny [], defaultSelectSettings)]

-- | A selector which will match all text nodes.
textSelector :: Selector
textSelector = MkSelector [(SelectText, defaultSelectSettings)]

instance IsString Selector where
  fromString = tagSelector . T.pack

data SelectNode = SelectNode !T.Text [AttributePredicate]
                | SelectAny [AttributePredicate]
                | SelectText

-- | The 'TagName' type is used when creating a 'Selector' to specify the name
-- of a tag.
data TagName = AnyTag | TagString Text

instance IsString TagName where
  fromString = TagString . T.pack

toSelectNode :: TagName -> [AttributePredicate] -> SelectNode
toSelectNode AnyTag = SelectAny
toSelectNode (TagString t) = SelectNode (T.toLower t)
