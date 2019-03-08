{-# LANGUAGE OverloadedStrings #-}
module Data.OrgMode.Print where

import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Text as T (Text, pack, unlines)
import Data.OrgMode.Types
import Data.Monoid
import Data.Thyme.Format
import GHC.Natural (naturalToWordMaybe)
import System.Locale

formatDoc :: Document -> [Text]
formatDoc doc = ((formatHeadline) <$> documentHeadlines doc)

formatHeadline :: Headline -> Text
formatHeadline hs = 
     renderHeadline hs
  <> ifNonNilNewline (T.unlines (renderSection (pack $ take (getDepth $ depth hs) $ Prelude.repeat ' ') (section hs)))
  <> mconcat (formatHeadline <$> subHeadlines hs)

ifNonNilNewline t = case t of { "" -> ""; _ -> "\n" <> t }

renderHeadline :: Headline -> Text
renderHeadline h = do
  mconcat [pack (take (getDepth $ depth h) $ Prelude.repeat '*'), " ", title h]

renderSection :: Text -> Section -> [Text]
renderSection indent s = do
  concat (map renderContent (sectionContents s)) ++ [renderSectionPlanning indent $ sectionPlannings s]

renderContent :: Content -> [Text]
renderContent (OrderedList items) = map (\(Item i) -> "- " <> T.unlines (map (T.unlines . renderContent) i)) items
renderContent (UnorderedList items) = zipWith (\n (Item i) -> T.pack (show n) <> T.pack ". " <> T.unlines (map (T.unlines . renderContent) i)) [1..] items
renderContent (Paragraph paras) = [mconcat (map renderMarkupText paras)]
renderContent (Drawer nm cts) = [":" <> nm <> ":", cts, ":" <> nm <> ":"]
renderContent (Block type_ data_ cts) = ["#+begin_" <> renderBlockType type_ <> maybe "" (" "<>) data_] <> cts <> ["#+end_" <> renderBlockType type_]

renderBlockType :: BlockType -> Text
renderBlockType Comment = "comment"
renderBlockType Example = "example"
renderBlockType Export = "export"
renderBlockType Src = "src"

surround :: Text -> Text -> Text
surround c s = c <> s <> c

renderMarkupText :: MarkupText -> Text
renderMarkupText (Plain x) = x
renderMarkupText (LaTeX x) = surround "$" x -- TODO: backslashify newlines
renderMarkupText (Verbatim x) = surround "=" x
renderMarkupText (Code x) = surround "~" x
renderMarkupText (Bold xs) = surround "*" (mconcat (map renderMarkupText xs))
renderMarkupText (UnderLine xs) = surround "_" (mconcat (map renderMarkupText xs))
renderMarkupText (Italic xs) = surround "/" (mconcat (map renderMarkupText xs))
renderMarkupText (Strikethrough xs) = surround "+" (mconcat (map renderMarkupText xs))
renderMarkupText (HyperLink lnk (Just desc)) = "[[" <> lnk <> "][" <> desc <> "]]"
renderMarkupText (HyperLink lnk Nothing) = "[[" <> lnk <> "]]"

renderSectionPlanning :: Text -> [Planning] -> Text
renderSectionPlanning indent hm = case foldl' (\a (Planning k v) -> [indent, " [", textShow k, " ", renderTimestamp v, "]"] ++  a) [] hm  of
  [] -> ""
  xs -> mconcat $ "\n" : xs

renderTimestamp :: Timestamp -> Text
renderTimestamp ts = pack $ (formatTime defaultTimeLocale "%d/%m/%Y" $ yearMonthDay $ tsTime ts)

textShow :: Show a => a -> Text
textShow = pack . show

getDepth :: Depth -> Int
getDepth (Depth x) = fromIntegral (fromJust (naturalToWordMaybe x))
