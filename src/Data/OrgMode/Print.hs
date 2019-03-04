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
  <> T.unlines (renderSection (pack $ take (getDepth $ depth hs) $ Prelude.repeat ' ') (section hs))
  <> mconcat (formatHeadline <$> subHeadlines hs)

renderHeadline :: Headline -> Text
renderHeadline h = do
  mconcat [pack (take (getDepth $ depth h) $ Prelude.repeat '*'), " ", title h]

renderSection :: Text -> Section -> [Text]
renderSection indent s = do
  map (T.unlines . renderContent indent) (sectionContents s) ++ [renderSectionPlanning indent $ sectionPlannings s]

renderContent :: Text -> Content -> [Text]
renderContent indent (OrderedList items) = map (\(Item i) -> "- " <> T.unlines (map (T.unlines . renderContent indent) i)) items
renderContent indent (UnorderedList items) = zipWith (\n (Item i) -> T.pack (show n) <> T.pack ". " <> T.unlines (map (T.unlines . renderContent indent) i)) [1..] items
renderContent indent (Paragraph paras) = map ((indent <>) . renderMarkupText) paras
renderContent indent (Drawer nm cts) = [indent <> ":" <> nm <> ":", cts, indent <> ":" <> nm <> ":"]
renderContent indent (Block type_ data_ cts) = [indent <> "#+begin_" <> renderBlockType type_ <> maybe "" (<>" ") data_] <> cts <> [indent <> "#+end_" <> renderBlockType type_]

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
