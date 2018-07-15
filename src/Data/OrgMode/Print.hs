{-# LANGUAGE OverloadedStrings #-}
module Data.OrgMode.Print where

import Data.Text as T (Text, pack, lines, unlines)
import Data.OrgMode.Types
import Data.Monoid
import Data.HashMap.Strict
import Data.Thyme.Format
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
  T.lines (sectionParagraph s) ++ [renderSectionPlanning (indent) $ sectionPlannings s]

renderSectionPlanning :: Text -> Plannings -> Text
renderSectionPlanning indent (Plns hm) = case foldlWithKey' (\a k v -> [indent, " [", textShow k, " ", renderTimestamp v, "]"] ++  a) [] hm  of
  [] -> ""
  xs -> mconcat $ "\n" : xs

renderTimestamp :: Timestamp -> Text
renderTimestamp ts = pack $ (formatTime defaultTimeLocale "%d/%m/%Y" $ yearMonthDay $ tsTime ts)

textShow :: Show a => a -> Text
textShow = pack . show

getDepth :: Depth -> Int
getDepth (Depth x) = x
