
fixComments :: AcidState Blog -> IO ()
fixComments acid = do
    entriesDE <- query' acid $ LatestEntries DE
    entriesEN <- query' acid $ LatestEntries EN
    filterComments entriesDE
    filterComments entriesEN
  where
    (cDate :: UTCTime) = fromJust $ parseTime defaultTimeLocale "%d.%m.%Y %T" "22.04.2012 21:57:35"
    foldOp :: [(EntryId, [UTCTime])] -> Entry -> [(EntryId, [UTCTime])]
    foldOp l e = let c = map cdate $ filter (\c1 -> cdate c1 > cDate) $ comments e
                 in if null c then l
                              else (entryId e, c) : l
    pred :: Entry -> Bool
    pred e = let f eId [] = False
                 f eId (c:r) = if (cdate c > cDate) then True
                                                    else f eId r
             in f (entryId e) (comments e)
    filterComments entries = mapM_ removeComments $ foldl foldOp [] $ filter pred entries
    removeComments :: (EntryId, [UTCTime]) -> IO ()
    removeComments (eId, comments) = mapM_ (\c -> update' acid $ DeleteComment eId c) comments