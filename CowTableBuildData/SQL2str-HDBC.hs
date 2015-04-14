{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import qualified Database.HDBC as DB
import           Database.HDBC.PostgreSQL
import           Yesod
import           Text.Blaze.Internal (preEscapedText)

data App = App {connection :: Connection}

mkYesod "App" [parseRoutes|
/ HomeR GET
/sql2str SQL2strR GET
|]

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance ToJSON [(String, DB.SqlValue)] where
  toJSON row = object $ map toJSONRow row
      where
        toJSONRow (n, (DB.SqlString s)) = ((T.pack (n++":s")) .= T.pack s)
        toJSONRow (n, (DB.SqlByteString s)) = ((T.pack (n++":b")) .= decodeUtf8 s)
        toJSONRow (n, (DB.SqlChar c)) = ((T.pack (n++":c")) .= (T.pack $ [c]))
        toJSONRow (n, x ) = ((T.pack (n++":default")) .= (toText x))
        toText _ = T.pack "<undefined>"

getHomeR :: Handler Html
getHomeR = do
  defaultLayout [whamlet|
<h1>表示項目の選択と、クエリの入力
<form action=@{SQL2strR} method="GET">
    SQL:
    <input name="SQL" type="text" value="SELECT to_json(*) FROM cow_table where name LIKE 'ハナ%'">
    <p>
    <input type="submit" value="送信">
                 |]
--  defaultLayout $(widgetFile "CowTable5.hamlet")

getSQL2strR :: Handler Html
getSQL2strR = do
  query <- (runInputGet $ iopt textField "SQL")
  App conn <- getYesod
  strss' <- liftIO $ do
    stmt <- DB.prepare conn $ queryConv query
    DB.execute stmt []
    DB.fetchAllRows' stmt
  let text = T.concat $ map (T.concat . (map toText)) strss'
  defaultLayout [whamlet|[#{preEscapedText text}]|]
    where
      queryConv (Just "")  = defaultStr
      queryConv (Just t) = T.unpack t
      queryConv Nothing  = defaultStr
      defaultStr = "SELECT to_json(kine) FROM kine where owner_id = 5"
      toText (DB.SqlString s) = T.pack s
      toText (DB.SqlByteString bs) = decodeUtf8 bs
      toText (DB.SqlChar c) = T.pack $ [c]
      toText _ = T.pack "<undefined>"

main :: IO ()
main = do
  conn <- connectPostgreSQL "host=localhost dbname=cow_table user=hirofumi password='hello'"
  tables <-DB.getTables conn
  tabCol <- mapM (describeList conn) tables
  print tabCol
  warp 3000 $ App {connection = conn}
  DB.disconnect conn
    where
      describeList conn tableName = do
        desc <- DB.describeTable conn tableName
        let labels = map fst desc
        return (tableName, labels)
