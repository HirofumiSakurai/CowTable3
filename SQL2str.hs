{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import           Control.Monad.Logger (runStderrLoggingT)
import           Data.Conduit            (awaitForever, ($$))
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Yesod

data App = App { appPool   :: ConnectionPool }

mkYesod "App" [parseRoutes|
/ HomeR GET
/sql2str SQL2strR GET
|]

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

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

getSQL2strR :: Handler TypedContent
getSQL2strR = do
  query' <- (runInputGet $ iopt textField "SQL")
  respondSourceDB typeJson $
    rawQuery (queryConv query') [] $$ awaitForever sendJSONs
    where
      queryConv (Just "") = defaultQuery
      queryConv (Just query) = query
      queryConv Nothing = defaultQuery
      defaultQuery = "SELECT to_json(kine) FROM kine where ownerId = 1"
      sendJSONs = do mapM_ (sendChunk . show)

numConn = 10
connStr = "host=localhost dbname=cow_table user=hirofumi password='hello'"
-- connStr = "host=my-db-instance.ch8pskdbgy45.ap-northeast-1.rds.amazonaws.com dbname=cow_table user=hirofumi password='############'"

main :: IO ()
main = do
  -- connStr' <- readFile "connStr.conf"
  -- connStr <- BS.pack $ connStr'
  runStderrLoggingT $ withPostgresqlPool connStr numConn $ \pool -> liftIO $ do
    warp 3001 $ App {appPool = pool}
