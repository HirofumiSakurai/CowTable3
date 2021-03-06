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
import qualified Data.Conduit.List as CL
import           Data.Conduit            (awaitForever, ($$), ($=), Conduit, Sink)
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

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend

    runDB action = do
        App pool <- getYesod
        runSqlPool action pool

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appPool

getHomeR :: Handler Html
getHomeR = do
  defaultLayout [whamlet|
<h1>表示項目の選択と、クエリの入力
<form action=@{SQL2strR} method="GET">
    SQL:
    <input name="SQL" type="text" value="select json_agg(kine) from kine where owner_id = 5">
    <p>
    <input type="submit" value="送信">
                 |]
--  defaultLayout $(widgetFile "CowTable5.hamlet")

getSQL2strR :: Handler TypedContent
getSQL2strR = do
  query' <- (runInputGet $ iopt textField "SQL")
  respondSourceDB typeJson $
    rawQuery (queryConv query') [] $= awaitForever sendJSONs
    where
      queryConv (Just "") = defaultQuery
      queryConv (Just query) = query
      queryConv Nothing = defaultQuery
      defaultQuery = "select json_agg(kine) from kine where owner_id = 5"

-- sendJSONs :: Monad m => [PersistValue] -> Conduit [PersistValue]  () m 
sendJSONs = \j -> mapM_ sendChunkPersist j
  where
    sendChunkPersist (PersistByteString bs) = sendChunkBS bs
    sendChunkPersist s                       = (sendChunk . show) s

numConn = 10
connStr = "host=localhost dbname=cow_table user=hirofumi password='hello'"
-- connStr = "host=my-db-instance.ch8pskdbgy45.ap-northeast-1.rds.amazonaws.com dbname=cow_table user=hirofumi password='XXXXXXXXXXXX'"

main :: IO ()
main = do
  -- connStr' <- readFile "connStr.conf"
  -- connStr <- BS.pack $ connStr'
  runStderrLoggingT $ withPostgresqlPool connStr numConn $ \pool -> liftIO $ do
    warp 3002 $ App {appPool = pool}
