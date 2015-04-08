{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

import           Data.List.Split (wordsBy)
import qualified Data.Text as T
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad
import           Control.Monad.Logger (runStderrLoggingT)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.), valkey)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Data.Time (Day, fromGregorian, showGregorian)
import           Data.Time.Clock
import           Data.Conduit            (awaitForever, ($=))
import           Yesod

data App = App
             { appPool   :: ConnectionPool }

--  "%Y-%m-%d" like "2000-01-02"
instance FromJSON Day where
    parseJSON (String t) = return $ fromGregorian y m d
      where
        [y, m', d'] = map (read.(T.unpack)) $ T.split (== '-') t
        m = (fromIntegral m') :: Int
        d = (fromIntegral d') :: Int

instance ToJSON Day where
    toJSON day = String (T.pack $ showGregorian day)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Owner json
    name  T.Text
Cow json
    earNum Int
    name   T.Text
    birth Day
    sex    T.Text
    ownerId OwnerId
    t1     Int Maybe
    t2     Int Maybe
    t3     Int Maybe
    t4     Int Maybe
    t5     Int Maybe
    t6     Int Maybe
    t7     Int Maybe
    t8     Int Maybe
    t9     Int Maybe
    t10    Int Maybe
    t11    Int Maybe
    t12    Int Maybe
    t13    Int Maybe
    t14    Int Maybe
    t15    Int Maybe
    t16    Int Maybe
    t17    Int Maybe
    t18    Int Maybe
    t19    Int Maybe
    t20    Int Maybe
    t21    Int Maybe
    t22    Int Maybe
    t23    Int Maybe
    t24    Int Maybe
    t25    Int Maybe
    t26    Int Maybe
    t27    Int Maybe
    t28    Int Maybe
    t29    Int Maybe
    t30    Int Maybe
    t31    Int Maybe
    t32    Int Maybe
    t33    Int Maybe
    t34    Int Maybe
    t35    Int Maybe
    t36    Int Maybe
    t37    Int Maybe
    t38    Int Maybe
    t39    Int Maybe
    t40    Int Maybe
    t41    Int Maybe
    t42    Int Maybe
    t43    Int Maybe
    t44    Int Maybe
    t45    Int Maybe
    t46    Int Maybe
    t47    Int Maybe
    t48    Int Maybe
    t49    Int Maybe
    t50    Int Maybe
|]


mkYesod "App" [parseRoutes|
/ HomeR GET
/eSelect ESelectR GET
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

data CowQuery = CowQuery {
  ear :: Maybe Int, name :: Maybe T.Text, sex :: T.Text, ownerId :: Maybe T.Text,
  t0 :: Bool, t1 :: Bool, t2 :: Bool, t3 :: Bool, t4 :: Bool }

cowForm :: Html -> MForm Handler (FormResult CowQuery, Widget)
cowForm = renderDivs $ CowQuery
    <$> aopt intField "ear" Nothing
    <*> aopt textField "Name" Nothing
    <*> areq textField "sex" (Just (T.pack "female"))
    <*> aopt textField "Owner" Nothing
    <*> areq checkBoxField "t1-10" Nothing
    <*> areq checkBoxField "t11-20" Nothing
    <*> areq checkBoxField "t21-30" Nothing
    <*> areq checkBoxField "t31-40" Nothing
    <*> areq checkBoxField "t41-50" Nothing


getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "CowTable3conduit"
  [whamlet|
<h1>表示項目の選択と、クエリの入力
<form action=@{ESelectR} method="GET">
    耳標番号:<input name="earNum" type="checkbox" checked value="1">
    名前:<input name="name" type="checkbox" checked value="1">
    誕生日:<input name="birth" type="checkbox" checked value="1">
    性別:<input name="sex" type="checkbox" checked value="1">
    所有者名:<input name="ownerName" type="checkbox" checked value="1">
    <p>
    t1-10:<input name="t1" type="checkbox" checked value="1">
    t11-20:<input name="t2" type="checkbox" checked value="1">
    t21-30:<input name="t3" type="checkbox" checked value="1">
    t31-40:<input name="t4" type="checkbox" checked value="1">
    t41-50:<input name="t5" type="checkbox" checked value="1">
    <p>
    検索条件(where):Owner.name(Default:Owner5)=
    <input name="OwnerName" type="text" value="Owner5">
    <p>
    <input type="submit" value="送信">
                 |]
    -- 検索条件(where):
    -- <input name="where" type="text" value="owner.name = 'Owner5'">
    -- <p>

getESelectR :: Handler TypedContent
getESelectR = do
  flags' <- mapM (\t -> runInputGet $ iopt textField t)
                ["earNum", "name", "birth", "sex", "ownerName",
                 "t1", "t2", "t3", "t4", "t5"]
  flags <- return $ map flagConv flags'
  ownerName' <- (runInputGet $ iopt textField "OwnerName")
  let ownerName = nameConv ownerName'
  -- liftIO $ print flags
  respondSourceDB typeJson $ cowsSrc ownerName $= awaitForever (toBuilder flags)
    where
        flagConv (Just "1") = True
        flagConv (Just "True") = True
        flagConv _ = False
        nameConv (Just name) = name
        nameConv (Nothing) = T.pack "Owner5"
        cowsSrc ownerName = 
          E.selectSource
            $ E.from $ \(cow `E.InnerJoin` owner) -> do
                E.on $ cow ^. CowOwnerId E.==. owner ^. OwnerId
                E.where_ $ owner ^. OwnerName `E.like` E.val ownerName
                return $ (
                  cow ^. CowEarNum, cow ^. CowName, cow ^. CowBirth,
                  cow ^. CowSex, owner ^. OwnerName,
                  (cow ^. CowT1, cow ^. CowT2, cow ^. CowT3, cow ^. CowT4,
                   cow ^. CowT5, cow ^. CowT6, cow ^. CowT7, cow ^. CowT8,
                   cow ^. CowT9, cow ^. CowT10),
                  (cow ^. CowT11, cow ^. CowT12, cow ^. CowT13, cow ^. CowT14,
                   cow ^. CowT15, cow ^. CowT16, cow ^. CowT17, cow ^. CowT18,
                   cow ^. CowT19, cow ^. CowT20),
                  (cow ^. CowT21, cow ^. CowT22, cow ^. CowT23, cow ^. CowT24,
                   cow ^. CowT25, cow ^. CowT26, cow ^. CowT27, cow ^. CowT28,
                   cow ^. CowT29, cow ^. CowT30),
                  (cow ^. CowT31, cow ^. CowT32, cow ^. CowT33, cow ^. CowT34,
                   cow ^. CowT35, cow ^. CowT36, cow ^. CowT37, cow ^. CowT38,
                   cow ^. CowT39, cow ^. CowT40),
                  (cow ^. CowT41, cow ^. CowT42, cow ^. CowT43, cow ^. CowT44,
                   cow ^. CowT45, cow ^. CowT46, cow ^. CowT47, cow ^. CowT48,
                   cow ^. CowT49, cow ^. CowT50))
        toBuilder flags (E.Value earNum, E.Value name, E.Value birth,
          E.Value sex, E.Value ownerName ,
          (E.Value t1, E.Value  t2, E.Value  t3, E.Value  t4, E.Value  t5,
          E.Value t6, E.Value  t7, E.Value  t8, E.Value  t9, E.Value t10),
          (E.Value t11, E.Value t12, E.Value t13, E.Value t14, E.Value t15,
          E.Value t16, E.Value t17, E.Value t18, E.Value t19, E.Value t20),
          (E.Value t21, E.Value t22, E.Value t23, E.Value t24, E.Value t25,
          E.Value t26, E.Value t27, E.Value t28, E.Value t29, E.Value t30),
          (E.Value t31, E.Value t32, E.Value t33, E.Value t34, E.Value t35,
          E.Value t36, E.Value t37, E.Value t38, E.Value t39, E.Value t40),
          (E.Value t41, E.Value t42, E.Value t43, E.Value t44, E.Value t45,
          E.Value t46, E.Value t47, E.Value t48, E.Value t49, E.Value t50))
          = do 
            sendChunkLBS $ encode $ object $ aesonList
            where
--              flags = [True, True, True,True, True, False, True]
              aesonList = concat $ map snd $ filter (\(f,e)->f) $ zip flags aesonOrig
              aesonOrig = [
                 ["earNum".= earNum]
                ,["name" .= name]
                ,["birth" .= birth]
                ,["sex" .= sex]
                ,["ownerName" .= ownerName]
                ,["t1"  .=  t1, "t2" .=  t2, "t3" .=  t3, "t4" .=  t4,  "t5" .=  t5
                 ,"t6"  .=  t6, "t7" .=  t7, "t8" .=  t8, "t9" .=  t9,"t10" .= t10]
                ,["t11" .= t11,"t12" .= t12,"t13" .= t13,"t14" .= t14,"t15" .= t15,
                  "t16" .= t16,"t17" .= t17,"t18" .= t18,"t19" .= t19,"t20" .= t20]
                ,["t21" .= t21,"t22" .= t22,"t23" .= t23,"t24" .= t24,"t25" .= t25,
                  "t26" .= t26,"t27" .= t27,"t28" .= t28,"t29" .= t29,"t30" .= t30]
                ,["t31" .= t31,"t32" .= t32,"t33" .= t33,"t34" .= t34,"t35" .= t35,
                  "t36" .= t36,"t37" .= t37,"t38" .= t38,"t39" .= t39,"t40" .= t40]
                ,["t41" .= t41,"t42" .= t42,"t43" .= t43,"t44" .= t44,"t45" .= t45,
                  "t46" .= t46,"t47" .= t47,"t48" .= t48,"t49" .= t49,"t50" .= t50]]
                -- "earNum".= earNum, "name" .= name, "birth" .= birth,
                -- "sex" .= sex,  "ownerName" .= ownerName,
                -- "t1"  .=  t1, "t2" .=  t2, "t3" .=  t3, "t4" .=  t4, "t5" .=  t5,
                -- "t6"  .=  t6, "t7" .=  t7, "t8" .=  t8, "t9" .=  t9,"t10" .= t10,
                -- "t11" .= t11,"t12" .= t12,"t13" .= t13,"t14" .= t14,"t15" .= t15,
                -- "t16" .= t16,"t17" .= t17,"t18" .= t18,"t19" .= t19,"t20" .= t20,
                -- "t21" .= t21,"t22" .= t22,"t23" .= t23,"t24" .= t24,"t25" .= t25,
                -- "t26" .= t26,"t27" .= t27,"t28" .= t28,"t29" .= t29,"t30" .= t30,
                -- "t31" .= t31,"t32" .= t32,"t33" .= t33,"t34" .= t34,"t35" .= t35,
                -- "t36" .= t36,"t37" .= t37,"t38" .= t38,"t39" .= t39,"t40" .= t40,
                -- "t41" .= t41,"t42" .= t42,"t43" .= t43,"t44" .= t44,"t45" .= t45,
                -- "t46" .= t46,"t47" .= t47,"t48" .= t48,"t49" .= t49,"t50" .= t50]

openConnectionCount :: Int
openConnectionCount = 10

connStr = "host=localhost dbname=cow_table user=hirofumi password='hello'"

main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool connStr openConnectionCount $ \pool -> liftIO $ do
    warp 3000 $ App {appPool = pool}
