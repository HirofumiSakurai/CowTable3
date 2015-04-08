import           Database.HDBC
import           Database.HDBC.PostgreSQL
import           Data.Time.Calendar (fromGregorian, addDays)

main :: IO ()
main = do
  conn <- connectPostgreSQL "host=localhost dbname=cow_table user=hirofumi password='hello'"
  run conn "CREATE TABLE owner (id INTEGER NOT NULL, name VARCHAR(80))" []
  run conn ("CREATE TABLE cow  (" ++ rows ++ ")") []
  commit conn
  stmt <- prepare conn "INSERT INTO owner VALUES (?, ?)"
  executeMany stmt $ map (\i -> [toSql i, toSql ("Owner"++(show i))])
                         ([1..10] :: [Int])
  commit conn
  stmt <- prepare conn "INSERT INTO cow VALUES (?,?,?,?,?,?\
                                              \,?,?,?,?,?,?,?,?,?,?\
                                              \,?,?,?,?,?,?,?,?,?,?\
                                              \,?,?,?,?,?,?,?,?,?,?\
                                              \,?,?,?,?,?,?,?,?,?,?\
                                              \,?,?,?,?,?,?,?,?,?,?)"
  executeMany stmt $ map cowCols [1..40000]
  commit conn
  disconnect conn
    where
        cowCols i = [toSql i, toSql i, toSql "ハナ", toSql (birthday i),
                     toSql "雌", toSql ((mod i 10) + 1)]
                      ++ map (\i -> toSql i) ([1..50] :: [Int])
        birthday i = addDays i $ fromGregorian 2000 1 1  
        rows = " id      INTEGER NOT NULL, ear_num INTEGER, name VARCHAR(80)\
              \, birth DATE,              sex VARCHAR(10), owner_id INTEGER\
              \, t1 INTEGER, t2 INTEGER, t3 INTEGER, t4 INTEGER, t5 INTEGER\
              \, t6 INTEGER, t7 INTEGER, t8 INTEGER, t9 INTEGER, t10 INTEGER\
              \, t11 INTEGER, t12 INTEGER, t13 INTEGER, t14 INTEGER\
              \, t15 INTEGER, t16 INTEGER, t17 INTEGER, t18 INTEGER\
              \, t19 INTEGER, t20 INTEGER\
              \, t21 INTEGER, t22 INTEGER, t23 INTEGER, t24 INTEGER\
              \, t25 INTEGER, t26 INTEGER, t27 INTEGER, t28 INTEGER\
              \, t29 INTEGER, t30 INTEGER\
              \, t31 INTEGER, t32 INTEGER, t33 INTEGER, t34 INTEGER\
              \, t35 INTEGER, t36 INTEGER, t37 INTEGER, t38 INTEGER\
              \, t39 INTEGER, t40 INTEGER\
              \, t41 INTEGER, t42 INTEGER, t43 INTEGER, t44 INTEGER\
              \, t45 INTEGER, t46 INTEGER, t47 INTEGER, t48 INTEGER\
              \, t49 INTEGER, t50 INTEGER\
              \"
