{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes, 
            TemplateHaskell, GADTs, FlexibleInstances,
            MultiParamTypeClasses, DeriveDataTypeable,
            GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Control.Monad.Logger (runStdoutLoggingT)

data Pagina = Pagina{connPool :: ConnectionPool}

instance Yesod Pagina

share [mkPersist sqlSettings, mkMigrate "migrateAll"] []

mkYesod "Pagina" [parseRoutes|
/ HomeR GET
|]

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Pagina FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

connStr = "dbname=d9pva9v7sc2rm1 host=ec2-54-163-240-97.compute-1.amazonaws.com user=bcgxcouvxtiphy password=2XQS1V-eZsjn7oXURLLvTnh2FK port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)