{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes, 
            TemplateHaskell, GADTs, FlexibleInstances,
            MultiParamTypeClasses, DeriveDataTypeable,
            GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Yesod.Static
import Database.Persist.Postgresql
import Text.Lucius (luciusFile)
import Data.Text
import Control.Monad.Logger (runStdoutLoggingT)

data Pagina = Pagina{getStatic :: Static, connPool :: ConnectionPool}

instance Yesod Pagina

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Usuario json
   nome Text
   senha Text
   tipo Text
   email Text
   deriving Show
|]

staticFiles "../static" 

mkYesod "Pagina" [parseRoutes|

/ HomeR GET
/usuario/cadastro UsuarioR GET POST
/usuario/checar/#UsuarioId ChecarUsuarioR GET
/erro ErroR GET
/static StaticR Static getStatic

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

formUsuario :: Form Usuario
formUsuario = renderDivs $ Usuario <$>
           areq textField "Nome: " Nothing <*>
           areq passwordField "Senha: " Nothing <*>
           areq (selectFieldList [("Professor" :: Text, "Professor"),("Aluno", "Aluno")]) "Tipo: " Nothing <*>
           areq emailField "Email: " Nothing
          
getUsuarioR :: Handler Html
getUsuarioR = do
            (widget, enctype) <- generateFormPost formUsuario
            defaultLayout $ do
                addStylesheet $ StaticR menu_css
                $(whamletFile "templates/hamlet/menu.hamlet")
                $(whamletFile "templates/hamlet/form/cadastrarUsuario.hamlet")

postUsuarioR :: Handler Html
postUsuarioR = do
           ((result, _), _) <- runFormPost formUsuario
           case result of 
               FormSuccess usuarioRetornoTela -> (runDB $ insert usuarioRetornoTela) >>= \uiid -> redirect (ChecarUsuarioR uiid)
               _ -> redirect ErroR

getChecarUsuarioR :: UsuarioId -> Handler Html
getChecarUsuarioR aid = do
    usuario <- runDB $ get404 aid
    defaultLayout $ do
        addStylesheet $ StaticR menu_css
        $(whamletFile "templates/hamlet/menu.hamlet")
        $(whamletFile "templates/hamlet/form/checarUsuario.hamlet")


getErroR :: Handler Html
getErroR = defaultLayout [whamlet|
    ocorreu uma falha no sucesso 
|]

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
                addStylesheet $ StaticR menu_css
                $(whamletFile "templates/hamlet/menu.hamlet")
                $(whamletFile "templates/hamlet/index.hamlet")

connStr = "dbname=d9pva9v7sc2rm1 host=ec2-54-163-240-97.compute-1.amazonaws.com user=bcgxcouvxtiphy password=2XQS1V-eZsjn7oXURLLvTnh2FK port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       t@(Static settings) <- static "static"
       warp 8080 (Pagina t pool)