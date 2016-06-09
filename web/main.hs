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
import Data.Time.Calendar

data Pagina = Pagina{getStatic :: Static, connPool :: ConnectionPool}

instance Yesod Pagina

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Usuario json
   nome Text
   senha Text
   tipo Text
   email Text
   deriving Show

Turma json
    nome Text
	deriving Show
	
Materia json
	nome Text
	turma TurmaId
	aluno [UsuarioId]
	deriving Show
	
Presenca json
    dia Day
    materia MateriaId
    alunospres [UsuarioId]
|]

staticFiles "../static" 

mkYesod "Pagina" [parseRoutes|
/ HomeR GET
/usuario/cadastro UsuarioR GET POST
/usuario/checar/#UsuarioId ChecarUsuarioR GET
/turma/cadastro TurmaR GET POST
/turma/checar/#TurmaId ChecarTurmaR GET
/materia/cadastro MateriaR GET POST
/materia/checar/#MateriaId ChecarMateriaR GET
/presenca CriarPresencaR GET

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


getCriarPresencaR :: Handler Html
getCriarPresencaR = do
            (widget, enctype) <- generateFormPost formCriarPresenca
            defaultLayout $ do
                addStylesheet $ StaticR menu_css
                $(whamletFile "templates/hamlet/menu.hamlet")
                $(whamletFile "templates/hamlet/form/iniciarCadPresenca.hamlet")


formCriarPresenca :: Form (Day, MateriaId)
formCriarPresenca = renderDivs $ (,) <$>
          areq dayField "Dia: " Nothing <*>
          areq (selectField presencas) "Tipo: " Nothing
           
presencas = do
      entidades <- runDB $ selectList ([]::[Filter Materia]) [] 
      optionsPairs $ fmap (\ent -> (materiaNome $ entityVal ent, entityKey ent)) entidades
       
           
formUsuario = renderDivs $ Usuario <$>
           areq textField "Nome: " Nothing <*>
           areq passwordField "Senha: " Nothing <*>
           areq (selectFieldList [("Professor" :: Text, "Professor"),("Aluno", "Aluno")]) "Tipo: " Nothing <*>
           areq emailField "Email: " Nothing

formTurma :: Form Turma
formTurma = renderDivs $ Turma <$>
           areq textField "Nome: " Nothing

formMateria :: Form Materia
formMateria = renderDivs $ Materia <$>
           areq textField "Nome: " Nothing <*>
           areq (selectField turmas) "Turma: " Nothing <*>
           areq (multiSelectField alunos) "Alunos: " Nothing
           
turmas = do
       entidades <- runDB $ selectList [] [Asc TurmaNome] 
       optionsPairs $ fmap (\ent -> (turmaNome $ entityVal ent, entityKey ent)) entidades
       
alunos = do
       entidades <- runDB $ selectList [UsuarioTipo ==. "Aluno"] [Asc UsuarioNome] 
       optionsPairs $ fmap (\ent -> (usuarioNome $ entityVal ent, entityKey ent)) entidades

getUsuarioR :: Handler Html
getUsuarioR = do
            (widget, enctype) <- generateFormPost formUsuario
            defaultLayout $ do
                addStylesheet $ StaticR menu_css
                $(whamletFile "templates/hamlet/menu.hamlet")
                $(whamletFile "templates/hamlet/form/cadastrarUsuario.hamlet")

getTurmaR :: Handler Html
getTurmaR = do
            (widget, enctype) <- generateFormPost formTurma
            defaultLayout $ do
                addStylesheet $ StaticR menu_css
                $(whamletFile "templates/hamlet/menu.hamlet")
                $(whamletFile "templates/hamlet/form/cadastrarTurma.hamlet")

getMateriaR :: Handler Html
getMateriaR = do
            (widget, enctype) <- generateFormPost formMateria
            defaultLayout $ do
                addStylesheet $ StaticR menu_css
                $(whamletFile "templates/hamlet/menu.hamlet")
                $(whamletFile "templates/hamlet/form/cadastrarMateria.hamlet")

postUsuarioR :: Handler Html
postUsuarioR = do
           ((result, _), _) <- runFormPost formUsuario
           case result of 
               FormSuccess usuarioRetornoTela -> (runDB $ insert usuarioRetornoTela) >>= \uiid -> redirect (ChecarUsuarioR uiid)
               _ -> redirect ErroR

postTurmaR :: Handler Html
postTurmaR = do
           ((result, _), _) <- runFormPost formTurma
           case result of 
               FormSuccess turmaRetornoTela -> (runDB $ insert turmaRetornoTela) >>= \tiid -> redirect (ChecarTurmaR tiid)
               _ -> redirect ErroR

postMateriaR :: Handler Html
postMateriaR = do
           ((result, _), _) <- runFormPost formMateria
           case result of 
               FormSuccess materiaRetornoTela -> (runDB $ insert materiaRetornoTela) >>= \miid -> redirect (ChecarMateriaR miid)
               _ -> redirect ErroR

getChecarUsuarioR :: UsuarioId -> Handler Html
getChecarUsuarioR aid = do
    usuario <- runDB $ get404 aid
    defaultLayout $ do
        addStylesheet $ StaticR menu_css
        $(whamletFile "templates/hamlet/menu.hamlet")
        $(whamletFile "templates/hamlet/form/checarUsuario.hamlet")

getChecarTurmaR :: TurmaId -> Handler Html
getChecarTurmaR aid = do
    turma <- runDB $ get404 aid
    defaultLayout $ do
        addStylesheet $ StaticR menu_css
        $(whamletFile "templates/hamlet/menu.hamlet")
        $(whamletFile "templates/hamlet/form/checarTurma.hamlet")

getChecarMateriaR :: MateriaId -> Handler Html
getChecarMateriaR aid = do
    materia <- runDB $ get404 aid
    defaultLayout $ do
        addStylesheet $ StaticR menu_css
        $(whamletFile "templates/hamlet/menu.hamlet")
        $(whamletFile "templates/hamlet/form/checarMateria.hamlet")

getErroR :: Handler Html
getErroR = defaultLayout $ do
                addStylesheet $ StaticR menu_css
                $(whamletFile "templates/hamlet/menu.hamlet")
                $(whamletFile "templates/hamlet/erro.hamlet")

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