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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Usuario json
   nome Text
   senha Text
   tipo Text
   email Text
   deriving Show
|]

mkYesod "Pagina" [parseRoutes|
/ HomeR GET
/usuario/cadastro UsuarioR GET POST
/usuario/checar/#UsuarioId ChecarUsuarioR GET
/erro ErroR GET
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
            defaultLayout $ [whamlet|
                <header>
                    <nav class="menu-container">
                        <div class="logo-container">
                            <a href="">Logo
                        <ul id="menu">
                            <li>
                                <a href="">Home
                            <li>
                                <a href="">Home
                            <li>
                                <a href="">Home
                        <div class="clearfix">
                        <div class="linha-menu">
                <section>
                    <div id="main">
                        <div class="main-content">
                            <h1 class="titulo">Cadastro de Usuário
                            <fieldset>
                                <legend>Preencha o formulário
                                <form method=post id="form-usuario" enctype=#{enctype} action=@{UsuarioR}>
                                    ^{widget}
                                    <input type="submit" value="Salvar" class="submit">
                                    <span class="clearfix">
|] >> toWidget [lucius| 
    *{box-sizing: border-box;}
    html, body{
        margin: 0;
        padding: 0;
    }
    body{
        background-color: #626266;
    }
    .clearfix{
        display: block;
        clear:both;
    }
    
    
    /* ---------------------------- 
                MENU
    ---------------------------- */
    .menu-container{
        background-color: #020304;
    }
    
    .logo-container{
        float: left;
    }
    .logo-container a{
        padding: 19px 30px;
        color: #CC9E61;
        font-size: 20px;
        font-family: fantasy;
        text-decoration: none;
        display: inline-block;
    }
    
    #menu{
        margin: 0;
        float: right;
        margin-right: 20px;
    }
    #menu li{
        display: inline;
    }
    #menu li a{
        padding: 25px 10px 15px;
        color: #CC9E61;
        font-size: 20px;
        font-family: Arial, sans-serif;
        text-decoration: none;
        display: inline-block;
    }
    #menu li a:hover{
        color: #541F14;
        background-color: #CC9E61;
        border-radius: 30px 30px 0 0;
    }
    
    .linha-menu{
        height: 5px;
        background-color: #CC9E61;
        box-shadow: 0 1px 10px black;
    }
    
    
    /* ---------------------------- 
                MAIN
    ---------------------------- */
    
    #main{
        /*background-color: white;
        padding: 10px;
        border-radius: 20px;*/
        margin: 30px auto;
        width: 800px;
        background-color: white;
        border: 5px solid #541F14;
        padding: 30px 50px 50px;
        border-radius: 20px;
        color: #541F14;
    }
    .main-content{
        
    }
    .titulo{
        font-family: Arial, sans-serif;
        text-align: center;
    }
    fieldset{
        padding: 20px;
    }
    form > div{
        margin-bottom: 20px;
    }
    form label{
        display: inline-block;
        width: 100px;
        height: 20px;
        line-height: 20px;
        text-align: right;
        font-family: Arial, sans-serif;
    }
    form input[type=text],
    form input[type=password],
    form input[type=email],
    form select{
        border-radius:5px;
        height: 20px;
        width: 300px;
    }
    .submit{
        background-color: #541F14;
        padding: 10px 20px;
        font-size: 15px;
        border: none;
        border-radius: 10px;
        box-shadow: 2px 2px 10px #999;
        cursor: pointer;
        color: white;
        float: right;
    }
|]

postUsuarioR :: Handler Html
postUsuarioR = do
           ((result, _), _) <- runFormPost formUsuario
           case result of 
               FormSuccess usuarioRetornoTela -> (runDB $ insert usuarioRetornoTela) >>= \uiid -> redirect (ChecarUsuarioR uiid)
               _ -> redirect ErroR

getChecarUsuarioR :: UsuarioId -> Handler Html
getChecarUsuarioR aid = do
    usuario <- runDB $ get404 aid
    defaultLayout $ [whamlet|
                <header>
                    <nav class="menu-container">
                        <div class="logo-container">
                            <a href="">Logo
                        <ul id="menu">
                            <li>
                                <a href="">Home
                            <li>
                                <a href="">Home
                            <li>
                                <a href="">Home
                        <div class="clearfix">
                        <div class="linha-menu">
                <section>
                    <div id="main">
                        <div class="main-content">
                            <p>Nome: #{usuarioNome usuario}
                            <p>Tipo: #{usuarioTipo usuario}
                            <p>Email: #{usuarioEmail usuario}
|] >> toWidget [lucius| 
    *{box-sizing: border-box;}
    html, body{
        margin: 0;
        padding: 0;
    }
    body{
        background-color: #626266;
    }
    .clearfix{
        display: block;
        clear:both;
    }
    
    
    /* ---------------------------- 
                MENU
    ---------------------------- */
    .menu-container{
        background-color: #020304;
    }
    
    .logo-container{
        float: left;
    }
    .logo-container a{
        padding: 19px 30px;
        color: #CC9E61;
        font-size: 20px;
        font-family: fantasy;
        text-decoration: none;
        display: inline-block;
    }
    
    #menu{
        margin: 0;
        float: right;
        margin-right: 20px;
    }
    #menu li{
        display: inline;
    }
    #menu li a{
        padding: 25px 10px 15px;
        color: #CC9E61;
        font-size: 20px;
        font-family: Arial, sans-serif;
        text-decoration: none;
        display: inline-block;
    }
    #menu li a:hover{
        color: #541F14;
        background-color: #CC9E61;
        border-radius: 30px 30px 0 0;
    }
    
    .linha-menu{
        height: 5px;
        background-color: #CC9E61;
        box-shadow: 0 1px 10px black;
    }
    
    
    /* ---------------------------- 
                MAIN
    ---------------------------- */
    
    #main{
        /*background-color: white;
        padding: 10px;
        border-radius: 20px;*/
        margin: 30px auto;
        width: 800px;
        background-color: white;
        border: 5px solid #541F14;
        padding: 30px 50px 50px;
        border-radius: 20px;
        color: #541F14;
    }
    .main-content{
        
    }
    .titulo{
        font-family: Arial, sans-serif;
        text-align: center;
    }
    fieldset{
        padding: 20px;
    }
|]

getErroR :: Handler Html
getErroR = defaultLayout [whamlet|
    ocorreu uma falha no sucesso 
|]

getHomeR :: Handler Html
getHomeR = defaultLayout $ [whamlet|
                <header>
                    <nav class="menu-container">
                        <div class="logo-container">
                            <a href="">Logo
                        <ul id="menu">
                            <li>
                                <a href="">Home
                            <li>
                                <a href="">Home
                            <li>
                                <a href="">Home
                        <div class="clearfix">
                        <div class="linha-menu">
                <section>
                    <div id="main">
                        <div class="main-content">
                            <h1 class="titulo">Seja Bem Vindo
|] >> toWidget [lucius| 
    *{box-sizing: border-box;}
    html, body{
        margin: 0;
        padding: 0;
    }
    body{
        background-color: #626266;
    }
    .clearfix{
        display: block;
        clear:both;
    }
    
    
    /* ---------------------------- 
                MENU
    ---------------------------- */
    .menu-container{
        background-color: #020304;
    }
    
    .logo-container{
        float: left;
    }
    .logo-container a{
        padding: 19px 30px;
        color: #CC9E61;
        font-size: 20px;
        font-family: fantasy;
        text-decoration: none;
        display: inline-block;
    }
    
    #menu{
        margin: 0;
        float: right;
        margin-right: 20px;
    }
    #menu li{
        display: inline;
    }
    #menu li a{
        padding: 25px 10px 15px;
        color: #CC9E61;
        font-size: 20px;
        font-family: Arial, sans-serif;
        text-decoration: none;
        display: inline-block;
    }
    #menu li a:hover{
        color: #541F14;
        background-color: #CC9E61;
        border-radius: 30px 30px 0 0;
    }
    
    .linha-menu{
        height: 5px;
        background-color: #CC9E61;
        box-shadow: 0 1px 10px black;
    }
    
    
    /* ---------------------------- 
                MAIN
    ---------------------------- */
    
    #main{
        /*background-color: white;
        padding: 10px;
        border-radius: 20px;*/
        margin: 30px auto;
        width: 800px;
        background-color: white;
        border: 5px solid #541F14;
        padding: 30px 50px 50px;
        border-radius: 20px;
        color: #541F14;
    }
    .main-content{
        
    }
    .titulo{
        font-family: Arial, sans-serif;
        text-align: center;
    }
    fieldset{
        padding: 20px;
    }
|]

connStr = "dbname=d9pva9v7sc2rm1 host=ec2-54-163-240-97.compute-1.amazonaws.com user=bcgxcouvxtiphy password=2XQS1V-eZsjn7oXURLLvTnh2FK port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)