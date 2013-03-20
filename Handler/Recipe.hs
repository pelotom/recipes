module Handler.Recipe where

import Import
import Text.Julius

getRecipeR :: RecipeId -> Handler RepHtml
getRecipeR recipeId = do
    (recipe, ingredients) <- runDB $ do
        recipe <- get404 recipeId
        ingredients <- findIngreds recipeId
        return (recipe, ingredients)
    defaultLayout $ do
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
        delButtonId <- lift newIdent
        $(widgetFile "recipe")
    
findIngreds :: RecipeId -> Persist [Entity Ingredient]
findIngreds recipeId = do
    joins <- selectList [RecipeIngredientRecipeId ==. recipeId] []
    let ingredIds = (recipeIngredientIngredientId . entityVal) <$> joins
    ingreds <- mapM get404 $ ingredIds
    return $ zipWith Entity ingredIds ingreds
    
deleteRecipeR :: RecipeId -> Handler ()
deleteRecipeR recipeId = do
    runDB $ do
        deleteWhere [RecipeId ==. recipeId]
        deleteWhere [RecipeIngredientRecipeId ==. recipeId]
    setMessage "Deleted recipe"
    redirect RecipesR
    
recipeForm :: Form Recipe
recipeForm = renderDivs $ Recipe
    <$> areq textField "Name" Nothing
    <*> areq textareaField "Directions" Nothing

getHomeR :: Handler ()
getHomeR = redirect RecipesR

getRecipesR :: Handler RepHtml
getRecipesR = defaultLayout $ do
    newRecipeWidget
    listRecipesWidget
  where
    newRecipeWidget = do
        aDomId <- lift newIdent
        (formWidget, encType) <- lift $ generateFormPost recipeForm
        $(widgetFile "newRecipe")
    listRecipesWidget = do
        entities <- lift $ runDB $ selectList noFilter []
        $(widgetFile "recipeList")

postRecipesR :: Handler RepHtml
postRecipesR = do
    ((result, _), _) <- runFormPost recipeForm
    case result of
        FormSuccess newRecipe -> do
            recipeId <- runDB $ do
                recipeId <- insert newRecipe
                ingredId <- getOrAddIngred "broccoli"
                _ <- insert $ RecipeIngredient recipeId ingredId
                return recipeId
            setMessage "New recipe created"
            redirect $ RecipeR recipeId
        _ -> redirect RecipesR

getOrAddIngred :: Text -> Persist IngredientId
getOrAddIngred name = do
    maybeEnt <- getBy $ UniqueIngredientName name
    case maybeEnt of
        Just entity -> return $ entityKey entity
        Nothing -> do
            insert $ Ingredient name

deleteRecipesR :: Handler ()
deleteRecipesR = do
    runDB $ deleteWhere noFilter
    redirect RecipesR
    
noFilter :: [Filter Recipe]
noFilter = []