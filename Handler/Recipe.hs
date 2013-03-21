{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Recipe where

import Import

data FullRecipe = FullRecipe Recipe [Ingredient]
    
recipeForm :: Form FullRecipe
recipeForm = renderDivs $ mkRecipe
    <$> areq textField "Name" Nothing
    <*> areq textareaField "Ingredients (one per line)" Nothing
    <*> areq textareaField "Directions" Nothing
  where
      mkRecipe :: Text -> Textarea -> Textarea -> FullRecipe
      mkRecipe n i d = FullRecipe (Recipe n d) $ map Ingredient $ lines $ unTextarea i

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

postDeleteRecipeR :: RecipeId -> Handler ()
postDeleteRecipeR recipeId = do
    deleteRecipeR recipeId
    setMessage "Deleted recipe"
    redirect RecipesR

getHomeR :: Handler ()
getHomeR = redirect RecipesR

getRecipesR :: Handler RepHtml
getRecipesR = defaultLayout $ do
    newRecipeWidget
    listRecipesWidget
  where
    newRecipeWidget :: Widget
    newRecipeWidget = do
        aDomId <- lift newIdent
        (formWidget, encType) <- lift $ generateFormPost recipeForm
        $(widgetFile "newRecipe")
    listRecipesWidget :: Widget
    listRecipesWidget = do
        entities <- lift $ runDB $ selectList noFilter []
        $(widgetFile "recipeList")

postRecipesR :: Handler RepHtml
postRecipesR = do
    ((result, _), _) <- runFormPost recipeForm
    case result of
        FormSuccess (FullRecipe recipe ingredients) -> do
            recipeId <- runDB $ do
                recipeId <- insert recipe
                forM_ ingredients $ \ingred -> do
                    ingredId <- getOrAddIngred ingred
                    insert $ RecipeIngredient recipeId ingredId
                return recipeId
            setMessage "New recipe created"
            redirect $ RecipeR recipeId
        _ -> redirect RecipesR

getOrAddIngred :: Ingredient -> Persist IngredientId
getOrAddIngred ingredient = do
    maybeEnt <- getBy $ UniqueIngredientName $ ingredientName ingredient
    case maybeEnt of
        Just entity -> return $ entityKey entity
        Nothing -> insert ingredient

deleteRecipesR :: Handler ()
deleteRecipesR = do
    runDB $ deleteWhere noFilter
    redirect RecipesR
    
noFilter :: [Filter Recipe]
noFilter = []