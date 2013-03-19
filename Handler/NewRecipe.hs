module Handler.NewRecipe where

import Import

recipeForm :: Form Recipe
recipeForm = renderDivs $ Recipe
    <$> areq textField "Name" Nothing

getNewRecipeR :: Handler RepHtml
getNewRecipeR = defaultLayout $ do
    newRecipeWidget
    listRecipesWidget

postNewRecipeR :: Handler RepHtml
postNewRecipeR = do
    ((result, _), _) <- runFormPost recipeForm
    case result of
        FormSuccess newRecipe -> do
            recipeId <- runDB $ insert newRecipe
            setMessage "New recipe created"
            redirect $ RecipeR recipeId
        _ -> redirect NewRecipeR

newRecipeWidget :: Widget
newRecipeWidget = do
    (formWidget, encType) <- lift $ generateFormPost recipeForm
    $(widgetFile "newRecipe")

listRecipesWidget :: Widget
listRecipesWidget = do
    entities <- lift $ runDB $ selectList noFilter []
    $(widgetFile "recipeList")

getClearR :: Handler ()
getClearR = do
    runDB $ deleteWhere noFilter
    redirect NewRecipeR
    
noFilter :: [Filter Recipe]
noFilter = []