module Handler.Recipe where

import Import
import Database.Persist.Store

getRecipeR :: RecipeId -> Handler RepHtml
getRecipeR recipeId = do
    recipe <- runDB $ get404 $ recipeId
    defaultLayout $(widgetFile "recipe")