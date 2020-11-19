{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Text.Digestive.Shpadoinkle
  ( inputText
  , inputTextArea
  , inputPassword
  , inputHidden
  , inputSelect
  , inputSelectGroup
  , inputRadio
  , inputCheckbox
  , inputFile
  , inputSubmit
  , inputWithType
  , Text.Digestive.Shpadoinkle.label
  , Text.Digestive.Shpadoinkle.form
  , errorList
  , childErrorList
  ) where

import           Data.Maybe          (maybeToList)
import           Data.Text           (Text, pack)
import           Shpadoinkle         (Html, Prop, flagProp, text, textProp)
import           Shpadoinkle.Html    as Html (br_, className, form, id', input',
                                              label, li, optgroup, option,
                                              select, textarea, ul, value)
import           Text.Digestive.View (View, absoluteRef, childErrors, errors,
                                      fieldInputBool, fieldInputChoice,
                                      fieldInputChoiceGroup, fieldInputText,
                                      viewEncType)


inputText :: Text -> View v -> Html m a
inputText = inputWithType "text" []


inputTextArea :: Maybe Int -> Maybe Int -> Text -> View (Html m a) -> Html m a
inputTextArea r c ref view =
  textarea ( [ ("id", textProp ref'), ("name", textProp ref') ]
           <> maybeToList (rows' <$> r)
           <> maybeToList (cols' <$> c) )
    [ text $ fieldInputText ref view ]
  where
    ref' = absoluteRef ref view
    rows' x = ("rows",) . textProp . pack $ show x
    cols' x = ("cols",) . textProp . pack $ show x


inputPassword :: Text -> View v -> Html m a
inputPassword = inputWithType "password" []


inputHidden :: Text -> View v -> Html m a
inputHidden = inputWithType "hidden" []


inputSelect :: Text -> View (Html m a) -> Html m a
inputSelect ref view =
  select [ ("id", textProp ref'), ("name", textProp ref') ]
    (choiceEl <$> choices)
  where ref'    = absoluteRef ref view
        valueProp i = ("value", textProp (ref' <> "." <> i))
        choiceEl (i, c, sel) = option [ valueProp i, ("selected", flagProp sel) ] [ c ]
        choices = mconcat $ snd <$> fieldInputChoiceGroup ref view


inputSelectGroup :: Text -> View (Html m a) -> Html m a
inputSelectGroup ref view = select [ ("id", textProp ref'), ("name", textProp ref') ]
                              (groupEl <$> groups)
  where ref' = absoluteRef ref view
        valueProp i = ("value", textProp (ref' <> "." <> i))
        groups = fieldInputChoiceGroup ref view
        groupEl (groupName, choices) = optgroup [("label", textProp groupName)] (choiceEl <$> choices)
        choiceEl (i, c, sel) = option [ valueProp i, ("selected", flagProp sel) ] [ c ]


inputWithType :: Text -> [(Text, Prop m a)] -> Text -> View v -> Html m a
inputWithType ty additionalAttrs ref view = input' attrs
  where
    ref' = absoluteRef ref view
    attrs = defAttrs `mappend` additionalAttrs
    defAttrs =
      [ ("type", textProp ty)
      , id' ref'
      , ("name", textProp ref')
      , value $ fieldInputText ref view ]


inputRadio :: Bool -> Text -> View (Html m a) -> [Html m a]
inputRadio addLineBreaks ref view = mconcat $ choiceEls <$> choices
  where choices = fieldInputChoice ref view
        ref' = absoluteRef ref view
        valueProp i = textProp (ref' <> "." <> i)
        choiceEls (i, c, sel) =
          [ input' [("type", textProp "radio"), ("value", valueProp i), ("checked", flagProp sel)]
          , Html.label [("for", valueProp i)] [ c ] ]
          <> ([br_ [] | addLineBreaks])

inputCheckbox :: Text -> View (Html m a) -> Html m a
inputCheckbox ref view =
  input' [ ("type", textProp "checkbox"), ("id", textProp ref'), ("name", textProp ref'), ("checked", flagProp selected) ]
  where ref'     = absoluteRef ref view
        selected = fieldInputBool ref view

inputFile :: Text -> View v -> Html m a
inputFile ref view = input' [ ("type", textProp "file"), ("id", textProp ref'), ("name", textProp ref') ]
  where ref' = absoluteRef ref view

inputSubmit :: Text -> Html m a
inputSubmit v = input' [("type", textProp "submit"), ("value", textProp v)]

label :: Text -> View v -> [Html m a] -> Html m a
label ref view = Html.label [ ("for", textProp ref') ]
  where ref' = absoluteRef ref view

form :: View (Html m a) -> Text -> [Html m a] -> Html m a
form view action = Html.form [ ("method", textProp "POST")
                             , ("enctype", textProp (pack . show $ viewEncType view))
                             , ("action", textProp action) ]

errorList :: Text -> View (Html m a) -> [Html m a]
errorList ref view = case errors ref view of
  []   -> []
  errs -> [ ul [ className "digestive-functors-error-list" ] $ errEl <$> errs ]
  where errEl e = li [ className "digestive-functors-error" ] [ e ]

childErrorList :: Text -> View (Html m a) -> [Html m a]
childErrorList ref view = case childErrors ref view of
  [] -> []
  errs -> [ ul [ className "digestive-functors-error-list" ] $ errEl <$> errs ]
  where errEl e = li [ className "digestive-functors-error" ] [ e ]
