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

import Data.Maybe (maybeToList)
import Data.Text
import Text.Digestive.View
import Shpadoinkle
import Shpadoinkle.Html as Html


inputText :: IsHtml h p => IsProp p e => Text -> View v -> h a
inputText = inputWithType "text" []


inputTextArea :: IsHtml h p => IsProp p e
              => Maybe Int -> Maybe Int -> Text -> View (h a) -> h a
inputTextArea r c ref view =
  textarea ( [ ("id", textProp ref'), ("name", textProp ref') ]
           <> maybeToList (rows' <$> r)
           <> maybeToList (cols' <$> c) )
    [ text $ fieldInputText ref view ]
  where
    ref' = absoluteRef ref view
    rows' x = ("rows",) . textProp . pack $ show x
    cols' x = ("cols",) . textProp . pack $ show x


inputPassword :: IsHtml h p => IsProp p e => Text -> View v -> h a
inputPassword = inputWithType "password" []


inputHidden :: IsHtml h p => IsProp p e => Text -> View v -> h a
inputHidden = inputWithType "hidden" []


inputSelect :: IsHtml h p => IsProp p e => Text -> View (h a) -> h a
inputSelect ref view =
  select [ ("id", textProp ref'), ("name", textProp ref') ]
    (choiceEl <$> choices)
  where ref'    = absoluteRef ref view
        valueProp i = ("value", textProp (ref' <> "." <> i))
        choiceEl (i, c, sel) = option [ valueProp i, ("selected", flagProp sel) ] [ c ]
        choices = mconcat $ snd <$> fieldInputChoiceGroup ref view


inputSelectGroup :: IsHtml h p => IsProp p e => Text -> View (h a) -> h a
inputSelectGroup ref view = select [ ("id", textProp ref'), ("name", textProp ref') ]
                              (groupEl <$> groups)
  where ref' = absoluteRef ref view
        valueProp i = ("value", textProp (ref' <> "." <> i))
        groups = fieldInputChoiceGroup ref view
        groupEl (groupName, choices) = optgroup [("label", textProp groupName)] (choiceEl <$> choices)
        choiceEl (i, c, sel) = option [ valueProp i, ("selected", flagProp sel) ] [ c ]


inputWithType :: IsHtml h p => IsProp p e => Text -> [(Text, p a)] -> Text -> View v -> h a
inputWithType ty additionalAttrs ref view = input' attrs
  where
    ref' = absoluteRef ref view
    attrs = defAttrs `mappend` additionalAttrs
    defAttrs =
      [ ("type", textProp ty)
      , id' ref'
      , ("name", textProp ref')
      , value $ fieldInputText ref view ]


inputRadio :: IsHtml h p => IsProp p e => Bool -> Text -> View (h a) -> [h a]
inputRadio addLineBreaks ref view = mconcat $ choiceEls <$> choices
  where choices = fieldInputChoice ref view
        ref' = absoluteRef ref view
        valueProp i = textProp (ref' <> "." <> i)
        choiceEls (i, c, sel) =
          [ input' [("type", textProp "radio"), ("value", valueProp i), ("checked", flagProp sel)]
          , Html.label [("for", valueProp i)] [ c ] ]
          <> (if addLineBreaks then [br_ []] else [])

inputCheckbox :: IsHtml h p => IsProp p e => Text -> View (h a) -> h a
inputCheckbox ref view =
  input' [ ("type", textProp "checkbox"), ("id", textProp ref'), ("name", textProp ref'), ("checked", flagProp selected) ]
  where ref'     = absoluteRef ref view
        selected = fieldInputBool ref view

inputFile :: IsHtml h p => IsProp p e => Text -> View v -> h a
inputFile ref view = input' [ ("type", textProp "file"), ("id", textProp ref'), ("name", textProp ref') ]
  where ref' = absoluteRef ref view

inputSubmit :: IsHtml h p => IsProp p e => Text -> h a
inputSubmit v = input' [("type", textProp "submit"), ("value", textProp v)]

label :: IsHtml h p => IsProp p e => Text -> View v -> [h a] -> h a
label ref view = Html.label [ ("for", textProp ref') ]
  where ref' = absoluteRef ref view

form :: IsHtml h p => IsProp p e => View (h a) -> Text -> [h a] -> h a
form view action = Html.form [ ("method", textProp "POST")
                             , ("enctype", textProp (pack . show $ viewEncType view))
                             , ("action", textProp action) ]

errorList :: IsHtml h p => IsProp p e => Text -> View (h a) -> [h a]
errorList ref view = case errors ref view of
  []   -> []
  errs -> [ ul [ className "digestive-functors-error-list" ] $ errEl <$> errs ]
  where errEl e = li [ className "digestive-functors-error" ] [ e ]

childErrorList :: IsHtml h p => IsProp p e => Text -> View (h a) -> [h a]
childErrorList ref view = case childErrors ref view of
  [] -> []
  errs -> [ ul [ className "digestive-functors-error-list" ] $ errEl <$> errs ]
  where errEl e = li [ className "digestive-functors-error" ] [ e ]
