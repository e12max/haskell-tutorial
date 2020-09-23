module HTML where 

class HTML a where
  toHTML :: a -> String

instance HTML a => HTML [a] where
  toHTML listOfElements =
    let
      elementToListElement :: HTML a => a -> String
      elementToListElement element = "<li>" ++ toHTML element ++ "</li>"
    in
      "<ul>" ++ concatMap elementToListElement listOfElements ++ "</ul>"

toHTMLPage :: HTML a => a -> String
toHTMLPage a =
  "<!DOCTYPE html>\
  \<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"\" xml:lang=\"\">\
  \<head>\
  \  <meta charset=\"utf-8\" />\
  \  <title>TODO list </title>\
  \</head>\
  \<body>\
  \ " ++ toHTML a ++
  "</body>"
