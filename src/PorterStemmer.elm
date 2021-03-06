module PorterStemmer exposing (stem)

{-| Elm implementation of the classical Porter Stemming-algorithm. The algorithm
is described in [this paper](http://tartarus.org/martin/PorterStemmer/def.txt)
and on [Wikipedia](https://en.wikipedia.org/wiki/Stemming).
The implementation is inspired by the [JavaScript](http://tartarus.org/martin/PorterStemmer/js.txt)-
and the [Haskell](http://tartarus.org/martin/PorterStemmer/haskell.txt)-implementation.

The module exposes a single function.

@docs stem
-}

import String
import Regex exposing (..)



regex : String -> Regex
regex str =
  Maybe.withDefault Regex.never <|
    Regex.fromString str 

-- consonant
c : String
c = "[^aeiou]"
-- vowel
v : String
v = "[aeiouy]"
-- consonant sequence
c_ : String
c_ = c ++ "[^aeiouy]*"
-- vowel sequence
v_ : String
v_ = v ++ "[aeiou]*"

-- [C]VC... is m > 0
mgr0 : Regex
mgr0 = regex ("^(" ++ c_ ++ ")?" ++ v_ ++ c_)
-- [C]VC[V] is m=1
meq1 : Regex
meq1 = regex ("^(" ++ c_ ++ ")?" ++ v_ ++ c_ ++ "(" ++ v_ ++ ")?$")
-- [C]VCVC... is m>1
mgr1 : Regex
mgr1 = regex ("^(" ++ c_ ++ ")?" ++ v_ ++ c_ ++ v_ ++ c_)
-- vowel in stem
s_v : Regex
s_v = regex ("^(" ++ c_ ++ ")?" ++ v)


step2list : List (String, String)
step2list =
  [ ("ational", "ate" )
  , ("tional",  "tion")
  , ("enci",    "ence")
  , ("anci",    "ance")
  , ("izer",    "ize" )
  , ("bli",     "ble" )
  , ("alli",    "al"  )
  , ("entli",   "ent" )
  , ("eli",     "e"   )
  , ("ousli",   "ous" )
  , ("ization", "ize" )
  , ("ation",   "ate" )
  , ("ator",    "ate" )
  , ("alism",   "al"  )
  , ("iveness", "ive" )
  , ("fulness", "ful" )
  , ("ousness", "ous" )
  , ("aliti",   "al"  )
  , ("iviti",   "ive" )
  , ("biliti",  "ble" )
  , ("logi",    "log" )
  ]


step3list : List (String, String)
step3list =
  [ ("icate", "ic")
  , ("ative", ""  )
  , ("alize", "al")
  , ("iciti", "ic")
  , ("ical" , "ic")
  , ("ful"  , ""  )
  , ("ness" , ""  )
  ]


step4list : List (String, String)
step4list =
  [ ("al",    ""  )
  , ("ance",  ""  )
  , ("ence",  ""  )
  , ("er",    ""  )
  , ("ic",    ""  )
  , ("able",  ""  )
  , ("ible",  ""  )
  , ("ant",   ""  )
  , ("ement", ""  )
  , ("ment",  ""  )
  , ("ent",   ""  )
  , ("sion",  "s" )
  , ("tion",  "t" )
  , ("ou",    ""  )
  , ("ism",   ""  )
  , ("ate",   ""  )
  , ("iti",   ""  )
  , ("ous",   ""  )
  , ("ive",   ""  )
  , ("ize",   ""  )
  ]


capture : Match -> String
capture match =
  let submatches = match.submatches |> List.filterMap identity
  in
    List.foldr (++) "" submatches


submatchHelper : Regex -> String -> String
submatchHelper re word =
  find re word
  |> List.head
  |> Maybe.map .submatches
  |> Maybe.withDefault []
  |> List.head
  |> Maybe.withDefault (Just "")
  |> Maybe.withDefault ""


step1a : String -> String
step1a word =
  let re = regex "^(.+?)(ss|i)es$"
      re2 = regex "^(.+?)([^s])s$"
  in
    if contains re word
      then replace re capture word
    else if contains re2 word
      then replace re2 capture word
    else
      word


step1b2 : Regex -> String -> String
step1b2 re word =
  let stemi = submatchHelper re word
  in
    if contains s_v stemi
      then
        let re2 = regex "(at|bl|iz)$"
            re3 = regex "([^aeiouylsz])\\1$"
            re4 = regex ("^" ++ c_ ++ v ++ "[^aeiouwxy]$")
        in
          if contains re2 stemi
            then stemi ++ "e"
          else if contains re3 stemi
            then replace (regex ".$") (\_ -> "") stemi
          else if contains re4 stemi
            then stemi ++ "e"
          else
            stemi
    else
      word


step1b_ : Regex -> String -> String
step1b_ re word =
  let fp = submatchHelper re word
  in
    if contains mgr0 fp
      then replace (regex ".$") (\_ -> "") word
    else
      word


step1b : String -> String
step1b word =
  let re = regex "^(.+?)eed$"
      re2 = regex "^(.+?)(ed|ing)$"
  in
    if contains re word
      then step1b_ re word
    else if contains re2 word
      then step1b2 re2 word
    else
      word


step1c : String -> String
step1c word =
  let re = regex "^(.+?)y$"
  in
    if contains re word
      then
        let stemi = submatchHelper re word
        in
          if contains s_v stemi
            then stemi ++ "i"
          else
            word
    else
      word


step1 : String -> String
step1 = step1a >> step1b >> step1c


step2and3and4 : Regex -> List (String, String) -> String -> String
step2and3and4 measure list word =
  let (from, to) =
        list
        |> List.filter (\(from2, _) -> String.endsWith from2 word)
        |> List.head
        |> Maybe.withDefault ("","")
      stemi = replace (regex from) (\_ -> "") word
  in
    if contains measure stemi
      then replace (regex from) (\_ -> to) word
    else
      word


step2 : String -> String
step2 = step2and3and4 mgr0 step2list


step3 : String -> String
step3 = step2and3and4 mgr0 step3list


step4 : String -> String
step4 = step2and3and4 mgr1 step4list


step5a : String -> String
step5a word =
  let re = regex "^(.+?)e$"
      test = find re word
  in
    if contains re word
      then
        let stemi = submatchHelper re word
            re2 = regex ("^" ++ c_ ++ v ++ "[^aeiouwxy]$")
        in
          if contains mgr1 stemi ||
              (contains meq1 stemi && (contains re2 stemi |> not))
            then stemi
          else
            word
    else
      word


step5b : String -> String
step5b word =
  if contains (regex "ll$") word && contains mgr1 word
    then replace (regex ".$") (\_ -> "") word
  else
    word


step5 : String -> String
step5 = step5a >> step5b


steps : String -> String
steps = step1 >> step2 >> step3 >> step4 >> step5


{-| The stem-function takes a word and returns its stem.

    stem "sky" == "sky"
    stem "hopefulness" == "hope"
-}
stem : String -> String
stem word =
  if String.length word < 3
    then word
  else if String.startsWith "y" word
    then
      "Y" ++ (String.dropLeft 1 word)
      |> steps
      |> String.dropLeft 1
      |> (++) "y"
  else
    steps word
