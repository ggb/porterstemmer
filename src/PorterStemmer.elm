module PorterStemmer (stem) where

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


-- consonant
c : String
c = "[^aeiou]"
-- vowel
v : String
v = "[aeiouy]"
-- consonant sequence
c' : String
c' = c ++ "[^aeiouy]*"
-- vowel sequence
v' : String
v' = v ++ "[aeiou]*"

-- [C]VC... is m > 0
mgr0 : Regex
mgr0 = regex ("^(" ++ c' ++ ")?" ++ v' ++ c')
-- [C]VC[V] is m=1
meq1 : Regex
meq1 = regex ("^(" ++ c' ++ ")?" ++ v' ++ c' ++ "(" ++ v' ++ ")?$")
-- [C]VCVC... is m>1
mgr1 : Regex
mgr1 = regex ("^(" ++ c' ++ ")?" ++ v' ++ c' ++ v' ++ c')
-- vowel in stem
s_v : Regex
s_v = regex ("^(" ++ c' ++ ")?" ++ v)


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
  find All re word 
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
      then replace All re capture word
    else if contains re2 word
      then replace All re2 capture word
    else
      word


step1b'' : Regex -> String -> String
step1b'' re word =
  let stem = submatchHelper re word
  in
    if contains s_v stem
      then
        let re2 = regex "(at|bl|iz)$"
            re3 = regex "([^aeiouylsz])\\1$"
            re4 = regex ("^" ++ c' ++ v ++ "[^aeiouwxy]$")
        in
          if contains re2 stem
            then stem ++ "e"
          else if contains re3 stem
            then replace All (regex ".$") (\_ -> "") stem
          else if contains re4 stem
            then stem ++ "e"
          else
            stem
    else
      word
   

step1b' : Regex -> String -> String
step1b' re word =
  let fp = submatchHelper re word
  in
    if contains mgr0 fp
      then replace All (regex ".$") (\_ -> "") word
    else
      word


step1b : String -> String
step1b word =
  let re = regex "^(.+?)eed$"
      re2 = regex "^(.+?)(ed|ing)$"
  in
    if contains re word
      then step1b' re word
    else if contains re2 word
      then step1b'' re2 word
    else
      word


step1c : String -> String
step1c word = 
  let re = regex "^(.+?)y$"
  in
    if contains re word
      then
        let stem = submatchHelper re word
        in
          if contains s_v stem
            then stem ++ "i"
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
        |> List.filter (\(from, _) -> String.endsWith from word)  
        |> List.head 
        |> Maybe.withDefault ("","")
      stem = replace All (regex from) (\_ -> "") word
  in
    if contains measure stem
      then replace All (regex from) (\_ -> to) word
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
      test = find All re word
  in
    if contains re word
      then
        let stem = submatchHelper re word
            re2 = regex ("^" ++ c' ++ v ++ "[^aeiouwxy]$")
        in 
          if contains mgr1 stem || 
              (contains meq1 stem && (contains re2 stem |> not))
            then stem
          else
            word
    else
      word


step5b : String -> String
step5b word =
  if contains (regex "ll$") word && contains mgr1 word
    then replace All (regex ".$") (\_ -> "") word
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