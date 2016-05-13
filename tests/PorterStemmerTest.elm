module PorterStemmerTest exposing (..)

import Html exposing(..)
import ElmTest exposing (..)
import String
import PorterStemmer exposing (stem)


tests : Test
tests =
  suite "Porter Stemmer Tests"
    [ defaultTest (assertEqual (stem "consign") "consign")
    , defaultTest (assertEqual (stem "consigned") "consign")
    , defaultTest (assertEqual (stem "consigning") "consign")
    , defaultTest (assertEqual (stem "consignment") "consign")
    , defaultTest (assertEqual (stem "consist") "consist")
    , defaultTest (assertEqual (stem "consisted") "consist")
    , defaultTest (assertEqual (stem "consistency") "consist")
    , defaultTest (assertEqual (stem "consistent") "consist")
    , defaultTest (assertEqual (stem "consistently") "consist")
    , defaultTest (assertEqual (stem "consisting") "consist")
    , defaultTest (assertEqual (stem "consists") "consist")
    , defaultTest (assertEqual (stem "consolation") "consol")
    , defaultTest (assertEqual (stem "consolations") "consol")
    , defaultTest (assertEqual (stem "consolatory") "consolatori")
    , defaultTest (assertEqual (stem "console") "consol")
    , defaultTest (assertEqual (stem "consoled") "consol")
    , defaultTest (assertEqual (stem "consoles") "consol")
    , defaultTest (assertEqual (stem "consolidate") "consolid")
    , defaultTest (assertEqual (stem "consolidated") "consolid")
    , defaultTest (assertEqual (stem "consolidating") "consolid")
    , defaultTest (assertEqual (stem "consoling") "consol")
    , defaultTest (assertEqual (stem "consols") "consol")
    , defaultTest (assertEqual (stem "consonant") "conson")
    , defaultTest (assertEqual (stem "consort") "consort")
    , defaultTest (assertEqual (stem "consorted") "consort")
    , defaultTest (assertEqual (stem "consorting") "consort")
    , defaultTest (assertEqual (stem "conspicuous") "conspicu")
    , defaultTest (assertEqual (stem "conspicuously") "conspicu")
    , defaultTest (assertEqual (stem "conspiracy") "conspiraci")
    , defaultTest (assertEqual (stem "conspirator") "conspir")
    , defaultTest (assertEqual (stem "conspirators") "conspir")
    , defaultTest (assertEqual (stem "conspire") "conspir")
    , defaultTest (assertEqual (stem "conspired") "conspir")
    , defaultTest (assertEqual (stem "conspiring") "conspir")
    , defaultTest (assertEqual (stem "constable") "constabl")
    , defaultTest (assertEqual (stem "constables") "constabl")
    , defaultTest (assertEqual (stem "constance") "constanc")
    , defaultTest (assertEqual (stem "constancy") "constanc")
    , defaultTest (assertEqual (stem "constant") "constant")
    , defaultTest (assertEqual (stem "knack") "knack")
    , defaultTest (assertEqual (stem "knackeries") "knackeri")
    , defaultTest (assertEqual (stem "knacks") "knack")
    , defaultTest (assertEqual (stem "knag") "knag")
    , defaultTest (assertEqual (stem "knave") "knave")
    , defaultTest (assertEqual (stem "knaves") "knave")
    , defaultTest (assertEqual (stem "knavish") "knavish")
    , defaultTest (assertEqual (stem "kneaded") "knead")
    , defaultTest (assertEqual (stem "kneading") "knead")
    , defaultTest (assertEqual (stem "knee") "knee")
    , defaultTest (assertEqual (stem "kneel") "kneel")
    , defaultTest (assertEqual (stem "kneeled") "kneel")
    , defaultTest (assertEqual (stem "kneeling") "kneel")
    , defaultTest (assertEqual (stem "kneels") "kneel")
    , defaultTest (assertEqual (stem "knees") "knee")
    , defaultTest (assertEqual (stem "knell") "knell")
    , defaultTest (assertEqual (stem "knelt") "knelt")
    , defaultTest (assertEqual (stem "knew") "knew")
    , defaultTest (assertEqual (stem "knick") "knick")
    , defaultTest (assertEqual (stem "knif") "knif")
    , defaultTest (assertEqual (stem "knife") "knife")
    , defaultTest (assertEqual (stem "knight") "knight")
    , defaultTest (assertEqual (stem "knights") "knight")
    , defaultTest (assertEqual (stem "knit") "knit")
    , defaultTest (assertEqual (stem "knits") "knit")
    , defaultTest (assertEqual (stem "knitted") "knit")
    , defaultTest (assertEqual (stem "knitting") "knit")
    , defaultTest (assertEqual (stem "knives") "knive")
    , defaultTest (assertEqual (stem "knob") "knob")
    , defaultTest (assertEqual (stem "knobs") "knob")
    , defaultTest (assertEqual (stem "knock") "knock")
    , defaultTest (assertEqual (stem "knocked") "knock")
    , defaultTest (assertEqual (stem "knocker") "knocker")
    , defaultTest (assertEqual (stem "knockers") "knocker")
    , defaultTest (assertEqual (stem "knocking") "knock")
    , defaultTest (assertEqual (stem "knocks") "knock")
    , defaultTest (assertEqual (stem "knopp") "knopp")
    , defaultTest (assertEqual (stem "knot") "knot")
    , defaultTest (assertEqual (stem "knots") "knot")
    --
    , defaultTest (assertEqual (stem "lay") "lai")
    , defaultTest (assertEqual (stem "try") "try")
    , defaultTest (assertEqual (stem "sky") "sky")
    , defaultTest (assertEqual (stem "happy") "happi")
    ]


line : String -> Html String
line s =
  div [ ] [
    text s,
    br [ ] [ ]
  ]


main : Html String
main =
  let
    lines = tests |> stringRunner |> String.lines
  in
    div [ ] (List.map (\s -> line s) lines)
