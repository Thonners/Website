module Font exposing (..)

import Element exposing (Attribute)
import Element.Font as Font


handwritingFont : Attribute msg
handwritingFont =
    Font.family
        [ Font.typeface "Virgil"
        , Font.sansSerif
        ]
