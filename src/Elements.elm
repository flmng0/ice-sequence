module Elements exposing (Element, displayName, generate)

import Debug
import Random


type Foot
    = Dominant
    | NonDominant


type Direction
    = Forward
    | Backward


type Side
    = Inside
    | Outside


type alias Edge =
    { foot : Foot, dir : Direction, side : Side }


type Name
    = Dependent String String String
    | Always String


type alias Element =
    { entry : Edge
    , exit : Edge
    , name : Name
    }


displayName : Element -> Bool -> String
displayName e rightFooted =
    case e.name of
        Dependent right left name ->
            if rightFooted then
                right ++ " " ++ name

            else
                left ++ " " ++ name

        Always name ->
            name



-- EDGE VARIANTS


dfi : Edge
dfi =
    Edge Dominant Forward Inside


dfo : Edge
dfo =
    Edge Dominant Forward Outside


dbi : Edge
dbi =
    Edge Dominant Backward Inside


dbo : Edge
dbo =
    Edge Dominant Backward Outside


nfi : Edge
nfi =
    Edge NonDominant Forward Inside


nfo : Edge
nfo =
    Edge NonDominant Forward Outside


nbi : Edge
nbi =
    Edge NonDominant Backward Inside


nbo : Edge
nbo =
    Edge NonDominant Backward Outside


landing : Edge
landing =
    dbo


addLeftRight : Foot -> String -> Name
addLeftRight foot name =
    case foot of
        Dominant ->
            Dependent "Right" "Left" name

        NonDominant ->
            Dependent "Left" "Right" name



-- ELEMENTS


threeTurns : List Element
threeTurns =
    [ Element dfi dbo (addLeftRight Dominant "Forward Inside 3-Turn")
    , Element dfo dbi (addLeftRight Dominant "Forward Outside 3-Turn")
    , Element nfi nbo (addLeftRight NonDominant "Forward Inside 3-Turn")
    , Element nfo nbi (addLeftRight NonDominant "Forward Outside 3-Turn")
    , Element dbi dfo (addLeftRight Dominant "Backward Inside 3-Turn")
    , Element dbo dfi (addLeftRight Dominant "Backward Outside 3-Turn")
    , Element nbi nfo (addLeftRight NonDominant "Backward Inside 3-Turn")
    , Element nbo nfi (addLeftRight NonDominant "Backward Outside 3-Turn")
    ]


mohawks : List Element
mohawks =
    [ Element dfi nbi (addLeftRight Dominant "Mohawk")
    , Element nfi dbi (addLeftRight NonDominant "Mohawk")
    ]


jumps : List Element
jumps =
    [ Element nfo landing (Always "Waltz")
    , Element nfo landing (Always "Axel")
    , Element dbo landing (Always "Toe-Loop")
    , Element dbo landing (Always "Loop")
    , Element nbi landing (Always "Salchow")
    , Element nbi landing (Always "Flip")
    , Element nbo landing (Always "Lutz")
    ]


forwardCrossovers : ( Element, Element )
forwardCrossovers =
    ( Element nfo dfi (Dependent "Counter-Clockwise" "Clockwise" "Forward Crossover")
    , Element dfo nfi (Dependent "Clockwise" "Counter-Clockwise" "Forward Crossover")
    )


backwardCrossovers : ( Element, Element )
backwardCrossovers =
    ( Element dbo nbi (Dependent "Counter-Clockwise" "Clockwise" "Backward Crossover")
    , Element nbo dbi (Dependent "Clockwise" "Counter-Clockwise" "Backward Crossover")
    )


crossovers : List Element
crossovers =
    let
        ( ccwf, cwf ) =
            forwardCrossovers

        ( ccwb, cwb ) =
            backwardCrossovers
    in
    [ ccwf, cwf, ccwb, cwb ]


footWorkElements : List Element
footWorkElements =
    List.concat
        [ threeTurns
        , mohawks
        , crossovers
        ]


allElements : List Element
allElements =
    List.concat
        [ footWorkElements
        , jumps
        ]


canTransition : Element -> Element -> Bool
canTransition a b =
    a.exit == b.entry


validElementsFrom : Element -> List Element
validElementsFrom e =
    List.filter (canTransition e) allElements


choose : a -> a -> Random.Generator a
choose a b =
    Random.uniform a [ b ]


randomSequence : Int -> Random.Generator a -> (a -> Random.Generator a) -> Random.Generator (List a)
randomSequence n seed produce =
    let
        next ( hd, tl ) =
            Random.pair (produce hd) (Random.constant (hd :: tl))

        aux i acc =
            if i == 1 then
                Random.map (\( hd, tl ) -> List.reverse (hd :: tl)) acc

            else
                aux (i - 1) (Random.andThen next acc)
    in
    if n > 0 then
        aux n (Random.pair seed (Random.constant []))

    else
        Random.constant []


generate : Int -> Random.Generator (List Element)
generate n =
    let
        seed =
            case crossovers of
                head :: tail ->
                    Random.uniform head tail

                [] ->
                    Random.constant (Tuple.first forwardCrossovers)

        produce m =
            case validElementsFrom m of
                head :: tail ->
                    Random.uniform head tail

                [] ->
                    if m.exit.dir == Forward then
                        choose (Tuple.first forwardCrossovers) (Tuple.second forwardCrossovers)

                    else
                        choose (Tuple.first backwardCrossovers) (Tuple.second backwardCrossovers)
    in
    randomSequence n seed produce
