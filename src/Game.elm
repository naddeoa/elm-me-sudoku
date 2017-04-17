module Game exposing (..)

import Dict exposing (Dict)
import Array exposing (Array)
import List exposing (map)
import Html exposing (Html, program, text)
import Html.Events as Events
import Html.Attributes as Attributes


-- Types


type alias Game =
    { board : Array (Array Symbol)
    , groups : List (List Point)
    , groupIndex : GroupIndex
    }


type Symbol
    = Symbol Int Point Group


type alias Point =
    ( Int, Int )


type GameEvent
    = Increment Symbol
    | Win
    | Wrong


type Group
    = Ungrouped
    | Group Int


type GroupIndex
    = GroupIndex (Dict Point Group) (Dict String (List Point))



-- Utility methods


getGroupName : Group -> String
getGroupName group =
    case group of
        Ungrouped ->
            "ungrouped"

        Group i ->
            "group-" ++ (toString i)


getGroupsPoints : Game -> Group -> List Point
getGroupsPoints { groupIndex } group =
    let
        (GroupIndex _ mapping) =
            groupIndex
    in
        Maybe.withDefault [] <| Dict.get (getGroupName group) mapping


defaultSymbol : Symbol
defaultSymbol =
    Symbol 0 ( 0, 0 ) Ungrouped


increment : Game -> Symbol -> Game
increment game (Symbol value ( row, column ) group) =
    let
        board =
            game.board

        targetRow : Array Symbol
        targetRow =
            Maybe.withDefault Array.empty (Array.get row game.board)

        updatedSymbol =
            if value == 5 then
                Symbol 0 ( row, column ) group
            else
                Symbol (value + 1) ( row, column ) group

        updatedRow =
            Array.set column updatedSymbol targetRow
    in
        { game | board = Array.set row updatedRow board }


{-| Get the Symbol at Point from the Game's board.
-}
getSymbol : Game -> Point -> Symbol
getSymbol { board } ( i, j ) =
    let
        row =
            Maybe.withDefault Array.empty <| Array.get i board
    in
        Maybe.withDefault defaultSymbol <| Array.get j row



-- Rendering methods


renderCell : Symbol -> Html GameEvent
renderCell (Symbol value point group) =
    Html.td
        [ Events.onClick (Increment (Symbol value point group))
        , Attributes.class <| "symbol-" ++ (toString value)
        , Attributes.class "symbol"
        , Attributes.class <| getGroupName group
        ]
        []


renderRow : Array Symbol -> Html GameEvent
renderRow row =
    Html.tr [] <| Array.toList (Array.map renderCell row)


render : Game -> Html GameEvent
render { board, groups } =
    Html.table [] <| Array.toList (Array.map renderRow board)


renderResults : Game -> Html a
renderResults { board, groups, groupIndex } =
    Html.ul []
        [ Html.li [] [ Html.text "fish" ]
        ]



-- Evaluating games win/lose state


evaluate : Game -> GameEvent
evaluate ({ board, groups } as game) =
    let
        results =
            Debug.log "results " <| List.map (evaluteGroup game) groups
    in
        if List.any ((==) Wrong) results then
            Wrong
        else
            Win


evaluteGroup : Game -> List Point -> GameEvent
evaluteGroup game points =
    let
        -- Get all of the symbols on the point list
        symbols : List Symbol
        symbols =
            List.map (getSymbol game) points

        -- The values that each of the symbols have currently. This is the number
        -- that represents the symbol shown.
        values : List Int
        values =
            List.map (\(Symbol value point group) -> value) symbols

        -- Used for keeping score on the group evaluation
        tracker : Array Int
        tracker =
            Array.repeat 6 0

        results : List Int
        results =
            Array.toList <|
                List.foldl
                    (\value acc -> Array.set value ((Maybe.withDefault 0 (Array.get value acc)) + 1) acc)
                    tracker
                    values

        -- Wrong if any of the numbers appear more than once or 0 appears at all. 0 is
        -- the default, unset state
        wrong =
            List.any (\i -> i > 1) results || (Maybe.withDefault 0 (List.head results)) /= 0
    in
        if wrong then
            Wrong
        else
            Win



-- GroupIndex stuff


indexGroup : List Point -> Group -> Dict Point Group
indexGroup points groupName =
    Dict.fromList <| List.map (\point -> ( point, groupName )) points


indexPoints : List Point -> Group -> Dict String (List Point)
indexPoints points group =
    Dict.singleton (getGroupName group) points


indexGroups : List (List Point) -> GroupIndex
indexGroups points =
    let
        indexedGroups =
            List.indexedMap (\i group -> indexGroup group (Group i)) points

        indexedPoints : List (Dict String (List Point))
        indexedPoints =
            List.indexedMap (\i group -> indexPoints group (Group i)) points
    in
        GroupIndex
            (List.foldl Dict.union Dict.empty indexedGroups)
            (List.foldl Dict.union Dict.empty indexedPoints)


getGroup : Game -> Symbol -> Group
getGroup { groupIndex } (Symbol _ point _) =
    let
        (GroupIndex dict _) =
            groupIndex
    in
        Maybe.withDefault Ungrouped <| Dict.get point dict


setGroup : GroupIndex -> Symbol -> Symbol
setGroup (GroupIndex index _) ((Symbol value point originalGroup) as symbol) =
    let
        group =
            Maybe.withDefault Ungrouped <| Dict.get point index
    in
        Symbol value point group



-- Default stuff


defaultGame : Game
defaultGame =
    let
        baseList =
            [ 0, 1, 2, 3, 4 ]

        initialRow : Int -> Array Symbol
        initialRow rowIndex =
            Array.fromList
                [ setGroup groupIndex <| (Symbol 0 ( rowIndex, 0 ) Ungrouped)
                , setGroup groupIndex <| (Symbol 0 ( rowIndex, 1 ) Ungrouped)
                , setGroup groupIndex <| (Symbol 0 ( rowIndex, 2 ) Ungrouped)
                , setGroup groupIndex <| (Symbol 0 ( rowIndex, 3 ) Ungrouped)
                , setGroup groupIndex <| (Symbol 0 ( rowIndex, 4 ) Ungrouped)
                ]

        specialGroups =
            [ [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 1, 0 ), ( 1, 1 ) ]
            , [ ( 2, 0 ), ( 3, 0 ), ( 4, 0 ), ( 3, 1 ), ( 4, 1 ) ]
            , [ ( 4, 2 ), ( 4, 3 ), ( 4, 4 ), ( 3, 3 ), ( 3, 4 ) ]
            , [ ( 0, 3 ), ( 0, 4 ), ( 1, 3 ), ( 1, 4 ), ( 2, 4 ) ]
            , [ ( 1, 2 ), ( 2, 1 ), ( 2, 2 ), ( 2, 3 ), ( 3, 2 ) ]
            ]

        groupIndex =
            indexGroups specialGroups
    in
        { board =
            Array.fromList
                [ (initialRow 0)
                , (initialRow 1)
                , (initialRow 2)
                , (initialRow 3)
                , (initialRow 4)
                ]
        , groupIndex = groupIndex
        , groups =
            List.append specialGroups
                [ -- All rows
                  List.indexedMap (\i n -> ( 0, i )) baseList
                , List.indexedMap (\i n -> ( 1, i )) baseList
                , List.indexedMap (\i n -> ( 2, i )) baseList
                , List.indexedMap (\i n -> ( 3, i )) baseList
                , List.indexedMap (\i n -> ( 4, i )) baseList
                  -- All columns
                , List.indexedMap (\i n -> ( i, 0 )) baseList
                , List.indexedMap (\i n -> ( i, 1 )) baseList
                , List.indexedMap (\i n -> ( i, 2 )) baseList
                , List.indexedMap (\i n -> ( i, 3 )) baseList
                , List.indexedMap (\i n -> ( i, 4 )) baseList
                ]
        }
