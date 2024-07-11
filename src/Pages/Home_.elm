module Pages.Home_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (..)
import View exposing (View)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)
import Html.Styled.Events exposing (..)
import Svg.Styled as Svg exposing (svg, path)
import Svg.Styled.Attributes as SvgAttr
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Css
import Tailwind.Theme as Theme
import Tailwind.Color exposing (arbitraryRgb)
import Css.Global
import Time
--import Task


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

-- INIT

type alias Model =
    { name : String
    , messageFromTauri : Maybe String
    , startTime: Maybe Time.Posix
    , time: Time.Posix
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { name = ""
      , messageFromTauri = Nothing
      , time = (Time.millisToPosix 0)
      , startTime = Nothing
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = ChangedName String
    | SubmittedForm
    | TauriResponded String
    | Tick Time.Posix
    | Start
    | Stop



update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ChangedName name ->
            ( { model | name = name }
            , Effect.none
            )

        SubmittedForm ->
            ( model
            , Effect.sendNameToTauri model.name
            )

        TauriResponded reply ->
            ( { model | messageFromTauri = Just reply }
            , Effect.none
            )

        Tick newTime ->
             ( { model | time = newTime }
             , Effect.none
             )

        Start -> 
            ({model| startTime = Just model.time}, Effect.none)
        Stop -> 
            ({model| startTime = Nothing }, Effect.none)




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [ Effect.listenForGreeting TauriResponded
    , Time.every 1000 Tick
    ]


-- VIEW

timeButton: Msg -> String -> Html Msg
timeButton msg label = 
         button
            [ Attr.type_ "button"
            , Attr.id "startButton"
            , onClick msg
            , css
                [ Tw.bg_color Tw.gray_600
                , Tw.px_16
                , Tw.py_5
                , Tw.mt_3
                , Tw.uppercase
                , Tw.tracking_wider
                , Tw.rounded_sm
                , Css.pseudoClass "active"
                    [ Tw.bg_color <| arbitraryRgb  66 16 66
                    ]
                , Css.hover
                    [ Tw.border_color <| arbitraryRgb  93 22 93
                    ]
                ]
            ]
            [ text label]

view : Model -> View Msg
view model =
   let
        secondsElapsed =
            case model.startTime of
                Just start ->
                    Time.posixToMillis model.time - Time.posixToMillis start
                        |> (\ms -> ms // 1000)

                Nothing ->
                    0
    in
    { title = "Homepage"
    , body =
        [
        Css.Global.global [Css.Global.body [Css.backgroundColor <| Css.rgb 60 20 60]]
        ,
        header
            [ css
                [ Tw.flex
                , Tw.justify_between
                , Tw.m_3
                ]
            ]
            [   
                h1 [] [ text <| String.fromInt secondsElapsed ]
                ,span
                [ Attr.id "logo"
                , css []
                ]
                [ a
                    [ Attr.href "/"
                    ]
                    [ text "Task Timer" ]
                ]
            , span
                [ Attr.id "sessionCount"
                , css []
                ]
                [ text "Session #1" ]
            , a
                [ css
                    [ Tw.flex
                    , Tw.items_center
                    ]
                , Attr.target "_blank"
                , Attr.href "https://github.com/freexploit/tufu"
                ]
                [ svg
                    [ SvgAttr.css
                        [ Tw.inline
                        ]
                    , SvgAttr.width "24"
                    , SvgAttr.height "24"
                    , SvgAttr.viewBox "0 0 24 24"
                    , SvgAttr.fill "none"
                    , SvgAttr.stroke "currentColor"
                    , SvgAttr.strokeWidth "2"
                    , SvgAttr.strokeLinecap "round"
                    , SvgAttr.strokeLinejoin "round"
                    ]
                    [ path
                        [ SvgAttr.d "M9 19c-5 1.5-5-2.5-7-3m14 6v-3.87a3.37 3.37 0 0 0-.94-2.61c3.14-.35 6.44-1.54 6.44-7A5.44 5.44 0 0 0 20 4.77 5.07 5.07 0 0 0 19.91 1S18.73.65 16 2.48a13.38 13.38 0 0 0-7 0C6.27.65 5.09 1 5.09 1A5.07 5.07 0 0 0 5 4.77a5.44 5.44 0 0 0-1.5 3.78c0 5.42 3.3 6.61 6.44 7A3.37 3.37 0 0 0 9 18.13V22"
                        ]
                        []
                    ]
                , span
                    [ css
                        [ Tw.pl_1
                        ]
                    ]
                    [ text "GitHub" ]
                ]
            ]
        ,     div
            [ Attr.id "clock"
            , css
                [ Tw.bg_color <| arbitraryRgb 17 17 18
                , Tw.text_center
                , Tw.pb_6
                ]
            ]
            [ div
                [ css
                    [ Tw.mx_auto
                    , Tw.relative
                    ]
                ]
                [ div
                    [ Attr.id "timer"
                    , css
                        [ Tw.filter
                        , Tw.drop_shadow
                        , Tw.text_7xl
                        , Tw.absolute
                        , Tw.top_1over2
                        , Tw.left_1over2
                        , Tw.neg_translate_x_1over2
                        , Tw.neg_translate_y_1over2
                        ]
                    ]
                    [ text <| String.fromInt secondsElapsed]
                , div
                    [ css
                        [ Tw.w_full
                        , Tw.mx_auto
                        , Bp.lg
                            [ Tw.w_2over5
                            ]
                        , Bp.md
                            [ Tw.w_1over2
                            ]
                        , Bp.sm
                            [ Tw.w_3over5
                            ]
                        , Bp.xl
                            [ Tw.w_1over3
                            ]
                        ]
                    ]
                    [ svg
                        [ SvgAttr.viewBox "0 0 39 39"
                        , SvgAttr.id "timerRing"
                        , SvgAttr.fill "none"
                        , SvgAttr.css []
                        ]
                        [ Svg.defs []
                            [ Svg.linearGradient
                                [ SvgAttr.id "timer-ring-gradient"
                                ]
                                [ Svg.stop
                                    [ SvgAttr.offset "0%"
                                    , SvgAttr.stopColor "#FFB56B"
                                    ]
                                    []
                                , Svg.stop
                                    [ SvgAttr.offset "30%"
                                    , SvgAttr.stopColor "#BC365D"
                                    ]
                                    []
                                , Svg.stop
                                    [ SvgAttr.offset "100%"
                                    , SvgAttr.stopColor "#1F005C"
                                    ]
                                    []
                                ]
                            ]
                        , Svg.circle
                            [ SvgAttr.id "shadow-circle"
                            , SvgAttr.css
                                [ Tw.filter
                                , Tw.drop_shadow
                                ]
                            , SvgAttr.stroke "#2D2D2D"
                            , SvgAttr.strokeWidth "3px"
                            , SvgAttr.cx "50%"
                            , SvgAttr.cy "50%"
                            , SvgAttr.r "15.9155"
                            ]
                            []
                        , Svg.circle
                            [ SvgAttr.id "timer-circle"
                            , SvgAttr.css
                                [ Tw.neg_rotate_90
                                , Tw.origin_center
                                ]
                            , SvgAttr.stroke "url(#timer-ring-gradient)"
                            , SvgAttr.strokeDasharray "0, 100"
                            , SvgAttr.strokeWidth "3px"
                            , SvgAttr.strokeLinecap "round"
                            , SvgAttr.cx "50%"
                            , SvgAttr.cy "50%"
                            , SvgAttr.r "15.9155"
                            ]
                            []
                        ]
                    ]
                ]
            , timeButton Start "Start"
            , timeButton Stop "Stop"
            ]
        ,     div
            [ Attr.id "options"
            , css
                [ Tw.container
                , Tw.mx_auto
                ]
            ]
            [ div
                [ Attr.id "timerOptions"
                , css
                    [ Tw.py_5
                    , Tw.flex
                    , Tw.justify_center
                    , Tw.items_center
                    , Tw.text_center
                    ]
                ]
                [ svg
                    [ SvgAttr.css
                        [ Tw.stroke_color Theme.current
                        , Tw.text_color Tw.rose_900
                        , Tw.inline_block
                        , Tw.mr_4
                        ]
                    , SvgAttr.width "30"
                    , SvgAttr.height "30"
                    , SvgAttr.viewBox "0 0 24 24"
                    , SvgAttr.fill "none"
                    , SvgAttr.strokeWidth "2.5"
                    , SvgAttr.strokeLinecap "round"
                    , SvgAttr.strokeLinejoin "round"
                    ]
                    [ Svg.circle
                        [ SvgAttr.cx "12"
                        , SvgAttr.cy "12"
                        , SvgAttr.r "10"
                        ]
                        []
                    , Svg.polyline
                        [ SvgAttr.points "12 6 12 12 16 14"
                        ]
                        []
                    ]
                , input
                    [ Attr.type_ "radio"
                    , Attr.name "timer-length"
                    , css
                        [ Tw.hidden
                        ]
                    , Attr.value "25"
                    , Attr.id "25"
                    , Attr.checked True
                    ]
                    []
                , label
                    [ css
                        [ Tw.w_24
                        , Tw.py_2
                        , Tw.inline_block
                        , Tw.bg_color <| Tw.gray_600
                        , Tw.cursor_pointer
                        , Tw.border_b_4
                        , Tw.border_color Tw.transparent
                        , Css.hover
                            [ Tw.border_color <| arbitraryRgb 93 22 93 
                            ]
                        ]
                    , Attr.for "25"
                    ]
                    [ text "25 MIN" ]
                , input
                    [ Attr.type_ "radio"
                    , Attr.name "timer-length"
                    , css
                        [ Tw.hidden
                        ]
                    , Attr.value "20"
                    , Attr.id "20"
                    ]
                    []
                , label
                    [ css
                        [ Tw.w_24
                        , Tw.py_2
                        , Tw.inline_block
                        , Tw.bg_color Tw.gray_600
                        , Tw.cursor_pointer
                        , Tw.border_b_4
                        , Tw.border_color Tw.transparent
                        , Css.hover
                            [ Tw.border_color <| arbitraryRgb 93 22 93
                            ]
                        ]
                    , Attr.for "20"
                    ]
                    [ text "20 MIN" ]
                , input
                    [ Attr.type_ "radio"
                    , Attr.name "timer-length"
                    , css
                        [ Tw.hidden
                        ]
                    , Attr.value "15"
                    , Attr.id "15"
                    ]
                    []
                , label
                    [ css
                        [ Tw.w_24
                        , Tw.py_2
                        , Tw.inline_block
                        , Tw.bg_color Tw.gray_600
                        , Tw.cursor_pointer
                        , Tw.border_b_4
                        , Tw.border_color Tw.transparent
                        , Css.hover
                            [ Tw.border_color <| arbitraryRgb 93 22 93
                            ]
                        ]
                    , Attr.for "15"
                    ]
                    [ text "15 MIN" ]
                , input
                    [ Attr.type_ "radio"
                    , Attr.name "timer-length"
                    , css
                        [ Tw.hidden
                        ]
                    , Attr.value "10"
                    , Attr.id "10"
                    ]
                    []
                , label
                    [ css
                        [ Tw.w_24
                        , Tw.py_2
                        , Tw.inline_block
                        , Tw.bg_color Tw.gray_600
                        , Tw.cursor_pointer
                        , Tw.border_b_4
                        , Tw.border_color Tw.transparent
                        , Css.hover
                            [ Tw.border_color <| arbitraryRgb  93 22 93
                            ]
                        ]
                    , Attr.for "10"
                    ]
                    [ text "10 MIN" ]
                ]
            ]
        ]
    }
