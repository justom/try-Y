-- Test Ether:
-- "Pay 1": "with test Ether (needs MetaMask and test Ether), or here"
-- real Eth addresses for the donees
-- take your cues for what should be shown instead of the balances by looking at the test Ether transactions. if payer balance is shown, it must be the user's payer account balance.
-- user must experience being payee (set donation percent), so must have help getting accounts set up.
-- to change donation percent, e.g. after payer has already paid, user must be using payee's account.


module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Round


main =
    Html.beginnerProgram
        { model = model
        , update = update
        , view = view
        }


type alias Donee =
    { name : String, balance : Float, address : String }


type View
    = Payee
    | Payer
    | All


type alias Model =
    { payeePercentInput : String
    , payeePercent : Float
    , payerPercent : Float
    , donee : { name : String, address : String }
    , donees : List Donee
    , balances : { payer : Float, payee : Float }
    , view : View
    }


model : Model
model =
    { payeePercentInput = ""
    , payeePercent = 0
    , payerPercent = 0
    , donee = { name = "", address = "" }
    , donees = []
    , balances = { payer = 100, payee = 0 }
    , view = Payee
    }


type Msg
    = PayeePercent String
    | PayeePercentOK
    | DoneeName String
    | DoneeAddress String
    | AddDonee String String
    | DeleteDonee String
    | Pay
    | Donees


update : Msg -> Model -> Model
update msg model =
    case msg of
        Donees ->
            { model
                | view =
                    case model.view of
                        Payer ->
                            All

                        _ ->
                            model.view
            }

        PayeePercent input ->
            { model
                | payeePercentInput = input
                , payeePercent = Result.withDefault 0 (String.toFloat input)
            }

        PayeePercentOK ->
            if validPercent model.payeePercent then
                -- payeePercent is valid
                { model
                    | payerPercent = model.payeePercent
                    , view =
                        case model.view of
                            Payee ->
                                Payer

                            _ ->
                                model.view
                }
            else
                model

        DoneeName name ->
            { model | donee = { name = name, address = model.donee.address } }

        DoneeAddress address ->
            { model | donee = { name = model.donee.name, address = address } }

        AddDonee name address ->
            if List.any (\donee -> donee.name == name || donee.address == address) model.donees || name == "" || address == "" then
                -- any of the donees have the same name or address, the name is blank, or the address is invalid (FIXME use web3.isAddress)
                model
            else
                { model
                    | donees =
                        model.donees
                            ++ [ { name = name
                                 , balance = 0
                                 , address = address
                                 }
                               ]
                    , donee = { name = "", address = "" }
                }

        DeleteDonee name ->
            { model | donees = List.filter (\donee -> donee.name /= name) model.donees }

        Pay ->
            let
                percent =
                    model.payeePercent
            in
                if List.length model.donees == 0 || not (validPercent percent) || model.balances.payer == 0 then
                    -- no donees, percent is not 0 < % < 100, or payer balance is 0
                    model
                else
                    let
                        payment =
                            1

                        donation =
                            -- payment * donation percent
                            payment * percent / 100
                    in
                        { model
                            | balances =
                                { payer = model.balances.payer - payment
                                , payee = model.balances.payee + (payment - donation)
                                }
                            , donees = List.map (\donee -> { donee | balance = donee.balance + donation / (List.length model.donees |> toFloat) }) model.donees
                            , view = All
                        }


validPercent percent =
    0 < percent && percent < 100


view : Model -> Html Msg
view model =
    div []
        [ text "Y is a payment system that donates a % of each payment."
        , pre []
            [ text """
     donee     payee
          \\   /
         % \\ /
            Y
            |
            |
          payer

"""
            ]
        , case model.view of
            -- FIXME Do this after the Test Ether: move instruction next to area where it applies, e.g. "Payee chooses donation percent" next to the control the payee uses on the All view.
            Payee ->
                div []
                    [ text "The payee decides how much to donate."
                      -- FIXME enter to submit
                      -- FIXME select input on page load
                    , div []
                        [ label [] [ text "Donate ", input [ type_ "number", onInput PayeePercent, value model.payeePercentInput, Html.Attributes.min "0", Html.Attributes.max "100" ] [], text "%" ]
                        , button [ onClick PayeePercentOK ] [ text "OK" ]
                        ]
                    ]

            Payer ->
                div []
                    [ text "The payer chooses what to donate to."
                    , div [ style [ ( "display", "flex" ), ( "justify-content", "space-around" ) ] ]
                        [ div []
                            ((List.map
                                (\donee ->
                                    div
                                        [ style
                                            [ ( "display", "flex" )
                                            , ( "justify-content", "space-between" )
                                            ]
                                        ]
                                        [ partyWithoutBalance donee
                                        , button [ onClick (DeleteDonee donee.name) ] [ text "X" ]
                                        ]
                                )
                                model.donees
                             )
                                ++ [ button [ onClick Donees, hidden (List.length model.donees == 0) ] [ text "OK" ] ]
                            )
                        , externalWebsites model
                        ]
                    ]

            All ->
                -- FIXME Y shaped
                -- FIXME do away with balances, which run out. Show the values of each transaction and see if that can replace balances. Clicking Pay again and again won't change anything without balances: the values of the transaction will keep being the same, appearing not to change. So come up with a way to show the transaction happening, so it's clear that the values from the last transaction aren't the ones for the new one. You want the values to persist after the transaction, but not into the new one, so get rid of them when Pay is clicked. That will mean there'll have to be a delay between clicking Pay and seeing the new values, so the old ones have time to leave before the new ones. E.g. If I click Pay again and again , at 1^% I'll just see "0.01 as the donation", and it won't be clear that 0.01 is happen ing again and aiagin too. Balances are less clear what's going on on the initial payment.
                -- FIXME align donee balances to payer and payee balances
                div []
                    [ div [ style [ ( "display", "flex" ), ( "justify-content", "space-around" ) ] ]
                        [ div []
                            ([ party { name = "payer", balance = model.balances.payer } 0
                             , party { name = "payee", balance = model.balances.payee } 2
                             ]
                                ++ List.map
                                    (\donee ->
                                        div
                                            [ style
                                                [ ( "display", "flex" )
                                                , ( "justify-content", "space-between" )
                                                ]
                                            ]
                                            [ party donee 3, button [ onClick (DeleteDonee donee.name) ] [ text "X" ] ]
                                    )
                                    model.donees
                            )
                        , externalWebsites model
                        ]
                    , div
                        [ style
                            [ ( "display", "flex" )
                            , ( "justify-content", "space-around" )
                            , ( "width", "50%" )
                              -- moves Pay away from websites. Keep it near to payer
                            ]
                        ]
                        [ div []
                            [ label [] [ text "Donate ", input [ type_ "number", onInput PayeePercent, value model.payeePercentInput, Html.Attributes.min "0", Html.Attributes.max "100" ] [], text "%" ]
                            , button [ onClick PayeePercentOK, hidden (model.payeePercent == model.payerPercent) ] [ text "OK" ]
                            ]
                        , button
                            [ onClick Pay
                            , disabled (not (List.length model.donees > 0) || not (validPercent model.payerPercent))
                              -- disables Pay button if there's not >= 1 donee or percent not valid
                            ]
                            [ text ("Pay 1, Donate " ++ (toString model.payerPercent) ++ "%") ]
                        ]
                    , div []
                        [ a [ href "https://github.com/willnwhite/Y" ] [ text "Code on GitHub" ]
                        ]
                    ]
        ]


externalWebsites model =
    div []
        [ text "donees' websites"
        , div []
            [ doneeWebsite "fire service" "0x1" (List.any (\donee -> donee.name == "fire service") model.donees)
            , doneeWebsite "health service" "0x2" (List.any (\donee -> donee.name == "health service") model.donees)
            , doneeWebsite "animal welfare" "0x3" (List.any (\donee -> donee.name == "animal welfare") model.donees)
              -- NOTE name and address must be unique
            ]
          -- TODO scan in QR code
        , text "or"
        , div []
            [ div [] [ label [] [ text "name", input [ value model.donee.name, onInput DoneeName, type_ "text" ] [] ] ]
            , div [] [ label [] [ text "Ethereum address", input [ value model.donee.address, placeholder "0x...", onInput DoneeAddress, type_ "text" ] [] ] ]
            , div [] [ button [ onClick (AddDonee model.donee.name model.donee.address) ] [ text "Donate with Y" ] ]
            ]
        ]


party party dp =
    div [ style [ ( "display", "flex" ), ( "justify-content", "space-between" ) ] ]
        -- FIXME proper fixed-width font for balance
        [ text party.name
        , span [ style [ ( "font-family", "monospace" ) ] ]
            [ text (" " ++ Round.round dp party.balance) ]
        ]


partyWithoutBalance party =
    div [ style [ ( "display", "flex" ), ( "justify-content", "space-between" ) ] ]
        [ text party.name ]


doneeWebsite name address donating =
    div [ style [ ( "border", "solid" ) ] ]
        [ text name
        , if donating then
            button [ onClick (DeleteDonee name) ] [ text "Donating with Y" ]
          else
            button [ onClick (AddDonee name address) ] [ text "Donate with Y" ]
        ]
