-- Test Ether:
-- if the donees' addresses accumulate test Ether, turn them into faucets for the demo
-- take your cues for what should be shown instead of the balances by looking at the test Ether transactions. if payer balance is shown, it must be the user's payer account balance.
-- user must experience being payee (set donation percent), so must have help getting accounts set up.
-- to change donation percent, e.g. after payer has already paid, user must be using payee's account.


port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Round
import Random
import Array exposing (get)


main =
    Html.program
        { init = ( model, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Donee =
    { name : String, balance : Float, address : String }


type PaidOrNot
    = HasPaid
    | NotPaid


type View
    = Payee
    | EtherOrNot
    | Payer
    | All PaidOrNot


type alias Model =
    { payeePercentInput : String
    , payeePercent : Float
    , payeeAddress :
        String
        -- for test Ether
    , payerPercent : Float
    , donee : { name : String, address : String }
    , donees : List Donee
    , balances : { payer : Float, payee : Float }
    , view : View
    , ether : Bool
    }


model : Model
model =
    { payeePercentInput = ""
    , payeePercent = 0
    , payeeAddress = ""
    , payerPercent = 0
    , donee = { name = "", address = "" }
    , donees = []
    , balances = { payer = 100, payee = 0 }
    , view = Payee
    , ether = False
    }



-- FIXME Decimal, not Float for setPercent


port setPercent : Float -> Cmd msg


port getBalances : { payee : String, donees : List String } -> Cmd msg


port balances : ({ payee : String, doneeBalances : List String } -> msg) -> Sub msg


port setPercentAgain : { percent : Float, payee : String } -> Cmd msg


port pay : { payment : Float, donee : String, payee : String } -> Cmd msg


port paid : (String -> msg) -> Sub msg


port payeeAddress : (String -> msg) -> Sub msg


subscriptions model =
    Sub.batch [ payeeAddress PayeeAddress, paid Paid, balances Balances ]


type Msg
    = PayeePercent String
    | PayeePercentOK
    | Ether
    | RandomInt Int
    | NoEther
    | PayeeAddress String
    | DoneeName String
    | DoneeAddress String
    | AddDonee String String
    | DeleteDonee String
    | DoneesOK
    | Balances { payee : String, doneeBalances : List String }
    | Pay
    | Paid String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PayeePercent input ->
            ( { model
                | payeePercentInput = input
                , payeePercent = Result.withDefault 0 (String.toFloat input)
              }
            , Cmd.none
            )

        PayeePercentOK ->
            -- TODO payee set percent on contract
            if validPercent model.payeePercent then
                -- payeePercent is valid
                ( { model
                    | payerPercent = model.payeePercent
                    , view =
                        case model.view of
                            Payee ->
                                EtherOrNot

                            _ ->
                                model.view
                  }
                , Cmd.none
                )
            else
                ( model, Cmd.none )

        Ether ->
            -- TODO now explain what's going to happen (ideally you should be able to pass a message for MetaMask to explain what's going on, because the user already trusts MetaMask (e.g. with their private keys)): "set payee's percent on Ethereum (costs Ether, so MetaMask will ask you to confirm)"
            -- TODO requires web3 and test ether: direct user to MetaMask or Parity plugin, faucet if they don't have enough test ether
            -- FIXME easy change view to "setting percent"
            ( { model | view = Payer, ether = True }, setPercent model.payeePercent )

        PayeeAddress address ->
            ( { model | payeeAddress = address }, Cmd.none )

        -- PercentSet ->
        -- TODO "4% will be donated."
        -- ({model | view = }, Cmd.none)
        NoEther ->
            ( { model | view = Payer, ether = False }, Cmd.none )

        DoneeName name ->
            ( { model | donee = { name = name, address = model.donee.address } }, Cmd.none )

        DoneeAddress address ->
            ( { model | donee = { name = model.donee.name, address = address } }, Cmd.none )

        AddDonee name address ->
            if List.any (\donee -> donee.name == name || donee.address == address) model.donees || name == "" || address == "" then
                -- any of the donees have the same name or address, the name is blank, or the address is invalid (FIXME use web3.isAddress)
                ( model, Cmd.none )
            else
                let
                    donees =
                        model.donees
                            ++ [ { name = name
                                 , balance = 0
                                 , address = address
                                 }
                               ]
                in
                    ( { model
                        | donees = donees
                        , donee = { name = "", address = "" }
                      }
                    , if model.ether then
                        getBalances { payee = model.payeeAddress, donees = (List.map .address donees) }
                      else
                        Cmd.none
                      -- FIXME get just this donee's balance
                    )

        DeleteDonee name ->
            ( { model | donees = List.filter (\donee -> donee.name /= name) model.donees }
            , Cmd.none
            )

        DoneesOK ->
            ( { model
                | view =
                    case model.view of
                        Payer ->
                            All NotPaid

                        _ ->
                            model.view
              }
            , if model.ether == True then
                -- TODO get balances if ether
                getBalances { payee = model.payeeAddress, donees = (List.map .address model.donees) }
              else
                Cmd.none
            )

        Balances { payee, doneeBalances } ->
            -- use "donees as doneeBalances"
            ( { model
                | balances = { payee = Result.withDefault 0 (String.toFloat payee), payer = model.balances.payer }
                , donees = List.map2 (\doneeBalance -> \donee -> { name = donee.name, address = donee.address, balance = Result.withDefault 0 (String.toFloat doneeBalance) }) doneeBalances model.donees
              }
            , Cmd.none
            )

        Pay ->
            case model.ether of
                True ->
                    ( model, Random.generate RandomInt (Random.int 0 (List.length model.donees - 1)) )

                False ->
                    -- FIXME donate to random donee too
                    let
                        percent =
                            model.payeePercent
                    in
                        if List.length model.donees == 0 || not (validPercent percent) || model.balances.payer == 0 then
                            -- no donees, percent is not 0 < % < 100, or payer balance is 0
                            ( model, Cmd.none )
                        else
                            let
                                payment =
                                    1

                                donation =
                                    -- payment * donation percent
                                    payment * percent / 100
                            in
                                ( { model
                                    | balances =
                                        { payer = model.balances.payer - payment
                                        , payee = model.balances.payee + (payment - donation)
                                        }
                                    , donees = List.map (\donee -> { donee | balance = donee.balance + donation / (List.length model.donees |> toFloat) }) model.donees
                                    , view = All HasPaid
                                  }
                                , Cmd.none
                                )

        RandomInt int ->
            ( model
            , pay
                { payment =
                    0.1
                    -- 0.1 so as not to spend too much test Ether at once
                    -- FIXME randomise payment amount
                , donee =
                    let
                        donee =
                            get int (Array.fromList model.donees)
                    in
                        case donee of
                            Just donee ->
                                donee.address

                            Nothing ->
                                ""
                , payee = model.payeeAddress
                }
            )

        Paid _ ->
            -- TODO update balances
            ( model, getBalances { payee = model.payeeAddress, donees = (List.map .address model.donees) } )


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
                    [ text "The payee decides how much to donate. "
                      -- FIXME enter to submit
                      -- FIXME select input on page load
                    , br [] []
                    , br [] []
                    , div []
                        [ label []
                            [ text "Donate ", input [ type_ "number", onInput PayeePercent, value model.payeePercentInput, Html.Attributes.min "0", Html.Attributes.max "100" ] [], text "%" ]
                        , button [ onClick PayeePercentOK, hidden (not (validPercent model.payeePercent)) ] [ text "OK" ]
                        ]
                    ]

            EtherOrNot ->
                div []
                    [ div []
                        [ button [ onClick Ether ] [ text "Try with test Ether (needs a Web3 provider, like MetaMask)" ]
                          -- FIXME check that user is not on Mainnet
                          -- TODO Say that it's only each time you want to change the percent (you don't have to set it for every payment) (2.12 GBP on Rinkeby)
                          -- TODO don't pay to set % if it's still the same for that payee
                        , button [ onClick NoEther ] [ text "Not now" ]
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
                                ++ [ button [ onClick DoneesOK, hidden (List.length model.donees == 0) ] [ text "OK" ] ]
                            )
                        , externalWebsites model
                        ]
                    ]

            All paidOrNot ->
                -- FIXME Y shaped
                -- FIXME do away with balances, which run out. Show the values of each transaction and see if that can replace balances. This is especially important when there are multiple donees and you can't see which one's balance changed. You don't want to have to work out which donee was donated to! Clicking Pay again and again won't change anything without balances: the values of the transaction will keep being the same, appearing not to change. So come up with a way to show the transaction happening, so it's clear that the values from the last transaction aren't the ones for the new one. You want the values to persist after the transaction, but not into the new one, so get rid of them when Pay is clicked. That will mean there'll have to be a delay between clicking Pay and seeing the new values, so the old ones have time to leave before the new ones. E.g. If I click Pay again and again , at 1^% I'll just see "0.01 as the donation", and it won't be clear that 0.01 is happen ing again and aiagin too. Balances are less clear what's going on on the initial payment.
                -- FIXME align donee balances to payer and payee balances
                div []
                    ((case paidOrNot of
                        NotPaid ->
                            -- FIXME "Switch to payer's account in MetaMask" if on payee's account.
                            [ text
                                ("The payer makes a payment."
                                    ++ if model.ether then
                                        " Switch to payer's account in MetaMask."
                                       else
                                        ""
                                )
                            ]

                        HasPaid ->
                            [ text "The code for Y is on ", a [ href "https://github.com/willnwhite/Y" ] [ text "GitHub" ], text "." ]
                      -- FIXME change to this after enough time has passed for the user to see that the balances have changed after Pay
                     )
                        ++ [ div [ style [ ( "display", "flex" ), ( "justify-content", "space-around" ) ] ]
                                [ div []
                                    ((if model.ether then
                                        [ party { name = "payee", balance = model.balances.payee } 2 ]
                                      else
                                        [ party { name = "payer", balance = model.balances.payer } 0
                                        , party { name = "payee", balance = model.balances.payee } 2
                                        ]
                                     )
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
                                    , button [ onClick PayeePercentOK, hidden (model.payeePercent == model.payerPercent || not (validPercent model.payeePercent)) ] [ text "OK" ]
                                    ]
                                , button
                                    [ onClick Pay
                                    , disabled (not (List.length model.donees > 0) || not (validPercent model.payerPercent))
                                      -- disables Pay button if there's not >= 1 donee or percent not valid
                                    ]
                                    -- FIXME randomise payment amount, to show that more is donated when more is paid (for the same donation percent)
                                    -- FIXME change "Pay" button to 0.1 with test Ether
                                    [ text ("Pay 1, donate " ++ (toString model.payerPercent) ++ "%") ]
                                ]
                           ]
                    )
        ]


externalWebsites model =
    div []
        [ text "donees' websites"
        , div []
            [ doneeWebsite "fire service" "0x1b1d8c35b19938ca017f4f28e9797c44a930b7b2" (List.any (\donee -> donee.name == "fire service") model.donees)
            , doneeWebsite "health service" "0x98eeeeb67ffe29b4e8d7c4a34613bd6569a17d94" (List.any (\donee -> donee.name == "health service") model.donees)
            , doneeWebsite "animal welfare" "0xb69d355e59acf8b6f345efb3a25432c36d98d70b" (List.any (\donee -> donee.name == "animal welfare") model.donees)
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
