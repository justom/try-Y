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
    , metaMask : Maybe Bool
    , payeeAddress : Maybe String
    , contractPercent : Maybe Float
    , donee : { name : String, address : String }
    , donees : List Donee
    , balances : { payer : Float, payee : Float }
    , view : View
    , ether : Maybe Bool
    , paying : Bool
    }


model : Model
model =
    { payeePercentInput = ""
    , payeePercent = 0
    , metaMask = Nothing
    , payeeAddress = Nothing
    , contractPercent = Nothing
    , donee = { name = "", address = "" }
    , donees = []
    , balances = { payer = 100, payee = 0 }
    , view = Payee
    , ether = Nothing
    , paying = False
    }



-- FIXME Decimal, not Float for setPercent


port haveMetaMask : () -> Cmd msg


port hasMetaMask : (Bool -> msg) -> Sub msg


port setPercent : Float -> Cmd msg


port getBalances : { payee : String, donees : List String } -> Cmd msg


port balances : ({ payee : String, doneeBalances : List String } -> msg) -> Sub msg


port pay : { payment : Float, donee : String, payee : String } -> Cmd msg


port paid : (() -> msg) -> Sub msg


port payeeAddress : (String -> msg) -> Sub msg


port percentSet : (() -> msg) -> Sub msg


port payerGetPercent : String -> Cmd msg


port percentGot : (String -> msg) -> Sub msg


subscriptions model =
    Sub.batch
        [ hasMetaMask HasMetaMask
        , payeeAddress PayeeAddress
        , paid Paid
        , balances Balances
        , percentSet PercentSet
        , percentGot PercentGot
        ]


type Msg
    = PayeePercent String
    | PayeePercentOK
    | HasMetaMask Bool
    | Ether
    | RandomInt Int
    | NoEther
    | PayeeAddress String
    | PercentSet ()
    | PercentGot String
    | DoneeName String
    | DoneeAddress String
    | AddDonee String String
    | DeleteDonee String
    | DoneesOK
    | Balances { payee : String, doneeBalances : List String }
    | Pay
    | Paid ()


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
            if validPercent model.payeePercent then
                -- payeePercent is valid
                case model.ether of
                    Nothing ->
                        -- model.ether is undecided the first time.
                        ( { model
                            | view =
                                case model.view of
                                    Payee ->
                                        EtherOrNot

                                    _ ->
                                        model.view
                          }
                        , haveMetaMask ()
                        )

                    Just True ->
                        -- FIXME publish: show user that percent is being set
                        -- FIXME don't let payer set percent
                        -- FIXME don't let percent be set while it is being set on the network (two requests at once)
                        ( { model
                            | view =
                                case model.view of
                                    Payee ->
                                        EtherOrNot

                                    _ ->
                                        model.view
                          }
                        , setPercent model.payeePercent
                        )

                    Just False ->
                        ( { model
                            | contractPercent = Just model.payeePercent
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

        HasMetaMask trueOrFalse ->
            ( { model | metaMask = Just trueOrFalse }, Cmd.none )

        Ether ->
            -- FIXME test-ether: If there's no MetaMask, don't do anything.
            -- FIXME test-ether: "Unlock MetaMask" if you have it but it's locked when it's needed (setPercent)
            -- TODO now explain what's going to happen (ideally you should be able to pass a message for MetaMask to explain what's going on, because the user already trusts MetaMask (e.g. with their private keys)): "set payee's percent on Ethereum (costs Ether, so MetaMask will ask you to confirm)"
            -- TODO requires web3 and test ether: direct user to MetaMask or Parity plugin, faucet if they don't have enough test ether
            -- FIXME easy change view to "setting percent"
            -- TODO "use account as payee account" before MetaMask pop-up window
            case model.metaMask of
                Just True ->
                    ( { model | view = Payer, ether = Just True }, setPercent model.payeePercent )

                _ ->
                    ( model, Cmd.none )

        PayeeAddress address ->
            ( { model | payeeAddress = Just address }, Cmd.none )

        PercentSet _ ->
            ( model
            , case model.payeeAddress of
                Just address ->
                    payerGetPercent address

                Nothing ->
                    Cmd.none
            )

        PercentGot percent ->
            -- TODO "4% will be donated." show percent as soon as it's in the model, not just in a certain view: show the % (and "setting/getting %") on the Y model.
            -- FIXME what if getPercent fails? Handle error in JS or here in Elm?
            let
                percentAsFloat =
                    String.toFloat percent
            in
                case percentAsFloat of
                    Ok percent ->
                        ( { model | contractPercent = Just percent }, Cmd.none )

                    Err _ ->
                        -- FIXME show error to user so they can fix it
                        ( model, Cmd.none )

        NoEther ->
            ( { model | view = Payer, ether = Just False, contractPercent = Just model.payeePercent }, Cmd.none )

        DoneeName name ->
            ( { model | donee = { name = name, address = model.donee.address } }, Cmd.none )

        DoneeAddress address ->
            ( { model | donee = { name = model.donee.name, address = address } }, Cmd.none )

        AddDonee name address ->
            if List.any (\donee -> donee.name == name || donee.address == address) model.donees || name == "" || address == "" then
                -- any of the donees have the same name or address, the name is blank, or the address is invalid (FIXME test-ether: use web3.isAddress)
                -- TODO error message for user if name or address is the same
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
                        , donee =
                            { name = "", address = "" }
                            -- resets donee fields
                      }
                    , case model.ether of
                        Just True ->
                            -- FIXME get just this donee's balance
                            -- FIXME caching: don't get it if you've already got it (e.g. if you delete donee then add it straight away)
                            case model.payeeAddress of
                                Just address ->
                                    getBalances { payee = address, donees = (List.map .address donees) }

                                Nothing ->
                                    Cmd.none

                        _ ->
                            Cmd.none
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
            , case model.ether of
                Just True ->
                    -- TODO get balances if ether
                    case model.payeeAddress of
                        Just address ->
                            getBalances { payee = address, donees = (List.map .address model.donees) }

                        Nothing ->
                            Cmd.none

                _ ->
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
            -- FIXME don't let payer pay if there's already a payment going through
            case model.ether of
                Just True ->
                    if model.paying then
                        ( model, Cmd.none )
                    else
                        case model.contractPercent of
                            Just _ ->
                                -- FIXME publish: show that tx is in progress (confirmations)
                                ( { model | paying = True }, Random.generate RandomInt (Random.int 0 (List.length model.donees - 1)) )

                            Nothing ->
                                ( model, Cmd.none )

                _ ->
                    -- FIXME test-ether: donate to random donee too, so it's the same as test ether
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
            , case model.payeeAddress of
                Just address ->
                    pay
                        { payment =
                            0.1
                            -- TODO randomise payment amount
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
                        , payee = address
                        }

                Nothing ->
                    Cmd.none
            )

        Paid _ ->
            -- FIXME publish: Paying goes to Pay before balances change. Presumably that's because the first confirmation caused paying = False here, but the balances didn't change until a later confirmation, or getting the balance was slow from the first confirmation. QUESTION What's the event where the new balances will be available? If it doesn't work like that, you could stop the Paying button changing to Pay until the balances change. Consider changing Msg.Paid to Msg.Confirmed, to more accurately reflect what's going on.
            -- tx means transaction
            ( { model | paying = False }
            , case model.payeeAddress of
                Just address ->
                    getBalances { payee = address, donees = (List.map .address model.donees) }

                Nothing ->
                    Cmd.none
            )


validPercent percent =
    0 < percent && percent < 100


view : Model -> Html Msg
view model =
    -- FIXME When payee changes percent, the percent on the payer's "Pay, donate" button does change immediately. This might be confusing. Make it clear that "this is what the payee sees" (the setPercent controls) and "this is what the payer sees" (the "Pay, donate" button): put the set percent controls near to the word "payee", and the "Pay, donate" button near to the word "payer": do this to the words on the Y. Then when the payee changes the percent, the payee's arm of Y can show that it's updating the "Y" contract in the middle.
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
            -- FIXME Do this after the Test Ether: move instruction next to area where it applies, e.g. "Payee chooses donation percent" next to the control the payee uses on the All view. Or, first, keep the instruction in one place, and highlight the control it's talking about. Later, move the instruction around, but this might require animation so the user can follow it around the screen.
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
                        -- FIXME test-ether: show the "needs MetaMask" bit only if there's no MetaMask.
                        [ button
                            [ disabled
                                (case model.metaMask of
                                    Just True ->
                                        False

                                    _ ->
                                        True
                                )
                            , onClick Ether
                            ]
                            [ text
                                (case model.metaMask of
                                    Just True ->
                                        "Try with test Ether"

                                    _ ->
                                        "Try with test Ether (needs a Web3 provider, like MetaMask)"
                                )
                            ]
                          -- TODO "payee: [address of selected account]"
                          -- TODO Say that it's only each time you want to change the percent (you don't have to set it for every payment) (2.12 GBP on Rinkeby)
                          -- FIXME don't pay to set % if it's still the same for that payee
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
                -- FIXME do away with balances, which run out. Show the values of each transaction and see if that can replace balances. This is especially important when there are multiple donees and you can't see which one's balance changed. You don't want to have to work out which donee was donated to! Clicking Pay again and again won't change anything without balances: the values of the transaction will keep being the same, appearing not to change. So come up with a way to show the transaction happening, so it's clear that the values from the last transaction aren't the ones for the new one. You want the values to persist after the transaction, but not into the new one, so get rid of them when Pay is clicked. That will mean there'll have to be a delay between clicking Pay and seeing the new values, so the old ones have time to leave before the new ones. E.g. If I click Pay again and again , at 1^% I'll just see "0.01 as the donation", and it won't be clear that 0.01 is happen ing again and aiagin too. Balances are less clear what's going on on the initial payment.
                -- TODO Y shaped
                div []
                    ([ text
                        ("The payer makes a payment."
                            ++ case model.ether of
                                Just True ->
                                    -- FIXME only if on payee's account.
                                    " Switch to payer's account in MetaMask."

                                _ ->
                                    ""
                        )
                     ]
                        ++ [ div [ style [ ( "display", "flex" ), ( "justify-content", "space-around" ) ] ]
                                [ div []
                                    ((case model.ether of
                                        Just True ->
                                            [ party { name = "payee", balance = model.balances.payee } 2 ]

                                        _ ->
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
                                    , button
                                        [ onClick PayeePercentOK
                                        , hidden
                                            ((case model.contractPercent of
                                                Just percent ->
                                                    model.payeePercent == percent

                                                Nothing ->
                                                    False
                                             )
                                                || not (validPercent model.payeePercent)
                                            )
                                        ]
                                        [ text "OK" ]
                                    ]
                                , button
                                    [ onClick Pay
                                    , disabled
                                        (not (List.length model.donees > 0)
                                            || not
                                                (case model.contractPercent of
                                                    Just percent ->
                                                        validPercent percent

                                                    Nothing ->
                                                        False
                                                )
                                            || model.paying
                                        )
                                      -- disables Pay button if there's not >= 1 donee or percent not valid
                                    ]
                                    -- TODO disable button and "switch to payer's account"
                                    -- FIXME randomise payment amount, to show that more is donated when more is paid (for the same donation percent)
                                    [ text
                                        (if model.paying then
                                            "Paying"
                                         else
                                            ("Pay "
                                                ++ (case model.ether of
                                                        Just True ->
                                                            "0.1"

                                                        _ ->
                                                            "1"
                                                   )
                                                ++ ", donate "
                                                ++ (case model.contractPercent of
                                                        Just percent ->
                                                            toString percent ++ "%"

                                                        Nothing ->
                                                            "(getting %)"
                                                   )
                                            )
                                        )
                                    ]
                                ]
                           ]
                    )
        ]


externalWebsites model =
    div []
        [ text "donees' websites"
        , div []
            [ doneeWebsite "fire service" "0x3DeC24f8EfFfB46Fd2D47503Ef60d9f5EE17227D" (List.any (\donee -> donee.name == "fire service") model.donees)
            , doneeWebsite "health service" "0x98eeeeb67ffe29b4e8d7c4a34613bd6569a17d94" (List.any (\donee -> donee.name == "health service") model.donees)
            , doneeWebsite "animal welfare" "0xb69d355e59acf8b6f345efb3a25432c36d98d70b" (List.any (\donee -> donee.name == "animal welfare") model.donees)
              -- NOTE name and address must be unique
            ]
          -- IDEA scan in QR code
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
