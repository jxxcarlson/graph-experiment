module Currency exposing (Currency, Expiration(..), acct, c1, c2, c3, credit, debit, debitFolder, m1, m2)

import List.Extra


type alias Currency =
    { amount : Float
    , time : Int
    , expiration : Expiration
    }


type Expiration
    = Infinite
    | Finite Int


c1 =
    { amount = 1, time = 0, expiration = Infinite }


c2 =
    { amount = 1, time = 5, expiration = Infinite }


c3 =
    { amount = 1, time = 88, expiration = Infinite }


m1 =
    { amount = 10, time = 0, expiration = Infinite }


m2 =
    { amount = 10, time = 5, expiration = Infinite }


acct =
    [ m1, m2 ]


debit : Float -> List Currency -> ( List Currency, List Currency )
debit amount account_ =
    let
        sortedAccount_ =
            List.sortBy (\c -> c.time) account_
    in
    List.foldl debitFolder ( amount, ( [], [] ) ) account_ |> Tuple.second


credit : Currency -> List Currency -> List Currency
credit c account_ =
    case List.filter (\e -> e.time == c.time) account_ of
        [ e ] ->
            List.Extra.updateIf (\ee -> ee.time == c.time) (\ee -> { ee | amount = ee.amount + c.amount }) account_

        _ ->
            c :: account_


debitFolder : Currency -> ( Float, ( List Currency, List Currency ) ) -> ( Float, ( List Currency, List Currency ) )
debitFolder c ( amtRemaining, ( withDrawal, account ) ) =
    let
        amountToWithdraw =
            min c.amount amtRemaining

        newAccountEntry =
            { c | amount = c.amount - amountToWithdraw }

        newTransaction =
            { amount = amountToWithdraw, time = c.time, expiration = c.expiration }
    in
    ( amtRemaining - amountToWithdraw, ( newTransaction :: withDrawal, newAccountEntry :: account ) )
