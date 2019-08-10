module Currency exposing (Currency, Expiration(..), credit, debit)

import List.Extra


type alias Currency =
    { amount : Float
    , time : Int
    , expiration : Expiration
    }


type Expiration
    = Infinite
    | Finite Int


type alias TimeUnit =
    Int


epsilon =
    0.000001


{-|

      > import TestCurrency exposing(..)
      > import Currency exposing(..)

      > debit 1 acct
      ([{ amount = 1, expiration = Infinite, time = 0 }],[{ amount = 10, expiration = Infinite, time = 5 },{ amount = 9, expiration = Infinite, time = 0 }])
          : ( List Currency, List Currency )
      > debit 10 acct
      ([{ amount = 10, expiration = Infinite, time = 0 }],[{ amount = 10, expiration = Infinite, time = 5 }])
          : ( List Currency, List Currency )
      > debit 11 acct
      ([{ amount = 1, expiration = Infinite, time = 5 },{ amount = 10, expiration = Infinite, time = 0 }],[{ amount = 9, expiration = Infinite, time = 5 }])
          : ( List Currency, List Currency )
      > debit 20 acct
      ([{ amount = 10, expiration = Infinite, time = 5 },{ amount = 10, expiration = Infinite, time = 0 }],[])
          : ( List Currency, List Currency )
      > debit 21 acct
      ([{ amount = 10, expiration = Infinite, time = 5 },{ amount = 10, expiration = Infinite, time = 0 }],[])
          : ( List Currency, List Currency )

-}
debit : TimeUnit -> Float -> List Currency -> ( List Currency, List Currency )
debit t amount account_ =
    let
        sortedAccount_ =
            account_
                |> List.filter (isValid t)
                |> List.sortBy (\c -> c.time)

        ( withDrawals, account2 ) =
            List.foldl debitFolder ( amount, ( [], [] ) ) account_ |> Tuple.second

        withDrawals2 =
            List.filter (\e -> abs e.amount > epsilon) withDrawals

        account3 =
            List.filter (\e -> abs e.amount > epsilon) account2
    in
    ( withDrawals2, account3 )


isValid : TimeUnit -> Currency -> Bool
isValid t c =
    case c.expiration of
        Infinite ->
            True

        Finite expirationTime ->
            expirationTime > t


{-|

      > credit c1 acct
      [{ amount = 11, expiration = Infinite, time = 0 },{ amount = 10, expiration = Infinite, time = 5 }]
          : List Currency
      > credit c2 acct
      [{ amount = 10, expiration = Infinite, time = 0 },{ amount = 11, expiration = Infinite, time = 5 }]
          : List Currency
      > credit c3 acct
      [{ amount = 1, expiration = Infinite, time = 88 },{ amount = 10, expiration = Infinite, time = 0 },{ amount = 10, expiration = Infinite, time = 5 }]
          : List Currency

-}
credit : TimeUnit -> Currency -> List Currency -> List Currency
credit t c account__ =
    let
        account_ =
            List.filter (isValid t) account__
    in
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
