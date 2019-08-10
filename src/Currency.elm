module Currency exposing
    ( Bank
    , BankTime
    , Currency
    , Expiration(..)
    , create
    , credit
    , creditMany
    , debit
    , debitMany
    , removeInvalid
    )

import List.Extra


type alias Currency =
    { amount : Float
    , time : Int
    , expiration : Expiration
    }


type alias Account =
    List Currency


type alias Transaction =
    List Currency


type alias Bank =
    { balance : List Currency }


type Expiration
    = Infinite
    | Finite Int


type alias BankTime =
    Int


type alias CurrencyUnit =
    Float


epsilon =
    0.000001


create : Expiration -> BankTime -> CurrencyUnit -> Bank -> Bank
create expiration creationTime amount bank =
    let
        newCurrency =
            { amount = amount, expiration = expiration, time = creationTime }
    in
    { bank | balance = credit creationTime newCurrency bank.balance }


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
debit : BankTime -> Float -> Account -> ( Transaction, Account )
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


isValid : BankTime -> Currency -> Bool
isValid t c =
    case c.expiration of
        Infinite ->
            True

        Finite expirationTime ->
            expirationTime > t


removeInvalid : BankTime -> List Currency -> List Currency
removeInvalid t currencyList =
    List.filter (isValid t) currencyList


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
credit : BankTime -> Currency -> List Currency -> List Currency
credit t c account__ =
    let
        account_ =
            List.filter (isValid t) account__
    in
    case List.filter (\e -> e.time == c.time && e.expiration == c.expiration) account_ of
        [ e ] ->
            List.Extra.updateIf (\ee -> ee.time == c.time) (\ee -> { ee | amount = ee.amount + c.amount }) account_

        _ ->
            c :: account_


creditMany : BankTime -> Transaction -> Account -> Account
creditMany t incoming account_ =
    List.foldl (\c acct -> credit t c acct) account_ incoming



-- debit : BankTime -> Float -> List Currency -> ( List Currency, List Currency )


debitMany : BankTime -> Transaction -> Account -> Account
debitMany t incoming account_ =
    List.foldl (\c acct -> debit t c.amount acct |> Tuple.second) account_ incoming


debitFolder : Currency -> ( Float, ( Transaction, Account ) ) -> ( Float, ( Transaction, Account ) )
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
