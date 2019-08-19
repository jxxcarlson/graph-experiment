module Currency exposing
    ( Account, Bank, BankTime
    , Currency, CurrencyType(..), CurrencyUnit, Expiration(..), isValid, removeInvalid
    , Transaction, create, credit, debit, creditMany, debitMany
    )

{-| The currency module models currency with an identity
and an expiration.

@docs Account, Bank, BankTime

@docs Currency, CurrencyType, CurrencyUnit, Expiration, isValid, removeInvalid

@docs Transaction, create, credit, debit, creditMany, debitMany

-}

import List.Extra


{-| Currency is the fundamental type of this module.
A Currency value has an omount, a type (Fiat or Complementary).
a time at which it was issued, and an expiration period,
which is either Infinite or Finite BankTime
-}
type alias Currency =
    { amount : Float
    , currencyType : CurrencyType
    , issueTime : Int
    , expiration : Expiration
    }


{-| CurrencyType is either Fiat (like dollars), or Complmentary
-}
type CurrencyType
    = Fiat
    | Complementary


{-| An account is a list of currency values. Such a structure
is needed to properly handle Currency, since different values
may have different expiration periods, etc.
-}
type alias Account =
    List Currency


{-| A transaction is a list of Currency values. Suppose we want to
debit 7 units from an account, and the account as list containing
5 units with an expiration of Finite 13 and 5 units with an Expiration
of Finite 11. Then (according to the algorithm used) the debit
transaction would be a list with 5 units with expiration Finite 11
and 2 units with expiration Finite 13 -)
-}
type alias Transaction =
    List Currency


{-| A Bank is a special Account
-}
type alias Bank =
    { balance : List Currency }


{-| The expiration of a Currency value
can be either Infinite or Finite k, where
k is measured in BankTime
-}
type Expiration
    = Infinite
    | Finite BankTime


{-| BankTime is an alias for Int. It represents
a discrete time, typically modeled as the tick of a clock.
It could represent a number of days, a multiple of a fraction of a day, etc.
-}
type alias BankTime =
    Int


{-| CurrencyUnit is just an alias for Float
-}
type alias CurrencyUnit =
    Float


epsilon =
    0.000001


tenUnits =
    { amount = 10, currencyType = Complementary, expiration = Finite 10, issueTime = 1 }


{-| Currency.create is used to create a new value in the account of a Bank
-}
create : CurrencyType -> Expiration -> BankTime -> CurrencyUnit -> Bank -> Bank
create currencyType expiration creationTime amount bank =
    let
        newCurrency =
            { amount = amount, expiration = expiration, issueTime = creationTime, currencyType = currencyType }
    in
    { bank | balance = credit creationTime newCurrency bank.balance }


{-|

      > import TestCurrency exposing(..)
      > import Currency exposing(..)

      > debit 1 acct
      ([{ amount = 1, expiration = Infinite, issueTime = 0 }],[{ amount = 10, expiration = Infinite, issueTime = 5 },{ amount = 9, expiration = Infinite, issueTime = 0 }])
          : ( List Currency, List Currency )
      > debit 10 acct
      ([{ amount = 10, expiration = Infinite, issueTime = 0 }],[{ amount = 10, expiration = Infinite, issueTime = 5 }])
          : ( List Currency, List Currency )
      > debit 11 acct
      ([{ amount = 1, expiration = Infinite, issueTime = 5 },{ amount = 10, expiration = Infinite, issueTime = 0 }],[{ amount = 9, expiration = Infinite, issueTime = 5 }])
          : ( List Currency, List Currency )
      > debit 20 acct
      ([{ amount = 10, expiration = Infinite, issueTime = 5 },{ amount = 10, expiration = Infinite, issueTime = 0 }],[])
          : ( List Currency, List Currency )
      > debit 21 acct
      ([{ amount = 10, expiration = Infinite, issueTime = 5 },{ amount = 10, expiration = Infinite, issueTime = 0 }],[])
          : ( List Currency, List Currency )

-}
debit : BankTime -> Float -> Account -> ( Transaction, Account )
debit t amount account_ =
    let
        sortedAccount_ =
            account_
                |> List.filter (isValid t)
                |> List.sortBy (\c -> c.issueTime)

        ( withDrawals, account2 ) =
            List.foldl debitFolder ( amount, ( [], [] ) ) account_ |> Tuple.second

        withDrawals2 =
            List.filter (\e -> abs e.amount > epsilon) withDrawals

        account3 =
            List.filter (\e -> abs e.amount > epsilon) account2
    in
    ( withDrawals2, account3 )


debitFolder : Currency -> ( Float, ( Transaction, Account ) ) -> ( Float, ( Transaction, Account ) )
debitFolder c ( amtRemaining, ( withDrawal, account ) ) =
    let
        amountToWithdraw =
            min c.amount amtRemaining

        newAccountEntry =
            { c | amount = c.amount - amountToWithdraw }

        newTransaction =
            { c | amount = amountToWithdraw }
    in
    ( amtRemaining - amountToWithdraw, ( newTransaction :: withDrawal, newAccountEntry :: account ) )


{-| isValid t c determines whether the currency value c is valid at time t.
-}
isValid : BankTime -> Currency -> Bool
isValid t c =
    case c.expiration of
        Infinite ->
            True

        Finite expirationTime ->
            expirationTime > t - c.issueTime


{-| removeInvalid t ll removes currency from the list ll
that is no longer valid at time t
-}
removeInvalid : BankTime -> List Currency -> List Currency
removeInvalid t currencyList =
    List.filter (isValid t) currencyList


{-| Currency.credit is used to credit an account with currency value.

      > credit c1 acct
      [{ amount = 11, expiration = Infinite, issueTime = 0 },{ amount = 10, expiration = Infinite, issueTime = 5 }]
          : List Currency
      > credit c2 acct
      [{ amount = 10, expiration = Infinite, issueTime = 0 },{ amount = 11, expiration = Infinite, issueTime = 5 }]
          : List Currency
      > credit c3 acct
      [{ amount = 1, expiration = Infinite, issueTime = 88 },{ amount = 10, expiration = Infinite, issueTime = 0 },{ amount = 10, expiration = Infinite, issueTime = 5 }]
          : List Currency

-}
credit : BankTime -> Currency -> List Currency -> List Currency
credit t c account__ =
    let
        account_ =
            List.filter (isValid t) account__
    in
    case List.filter (\e -> e.issueTime == c.issueTime && e.expiration == c.expiration) account_ of
        [ e ] ->
            List.Extra.updateIf (\ee -> ee.issueTime == c.issueTime) (\ee -> { ee | amount = ee.amount + c.amount }) account_

        _ ->
            c :: account_


{-| creditMany is used to credit a transaction to an account.
-}
creditMany : BankTime -> Transaction -> Account -> Account
creditMany t incoming account_ =
    List.foldl (\c acct -> credit t c acct) account_ incoming



-- debit : BankTime -> Float -> List Currency -> ( List Currency, List Currency )


{-| debitMany is used to debit a transaction from an account.
-}
debitMany : BankTime -> Transaction -> Account -> Account
debitMany t incoming account_ =
    List.foldl (\c acct -> debit t c.amount acct |> Tuple.second) account_ incoming
