module Bank where

import Control.Monad.State

-- Type alias for Bank operations
type BankOp = State BankState

-- Bank state representation
data BankState = BankState {
    balance :: Float,   -- Current balance
    overdrawn :: Bool   -- Whether the account is overdrawn
} deriving Show

-- Initial bank state
initBankState :: BankState
initBankState = BankState {
    balance = 0.0,
    overdrawn = False
}

-- Deposit operation
deposit :: Float -> BankOp ()
deposit amount = modify $ \s -> s { balance = balance s + amount }

-- Withdraw operation
withdraw :: Float -> BankOp Float
withdraw amount = do
    s <- get
    let newBalance = balance s - amount
        isOverdrawn = newBalance < 0  -- Determine if overdrawn
        actualWithdraw = if newBalance < -100 then balance s + 100 else amount
    put $ s { balance = balance s - actualWithdraw, overdrawn = isOverdrawn }
    return actualWithdraw

-- Get the current balance
getBalance :: BankOp Float
getBalance = gets balance

-- Get whether the account is overdrawn
getOverdrawn :: BankOp Bool
getOverdrawn = gets overdrawn

-- Run a Bank operation starting with the initial state
runBankOp :: BankOp a -> a
runBankOp op = evalState op initBankState
