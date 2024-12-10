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
deposit amount = modify $ \s -> s {
    balance = balance s + amount,
    overdrawn = balance s + amount < 0  -- Update overdrawn flag
}

-- Withdraw operation
withdraw :: Float -> BankOp Float
withdraw amount = do
    s <- get
    let currentBalance = balance s
        newBalance = currentBalance - amount
        actualWithdraw =
            if newBalance < -100 -- Limit to $100 overdraft
            then currentBalance + 100
            else amount
        isOverdrawn = (currentBalance - actualWithdraw) < 0
    put $ s { balance = currentBalance - actualWithdraw, overdrawn = isOverdrawn }
    return actualWithdraw

-- Get the current balance
getBalance :: BankOp Float
getBalance = gets balance

-- Run a Bank operation starting with the initial state
runBankOp :: BankOp a -> a
runBankOp op = evalState op initBankState
