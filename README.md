Timelock Vault Smart Contract
Purpose: Lock STX tokens until a specific block height, then allow withdrawal.

Key Features:

Create time-locked STX deposits with designated beneficiaries
Top-up existing locks with additional STX
Extend unlock times (one-way only)
Change beneficiaries before withdrawal
Secure withdrawal system
Main Functions:

create-lock - Lock STX until specified block height
top-up - Add more STX to existing lock (owner only)
extend-lock - Push unlock time further (owner only)
withdraw - Claim locked STX after unlock (beneficiary only)
set-beneficiary - Change who can withdraw (owner only)
Error Codes: u100-u106 covering ownership, timing, and validation errors.

Use Cases: Token vesting, escrow services, savings accounts, grant distribution.

The contract includes admin sweep functionality and emits structured events for easy indexing.
