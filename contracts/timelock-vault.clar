;; ------------------------------------------------------------
;; Contract: timelock-vault
;; Purpose : Lock STX until a specific block height, then allow withdrawal.
;; Version : Clarity v2
;; Author  : you + ChatGPT
;;
;; Features:
;; - Create a lock with STX -> beneficiary can withdraw after unlock-height.
;; - Increase the locked amount later (multiple top-ups).
;; - Extend the unlock time (one-way; can only move farther in the future).
;; - Change the beneficiary before unlock (owner-only).
;; - Read-only views for UI/analytics.
;;
;; Notes:
;; - This implementation focuses on STX (native token). See Extensions
;;   section in comments below for how to support SIP-010 tokens or NFTs.
;; ------------------------------------------------------------

;; -------------------------
;; Error codes
;; -------------------------
(define-constant ERR-NOT-OWNER            (err u100))
(define-constant ERR-NOT-BENEFICIARY      (err u101))
(define-constant ERR-INVALID-AMOUNT       (err u102))
(define-constant ERR-ALREADY-WITHDRAWN    (err u103))
(define-constant ERR-NOT-YET-UNLOCKED     (err u104))
(define-constant ERR-LOCK-NOT-FOUND       (err u105))
(define-constant ERR-CANNOT-SHORTEN-LOCK  (err u106))

;; -------------------------
;; Storage
;; -------------------------

(define-data-var next-lock-id uint u1)
(define-data-var contract-owner principal tx-sender)

(define-map locks
  { id: uint }
  {
    owner: principal,
    beneficiary: principal,
    amount-ustx: uint,
    created-height: uint,
    unlock-height: uint,
    withdrawn: bool
  }
)

;; -------------------------
;; Events (via print)
;; -------------------------
;; For indexers/UX. These are structured for easy off-chain parsing.
;; - lock-created
;; - lock-topped-up
;; - lock-extended
;; - beneficiary-updated
;; - lock-withdrawn

;; -------------------------
;; Helpers
;; -------------------------

(define-read-only (get-lock (lock-id uint))
  (match (map-get? locks { id: lock-id })
    some-lock (ok some-lock)
    (err u404))
)

(define-read-only (current-id)
  (var-get next-lock-id)
)

(define-read-only (now)
  burn-block-height
)

(define-read-only (is-unlocked (lock-id uint))
  (match (map-get? locks { id: lock-id })
    some-lock (ok (>= burn-block-height (get unlock-height some-lock)))
    (err u404))
)

(define-read-only (time-remaining (lock-id uint))
  (match (map-get? locks { id: lock-id })
    some-lock 
    (let ((u (get unlock-height some-lock))
          (h burn-block-height))
      (ok (if (>= h u) u0 (- u h))))
    (err u404))
)

;; -------------------------
;; Public entrypoints
;; -------------------------

;; Create a new STX timelock. Caller provides:
;; - amount in uSTX to lock
;; - unlock-height: block height when funds become withdrawable
;; - beneficiary: who can withdraw later (can be the owner themself)
(define-public (create-lock (amount-ustx uint) (unlock-height uint) (beneficiary principal))
  (begin
    (asserts! (> amount-ustx u0) ERR-INVALID-AMOUNT)
    (asserts! (> unlock-height burn-block-height) ERR-NOT-YET-UNLOCKED)

    ;; Pull STX from the creator into this contract as collateral
    (try! (stx-transfer? amount-ustx tx-sender (as-contract tx-sender)))

    (let ((id (var-get next-lock-id)))
      ;; persist
      (map-set locks { id: id }
        {
          owner: tx-sender,
          beneficiary: beneficiary,
          amount-ustx: amount-ustx,
          created-height: burn-block-height,
          unlock-height: unlock-height,
          withdrawn: false
        })
      (var-set next-lock-id (+ id u1))

      (print { event: "lock-created", id: id, owner: tx-sender, beneficiary: beneficiary, amount: amount-ustx, unlock: unlock-height })
      (ok id))
  )
)

;; Increase the amount locked in an existing lock (owner-only).
;; Allows a lock owner to add more STX to an existing timelock.
;; @param lock-id The unique identifier of the existing timelock
;; @param additional-ustx The additional amount of uSTX to add to the lock
;; @returns (ok true) if top-up succeeds
;;          (err ERR-LOCK-NOT-FOUND) if lock with given ID doesn't exist
;;          (err ERR-INVALID-AMOUNT) if additional amount is 0
;;          (err ERR-NOT-OWNER) if caller is not the lock owner
;;          (err ERR-ALREADY-WITHDRAWN) if lock has already been withdrawn
(define-public (top-up (lock-id uint) (additional-ustx uint))
  (asserts! (< lock-id (var-get next-lock-id)) ERR-LOCK-NOT-FOUND)
  (match (map-get? locks { id: lock-id })
    (ok l) (begin
      (asserts! (> additional-ustx u0) ERR-INVALID-AMOUNT)
      (asserts! (is-eq tx-sender (get owner l)) ERR-NOT-OWNER)
      (asserts! (not (get withdrawn l)) ERR-ALREADY-WITHDRAWN)

      ;; transfer more STX into the vault
      (try! (stx-transfer? additional-ustx tx-sender (as-contract tx-sender)))

      (map-set locks { id: lock-id }
        (merge l 
          { amount-ustx: (+ (get amount-ustx l) additional-ustx) }))

      (print { event: "lock-topped-up", id: lock-id, by: tx-sender, added: additional-ustx, newTotal: (+ (get amount-ustx l) additional-ustx) })
      (ok true))
    ERR-LOCK-NOT-FOUND)
)

;; Extend the unlock height (owner-only). Can only increase (push later).
(define-public (extend-lock (lock-id uint) (new-unlock-height uint))
  (asserts! (< lock-id (var-get next-lock-id)) ERR-LOCK-NOT-FOUND)
  (match (map-get? locks { id: lock-id })
    some-lock 
    (begin
      (asserts! (is-eq tx-sender (get owner some-lock)) ERR-NOT-OWNER)
      (asserts! (not (get withdrawn some-lock)) ERR-ALREADY-WITHDRAWN)
      (asserts! (> new-unlock-height (get unlock-height some-lock)) ERR-CANNOT-SHORTEN-LOCK)

      (map-set locks { id: lock-id }
        (merge some-lock { unlock-height: new-unlock-height }))

      (print { event: "lock-extended", id: lock-id, newUnlock: new-unlock-height })
      (ok true))
    ERR-LOCK-NOT-FOUND)
)

;; Update the beneficiary (owner-only) before withdrawal.
(define-public (set-beneficiary (lock-id uint) (new-beneficiary principal))
  (asserts! (< lock-id (var-get next-lock-id)) ERR-LOCK-NOT-FOUND)
  (match (map-get? locks { id: lock-id })
    some-lock 
    (begin
      (asserts! (is-eq tx-sender (get owner some-lock)) ERR-NOT-OWNER)
      (asserts! (not (get withdrawn some-lock)) ERR-ALREADY-WITHDRAWN)

      (map-set locks { id: lock-id }
        (merge some-lock { beneficiary: new-beneficiary }))

      (print { event: "beneficiary-updated", id: lock-id, beneficiary: new-beneficiary })
      (ok true))
    ERR-LOCK-NOT-FOUND)
)

;; Withdraw the locked STX (beneficiary-only) when unlock-height is reached.
(define-public (withdraw (lock-id uint))
  (asserts! (< lock-id (var-get next-lock-id)) ERR-LOCK-NOT-FOUND)
  (match (map-get? locks { id: lock-id })
    some-lock 
    (begin
      (asserts! (not (get withdrawn some-lock)) ERR-ALREADY-WITHDRAWN)
      (asserts! (is-eq tx-sender (get beneficiary some-lock)) ERR-NOT-BENEFICIARY)
      (asserts! (>= burn-block-height (get unlock-height some-lock)) ERR-NOT-YET-UNLOCKED)

      (let ((amt (get amount-ustx some-lock))
            (to (get beneficiary some-lock)))
        ;; send STX out
        (try! (stx-transfer? amt (as-contract tx-sender) to))

        ;; mark withdrawn
        (map-set locks { id: lock-id }
          (merge some-lock { withdrawn: true, amount-ustx: u0 }))

        (print { event: "lock-withdrawn", id: lock-id, to: to, amount: amt })
        (ok true)))
    ERR-LOCK-NOT-FOUND)
)

;; -------------------------
;; Admin (optional safety)
;; -------------------------

;; Sweep stray STX accidentally sent without using the API.
;; Only the contract deployer can call this; does not touch active locks.
(define-public (admin-sweep-stx (to principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err u401))
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (not (is-eq to (as-contract tx-sender))) (err u405))
    (try! (stx-transfer? amount (as-contract tx-sender) to))
    (ok true))
)

;; -------------------------
;; Extensions (notes)
;; -------------------------
;; - SIP-010 fungible tokens:
;;   You can mirror this design by storing {token: principal, amount: uint}
;;   and requiring users to FIRST transfer tokens to this contract, then
;;   record the lock with a register-ft-lock entrypoint. On withdrawal,
;;   the contract transfers tokens out to the beneficiary. Because SIP-010
;;   does not standardize allowances, pulling tokens from the user inside
;;   this contract is not reliable across all tokens.
;;
;; - SIP-009 NFTs:
;;   Store {nft: principal, token-id: uint}. Require users to transfer the
;;   NFT into the contract before recording the lock. Withdraw sends it back.
;;
;; - Cliff + vesting:
;;   Replace unlock-height with a schedule and unlock portions over time.
;;
;; - Composability:
;;   Consider emitting richer events (via print) that indexers can consume.
