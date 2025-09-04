;; escrow Smart Contract
;; Part of the Pay-As-You-Go Solar Tokens Unlock daily solar access with token payments project
;; 
;; This contract implements escrow functionality
;; with comprehensive features and proper error handling.

;; Error constants
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-EXISTS (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-INSUFFICIENT-BALANCE (err u104))
(define-constant ERR-INVALID-RECIPIENT (err u105))
(define-constant ERR-CONTRACT-PAUSED (err u106))
(define-constant ERR-INVALID-PARAMETER (err u107))
(define-constant ERR-OPERATION-FAILED (err u108))
(define-constant ERR-TIMEOUT (err u109))

;; Data variables
(define-data-var contract-owner principal tx-sender)
(define-data-var contract-paused bool false)
(define-data-var total-supply uint u0)
(define-data-var fee-rate uint u100) ;; 1% fee (100 basis points)
(define-data-var min-amount uint u1)
(define-data-var max-amount uint u1000000)

;; Data maps
(define-map user-balances principal uint)
(define-map user-permissions principal bool)
(define-map operation-history
  { user: principal, operation-id: uint }
  {
    amount: uint,
    timestamp: uint,
    operation-type: (string-ascii 20),
    status: (string-ascii 10)
  }
)
(define-map pending-operations uint
  {
    user: principal,
    amount: uint,
    recipient: (optional principal),
    timestamp: uint,
    operation-type: (string-ascii 20)
  }
)

;; Auto-incrementing operation ID
(define-data-var next-operation-id uint u1)

;; Admin functions
(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)

(define-public (toggle-contract-pause)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (var-set contract-paused (not (var-get contract-paused)))
    (ok (var-get contract-paused))
  )
)

(define-public (set-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (asserts! (<= new-rate u1000) ERR-INVALID-PARAMETER) ;; Max 10% fee
    (var-set fee-rate new-rate)
    (ok new-rate)
  )
)

(define-public (set-amount-limits (min-amt uint) (max-amt uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (asserts! (< min-amt max-amt) ERR-INVALID-PARAMETER)
    (var-set min-amount min-amt)
    (var-set max-amount max-amt)
    (ok { min: min-amt, max: max-amt })
  )
)

;; Core functionality
(define-public (deposit (amount uint))
  (let (
    (current-balance (default-to u0 (map-get? user-balances tx-sender)))
    (operation-id (var-get next-operation-id))
  )
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    (asserts! (>= amount (var-get min-amount)) ERR-INVALID-AMOUNT)
    (asserts! (<= amount (var-get max-amount)) ERR-INVALID-AMOUNT)
    
    ;; Update balance
    (map-set user-balances tx-sender (+ current-balance amount))
    
    ;; Update total supply
    (var-set total-supply (+ (var-get total-supply) amount))
    
    ;; Record operation
    (map-set operation-history
      { user: tx-sender, operation-id: operation-id }
      {
        amount: amount,
        timestamp: stacks-block-height,
        operation-type: "deposit",
        status: "completed"
      }
    )
    
    ;; Increment operation ID
    (var-set next-operation-id (+ operation-id u1))
    
    (ok { operation-id: operation-id, new-balance: (+ current-balance amount) })
  )
)

(define-public (withdraw (amount uint))
  (let (
    (current-balance (default-to u0 (map-get? user-balances tx-sender)))
    (fee (/ (* amount (var-get fee-rate)) u10000))
    (net-amount (- amount fee))
    (operation-id (var-get next-operation-id))
  )
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    (asserts! (>= current-balance amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (>= amount (var-get min-amount)) ERR-INVALID-AMOUNT)
    
    ;; Update balance
    (map-set user-balances tx-sender (- current-balance amount))
    
    ;; Update total supply
    (var-set total-supply (- (var-get total-supply) amount))
    
    ;; Record operation
    (map-set operation-history
      { user: tx-sender, operation-id: operation-id }
      {
        amount: amount,
        timestamp: stacks-block-height,
        operation-type: "withdraw",
        status: "completed"
      }
    )
    
    ;; Increment operation ID
    (var-set next-operation-id (+ operation-id u1))
    
    (ok { operation-id: operation-id, net-amount: net-amount, fee: fee, new-balance: (- current-balance amount) })
  )
)

(define-public (transfer (recipient principal) (amount uint))
  (let (
    (sender-balance (default-to u0 (map-get? user-balances tx-sender)))
    (recipient-balance (default-to u0 (map-get? user-balances recipient)))
    (operation-id (var-get next-operation-id))
  )
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    (asserts! (>= sender-balance amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (not (is-eq tx-sender recipient)) ERR-INVALID-RECIPIENT)
    
    ;; Update balances
    (map-set user-balances tx-sender (- sender-balance amount))
    (map-set user-balances recipient (+ recipient-balance amount))
    
    ;; Record operation
    (map-set operation-history
      { user: tx-sender, operation-id: operation-id }
      {
        amount: amount,
        timestamp: stacks-block-height,
        operation-type: "transfer",
        status: "completed"
      }
    )
    
    ;; Increment operation ID
    (var-set next-operation-id (+ operation-id u1))
    
    (ok { operation-id: operation-id, recipient: recipient, amount: amount })
  )
)

;; Read-only functions
(define-read-only (get-balance (user principal))
  (default-to u0 (map-get? user-balances user))
)

(define-read-only (get-total-supply)
  (var-get total-supply)
)

(define-read-only (get-contract-info)
  {
    owner: (var-get contract-owner),
    paused: (var-get contract-paused),
    total-supply: (var-get total-supply),
    fee-rate: (var-get fee-rate),
    min-amount: (var-get min-amount),
    max-amount: (var-get max-amount),
    next-operation-id: (var-get next-operation-id)
  }
)

(define-read-only (get-operation-history (user principal) (operation-id uint))
  (map-get? operation-history { user: user, operation-id: operation-id })
)

(define-read-only (has-permission (user principal))
  (default-to false (map-get? user-permissions user))
)

;; Permission management
(define-public (grant-permission (user principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (map-set user-permissions user true)
    (ok true)
  )
)

(define-public (revoke-permission (user principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (map-delete user-permissions user)
    (ok true)
  )
)

;; Emergency functions
(define-public (emergency-withdraw-all)
  (let (
    (user-balance (get-balance tx-sender))
  )
    (asserts! (or 
      (is-eq tx-sender (var-get contract-owner))
      (has-permission tx-sender)
    ) ERR-UNAUTHORIZED)
    
    (if (> user-balance u0)
      (begin
        (map-delete user-balances tx-sender)
        (var-set total-supply (- (var-get total-supply) user-balance))
        (ok user-balance)
      )
      (ok u0)
    )
  )
)
