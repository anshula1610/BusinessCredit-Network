;; BusinessCredit Network Contract
;; B2B credit scoring and lending based on transaction history and business relationships

;; Define the fungible token for credit tokens
(define-fungible-token credit-token)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-unauthorized (err u101))
(define-constant err-insufficient-credit (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-business-not-found (err u104))
(define-constant err-already-registered (err u105))

;; Data variables
(define-data-var total-businesses uint u0)
(define-data-var total-credit-issued uint u0)

;; Business profile structure
(define-map business-profiles 
  principal 
  {
    business-name: (string-ascii 50),
    registration-date: uint,
    credit-score: uint,
    total-transactions: uint,
    transaction-volume: uint,
    active: bool
  })

;; Business relationships and transaction history
(define-map business-transactions
  {sender: principal, recipient: principal}
  {
    total-amount: uint,
    transaction-count: uint,
    last-transaction: uint,
    relationship-score: uint
  })

;; Credit limits for businesses
(define-map credit-limits principal uint)

;; Function 1: Register Business and Calculate Initial Credit Score
(define-public (register-business (business-name (string-ascii 50)) (initial-transaction-volume uint))
  (let (
    (existing-business (map-get? business-profiles tx-sender))
    (initial-credit-score (calculate-initial-credit-score initial-transaction-volume))
    (credit-limit (/ (* initial-credit-score u1000) u100)) ;; Credit limit based on score
  )
    ;; Check if business is already registered
    (asserts! (is-none existing-business) err-already-registered)
    (asserts! (> (len business-name) u0) err-invalid-amount)
    (asserts! (>= initial-transaction-volume u0) err-invalid-amount)
    
    ;; Register the business profile
    (map-set business-profiles tx-sender {
      business-name: business-name,
      registration-date:stacks-block-height,
      credit-score: initial-credit-score,
      total-transactions: u0,
      transaction-volume: initial-transaction-volume,
      active: true
    })
    
    ;; Set initial credit limit
    (map-set credit-limits tx-sender credit-limit)
    
    ;; Mint initial credit tokens based on credit score
    (try! (ft-mint? credit-token credit-limit tx-sender))
    
    ;; Update total businesses count
    (var-set total-businesses (+ (var-get total-businesses) u1))
    (var-set total-credit-issued (+ (var-get total-credit-issued) credit-limit))
    
    (ok {
      credit-score: initial-credit-score,
      credit-limit: credit-limit,
      business-id: tx-sender
    })))

;; Function 2: Execute B2B Transaction and Update Credit Scores
(define-public (execute-b2b-transaction (recipient principal) (amount uint) (memo (optional (buff 34))))
  (let (
    (sender-profile (unwrap! (map-get? business-profiles tx-sender) err-business-not-found))
    (recipient-profile (unwrap! (map-get? business-profiles recipient) err-business-not-found))
    (existing-relationship (default-to 
      {total-amount: u0, transaction-count: u0, last-transaction: u0, relationship-score: u0} 
      (map-get? business-transactions {sender: tx-sender, recipient: recipient})))
    (sender-credit-limit (unwrap! (map-get? credit-limits tx-sender) err-insufficient-credit))
  )
    ;; Validate transaction
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (get active sender-profile) err-unauthorized)
    (asserts! (get active recipient-profile) err-unauthorized)
    (asserts! (>= (ft-get-balance credit-token tx-sender) amount) err-insufficient-credit)
    
    ;; Execute the credit token transfer
    (try! (ft-transfer? credit-token amount tx-sender recipient))
    
    ;; Update sender's business profile
    (map-set business-profiles tx-sender (merge sender-profile {
      total-transactions: (+ (get total-transactions sender-profile) u1),
      transaction-volume: (+ (get transaction-volume sender-profile) amount),
      credit-score: (calculate-updated-credit-score 
        (get credit-score sender-profile) 
        (get total-transactions sender-profile)
        amount)
    }))
    
    ;; Update recipient's business profile  
    (map-set business-profiles recipient (merge recipient-profile {
      total-transactions: (+ (get total-transactions recipient-profile) u1),
      transaction-volume: (+ (get transaction-volume recipient-profile) amount),
      credit-score: (calculate-updated-credit-score 
        (get credit-score recipient-profile) 
        (get total-transactions recipient-profile)
        amount)
    }))
    
    ;; Update business relationship
    (map-set business-transactions {sender: tx-sender, recipient: recipient} {
      total-amount: (+ (get total-amount existing-relationship) amount),
      transaction-count: (+ (get transaction-count existing-relationship) u1),
      last-transaction: stacks-block-height,
      relationship-score: (calculate-relationship-score 
        (+ (get transaction-count existing-relationship) u1)
        (+ (get total-amount existing-relationship) amount))
    })
    
    ;; Print memo if provided
    (match memo to-print (print to-print) 0x)
    
    (ok {
      transaction-amount: amount,
      new-sender-score: (calculate-updated-credit-score 
        (get credit-score sender-profile) 
        (get total-transactions sender-profile)
        amount),
      relationship-updated: true
    })))

;; Helper function: Calculate initial credit score based on business volume
(define-private (calculate-initial-credit-score (volume uint))
  (if (<= volume u10000) 
    u300  ;; Low volume businesses start with 300 score
    (if (<= volume u100000)
      u500  ;; Medium volume: 500 score
      u700))) ;; High volume: 700 score

;; Helper function: Calculate updated credit score
(define-private (calculate-updated-credit-score (current-score uint) (transaction-count uint) (amount uint))
  (let ((transaction-factor (/ transaction-count u10))
        (amount-factor (/ amount u1000)))
    (+ current-score transaction-factor amount-factor)))

;; Helper function: Calculate relationship score between businesses
(define-private (calculate-relationship-score (transaction-count uint) (total-amount uint))
  (+ (* transaction-count u5) (/ total-amount u10000)))

;; Read-only functions for data retrieval
(define-read-only (get-business-profile (business principal))
  (map-get? business-profiles business))

(define-read-only (get-credit-limit (business principal))
  (map-get? credit-limits business))

(define-read-only (get-business-relationship (sender principal) (recipient principal))
  (map-get? business-transactions {sender: sender, recipient: recipient}))

(define-read-only (get-credit-balance (business principal))
  (ok (ft-get-balance credit-token business)))

(define-read-only (get-network-stats)
  (ok {
    total-businesses: (var-get total-businesses),
    total-credit-issued: (var-get total-credit-issued)
  }))