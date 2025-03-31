;; An enhanced NFT marketplace that allows users to list NFTs, purchase NFTs,
;; and ensures payment security, including transaction fees, escrow, and listing expiry.

(use-trait nft-trait 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait.nft-trait)
(use-trait ft-trait  'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.sip-010-trait-ft-standard.sip-010-trait)

(define-constant contract-owner tx-sender)

;; Transaction fee constants
(define-constant TRANSACTION_FEE_PERCENTAGE u2) ;; 2% fee
(define-constant TRANSACTION_FEE_FLAT u1000 ;; Flat fee of 1000 STX

;; Listing errors
(define-constant ERR_EXPIRY_IN_PAST (err u1000))
(define-constant ERR_PRICE_ZERO (err u1001))
(define-constant ERR_INVALID_PAYMENT (err u1002))
(define-constant ERR_LISTING_EXPIRED (err u1003))

;; Cancel and fulfill errors
(define-constant ERR_UNKNOWN_LISTING (err u2000))
(define-constant ERR_UNAUTHORISED (err u2001))
(define-constant ERR_LISTING_EXPIRED (err u2002))
(define-constant ERR_NFT_ASSET_MISMATCH (err u2003))

;; Define the asset listings map
(define-map listings
  uint
  {
    maker: principal,
    taker: (optional principal),
    token-id: uint,
    nft-asset-contract: principal,
    expiry: uint,
    price: uint,
    payment-asset-contract: (optional principal),
    escrow: bool, ;; Added for payment holding
  }
)

;; Used for unique IDs for each listing
(define-data-var listing-nonce uint u0)

;; Function to set transaction fees (only contract owner can set)
(define-public (set-transaction-fee (fee-type uint) (fee-amount uint))
  (begin
    (asserts! (is-eq contract-owner tx-sender) ERR_UNAUTHORISED)
    (match fee-type
      u0 (var-set TRANSACTION_FEE_PERCENTAGE fee-amount)
      u1 (var-set TRANSACTION_FEE_FLAT fee-amount)
      (err u9999) ;; Invalid fee type
    )
    (ok true)
  )
)

;; Internal function for escrow management
(define-private (transfer-escrow
  (token-contract <ft-trait>)
  (amount uint)
  (sender principal)
  (recipient principal)
)
  (contract-call? token-contract transfer amount sender recipient none)
)

;; Public function to list an asset for sale
(define-public (list-asset
  (nft-asset-contract <nft-trait>)
  (nft-asset {
    taker: (optional principal),
    token-id: uint,
    expiry: uint,
    price: uint,
    payment-asset-contract: (optional principal)
  })
)
  (let ((listing-id (var-get listing-nonce)))
    ;; Validate price is greater than zero
    (asserts! (> (get price nft-asset) u0) ERR_PRICE_ZERO)
    
    ;; Verify asset contract is whitelisted
    (asserts! (is-whitelisted (contract-of nft-asset-contract)) ERR_ASSET_CONTRACT_NOT_WHITELISTED)
    
    ;; Verify the expiry date is not in the past
    (asserts! (> (get expiry nft-asset) burn-block-height) ERR_EXPIRY_IN_PAST)
    
    ;; Transfer NFT to the marketplace contract
    (try! (transfer-nft nft-asset-contract (get token-id nft-asset) tx-sender (as-contract tx-sender)))
    
    ;; Add the listing to the map with escrow set to true
    (map-set listings listing-id {
      maker: tx-sender,
      taker: (get taker nft-asset),
      token-id: (get token-id nft-asset),
      nft-asset-contract: (contract-of nft-asset-contract),
      expiry: (get expiry nft-asset),
      price: (get price nft-asset),
      payment-asset-contract: (get payment-asset-contract nft-asset),
      escrow: true
    })
    
    ;; Increment the nonce for the next listing
    (var-set listing-nonce (+ listing-id u1))
    (ok listing-id)
  )
)

;; Function to fulfill a listing with STX as payment
(define-public (fulfil-listing-stx (listing-id uint) (nft-asset-contract <nft-trait>))
  (let (
    (listing (unwrap! (map-get? listings listing-id) ERR_UNKNOWN_LISTING))
    (taker tx-sender)
  )
    ;; Ensure the listing is not expired
    (asserts! (< burn-block-height (get expiry listing)) ERR_LISTING_EXPIRED)
    
    ;; Validate that the taker is not the maker of the listing
    (asserts! (not (is-eq (get maker listing) taker)) ERR_UNAUTHORISED)
    
    ;; Deduct transaction fee and transfer the remaining amount to the maker
    (let ((fee (if (is-eq TRANSACTION_FEE_PERCENTAGE u0) 
                   TRANSACTION_FEE_FLAT 
                   (mul (get price listing) TRANSACTION_FEE_PERCENTAGE u100))))
      ;; Transfer payment minus fee to maker
      (stx-transfer? (- (get price listing) fee) taker (get maker listing))
      
      ;; Transfer the fee to the contract owner
      (stx-transfer? fee taker contract-owner)
      
      ;; Transfer the NFT to the taker
      (try! (as-contract (transfer-nft nft-asset-contract (get token-id listing) taker taker)))
      
      ;; Remove the listing from marketplace
      (map-delete listings listing-id)
      (ok listing-id)
  )
)

;; Function to fulfill a listing with a fungible token as payment
(define-public (fulfil-listing-ft
  (listing-id uint)
  (nft-asset-contract <nft-trait>)
  (payment-asset-contract <ft-trait>)
)
  (let (
    (listing (unwrap! (map-get? listings listing-id) ERR_UNKNOWN_LISTING))
    (taker tx-sender)
  )
    ;; Ensure the listing is not expired
    (asserts! (< burn-block-height (get expiry listing)) ERR_LISTING_EXPIRED)
    
    ;; Validate that the taker is not the maker of the listing
    (asserts! (not (is-eq (get maker listing) taker)) ERR_UNAUTHORISED)
    
    ;; Deduct transaction fee and transfer the remaining amount to the maker
    (let ((fee (if (is-eq TRANSACTION_FEE_PERCENTAGE u0) 
                   TRANSACTION_FEE_FLAT 
                   (mul (get price listing) TRANSACTION_FEE_PERCENTAGE u100))))
      ;; Transfer payment minus fee to maker
      (try! (transfer-ft payment-asset-contract (- (get price listing) fee) taker (get maker listing)))
      
      ;; Transfer the fee to the contract owner
      (try! (transfer-ft payment-asset-contract fee taker contract-owner))
      
      ;; Transfer the NFT to the taker
      (try! (as-contract (transfer-nft nft-asset-contract (get token-id listing) taker taker)))
      
      ;; Remove the listing from marketplace
      (map-delete listings listing-id)
      (ok listing-id)
  )
)

;; Function to cancel a listing
(define-public (cancel-listing (listing-id uint) (nft-asset-contract <nft-trait>))
  (let (
    (listing (unwrap! (map-get? listings listing-id) ERR_UNKNOWN_LISTING))
    (maker (get maker listing))
  )
    ;; Only the maker can cancel their listing
    (asserts! (is-eq maker tx-sender) ERR_UNAUTHORISED)
    
    ;; Transfer the NFT back to the maker
    (try! (as-contract (transfer-nft nft-asset-contract (get token-id listing) tx-sender maker)))
    
    ;; Remove the listing
    (map-delete listings listing-id)
    (ok true)
  )
)
