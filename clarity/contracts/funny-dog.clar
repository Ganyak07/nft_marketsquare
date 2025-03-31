;; Implement the SIP-009 Non-Fungible Token Trait
(impl-trait 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait.nft-trait)

;; Define the NFT's name
(define-non-fungible-token funny-dog uint)

;; Keep track of the last minted token ID
(define-data-var last-token-id uint u0)

;; Define constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant COLLECTION_LIMIT u10000000)
(define-constant ERR_OWNER_ONLY (err u100))
(define-constant ERR_NOT_TOKEN_OWNER (err u101))
(define-constant ERR_SOLD_OUT (err u300))
(define-constant ERR_NOT_FOR_SALE (err u400))
(define-constant ERR_INSUFFICIENT_FUNDS (err u401))

;; Store NFT metadata
(define-map token-metadata uint { name: (string-ascii 32), description: (string-ascii 128), image-uri: (string-ascii 256) })

;; Store NFT listings
(define-map nft-listings uint { seller: principal, price: uint })

;; SIP-009 function: Get the last minted token ID.
(define-read-only (get-last-token-id)
  (ok (var-get last-token-id))
)

;; SIP-009 function: Get NFT metadata
(define-read-only (get-token-metadata (token-id uint))
  (ok (map-get? token-metadata token-id))
)

;; List an NFT for sale
(define-public (list-nft (token-id uint) (price uint))
  (begin
    (asserts! (is-eq tx-sender (unwrap! (nft-get-owner? funny-dog token-id) ERR_NOT_TOKEN_OWNER)) ERR_NOT_TOKEN_OWNER)
    (map-set nft-listings token-id { seller: tx-sender, price: price })
    (ok token-id)
  )
)

;; Purchase an NFT
(define-public (purchase-nft (token-id uint))
  (let ((listing (unwrap! (map-get? nft-listings token-id) ERR_NOT_FOR_SALE)))
    (begin
      (asserts! (>= (stx-get-balance tx-sender) listing.price) ERR_INSUFFICIENT_FUNDS)
      (try! (stx-transfer? listing.price tx-sender listing.seller))
      (try! (nft-transfer? funny-dog token-id listing.seller tx-sender))
      (map-delete nft-listings token-id)
      (ok token-id)
    )
  )
)

;; Mint a new NFT with metadata
(define-public (mint (recipient principal) (name (string-ascii 32)) (description (string-ascii 128)) (image-uri (string-ascii 256)))
  (let ((token-id (+ (var-get last-token-id) u1)))
    (asserts! (< (var-get last-token-id) COLLECTION_LIMIT) ERR_SOLD_OUT)
    (try! (nft-mint? funny-dog token-id recipient))
    (map-set token-metadata token-id { name: name, description: description, image-uri: image-uri })
    (var-set last-token-id token-id)
    (ok token-id)
  )
)
