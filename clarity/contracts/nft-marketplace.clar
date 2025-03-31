(define-trait nft-trait
  ((transfer (principal uint principal))
   (get-owner (uint) (response (optional principal) uint))))

(define-trait ft-trait
  ((transfer (principal uint principal) (response bool uint))
   (get-balance (principal) (response uint uint))))

(define-constant MARKETPLACE-OWNER 'SPXXXXXX)
(define-constant FEE-PERCENTAGE 5) ;; Marketplace commission fee (5%)
(define-constant MIN-BID-INCREASE 10) ;; Minimum bid increment

(define-map auctions
  ((nft-id uint))
  {
    seller: principal,
    highest-bidder: (optional principal),
    highest-bid: uint,
    reserve-price: uint,
    royalty-recipient: principal,
    royalty-fee: uint
  })

(define-map balances ((user principal)) uint)

(define-public (create-auction (nft-id uint) (reserve-price uint) (royalty-recipient principal) (royalty-fee uint))
  (begin
    (asserts! (<= royalty-fee 10) "Royalty fee too high")
    (asserts! (is-none (map-get? auctions {nft-id: nft-id})) "Auction already exists")
    (map-insert auctions {nft-id: nft-id} {
      seller: tx-sender,
      highest-bidder: none,
      highest-bid: 0,
      reserve-price: reserve-price,
      royalty-recipient: royalty-recipient,
      royalty-fee: royalty-fee
    })
    (ok "Auction created")))

(define-public (place-bid (nft-id uint) (bid-amount uint))
  (let ((auction (map-get? auctions {nft-id: nft-id})))
    (match auction
      some {
        seller: seller,
        highest-bidder: highest-bidder,
        highest-bid: highest-bid,
        reserve-price: reserve-price,
        royalty-recipient: royalty-recipient,
        royalty-fee: royalty-fee
      }
      (begin
        (asserts! (> bid-amount (+ highest-bid MIN-BID-INCREASE)) "Bid too low")
        (map-insert balances {user: tx-sender} bid-amount)
        (map-insert auctions {nft-id: nft-id} {
          seller: seller,
          highest-bidder: (some tx-sender),
          highest-bid: bid-amount,
          reserve-price: reserve-price,
          royalty-recipient: royalty-recipient,
          royalty-fee: royalty-fee
        })
        (ok "Bid placed")))
      (err "Auction not found"))))

(define-public (finalize-auction (nft-id uint))
  (let ((auction (map-get? auctions {nft-id: nft-id})))
    (match auction
      some {
        seller: seller,
        highest-bidder: highest-bidder,
        highest-bid: highest-bid,
        reserve-price: reserve-price,
        royalty-recipient: royalty-recipient,
        royalty-fee: royalty-fee
      }
      (begin
        (asserts! (is-some highest-bidder) "No bids placed")
        (let ((fee (/ (* highest-bid FEE-PERCENTAGE) 100))
              (royalty (/ (* highest-bid royalty-fee) 100))
              (seller-amount (- highest-bid (+ fee royalty))))
          (map-insert balances {user: MARKETPLACE-OWNER} fee)
          (map-insert balances {user: royalty-recipient} royalty)
          (map-insert balances {user: seller} seller-amount)
          (map-delete auctions {nft-id: nft-id})
          (ok "Auction finalized"))))
      (err "Auction not found"))))
