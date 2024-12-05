(define-non-fungible-token creator-coin-nft uint)

(define-map token-royalties 
  {token-id: uint} 
  {creator: principal, royalty-percentage: uint})

(define-map fractional-ownership 
  {token-id: uint} 
  {total-shares: uint, share-owners: (list 10 principal)})

(define-constant err-invalid-recipient (err u1))
(define-constant err-invalid-shares (err u2))
(define-constant err-unauthorized (err u3))

(define-public (mint-nft 
  (token-uri (string-ascii 256))
  (royalty-percentage uint))
  (let ((token-id (+ (var-get last-token-id) u1))
        (sender tx-sender))
    (asserts! (<= royalty-percentage u100) (err u1))
    (try! (nft-mint? creator-coin-nft token-id sender))
    (map-set token-royalties 
      {token-id: token-id} 
      {creator: sender, royalty-percentage: royalty-percentage})
    (var-set last-token-id token-id)
    (ok token-id)))

(define-public (transfer-nft 
  (token-id uint) 
  (recipient principal))
  (begin
    (asserts! (not (is-eq recipient tx-sender)) err-invalid-recipient)
    (let ((current-owner (unwrap! (nft-get-owner? creator-coin-nft token-id) (err u2)))
          (royalty-info (unwrap! 
            (map-get? token-royalties {token-id: token-id}) 
            (err u3)))
          (sale-price (var-get last-sale-price))
          (royalty-amount (/ (* sale-price (get royalty-percentage royalty-info)) u100)))
      
      (asserts! (is-eq tx-sender current-owner) err-unauthorized)
      
      (if (> royalty-amount u0)
        (try! (stx-transfer? royalty-amount current-owner 
          (get creator royalty-info)))
        true)
      
      (try! (nft-transfer? creator-coin-nft token-id current-owner recipient))
      (ok true))))

(define-public (create-fractional-shares 
  (token-id uint)
  (total-shares uint))
  (begin
    (asserts! (> total-shares u0) err-invalid-shares)
    (let ((current-owner (unwrap! (nft-get-owner? creator-coin-nft token-id) (err u5))))
      (asserts! (is-eq tx-sender current-owner) err-unauthorized)
      (map-set fractional-ownership 
        {token-id: token-id}
        {total-shares: total-shares, share-owners: (list)})
      (ok true))))

(define-public (buy-fractional-share 
  (token-id uint)
  (share-amount uint))
  (begin
    (asserts! (> share-amount u0) err-invalid-shares)
    (let ((fractional-details (unwrap! 
            (map-get? fractional-ownership {token-id: token-id}) 
            (err u7)))
          (owner (unwrap! (nft-get-owner? creator-coin-nft token-id) (err u8)))
          (price-per-share (/ (var-get last-sale-price) 
            (get total-shares fractional-details))))
      
      (try! (stx-transfer? (* price-per-share share-amount) 
        tx-sender 
        owner))
      
      (ok true))))

(define-data-var last-token-id uint u0)
(define-data-var last-sale-price uint u0)