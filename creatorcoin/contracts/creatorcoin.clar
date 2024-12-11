;; Define the contract owner
(define-constant contract-owner tx-sender)

;; Define the NFT with a specific name
(define-non-fungible-token creator-nft uint)

;; Define maps
(define-map token-royalties 
  {token-id: uint} 
  {creator: principal, royalty-percentage: uint})

(define-map fractional-ownership 
  {token-id: uint} 
  {total-shares: uint, share-owners: (list 20 principal)})

;; Define constants for error codes
(define-constant ERR_INVALID_RECIPIENT (err u1))
(define-constant ERR_INVALID_SHARES (err u2))
(define-constant ERR_UNAUTHORIZED (err u3))
(define-constant ERR_INSUFFICIENT_SHARES (err u4))
(define-constant ERR_NO_SHARES (err u5))
(define-constant ERR_TOKEN_NOT_FOUND (err u6))
(define-constant ERR_FRACTIONAL_NOT_FOUND (err u7))
(define-constant ERR_INVALID_PRICE (err u8))
(define-constant ERR_INVALID_TOKEN_ID (err u9))

;; Define variables
(define-data-var last-token-id uint u0)
(define-data-var last-sale-price uint u0)

;; Mint NFT function
(define-public (mint-nft 
  (token-uri (string-ascii 256))
  (royalty-percentage uint))
  (let 
    ((token-id (+ (var-get last-token-id) u1))
     (sender tx-sender))
    (asserts! (<= royalty-percentage u100) ERR_INVALID_SHARES)
    
    ;; Additional validation for token-id
    (asserts! (> token-id u0) ERR_INVALID_TOKEN_ID)
    
    (try! (nft-mint? creator-nft token-id sender))
    
    ;; Validate token-id before setting royalties
    (map-set token-royalties 
      {token-id: token-id} 
      {creator: sender, royalty-percentage: royalty-percentage})
    
    (var-set last-token-id token-id)
    (ok token-id)))

;; Transfer NFT function
(define-public (transfer-nft 
  (token-id uint) 
  (recipient principal))
  (let ((current-owner (unwrap! (nft-get-owner? creator-nft token-id) ERR_TOKEN_NOT_FOUND))
        (royalty-info (unwrap! 
          (map-get? token-royalties {token-id: token-id}) 
          ERR_TOKEN_NOT_FOUND))
        (sale-price (var-get last-sale-price))
        (royalty-amount (/ (* sale-price (get royalty-percentage royalty-info)) u100)))
    
    (asserts! (not (is-eq recipient tx-sender)) ERR_INVALID_RECIPIENT)
    (asserts! (is-eq tx-sender current-owner) ERR_UNAUTHORIZED)
    
    (if (> royalty-amount u0)
      (try! (stx-transfer? royalty-amount current-owner 
        (get creator royalty-info)))
      true)
    
    (try! (nft-transfer? creator-nft token-id current-owner recipient))
    (ok true)))

;; Create fractional shares function
(define-public (create-fractional-shares 
  (token-id uint)
  (total-shares uint))
  (let ((current-owner (unwrap! (nft-get-owner? creator-nft token-id) ERR_TOKEN_NOT_FOUND)))
    ;; Additional explicit validation for token-id
    (asserts! (> token-id u0) ERR_INVALID_TOKEN_ID)
    
    (asserts! (> total-shares u0) ERR_INVALID_SHARES)
    (asserts! (is-eq tx-sender current-owner) ERR_UNAUTHORIZED)
    
    ;; Validate token-id before setting fractional ownership
    (map-set fractional-ownership 
      {token-id: token-id}
      {total-shares: total-shares, share-owners: (list tx-sender)})
    
    (ok true)))

;; Buy fractional share function
(define-public (buy-fractional-share 
  (token-id uint)
  (share-amount uint))
  (begin
    ;; Explicit check for token-id
    (asserts! (> token-id u0) ERR_INVALID_TOKEN_ID)
    
    (let ((fractional-details (unwrap! 
            (map-get? fractional-ownership {token-id: token-id}) 
            ERR_FRACTIONAL_NOT_FOUND))
          (owner (unwrap! (nft-get-owner? creator-nft token-id) ERR_TOKEN_NOT_FOUND))
          (price-per-share (/ (var-get last-sale-price) 
            (get total-shares fractional-details)))
          (current-owners (get share-owners fractional-details))
          (updated-owners (unwrap! 
            (as-max-len? (concat current-owners (list tx-sender)) u20) 
            ERR_INVALID_SHARES)))
      
      (asserts! (> share-amount u0) ERR_INVALID_SHARES)
      (try! (stx-transfer? (* price-per-share share-amount) 
        tx-sender 
        owner))
      
      (map-set fractional-ownership
        {token-id: token-id}
        {total-shares: (get total-shares fractional-details),
         share-owners: updated-owners})
      
      (ok true))))

;; Get fractional ownership details
(define-read-only (get-fractional-ownership (token-id uint))
  (match (map-get? fractional-ownership {token-id: token-id})
    ownership-data (ok ownership-data)
    (err ERR_FRACTIONAL_NOT_FOUND)))

;; Get token royalties
(define-read-only (get-token-royalties (token-id uint))
  (match (map-get? token-royalties {token-id: token-id})
    royalty-data (ok royalty-data)
    (err ERR_TOKEN_NOT_FOUND)))

;; Set last sale price
(define-public (set-last-sale-price (price uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) ERR_UNAUTHORIZED)
    (asserts! (> price u0) ERR_INVALID_PRICE)
    (var-set last-sale-price price)
    (ok true)))