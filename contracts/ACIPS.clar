(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-ALREADY-INSURED (err u102))
(define-constant ERR-NOT-INSURED (err u103))
(define-constant ERR-INVALID-WEATHER-DATA (err u104))
(define-constant ERR-PAYOUT-FAILED (err u105))

(define-constant PREMIUM-AMOUNT u1000000)
(define-constant PAYOUT-AMOUNT u3000000)
(define-constant MINIMUM-RAINFALL u500)
(define-constant CONTRACT-OWNER tx-sender)

(define-data-var total-premiums uint u0)
(define-data-var total-payouts uint u0)
(define-data-var oracle-address principal 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)

(define-map insured-farmers 
  principal 
  {
    active: bool,
    premium-paid: uint,
    last-payout: uint,
    region: (string-ascii 32)
  }
)

(define-map weather-data
  (string-ascii 32)
  {
    rainfall: uint,
    temperature: uint,
    timestamp: uint
  }
)

(define-public (purchase-insurance (region (string-ascii 32)))
  (let ((farmer-data (default-to 
    { active: false, premium-paid: u0, last-payout: u0, region: region }
    (map-get? insured-farmers tx-sender))))
    (asserts! (not (get active farmer-data)) ERR-ALREADY-INSURED)
    (try! (stx-transfer? PREMIUM-AMOUNT tx-sender (as-contract tx-sender)))
    (var-set total-premiums (+ (var-get total-premiums) PREMIUM-AMOUNT))
    (ok (map-set insured-farmers tx-sender
      {
        active: true,
        premium-paid: PREMIUM-AMOUNT,
        last-payout: u0,
        region: region
      }))))

(define-public (cancel-insurance)
  (let ((farmer-data (default-to 
    { active: false, premium-paid: u0, last-payout: u0, region: "" }
    (map-get? insured-farmers tx-sender))))
    (asserts! (get active farmer-data) ERR-NOT-INSURED)
    (ok (map-delete insured-farmers tx-sender))))

(define-public (update-weather-data (region (string-ascii 32)) (rainfall uint) (temperature uint))
  (begin
    (asserts! (is-eq tx-sender (var-get oracle-address)) ERR-NOT-AUTHORIZED)
    (ok (map-set weather-data region
      {
        rainfall: rainfall,
        temperature: temperature,
        timestamp: stacks-block-height
      }))))

(define-public (claim-payout)
  (let (
    (farmer-data (unwrap! (map-get? insured-farmers tx-sender) ERR-NOT-INSURED))
    (weather (unwrap! (map-get? weather-data (get region farmer-data)) ERR-INVALID-WEATHER-DATA))
  )
    (asserts! (get active farmer-data) ERR-NOT-INSURED)
    (asserts! (< (get rainfall weather) MINIMUM-RAINFALL) ERR-INVALID-WEATHER-DATA)
    (try! (as-contract (stx-transfer? PAYOUT-AMOUNT (as-contract tx-sender) tx-sender)))
    (var-set total-payouts (+ (var-get total-payouts) PAYOUT-AMOUNT))
    (ok (map-set insured-farmers tx-sender
      (merge farmer-data { last-payout: stacks-block-height })))))

(define-public (change-oracle (new-oracle principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (ok (var-set oracle-address new-oracle))))

(define-read-only (get-farmer-info (farmer principal))
  (ok (map-get? insured-farmers farmer)))

(define-read-only (get-weather-info (region (string-ascii 32)))
  (ok (map-get? weather-data region)))

(define-read-only (get-contract-info)
  (ok {
    total-premiums: (var-get total-premiums),
    total-payouts: (var-get total-payouts),
    oracle: (var-get oracle-address)
  }))


(define-constant BASIC-TIER-PREMIUM u500000)
(define-constant BASIC-TIER-PAYOUT u1500000)
(define-constant PREMIUM-TIER-PREMIUM u2000000) 
(define-constant PREMIUM-TIER-PAYOUT u6000000)

(define-map insurance-tiers
  uint
  {
    premium: uint,
    payout: uint,
    name: (string-ascii 10)
  }
)

(define-public (initialize-tiers)
  (begin
    (map-set insurance-tiers u1 
      {
        premium: BASIC-TIER-PREMIUM,
        payout: BASIC-TIER-PAYOUT,
        name: "BASIC"
      })
    (map-set insurance-tiers u2
      {
        premium: PREMIUM-TIER-PREMIUM,
        payout: PREMIUM-TIER-PAYOUT,
        name: "PREMIUM"
      })
    (ok true)))

(define-public (purchase-tiered-insurance (region (string-ascii 32)) (tier-id uint))
  (let (
    (tier (unwrap! (map-get? insurance-tiers tier-id) ERR-INVALID-AMOUNT))
    (farmer-data (default-to 
      { active: false, premium-paid: u0, last-payout: u0, region: region }
      (map-get? insured-farmers tx-sender))))
    (asserts! (not (get active farmer-data)) ERR-ALREADY-INSURED)
    (try! (stx-transfer? (get premium tier) tx-sender (as-contract tx-sender)))
    (var-set total-premiums (+ (var-get total-premiums) (get premium tier)))
    (ok (map-set insured-farmers tx-sender
      {
        active: true,
        premium-paid: (get premium tier),
        last-payout: u0,
        region: region
      }))))


(define-map risk-factors
  (string-ascii 32)
  {
    risk-score: uint,
    last-updated: uint,
    consecutive-droughts: uint
  }
)

(define-constant RISK-MULTIPLIER u100)
(define-constant BASE-RISK-SCORE u1000)

(define-public (update-risk-factors (region (string-ascii 32)))
  (let (
    (current-weather (unwrap! (map-get? weather-data region) ERR-INVALID-WEATHER-DATA))
    (current-risk (default-to 
      { risk-score: BASE-RISK-SCORE, last-updated: u0, consecutive-droughts: u0 }
      (map-get? risk-factors region)))
    (new-drought-count (if (< (get rainfall current-weather) MINIMUM-RAINFALL)
      (+ (get consecutive-droughts current-risk) u1)
      u0)))
    (ok (map-set risk-factors region
      {
        risk-score: (+ BASE-RISK-SCORE (* new-drought-count RISK-MULTIPLIER)),
        last-updated: stacks-block-height,
        consecutive-droughts: new-drought-count
      }))))

(define-read-only (get-risk-adjusted-payout (region (string-ascii 32)))
  (let (
    (risk-data (unwrap! (map-get? risk-factors region) ERR-INVALID-WEATHER-DATA)))
    (ok (* PAYOUT-AMOUNT (/ (get risk-score risk-data) BASE-RISK-SCORE)))))