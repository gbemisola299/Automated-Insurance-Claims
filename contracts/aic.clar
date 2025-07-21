;; Automated Insurance Claims
;; A parametric insurance contract that automatically processes claims based on predefined conditions


;; Constants for errors
(define-constant ERR-NOT-AUTHORIZED u1)
(define-constant ERR-POLICY-NOT-FOUND u2)
(define-constant ERR-POLICY-EXPIRED u3)
(define-constant ERR-POLICY-NOT-ACTIVE u4)
(define-constant ERR-INSUFFICIENT-PAYMENT u5)
(define-constant ERR-INVALID-RISK-PROFILE u6)
(define-constant ERR-INVALID-COVERAGE-AMOUNT u7)
(define-constant ERR-ALREADY-CLAIMED u8)
(define-constant ERR-CLAIM-NOT-FOUND u9)
(define-constant ERR-INVALID-ORACLE-DATA u10)
(define-constant ERR-CLAIM-CONDITION-NOT-MET u11)
(define-constant ERR-ORACLE-NOT-REGISTERED u12)
(define-constant ERR-NO-ORACLE-DATA u13)
(define-constant ERR-INVALID-PARAMETERS u14)
(define-constant ERR-NOT-CLAIMABLE-YET u15)
(define-constant ERR-PAYMENT-FAILED u16)
(define-constant ERR-POLICY-NOT-EXPIRED u17)


;; Constants for policy status
(define-constant POLICY-STATUS-ACTIVE u1)
(define-constant POLICY-STATUS-EXPIRED u2)
(define-constant POLICY-STATUS-CANCELED u3)
(define-constant POLICY-STATUS-CLAIMED u4)


;; Constants for claim status
(define-constant CLAIM-STATUS-PENDING u1)
(define-constant CLAIM-STATUS-APPROVED u2)
(define-constant CLAIM-STATUS-REJECTED u3)
(define-constant CLAIM-STATUS-PAID u4)


;; Constants for weather event types
(define-constant WEATHER-RAINFALL u1)
(define-constant WEATHER-TEMPERATURE u2)
(define-constant WEATHER-WIND-SPEED u3)
(define-constant WEATHER-HUMIDITY u4)
(define-constant WEATHER-HURRICANE u5)
(define-constant WEATHER-FLOOD u6)
(define-constant WEATHER-DROUGHT u7)


;; Constants for condition operators
(define-constant OPERATOR-GREATER-THAN u1)
(define-constant OPERATOR-LESS-THAN u2)
(define-constant OPERATOR-EQUAL-TO u3)
(define-constant OPERATOR-GREATER-THAN-OR-EQUAL u4)
(define-constant OPERATOR-LESS-THAN-OR-EQUAL u5)


;; Data variables
(define-data-var contract-owner principal tx-sender)
(define-data-var next-policy-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var treasury-balance uint u0)
(define-data-var total-premiums-collected uint u0)
(define-data-var total-claims-paid uint u0)


;; Maps for oracles
(define-map oracle-registry
 { oracle-id: (string-ascii 36) }
 {
   oracle-principal: principal,
   oracle-name: (string-utf8 100),
   oracle-type: uint,
   is-active: bool,
   registration-block: uint
 }
)


;; Maps for oracle data
(define-map oracle-data
 { oracle-id: (string-ascii 36), block-height: uint }
 {
   weather-type: uint,
   location: (string-utf8 100),
   value: uint,
   timestamp: uint
 }
)


;; Maps for risk profiles
(define-map risk-profiles
 { profile-id: uint }
 {
   profile-name: (string-utf8 100),
   base-premium-rate: uint,  ;; basis points (1/100 of 1%)
   coverage-multiplier: uint, ;; multiplier for coverage calculation
   risk-factor: uint,  ;; additional risk factor (basis points)
   min-coverage: uint,
   max-coverage: uint,
   description: (string-utf8 500)
 }
)


;; Maps for policies
(define-map policies
 { policy-id: uint }
 {
   policyholder: principal,
   risk-profile-id: uint,
   coverage-amount: uint,
   premium-amount: uint,
   start-block: uint,
   end-block: uint,
   policy-status: uint,
   renewal-count: uint,
   auto-renew: bool,
   location: (string-utf8 100),
   created-at: uint,
   last-updated: uint
 }
)

;; Maps for policy conditions (claim triggers)
(define-map policy-conditions
 { policy-id: uint, condition-index: uint }
 {
   weather-type: uint,
   operator: uint,
   threshold-value: uint,
   payout-percentage: uint, ;; percentage of coverage to pay out (basis points)
   oracle-id: (string-ascii 36)
 }
)


;; Maps for claims
(define-map claims
 { claim-id: uint }
 {
   policy-id: uint,
   claimant: principal,
   claim-status: uint,
   claim-amount: uint,
   weather-event-type: uint,
   weather-event-value: uint,
   condition-index: uint,
   submitted-block: uint,
   processed-block: (optional uint),
   paid-block: (optional uint),
   oracle-data-block: uint
 }
)


;; Maps for claim history by policy
(define-map policy-claims
 { policy-id: uint, claim-index: uint }
 { claim-id: uint }
)


;; Maps for policy count by user
(define-map user-policy-count
 { user: principal }
 { count: uint }
)


;; Maps for policy indices by user
(define-map user-policies
 { user: principal, index: uint }
 { policy-id: uint }
)

;; Read-only functions


;; Get policy details
(define-read-only (get-policy (policy-id uint))
 (map-get? policies { policy-id: policy-id })
)


;; Get claim details
(define-read-only (get-claim (claim-id uint))
 (map-get? claims { claim-id: claim-id })
)


;; Get risk profile details
(define-read-only (get-risk-profile (profile-id uint))
 (map-get? risk-profiles { profile-id: profile-id })
)


;; Get oracle details
(define-read-only (get-oracle (oracle-id (string-ascii 36)))
 (map-get? oracle-registry { oracle-id: oracle-id })
)


;; Get oracle data
(define-read-only (get-oracle-data (oracle-id (string-ascii 36)) (block-height uint))
 (map-get? oracle-data { oracle-id: oracle-id, block-height: block-height })
)


;; Get latest oracle data
(define-read-only (get-latest-oracle-data (oracle-id (string-ascii 36)))
 (get-oracle-data oracle-id stacks-block-height)
)

;; Calculate premium for a given risk profile and coverage amount
(define-read-only (calculate-premium (profile-id uint) (coverage-amount uint) (location (string-utf8 100)))
 (match (get-risk-profile profile-id)
   profile
   (let
     (
       (base-rate (get base-premium-rate profile))
       (risk-factor (get risk-factor profile))
       ;; In a real contract, location might affect premium calculation
       ;; For simplicity, we're ignoring location in this implementation
       (premium (/ (* coverage-amount (+ base-rate risk-factor)) u10000))
     )
     (ok premium)
   )
   (err ERR-INVALID-RISK-PROFILE)
 )
)


;; Check if a policy is active
(define-read-only (is-policy-active (policy-id uint))
 (match (get-policy policy-id)
   policy
   (and
     (is-eq (get policy-status policy) POLICY-STATUS-ACTIVE)
     (>= stacks-block-height (get start-block policy))
     (<= stacks-block-height (get end-block policy))
   )
   false
 )
)


;; Check if a policy is claimable based on oracle data
(define-read-only (is-policy-claimable (policy-id uint))
 (match (get-policy policy-id)
   policy
   (if (is-policy-active policy-id)
     (some-condition-met policy-id)
     false
   )
   false
 )
)
;; Check if any condition for a policy is met
(define-read-only (some-condition-met (policy-id uint))
 ;; In a real implementation, this would check all conditions for the policy
 ;; For simplicity, we just check condition at index 0
 (match (map-get? policy-conditions { policy-id: policy-id, condition-index: u0 })
   condition
   (let
     (
       (oracle-id (get oracle-id condition))
       (weather-type (get weather-type condition))
       (operator (get operator condition))
       (threshold (get threshold-value condition))
     )
     (match (get-latest-oracle-data oracle-id)
       oracle-data-value
       (if (is-eq (get weather-type oracle-data-value) weather-type)
         (let
           (
             (current-value (get value oracle-data-value))
           )
           (condition-check operator current-value threshold)
         )
         false
       )
       false
     )
   )
   false
 )
)




;; Helper function to check condition based on operator
(define-read-only (condition-check (operator uint) (current-value uint) (threshold uint))
 (if (is-eq operator OPERATOR-GREATER-THAN)
   (> current-value threshold)
   (if (is-eq operator OPERATOR-LESS-THAN)
     (< current-value threshold)
     (if (is-eq operator OPERATOR-EQUAL-TO)
       (is-eq current-value threshold)
       (if (is-eq operator OPERATOR-GREATER-THAN-OR-EQUAL)
         (>= current-value threshold)
         (if (is-eq operator OPERATOR-LESS-THAN-OR-EQUAL)
           (<= current-value threshold)
           false
         )
       )
     )
   )
 )
)


;; Get all claims for a policy
(define-read-only (get-policy-claims (policy-id uint))
 ;; In a real implementation, this would return a list of all claims
 ;; For simplicity, we just check if there's a claim at index 0
 (map-get? policy-claims { policy-id: policy-id, claim-index: u0 })
)


;; Get treasury statistics
(define-read-only (get-treasury-stats)
 {
   balance: (var-get treasury-balance),
   total-premiums: (var-get total-premiums-collected),
   total-claims-paid: (var-get total-claims-paid)
 }
)

;; Public functions


;; Register a new oracle
(define-public (register-oracle
 (oracle-id (string-ascii 36))
 (oracle-name (string-utf8 100))
 (oracle-type uint)
)
 (begin
   ;; Only contract owner can register oracles
   (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-NOT-AUTHORIZED))
  
   (map-set oracle-registry
     { oracle-id: oracle-id }
     {
       oracle-principal: tx-sender,
       oracle-name: oracle-name,
       oracle-type: oracle-type,
       is-active: true,
       registration-block: stacks-block-height
     }
   )
  
   (ok true)
 )
)


;; Submit oracle data
(define-public (submit-oracle-data
 (oracle-id (string-ascii 36))
 (weather-type uint)
 (location (string-utf8 100))
 (value uint)
 (timestamp uint)
)
 (let
   (
     (oracle (unwrap! (get-oracle oracle-id) (err ERR-ORACLE-NOT-REGISTERED)))
   )
  
 ;; Only the registered oracle principal can submit data
   (asserts! (is-eq tx-sender (get oracle-principal oracle)) (err ERR-NOT-AUTHORIZED))
  
   ;; Ensure oracle is active
   (asserts! (get is-active oracle) (err ERR-ORACLE-NOT-REGISTERED))
  
   ;; Store oracle data
   (map-set oracle-data
     { oracle-id: oracle-id, block-height: stacks-block-height }
     {
       weather-type: weather-type,
       location: location,
       value: value,
       timestamp: timestamp
     }
   )
  
   (ok true)
 )
)


;; Create a new insurance policy
(define-public (create-policy
 (risk-profile-id uint)
 (coverage-amount uint)
 (duration-blocks uint)
 (auto-renew bool)
 (location (string-utf8 100))
)
 (let
   (
     (policy-id (var-get next-policy-id))
     (risk-profile (unwrap! (get-risk-profile risk-profile-id) (err ERR-INVALID-RISK-PROFILE)))
     (premium-result (unwrap! (calculate-premium risk-profile-id coverage-amount location) (err ERR-INVALID-PARAMETERS)))
   )
  
   ;; Validate coverage amount
   (asserts! (and
              (>= coverage-amount (get min-coverage risk-profile))
             (<= coverage-amount (get max-coverage risk-profile))
             )
             (err ERR-INVALID-COVERAGE-AMOUNT))
  
   ;; Collect premium payment
   (try! (stx-transfer? premium-result tx-sender (as-contract tx-sender)))
  
   ;; Update treasury
   (var-set treasury-balance (+ (var-get treasury-balance) premium-result))
   (var-set total-premiums-collected (+ (var-get total-premiums-collected) premium-result))
  
   ;; Create policy
   (map-set policies
     { policy-id: policy-id }
     {
       policyholder: tx-sender,
       risk-profile-id: risk-profile-id,
       coverage-amount: coverage-amount,
       premium-amount: premium-result,
       start-block: stacks-block-height,
       end-block: (+ stacks-block-height duration-blocks),
       policy-status: POLICY-STATUS-ACTIVE,
       renewal-count: u0,
       auto-renew: auto-renew,
       location: location,
       created-at: stacks-block-height,
       last-updated: stacks-block-height
     }
   )
  
   ;; Update user policy tracking
   (match (map-get? user-policy-count { user: tx-sender })
     existing-count
     (let
       (
         (new-count (+ (get count existing-count) u1))
       )
       (map-set user-policy-count


          { user: tx-sender }
         { count: new-count }
       )
       (map-set user-policies
         { user: tx-sender, index: (- new-count u1) }
         { policy-id: policy-id }
       )
     )
     (begin
       (map-set user-policy-count
         { user: tx-sender }
         { count: u1 }
       )
       (map-set user-policies
         { user: tx-sender, index: u0 }
         { policy-id: policy-id }
       )
     )
   )
  
   ;; Increment policy ID counter
   (var-set next-policy-id (+ policy-id u1))
  
   (ok policy-id)
 )
)
  
;; Add a condition to policy
(define-public (add-policy-condition
 (policy-id uint)
 (weather-type uint)
 (operator uint)
 (threshold-value uint)
 (payout-percentage uint)
 (oracle-id (string-ascii 36))
)
 (let
   (
     (policy (unwrap! (get-policy policy-id) (err ERR-POLICY-NOT-FOUND)))
     (condition-index u0) ;; For simplicity, we only allow one condition per policy
   )
  
   ;; Check if caller is policy holder
   (asserts! (is-eq tx-sender (get policyholder policy)) (err ERR-NOT-AUTHORIZED))
  
   ;; Check if policy is active
   (asserts! (is-eq (get policy-status policy) POLICY-STATUS-ACTIVE) (err ERR-POLICY-NOT-ACTIVE))
  
   ;; Check if oracle exists
   (asserts! (is-some (get-oracle oracle-id)) (err ERR-ORACLE-NOT-REGISTERED))
  
   ;; Validate payout percentage (max 100%)
   (asserts! (<= payout-percentage u10000) (err ERR-INVALID-PARAMETERS))
  
   ;; Add condition
   (map-set policy-conditions
     { policy-id: policy-id, condition-index: condition-index }
     {
       weather-type: weather-type,
       operator: operator,
       threshold-value: threshold-value,
       payout-percentage: payout-percentage,
  oracle-id: oracle-id
     }
   )
  
   (ok true)
 )
)

;; Submit a claim
(define-public (submit-claim (policy-id uint))
 (let
   (
     (policy (unwrap! (get-policy policy-id) (err ERR-POLICY-NOT-FOUND)))
     (claim-id (var-get next-claim-id))
     (claim-index u0) ;; For simplicity, we only allow one claim per policy
   )
  
   ;; Check if caller is policy holder
   (asserts! (is-eq tx-sender (get policyholder policy)) (err ERR-NOT-AUTHORIZED))
  
   ;; Check if policy is active
   (asserts! (is-policy-active policy-id) (err ERR-POLICY-NOT-ACTIVE))
  
   ;; Check if policy hasn't been claimed yet
   (asserts! (not (is-eq (get policy-status policy) POLICY-STATUS-CLAIMED)) (err ERR-ALREADY-CLAIMED))
  
   ;; Check if policy is claimable (condition met)
   (asserts! (is-policy-claimable policy-id) (err ERR-CLAIM-CONDITION-NOT-MET))
  
   ;; Process the claim
   (let
     (
       (condition (unwrap! (map-get? policy-conditions { policy-id: policy-id, condition-index: u0 }) (err ERR-INVALID-PARAMETERS)))
       (oracle-data-value (unwrap! (get-latest-oracle-data (get oracle-id condition)) (err ERR-NO-ORACLE-DATA)))
       (payout-percentage (get payout-percentage condition))

            (claim-amount (/ (* (get coverage-amount policy) payout-percentage) u10000))
     )
    
;; Create claim record
     (map-set claims
       { claim-id: claim-id }
       {
         policy-id: policy-id,
         claimant: tx-sender,
         claim-status: CLAIM-STATUS-APPROVED, ;; Auto-approved for parametric insurance
         claim-amount: claim-amount,
         weather-event-type: (get weather-type condition),
         weather-event-value: (get value oracle-data-value),
         condition-index: u0,
         submitted-block: stacks-block-height,
         processed-block: (some stacks-block-height),
         paid-block: none,
         oracle-data-block: stacks-block-height
       }
     )
    
     ;; Link claim to policy
     (map-set policy-claims
       { policy-id: policy-id, claim-index: claim-index }
       { claim-id: claim-id }
     )
    
     ;; Update policy status
     (map-set policies
       { policy-id: policy-id }
       (merge policy {
         policy-status: POLICY-STATUS-CLAIMED,
last-updated: stacks-block-height
       })
     )
    
     ;; Increment claim ID counter
     (var-set next-claim-id (+ claim-id u1))
    
     (ok claim-id)
   )
 )
)


;; Process claim payment
(define-public (process-claim-payment (claim-id uint))
 (let
   (
     (claim (unwrap! (get-claim claim-id) (err ERR-CLAIM-NOT-FOUND)))
     (policy (unwrap! (get-policy (get policy-id claim)) (err ERR-POLICY-NOT-FOUND)))
   )
  
   ;; Check if claim is approved but not paid
   (asserts! (is-eq (get claim-status claim) CLAIM-STATUS-APPROVED) (err ERR-INVALID-PARAMETERS))
   (asserts! (is-none (get paid-block claim)) (err ERR-ALREADY-CLAIMED))
  
;; Check if treasury has enough balance
   (asserts! (>= (var-get treasury-balance) (get claim-amount claim)) (err ERR-PAYMENT-FAILED))
  
   ;; Process payment
   (try! (as-contract (stx-transfer? (get claim-amount claim) tx-sender (get claimant claim))))
  
   ;; Update treasury
   (var-set treasury-balance (- (var-get treasury-balance) (get claim-amount claim)))
   (var-set total-claims-paid (+ (var-get total-claims-paid) (get claim-amount claim)))
  
   ;; Update claim status
   (map-set claims
     { claim-id: claim-id }
     (merge claim {
       claim-status: CLAIM-STATUS-PAID,
       paid-block: (some stacks-block-height)
     })
   )
  
   (ok true)
 )
)

;; Renew a policy
(define-public (renew-policy (policy-id uint) (duration-blocks uint))
 (let
   (
     (policy (unwrap! (get-policy policy-id) (err ERR-POLICY-NOT-FOUND)))
   )
  
   ;; Check if caller is policy holder
   (asserts! (is-eq tx-sender (get policyholder policy)) (err ERR-NOT-AUTHORIZED))
  
   ;; Check if policy has expired but wasn't claimed
   (asserts! (and
              (> stacks-block-height (get end-block policy))
              (not (is-eq (get policy-status policy) POLICY-STATUS-CLAIMED))
             )
             (err ERR-POLICY-NOT-EXPIRED))
  
   ;; Calculate new premium
   (let
     (
       (premium-result (unwrap! (calculate-premium
                                (get risk-profile-id policy)
                                (get coverage-amount policy)
                                (get location policy))
                              (err ERR-INVALID-PARAMETERS)))
)
    
     ;; Collect premium payment
     (try! (stx-transfer? premium-result tx-sender (as-contract tx-sender)))
    
     ;; Update treasury
     (var-set treasury-balance (+ (var-get treasury-balance) premium-result))
     (var-set total-premiums-collected (+ (var-get total-premiums-collected) premium-result))
    
     ;; Update policy
     (map-set policies
       { policy-id: policy-id }
       (merge policy {
         premium-amount: premium-result,
         start-block: stacks-block-height,
         end-block: (+ stacks-block-height duration-blocks),
         policy-status: POLICY-STATUS-ACTIVE,
         renewal-count: (+ (get renewal-count policy) u1),
         last-updated: stacks-block-height
       })
     )
    
     (ok true)
   )
 )
)
;; Cancel a policy
(define-public (cancel-policy (policy-id uint))
 (let
   (
     (policy (unwrap! (get-policy policy-id) (err ERR-POLICY-NOT-FOUND)))
   )
  
   ;; Check if caller is policy holder
   (asserts! (is-eq tx-sender (get policyholder policy)) (err ERR-NOT-AUTHORIZED))
  
   ;; Check if policy is active
   (asserts! (is-policy-active policy-id) (err ERR-POLICY-NOT-ACTIVE))
  
   ;; Update policy status
   (map-set policies
     { policy-id: policy-id }
     (merge policy {
       policy-status: POLICY-STATUS-CANCELED,
       last-updated: stacks-block-height
     })
   )
  
   ;; Note: In a real implementation, we might refund a portion of the premium
  (ok true)
 )
)


;; Add or update a risk profile (admin only)
(define-public (set-risk-profile
 (profile-id uint)
 (profile-name (string-utf8 100))
 (base-premium-rate uint)
 (coverage-multiplier uint)
 (risk-factor uint)
 (min-coverage uint)
 (max-coverage uint)
 (description (string-utf8 500))
)
 (begin
   ;; Only contract owner can update risk profiles
   (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-NOT-AUTHORIZED))
  
   (map-set risk-profiles
     { profile-id: profile-id }
     {
       profile-name: profile-name,
       base-premium-rate: base-premium-rate,
       coverage-multiplier: coverage-multiplier,


        risk-factor: risk-factor,
       min-coverage: min-coverage,
       max-coverage: max-coverage,
       description: description
     }
   )
  
   (ok true)
 )
)


;; Transfer contract ownership
(define-public (transfer-ownership (new-owner principal))
 (begin
   (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-NOT-AUTHORIZED))
   (var-set contract-owner new-owner)
   (ok true)
 )
)


;; Initialize common risk profiles
(map-set risk-profiles
 { profile-id: u1 }
 {
   profile-name: u"Agricultural Drought Insurance",
  base-premium-rate: u500, ;; 5%
   coverage-multiplier: u1000, ;; 10x
   risk-factor: u300, ;; 3%
   min-coverage: u10000000, ;; 100 STX
   max-coverage: u1000000000, ;; 10,000 STX
   description: u"Insurance for farmers against drought conditions"
 }
)


(map-set risk-profiles
 { profile-id: u2 }
 {
   profile-name: u"Flood Insurance",
   base-premium-rate: u750, ;; 7.5%
   coverage-multiplier: u800, ;; 8x
   risk-factor: u500, ;; 5%
   min-coverage: u20000000, ;; 200 STX
   max-coverage: u2000000000, ;; 20,000 STX
   description: u"Insurance against flood damage"
 }
)


(map-set risk-profiles
 { profile-id: u3 }
 {
   profile-name: u"Hurricane Insurance",
   base-premium-rate: u1000, ;; 10%
   coverage-multiplier: u600, ;; 6x
    risk-factor: u800, ;; 8%
   min-coverage: u50000000, ;; 500 STX
   max-coverage: u5000000000, ;; 50,000 STX
   description: u"Insurance against hurricane damage"
 }
)


(map-set risk-profiles
 { profile-id: u4 }
 {
   profile-name: u"Frost Insurance",
   base-premium-rate: u400, ;; 4%
   coverage-multiplier: u1200, ;; 12x
   risk-factor: u200, ;; 2%
   min-coverage: u5000000, ;; 50 STX
   max-coverage: u500000000, ;; 5,000 STX
   description: u"Insurance for farmers against frost damage to crops"
 }
)
