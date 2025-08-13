;; Decentralized Gig Work Platform

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-gig-closed (err u103))

(define-constant err-insufficient-funds (err u200))
(define-constant err-funds-locked (err u201))
(define-constant err-already-funded (err u202))
(define-constant err-not-funded (err u203))

(define-map escrow-funds
    { gig-id: uint }
    {
        amount: uint,
        funded: bool,
        locked: bool,
        funder: principal
    }
)

;; Data Maps
(define-map gigs 
    { gig-id: uint }
    {
        owner: principal,
        title: (string-ascii 50),
        description: (string-ascii 500),
        payment: uint,
        worker: (optional principal),
        completed: bool,
        paid: bool
    }
)

(define-map gig-count
    { owner: principal }
    { count: uint }
)

;; Public Functions
(define-public (create-gig (title (string-ascii 50)) (description (string-ascii 500)) (payment uint))
    (let
        (
            (owner tx-sender)
            (current-count (default-to { count: u0 } (map-get? gig-count { owner: owner })))
            (new-gig-id (+ (get count current-count) u1))
        )
        (map-set gigs
            { gig-id: new-gig-id }
            {
                owner: owner,
                title: title,
                description: description,
                payment: payment,
                worker: none,
                completed: false,
                paid: false
            }
        )
        (map-set gig-count { owner: owner } { count: new-gig-id })
        (ok new-gig-id)
    )
)

(define-public (accept-gig (gig-id uint))
    (let
        ((gig (unwrap! (map-get? gigs { gig-id: gig-id }) (err err-not-found))))
        (asserts! (is-none (get worker gig)) (err err-already-exists))
        (map-set gigs
            { gig-id: gig-id }
            (merge gig { worker: (some tx-sender) })
        )
        (ok true)
    )
)

(define-public (complete-gig (gig-id uint))
    (let
        ((gig (unwrap! (map-get? gigs { gig-id: gig-id }) (err err-not-found))))
        (asserts! (is-eq (some tx-sender) (get worker gig)) (err err-owner-only))
        (map-set gigs
            { gig-id: gig-id }
            (merge gig { completed: true })
        )
        (ok true)
    )
)

(define-public (release-payment (gig-id uint))
    (let
        ((gig (unwrap! (map-get? gigs { gig-id: gig-id }) (err u404))))
        (asserts! (is-eq tx-sender (get owner gig)) (err u403))
        (asserts! (get completed gig) (err u400))
        (asserts! (not (get paid gig)) (err u409))
        (try! (stx-transfer? (get payment gig) tx-sender (unwrap! (get worker gig) (err u404))))
        (map-set gigs
            { gig-id: gig-id }
            (merge gig { paid: true })
        )
        (ok true)
    )
)

;; Read-only Functions
(define-read-only (get-gig (gig-id uint))
    (map-get? gigs { gig-id: gig-id })
)

(define-read-only (get-user-gigs (user principal))
    (map-get? gig-count { owner: user })
)


;; Add to Data Maps
(define-map user-ratings
    { user: principal }
    { total-ratings: uint, rating-sum: uint }
)

;; Add Public Function
(define-public (rate-user (user principal) (rating uint))
    (let
        ((current-ratings (default-to { total-ratings: u0, rating-sum: u0 } 
                         (map-get? user-ratings { user: user }))))
        (asserts! (< rating u6) (err u300)) ;; ratings 1-5 only
        (map-set user-ratings
            { user: user }
            { 
                total-ratings: (+ (get total-ratings current-ratings) u1),
                rating-sum: (+ (get rating-sum current-ratings) rating)
            }
        )
        (ok true)
    )
)


;; Add to Data Maps
(define-map disputes
    { gig-id: uint }
    { 
        complainant: principal,
        description: (string-ascii 500),
        resolved: bool
    }
)

;; Add Public Function
(define-public (create-dispute (gig-id uint) (description (string-ascii 500)))
    (let
        ((gig (unwrap! (map-get? gigs { gig-id: gig-id }) (err err-not-found))))
        (map-set disputes
            { gig-id: gig-id }
            {
                complainant: tx-sender,
                description: description,
                resolved: false
            }
        )
        (ok true)
    )
)



;; Add to Data Maps
(define-map milestones
    { gig-id: uint, milestone-id: uint }
    {
        description: (string-ascii 100),
        amount: uint,
        completed: bool,
        paid: bool
    }
)

;; Add Public Function
(define-public (add-milestone (gig-id uint) (description (string-ascii 100)) (amount uint))
    (let
        ((gig (unwrap! (map-get? gigs { gig-id: gig-id }) (err err-not-found))))
        (asserts! (is-eq tx-sender (get owner gig)) (err err-owner-only))
        (map-set milestones
            { gig-id: gig-id, milestone-id: u1 }
            {
                description: description,
                amount: amount,
                completed: false,
                paid: false
            }
        )
        (ok true)
    )
)



;; Add to Data Maps
(define-map gig-categories
    { gig-id: uint }
    { categories: (list 5 (string-ascii 20)) }
)

;; Add Public Function
(define-public (add-categories (gig-id uint) (categories (list 5 (string-ascii 20))))
    (let
        ((gig (unwrap! (map-get? gigs { gig-id: gig-id }) (err err-not-found))))
        (asserts! (is-eq tx-sender (get owner gig)) (err err-owner-only))
        (map-set gig-categories
            { gig-id: gig-id }
            { categories: categories }
        )
        (ok true)
    )
)



;; Add to Data Maps
(define-map worker-portfolios
    { worker: principal }
    {
        completed-gigs: (list 10 uint),
        skills: (list 5 (string-ascii 20)),
        experience: (string-ascii 500)
    }
)

;; Add Public Function
(define-public (update-portfolio (skills (list 5 (string-ascii 20))) (experience (string-ascii 500)))
    (let
        ((current-portfolio (default-to { completed-gigs: (list ), skills: (list ), experience: "" }
                          (map-get? worker-portfolios { worker: tx-sender }))))
        (map-set worker-portfolios
            { worker: tx-sender }
            {
                completed-gigs: (get completed-gigs current-portfolio),
                skills: skills,
                experience: experience
            }
        )
        (ok true)
    )
)


;; Add to Data Maps
(define-map time-logs
    { gig-id: uint, log-id: uint }
    {
        worker: principal,
        hours: uint,
        description: (string-ascii 100),
        date: uint
    }
)

(define-map time-log-count
    { gig-id: uint }
    { count: uint }
)

;; Add Public Function
(define-public (log-time (gig-id uint) (hours uint) (description (string-ascii 100)))
    (let 
        (
            (gig (unwrap! (map-get? gigs { gig-id: gig-id }) (err err-not-found)))
            (current-count (default-to { count: u0 } (map-get? time-log-count { gig-id: gig-id })))
            (new-log-id (+ (get count current-count) u1))
        )
        (asserts! (is-eq (some tx-sender) (get worker gig)) (err err-owner-only))
        (map-set time-logs
            { gig-id: gig-id, log-id: new-log-id }
            {
                worker: tx-sender,
                hours: hours,
                description: description,
                date: stacks-block-height
            }
        )
        (map-set time-log-count { gig-id: gig-id } { count: new-log-id })
        (ok new-log-id)
    )
)


;; Add to Data Maps
(define-map skill-endorsements
    { user: principal, skill: (string-ascii 20) }
    { endorsers: (list 50 principal) }
)

;; Add Public Function
(define-public (endorse-skill (user principal) (skill (string-ascii 20)))
    (let
        ((current-endorsements (default-to { endorsers: (list ) }
                             (map-get? skill-endorsements { user: user, skill: skill }))))
        (asserts! (not (is-eq tx-sender user)) (err u500))
        (map-set skill-endorsements
            { user: user, skill: skill }
            { endorsers: (unwrap! (as-max-len? (append (get endorsers current-endorsements) tx-sender) u50)
                                (err u501)) }
        )
        (ok true)
    )
)


;; Add to Data Maps
(define-map referrals
    { referrer: principal }
    { 
        referred-users: (list 100 principal),
        total-rewards: uint
    }
)

;; Add Constants
(define-constant referral-reward u100) ;; in STX tokens

;; Add Public Function
(define-public (refer-user (new-user principal))
    (let
        ((current-referrals (default-to { referred-users: (list ), total-rewards: u0 }
                          (map-get? referrals { referrer: tx-sender }))))
        (asserts! (not (is-eq tx-sender new-user)) (err u600))
        (try! (stx-transfer? referral-reward contract-owner tx-sender))
        (map-set referrals
            { referrer: tx-sender }
            {
                referred-users: (unwrap! (as-max-len? (append (get referred-users current-referrals) new-user) u100)
                                      (err u601)),
                total-rewards: (+ (get total-rewards current-referrals) referral-reward)
            }
        )
        (ok true)
    )
)


;; Add to Data Maps
(define-map bids
    { gig-id: uint, bidder: principal }
    {
        amount: uint,
        proposal: (string-ascii 500),
        status: (string-ascii 10)
    }
)

;; Add Public Function
(define-public (place-bid (gig-id uint) (amount uint) (proposal (string-ascii 500)))
    (let
        ((gig (unwrap! (map-get? gigs { gig-id: gig-id }) (err err-not-found))))
        (asserts! (is-none (get worker gig)) (err err-gig-closed))
        (map-set bids
            { gig-id: gig-id, bidder: tx-sender }
            {
                amount: amount,
                proposal: proposal,
                status: "pending"
            }
        )
        (ok true)
    )
)


;; Add to Constants
(define-constant cancellation-fee u10) ;; 10% fee

;; Add to Data Maps
(define-map cancelled-gigs
    { gig-id: uint }
    {
        cancellation-reason: (string-ascii 500),
        cancelled-by: principal,
        refund-amount: uint,
        timestamp: uint
    }
)

;; Add Public Function
(define-public (emergency-cancel-gig (gig-id uint) (reason (string-ascii 500)))
    (let
        ((gig (unwrap! (map-get? gigs { gig-id: gig-id }) (err err-not-found)))
         (fee-amount (/ (* (get payment gig) cancellation-fee) u100)))
        (asserts! (or (is-eq tx-sender (get owner gig))
                     (is-eq (some tx-sender) (get worker gig))) 
                 (err err-owner-only))
        (map-set cancelled-gigs
            { gig-id: gig-id }
            {
                cancellation-reason: reason,
                cancelled-by: tx-sender,
                refund-amount: (- (get payment gig) fee-amount),
                timestamp: stacks-block-height
            }
        )
        (ok true)
    )
)

;; Add to Data Maps
(define-map reputation-points
    { user: principal }
    {
        points: uint,
        level: uint,
        badges: (list 10 (string-ascii 20)),
        total-gigs: uint
    }
)

;; Add Public Function
(define-public (award-reputation-points (user principal) (points uint))
    (let
        ((current-rep (default-to { points: u0, level: u1, badges: (list ), total-gigs: u0 }
                                (map-get? reputation-points { user: user }))))
        (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
        (map-set reputation-points
            { user: user }
            {
                points: (+ (get points current-rep) points),
                level: (+ (get level current-rep) 
                         (if (> (+ (get points current-rep) points) (* (get level current-rep) u100))
                             u1
                             u0)),
                badges: (get badges current-rep),
                total-gigs: (get total-gigs current-rep)
            }
        )
        (ok true)
    )
)

;; Add Constants
(define-constant premium-fee-monthly u1000) ;; in STX tokens
(define-constant premium-blocks-monthly u4320) ;; ~30 days in blocks

;; Add to Data Maps
(define-map premium-workers
    { worker: principal }
    {
        premium-until: uint,
        subscription-tier: (string-ascii 10), ;; "basic", "silver", "gold"
        benefits: (list 5 (string-ascii 20)),
        auto-renew: bool
    }
)

;; Add Public Function
(define-public (subscribe-premium (tier (string-ascii 10)) (auto-renew bool))
    (let
        ((fee (if (is-eq tier "gold")
                (* premium-fee-monthly u3)
                (if (is-eq tier "silver")
                   (* premium-fee-monthly u2)
                   premium-fee-monthly)))
         (duration (if (is-eq tier "gold")
                      (* premium-blocks-monthly u3)
                      (if (is-eq tier "silver")
                         (* premium-blocks-monthly u2)
                         premium-blocks-monthly)))
         (benefits (if (is-eq tier "gold")
                      (list "priority" "featured" "discount" "badge" "analytics")
                      (if (is-eq tier "silver")
                         (list "priority" "featured" "discount")
                         (list "priority")))))
        
        ;; Transfer premium fee to contract
        (try! (stx-transfer? fee tx-sender (as-contract tx-sender)))
        
        ;; Set premium status
        (map-set premium-workers
            { worker: tx-sender }
            {
                premium-until: (+ stacks-block-height duration),
                subscription-tier: tier,
                benefits: benefits,
                auto-renew: auto-renew
            }
        )
        
        (ok true)
    )
)

(define-public (cancel-premium-subscription)
    (let
        ((premium-status (unwrap! (map-get? premium-workers { worker: tx-sender }) (err u1100))))
        
        (map-set premium-workers
            { worker: tx-sender }
            (merge premium-status { auto-renew: false })
        )
        
        (ok true)
    )
)

(define-read-only (is-premium-worker (worker principal))
    (let
        ((premium-status (map-get? premium-workers { worker: worker })))
        (if (is-some premium-status)
            (< stacks-block-height (get premium-until (unwrap! premium-status false)))
            false
        )
    )
)



;; Add Constants
(define-constant referral-bonus-percentage u5) ;; 5% bonus
(define-constant max-referrals-per-user u10)

;; Add to Data Maps
(define-map referral-tracking
    { referrer: principal }
    {
        referred-users: (list 10 principal),
        total-earnings: uint,
        active-referrals: uint
    }
)

(define-public (claim-referral-bonus (referred-user principal))
    (let
        ((current-referrals (default-to 
            { referred-users: (list ), total-earnings: u0, active-referrals: u0 }
            (map-get? referral-tracking { referrer: tx-sender }))))
        
        (asserts! (< (get active-referrals current-referrals) max-referrals-per-user) (err u1300))
        
        (map-set referral-tracking
            { referrer: tx-sender }
            {
                referred-users: (unwrap! (as-max-len? 
                    (append (get referred-users current-referrals) referred-user)
                    u10) 
                    (err u1301)),
                total-earnings: (get total-earnings current-referrals),
                active-referrals: (+ (get active-referrals current-referrals) u1)
            }
        )
        (ok true)
    )
)



;; Add Constants
(define-constant early-bird-discount u10) ;; 10% discount
(define-constant rush-hour-premium u20) ;; 20% premium

;; Add to Data Maps
(define-map gig-pricing-rules
    { gig-id: uint }
    {
        base-price: uint,
        early-bird-ends: uint,
        rush-hour-starts: uint,
        current-price: uint
    }
)

(define-public (set-dynamic-pricing 
    (gig-id uint)
    (early-bird-duration uint)
    (rush-hour-start uint))
    
    (let
        ((gig (unwrap! (map-get? gigs { gig-id: gig-id }) (err err-not-found))))
        (asserts! (is-eq tx-sender (get owner gig)) (err err-owner-only))
        
        (map-set gig-pricing-rules
            { gig-id: gig-id }
            {
                base-price: (get payment gig),
                early-bird-ends: (+ stacks-block-height early-bird-duration),
                rush-hour-starts: (+ stacks-block-height rush-hour-start),
                current-price: (get payment gig)
            }
        )
        (ok true)
    )
)


(define-map gig-deadlines
    { gig-id: uint }
    {
        deadline: uint,
        extended-count: uint,
        completed-on-time: bool,
        status: (string-ascii 20)
    }
)

(define-constant max-extensions u3)
(define-constant extension-fee u50)

(define-public (set-gig-deadline (gig-id uint) (blocks uint))
    (let
        ((gig (unwrap! (map-get? gigs { gig-id: gig-id }) (err err-not-found))))
        (asserts! (is-eq tx-sender (get owner gig)) (err err-owner-only))
        (map-set gig-deadlines
            { gig-id: gig-id }
            {
                deadline: (+ stacks-block-height blocks),
                extended-count: u0,
                completed-on-time: false,
                status: "active"
            }
        )
        (ok true)
    )
)

(define-public (extend-deadline (gig-id uint) (additional-blocks uint))
    (let
        ((deadline-data (unwrap! (map-get? gig-deadlines { gig-id: gig-id }) (err err-not-found)))
         (gig (unwrap! (map-get? gigs { gig-id: gig-id }) (err err-not-found))))
        (asserts! (is-eq (some tx-sender) (get worker gig)) (err err-owner-only))
        ;; (asserts! (< (get extended-count deadline-data) max-extensions) (err u700))
        ;; (try! (stx-transfer? extension-fee tx-sender (get owner gig)))
        (map-set gig-deadlines
            { gig-id: gig-id }
            (merge deadline-data {
                deadline: (+ (get deadline deadline-data) additional-blocks),
                extended-count: (+ (get extended-count deadline-data) u1)
            })
        )
        (ok true)
    )
)


(define-map collaborators
    { gig-id: uint, worker: principal }
    {
        role: (string-ascii 50),
        share-percentage: uint,
        status: (string-ascii 20),
        joined-at: uint
    }
)

(define-map collaboration-settings
    { gig-id: uint }
    {
        max-collaborators: uint,
        available-slots: uint,
        total-shares: uint
    }
)

(define-public (initialize-collaboration (gig-id uint) (max-workers uint))
    (let
        ((gig (unwrap! (map-get? gigs { gig-id: gig-id }) (err err-not-found))))
        (asserts! (is-eq tx-sender (get owner gig)) (err err-owner-only))
        (map-set collaboration-settings
            { gig-id: gig-id }
            {
                max-collaborators: max-workers,
                available-slots: max-workers,
                total-shares: u0
            }
        )
        (ok true)
    )
)

(define-public (join-as-collaborator (gig-id uint) (role (string-ascii 50)) (share-percentage uint))
    (let
        ((settings (unwrap! (map-get? collaboration-settings { gig-id: gig-id }) (err err-not-found))))
        ;; (asserts! (> (get available-slots settings) u0) (err u800))
        ;; (asserts! (<= (+ (get total-shares settings) share-percentage) u100) (err u801))
        (map-set collaborators
            { gig-id: gig-id, worker: tx-sender }
            {
                role: role,
                share-percentage: share-percentage,
                status: "active",
                joined-at: stacks-block-height
            }
        )
        (map-set collaboration-settings
            { gig-id: gig-id }
            (merge settings {
                available-slots: (- (get available-slots settings) u1),
                total-shares: (+ (get total-shares settings) share-percentage)
            })
        )
        (ok true)
    )
)



(define-public (fund-escrow (gig-id uint))
    (let
        ((gig (unwrap! (map-get? gigs { gig-id: gig-id }) err-not-found))
         (existing-escrow (map-get? escrow-funds { gig-id: gig-id })))
        (asserts! (is-eq tx-sender (get owner gig)) err-owner-only)
        (asserts! (is-none existing-escrow) err-already-funded)
        (try! (stx-transfer? (get payment gig) tx-sender (as-contract tx-sender)))
        (map-set escrow-funds
            { gig-id: gig-id }
            {
                amount: (get payment gig),
                funded: true,
                locked: true,
                funder: tx-sender
            }
        )
        (ok true)
    )
)

(define-public (release-escrow-payment (gig-id uint))
    (let
        ((gig (unwrap! (map-get? gigs { gig-id: gig-id }) err-not-found))
         (escrow (unwrap! (map-get? escrow-funds { gig-id: gig-id }) err-not-funded)))
        (asserts! (is-eq tx-sender (get owner gig)) err-owner-only)
        (asserts! (get completed gig) (err u400))
        (asserts! (get funded escrow) err-not-funded)
        (asserts! (get locked escrow) err-funds-locked)
        (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (unwrap! (get worker gig) err-not-found))))
        (map-set escrow-funds
            { gig-id: gig-id }
            (merge escrow { locked: false })
        )
        (map-set gigs
            { gig-id: gig-id }
            (merge gig { paid: true })
        )
        (ok true)
    )
)

(define-public (refund-escrow (gig-id uint))
    (let
        ((gig (unwrap! (map-get? gigs { gig-id: gig-id }) err-not-found))
         (escrow (unwrap! (map-get? escrow-funds { gig-id: gig-id }) err-not-funded)))
        (asserts! (is-eq tx-sender (get owner gig)) err-owner-only)
        (asserts! (is-none (get worker gig)) (err u405))
        (asserts! (get funded escrow) err-not-funded)
        (asserts! (get locked escrow) err-funds-locked)
        (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get funder escrow))))
        (map-set escrow-funds
            { gig-id: gig-id }
            (merge escrow { locked: false })
        )
        (ok true)
    )
)

(define-read-only (get-escrow-status (gig-id uint))
    (map-get? escrow-funds { gig-id: gig-id })
)

(define-read-only (is-gig-funded (gig-id uint))
    (match (map-get? escrow-funds { gig-id: gig-id })
        escrow (get funded escrow)
        false
    )
)

;; === SKILL VERIFICATION AND CERTIFICATION SYSTEM ===

;; Constants for skill verification
(define-constant certification-fee u50) ;; Fee to take a skill assessment
(define-constant evaluator-reward u25) ;; Reward for evaluators per assessment
(define-constant passing-score u70) ;; Minimum score to pass (out of 100)
(define-constant max-attempts u3) ;; Maximum attempts per skill challenge

;; Data structures for skill challenges created by evaluators
(define-map skill-challenges
    { challenge-id: uint }
    {
        evaluator: principal,
        skill-name: (string-ascii 30),
        description: (string-ascii 200),
        max-score: uint,
        active: bool,
        created-at: uint,
        difficulty-level: (string-ascii 10), ;; "beginner", "intermediate", "advanced"
        estimated-duration: uint ;; in minutes
    }
)

;; Track individual assessment attempts by workers
(define-map skill-assessments
    { worker: principal, challenge-id: uint, attempt: uint }
    {
        score: uint,
        passed: bool,
        submitted-at: uint,
        evaluated-at: uint,
        evaluator-notes: (string-ascii 300)
    }
)

;; Store verified certifications earned by workers
(define-map worker-certifications
    { worker: principal, skill-name: (string-ascii 30) }
    {
        challenge-id: uint,
        score: uint,
        certified-at: uint,
        evaluator: principal,
        expiry-block: uint,
        certification-level: (string-ascii 10) ;; based on score ranges
    }
)

;; Track worker's attempts per challenge
(define-map assessment-attempts
    { worker: principal, challenge-id: uint }
    { attempts-used: uint }
)

;; Evaluator performance and reputation tracking
(define-map evaluator-stats
    { evaluator: principal }
    {
        challenges-created: uint,
        assessments-conducted: uint,
        average-rating: uint,
        total-earnings: uint,
        certified-evaluator: bool
    }
)

;; Required certifications for specific gigs
(define-map gig-certification-requirements
    { gig-id: uint }
    { required-skills: (list 5 (string-ascii 30)) }
)

;; Global counters
(define-map challenge-counter
    { dummy: uint }
    { count: uint }
)

;; Create a new skill challenge (only for certified evaluators)
(define-public (create-skill-challenge 
    (skill-name (string-ascii 30))
    (description (string-ascii 200))
    (max-score uint)
    (difficulty-level (string-ascii 10))
    (estimated-duration uint))
    (let
        ((current-count (default-to { count: u0 } (map-get? challenge-counter { dummy: u0 })))
         (new-challenge-id (+ (get count current-count) u1))
         (evaluator-info (default-to 
            { challenges-created: u0, assessments-conducted: u0, average-rating: u0, total-earnings: u0, certified-evaluator: false }
            (map-get? evaluator-stats { evaluator: tx-sender }))))
        
        ;; Only certified evaluators or contract owner can create challenges
        (asserts! (or (get certified-evaluator evaluator-info) 
                     (is-eq tx-sender contract-owner)) (err u1400))
        (asserts! (> max-score u0) (err u1401))
        
        ;; Create the challenge
        (map-set skill-challenges
            { challenge-id: new-challenge-id }
            {
                evaluator: tx-sender,
                skill-name: skill-name,
                description: description,
                max-score: max-score,
                active: true,
                created-at: stacks-block-height,
                difficulty-level: difficulty-level,
                estimated-duration: estimated-duration
            }
        )
        
        ;; Update counters
        (map-set challenge-counter { dummy: u0 } { count: new-challenge-id })
        (map-set evaluator-stats
            { evaluator: tx-sender }
            (merge evaluator-info { challenges-created: (+ (get challenges-created evaluator-info) u1) })
        )
        
        (ok new-challenge-id)
    )
)

;; Submit assessment attempt for a skill challenge
(define-public (submit-skill-assessment 
    (challenge-id uint)
    (self-reported-score uint))
    (let
        ((challenge (unwrap! (map-get? skill-challenges { challenge-id: challenge-id }) (err u1402)))
         (attempt-record (default-to { attempts-used: u0 } 
            (map-get? assessment-attempts { worker: tx-sender, challenge-id: challenge-id })))
         (next-attempt (+ (get attempts-used attempt-record) u1)))
        
        ;; Validation checks
        (asserts! (get active challenge) (err u1403))
        (asserts! (<= next-attempt max-attempts) (err u1404))
        (asserts! (<= self-reported-score (get max-score challenge)) (err u1405))
        
        ;; Pay assessment fee
        (try! (stx-transfer? certification-fee tx-sender (get evaluator challenge)))
        
        ;; Record the assessment attempt
        (map-set skill-assessments
            { worker: tx-sender, challenge-id: challenge-id, attempt: next-attempt }
            {
                score: self-reported-score,
                passed: (>= (* self-reported-score u100) (* passing-score (get max-score challenge))),
                submitted-at: stacks-block-height,
                evaluated-at: u0,
                evaluator-notes: ""
            }
        )
        
        ;; Update attempt counter
        (map-set assessment-attempts
            { worker: tx-sender, challenge-id: challenge-id }
            { attempts-used: next-attempt }
        )
        
        (ok next-attempt)
    )
)

;; Evaluator confirms and finalizes assessment score
(define-public (evaluate-assessment 
    (worker principal)
    (challenge-id uint)
    (attempt uint)
    (final-score uint)
    (evaluator-notes (string-ascii 300)))
    (let
        ((challenge (unwrap! (map-get? skill-challenges { challenge-id: challenge-id }) (err u1402)))
         (assessment (unwrap! (map-get? skill-assessments { worker: worker, challenge-id: challenge-id, attempt: attempt }) (err u1406)))
         (evaluator-info (default-to 
            { challenges-created: u0, assessments-conducted: u0, average-rating: u0, total-earnings: u0, certified-evaluator: false }
            (map-get? evaluator-stats { evaluator: tx-sender })))
         (passed (>= (* final-score u100) (* passing-score (get max-score challenge))))
         (certification-level (if (>= final-score (* (get max-score challenge) u90))
                                 "expert"
                                 (if (>= final-score (* (get max-score challenge) u80))
                                    "advanced"
                                    "certified"))))
        
        ;; Only the challenge creator can evaluate
        (asserts! (is-eq tx-sender (get evaluator challenge)) (err u1407))
        (asserts! (is-eq (get evaluated-at assessment) u0) (err u1408)) ;; Not already evaluated
        (asserts! (<= final-score (get max-score challenge)) (err u1405))
        
        ;; Update assessment with final evaluation
        (map-set skill-assessments
            { worker: worker, challenge-id: challenge-id, attempt: attempt }
            (merge assessment {
                score: final-score,
                passed: passed,
                evaluated-at: stacks-block-height,
                evaluator-notes: evaluator-notes
            })
        )
        
        ;; If passed, issue certification
        (if passed
            (map-set worker-certifications
                { worker: worker, skill-name: (get skill-name challenge) }
                {
                    challenge-id: challenge-id,
                    score: final-score,
                    certified-at: stacks-block-height,
                    evaluator: tx-sender,
                    expiry-block: (+ stacks-block-height u52560), ;; ~1 year validity
                    certification-level: certification-level
                }
            )
            true
        )
        
        ;; Pay evaluator reward
        (try! (as-contract (stx-transfer? evaluator-reward tx-sender tx-sender)))
        
        ;; Update evaluator stats
        (map-set evaluator-stats
            { evaluator: tx-sender }
            (merge evaluator-info { 
                assessments-conducted: (+ (get assessments-conducted evaluator-info) u1),
                total-earnings: (+ (get total-earnings evaluator-info) evaluator-reward)
            })
        )
        
        (ok passed)
    )
)

;; Set required certifications for a gig
(define-public (set-gig-certification-requirements 
    (gig-id uint)
    (required-skills (list 5 (string-ascii 30))))
    (let
        ((gig (unwrap! (map-get? gigs { gig-id: gig-id }) err-not-found)))
        (asserts! (is-eq tx-sender (get owner gig)) err-owner-only)
        (map-set gig-certification-requirements
            { gig-id: gig-id }
            { required-skills: required-skills }
        )
        (ok true)
    )
)

;; Check if worker meets certification requirements for a gig
(define-public (verify-worker-certifications 
    (worker principal)
    (gig-id uint))
    (let
        ((requirements (map-get? gig-certification-requirements { gig-id: gig-id })))
        (match requirements
            req (ok (check-all-certifications worker (get required-skills req)))
            (ok true) ;; No requirements set
        )
    )
)

;; Helper function to check if worker has all required certifications
(define-private (check-all-certifications 
    (worker principal)
    (skills (list 5 (string-ascii 30))))
    (fold check-single-certification skills true)
)

(define-private (check-single-certification 
    (skill (string-ascii 30))
    (all-valid bool))
    (let
        ((cert (map-get? worker-certifications { worker: tx-sender, skill-name: skill })))
        (match cert
            c (and all-valid (< stacks-block-height (get expiry-block c)))
            false
        )
    )
)

;; Certify an evaluator (only contract owner)
(define-public (certify-evaluator (evaluator principal))
    (let
        ((evaluator-info (default-to 
            { challenges-created: u0, assessments-conducted: u0, average-rating: u0, total-earnings: u0, certified-evaluator: false }
            (map-get? evaluator-stats { evaluator: evaluator }))))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set evaluator-stats
            { evaluator: evaluator }
            (merge evaluator-info { certified-evaluator: true })
        )
        (ok true)
    )
)

;; Read-only functions for skill verification system

(define-read-only (get-skill-challenge (challenge-id uint))
    (map-get? skill-challenges { challenge-id: challenge-id })
)

(define-read-only (get-worker-certification (worker principal) (skill-name (string-ascii 30)))
    (map-get? worker-certifications { worker: worker, skill-name: skill-name })
)

(define-read-only (get-assessment-result (worker principal) (challenge-id uint) (attempt uint))
    (map-get? skill-assessments { worker: worker, challenge-id: challenge-id, attempt: attempt })
)

(define-read-only (get-evaluator-stats (evaluator principal))
    (map-get? evaluator-stats { evaluator: evaluator })
)

(define-read-only (get-gig-requirements (gig-id uint))
    (map-get? gig-certification-requirements { gig-id: gig-id })
)

(define-read-only (is-certification-valid (worker principal) (skill-name (string-ascii 30)))
    (let
        ((cert (map-get? worker-certifications { worker: worker, skill-name: skill-name })))
        (match cert
            c (< stacks-block-height (get expiry-block c))
            false
        )
    )
)

(define-constant auto-release-delay u144)
(define-constant verification-required u2)

(define-map milestone-payments
    { gig-id: uint, milestone-id: uint }
    {
        amount: uint,
        description: (string-ascii 200),
        deliverable-hash: (string-ascii 64),
        escrow-funded: bool,
        worker-verified: bool,
        client-verified: bool,
        auto-release-block: uint,
        payment-released: bool,
        disputed: bool
    }
)

(define-map milestone-escrows
    { gig-id: uint, milestone-id: uint }
    {
        amount: uint,
        locked: bool,
        funder: principal
    }
)

(define-map milestone-counter
    { gig-id: uint }
    { count: uint }
)

(define-public (create-milestone-payment 
    (gig-id uint) 
    (amount uint) 
    (description (string-ascii 200)))
    (let
        ((gig (unwrap! (map-get? gigs { gig-id: gig-id }) err-not-found))
         (current-count (default-to { count: u0 } (map-get? milestone-counter { gig-id: gig-id })))
         (new-milestone-id (+ (get count current-count) u1)))
        (asserts! (is-eq tx-sender (get owner gig)) err-owner-only)
        (asserts! (is-some (get worker gig)) (err u900))
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set milestone-payments
            { gig-id: gig-id, milestone-id: new-milestone-id }
            {
                amount: amount,
                description: description,
                deliverable-hash: "",
                escrow-funded: true,
                worker-verified: false,
                client-verified: false,
                auto-release-block: u0,
                payment-released: false,
                disputed: false
            }
        )
        (map-set milestone-escrows
            { gig-id: gig-id, milestone-id: new-milestone-id }
            {
                amount: amount,
                locked: true,
                funder: tx-sender
            }
        )
        (map-set milestone-counter { gig-id: gig-id } { count: new-milestone-id })
        (ok new-milestone-id)
    )
)

(define-public (submit-milestone-deliverable 
    (gig-id uint) 
    (milestone-id uint) 
    (deliverable-hash (string-ascii 64)))
    (let
        ((gig (unwrap! (map-get? gigs { gig-id: gig-id }) err-not-found))
         (milestone (unwrap! (map-get? milestone-payments { gig-id: gig-id, milestone-id: milestone-id }) err-not-found)))
        (asserts! (is-eq (some tx-sender) (get worker gig)) err-owner-only)
        (asserts! (get escrow-funded milestone) (err u901))
        (asserts! (not (get payment-released milestone)) (err u902))
        (map-set milestone-payments
            { gig-id: gig-id, milestone-id: milestone-id }
            (merge milestone {
                deliverable-hash: deliverable-hash,
                worker-verified: true,
                auto-release-block: (+ stacks-block-height auto-release-delay)
            })
        )
        (ok true)
    )
)


(define-public (claim-auto-release-payment 
    (gig-id uint) 
    (milestone-id uint))
    (let
        ((milestone (unwrap! (map-get? milestone-payments { gig-id: gig-id, milestone-id: milestone-id }) err-not-found)))
        (asserts! (> stacks-block-height (get auto-release-block milestone)) (err u904))
        (asserts! (get worker-verified milestone) (err u903))
        (asserts! (not (get client-verified milestone)) (err u905))
        (asserts! (not (get payment-released milestone)) (err u902))
        (asserts! (not (get disputed milestone)) (err u906))
        (try! (release-milestone-payment-internal gig-id milestone-id))
        (ok true)
    )
)

(define-public (dispute-milestone 
    (gig-id uint) 
    (milestone-id uint) 
    (reason (string-ascii 500)))
    (let
        ((gig (unwrap! (map-get? gigs { gig-id: gig-id }) err-not-found))
         (milestone (unwrap! (map-get? milestone-payments { gig-id: gig-id, milestone-id: milestone-id }) err-not-found)))
        (asserts! (or (is-eq tx-sender (get owner gig)) 
                     (is-eq (some tx-sender) (get worker gig))) err-owner-only)
        (asserts! (get worker-verified milestone) (err u903))
        (asserts! (not (get payment-released milestone)) (err u902))
        (map-set milestone-payments
            { gig-id: gig-id, milestone-id: milestone-id }
            (merge milestone { disputed: true })
        )
        (map-set disputes
            { gig-id: gig-id }
            {
                complainant: tx-sender,
                description: reason,
                resolved: false
            }
        )
        (ok true)
    )
)

(define-private (release-milestone-payment-internal 
    (gig-id uint) 
    (milestone-id uint))
    (let
        ((gig (unwrap! (map-get? gigs { gig-id: gig-id }) err-not-found))
         (milestone (unwrap! (map-get? milestone-payments { gig-id: gig-id, milestone-id: milestone-id }) err-not-found))
         (escrow (unwrap! (map-get? milestone-escrows { gig-id: gig-id, milestone-id: milestone-id }) err-not-found)))
        (asserts! (get locked escrow) (err u907))
        (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (unwrap! (get worker gig) err-not-found))))
        (map-set milestone-payments
            { gig-id: gig-id, milestone-id: milestone-id }
            (merge milestone { payment-released: true })
        )
        (map-set milestone-escrows
            { gig-id: gig-id, milestone-id: milestone-id }
            (merge escrow { locked: false })
        )
        (ok true)
    )
)

(define-read-only (get-milestone-status 
    (gig-id uint) 
    (milestone-id uint))
    (map-get? milestone-payments { gig-id: gig-id, milestone-id: milestone-id })
)

(define-read-only (get-milestone-escrow 
    (gig-id uint) 
    (milestone-id uint))
    (map-get? milestone-escrows { gig-id: gig-id, milestone-id: milestone-id })
)

(define-read-only (can-auto-release 
    (gig-id uint) 
    (milestone-id uint))
    (let
        ((milestone (map-get? milestone-payments { gig-id: gig-id, milestone-id: milestone-id })))
        (match milestone
            m (and (get worker-verified m)
                  (not (get client-verified m))
                  (not (get disputed m))
                  (not (get payment-released m))
                  (> stacks-block-height (get auto-release-block m)))
            false
        )
    )
)

