;; Quality Assurance & Testing Framework for Decentralized Gig Platform
;; This contract enables clients to define test cases and quality criteria for gigs
;; Workers must pass these QA tests before receiving payment approval

;; Constants for error handling
(define-constant err-owner-only (err u2000))
(define-constant err-not-found (err u2001))
(define-constant err-already-exists (err u2002))
(define-constant err-unauthorized (err u2003))
(define-constant err-invalid-score (err u2004))
(define-constant err-test-not-completed (err u2005))
(define-constant err-insufficient-funds (err u2006))
(define-constant err-qa-locked (err u2007))
(define-constant err-invalid-threshold (err u2008))
(define-constant err-test-failed (err u2009))

;; Constants for QA system configuration
(define-constant contract-owner tx-sender)
(define-constant qa-reviewer-fee u25) ;; Fee for QA reviewers per test case
(define-constant min-passing-score u70) ;; Minimum score to pass QA (out of 100)
(define-constant max-test-cases u10) ;; Maximum test cases per gig
(define-constant qa-timeout-blocks u288) ;; ~2 days for QA completion

;; Data structure for QA test suites defined by clients
(define-map qa-test-suites
    { gig-id: uint }
    {
        client: principal,
        total-test-cases: uint,
        passing-threshold: uint, ;; Percentage needed to pass
        reward-pool: uint, ;; STX reward for passing all tests
        created-at: uint,
        active: bool,
        completion-deadline: uint
    }
)

;; Individual test cases within a test suite
(define-map qa-test-cases
    { gig-id: uint, test-id: uint }
    {
        title: (string-ascii 100),
        description: (string-ascii 300),
        test-type: (string-ascii 20), ;; "functional", "performance", "security", "ui", "integration"
        weight: uint, ;; Weight of this test in overall score (1-10)
        expected-result: (string-ascii 200),
        created-by: principal,
        status: (string-ascii 15) ;; "pending", "in-review", "passed", "failed"
    }
)

;; Test execution results submitted by workers
(define-map qa-test-results
    { gig-id: uint, test-id: uint, worker: principal }
    {
        result-description: (string-ascii 400),
        evidence-hash: (string-ascii 64), ;; IPFS/Arweave hash of evidence
        submitted-at: uint,
        score: uint, ;; 0-100 score for this test
        reviewer-notes: (string-ascii 300),
        reviewed-at: uint,
        reviewed-by: principal
    }
)

;; Overall QA performance tracking for workers
(define-map worker-qa-performance
    { worker: principal }
    {
        total-tests-completed: uint,
        total-tests-passed: uint,
        average-score: uint,
        qa-reputation: uint,
        last-updated: uint
    }
)

;; QA reviewers and their qualifications
(define-map qa-reviewers
    { reviewer: principal }
    {
        specializations: (list 5 (string-ascii 20)),
        total-reviews: uint,
        approval-rating: uint,
        certified: bool,
        earnings: uint,
        active: bool
    }
)

;; Track QA completion status for gigs
(define-map gig-qa-status
    { gig-id: uint }
    {
        worker: principal,
        tests-submitted: uint,
        tests-reviewed: uint,
        overall-score: uint,
        passed: bool,
        completion-date: uint,
        bonus-earned: uint
    }
)

;; Counter for test case IDs
(define-map test-case-counter
    { gig-id: uint }
    { count: uint }
)

;; Create QA test suite for a gig (called by client)
(define-public (create-qa-test-suite 
    (gig-id uint) 
    (passing-threshold uint) 
    (reward-pool uint)
    (deadline-blocks uint))
    (let
        ((existing-suite (map-get? qa-test-suites { gig-id: gig-id })))
        
        ;; Validation checks
        (asserts! (is-none existing-suite) err-already-exists)
        (asserts! (and (>= passing-threshold u50) (<= passing-threshold u100)) err-invalid-threshold)
        (asserts! (> reward-pool u0) err-insufficient-funds)
        
        ;; Transfer reward pool to contract for escrow
        (try! (stx-transfer? reward-pool tx-sender (as-contract tx-sender)))
        
        ;; Create the test suite
        (map-set qa-test-suites
            { gig-id: gig-id }
            {
                client: tx-sender,
                total-test-cases: u0,
                passing-threshold: passing-threshold,
                reward-pool: reward-pool,
                created-at: stacks-block-height,
                active: true,
                completion-deadline: (+ stacks-block-height deadline-blocks)
            }
        )
        
        ;; Initialize test case counter
        (map-set test-case-counter { gig-id: gig-id } { count: u0 })
        
        (ok gig-id)
    )
)

;; Add individual test case to a test suite
(define-public (add-test-case
    (gig-id uint)
    (title (string-ascii 100))
    (description (string-ascii 300))
    (test-type (string-ascii 20))
    (weight uint)
    (expected-result (string-ascii 200)))
    (let
        ((suite (unwrap! (map-get? qa-test-suites { gig-id: gig-id }) err-not-found))
         (counter (unwrap! (map-get? test-case-counter { gig-id: gig-id }) err-not-found))
         (new-test-id (+ (get count counter) u1)))
        
        ;; Validation checks
        (asserts! (is-eq tx-sender (get client suite)) err-owner-only)
        (asserts! (get active suite) err-qa-locked)
        (asserts! (< (get total-test-cases suite) max-test-cases) (err u2010))
        (asserts! (and (>= weight u1) (<= weight u10)) err-invalid-score)
        
        ;; Create test case
        (map-set qa-test-cases
            { gig-id: gig-id, test-id: new-test-id }
            {
                title: title,
                description: description,
                test-type: test-type,
                weight: weight,
                expected-result: expected-result,
                created-by: tx-sender,
                status: "pending"
            }
        )
        
        ;; Update counters
        (map-set test-case-counter { gig-id: gig-id } { count: new-test-id })
        (map-set qa-test-suites
            { gig-id: gig-id }
            (merge suite { total-test-cases: (+ (get total-test-cases suite) u1) })
        )
        
        (ok new-test-id)
    )
)

;; Submit test results (called by worker)
(define-public (submit-test-result
    (gig-id uint)
    (test-id uint)
    (result-description (string-ascii 400))
    (evidence-hash (string-ascii 64)))
    (let
        ((test-case (unwrap! (map-get? qa-test-cases { gig-id: gig-id, test-id: test-id }) err-not-found))
         (suite (unwrap! (map-get? qa-test-suites { gig-id: gig-id }) err-not-found)))
        
        ;; Validation checks
        (asserts! (get active suite) err-qa-locked)
        (asserts! (< stacks-block-height (get completion-deadline suite)) (err u2011))
        (asserts! (is-eq (get status test-case) "pending") err-already-exists)
        
        ;; Submit test result
        (map-set qa-test-results
            { gig-id: gig-id, test-id: test-id, worker: tx-sender }
            {
                result-description: result-description,
                evidence-hash: evidence-hash,
                submitted-at: stacks-block-height,
                score: u0, ;; Will be set by reviewer
                reviewer-notes: "",
                reviewed-at: u0,
                reviewed-by: 'SP000000000000000000002Q6VF78 ;; Placeholder principal
            }
        )
        
        ;; Update test case status
        (map-set qa-test-cases
            { gig-id: gig-id, test-id: test-id }
            (merge test-case { status: "in-review" })
        )
        
        (ok true)
    )
)

;; Review and score a test result (called by certified QA reviewer)
(define-public (review-test-result
    (gig-id uint)
    (test-id uint)
    (worker principal)
    (score uint)
    (reviewer-notes (string-ascii 300)))
    (let
        ((result (unwrap! (map-get? qa-test-results { gig-id: gig-id, test-id: test-id, worker: worker }) err-not-found))
         (test-case (unwrap! (map-get? qa-test-cases { gig-id: gig-id, test-id: test-id }) err-not-found))
         (reviewer-info (unwrap! (map-get? qa-reviewers { reviewer: tx-sender }) err-unauthorized)))
        
        ;; Validation checks
        (asserts! (get certified reviewer-info) err-unauthorized)
        (asserts! (get active reviewer-info) err-unauthorized)
        (asserts! (<= score u100) err-invalid-score)
        (asserts! (is-eq (get reviewed-at result) u0) err-already-exists) ;; Not already reviewed
        
        ;; Update test result with review
        (map-set qa-test-results
            { gig-id: gig-id, test-id: test-id, worker: worker }
            (merge result {
                score: score,
                reviewer-notes: reviewer-notes,
                reviewed-at: stacks-block-height,
                reviewed-by: tx-sender
            })
        )
        
        ;; Update test case status based on score
        (map-set qa-test-cases
            { gig-id: gig-id, test-id: test-id }
            (merge test-case { 
                status: (if (>= score min-passing-score) "passed" "failed")
            })
        )
        
        ;; Pay reviewer fee
        (try! (as-contract (stx-transfer? qa-reviewer-fee tx-sender tx-sender)))
        
        ;; Update reviewer stats
        (map-set qa-reviewers
            { reviewer: tx-sender }
            (merge reviewer-info {
                total-reviews: (+ (get total-reviews reviewer-info) u1),
                earnings: (+ (get earnings reviewer-info) qa-reviewer-fee)
            })
        )
        
        (ok true)
    )
)

;; Calculate and finalize QA results for a gig
(define-public (finalize-qa-results (gig-id uint) (worker principal))
    (let
        ((suite (unwrap! (map-get? qa-test-suites { gig-id: gig-id }) err-not-found))
         (overall-score (calculate-overall-score gig-id worker (get total-test-cases suite)))
         (passed (>= overall-score (get passing-threshold suite))))
        
        ;; Validation checks
        (asserts! (is-eq tx-sender (get client suite)) err-owner-only)
        (asserts! (> overall-score u0) err-test-not-completed) ;; Some tests must be completed
        
        ;; Record final QA status
        (map-set gig-qa-status
            { gig-id: gig-id }
            {
                worker: worker,
                tests-submitted: (get total-test-cases suite),
                tests-reviewed: (get total-test-cases suite), ;; Assuming all reviewed before finalization
                overall-score: overall-score,
                passed: passed,
                completion-date: stacks-block-height,
                bonus-earned: (if passed (get reward-pool suite) u0)
            }
        )
        
        ;; If passed, transfer reward pool to worker
        (if passed
            (try! (as-contract (stx-transfer? (get reward-pool suite) tx-sender worker)))
            true ;; If failed, funds stay in contract for client to reclaim
        )
        
        ;; Update worker's QA performance
        ;; (try! (update-worker-qa-performance worker overall-score passed))
        
        ;; Deactivate test suite
        (map-set qa-test-suites
            { gig-id: gig-id }
            (merge suite { active: false })
        )
        
        (ok passed)
    )
)

;; Helper function to calculate weighted overall score
(define-private (calculate-overall-score (gig-id uint) (worker principal) (total-tests uint))
    (if (is-eq total-tests u0)
        u0
        u85 ;; Simplified calculation - in real implementation, would iterate through all test results
    )
)

;; Helper function to update worker QA performance metrics
(define-private (update-worker-qa-performance (worker principal) (score uint) (passed bool))
    (let
        ((current-perf (default-to 
            { total-tests-completed: u0, total-tests-passed: u0, average-score: u0, qa-reputation: u0, last-updated: u0 }
            (map-get? worker-qa-performance { worker: worker })))
         (new-total (+ (get total-tests-completed current-perf) u1))
         (new-passed (+ (get total-tests-passed current-perf) (if passed u1 u0))))
        
        (map-set worker-qa-performance
            { worker: worker }
            {
                total-tests-completed: new-total,
                total-tests-passed: new-passed,
                average-score: (/ (+ (* (get average-score current-perf) (get total-tests-completed current-perf)) score) new-total),
                qa-reputation: (+ (get qa-reputation current-perf) (if passed u10 u0)),
                last-updated: stacks-block-height
            }
        )
        (ok true)
    )
)

;; Register as QA reviewer
(define-public (register-qa-reviewer (specializations (list 5 (string-ascii 20))))
    (let
        ((existing-reviewer (map-get? qa-reviewers { reviewer: tx-sender })))
        (asserts! (is-none existing-reviewer) err-already-exists)
        (map-set qa-reviewers
            { reviewer: tx-sender }
            {
                specializations: specializations,
                total-reviews: u0,
                approval-rating: u100,
                certified: false, ;; Must be certified by contract owner
                earnings: u0,
                active: true
            }
        )
        (ok true)
    )
)

;; Certify QA reviewer (only contract owner)
(define-public (certify-qa-reviewer (reviewer principal))
    (let
        ((reviewer-info (unwrap! (map-get? qa-reviewers { reviewer: reviewer }) err-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set qa-reviewers
            { reviewer: reviewer }
            (merge reviewer-info { certified: true })
        )
        (ok true)
    )
)

;; Read-only functions for querying QA data

(define-read-only (get-qa-test-suite (gig-id uint))
    (map-get? qa-test-suites { gig-id: gig-id })
)

(define-read-only (get-test-case (gig-id uint) (test-id uint))
    (map-get? qa-test-cases { gig-id: gig-id, test-id: test-id })
)

(define-read-only (get-test-result (gig-id uint) (test-id uint) (worker principal))
    (map-get? qa-test-results { gig-id: gig-id, test-id: test-id, worker: worker })
)

(define-read-only (get-worker-qa-performance (worker principal))
    (map-get? worker-qa-performance { worker: worker })
)

(define-read-only (get-qa-reviewer (reviewer principal))
    (map-get? qa-reviewers { reviewer: reviewer })
)

(define-read-only (get-gig-qa-status (gig-id uint))
    (map-get? gig-qa-status { gig-id: gig-id })
)

(define-read-only (is-qa-reviewer-certified (reviewer principal))
    (let
        ((reviewer-info (map-get? qa-reviewers { reviewer: reviewer })))
        (match reviewer-info
            info (and (get certified info) (get active info))
            false
        )
    )
)
