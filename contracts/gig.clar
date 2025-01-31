;; Decentralized Gig Work Platform

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-gig-closed (err u103))

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
