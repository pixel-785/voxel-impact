;; VoxelImpact - Decentralized Social Impact Platform
;; A platform for measuring and verifying social change through spatial data

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-insufficient-stake (err u104))
(define-constant err-invalid-status (err u105))
(define-constant err-already-validated (err u106))
(define-constant err-validation-incomplete (err u107))

;; Minimum stake required for validators (in microSTX)
(define-constant min-validator-stake u1000000)

;; Data Variables
(define-data-var platform-fee-percentage uint u2) ;; 2% platform fee

;; Data Maps

;; Voxel represents a unit of social impact in specific geographic coordinates
(define-map voxels
    { voxel-id: uint }
    {
        creator: principal,
        latitude: int,
        longitude: int,
        altitude: int,
        impact-type: (string-ascii 50),
        target-amount: uint,
        funded-amount: uint,
        milestone-count: uint,
        completed-milestones: uint,
        status: (string-ascii 20),
        created-at: uint
    }
)

;; Projects associated with voxels
(define-map projects
    { project-id: uint }
    {
        voxel-id: uint,
        ngo: principal,
        title: (string-utf8 100),
        description: (string-utf8 500),
        funding-goal: uint,
        current-funding: uint,
        start-block: uint,
        end-block: uint,
        status: (string-ascii 20)
    }
)

;; Milestones for projects
(define-map milestones
    { project-id: uint, milestone-id: uint }
    {
        description: (string-utf8 200),
        required-amount: uint,
        satellite-validated: bool,
        witness-validated: bool,
        beneficiary-validated: bool,
        validation-count: uint,
        completed: bool,
        completed-at: uint
    }
)

;; Validators who stake tokens to verify impact
(define-map validators
    { validator: principal }
    {
        stake-amount: uint,
        reputation-score: uint,
        total-validations: uint,
        accurate-validations: uint,
        registered-at: uint
    }
)

;; Validation records
(define-map validations
    { project-id: uint, milestone-id: uint, validator: principal }
    {
        validation-type: (string-ascii 20), ;; satellite, witness, beneficiary
        approved: bool,
        evidence-hash: (string-ascii 64),
        validated-at: uint
    }
)

;; Impact investments
(define-map investments
    { investor: principal, project-id: uint }
    {
        amount: uint,
        invested-at: uint,
        withdrawn: bool
    }
)

;; Counters
(define-data-var voxel-nonce uint u0)
(define-data-var project-nonce uint u0)

;; Read-only functions

(define-read-only (get-voxel (voxel-id uint))
    (map-get? voxels { voxel-id: voxel-id })
)

(define-read-only (get-project (project-id uint))
    (map-get? projects { project-id: project-id })
)

(define-read-only (get-milestone (project-id uint) (milestone-id uint))
    (map-get? milestones { project-id: project-id, milestone-id: milestone-id })
)

(define-read-only (get-validator (validator principal))
    (map-get? validators { validator: validator })
)

(define-read-only (get-validation (project-id uint) (milestone-id uint) (validator principal))
    (map-get? validations { project-id: project-id, milestone-id: milestone-id, validator: validator })
)

(define-read-only (get-investment (investor principal) (project-id uint))
    (map-get? investments { investor: investor, project-id: project-id })
)

(define-read-only (get-platform-fee)
    (var-get platform-fee-percentage)
)

;; Public functions

;; Create a new voxel for social impact tracking
(define-public (create-voxel (latitude int) (longitude int) (altitude int) (impact-type (string-ascii 50)))
    (let
        (
            (voxel-id (+ (var-get voxel-nonce) u1))
        )
        (map-set voxels
            { voxel-id: voxel-id }
            {
                creator: tx-sender,
                latitude: latitude,
                longitude: longitude,
                altitude: altitude,
                impact-type: impact-type,
                target-amount: u0,
                funded-amount: u0,
                milestone-count: u0,
                completed-milestones: u0,
                status: "active",
                created-at: block-height
            }
        )
        (var-set voxel-nonce voxel-id)
        (ok voxel-id)
    )
)

;; Create a project within a voxel
(define-public (create-project 
    (voxel-id uint) 
    (title (string-utf8 100)) 
    (description (string-utf8 500))
    (funding-goal uint)
    (duration uint))
    (let
        (
            (project-id (+ (var-get project-nonce) u1))
            (voxel (unwrap! (get-voxel voxel-id) err-not-found))
        )
        (map-set projects
            { project-id: project-id }
            {
                voxel-id: voxel-id,
                ngo: tx-sender,
                title: title,
                description: description,
                funding-goal: funding-goal,
                current-funding: u0,
                start-block: block-height,
                end-block: (+ block-height duration),
                status: "funding"
            }
        )
        (var-set project-nonce project-id)
        (ok project-id)
    )
)

;; Register as a validator by staking tokens
(define-public (register-validator (stake-amount uint))
    (begin
        (asserts! (>= stake-amount min-validator-stake) err-insufficient-stake)
        (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
        (map-set validators
            { validator: tx-sender }
            {
                stake-amount: stake-amount,
                reputation-score: u100,
                total-validations: u0,
                accurate-validations: u0,
                registered-at: block-height
            }
        )
        (ok true)
    )
)

;; Invest in a project
(define-public (invest-in-project (project-id uint) (amount uint))
    (let
        (
            (project (unwrap! (get-project project-id) err-not-found))
            (current-investment (default-to 
                { amount: u0, invested-at: u0, withdrawn: false }
                (get-investment tx-sender project-id)))
        )
        (asserts! (is-eq (get status project) "funding") err-invalid-status)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        ;; Update project funding
        (map-set projects
            { project-id: project-id }
            (merge project { current-funding: (+ (get current-funding project) amount) })
        )
        
        ;; Record investment
        (map-set investments
            { investor: tx-sender, project-id: project-id }
            {
                amount: (+ (get amount current-investment) amount),
                invested-at: block-height,
                withdrawn: false
            }
        )
        (ok true)
    )
)

;; Create a milestone for a project
(define-public (create-milestone 
    (project-id uint) 
    (milestone-id uint)
    (description (string-utf8 200))
    (required-amount uint))
    (let
        (
            (project (unwrap! (get-project project-id) err-not-found))
        )
        (asserts! (is-eq (get ngo project) tx-sender) err-unauthorized)
        (asserts! (is-none (get-milestone project-id milestone-id)) err-already-exists)
        
        (map-set milestones
            { project-id: project-id, milestone-id: milestone-id }
            {
                description: description,
                required-amount: required-amount,
                satellite-validated: false,
                witness-validated: false,
                beneficiary-validated: false,
                validation-count: u0,
                completed: false,
                completed-at: u0
            }
        )
        (ok true)
    )
)

;; Submit validation for a milestone
(define-public (submit-validation
    (project-id uint)
    (milestone-id uint)
    (validation-type (string-ascii 20))
    (approved bool)
    (evidence-hash (string-ascii 64)))
    (let
        (
            (validator-data (unwrap! (get-validator tx-sender) err-unauthorized))
            (milestone (unwrap! (get-milestone project-id milestone-id) err-not-found))
        )
        (asserts! (is-none (get-validation project-id milestone-id tx-sender)) err-already-validated)
        
        ;; Record validation
        (map-set validations
            { project-id: project-id, milestone-id: milestone-id, validator: tx-sender }
            {
                validation-type: validation-type,
                approved: approved,
                evidence-hash: evidence-hash,
                validated-at: block-height
            }
        )
        
        ;; Update validator stats
        (map-set validators
            { validator: tx-sender }
            (merge validator-data { 
                total-validations: (+ (get total-validations validator-data) u1)
            })
        )
        
        ;; Update milestone validation status
        (if approved
            (let
                (
                    (updated-milestone (merge milestone {
                        validation-count: (+ (get validation-count milestone) u1),
                        satellite-validated: (if (is-eq validation-type "satellite") true (get satellite-validated milestone)),
                        witness-validated: (if (is-eq validation-type "witness") true (get witness-validated milestone)),
                        beneficiary-validated: (if (is-eq validation-type "beneficiary") true (get beneficiary-validated milestone))
                    }))
                )
                (map-set milestones
                    { project-id: project-id, milestone-id: milestone-id }
                    updated-milestone
                )
                (ok true)
            )
            (ok true)
        )
    )
)

;; Complete milestone and release funds (requires all three validations)
(define-public (complete-milestone (project-id uint) (milestone-id uint))
    (let
        (
            (project (unwrap! (get-project project-id) err-not-found))
            (milestone (unwrap! (get-milestone project-id milestone-id) err-not-found))
        )
        (asserts! (is-eq (get ngo project) tx-sender) err-unauthorized)
        (asserts! (get satellite-validated milestone) err-validation-incomplete)
        (asserts! (get witness-validated milestone) err-validation-incomplete)
        (asserts! (get beneficiary-validated milestone) err-validation-incomplete)
        (asserts! (not (get completed milestone)) err-invalid-status)
        
        ;; Calculate platform fee
        (let
            (
                (fee-amount (/ (* (get required-amount milestone) (var-get platform-fee-percentage)) u100))
                (ngo-amount (- (get required-amount milestone) fee-amount))
            )
            ;; Transfer funds to NGO
            (try! (as-contract (stx-transfer? ngo-amount tx-sender (get ngo project))))
            
            ;; Mark milestone as completed
            (map-set milestones
                { project-id: project-id, milestone-id: milestone-id }
                (merge milestone { 
                    completed: true,
                    completed-at: block-height
                })
            )
            (ok true)
        )
    )
)

;; Admin function to update platform fee
(define-public (set-platform-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-fee u10) err-invalid-status) ;; Max 10%
        (var-set platform-fee-percentage new-fee)
        (ok true)
    )
)