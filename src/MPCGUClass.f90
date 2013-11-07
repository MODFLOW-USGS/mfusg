module MPCGUClass
  
    type, public :: pcgusolver
      integer :: initialized = 0
      ! solver parameters
      integer :: iout
      integer :: maxinner
      integer :: ilinmeth
      integer :: ipc
      integer :: iscl
      integer :: iord
      integer :: iaord
      integer :: icnvgopt
      real :: hclose
      real :: rclose
      real :: relax
      ! working variables
      integer :: iacpc
      integer :: iaord_init
      integer :: niter
      doubleprecision :: epfact
      doubleprecision :: l2norm0
      ! crs data
      integer :: nia_all = 0
      integer :: nnz_all = 0
      integer :: nia     = 0
      integer :: nnz     = 0
      integer, allocatable, dimension(:) :: ia
      integer, allocatable, dimension(:) :: ja
      ! solver storage
      doubleprecision, allocatable, dimension(:) :: a
      doubleprecision, allocatable, dimension(:) :: x
      doubleprecision, allocatable, dimension(:) :: b
      ! solver storage that depends on solver parameters
      ! preconditioner storage
      integer :: niapc  = 0
      integer :: njapc  = 0
      integer :: nnzapc = 0
      integer, allocatable, dimension(:) :: iapc
      integer, allocatable, dimension(:) :: japc
      doubleprecision, allocatable, dimension(:) :: apc
      ! scaling
      doubleprecision, allocatable, dimension(:) :: dscale
      doubleprecision, allocatable, dimension(:) :: dscale2
      ! reordering for active nodes
      integer, allocatable, dimension(:) :: lorder_act
      integer, allocatable, dimension(:) :: iorder_act
      ! rkm and minimum degree reordering
      integer, allocatable, dimension(:) :: lorder
      integer, allocatable, dimension(:) :: iorder
      ! temporary vectors for use with reordering and calls to linear solvers
      integer,          allocatable, dimension(:)    :: ia0
      integer,          allocatable, dimension(:)    :: ja0
      doubleprecision,  allocatable, dimension(:)    :: a0
      integer,          allocatable, dimension(:)    :: ia1
      integer,          allocatable, dimension(:)    :: ja1
      doubleprecision,  allocatable, dimension(:)    :: a1
      ! cg working vectors
      integer, allocatable, dimension(:) :: iw
      doubleprecision, allocatable, dimension(:) :: w
      integer, allocatable, dimension(:) :: id
      doubleprecision, allocatable, dimension(:) :: d
      doubleprecision, allocatable, dimension(:) :: p
      doubleprecision, allocatable, dimension(:) :: q
      doubleprecision, allocatable, dimension(:) :: z
      ! additional working vectors for bcgs
      integer :: niabcgs
      doubleprecision, allocatable, dimension(:) :: t
      doubleprecision, allocatable, dimension(:) :: v
      doubleprecision, allocatable, dimension(:) :: dhat
      doubleprecision, allocatable, dimension(:) :: phat
      doubleprecision, allocatable, dimension(:) :: qhat
      ! procedures (methods)
      contains
        procedure :: init => initialize
        procedure, private :: initialize_crs
        procedure :: solve
        procedure, private :: cg
        procedure, private :: bcgs
        procedure :: destroy
        procedure :: getv
        procedure :: geta
        procedure :: writev
        procedure :: writea
        procedure :: writeapc
    end type pcgusolver
          
    contains

      ! destroy instance of pcgu solver class - deallocates memory and resets size variables
      ! solver parameters are retained
      subroutine destroy(this)
        implicit none
        class(pcgusolver), intent(inout) :: this
        !--deallocate allocated arrays
        ! crs
        deallocate(this%ia)
        deallocate(this%ja)
        ! base solver storage
        deallocate(this%a)
        deallocate(this%x)
        deallocate(this%b)
        ! preconditioner storage
        deallocate(this%iapc)
        deallocate(this%japc)
        deallocate(this%apc)
        ! scaling
        deallocate(this%dscale)
        deallocate(this%dscale2)
        ! reordering for active nodes
        deallocate(this%lorder_act)
        deallocate(this%iorder_act)
        ! rkm and minimum degree reordering
        deallocate(this%lorder)
        deallocate(this%iorder)
        ! reordering and solution vectors
        deallocate(this%ia0)
        deallocate(this%ja0)
        deallocate(this%a0)
        deallocate(this%ia1)
        deallocate(this%ja1)
        deallocate(this%a1)
        ! cg working vectors
        deallocate(this%iw)
        deallocate(this%w)
        deallocate(this%id)
        deallocate(this%d)
        deallocate(this%p)
        deallocate(this%q)
        deallocate(this%z)
        ! additional working vectors for bcgs
        deallocate(this%t)
        deallocate(this%v)
        deallocate(this%dhat)
        deallocate(this%phat)
        deallocate(this%qhat)
        !--reset variables
        this%initialized = 0
        this%iout        = 0
        this%iacpc       = 1
        this%iaord_init  = 0
        this%nia_all     = 0
        this%nia         = 0
        this%nnz_all     = 0
        this%nnz         = 0
        this%niapc       = 0
        this%nnzapc      = 0
      end subroutine destroy

      ! initialize instance of pcgu solver class using passed solver parameters
      integer function initialize(this,iout,maxinner,ilinmeth,ipc,iscl,iord,iaord,icnvgopt,hclose,rclose,relax,nia,nnz,ia,ja) result(ival)
        implicit none
        ! dummy variables
        class(pcgusolver), intent(inout) :: this
        integer,intent(in) :: iout
        integer,intent(in) :: maxinner
        integer,intent(in) :: ilinmeth,ipc,iscl,iord,iaord
        integer,intent(in) :: icnvgopt
        real,intent(in) :: hclose,rclose,relax
        integer,intent(in) :: nia,nnz
        integer, dimension(nia+1), intent(in) :: ia
        integer, dimension(nnz), intent(in)   :: ja
        ! local variables
        ! code
        ival = 0
        if ( this%initialized .ne. 0 ) return
        ival = 1
        this%initialized = 1
        this%iout        = iout
        this%maxinner    = maxinner
        this%ilinmeth    = ilinmeth
        this%ipc         = ipc
        this%iscl        = iscl
        this%iord        = iord
        this%iaord       = iaord
        this%icnvgopt    = icnvgopt
        this%hclose      = hclose
        this%rclose      = rclose
        this%relax       = relax
        this%iacpc       = 1
        this%iaord_init  = 0
        this%nia_all     = nia
        this%nia         = nia
        this%nnz_all     = nnz
        this%nnz         = nnz
        ! fill ia and ja and allocate space
        ival = this%initialize_crs( ia, ja )
        ! return
        return
      end function initialize
      
      ! fill class instance with ia and ja and then allocate
      ! memory for solver arrays based on specified solver parameters
      integer function initialize_crs(this, ia, ja) result(ival)
        implicit none
        class(pcgusolver), intent(inout) :: this
        integer, dimension(this%nia+1), intent(in) :: ia
        integer, dimension(this%nnz), intent(in)   :: ja
        ! local
        integer :: n
        integer iscllen, i0, iolen
        integer :: iin, iiz
        integer, parameter :: izero = 0
        doubleprecision, parameter :: dzero = 0.0d0
        doubleprecision, parameter :: done = 1.0d0
        ! code
        ival = 0
        if ( this%initialized .eq. 0 ) return
        ! allocate crs data
        allocate(this%ia(this%nia+1))
        allocate(this%ja(this%nnz))
        ! fill ia and ja with passed data
        do n = 1, this%nia+1
          this%ia(n) = ia(n)
        end do
        do n = 1, this%nnz
          this%ja(n) = ja(n)
        end do
        ! allocate and initialize base solver storage (a, x, and b)
        allocate(this%a(this%nnz))
        allocate(this%x(this%nia))
        allocate(this%b(this%nia))
        do n = 1, this%nia
          this%x(n) = dzero
          this%b(n) = dzero
        end do
        do n = 1, this%nnz
          this%a(n) = dzero
        end do
        ! allocate and initialize storage that depends on solver parameters
        iscllen  = 1
        if ( this%iscl.ne.0 ) iscllen  = this%nia
        allocate ( this%dscale(iscllen), this%dscale2(iscllen) )
        ! allocate memory for preconditioning matrix
        this%niapc  = this%nia
        this%njapc  = this%nnz
        this%nnzapc = this%nnz
        if ( this%ipc.eq.0 ) then
          this%niapc  = 1
          this%njapc  = 1
          this%nnzapc = 1
        else if ( this%ipc.eq.1 ) then
          this%niapc  = 1
          this%njapc  = 1
          this%nnzapc = this%nnz
        end if
        allocate( this%iapc(this%niapc+1) )
        allocate( this%japc(this%njapc) )
        allocate( this%apc(this%nnzapc) )
        ! allocate memory for ilu0 and milu0 non-zero row entry vector
        allocate( this%iw(this%niapc) )
        allocate( this%w(this%niapc) )
        ! generate iapc and japc
        if ( this%ipc.eq.2 .or. this%ipc.eq.3 ) then
          call SPCGU_PCCRS(this%nia,this%nnz,this%ia,this%ja,this%iapc,this%japc)
        end if
        ! generate vectors for inactive cell reordering
        i0     = 1
        iolen  = 1
        if ( this%iaord.ne.0 ) then
          i0     = this%nia
          iolen  = this%nnz
        end if
        allocate( this%lorder_act(i0)  )
        allocate( this%iorder_act(i0)  )
        ! allocate space for permutation vector
        i0     = 1
        iolen  = 1
        if ( this%iord.ne.0 ) then
          i0     = this%nia
          iolen  = this%nnz
        end if
        allocate( this%lorder(i0)  )
        allocate( this%iorder(i0)  )
        ! allocate temporary vectors for reordering and use in
        ! linear solvers
        iin = 1
        iiz = 1
        if( this%iaord.ne.0 ) then
          iin = this%nia
          iiz = this%nnz
        end if
        allocate( this%ia0(iin+1)      )
        allocate( this%ja0(iiz)        )
        allocate( this%a0(iiz)         )
        allocate( this%ia1(this%nia+1) )
        allocate( this%ja1(this%nnz)   )
        allocate( this%a1(this%nnz)    )
        ! allocate working vectors for pcgu solver      
        allocate( this%id(this%nia) )
        allocate( this%d(this%nia)  )
        allocate( this%p(this%nia)  )
        allocate( this%q(this%nia)  )
        allocate( this%z(this%nia)  )
        ! allocate memory for bcgs working arrays
        this%niabcgs = 1
        if ( this%ilinmeth.eq.2 ) then
          this%niabcgs = this%nia
        end if
        allocate( this%t(this%niabcgs) )
        allocate( this%v(this%niabcgs) )
        allocate( this%dhat(this%niabcgs) )
        allocate( this%phat(this%niabcgs) )
        allocate( this%qhat(this%niabcgs) )
        ! initalize pcgu vectors
        do n = 1, iscllen
          this%dscale(n)  = done
          this%dscale2(n) = done
        end do
        do n = 1, this%nnzapc
          this%apc(n)  = dzero
        end do
        ! working vectors
        do n = 1, this%nia
          this%id(n)    = izero
          this%d(n)    = dzero
          this%p(n)    = dzero
          this%q(n)    = dzero
          this%z(n)    = dzero
        end do
        do n = 1, this%niapc
          this%iw(n)   = izero
          this%w(n)    = dzero
        end do
        ! bcgs working vectors
        do n = 1, this%niabcgs
          this%t(n)    = dzero
          this%v(n)    = dzero
          this%dhat(n) = dzero
          this%phat(n) = dzero
          this%qhat(n) = dzero
        end do
        ! reverse cuthill mckee ordering
        ! note - using gnrcm and odrv subroutines in reorder_subs.f
        if ( this%iord.ne.0 ) then
          call SSPCGU_CALC_ORDER( this%iout,this%iord,this%nia,this%nnz,this%ia,this%ja,this%lorder,this%iorder )
        END IF
        ! return
        return
      end function initialize_crs
      
      ! subroutine to solve linear system of equations defined in instance of pcgu solver class
      subroutine solve(this, kstp, kiter, icelconv, ibound, ac, hnew, rhs, icnvg, innerit)
        implicit none
        ! dummy variables        
        class(pcgusolver), intent(inout) :: this
        integer, intent(in) :: kstp
        integer, intent(in) :: kiter
        integer, intent(in) :: icelconv
        integer, dimension(this%nia_all), intent(in) :: ibound
        doubleprecision, dimension(this%nnz_all), intent(in) :: ac
        doubleprecision, dimension(this%nia_all), intent(inout) :: hnew
        doubleprecision, dimension(this%nia_all), intent(in) :: rhs
        integer, intent(inout) :: icnvg
        integer, intent(inout) :: innerit
        ! local variables 
        integer :: n
        integer :: itmax
        doubleprecision :: tv
        doubleprecision :: rmax
        doubleprecision, parameter :: dzero = 0.0d0
        ! code
        icnvg = 0
        if ( this%initialized .eq. 0 ) return
        ! set epfact        
        if ( kstp.eq.1 ) then
          this%epfact = 0.01d0
        else
          this%epfact = 0.10d0
        end if
        ! fill a, x, and b with ac, hnew, and rhs
        call SSPCGU_DCOPY(this%nnz_all, ac, this%a)
        call SSPCGU_DCOPY(this%nia_all, hnew, this%x)
        call SSPCGU_DCOPY(this%nia_all, rhs, this%b)
        ! scale the problem
        if ( this%iscl.ne.0 ) then
          call SPCGU_SCALE(0,this%iscl,this%nia_all,this%nnz_all,this%ia,this%ja, &
                        &  this%a,this%x,this%b,this%dscale,this%dscale2)
        end if
        ! if inactive reordering and/or rkm/md reordering is not used
        ! set ia1, ja1, and a1 to ia, ja, and a
        if ( this%iaord.eq.0 .and. this%iord.eq.0 ) then
          call SSPCGU_ICOPY(this%nia_all, this%ia, this%ia1)
          call SSPCGU_ICOPY(this%nnz_all, this%ja, this%ja1)
          call SSPCGU_DCOPY(this%nnz_all, this%a,  this%a1)
        ! reorder system of equations to move inactive nodes to rows nia+1 to nia_all
        else if ( this%iaord.ne.0 ) then
          IF ( this%iaord_init.EQ.0 .OR. ICELCONV.EQ.1 ) THEN
            this%iacpc = 1
            CALL SSPCGU_CALC_IAORDER(this%nia_all, this%nnz_all, this%ia, this%ja, &
                                  &  this%a,ibound, &
                                  &  this%nia,this%nnz,this%ia0,this%ja0,this%a0, &
                                  &  this%lorder_act,this%iorder_act)
            this%iaord_init = 1
          else
            call SSPCGU_IAPERM(this%nia_all,this%nnz_all,this%ia,this%ja,this%a, &
                              & ibound,this%nia,this%a0,this%iorder_act)
          end if
          CALL VPERM(this%nia_all, this%x, this%lorder_act)  
          CALL VPERM(this%nia_all, this%b, this%lorder_act)
          if ( this%iord.eq.0 ) then
            call SSPCGU_ICOPY(this%nia_all, this%ia0, this%ia1)
            call SSPCGU_ICOPY(this%nnz_all, this%ja0, this%ja1)
            call SSPCGU_DCOPY(this%nnz_all, this%a0,  this%a1)
          else
            ! recalculate lorder and iorder for rcm and l2-norm reordering        
            call SSPCGU_CALC_ORDER( this%iout,this%iord,this%nia,this%nnz,this%ia0,this%ja0, &
                                  &  this%lorder,this%iorder )
            ! permute rows, columns, and rhs after reordering inactive cells
            call DPERM(this%nia,this%a0,this%ja0,this%ia0,this%a1,this%ja1,this%ia1,this%lorder,this%id,1)
            call VPERM(this%nia, this%x, this%lorder)  
            call VPERM(this%nia, this%b, this%lorder)
          end if
        ! permute rows, columns, and rhs
        else
          call DPERM(this%nia,this%a,this%ja,this%ia,this%a1,this%ja1,this%ia1,this%lorder,this%id,1)
          call VPERM(this%nia, this%x, this%lorder)  
          call VPERM(this%nia, this%b, this%lorder)
        end if
        ! recalculate preconditioner iapc and japc
        if ( this%iacpc.ne.0 ) then
          if ( this%ipc.eq.2 .or. this%ipc.eq.3 ) then
            call SPCGU_PCCRS(this%nia,this%nnz,this%ia1,this%ja1, &
                          &  this%iapc,this%japc)
          end if
          this%iacpc = 0
        end if
        ! update preconditioner
        call SPCGU_PCU(this%iout,this%nnz,this%nia, &
                    &  this%niapc,this%njapc,this%nnzapc, &
                    &  this%ipc,this%relax, &
                    &  this%a1,this%ia1,this%ja1, &
                    &  this%apc,this%iapc,this%japc,this%iw,this%w)
        ! initialize solution solver vectors
        if ( kiter.eq.1 ) this%niter = 0
        do n = 1, this%nia
          this%d(n) = dzero
          this%p(n) = dzero
          this%q(n) = dzero
          this%z(n) = dzero
        end do
        ! calculate initial residual
        call SPCGU_MV(this%nnz,this%nia,this%a1,this%x,this%d,this%ia1,this%ja1)
        rmax = dzero
        this%l2norm0 = dzero
        do n = 1, this%nia
          tv     = this%d(n)
          this%d(n) = this%b(n) - tv
          IF ( ABS( this%d(n) ).gt.rmax ) rmax = ABS( this%d(n) )
          this%l2norm0 = this%l2norm0 + this%d(n) * this%d(n)
        END DO
        this%l2norm0 = SQRT(this%l2norm0)
        ! check for exact solution
        itmax = this%maxinner
        IF ( rmax.eq.dzero ) itmax = 0
        ! call the appropriate linear solver method
        ! conjugate gradient
        if ( this%ilinmeth.eq.1 ) then
          call this%cg(icnvg,itmax,innerit)
        ! bi-conjugate gradient stabilized
        else if ( this%ilinmeth.eq.2 ) then
          call this%bcgs(icnvg,itmax,innerit)
        end if
        ! back permute solution
        if ( this%iord.ne.0 ) then
          call VPERM( this%nia, this%x, this%iorder )
        end if
        ! back permute solution if iactive cells reordered
        if ( this%iaord.ne.0 ) then
          call VPERM( this%nia_all, this%x, this%iorder_act )
        end if
        ! unscale problem
        if ( this%iscl.ne.0 ) then
          call SPCGU_SCALE(1,this%iscl,this%nia_all,this%nnz_all,this%ia,this%ja, &
     &                     this%a,this%x,this%b,this%dscale,this%dscale2)
        end if
        ! fill hnew with x
        call SSPCGU_DCOPY(this%nia_all, this%x, hnew)
        ! return
        return
      end subroutine solve

      ! conjugate gradient method
      subroutine cg(this,icnvg,itmax,innerit)
        implicit none
        !+ + + dummy arguments + + +
        class(pcgusolver), intent(inout) :: this
        integer, intent(inout) :: icnvg
        integer, intent(in)    :: itmax
        integer, intent(inout) :: innerit
        !+ + + local definitions + + +
        integer :: n
        integer :: iiter
        doubleprecision :: dhclose, drclose
        doubleprecision :: tv
        doubleprecision :: deltax
        doubleprecision :: rmax
        doubleprecision :: l2norm
        doubleprecision :: rcnvg
        doubleprecision :: alpha, beta
        doubleprecision :: rho, rho0
        doubleprecision :: machprec
        !+ + + parameters + + + 
        doubleprecision, parameter :: dzero = 0.0d0
        doubleprecision, parameter :: done  = 1.0d0
        !+ + + functions + + +
        doubleprecision :: SPCGU_DP
        !+ + + code + + +
        innerit  = 0
        machprec = epsilon( dzero )
        dhclose  = dble( this%hclose )
        drclose  = dble( this%rclose )
        ! inner iteration          
        inner: do iiter = 1, itmax
           innerit = innerit + 1 
           this%niter  = this%niter  + 1 
           ! apply preconditioner
          select case (this%ipc)
            ! no preconditioner
            case (0)
              do n = 1, this%nia
                this%z(n) = this%d(n)
              end do
            ! jacobi preconditioner
            case (1)
              call SPCGU_JACA(this%nia,this%apc,this%d,this%z)
            case (2,3)
              call SPCGU_ILU0A(this%nnz,this%nia,this%apc,this%iapc,this%japc,this%d,this%z)
          end select
          rho = SPCGU_DP( this%nia, this%d, this%z )
          ! compute directional vectors
          if (iiter.eq.1) then
            do n = 1, this%nia
              this%p(n) = this%z(n)
            end do
          else
            beta = rho / rho0
            do n = 1, this%nia
              this%p(n) = this%z(n) + beta * this%p(n)
            end do
          end if
          ! compute iterates
          !  update qc
          call SPCGU_MV(this%nnz,this%nia,this%a1,this%p,this%q,this%ia1,this%ja1)
          alpha = rho / SPCGU_DP( this%nia, this%p, this%q )
          ! update x and residual
          deltax = dzero
          rmax   = dzero
          l2norm = dzero
          do n = 1, this%nia
            tv        = alpha * this%p(n)
            this%x(n) = this%x(n) + tv
            deltax = max( abs(tv), deltax )
            tv      = this%d(n)
            tv      = tv - alpha * this%q(n)
            this%d(n)  = tv
            rmax   = max( abs(tv), rmax )
            l2norm = l2norm + tv * tv
          end do
          l2norm = sqrt(l2norm)
          ! test for solver convergence
          if ( this%icnvgopt.eq.1 ) then
            rcnvg = l2norm
          else
            rcnvg = rmax
          end if
          call SPCGU_TESTCNVG( this%icnvgopt,icnvg, &
     &                         deltax,rcnvg, &
     &                         this%l2norm0,this%epfact,dhclose,drclose )
          ! check for exact solution
          if ( rcnvg.eq.dzero ) icnvg = 1
          if ( icnvg.eq.1 ) exit inner
          ! check that current and previous rho are different
          if ( abs( rho - rho0 ).lt.machprec ) then
            rho0 = rho
            exit inner
          end if
          ! save current inner iterates
          rho0 = rho
        end do inner
        ! return
        return
      end subroutine cg
      
      ! bi-conjugate gradient stabilized method
      subroutine bcgs(this,icnvg,itmax,innerit)
        implicit none
        !+ + + dummy arguments + + +
        class(pcgusolver), intent(inout) :: this
        integer, intent(inout) :: icnvg
        integer, intent(in)    :: itmax
        integer, intent(inout) :: innerit
        !+ + + local definitions + + +
        integer :: n
        integer :: iiter
        doubleprecision :: dhclose, drclose
        doubleprecision :: tv
        doubleprecision :: deltax
        doubleprecision :: rmax
        doubleprecision :: l2norm
        doubleprecision :: rcnvg
        doubleprecision :: alpha, alpha0 
        doubleprecision :: beta
        doubleprecision :: rho, rho0
        doubleprecision :: omega, omega0
        doubleprecision :: machprec
        doubleprecision :: numer, denom
        !+ + + parameters + + + 
        doubleprecision, parameter :: dzero = 0.0d0
        doubleprecision, parameter :: done  = 1.0d0
        doubleprecision, parameter :: dtwo  = 2.0d0
        !+ + + functions + + +
        doubleprecision :: SPCGU_DP
        !+ + + code + + +
        innerit  = 0
        machprec = epsilon( dzero )
        dhclose  = dble( this%hclose )
        drclose  = dble( this%rclose )
        
        alpha = dzero
        beta  = dzero
        rho   = dzero
        rho0  = dzero
        ! save initial residual
        do n = 1, this%nia
          this%dhat(n) = this%d(n)
        end do
        ! inner iteration          
        inner: do iiter = 1, itmax
           innerit = innerit + 1 
           this%niter = this%niter + 1 
           ! calculate rho
          rho = SPCGU_DP( this%nia, this%dhat, this%d )
          ! compute directional vectors
          if (iiter.eq.1) then
            do n = 1, this%nia
              this%p(n) = this%d(n)
            end do
          else
            beta = ( rho / rho0 ) * ( alpha0 / omega0 )
            do n = 1, this%nia
              this%p(n) = this%d(n) + beta * ( this%p(n) - omega * this%v(n) )
            end do
          end if
          ! apply preconditioner to update phat
          select case (this%ipc)
            ! no preconditioner
            case (0)
              do n = 1, this%nia
                this%phat(n) = this%p(n)
              end do
            ! jacobi preconditioner
            case (1)
              call SPCGU_JACA(this%nia,this%apc,this%p,this%phat)
            case (2,3)
              call SPCGU_ILU0A(this%nnz,this%nia,this%apc,this%iapc,this%japc,this%p,this%phat)
          end select
          ! compute iterates
          ! update v with a and phat
          call SPCGU_MV(this%nnz,this%nia,this%a1,this%phat,this%v,this%ia1,this%ja1)
          ! update alpha with dhat and v
          denom = SPCGU_DP( this%nia, this%dhat, this%v )
          alpha = rho / denom
          ! update q
          do n = 1, this%nia
            this%q(n) = this%d(n) - alpha * this%v(n)
          end do
          ! calculate infinity norm of q - test for termination
          ! terminate if rmax is less than machine precision (machprec)
          rmax = dzero
          do n = 1, this%nia
              tv = this%q(n)
              if ( this%iscl.ne.0 ) tv = tv / this%dscale(n)
              if ( abs(tv).gt.abs(rmax) ) rmax = tv
          end do
          if ( abs(rmax).le.machprec ) then
            deltax = dzero
            do n = 1, this%nia
              tv      = alpha * this%phat(n)
              if ( this%iscl.ne.0 ) then
                tv = tv * this%dscale(n)
              end if
              this%x(n)  = this%x(n) + tv
              if ( abs(tv).gt.abs(deltax) ) deltax = tv
            end do
            call SPCGU_TESTCNVG( this%icnvgopt,icnvg, &
     &                           deltax,rmax, &
     &                           rmax,this%epfact,dhclose,drclose )
            if ( icnvg.eq.1 ) exit inner
          end if
          ! apply preconditioner to update qhat
          select case (this%ipc)
            ! no preconditioner
            case (0)
              do n = 1, this%nia
                this%qhat(n) = this%q(n)
              end do
            ! jacobi preconditioner
            case (1)
              call SPCGU_JACA(this%nia,this%apc,this%q,this%qhat)
            ! ilu0 and milu0
            case (2,3)
              call SPCGU_ILU0A(this%nnz,this%nia,this%apc,this%iapc,this%japc,this%q,this%qhat)
          end select
          ! update t with a and qhat
          call SPCGU_MV(this%nnz,this%nia,this%a1,this%qhat,this%t,this%ia1,this%ja1)
          ! update omega
          numer = SPCGU_DP( this%nia, this%t, this%q )
          denom = SPCGU_DP( this%nia, this%t, this%t )
          denom = denom + sign(machprec,denom)
          omega = numer / denom
          ! update x and residual
          deltax = dzero
          rmax   = dzero
          l2norm = dzero
          do n = 1, this%nia
            ! x and dx            
            tv      = alpha * this%phat(n) + omega * this%qhat(n)
            this%x(n)  = this%x(n) + tv
            if ( this%iscl.ne.0 ) then
              tv = tv * this%dscale(n)
            end if
            if ( abs(tv).gt.abs(deltax) ) deltax = tv
            ! residual
            tv      = this%q(n) - omega * this%t(n)
            this%d(n)  = tv
            if ( this%iscl.ne.0 ) then
              tv = tv / this%dscale(n)
            end if
            if ( abs(tv).gt.abs(rmax) ) rmax = tv
            l2norm = l2norm + tv * tv
          end do
          ! test for solver convergence
          if ( this%icnvgopt.eq.1 ) then
            rcnvg = l2norm
          else
            rcnvg = rmax
          end if
          call SPCGU_TESTCNVG( this%icnvgopt,icnvg, &
     &                         deltax,rcnvg, &
     &                         this%l2norm0,this%epfact,dhclose,drclose )
          ! check for exact solution
          if ( rcnvg.eq.dzero ) icnvg = 1
          if ( icnvg.eq.1 ) exit inner
          ! check that current and previous rho, alpha, and omega are different
          if ( abs( rho - rho0 ).lt.machprec ) then
            rho0 = rho
            exit inner
          end if
          if ( abs( alpha - alpha0 ).lt.machprec ) then
            alpha0 = alpha
            exit inner
          end if
          if ( abs( omega - omega0 ).lt.machprec ) then
            omega0 = omega
            exit inner
          end if
          ! save current inner iterates
          rho0   = rho
          alpha0 = alpha
          omega0 = omega
        end do inner
        ! return
        return
      end subroutine bcgs
      
      ! integer functions to get a, x, and b data from instance of pcgu solver class
      ! get the vector specified by vflag      
      function getv(this, vflag)
        implicit none
        class(pcgusolver), intent(inout) :: this
        character (len=1), intent(in) :: vflag
        doubleprecision, dimension(this%nia_all) :: getv
        if ( this%initialized .eq. 0 ) return
        select case ( vflag )
          case ( 'x', 'X' )
            call getg( this%nia_all, this%x, getv )
          case ( 'b', 'B' )
            call getg( this%nia_all, this%b, getv )
        end select
        ! return
        return
      end function getv

      ! generic subroutine that actually gets the dummy vector g      
      subroutine getg(nv, g, v)
        implicit none
        integer, intent(in) :: nv
        doubleprecision, dimension(nv), intent(in) :: g
        doubleprecision, dimension(nv), intent(inout) :: v
        ! local variables
        integer :: n
        ! code
        do n = 1, nv
          v(n) = g(n)
        end do
        ! return
        return
      end subroutine getg

      ! get the A matrix
      function geta(this)
        implicit none
        class(pcgusolver), intent(inout) :: this
        ! local variables
        integer :: n
        doubleprecision, dimension(this%nnz_all) :: geta
        if ( this%initialized .eq. 0 ) return
        ! code
        do n = 1, this%nnz_all
          geta(n) = this%a(n)
        end do
        ! return
        return
      end function geta
      
      ! routines to write matrix data in Matrix Market format
      ! write the vector specified by vflag      
      subroutine writev(this,fname,vflag)
        implicit none
        class(pcgusolver), intent(in) :: this
        character (len=*), intent(in) :: fname
        character (len=1), intent(in) :: vflag
        ! local
        integer :: iunit
        integer :: i,j
        integer :: i0, i1, jcol
        ! code
        if ( this%initialized .eq. 0 ) return
        select case ( vflag )
          case ( 'x', 'X' )
            call writeg( fname, this%nia, this%x )
          case ( 'b', 'B' )
            call writeg( fname, this%nia, this%b )
        end select
        ! return
        return
      end subroutine writev
      
      ! generic subroutine that actually writes the dummy vector g in Matrix Market format
      subroutine writeg(fname,nv,v)
        implicit none
        character (len=*), intent(in) :: fname
        integer, intent(in) :: nv
        doubleprecision, dimension(nv), intent(in) :: v
        ! local
        integer :: iunit
        integer :: i
        ! code
        open (newunit=iunit, file=fname)
        write (iunit,*) '%%matrixmarket matrix coordinate real general'
        write (iunit,*) nv,1
        do i = 1, nv
          write (iunit,*) v(i)
        end do
        close (unit=iunit)
        ! return
        return
      end subroutine writeg
      
      ! subroutine that writes the A matrix in Matrix Market format
      subroutine writea(this,fname)
        implicit none
        class(pcgusolver), intent(in) :: this
        character (len=*), intent(in) :: fname
        ! local
        integer :: iunit
        integer :: i,j
        integer :: i0, i1, jcol
        ! code
        if ( this%initialized .eq. 0 ) return
        open (newunit=iunit, file=fname)
        write (iunit,*) '%%matrixmarket matrix coordinate real general'
        write (iunit,*) this%nia,this%nia,this%nnz
        do i = 1, this%nia
          i0 = this%ia(i)
          i1 = this%ia(i+1)-1
          do j = i0, i1
            jcol = this%ja(j)
            write (iunit,*) i,jcol,this%a(j)
          end do
        end do
        close (unit=iunit)
        ! return
        return
      end subroutine writea
      
      ! subroutine that writes the APC matrix in Matrix Market format
      subroutine writeapc(this,fname)
        implicit none
        class(pcgusolver), intent(in) :: this
        character (len=*), intent(in) :: fname
        ! local
        integer :: iunit
        integer :: i,j
        integer :: i0, i1, jcol
        ! code
        if ( this%initialized .eq. 0 ) return
        if ( this%ipc .eq. 0 ) return
        open (newunit=iunit, file=fname)
        write (iunit,*) '%%matrixmarket matrix coordinate real general'
        select case ( this%ipc )
          case (1)
            write (iunit,*) this%nia,1
            do i = 1, this%nia
              write (iunit,*) this%apc(i)
            end do
          case (2,3)
            write (iunit,*) this%niapc,this%niapc,this%nnzapc
            do i = 1, this%niapc
              write (iunit,*) i,i,this%apc(i)
              i0 = this%iapc(i)
              i1 = this%iapc(i+1)-1
              do j = i0, i1
                jcol = this%japc(j)
                write (iunit,*) i, jcol, this%apc(j)
              end do
            end do
        end select
        close (unit=iunit)
        ! return
        return
      end subroutine writeapc
    
end module MPCGUClass