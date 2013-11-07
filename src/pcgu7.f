      MODULE PCGUMODULE
        INTEGER  :: ILINMETH
        INTEGER  :: ITER1C,IPC,ISCL,IORD,ICNVGOPT
        INTEGER  :: IAORD, IAORD_INIT
        INTEGER  :: IACPC
        INTEGER  :: NITERC,NNZC,NIAC,NNZ_ALL,NIA_ALL
        INTEGER  :: NIABCGS
        INTEGER  :: NIAPC,NJAPC,NNZAPC
        REAL   :: HCLOSEPCGU,RCLOSEPCGU
        REAL   :: RELAXPCGU
        DOUBLEPRECISION :: EPFACT,L2NORM0
        
        DOUBLEPRECISION, ALLOCATABLE, DIMENSION(:)         :: DSCALE
        DOUBLEPRECISION, ALLOCATABLE, DIMENSION(:)         :: DSCALE2
        INTEGER,          ALLOCATABLE, DIMENSION(:)        :: IAPC
        INTEGER,          ALLOCATABLE, DIMENSION(:)        :: JAPC
        DOUBLEPRECISION, ALLOCATABLE, DIMENSION(:)         :: APC
        INTEGER,          ALLOCATABLE, DIMENSION(:)        :: LORDER_ACT
        INTEGER,          ALLOCATABLE, DIMENSION(:)        :: IORDER_ACT
        INTEGER,          ALLOCATABLE, DIMENSION(:)        :: LORDER
        INTEGER,          ALLOCATABLE, DIMENSION(:)        :: IORDER
        INTEGER,          ALLOCATABLE, DIMENSION(:),TARGET :: IARO_ACT
        INTEGER,          ALLOCATABLE, DIMENSION(:),TARGET :: JARO_ACT
        DOUBLEPRECISION,  ALLOCATABLE, DIMENSION(:),TARGET :: ARO_ACT
        INTEGER,          ALLOCATABLE, DIMENSION(:),TARGET :: IARO
        INTEGER,          ALLOCATABLE, DIMENSION(:),TARGET :: JARO
        DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:),TARGET :: ARO
C         WORKING ARRAYS        
        INTEGER,         ALLOCATABLE, DIMENSION(:)         :: IW
        DOUBLEPRECISION, ALLOCATABLE, DIMENSION(:)         :: W
        INTEGER,         ALLOCATABLE, DIMENSION(:)         :: ID
        DOUBLEPRECISION, ALLOCATABLE, DIMENSION(:)         :: X
        DOUBLEPRECISION, ALLOCATABLE, DIMENSION(:)         :: D
        DOUBLEPRECISION, ALLOCATABLE, DIMENSION(:)         :: P
        DOUBLEPRECISION, ALLOCATABLE, DIMENSION(:)         :: Q
        DOUBLEPRECISION, ALLOCATABLE, DIMENSION(:)         :: Z
C         BICGSTAB WORKING ARRAYS
        DOUBLEPRECISION, ALLOCATABLE, DIMENSION(:)         :: T
        DOUBLEPRECISION, ALLOCATABLE, DIMENSION(:)         :: V
        DOUBLEPRECISION, ALLOCATABLE, DIMENSION(:)         :: DHAT
        DOUBLEPRECISION, ALLOCATABLE, DIMENSION(:)         :: PHAT
        DOUBLEPRECISION, ALLOCATABLE, DIMENSION(:)         :: QHAT
C         POINTERS FOR USE WITH BOTH ORIGINAL AND RCM ORDERINGS
        INTEGER,          POINTER, DIMENSION(:)            :: IA0
        INTEGER,          POINTER, DIMENSION(:)            :: JA0
        DOUBLEPRECISION,  POINTER, DIMENSION(:)            :: A0
      END MODULE PCGUMODULE


      SUBROUTINE PCGU7U1AR(IN, NJA, NEQS, MXITER, HICLOSE, ITER1, 
     1   IFDPARAM,IPCGUM)
C     ******************************************************************
C     ALLOCATE STORAGE FOR PCG ARRAYS AND READ PCGU DATA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY:IOUT, IA, JA
      USE PCGUMODULE
      IMPLICIT NONE
C     + + + DUMMY VARIABLES + + +
      INTEGER, INTENT(IN) :: IN
      INTEGER, INTENT(IN) :: NJA
      INTEGER, INTENT(IN) :: NEQS
      INTEGER, INTENT(IN) :: MXITER
      DOUBLE PRECISION, INTENT(IN) :: HICLOSE
      INTEGER, INTENT(IN) :: ITER1
      INTEGER, INTENT(IN) :: IFDPARAM
      INTEGER, INTENT(INOUT) :: IPCGUM
C     + + + LOCAL VARIABLES + + +
      CHARACTER*200 LINE
      CHARACTER (LEN= 10) :: clin(1:2)
      CHARACTER (LEN= 20) :: cipc(0:3)
      CHARACTER (LEN= 20) :: cscale(0:2)
      CHARACTER (LEN= 25) :: corder(0:2)
      CHARACTER (LEN=16), DIMENSION(2) :: ccnvgopt
      CHARACTER (LEN=  4) :: cval
      INTEGER :: lloc
      INTEGER :: istart, istop
      INTEGER :: i, n
      INTEGER :: i0
      INTEGER :: iscllen, iolen
      REAL :: r
C     + + + PARAMETERS + + + 
      INTEGER, PARAMETER :: IZERO = 0
      REAL, PARAMETER :: RZERO = 0.0
      DOUBLE PRECISION, PARAMETER :: DZERO = 0.0D0
      DOUBLE PRECISION, PARAMETER :: DONE  = 1.0D0
C       DATA
      DATA clin  /'CG        ',
     2            'BCGS      '/
      DATA cipc  /'NONE                ',
     2            'JACOBI              ',
     3            'INCOMPLETE LU       ',
     4            'MOD. INCOMPLETE LU  '/
      DATA cscale/'NO SCALING          ',
     2            'SYMMETRIC SCALING   ',
     3            'L2 NORM SCALING     '/
      DATA corder/'ORIGINAL ORDERING        ',
     2            'RCM ORDERING             ',
     3            'MINIMUM DEGREE ORDERING  '/
      DATA ccnvgopt  /'INFINITY NORM   ',
     2                'L2 NORM         '/
C       OUTPUT FORMATS
02010 FORMAT (1X,/,14X,'SOLUTION BY THE CONJUGATE-GRADIENT METHOD',
     &        /,1X,66('-'),/,
     &        ' MAXIMUM OF ',I6,' CALLS OF SOLUTION ROUTINE',/,
     &        ' MAXIMUM OF ',I6,
     &        ' INTERNAL ITERATIONS PER CALL TO SOLUTION ROUTINE',/,
     &        ' LINEAR ACCELERATION METHOD            =',1X,A,/,
     &        ' MATRIX PRECONDITIONING TYPE           =',1X,A,/,
     &        ' MATRIX SCALING APPROACH               =',1X,A,/,
     &        ' MATRIX REORDERING APPROACH            =',1X,A,/,
     &        ' REORDER INACTIVE NODES                =',1X,L1,/,
     &        ' HEAD CHANGE CRITERION FOR CLOSURE     =',E15.5,/,
     &        ' RESIDUAL CHANGE CRITERION FOR CLOSURE =',E15.5,/,
     &        ' RESIDUAL CONVERGENCE OPTION           =',I9,/,
     &        ' RESIDUAL CONVERGENCE NORM             =',1X,A,/,
     &        ' RELAXATION FACTOR                     =',E15.5,/,
     &        '  ONLY USED WITH MILU0 PRECONDITIONER',//)
02020 FORMAT (///,1X,'PCGU DATA INPUT ERROR:',
     &          /,2X,'SCALING MUST BE USED (ISCL.GT.0) IF USING',
     &          /,2X,'THE ILU0 OR MILU0 PRECONDITIONERS (IPC.EQ.2 OR',
     &          /,2X,'IPC.EQ.3) WITH MATRIX REORDERING (IORD.GT.0)')
2030  FORMAT(1X,A20,1X,6(I6,1X))
2040  FORMAT(1X,20('-'),1X,6(6('-'),1X))
2050  FORMAT(1X,62('-'),/)
C     ------------------------------------------------------------------
C
C-------TRANSFER COMMON VARIABLES FROM SMS TO UPCG
      ILINMETH = 0
      NNZC = NJA
      NIAC = NEQS
      NNZ_ALL = NJA
      NIA_ALL = NEQS
      
      HCLOSEPCGU = HICLOSE
      ITER1C = ITER1
      IAORD = 0
      IAORD_INIT = 0
      IACPC = 1
      
      ICNVGOPT = 0
C
C-------PRINT A MESSAGE IDENTIFYING UPCG PACKAGE
      WRITE (IOUT,2000)
02000 FORMAT (1X,/1X,'PCGU -- UNSTRUCTURED CONJUGATE-GRADIENT SOLUTION',
     &        ' PACKAGE, VERSION 7.02, 08/13/2013')
C
C-------READ AND PRINT COMMENTS
      CALL URDCOM(IN,IOUT,LINE)
      IF ( IFDPARAM.EQ.0 ) THEN
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,IPC,R,IOUT,IN)
        cval = LINE(ISTART:ISTOP)
        SELECT CASE (cval)
          CASE ( 'CG' )
            ILINMETH = 1
          CASE ( 'BCGS' )
            ILINMETH = 2
          CASE DEFAULT
            ILINMETH = 1
            READ (CVAL,*) IPC
        END SELECT
        IF ( cval.EQ.'CG  ' .OR. cval.EQ.'BCGS' ) THEN
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IPC,R,IOUT,IN)
        END IF
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISCL,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IORD,R,IOUT,IN)
        IF ( IORD.GE.10 ) THEN
          IORD  = IORD - 10
          IAORD = 1
        END IF
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,RCLOSEPCGU,IOUT,IN)
        IF ( RCLOSEPCGU.LT.RZERO ) THEN
          RCLOSEPCGU = ABS( RCLOSEPCGU )
          ICNVGOPT = 1
        END IF
        IF ( IPC.EQ.3 ) THEN
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,RELAXPCGU,-IOUT,IN)
          IF ( ISTART.EQ.200 ) THEN
            IF ( ISTOP.EQ.200 ) RELAXPCGU = 0.97
          END IF
          IF ( RELAXPCGU.EQ.RZERO ) THEN
            IF ( LINE(200:200).EQ.'E' ) RELAXPCGU = 0.97
          END IF
        ELSE
          RELAXPCGU = 0.0
        END IF
      ELSE
        CALL SET_PCGUINPUT(IFDPARAM)
      END IF
      IPCGUM = ILINMETH
C
C-------ERROR CHECKING FOR OPTIONS
      IF ( IPC.LT.0  ) IPC  = 0
      IF ( IPC.GT.3  ) THEN
        WRITE( IOUT,'(A)' ) 'PCGU7AR: IPC  MUST BE .LE. 3'
        CALL USTOP('PCGU7AR: IPC  MUST BE .LE. 3')
      END IF
      IF ( ISCL.LT.0 ) ISCL = 0
      IF ( ISCL.GT.2  ) THEN
        WRITE( IOUT,'(A)' ) 'PCGU7AR: ISCL MUST BE .LE. 2'
        CALL USTOP('PCGU7AR: ISCL MUST BE .LE. 2')
      END IF
      IF ( IORD.LT.0 ) IORD = 0
      IF ( IORD.GT.2  ) THEN
        WRITE( IOUT,'(A)' ) 'PCGU7AR: IORD MUST BE .LE. 2'
        CALL USTOP('PCGU7AR: IORD MUST BE .LE. 2')
      END IF
      IF ( RCLOSEPCGU.EQ.0.0 ) THEN
        WRITE( IOUT,'(A)' ) 'PCGU7AR: RCLOSEPCGU MUST .NE. 0.0'
        CALL USTOP('PCGU7AR: RCLOSEPCGU MUST .NE. 0.0')
      END IF
      IF ( RELAXPCGU.LT.0.0 ) THEN
        WRITE( IOUT,'(A)' ) 'PCGU7AR: RELAXPCGU MUST BE .GE. 0.0'
        CALL USTOP('PCGU7AR: RELAXPCGU MUST BE .GE. 0.0')
      END IF
      IF ( RELAXPCGU.GT.1.0 ) THEN
        WRITE( IOUT,'(A)' ) 'PCGU7AR: RELAXPCGU MUST BE .LE. 1.0'
        CALL USTOP('PCGU7AR: RELAXPCGU MUST BE .LE. 1.0')
      END IF
C
C-------PRINT MXITER,ITER1C,IPC,ISCL,IORD,HCLOSEPCGU,RCLOSEPCGU
      WRITE (IOUT,2010) MXITER, ITER1C, 
     2                   clin(ILINMETH), cipc(IPC), 
     3                   cscale(ISCL), corder(IORD), (IAORD.GT.0),
     4                   HCLOSEPCGU, RCLOSEPCGU, 
     5                   ICNVGOPT,ccnvgopt(ICNVGOPT+1),
     6                   RELAXPCGU
C
C-------ENSURE THAT SCALING IS USED WITH THE ILU0 AND MILU0
C       PRECONDITIONERS IF RCM OR MINIMUM DEGREE ORDERING IS USED
      IF ( IPC.EQ.2 .OR. IPC.EQ.3 ) THEN
        IF ( IORD.NE.0 ) THEN
          IF ( ISCL.EQ.0 ) THEN
            WRITE ( IOUT,2020 )
            CALL USTOP('SCALING MUST BE USED FOR ILU0 AND MILU0 '//
     2                 'WITH REORDERING')
          END IF
        END IF
      END IF
C
C-------INITIALIZE PCGU VARIABLES
      NITERC = 0
C
C-------ALLOCATE AND INITIALIZE MEMORY FOR PCGU
      iscllen  = 1
      IF ( ISCL.NE.0 ) iscllen  = NIAC
      ALLOCATE ( DSCALE(iscllen), DSCALE2(iscllen) )
C       ALLOCATE MEMORY FOR PRECONDITIONING MATRIX
      NIAPC  = NIAC
      NJAPC  = NNZC
      NNZAPC = NNZC
      IF ( IPC.EQ.0 ) THEN
        NIAPC  = 1
        NJAPC  = 1
        NNZAPC = 1
      ELSE IF ( IPC.EQ.1 ) THEN
        NIAPC  = 1
        NJAPC  = 1
        NNZAPC = NNZC
      END IF
      ALLOCATE( IAPC(NIAPC+1) )
      ALLOCATE( JAPC(NJAPC) )
      ALLOCATE( APC(NNZAPC) )
C       ALLOCATE MEMORY FOR ILU0 AND MILU0 NON-ZERO ROW ENTRY VECTOR
      ALLOCATE( IW(NIAPC) )
      ALLOCATE( W(NIAPC) )
C       GENERATE IAPC AND JAPC
      IF ( IPC.EQ.2 .OR. IPC.EQ.3 ) THEN
        CALL SPCGU_PCCRS(NIAC,NNZC,IA,JA,
     2                   IAPC,JAPC)
      END IF
C
      i0     = 1
      iolen  = 1
      IF ( IAORD.NE.0 ) THEN
        i0     = NIAC
        iolen  = NNZC
      END IF
      ALLOCATE( LORDER_ACT(i0)  )
      ALLOCATE( IORDER_ACT(i0)  )
      ALLOCATE( IARO_ACT(i0+1)  )
      ALLOCATE( JARO_ACT(iolen) )
      ALLOCATE( ARO_ACT(iolen)  )
      DO n = 1, i0 + 1
        IARO_ACT(n) = IZERO
      END DO
      DO n = 1, iolen
        JARO_ACT(n) = IZERO
        ARO_ACT(n)  = DZERO
      END DO
C       ALLOCATE SPACE FOR PERMUTATION VECTOR
      i0     = 1
      iolen  = 1
      IF ( IORD.NE.0 ) THEN
        i0     = NIAC
        iolen  = NNZC
      END IF
      ALLOCATE( LORDER(i0)  )
      ALLOCATE( IORDER(i0)  )
      ALLOCATE( IARO(i0+1)  )
      ALLOCATE( JARO(iolen) )
      ALLOCATE( ARO(iolen)  )
C       ALLOCATE WORKING VECTORS FOR PCGU SOLVER      
      ALLOCATE( ID(NIAC) )
      ALLOCATE( X(NIAC) )
      ALLOCATE( D(NIAC) )
      ALLOCATE( P(NIAC) )
      ALLOCATE( Q(NIAC) )
      ALLOCATE( Z(NIAC))
C       ALLOCATE MEMORY FOR BCGS WORKING ARRAYS
      NIABCGS = 1
      IF ( ILINMETH.EQ.2 ) THEN
        NIABCGS = NIAC
      END IF
      ALLOCATE( T(NIABCGS) )
      ALLOCATE( V(NIABCGS) )
      ALLOCATE( DHAT(NIABCGS) )
      ALLOCATE( PHAT(NIABCGS) )
      ALLOCATE( QHAT(NIABCGS) )
C       INITALIZE PCGU VECTORS
      DO n = 1, iscllen
        DSCALE(n)  = DONE
        DSCALE2(n) = DONE
      END DO
      DO n = 1, NNZAPC
        APC(n)  = DZERO
      END DO
C       WORKING VECTORS
      DO n = 1, NIAC
        ID(n)    = IZERO
        X(n)    = DZERO
        D(n)    = DZERO
        P(n)    = DZERO
        Q(n)    = DZERO
        Z(n)    = DZERO
      END DO
      DO n = 1, NIAPC
        IW(n)   = IZERO
        W(n)    = DZERO
      END DO
C       BCGS WORKING VECTORS
      DO n = 1, NIABCGS
        T(n)    = DZERO
        V(n)    = DZERO
        DHAT(n) = DZERO
        PHAT(n) = DZERO
        QHAT(n) = DZERO
      END DO
C-------REORDERING VECTORS
      !DO n = 1, i0
      !  LORDER(n) = IZERO
      !  IORDER(n) = IZERO
      !END DO
      DO n = 1, i0 + 1
        IARO(n) = IZERO
      END DO
      DO n = 1, iolen
        JARO(n) = IZERO
        ARO(n)  = DZERO
      END DO
C
C-------REVERSE CUTHILL MCKEE ORDERING
C       NOTE - USING GNRCM AND ODRV SUBROUTINES IN THE XMD SOLVER SOURCE CODE
C              SPECIFICALLY IN xmblib.f
      IF ( IORD.NE.0 ) THEN
        CALL SSPCGU_CALC_ORDER( IOUT,IORD,NIAC,NNZC,IA,JA,
     2                          LORDER,IORDER )
      END IF
!C
!C-------SET POINTERS FOR GRID
!      CALL PCGU7PSV(IGRID)
C
C-------RETURN
      RETURN
      END SUBROUTINE PCGU7U1AR
C
      SUBROUTINE PCGU7U1AP(AC,RHS,HNEW,IA,JA,
     &                     ICNVG,KSTP,KITER,IN_ITER,IOUT)
C
C     ******************************************************************
C     SOLUTION BY THE CONJUGATE GRADIENT METHOD -
C                                          UP TO ITER1 ITERATIONS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY: ICELCONV,IBOUND
      USE PCGUMODULE
      IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
      DOUBLEPRECISION, TARGET, DIMENSION(NNZ_ALL), INTENT(INOUT) :: AC
      DOUBLEPRECISION, DIMENSION(NIA_ALL), INTENT(INOUT)         :: RHS
      DOUBLEPRECISION, DIMENSION(NIA_ALL), INTENT(INOUT)         :: HNEW
      INTEGER, TARGET,  DIMENSION(NIA_ALL+1),INTENT(IN)          :: IA
      INTEGER, TARGET,  DIMENSION(NNZ_ALL),INTENT(IN)            :: JA
      INTEGER, INTENT(INOUT)                                  :: ICNVG
      INTEGER, INTENT(IN)                                     :: KSTP
      INTEGER, INTENT(IN)                                     :: KITER
      INTEGER, INTENT(INOUT)                                  :: IN_ITER
      INTEGER, INTENT(IN)                                     :: IOUT
C     + + + LOCAL DEFINITIONS + + +
      INTEGER :: n
      INTEGER :: innerit
      INTEGER :: irc
      INTEGER :: itmax
      DOUBLEPRECISION :: tv
      DOUBLEPRECISION :: rmax
C     + + + PARAMETERS + + + 
      DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
      DOUBLEPRECISION, PARAMETER :: DONE  = 1.0D0
C     + + + FUNCTIONS + + +
C
C     + + + CODE + + +
C
C-------SET EPFACT BASED ON MFUSG TIMESTEP
      IF ( KSTP.EQ.1 ) THEN
        EPFACT = 0.01
      ELSE
        EPFACT = 0.10
      END IF
C
C-------FILL X WITH HNEW
      DO n = 1,NIA_ALL
        X(n) = HNEW(n)
      ENDDO

C-------SCALE PROBLEM
      IF ( ISCL.NE.0 ) THEN
        CALL SPCGU_SCALE(0,ISCL,NIA_ALL,NNZ_ALL,IA,JA,AC,X,RHS,
     2                   DSCALE,DSCALE2)
      END IF
C
C-------REORDER SYSTEM OF EQUATIONS TO MOVE INACTIVE NODES TO ROWS 
C       NIAC+1 TO NIA_ALL
      IF ( IAORD.NE.0 ) THEN
        IF ( IAORD_INIT.EQ.0 .OR. ICELCONV.EQ.1 ) THEN
          IACPC = 1
!          WRITE(IOUT,'(/1XA/)') 
!     2      'RECALCULATING REORDERED IA AND JA FOR ACTIVE CELLS'
          CALL SSPCGU_CALC_IAORDER(NIA_ALL,NNZ_ALL,IA,JA,
     2                             AC,IBOUND,
     3                             NIAC,NNZC,IARO_ACT,JARO_ACT,ARO_ACT,
     4                             LORDER_ACT,IORDER_ACT)
          IAORD_INIT = 1
        ELSE
          CALL SSPCGU_IAPERM(NIA_ALL,NNZ_ALL,IA,JA,AC,
     2                      IBOUND,NIAC,ARO_ACT,IORDER_ACT)
        END IF
        CALL VPERM(NIA_ALL,  X, LORDER_ACT)  
        CALL VPERM(NIA_ALL, RHS, LORDER_ACT)
        IA0 => IARO_ACT
        JA0 => JARO_ACT
        A0  => ARO_ACT
C         RECALCULATE LORDER AND IORDER FOR RCM AND L2-NORM REORDERING        
        IF ( IORD.NE.0 ) THEN
          CALL SSPCGU_CALC_ORDER( IOUT,IORD,NIAC,NNZC,IA0,JA0,
     2                            LORDER,IORDER )
        END IF
C        
      ELSE
        IA0 => IA
        JA0 => JA
        A0  => AC
      END IF
C
C-------PERMUTE ROWS, COLUMNS, AND RHS
      IF ( IORD.NE.0 ) THEN
        CALL DPERM(NIAC,A0,JA0,IA0,ARO,JARO,IARO,LORDER,ID,1)
        CALL VPERM(NIAC,  X, LORDER)  
        CALL VPERM(NIAC, RHS, LORDER)
        IA0 => IARO
        JA0 => JARO
        A0  => ARO
!        IF ( IPC.EQ.2 .OR. IPC.EQ.3 ) THEN
!          CALL SPCGU_PCCRS(NIAC,NNZC,IARO,JARO,
!     2                     IAPC,JAPC)
!        END IF
      END IF
C
C------CALCULATE PRECONDITIONER IAPC AND JAPC
      IF ( IACPC.NE.0 ) THEN
        IF ( IPC.EQ.2 .OR. IPC.EQ.3 ) THEN
          CALL SPCGU_PCCRS(NIAC,NNZC,IA0,JA0,
     2                     IAPC,JAPC)
        END IF
        IACPC = 0
      END IF
C
C-------UPDATE PRECONDITIONER
      CALL SPCGU_PCU(IOUT,NNZC,NIAC,NIAPC,NJAPC,NNZAPC,IPC,RELAXPCGU,
     2               A0,IA0,JA0,APC,IAPC,JAPC,IW,W)
C-------INITILIZE SOLUTION VARIABLE AND ARRAYS
      IF ( KITER.EQ.1 ) NITERC = 0
      irc    = 1
      ICNVG  = 0
      DO n = 1, NIAC
        D(n) = DZERO
        P(n) = DZERO
        Q(n) = DZERO
        Z(n) = DZERO
      END DO
C-------CALCULATE INITIAL RESIDUAL
      CALL SPCGU_MV(NNZC,NIAC,A0,X,D,IA0,JA0)
      rmax = DZERO
      L2NORM0 = DZERO
      DO n = 1, NIAC
        tv     = D(n)
        D(n) = RHS(n) - tv
        IF ( ABS( D(n) ).GT.rmax ) rmax = ABS( D(n) )
        L2NORM0 = L2NORM0 + D(n) * D(n)
      END DO
      L2NORM0 = SQRT(L2NORM0)
C-------CHECK FOR EXACT SOLUTION
      itmax = ITER1C
      IF ( rmax.EQ.DZERO ) itmax = 0
C-------SOLUTION BY THE CONJUGATE GRADIENT METHOD      
      IF ( ILINMETH.EQ.1 ) THEN
        CALL SPCGU_CG(ICNVG,itmax,innerit)
C-------SOLUTION BY THE BICONJUGATE GRADIENT STABILIZED METHOD      
      ELSE IF ( ILINMETH.EQ.2 ) THEN
        CALL SPCGU_BCGS(ICNVG,itmax,innerit)
      END IF
C
C-------BACK PERMUTE SOLUTION AND RHS
      IF ( IORD.NE.0 ) THEN
        CALL VPERM(NIAC,  X, IORDER)  
        CALL VPERM(NIAC, RHS, IORDER)  
      END IF
C
C-------BACK PERMUTE SOLUTION AND RHS
      IF ( IAORD.NE.0 ) THEN
        CALL VPERM(NIA_ALL,  X, IORDER_ACT)  
        CALL VPERM(NIA_ALL, RHS, IORDER_ACT)  
      END IF
C
C-------UNSCALE PROBLEM
      IF ( ISCL.NE.0 ) THEN
        CALL SPCGU_SCALE(1,ISCL,NIA_ALL,NNZ_ALL,IA,JA,AC,X,RHS,
     2                   DSCALE,DSCALE2)
      END IF
C
C-------FILL HNEW WITH NEW ESTIMATE
      DO n = 1, NIA_ALL
        HNEW(n) = X(n)
      END DO
C
C-------SET SMS INNER ITERATION NUMBER (IN_ITER) TO NUMBER OF 
C       PCGU INNER ITERATIONS (innerit)
      IN_ITER = innerit
C
C-------RETURN
      RETURN
C
      END SUBROUTINE PCGU7U1AP
C
C
      SUBROUTINE PCGU7DA()
C  Deallocate PCGU DATA
      USE PCGUMODULE
      IMPLICIT NONE
C     + + + CODE + + +
        DEALLOCATE(DSCALE)
        DEALLOCATE(DSCALE2)
        DEALLOCATE(IAPC)
        DEALLOCATE(JAPC)
        DEALLOCATE(APC)
        DEALLOCATE(LORDER_ACT)
        DEALLOCATE(IORDER_ACT)
        DEALLOCATE(IARO_ACT)
        DEALLOCATE(JARO_ACT)
        DEALLOCATE(ARO_ACT)
        DEALLOCATE(LORDER)
        DEALLOCATE(IORDER)
        DEALLOCATE(IARO)
        DEALLOCATE(JARO)
        DEALLOCATE(ARO)
C       WORKING ARRAYS
        DEALLOCATE(IW)
        DEALLOCATE(W)
        DEALLOCATE(ID)
        DEALLOCATE(X)
        DEALLOCATE(D)
        DEALLOCATE(P)
        DEALLOCATE(Q)
        DEALLOCATE(Z)
C       BICGSTAB WORKING ARRAYS
        DEALLOCATE(T)
        DEALLOCATE(V)
        DEALLOCATE(DHAT)
        DEALLOCATE(PHAT)
        DEALLOCATE(QHAT)
C
C-------RETURN
      RETURN
      END SUBROUTINE PCGU7DA
C      
C
C-------ROUTINE TO CALCULATE LORDER AND IORDER FOR
C       ELIMINATING INACTIVE CELLS FROM THE MATRIX
C       INACTIVE CELLS ARE MOVED TO THE END OF THE
C       MATRIX
C
      SUBROUTINE SSPCGU_CALC_IAORDER(NIA_ALL,NNZ_ALL,
     2                              IAC_ALL,JAC_ALL,AC,IBOUND,
     3                              NIAC,NNZC,
     4                              IARO_ACT,JARO_ACT,ARO_ACT,
     4                              LORDER_ACT,IORDER_ACT)
      IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN)    :: NIA_ALL
        INTEGER, INTENT(IN)    :: NNZ_ALL
        INTEGER, DIMENSION(NIA_ALL+1), INTENT(IN) :: IAC_ALL
        INTEGER, DIMENSION(NNZ_ALL), INTENT(IN)   :: JAC_ALL
        DOUBLE PRECISION, DIMENSION(NNZ_ALL), INTENT(IN)   :: AC
        INTEGER, DIMENSION(NIA_ALL), INTENT(IN)   :: IBOUND
        INTEGER, INTENT(INOUT) :: NIAC
        INTEGER, INTENT(INOUT) :: NNZC
        INTEGER, DIMENSION(NIA_ALL+1), INTENT(INOUT) :: IARO_ACT
        INTEGER, DIMENSION(NNZ_ALL), INTENT(INOUT)   :: JARO_ACT
        DOUBLE PRECISION, DIMENSION(NNZ_ALL), INTENT(INOUT) :: ARO_ACT
        INTEGER, DIMENSION(NIA_ALL), INTENT(INOUT) :: LORDER_ACT
        INTEGER, DIMENSION(NIA_ALL), INTENT(INOUT) :: IORDER_ACT
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: j, n
        INTEGER :: i0, i1
        INTEGER :: jcol
        INTEGER :: jt, nt
        INTEGER :: icnt
C     + + + FUNCTIONS + + +
C     + + + FORMATS + + +
C     + + + CODE + + +
        icnt = 0
        DO n = 1, NIA_ALL
        	IF ( IBOUND(n).NE.0 ) THEN
        		icnt = icnt + 1
        		IORDER_ACT(icnt) = n
        	END IF
        END DO
        NIAC = icnt
        IF ( icnt.LT.NIA_ALL ) THEN
          DO n = 1, NIA_ALL
        	  IF ( IBOUND(n).EQ.0 ) THEN
        		  icnt = icnt + 1
        		  IORDER_ACT(icnt) = n
            END IF
          END DO
        END IF
C
C         GENERATE INVERSE OF IORDER_ACT
        DO n = 1, NIA_ALL
          LORDER_ACT( IORDER_ACT(n) ) = n
        END DO
C---------CREATE IARO_ACT, JARO_ACT, AND ARO_ACT
        IARO_ACT(1) = 1
        icnt = 0
        DO n = 1, NIA_ALL
          nt = IORDER_ACT(n)
          i0 = IAC_ALL(nt)
          i1 = IAC_ALL(nt+1) - 1
C           DIAGONAL          
          icnt = icnt + 1
          JARO_ACT(icnt) = n
          ARO_ACT(icnt)  = AC(i0)
          IF ( IBOUND(nt).NE.0 ) THEN
            DO j = i0+1, i1
              jcol = JAC_ALL(j)
              jt   = LORDER_ACT(jcol)
              IF ( IBOUND(jcol).EQ.0 ) THEN
                CYCLE
              END IF
              icnt = icnt + 1
              JARO_ACT(icnt) = jt
              ARO_ACT(icnt)  = AC(j)
            END DO
          END IF
          IARO_ACT(n+1) = icnt + 1
        END DO
!        NNZC = icnt
        NNZC = IARO_ACT(NIAC+1) - 1
!C---------RESET THE REMAINDER OF JARO_ACT
!        DO n = NNZC+1, NNZ_ALL
!          JARO_ACT(n) = 0
!        END DO
C---------RETURN
        RETURN
      END SUBROUTINE SSPCGU_CALC_IAORDER

      SUBROUTINE SSPCGU_IAPERM(NIA_ALL,NNZ_ALL,IAC_ALL,JAC_ALL,AC,
     2                         IBOUND,NIAC,ARO_ACT,IORDER_ACT)
      IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN)    :: NIA_ALL
        INTEGER, INTENT(IN)    :: Nnz_ALL
        INTEGER, DIMENSION(NIA_ALL+1), INTENT(IN) :: IAC_ALL
        INTEGER, DIMENSION(NNZ_ALL), INTENT(IN)   :: JAC_ALL
        DOUBLE PRECISION, DIMENSION(NNZ_ALL), INTENT(IN)   :: AC
        INTEGER, DIMENSION(NIA_ALL), INTENT(IN)   :: IBOUND
        INTEGER, INTENT(INOUT) :: NIAC
        DOUBLE PRECISION, DIMENSION(NNZ_ALL), INTENT(INOUT) :: ARO_ACT
        INTEGER, DIMENSION(NIA_ALL), INTENT(IN) :: IORDER_ACT
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: j, n
        INTEGER :: i0, i1
        INTEGER :: jcol
        INTEGER :: nt
        INTEGER :: icnt
C     + + + FUNCTIONS + + +
C     + + + FORMATS + + +
C     + + + CODE + + +
C
C---------PERMUTATE ARO_ACT
        icnt = 0
        DO n = 1, NIAC
          nt = IORDER_ACT(n)
          i0 = IAC_ALL(nt)
          i1 = IAC_ALL(nt+1) - 1
          DO j = i0, i1
            jcol = JAC_ALL(j)
            IF ( IBOUND(jcol).EQ.0 ) CYCLE
            icnt = icnt + 1
            ARO_ACT(icnt)  = AC(j)
          END DO
        END DO
C---------RETURN
        RETURN
      END SUBROUTINE SSPCGU_IAPERM
C
C-------ROUTINE TO CALCULATE LORDER AND IORDER FOR REORDERING
C       NOTE - USING GNRCM AND ODRV SUBROUTINES IN THE XMD SOLVER SOURCE CODE
C              SPECIFICALLY IN xmblib.f
      SUBROUTINE SSPCGU_CALC_ORDER( IOUT,IORD,NIAC,NNZC,IA,JA,
     2                              LORDER,IORDER )
      USE SMSMODULE,   ONLY: IPRSMS
      IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: IOUT
        INTEGER, INTENT(IN) :: IORD
        INTEGER, INTENT(IN) :: NIAC
        INTEGER, INTENT(IN) :: NNZC
        INTEGER, DIMENSION(NIAC+1), INTENT(IN)  :: IA
        INTEGER, DIMENSION(NNZC),   INTENT(IN)  :: JA
        INTEGER, DIMENSION(NIAC), INTENT(INOUT) :: LORDER
        INTEGER, DIMENSION(NIAC), INTENT(INOUT) :: IORDER
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
        INTEGER :: nsp
        INTEGER, DIMENSION(:), ALLOCATABLE :: iwork0, iwork1
        INTEGER :: iflag
        INTEGER :: i,j
C     + + + PARAMETERS + + + 
        INTEGER, PARAMETER :: IZERO = 0
C     + + + FUNCTIONS + + +
C     + + + FORMATS + + +
2030  FORMAT(1X,A20,1X,6(I6,1X))
2040  FORMAT(1X,20('-'),1X,6(6('-'),1X))
2050  FORMAT(1X,62('-'),/)
C     + + + CODE + + +
        DO n = 1, NIAC
          LORDER(n) = IZERO
          IORDER(n) = IZERO
        END DO
        ALLOCATE ( iwork0(NIAC)  )
        SELECT CASE ( IORD )
          CASE ( 1 )
            ALLOCATE ( iwork1(NIAC) )
            CALL GENRCM(NIAC, NNZC, IA, JA,
     2                  LORDER, iwork0, iwork1 )
          CASE ( 2 )
            nsp = 3 * NIAC + 4 * NNZC
            ALLOCATE ( iwork1(nsp)  )
            CALL ODRV( IA, JA, LORDER, iwork0, iwork1,
     2                 NIAC, NNZC, nsp, iflag )
            IF ( iflag.NE.0 ) THEN
              CALL USTOP('ERROR CREATING MINIMUM DEGREE ORDER'//
     2                   'PERMUTATION ') 
            END IF
        END SELECT
C
C         GENERATE INVERSE OF LORDER
        DO n = 1, NIAC
          IORDER( LORDER(n) ) = n
        END DO
C
C         WRITE SUMMARY OF REORDERING INFORMATION
C         TO LIST FILE
        IF ( IPRSMS.EQ.2 ) THEN
          DO i = 1, NIAC, 6
            WRITE (IOUT,2030) 'ORIGINAL NODE      :',
     2                        (j,j=i,MIN(i+5,NIAC))
            WRITE (IOUT,2040)
            WRITE (IOUT,2030) 'REORDERED INDEX    :',
     2                        (LORDER(j),j=i,MIN(i+5,NIAC))
            WRITE (IOUT,2030) 'REORDERED NODE     :',
     2                        (IORDER(j),j=i,MIN(i+5,NIAC))
            WRITE (IOUT,2050)
         END DO
         END IF
C         DEALLOCATE TEMPORARY STORAGE
        DEALLOCATE ( iwork0, iwork1 )
C---------RETURN
        RETURN
      END SUBROUTINE SSPCGU_CALC_ORDER
C
C-------ROUTINE TO SCALE THE COEFFICIENT MATRIX (AC), 
C       THE RHS (B), AND THE ESTIMATE OF HNEW (X)
      SUBROUTINE SPCGU_SCALE(IOPT,ISCL,NIAC,NNZC,IA,JA,AC,X,B,
     2                       DSCALE,DSCALE2)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: IOPT
        INTEGER, INTENT(IN) :: ISCL
        INTEGER, INTENT(IN) :: NIAC
        INTEGER, INTENT(IN) :: NNZC
        INTEGER, DIMENSION(NIAC+1), INTENT(IN) :: IA
        INTEGER, DIMENSION(NNZC),   INTENT(IN) :: JA
        DOUBLEPRECISION, DIMENSION(NNZC),  INTENT(INOUT) :: AC
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT) :: X
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT) :: B
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT) :: DSCALE
        DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)  :: DSCALE2
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, n
        INTEGER :: id, jc
        INTEGER :: i0, i1
        DOUBLEPRECISION :: v, c1, c2
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C
C---------SCALE SCALE AC, X, AND B
        IF ( IOPT.EQ.0 ) THEN
C-----------SYMMETRIC SCALING
          SELECT CASE ( ISCL )
            CASE ( 1 )
              DO n = 1, NIAC
                id   = IA(n)
                v    = AC(id)
                c1   = 1.0D0 / SQRT( ABS( v ) )
                DSCALE(n)  = c1
                DSCALE2(n) = c1
              END DO
C               SCALE AC -- AC = DSCALE(row) * AC(i) * DSCALE2(col)
              DO n = 1, NIAC
                c1 = DSCALE(n)
                i0 = IA(n)
                i1 = IA(n+1) - 1
                DO i = i0, i1
                  jc = JA(i)
                  c2 = DSCALE2(jc)
                  AC(i) = c1 * AC(i) * c2 
                END DO
              END DO
C-----------L-2 NORM SCALING
            CASE ( 2 )
C               SCALE EACH ROW SO THAT THE L-2 NORM IS 1
              DO n = 1, NIAC
                c1 = 0.0D0
                i0 = IA(n)
                i1 = IA(n+1) - 1
                DO i = i0, i1
                  c1 = c1 + AC(i) * AC(i)
                END DO
                c1 = SQRT( c1 )
                IF ( c1.EQ.0.0D0 ) THEN
                  c1 = 1.0D0
                ELSE
                  c1 = 1.0D0 / c1
                END IF
                DSCALE(n) = c1 
C                 INITIAL SCALING OF AC -- AC = DSCALE(row) * AC(i)              
                DO i = i0, i1
                  AC(i) = c1 * AC(i)
                END DO
              END DO
C               SCALE EACH COLUMN SO THAT THE L-2 NORM IS 1
              DO n = 1, NIAC
                DSCALE2(n) = 0.0D0
              END DO
              c2 = 0.0D0
              DO n = 1, NIAC
                i0 = IA(n)
                i1 = IA(n+1) - 1
                DO i = i0, i1
                  jc = JA(i)
                  c2 = AC(i)
                  DSCALE2(jc) = DSCALE2(jc) + c2 * c2
                END DO
              END DO
              DO n = 1, NIAC
                c2 = DSCALE2(n)
                IF ( c2.EQ.0.0D0 ) THEN
                  c2 = 1.0D0
                ELSE
                  c2 = 1.0D0 / SQRT( c2 )
                END IF
                DSCALE2(n) = c2
              END DO
C               FINAL SCALING OF AC -- AC = DSCALE2(col) * AC(i)              
              DO n = 1, NIAC
                i0 = IA(n)
                i1 = IA(n+1) - 1
                DO i = i0, i1
                  jc = JA(i)
                  c2 = DSCALE2(jc)
                  AC(i) = c2 * AC(i)
                END DO
              END DO
          END SELECT
C-----------SCALE X AND B
          DO n = 1, NIAC
            c1    = DSCALE(n)
            c2    = DSCALE2(n)
            X(n)  = X(n) / c2
            B(n)  = B(n) * c1
          END DO
C---------UNSCALE SCALE AC, X, AND B
        ELSE
          DO n = 1, NIAC
            c1 = DSCALE(n)
            i0 = IA(n)
            i1 = IA(n+1) - 1
C             UNSCALE AC
            DO i = i0, i1
              jc = JA(i)
              c2 = DSCALE2(jc)
              AC(i) = ( 1.0D0 / c1 ) * AC(i) * ( 1.0D0 / c2 ) 
            END DO
C             UNSCALE X AND B
            c2   = DSCALE2(n)
            X(n) = X(n) * c2
            B(n) = B(n) / c1
          END DO     
        END IF
C---------RETURN
        RETURN
      END SUBROUTINE SPCGU_SCALE
C
C-------ROUTINE TO UPDATE THE PRECONDITIONER
      SUBROUTINE SPCGU_PCU(IOUT,NNZC,NIAC,NIAPC,NJAPC,NNZAPC,IPC,RELAX,
     2                     AC,IA,JA,APC,IAPC,JAPC,IW,W)
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: IOUT
        INTEGER, INTENT(IN) :: NNZC
        INTEGER, INTENT(IN) :: NIAC
        INTEGER, INTENT(IN) :: NIAPC
        INTEGER, INTENT(IN) :: NJAPC
        INTEGER, INTENT(IN) :: NNZAPC
        INTEGER, INTENT(IN) :: IPC
        REAL, INTENT(IN) :: RELAX
        DOUBLEPRECISION, DIMENSION(NNZC),  INTENT(IN)     :: AC
        INTEGER, DIMENSION(NIAC+1), INTENT(IN)    :: IA
        INTEGER, DIMENSION(NNZC), INTENT(IN)      :: JA
        DOUBLEPRECISION, DIMENSION(NNZAPC), INTENT(INOUT) :: APC
        INTEGER, DIMENSION(NIAPC+1), INTENT(INOUT) :: IAPC
        INTEGER, DIMENSION(NJAPC), INTENT(INOUT)   :: JAPC
        INTEGER, DIMENSION(NIAPC), INTENT(INOUT)   :: IW
        DOUBLEPRECISION, DIMENSION(NIAPC), INTENT(INOUT) :: W
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: izero
        DOUBLEPRECISION :: delta
C     + + + FUNCTIONS + + +
C     + + + FORMATS + + +
!2000    FORMAT (/,' MATRIX IS SEVERELY NON-DIAGONALLY DOMINANT.  CHECK',
!     &          ' INPUT FILES.',/,' -- STOP EXECUTION (SPCGU_PCU)')
2000    FORMAT (/,' MATRIX IS SEVERELY NON-DIAGONALLY DOMINANT.',
     &          /,' ADDING SMALL VALUE TO PIVOT (SPCGU_PCU)')
C     + + + CODE + + +
        izero = 0
        delta = 0.0D0
        SELECT CASE(IPC)
C           NO PRE-CONDITIONER
          CASE (0)
C           JACOBI PRE-CONDITIONER
          CASE (1)
            CALL SPCGU_PCJ(NNZC,NIAC,AC,APC,IA,JA)
C           ILU0 AND MILU0
          CASE (2,3)
            LUPC: DO
              CALL SPCGU_PCILU0(NNZC,NIAC,AC,IA,JA,
     2                          APC,IAPC,JAPC,IW,W,
     3                          RELAX,izero,delta)
              IF ( izero.LT.1 ) THEN
                EXIT LUPC
              END IF
              delta = 1.5D0 * delta + 0.001
              izero = 0
              IF ( delta.GT.0.5D0 ) THEN
                WRITE(IOUT,2000)
                delta = 0.5D0
                izero = 2
              END IF
            END DO LUPC
C           ADDITIONAL PRECONDITIONERS - ILUT, etc.
        END SELECT
C---------RETURN
        RETURN
      END SUBROUTINE SPCGU_PCU
C
C-------JACOBI PRECONDITIONER - INVERSE OF DIAGONAL 
      SUBROUTINE SPCGU_PCJ(NNZC,NIAC,AC,APC,IA,JA)
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NNZC
        INTEGER, INTENT(IN) :: NIAC
        DOUBLEPRECISION, DIMENSION(NNZC),  INTENT(IN)      :: AC
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT)   :: APC
        INTEGER, DIMENSION(NIAC+1), INTENT(IN) :: IA
        INTEGER, DIMENSION(NNZC),   INTENT(IN) :: JA
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: i, n
        INTEGER :: ic0, ic1
        INTEGER :: id
        DOUBLEPRECISION :: tv
C     + + + PARAMETERS + + + 
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
        DOUBLEPRECISION, PARAMETER :: DONE  = 1.0D0
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        DO n = 1, NIAC
            ic0 = IA(n)
            ic1 = IA(n+1) - 1
            id = IA(n)
            DO i = ic0, ic1
              IF ( JA(i).EQ.n ) THEN
                id = i
                EXIT
              END IF
            END DO
            tv  = AC(id)
            IF ( ABS( tv ).GT.DZERO ) tv = DONE / tv
            APC(n) = tv
        END DO
C---------RETURN
        RETURN
      END SUBROUTINE SPCGU_PCJ

      SUBROUTINE SPCGU_JACA(NIAC,A,D1,D2)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NIAC
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(IN)    :: A
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(IN)    :: D1
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT) :: D2
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
        DOUBLEPRECISION :: tv
C     + + + PARAMETERS + + + 
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
        DOUBLEPRECISION, PARAMETER :: DONE  = 1.0D0
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        DO n = 1, NIAC
          tv     = A(n) * D1(n)
          D2(n) = tv
        END DO
C---------RETURN
        RETURN
      END SUBROUTINE SPCGU_JACA

      SUBROUTINE SPCGU_PCILU0(NNZC,NIAC,AC,IA,JA,
     2                        APC,IAPC,JAPC,IW,W,
     3                        RELAX,IZERO,DELTA)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NNZC
        INTEGER, INTENT(IN) :: NIAC
        DOUBLEPRECISION, DIMENSION(NNZC),  INTENT(IN)     :: AC
        INTEGER, DIMENSION(NIAC+1), INTENT(IN)    :: IA
        INTEGER, DIMENSION(NNZC), INTENT(IN)      :: JA
        DOUBLEPRECISION, DIMENSION(NNZC), INTENT(INOUT)   :: APC
        INTEGER, DIMENSION(NIAC+1), INTENT(INOUT) :: IAPC
        INTEGER, DIMENSION(NNZC), INTENT(INOUT)   :: JAPC
        INTEGER, DIMENSION(NIAC), INTENT(INOUT)   :: IW
        DOUBLEPRECISION, DIMENSION(NIAC), INTENT(INOUT)   :: W
        REAL, INTENT(IN) :: RELAX
        INTEGER, INTENT(INOUT) :: IZERO
        DOUBLEPRECISION, INTENT(IN) :: DELTA
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: ic0, ic1
        INTEGER :: iic0, iic1
        INTEGER :: iu, iiu
        INTEGER :: j, n
        INTEGER :: jj
        INTEGER :: jcol, jw
        INTEGER :: jjcol
        DOUBLEPRECISION :: drelax
        DOUBLEPRECISION :: sd1
        DOUBLEPRECISION :: tl
        DOUBLEPRECISION :: rs
        DOUBLEPRECISION :: d
C     + + + PARAMETERS + + + 
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
        DOUBLEPRECISION, PARAMETER :: DONE  = 1.0D0
        DOUBLEPRECISION, PARAMETER :: DTINY = 1.0D-06
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        drelax = DBLE( RELAX )
        DO n = 1, NIAC
          IW(n)  = 0
          W(n)   = DZERO
        END DO
        MAIN: DO n = 1, NIAC
          ic0 = IA(n)
          ic1 = IA(n+1) - 1
          DO j = ic0, ic1
            jcol      = JA(j)
            IW(jcol) = 1
            W(jcol) = W(jcol) + AC(j)
          END DO
          ic0 = IAPC(n)
          ic1 = IAPC(n+1) - 1
          iu  = JAPC(n)
          rs   = DZERO
          LOWER: DO j = ic0, iu-1
            jcol     = JAPC(j)
            iic0     = IAPC(jcol) 
            iic1     = IAPC(jcol+1) - 1
            iiu      = JAPC(jcol)
            tl       = W(jcol) * APC(jcol)
            W(jcol) = tl
            DO jj = iiu, iic1
              jjcol = JAPC(jj)
              jw    = IW(jjcol)
              IF ( jw.NE.0 ) THEN
                W(jjcol) = W(jjcol) - tl * APC(jj)
              ELSE
                rs = rs + tl * APC(jj)
              END IF
            END DO
          END DO LOWER
C           DIAGONAL - CALCULATE INVERSE OF DIAGONAL FOR SOLUTION
          d   = W(n)
          tl  = ( DONE + DELTA ) * d - ( drelax * rs )
C-----------ENSURE THAT THE SIGN OF THE DIAGONAL HAS NOT CHANGED AND IS NOT ZERO
          sd1 = SIGN(d,tl)
          IF ( sd1.NE.d ) THEN
C             USE SMALL VALUE IF DIAGONAL SCALING IS NOT EFFECTIVE FOR ELIMINATING
C             PIVOTS THAT CHANGE THE SIGN OF THE DIAGONAL
            IF ( IZERO.GT.1 ) THEN
              tl = SIGN(DTINY,d)
C             DIAGONAL SCALING CONTINUES TO BE EFFECTIVE
            ELSE
              IZERO = 1
              EXIT MAIN
            END IF
          END IF
          IF ( ABS(tl).EQ.DZERO ) THEN
C             USE SMALL VALUE IF DIAGONAL SCALING IS NOT EFFECTIVE FOR ELIMINATING
C             ZERO PIVOTS
            IF ( IZERO.GT.1 ) THEN
              tl = SIGN(DTINY,d)
C             DIAGONAL SCALING CONTINUES TO BE EFFECTIVE FOR ELIMIATING ZERO PIVOTS
            ELSE
              IZERO = 1
              EXIT MAIN
            END IF
          END IF
          APC(n) = DONE / tl
C           RESET POINTER FOR IW TO ZERO
          IW(n) = 0
          W(n)  = DZERO
          DO j = ic0, ic1
            jcol = JAPC(j)
            APC(j) = W(jcol)
            IW(jcol) = 0
            W(jcol) = DZERO
          END DO
      END DO MAIN
!      
!        do n = 1, niac
!          ic0 = IAPC(n)
!          ic1 = IAPC(n+1) - 1
!          iu  = JAPC(n)
!          do j = ic0,iu-1
!            jcol = JAPC(j)
!            write (10991,'(i10,i10,g15.7)' ) n, jcol, 
!     2                                       APC(j)
!          end do
!          write (10991,'(i10,i10,g15.7)' ) n, n, 
!     2                                     APC(n)
!          do j = iu,ic1
!            jcol = JAPC(j)
!            write (10991,'(i10,i10,g15.7)' ) n, jcol, 
!     2                                       APC(j)
!          end do
!        end do
C
C---------RESET IZERO IF SUCCESSFUL COMPLETION OF MAIN
        IZERO = 0
C
C---------RETURN
        RETURN
      END SUBROUTINE SPCGU_PCILU0

      SUBROUTINE SPCGU_ILU0A(NNZC,NIAC,APC,IAPC,JAPC,R,D)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NNZC
        INTEGER, INTENT(IN) :: NIAC
        DOUBLEPRECISION, DIMENSION(NNZC),  INTENT(INOUT)  :: APC
        INTEGER, DIMENSION(NIAC+1), INTENT(IN) :: IAPC
        INTEGER, DIMENSION(NNZC), INTENT(IN)   :: JAPC
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(IN)     :: R
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT)  :: D
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: ic0, ic1
        INTEGER :: iu
        INTEGER :: jcol
        INTEGER :: j, n
        DOUBLEPRECISION :: tv
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
C         FORWARD SOLVE - APC * D = R
        FORWARD: DO n = 1, NIAC
          tv   = R(n)
          ic0 = IAPC(n)
          ic1 = IAPC(n+1) - 1
          iu  = JAPC(n) - 1
          LOWER: DO j = ic0, iu
            jcol = JAPC(j)
            tv    = tv - APC(j) * D(jcol)
          END DO LOWER
          D(n) = tv
        END DO FORWARD
C         BACKWARD SOLVE - D = D / U
        BACKWARD: DO n = NIAC, 1, -1
          ic0 = IAPC(n)
          ic1 = IAPC(n+1) - 1
          iu  = JAPC(n)
          tv   = D(n)
          UPPER: DO j = iu, ic1
            jcol = JAPC(j)
            tv    = tv - APC(j) * D(jcol)
          END DO UPPER
C           COMPUTE D FOR DIAGONAL - D = D / U
          D(n) =  tv * APC(n)
        END DO BACKWARD
C---------RETURN
        RETURN
      END SUBROUTINE SPCGU_ILU0A

      SUBROUTINE SPCGU_CG(ICNVG,ITMAX,INNERIT)
        USE PCGUMODULE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(INOUT) :: ICNVG
        INTEGER, INTENT(IN)    :: ITMAX
        INTEGER, INTENT(INOUT) :: INNERIT
C       + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
        INTEGER :: iiter
        DOUBLEPRECISION :: dhclose, drclose
        DOUBLEPRECISION :: tv
        DOUBLEPRECISION :: deltax
        DOUBLEPRECISION :: rmax
        DOUBLEPRECISION :: l2norm
        DOUBLEPRECISION :: rcnvg
        DOUBLEPRECISION :: alpha, beta
        DOUBLEPRECISION :: rho, rho0
        DOUBLEPRECISION :: machprec
C     + + + PARAMETERS + + + 
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
        DOUBLEPRECISION, PARAMETER :: DONE  = 1.0D0
C         + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SPCGU_DP
C
C         + + + CODE + + +
        INNERIT  = 0
        machprec = EPSILON( DZERO )
        dhclose  = DBLE( HCLOSEPCGU )
        drclose  = DBLE( RCLOSEPCGU )
C
C-------INNER ITERATION          
        INNER: DO iiter = 1, itmax
           INNERIT = INNERIT + 1 
           NITERC  = NITERC  + 1 
C----------APPLY PRECONDITIONER
          SELECT CASE (IPC)
C             NO PRECONDITIONER
            CASE (0)
              DO n = 1, NIAC
                Z(n) = D(n)
              END DO
C             JACOBI PRECONDITIONER
            CASE (1)
              CALL SPCGU_JACA(NIAC,APC,D,Z)
            CASE (2,3)
              CALL SPCGU_ILU0A(NNZC,NIAC,APC,IAPC,JAPC,D,Z)
          END SELECT
          rho = SPCGU_DP( NIAC, D, Z )
C-----------COMPUTE DIRECTIONAL VECTORS
          IF (IITER.EQ.1) THEN
            DO n = 1, NIAC
              P(n) = Z(n)
            END DO
          ELSE
            beta = rho / rho0
            DO n = 1, NIAC
              P(n) = Z(n) + beta * P(n)
            END DO
          END IF
C-----------COMPUTE ITERATES
C           UPDATE qc
          CALL SPCGU_MV(NNZC,NIAC,A0,P,Q,IA0,JA0)

          alpha = rho / SPCGU_DP( NIAC, P, Q )
C-----------UPDATE X AND RESIDUAL
          deltax = DZERO
          rmax   = DZERO
          l2norm = DZERO
          DO n = 1, NIAC
            tv      = alpha * P(n)
            X(n)  = X(n) + tv
            deltax = MAX( ABS(tv), deltax )
            tv      = D(n)
            tv      = tv - alpha * Q(n)
            D(n)  = tv
            rmax   = MAX( ABS(tv), rmax )
            l2norm = l2norm + tv * tv
          END DO
          l2norm = SQRT(l2norm)
C-----------TEST FOR SOLVER CONVERGENCE
          IF ( ICNVGOPT.EQ.1 ) THEN
            rcnvg = l2norm
          ELSE
            rcnvg = rmax
          END IF
          CALL SPCGU_TESTCNVG( ICNVGOPT,ICNVG,
     2                         deltax,rcnvg,
     3                         L2NORM0,EPFACT,dhclose,drclose )
C           CHECK FOR EXACT SOLUTION
          IF ( rcnvg.EQ.DZERO ) ICNVG = 1
          IF ( ICNVG.EQ.1 ) EXIT INNER
C-----------CHECK THAT CURRENT AND PREVIOUS rho ARE DIFFERENT
          IF ( ABS( rho - rho0 ).LT.machprec ) THEN
            rho0 = rho
            EXIT INNER
          END IF
C-----------SAVE CURRENT INNER ITERATES
          rho0 = rho
        END DO INNER
C---------RETURN
        RETURN
      END SUBROUTINE SPCGU_CG

      SUBROUTINE SPCGU_BCGS(ICNVG,ITMAX,INNERIT)
        USE PCGUMODULE
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(INOUT) :: ICNVG
        INTEGER, INTENT(IN)    :: ITMAX
        INTEGER, INTENT(INOUT) :: INNERIT
C       + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
        INTEGER :: iiter
        DOUBLEPRECISION :: dhclose, drclose
        DOUBLEPRECISION :: tv
        DOUBLEPRECISION :: deltax
        DOUBLEPRECISION :: rmax
        DOUBLEPRECISION :: l2norm
        DOUBLEPRECISION :: rcnvg
        DOUBLEPRECISION :: alpha, alpha0 
        DOUBLEPRECISION :: beta
        DOUBLEPRECISION :: rho, rho0
        DOUBLEPRECISION :: omega, omega0
        DOUBLEPRECISION :: machprec
        DOUBLEPRECISION :: numer, denom
C     + + + PARAMETERS + + + 
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
        DOUBLEPRECISION, PARAMETER :: DONE  = 1.0D0
        DOUBLEPRECISION, PARAMETER :: DTWO  = 2.0D0
C         + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SPCGU_DP
!        DOUBLEPRECISION :: SPCGU_RES
!        DOUBLEPRECISION :: SPCGU_L2NORM
C
C         + + + CODE + + +
        INNERIT  = 0
        machprec = EPSILON( DZERO )
        dhclose  = DBLE( HCLOSEPCGU )
        drclose  = DBLE( RCLOSEPCGU )
        
        alpha = DZERO
        beta  = DZERO
        rho   = DZERO
        rho0  = DZERO
C        
C-------SAVE INITIAL RESIDUAL
        DO n = 1, NIAC
          DHAT(n) = D(n)
        END DO
C
C-------INNER ITERATION          
        INNER: DO iiter = 1, itmax
           INNERIT = INNERIT + 1 
           NITERC = NITERC + 1 
C----------CALCULATE rho
          rho = SPCGU_DP( NIAC, DHAT, D )
C-----------COMPUTE DIRECTIONAL VECTORS
          IF (IITER.EQ.1) THEN
            DO n = 1, NIAC
              P(n) = D(n)
            END DO
          ELSE
            beta = ( rho / rho0 ) * ( alpha0 / omega0 )
            DO n = 1, NIAC
              P(n) = D(n) + beta * ( P(N) - omega * V(n) )
            END DO
          END IF
C----------APPLY PRECONDITIONER TO UPDATE PHAT
          SELECT CASE (IPC)
C             NO PRECONDITIONER
            CASE (0)
              DO n = 1, NIAC
                PHAT(n) = P(n)
              END DO
C             JACOBI PRECONDITIONER
            CASE (1)
              CALL SPCGU_JACA(NIAC,APC,P,PHAT)
            CASE (2,3)
              CALL SPCGU_ILU0A(NNZC,NIAC,APC,IAPC,JAPC,P,PHAT)
          END SELECT
C-----------COMPUTE ITERATES
C           UPDATE V WITH A AND PHAT
          CALL SPCGU_MV(NNZC,NIAC,A0,PHAT,V,IA0,JA0)
C           UPDATE alpha WITH DHAT AND V
          denom = SPCGU_DP( NIAC, DHAT, V )
          !denom = denom + SIGN(dsmall,denom)
          alpha = rho / denom
C-----------UPDATE Q
          DO n = 1, NIAC
            Q(n) = D(n) - alpha * V(n)
          END DO
C-----------CALCULATE INFINITY NORM OF Q - TEST FOR TERMINATION
C           TERMINATE IF rmax IS LESS THAN MACHINE PRECISION (machprec)
          rmax = DZERO
          DO n = 1, NIAC
              tv = Q(n)
              IF ( ISCL.NE.0 ) tv = tv / DSCALE(n)
              IF ( ABS(tv).GT.ABS(rmax) ) rmax = tv
          END DO
          IF ( ABS(rmax).LE.machprec ) THEN
            deltax = DZERO
            DO n = 1, NIAC
              tv      = alpha * PHAT(n)
              IF ( ISCL.NE.0 ) THEN
                tv = tv * DSCALE(n)
              END IF
              X(n)  = X(n) + tv
              IF ( ABS(tv).GT.ABS(deltax) ) deltax = tv
            END DO
            CALL SPCGU_TESTCNVG( ICNVGOPT,ICNVG,
     2                           deltax,rmax,
     3                           rmax,EPFACT,dhclose,drclose )
            IF ( ICNVG.EQ.1 ) EXIT INNER
          END IF
C-----------APPLY PRECONDITIONER TO UPDATE QHAT
          SELECT CASE (IPC)
C             NO PRECONDITIONER
            CASE (0)
              DO n = 1, NIAC
                QHAT(n) = Q(n)
              END DO
C             JACOBI PRECONDITIONER
            CASE (1)
              CALL SPCGU_JACA(NIAC,APC,Q,QHAT)
            CASE (2,3)
              CALL SPCGU_ILU0A(NNZC,NIAC,APC,IAPC,JAPC,Q,QHAT)
          END SELECT
C           UPDATE T WITH A AND QHAT
          CALL SPCGU_MV(NNZC,NIAC,A0,QHAT,T,IA0,JA0)
C-----------UPDATE omega
          numer = SPCGU_DP( NIAC, T, Q )
          denom = SPCGU_DP( NIAC, T, T )
          denom = denom + SIGN(machprec,denom)
          omega = numer / denom
C-----------UPDATE X AND RESIDUAL
          deltax = DZERO
          rmax   = DZERO
          l2norm = DZERO
          DO n = 1, NIAC
C-------------X AND DX            
            tv      = alpha * PHAT(n) + omega * QHAT(n)
            X(n)  = X(n) + tv
            IF ( ISCL.NE.0 ) THEN
              tv = tv * DSCALE(n)
            END IF
            IF ( ABS(tv).GT.ABS(deltax) ) deltax = tv
C-------------RESIDUAL
            tv      = Q(n) - omega * T(n)
            D(n)  = tv
            IF ( ISCL.NE.0 ) THEN
              tv = tv / DSCALE(n)
            END IF
            IF ( ABS(tv).GT.ABS(rmax) ) rmax = tv
            l2norm = l2norm + tv * tv
          END DO
C-----------TEST FOR SOLVER CONVERGENCE
          IF ( ICNVGOPT.EQ.1 ) THEN
            rcnvg = l2norm
          ELSE
            rcnvg = rmax
          END IF
          CALL SPCGU_TESTCNVG( ICNVGOPT,ICNVG,
     2                         deltax,rcnvg,
     3                         L2NORM0,EPFACT,dhclose,drclose )
C           CHECK FOR EXACT SOLUTION
          IF ( rcnvg.EQ.DZERO ) ICNVG = 1
          IF ( ICNVG.EQ.1 ) EXIT INNER
C-----------CHECK THAT CURRENT AND PREVIOUS rho, alpha, AND omega ARE DIFFERENT
          IF ( ABS( rho - rho0 ).LT.machprec ) THEN
            rho0 = rho
            EXIT INNER
          END IF
          IF ( ABS( alpha - alpha0 ).LT.machprec ) THEN
            alpha0 = alpha
            EXIT INNER
          END IF
          IF ( ABS( omega - omega0 ).LT.machprec ) THEN
            omega0 = omega
            EXIT INNER
      END IF
C-----------SAVE CURRENT INNER ITERATES
          rho0   = rho
          alpha0 = alpha
          omega0 = omega
        END DO INNER
C---------RETURN
        RETURN
      END SUBROUTINE SPCGU_BCGS
C
C---------TEST FOR SOLVER CONVERGENCE
        SUBROUTINE SPCGU_TESTCNVG( Icnvgopt,Icnvg,
     2                             Hmax,Rmax,
     3                             Rmax0,Epfact,Hclose,Rclose )
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN)         :: Icnvgopt
        INTEGER, INTENT(INOUT)      :: Icnvg
        DOUBLEPRECISION, INTENT(IN) :: Hmax
        DOUBLEPRECISION, INTENT(IN) :: Rmax
        DOUBLEPRECISION, INTENT(IN) :: Rmax0
        DOUBLEPRECISION, INTENT(IN) :: Epfact
        DOUBLEPRECISION, INTENT(IN) :: Hclose
        DOUBLEPRECISION, INTENT(IN) :: Rclose
C     + + + LOCAL DEFINITIONS + + +
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        IF ( Icnvgopt.EQ.0 ) THEN
          IF ( ABS(Hmax).LE.Hclose .AND. ABS(Rmax).LE.Rclose ) THEN
            Icnvg = 1
          END IF
        ELSE
          IF ( Rmax.LE.Rclose ) THEN
            Icnvg = 1
          ELSE IF ( Rmax.LE.Rmax0*Epfact ) THEN
            Icnvg = 1
          END IF
        END IF
C---------RETURN
        RETURN
      END SUBROUTINE SPCGU_TESTCNVG
C
C---------GENERATE IAPC AND JAPC FROM IA AND JA
C         JAPC(1:NIAC) HAS THE POSITION OF THE UPPER ENTRY FOR A ROW
C         JAPC(NIAC+1:NNZC) IS THE COLUMN POSITION FOR ENTRY
C         APC(1:NIAC) PRECONDITIONED INVERSE OF THE DIAGONAL
C         APC(NIAC+1:NNZC) PRECONDITIONED ENTRIES FOR OFF DIAGONALS
        SUBROUTINE SPCGU_PCCRS(NIAC,NNZC,IA,JA,
     2                         IAPC,JAPC)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN)         :: NIAC
        INTEGER, INTENT(IN)         :: NNZC
        INTEGER, DIMENSION(NIAC+1), INTENT(IN)    :: IA
        INTEGER, DIMENSION(NNZC), INTENT(IN)      :: JA
        INTEGER, DIMENSION(NIAC+1), INTENT(INOUT) :: IAPC
        INTEGER, DIMENSION(NNZC), INTENT(INOUT)   :: JAPC
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n, j
        INTEGER :: i0, i1
        INTEGER :: nlen
        INTEGER :: ic,ip
        INTEGER :: jcol
        INTEGER, DIMENSION(:), ALLOCATABLE :: iarr
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        ip = NIAC + 1
        DO n = 1, NIAC
          i0 = IA(n)
          i1 = IA(n+1) - 1
          nlen = i1 - i0
          ALLOCATE( iarr(nlen) )
          ic = 0
          DO j = i0, i1
            jcol = JA(j)
            IF ( jcol.EQ.n ) CYCLE
            ic = ic + 1
            iarr(ic) = jcol
          END DO
          CALL SPCGU_ISORT(nlen,iarr)
          IAPC(n) = ip
          DO j = 1, nlen
            jcol = iarr(j)
            JAPC(ip) = jcol
            ip = ip + 1
          END DO
          DEALLOCATE(iarr)
        END DO
        IAPC(NIAC+1) = NNZC + 1
C---------POSITION OF THE FIRST UPPER ENTRY FOR ROW         
        DO n = 1, NIAC
          i0 = IAPC(n)
          i1 = IAPC(n+1) - 1
          JAPC(n) = IAPC(n+1)
          DO j = i0, i1
            jcol = JAPC(j)
            IF ( jcol.GT.n ) THEN
              JAPC(n) = j
              EXIT
            END IF
          END DO
        END DO
C---------RETURN
        RETURN
      END SUBROUTINE SPCGU_PCCRS
C
C-------SIMPLE INPLACE SORTING ROUTINE FOR AN INTEGER ARRAY      
      SUBROUTINE SPCGU_ISORT(NVAL,IARRAY)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER,INTENT(IN) :: NVAL
        INTEGER,DIMENSION(NVAL),INTENT(INOUT) :: IARRAY
C     + + + LOCAL DEFINITIONS + + +
        integer :: i, j, itemp
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        DO i = 1, NVAL-1
            DO j = i+1, NVAL
                if(IARRAY(i).GT.IARRAY(j)) then
                    itemp = IARRAY(j)
                    IARRAY(j) = IARRAY(i)
                    IARRAY(i) = itemp
                END IF
            END DO
        END DO
      END SUBROUTINE SPCGU_ISORT
      
C       COPY ONE DOUBLEPRECISION VECTOR TO ANOTHER
      SUBROUTINE SSPCGU_DCOPY(NR, V, R)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NR
        DOUBLEPRECISION, DIMENSION(NR), INTENT(IN)    :: V
        DOUBLEPRECISION, DIMENSION(NR), INTENT(INOUT) :: R
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        DO n = 1, NR
          R(n) = V(n)
        END DO
C---------RETURN
        RETURN
      END SUBROUTINE SSPCGU_DCOPY

C       COPY ONE INTEGER VECTOR TO ANOTHER
      SUBROUTINE SSPCGU_ICOPY(NR, V, R)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NR
        INTEGER, DIMENSION(NR), INTENT(IN)    :: V
        INTEGER, DIMENSION(NR), INTENT(INOUT) :: R
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        DO n = 1, NR
          R(n) = V(n)
        END DO
C---------RETURN
        RETURN
      END SUBROUTINE SSPCGU_ICOPY
      
      SUBROUTINE SPCGU_MV(NNZC,NIAC,A,D1,D2,IA,JA)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NNZC
        INTEGER, INTENT(IN) :: NIAC
        DOUBLEPRECISION, DIMENSION(NNZC),  INTENT(IN)    :: A
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(IN)    :: D1
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(INOUT) :: D2
        INTEGER, DIMENSION(NIAC+1), INTENT(IN) :: IA
        INTEGER, DIMENSION(NNZC), INTENT(IN)   :: JA
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: ic0, ic1
        INTEGER :: icol
        INTEGER :: m, n
        DOUBLEPRECISION :: tv
C     + + + PARAMETERS + + + 
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        DO n = 1, NIAC
C           ADD DIAGONAL AND OFF-DIAGONAL TERMS
          tv     = DZERO
          ic0   = IA(n)
          ic1   = IA(n+1)-1
          DO m = ic0, ic1
            icol = JA(m)
            tv  = tv + A(m) * D1(icol)
          END DO
          D2(n) = tv
        END DO
C---------RETURN
        RETURN
      END SUBROUTINE SPCGU_MV

      DOUBLEPRECISION FUNCTION SPCGU_DP(NIAC,A,B) RESULT(C)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NIAC
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(IN)    :: A
        DOUBLEPRECISION, DIMENSION(NIAC),  INTENT(IN)    :: B
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: n
C     + + + PARAMETERS + + + 
        DOUBLEPRECISION, PARAMETER :: DZERO = 0.0D0
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        C = DZERO
        DO n = 1, NIAC
          C = C + A(n) * B(n)
        END DO
C---------RETURN
        RETURN
      END FUNCTION SPCGU_DP
C
C-------CALCULATE THE L2 NORM OF A VECTOR
      DOUBLEPRECISION FUNCTION SPCGU_L2NORM(NIAC,V) RESULT(value)
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: NIAC
        DOUBLEPRECISION, DIMENSION(NIAC), INTENT(IN) :: V
C     + + + LOCAL DEFINITIONS + + +
        DOUBLEPRECISION :: dotp
C     + + + FUNCTIONS + + +
        DOUBLEPRECISION :: SPCGU_DP
C     + + + CODE + + +
        dotp  = SPCGU_DP(NIAC,V,V)
        value = SQRT(dotp)
C---------RETURN
        RETURN
      END FUNCTION SPCGU_L2NORM
C
C-------CALCULATE THE RESIDUAL FOR A NODE USING CURRENT VALUES OF
C       AC, X, AND THE RHS FOR THE NODE PASSED TO FUNCTION     
      DOUBLEPRECISION FUNCTION SPCGU_RES(N,IA,JA,AC,X,B) 
     2  RESULT(value)
        USE PCGUMODULE, ONLY: NIAC,NNZC
        IMPLICIT NONE
C     + + + DUMMY ARGUMENTS + + +
        INTEGER, INTENT(IN) :: N
        INTEGER, DIMENSION(NIAC+1), INTENT(IN) :: IA
        INTEGER, DIMENSION(NNZC), INTENT(IN)   :: JA
        DOUBLEPRECISION, DIMENSION(NNZC), INTENT(IN)   :: AC
        DOUBLEPRECISION, DIMENSION(NIAC), INTENT(IN)   :: X
        DOUBLEPRECISION, INTENT(IN)   :: B
C     + + + LOCAL DEFINITIONS + + +
        INTEGER :: j
        INTEGER :: i0, i1
        INTEGER :: jcol
C     + + + FUNCTIONS + + +
C     + + + CODE + + +
        value = 0.0
        i0 = IA(N)
        i1 = IA(N+1) - 1
        DO j = i0, i1
          jcol = JA(j)
          value = value + AC(j) * X(jcol)
        END DO
        value = B - value
C---------RETURN
        RETURN
      END FUNCTION SPCGU_RES
C
C-------ROUTINES FROM SPARSKIT TO PERMUTATE A LINEAR SYSTEM OF EQUATIONS
C       IN ORDER TO REORDER THE MATRIX TO MINIMIZE THE BANDWIDTH USING
C       THE REVERSE CUTHILL MCKEE ALGORITHM
      subroutine dperm (nrow,a,ja,ia,ao,jao,iao,perm,qperm,job)
      integer nrow,ja(*),ia(nrow+1),jao(*),iao(nrow+1),perm(nrow),
     +        qperm(*),job
      real*8 a(*),ao(*)
!-----------------------------------------------------------------------
! This routine permutes the rows and columns of a matrix stored in CSR
! format. i.e., it computes P A Q, where P, Q are permutation matrices.
! P maps row i into row perm(i) and Q maps column j into column qperm(j):
!      a(i,j)    becomes   a(perm(i),qperm(j)) in new matrix
! In the particular case where Q is the transpose of P (symmetric
! permutation of A) then qperm is not needed.
! note that qperm should be of length ncol (number of columns) but this
! is not checked.
!-----------------------------------------------------------------------
! Y. Saad, Sep. 21 1989 / recoded Jan. 28 1991.
!-----------------------------------------------------------------------
! on entry:
!----------
! n       = dimension of the matrix
! a, ja,
!    ia = input matrix in a, ja, ia format
! perm       = integer array of length n containing the permutation arrays
!        for the rows: perm(i) is the destination of row i in the
!         permuted matrix -- also the destination of column i in case
!         permutation is symmetric (job .le. 2)
!
! qperm      = same thing for the columns. This should be provided only
!         if job=3 or job=4, i.e., only in the case of a nonsymmetric
!        permutation of rows and columns. Otherwise qperm is a dummy
!
! job      = integer indicating the work to be done:
! * job = 1,2 permutation is symmetric  Ao :== P * A * transp(P)
!             job = 1      permute a, ja, ia into ao, jao, iao
!             job = 2 permute matrix ignoring real values.
! * job = 3,4 permutation is non-symmetric  Ao :== P * A * Q
!             job = 3      permute a, ja, ia into ao, jao, iao
!             job = 4 permute matrix ignoring real values.
!
! on return:
!-----------
! ao, jao, iao = input matrix in a, ja, ia format
!
! in case job .eq. 2 or job .eq. 4, a and ao are never referred to
! and can be dummy arguments.
! Notes:
!-------
!  1) algorithm is in place
!  2) column indices may not be sorted on return even  though they may be
!     on entry.
!----------------------------------------------------------------------c
! local variables
      integer locjob, mod
!
!     locjob indicates whether or not real values must be copied.
!
      locjob = mod(job,2)
!
! permute rows first
!
      call rperm (nrow,a,ja,ia,ao,jao,iao,perm,locjob)
!
! then permute columns
!
      locjob = 0
!
      if (job .le. 2) then
         call cperm (nrow,ao,jao,iao,ao,jao,iao,perm,locjob)
      else
         call cperm (nrow,ao,jao,iao,ao,jao,iao,qperm,locjob)
      endif
!
      return
!-------end-of-dperm----------------------------------------------------
      end
!-----------------------------------------------------------------------
      subroutine rperm (nrow,a,ja,ia,ao,jao,iao,perm,job)
      integer nrow,ja(*),ia(nrow+1),jao(*),iao(nrow+1),perm(nrow),job
      real*8 a(*),ao(*)
!-----------------------------------------------------------------------
! this subroutine permutes the rows of a matrix in CSR format.
! rperm  computes B = P A  where P is a permutation matrix.
! the permutation P is defined through the array perm: for each j,
! perm(j) represents the destination row number of row number j.
! Youcef Saad -- recoded Jan 28, 1991.
!-----------------------------------------------------------------------
! on entry:
!----------
! n       = dimension of the matrix
! a, ja, ia = input matrix in csr format
! perm       = integer array of length nrow containing the permutation arrays
!        for the rows: perm(i) is the destination of row i in the
!         permuted matrix.
!         ---> a(i,j) in the original matrix becomes a(perm(i),j)
!         in the output  matrix.
!
! job      = integer indicating the work to be done:
!             job = 1      permute a, ja, ia into ao, jao, iao
!                       (including the copying of real values ao and
!                       the array iao).
!             job .ne. 1 :  ignore real values.
!                     (in which case arrays a and ao are not needed nor
!                      used).
!
!------------
! on return:
!------------
! ao, jao, iao = input matrix in a, ja, ia format
! note :
!        if (job.ne.1)  then the arrays a and ao are not used.
!----------------------------------------------------------------------c
!           Y. Saad, May  2, 1990                                      c
!----------------------------------------------------------------------c
      logical values
      values = (job .eq. 1)
!
!     determine pointers for output matix.
!
      do 50 j=1,nrow
         i = perm(j)
         iao(i+1) = ia(j+1) - ia(j)
 50   continue
!
! get pointers from lengths
!
      iao(1) = 1
      do 51 j=1,nrow
         iao(j+1)=iao(j+1)+iao(j)
 51   continue
!
! copying
!
      do 100 ii=1,nrow
!
! old row = ii  -- new row = iperm(ii) -- ko = new pointer
!
         ko = iao(perm(ii))
         do 60 k=ia(ii), ia(ii+1)-1
            jao(ko) = ja(k)
            if (values) ao(ko) = a(k)
            ko = ko+1
 60      continue
 100  continue
!
      return
!---------end-of-rperm -------------------------------------------------
!-----------------------------------------------------------------------
      end

!-----------------------------------------------------------------------
      subroutine cperm (nrow,a,ja,ia,ao,jao,iao,perm,job)
      integer nrow,ja(*),ia(nrow+1),jao(*),iao(nrow+1),perm(*), job
      real*8 a(*), ao(*)
!-----------------------------------------------------------------------
! this subroutine permutes the columns of a matrix a, ja, ia.
! the result is written in the output matrix  ao, jao, iao.
! cperm computes B = A P, where  P is a permutation matrix
! that maps column j into column perm(j), i.e., on return
!      a(i,j) becomes a(i,perm(j)) in new matrix
! Y. Saad, May 2, 1990 / modified Jan. 28, 1991.
!-----------------------------------------------------------------------
! on entry:
!----------
! nrow       = row dimension of the matrix
!
! a, ja, ia = input matrix in csr format.
!
! perm      = integer array of length ncol (number of columns of A
!         containing the permutation array  the columns:
!         a(i,j) in the original matrix becomes a(i,perm(j))
!         in the output matrix.
!
! job      = integer indicating the work to be done:
!             job = 1      permute a, ja, ia into ao, jao, iao
!                       (including the copying of real values ao and
!                       the array iao).
!             job .ne. 1 :  ignore real values ao and ignore iao.
!
!------------
! on return:
!------------
! ao, jao, iao = input matrix in a, ja, ia format (array ao not needed)
!
! Notes:
!-------
! 1. if job=1 then ao, iao are not used.
! 2. This routine is in place: ja, jao can be the same.
! 3. If the matrix is initially sorted (by increasing column number)
!    then ao,jao,iao  may not be on return.
!
!----------------------------------------------------------------------c
! local parameters:
      integer k, i, nnz
!
      nnz = ia(nrow+1)-1
      do 100 k=1,nnz
         jao(k) = perm(ja(k))
 100  continue
!
!     done with ja array. return if no need to touch values.
!
      if (job .ne. 1) return
!
! else get new pointers -- and copy values too.
!
      do 1 i=1, nrow+1
         iao(i) = ia(i)
 1    continue
!
      do 2 k=1, nnz
         ao(k) = a(k)
 2    continue
!
      return
!---------end-of-cperm--------------------------------------------------
!-----------------------------------------------------------------------
      end
!----------------------------------------------------------------------- 
      subroutine vperm (n, x, perm) 
      integer n, perm(n) 
      real*8 x(n)
!-----------------------------------------------------------------------
! this subroutine performs an in-place permutation of a real vector x 
! according to the permutation array perm(*), i.e., on return, 
! the vector x satisfies,
!
!	x(perm(j)) :== x(j), j=1,2,.., n
!
!-----------------------------------------------------------------------
! on entry:
!---------
! n 	= length of vector x.
! perm 	= integer array of length n containing the permutation  array.
! x	= input vector
!
! on return:
!---------- 
! x	= vector x permuted according to x(perm(*)) :=  x(*)
!
!----------------------------------------------------------------------c
!           Y. Saad, Sep. 21 1989                                      c
!----------------------------------------------------------------------c
! local variables 
      real*8 tmp, tmp1
!
      init      = 1
      tmp       = x(init)
      ii        = perm(init)
      perm(init)= -perm(init)
      k         = 0
!     
! loop
! 
 6    k = k+1
!
! save the chased element --
! 
      tmp1      = x(ii) 
      x(ii)     = tmp
      next      = perm(ii) 
      if (next .lt. 0 ) goto 65
!     
! test for end 
!
      if (k .gt. n) goto 101
      tmp       = tmp1
      perm(ii)  = - perm(ii)
      ii        = next 
!
! end loop 
!
      goto 6
!
! reinitilaize cycle --
!
 65   init      = init+1
      if (init .gt. n) goto 101
      if (perm(init) .lt. 0) goto 65
      tmp       = x(init)
      ii        = perm(init)
      perm(init)=-perm(init)
      goto 6
!     
 101  continue
      do 200 j=1, n
         perm(j) = -perm(j)
 200  continue 
!     
      return
!-------------------end-of-vperm--------------------------------------- 
!-----------------------------------------------------------------------
      end
!
      SUBROUTINE SET_PCGUINPUT(IFDPARAM)
      USE PCGUMODULE, ONLY:  IPC,ISCL,IORD,RCLOSEPCGU,RELAXPCGU,ILINMETH
      INTEGER IFDPARAM
C Simple option
      SELECT CASE ( IFDPARAM )
      CASE(1)
        ILINMETH=1
        IPC = 2
        ISCL = 0
        IORD = 0
        RCLOSEPCGU = 1.0e-1
        RELAXPCGU = 0.0
C Moderate
      CASE(2)
        ILINMETH=2
        IPC = 3
        ISCL = 0
        IORD = 0
        RCLOSEPCGU = 1.0e-1
        RELAXPCGU = 0.97
C Complex
      CASE(3)
        ILINMETH=2
        IPC = 3
        ISCL = 0
        IORD = 0
        RCLOSEPCGU = 1.0e-1
        RELAXPCGU = 0.97
      END SELECT
      RETURN
      END
      