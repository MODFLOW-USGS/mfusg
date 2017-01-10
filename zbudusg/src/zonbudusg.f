      MODULE ZONBUDMODULE
        INTEGER IPREC,NEQ,NA
        REAL, ALLOCATABLE, DIMENSION(:) ::BUFF
        REAL, ALLOCATABLE, DIMENSION(:) ::BUFFD
        INTEGER, ALLOCATABLE, DIMENSION(:)    ::IA
        INTEGER, ALLOCATABLE, DIMENSION(:)    ::JA
        REAL,    ALLOCATABLE, DIMENSION(:)    ::A
      END MODULE
C     ******************************************************************
C     Program to compute and print volumetric budgets over subregions
C     of a flow system that is being simulated using the USGS
C     unstructured Grid Model (USG).
C
C     Jan., 2017 -- Updated to terminate if the number of entries in
C       the budget file have changed from one step to the next.
C
C     ******************************************************************
C        SPECIFICATIONS:
      USE ZONBUDMODULE
      PARAMETER (NTRDIM=50,MXCOMP=100,MXZWCZ=50,MXZONE=999)
C-----   NTRDIM must be greater than or equal to the number of budget
C-----          terms, other than flow between zones, that will appear
C-----          the budget.  In the original model, there is a maximum
C-----          of 8 terms -- constant-head, storage, wells, rivers,
C-----          drains, recharge, general-head boundaries, and
C-----          evapotranspiration.
C-----   MXZONE is the maximum number of zones.
C-----   NZDIM  is the actual number of zones being used.
C-----   MXCOMP is the maximum number of composite zones.
C-----   MXZWCZ is the maximum number of numeric zones within each
C-----          composite zone.
C-----   LSTZON is a list of all of the zones.
C-----          used.
      ALLOCATABLE IZONE(:),ICH(:),IBUFF(:),
     1            VBVL(:,:,:),VBZNFL(:,:,:)

      DOUBLE PRECISION VBVL,VBZNFL
      DOUBLE PRECISION DELTD,PERTIMD,TOTIMD,VALD(20),DZERO,TOTIMDOLD
      DIMENSION ICOMP(MXZWCZ,MXCOMP),NZWCZ(MXCOMP),LSTZON(0:MXZONE)
      CHARACTER*10 NAMCOMP(MXCOMP)
      DIMENSION ITIME(2,10)
      CHARACTER*80 TITLE
      CHARACTER*80 NAME,BASENAME
      CHARACTER*16 VBNM(NTRDIM),TEXT,CTMP
      CHARACTER*1 METHOD,IANS
      CHARACTER*40 VERSON
      DIMENSION VAL(20)
      INCLUDE 'openspec.inc'
C     ------------------------------------------------------------------
      VERSON='ZONEBUDGET USG version 1.01'
C
C-----DEFINE INPUT AND OUTPUT UNITS AND INITIALIZE OTHER VARIABLES
      INZN1=10
      INZN2=11
      INBUD=12
      IOUT=0
      IUZBLST=0
      IUCSV=0
      IUCSV2=0
      K1=0
      K2=0
      MSUM=0
      DZERO=0.0
      NLIST=0
      NVAL=1
      TOTIMD=-1.0
      TOTIMDOLD=-1.0
C
C-----TELL THE USER WHAT THIS PROGRAM IS
      WRITE(*,*)
      WRITE(*,4) VERSON
4     FORMAT(1X,A/
     1' Program to compute a flow budget for subregions of a model using
     2'/' budget data from MODFLOW-USG')
C
C-----OPEN LISTING FILE(S)
      WRITE(*,*)
7     WRITE(*,*)' Enter a LISTING FILE for results',
     1                     ' or a base name and file types:'
      READ(*,'(A)') NAME
      LLOC=1
      CALL URWORD(NAME,LLOC,ISTART,ISTOP,0,I,R,0,IN)
      BASENAME=NAME(ISTART:ISTOP)
      CALL URWORD(NAME,LLOC,ISTART,ISTOP,1,I,R,0,IN)
      IF(NAME(ISTART:ISTOP).NE.' ') THEN
8       IF(NAME(ISTART:ISTOP).EQ.'CSV') THEN
          IUCSV=14
          OPEN(UNIT=IUCSV,FILE=TRIM(BASENAME)//'.csv',ERR=7)
          WRITE(*,*) 'CSV output file: ',TRIM(BASENAME)//'.csv'
        ELSE IF(NAME(ISTART:ISTOP).EQ.'CSV2') THEN
          IUCSV2=15
          OPEN(UNIT=IUCSV2,FILE=TRIM(BASENAME)//'.2.csv',ERR=7)
          WRITE(*,*) 'CSV2 output file: ',TRIM(BASENAME)//'.2.csv'
        ELSE IF(NAME(ISTART:ISTOP).EQ.'ZBLST') THEN
          IOUT=13
          IUZBLST=IOUT
          OPEN(UNIT=IOUT,FILE=TRIM(BASENAME)//'.zblst',ERR=7)
          WRITE(*,*) 'Standard Zonebudget output file: ',
     1           TRIM(BASENAME)//'.zblst'
        END IF
        CALL URWORD(NAME,LLOC,ISTART,ISTOP,1,I,R,0,IN)
        IF(NAME(ISTART:ISTOP).NE.' ') GO TO 8
      ELSE
        IOUT=13
        IUZBLST=IOUT
        OPEN(UNIT=IOUT,FILE=BASENAME,ERR=7)
      END IF
      IF(IOUT.EQ.0) THEN
        IOUT=13
        OPEN(UNIT=IOUT,FILE=TRIM(BASENAME)//'.log',ERR=7)
        WRITE(*,*) 'Zonebudget log file: ',
     1         TRIM(BASENAME)//'.log'
      END IF
C
C-----WRITE OUTPUT FILE
      WRITE(IOUT,4) VERSON
C
C-----OPEN THE DISU AND CELL-BY-CELL BUDGET FILES
9     WRITE(*,*) ' Enter the name of the DISU file:'
      READ(*,'(A)') NAME
      OPEN(UNIT=INBUD,FILE=NAME,STATUS='OLD',ERR=9)
      CALL DISURD(INBUD,IOUT)
      WRITE(*,*)
10    WRITE(*,*) ' Enter the name of the file containing CELL-BY-CELL BU
     1DGET TERMS:'
      READ(*,'(A)') NAME
      OPEN(UNIT=INBUD,FILE=NAME,STATUS='OLD',FORM=FORM,ACCESS=ACCESS,
     1               ERR=10)
      WRITE(IOUT,*)
      WRITE(IOUT,*) ' The cell-by-cell budget file is:'
      WRITE(IOUT,*) NAME
C
C-----Check for valid budget file, and allocate memory
      CALL BUDGETPRECISION(INBUD,NCOL,NROW,NLAY)
      IF(IPREC.LT.1) THEN
        WRITE(*,*) 'Stopping because budget file is invalid'
        STOP
      ELSEIF(IPREC.EQ.1) THEN
        WRITE(IOUT,*) ' Single precision budget file'
      ELSE IF(IPREC.EQ.2) THEN
        WRITE(IOUT,*) ' Double precision budget file'
      END IF
C
C-----READ A TITLE TO BE PRINTED IN THE LISTING
      WRITE(*,*)
      WRITE(*,*) ' Enter a TITLE to be printed in the listing:'
      READ(*,'(A)') TITLE
      WRITE(IOUT,*)
      WRITE(IOUT,'(1X,A)') TITLE
C
C-----OPEN THE ZONE FILE IF IT EXISTS
16    WRITE(*,*)
      WRITE(*,*) ' Enter the name of your ZONE INPUT FILE:'
      READ(*,'(A)') NAME
C
C-----IF NAME IS BLANK, TRY AGAIN
      IF(NAME.EQ.' ') GO TO 16
C
C-----OPEN ZONE FILE, AND CHECK GRID DIMENSIONS
      OPEN(UNIT=INZN1,FILE=NAME,STATUS='OLD',ERR=16)
      WRITE(IOUT,*)
      WRITE(IOUT,*) ' The zone file is:'
      WRITE(IOUT,*) NAME
C
C-----READ ZONE ARRAY
      ALLOCATE(ICH(NEQ))
      ALLOCATE(IBUFF(NEQ))
      ALLOCATE(IZONE(NEQ))
      CALL IZREAD(IZONE,NEQ,INZN1,IOUT)
C
C-----DONE WITH ZONE DEFINITION.  Create the zone list, LSTZON.
      CALL ZONCOUNT(NZDIM,LSTZON,MXZONE,IZONE,NEQ,IOUT)
      ALLOCATE (VBVL(2,NTRDIM,NZDIM))
      ALLOCATE (VBZNFL(2,0:NZDIM,0:NZDIM))
C
C-----READ COMPOSITE ZONES
      IF(NAME.NE.' ') THEN
         CALL INCOMP(ICOMP,NZWCZ,MXCOMP,MXZWCZ,NCOMP,INZN1,LSTZON,
     1               NZDIM,IOUT,NAMCOMP)
         CLOSE(UNIT=INZN1)
      END IF
C
C-----CHECK WHAT METHOD TO USE FOR SPECIFYING WHEN TO CALCULATE BUDGETS
50    WRITE(*,*)
      WRITE(*,*) ' Choose the option for specifying when budgets are cal
     1culated:'
      WRITE(*,*) ' A = ALL times stored in the budget file.'
      WRITE(*,*) ' P = For each time stored in the budget file, PROMPT u
     1ser.'
      WRITE(*,*) ' L = Enter a LIST of times.'
      READ(*,'(A)') METHOD
      IF(METHOD.EQ.'A' .OR. METHOD.EQ.'a') THEN
         METHOD='A'
      ELSE IF(METHOD.EQ.'P' .OR. METHOD.EQ.'p') THEN
         METHOD='P'
      ELSE IF(METHOD.EQ.'L' .OR. METHOD.EQ.'l') THEN
         METHOD='L'
         DO 60 I=1,10
         WRITE(*,*) ' Enter a time step, stress period at which to calcu
     1late budgets (0,0=done):'
         READ(*,*) ITIME(1,I),ITIME(2,I)
         IF(ITIME(1,I).EQ.0 .AND. ITIME(2,I).EQ.0) GO TO 65
60       CONTINUE
         I=11
65       NTIMES=I-1
      ELSE
         WRITE(*,*) 'Invalid choice; you must enter "A", "P", or "L"'
         GO TO 50
      END IF
      WRITE(*,*)
      ICALC=0
C
C
C-----READ BUDGET DATA AND ACCUMULATE AS LONG AS TIME REMAINS CONSTANT.
C-----WHEN TIME CHANGES, PRINT THE BUDGET, REINITIALIZE, AND START OVER
100   READ(INBUD,END=1000,ERR=1000) KSTP,KPER,TEXT,NREAL,NROW,NL
      ITYPE=0
      IF(NL.LT.0) THEN
        READ(INBUD) ITYPE
      END IF
      IF(ADJUSTL(TEXT).EQ.'FLOW JA FACE' ) THEN
        IF(NREAL.GT.NA) THEN
          WRITE(*,*) ' BUDGET FILE FOR FLOW JA FACE HAS MORE VALUES',
     1                  ' THAN NJAG:'
          WRITE(*,*) ' NUMBER OF JA FACE VALUES IN BUDGET FILE:',NREAL
          WRITE(*,*) ' NJAG VALUE:',NA
          STOP
        END IF
      ELSE
        IF((ITYPE.NE.3 .AND. ITYPE.NE.4) .AND. NREAL.NE.NEQ) THEN
          WRITE(*,*) ' BUDGET FILE HAS DIFFERENT NUMBER OF NODES THAN',
     1                  ' IN DISU FILE:'
          WRITE(*,*) ' NUMBER OF NODES IN BUDGET FILE:',NREAL
          WRITE(*,*) ' NUMBER OF NODES IN DISU FILE:',NEQ
          STOP
        END IF
      END IF
      IF(NL.LT.0) THEN
         TOTIMDOLD=TOTIMD
         IF(IPREC.EQ.1) THEN
           READ(INBUD) DELT,PERTIM,TOTIM
           DELTD=DELT
           PERTIMD=PERTIM
           TOTIMD=TOTIM
         ELSE
           READ(INBUD) DELTD,PERTIMD,TOTIMD
         END IF
         NVAL=1
         IF(ITYPE.EQ.5) THEN
            READ(INBUD) NVAL
            IF(NVAL.GT.1) THEN
               DO 101 N=2,NVAL
               READ(INBUD) CTMP
101            CONTINUE
            END IF
         END IF
         IF(ITYPE.EQ. 2 .OR. ITYPE.EQ.5) READ(INBUD) NLIST
      END IF
C
C-----CHECK IF STARTING A NEW TIME STEP
      IF(K1.NE.KSTP .OR. K2.NE.KPER) THEN
C
C-----IF STARTING A NEW TIME STEP, PRINT A BUDGET AND REINITIALIZE ALL
C-----BUDGET ACCUMULATORS
C-----AT THE VERY BEGINNING WHEN K1=K2=0, DON'T PRINT THE BUDGET BECAUSE
C-----NOTHING HAS BEEN ACCUMULATED YET
         IF(K1.NE.0 .AND. K2.NE.0 .AND. ICALC.NE.0) THEN
C
C-----GENERATE ZONE FLOW VALUES GOING FROM ONE ZONE INTO A LOWER ZONE --
C-----EG GOING FROM B TO A WHERE A IS A LOWER ZONE THAN B
C-----THESE ARE JUST THE SAME AS THE CORRESPONDING VALUES GOING
C-----FROM A TO B EXCEPT THAT INS AND OUTS ARE REVERSED
            DO 150 K=0,NZDIM-1
            DO 150 J=K+1,NZDIM
            DO 150 I=1,2
            VBZNFL(I,J,K)=VBZNFL(3-I,K,J)
150         CONTINUE
            IF(IUZBLST.GT.0) THEN
              CALL SUBPR(K1,K2,VBNM,VBVL,VBZNFL,MSUM,IOUT,NTRDIM,
     1            LSTZON,NZDIM,TITLE)
              IF(NCOMP.GT.0) CALL COMPPR(K1,K2,VBNM,VBVL,VBZNFL,MSUM,
     1            IOUT,NTRDIM,NZDIM,TITLE,ICOMP,NZWCZ,NCOMP,
     2            MXCOMP,MXZWCZ,LSTZON,NAMCOMP)
            END IF
            IF(IUCSV.GT.0) CALL CSVSUBPR(K1,K2,VBNM,VBVL,VBZNFL,MSUM,
     1            IUCSV,NTRDIM,NZDIM,TITLE,TOTIMDOLD,LSTZON)
            IF(IUCSV2.GT.0) CALL CSVSUBPR2(K1,K2,VBNM,VBVL,VBZNFL,MSUM,
     2            IUCSV2,NTRDIM,NZDIM,TITLE,TOTIMDOLD,LSTZON,IOUT)
         END IF
C
C-----SET TIME CHANGE INDICATORS
         K1=KSTP
         K2=KPER
C
C-----DECIDE WHETHER OR NOT TO CALCULATE THE BUDGET FOR THIS TIME STEP
         ICALC=0
         IF(METHOD.EQ.'A') THEN
            ICALC=1
         ELSE IF(METHOD.EQ.'P') THEN
102         WRITE(*,105) KSTP,KPER
105         FORMAT(1X,'Do you want to calculate budgets for time step',
     1             I4,' in stress period',I4,' (Y/N)?')
            READ(*,'(A)') IANS
            IF(IANS.EQ.'Y' .OR. IANS.EQ.'y') THEN
               ICALC=1
            ELSE IF(IANS.EQ.'N' .OR. IANS.EQ.'n') THEN
            ELSE
               GO TO 102
            END IF
         ELSE
            DO 110 I=1,NTIMES
            IF(KSTP.NE.ITIME(1,I) .OR. KPER.NE.ITIME(2,I)) GO TO 110
            ICALC=1
            GO TO 120
110         CONTINUE
120         CONTINUE
         END IF
         IF(ICALC.EQ.0) THEN
            WRITE(*,121) KSTP,KPER
121         FORMAT(' Skipping the budget for time step',I4,
     1       ' in stress period',I4)
         ELSE
            MSUM=1
            DO 210 I=1,NZDIM
            DO 210 J=1,NTRDIM
            DO 210 K=1,2
            VBVL(K,J,I)=DZERO
210         CONTINUE
            DO 220 I=0,NZDIM
            DO 220 J=0,NZDIM
            DO 220 K=1,2
            VBZNFL(K,J,I)=DZERO
220         CONTINUE
            WRITE(*,221) KSTP,KPER
221         FORMAT(' Computing the budget for time step',I4,
     1       ' in stress period',I4)
         END IF
      END IF
C
C-----READ THE BUDGET TERM DATA UNDER THE FOLLOWING CONDITIONS:
      IF(ITYPE.EQ.0 .OR. ITYPE.EQ.1) THEN
C  FULL 3-D ARRAY
         IF(IPREC.EQ.1) THEN
           READ(INBUD) (BUFF(J),J=1,NREAL)
           DO 250 J=1,NREAL
           BUFFD(J)=BUFF(J)
250        CONTINUE
         ELSE
           READ(INBUD) (BUFFD(J),J=1,NREAL)
         END IF
      ELSE IF(ITYPE.EQ.3) THEN
C  1-LAYER ARRAY WITH LAYER INDICATOR ARRAY
         BUFFD=DZERO
         READ(INBUD) (IBUFF(N),N=1,NREAL)
         IF(IPREC.EQ.1) THEN
           READ(INBUD) (BUFF(N),N=1,NREAL)
           DO 265 N=1,NREAL
           BUFFD(N)=BUFF(N)
265        CONTINUE
         ELSE
           READ(INBUD) (BUFFD(N),N=1,NREAL)
         END IF
         DO 270 I=1,NREAL
         IF(I.NE.IBUFF(I)) THEN
            BUFFD(IBUFF(I))=BUFFD(I)
            BUFFD(I)=DZERO
          END IF
270      CONTINUE
      ELSE IF(ITYPE.EQ.4) THEN
C  1-LAYER ARRAY THAT DEFINES LAYER 1
         BUFFD=DZERO
         IF(IPREC.EQ.1) THEN
           READ(INBUD) (BUFF(N),N=1,NREAL)
           DO 275 N=1,NREAL
           BUFFD(N)=BUFF(N)
275        CONTINUE
         ELSE
           READ(INBUD) (BUFFD(N),N=1,NREAL)
         END IF
      ELSE IF(ICALC.EQ.0 .AND. NLIST.GT.0) THEN
C  LIST -- READ ONLY IF THE VALUES NEED TO BE SKIPPED.
C  ACCM will read the list if the budget is being computed this time step.
         DO 300 N=1,NLIST
         IF(IPREC.EQ.1) THEN
           READ(INBUD) LOC,(VAL(I),I=1,NVAL)
         ELSE
           READ(INBUD) LOC,(VALD(I),I=1,NVAL)
         END IF
300      CONTINUE
      END IF
C
C-----BEFORE PROCESSING A BUDGET TERM, CHECK IF THERE IS ENOUGH SPACE
      IF(MSUM.GT.NTRDIM) THEN
         WRITE(*,*) 'PROGRAM PARAMETER NTRDIM IS TOO SMALL'
         WRITE(*,*) 'PARAMETER NTRDIM IS CURRENTLY',NTRDIM
         WRITE(*,*) 'CHANGE NTRDIM TO BE EQUAL TO THE MAXIMUM NUMBER OF
     1BUDGET TERMS'
         STOP
      END IF
C
C-----PROCESS A BUDGET TERM AND THEN START THE READ PROCESS OVER
      IF(ICALC.NE.0) CALL ACCM(IZONE,ICH,VBNM,VBVL,
     1           VBZNFL,MSUM,TEXT,NTRDIM,NZDIM,MSUMCH,
     2           ITYPE,NLIST,INBUD,NVAL,NREAL)
      GO TO 100
C
C  END OF FILE. PRINT FINAL BUDGET IF FLAG IS SET.
1000  IF(ICALC.NE.0) THEN
C-----GENERATE ZONE FLOW VALUES GOING FROM ONE ZONE INTO A LOWER ZONE --
C-----EG GOING FROM B TO A WHERE A IS A LOWER ZONE THAN B
C-----THESE ARE JUST THE SAME AS THE CORRESPONDING VALUES GOING
C-----FROM A TO B EXCEPT THAT INS AND OUTS ARE REVERSED
        DO 1050 K=0,NZDIM-1
        DO 1050 J=K+1,NZDIM
        DO 1050 I=1,2
        VBZNFL(I,J,K)=VBZNFL(3-I,K,J)
1050    CONTINUE
        IF(IUZBLST.GT.0) THEN
          CALL SUBPR(K1,K2,VBNM,VBVL,VBZNFL,MSUM,
     1        IOUT,NTRDIM,LSTZON,NZDIM,TITLE)
          IF(NCOMP.GT.0) CALL COMPPR(K1,K2,VBNM,VBVL,
     1      VBZNFL,MSUM,IOUT,NTRDIM,NZDIM,TITLE,ICOMP,NZWCZ,
     2      NCOMP,MXCOMP,MXZWCZ,LSTZON,NAMCOMP)
        END IF
        IF(IUCSV.GT.0) CALL CSVSUBPR(K1,K2,VBNM,VBVL,
     1     VBZNFL,MSUM,IUCSV,NTRDIM,NZDIM,TITLE,TOTIMD,LSTZON)
        IF(IUCSV2.GT.0) CALL CSVSUBPR2(K1,K2,VBNM,VBVL,
     1     VBZNFL,MSUM,IUCSV2,NTRDIM,NZDIM,TITLE,TOTIMD,LSTZON,IOUT)
      END IF
      STOP
C
C-----EMPTY BUDGET FILE
2000  WRITE(*,*) 'CELL-BY-CELL FLOW TERM FILE WAS EMPTY'
      STOP
C
      END
      SUBROUTINE DISURD(IN,IOUT)
C     ******************************************************************
C     ROUTINE TO READ THE DISU FILE AND CONSTRUCT IA AND JA
C     ******************************************************************
C        SPECIFICATIONS:
      USE ZONBUDMODULE
      INTEGER, ALLOCATABLE,DIMENSION(:)  ::NODELAY
      INTEGER, ALLOCATABLE,DIMENSION(:)  ::IAC
      CHARACTER*24 ANAME
      CHARACTER*120 LINE
C
5     READ(IN,'(A)') LINE
      IF(LINE.EQ.' ') GO TO 5
      IF(LINE(1:1).EQ.'#') THEN
        WRITE(IOUT,'(A)') LINE
        GO TO 5
      END IF
      LLOC=1
      CALL URWORD(LINE,LLOC,L1,L2,2,NEQ,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,L1,L2,2,NLAY,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,L1,L2,2,NJAG,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,L1,L2,2,IVSD,R,IOUT,IN)
      CALL URWORD(LINE,LLOC,L1,L2,2,NPER,R,IOUT,IN)
      WRITE(IOUT,*) 'NEQ=',NEQ
      WRITE(IOUT,*) 'NLAY=',NLAY
      WRITE(IOUT,*) 'NJAG=',NJAG
      WRITE(IOUT,*) 'IVSD=',IVSD
      NA=NJAG
      ALLOCATE (BUFF(NA))
      ALLOCATE (BUFFD(NA))
      ALLOCATE (IA(NEQ+1))
      ALLOCATE (JA(NA))
      ALLOCATE (NODELAY(NLAY))
      ALLOCATE (IAC(NEQ))
C
C LAYCBD
      READ(IN,*) (II,I=1,NLAY)
C NODELAY
      ANAME='NODELAY'
      CALL U1DINT(NODELAY,ANAME,NLAY,0,IN,IOUT)
C TOP
      DO 10 K=1,NLAY
      ANAME='TOP'
      CALL U1DREL(BUFF,ANAME,NODELAY(K),IN,IOUT)
10    CONTINUE
C BOT
      DO 20 K=1,NLAY
      ANAME='BOT'
      CALL U1DREL(BUFF,ANAME,NODELAY(K),IN,IOUT)
20    CONTINUE
C  AREA
        ANAME='AREA'
      IF(IVSD.EQ.-1) THEN
        CALL U1DREL(BUFF,ANAME,NODELAY(1),IN,IOUT)
      ELSE
        DO 30 K=1,NLAY
        CALL U1DREL(BUFF,ANAME,NODELAY(K),IN,IOUT)
30    CONTINUE
      END IF
C
C  IAC
      ANAME='IAC'
      CALL U1DINT(IAC,ANAME,NEQ,0,IN,IOUT)
      IA(1)=1
      DO 50 I=2,NEQ+1
      IA(I)=IA(I-1)+IAC(I-1)
50    CONTINUE
C  JA
      ANAME='JA'
      CALL U1DINT(JA,ANAME,NA,0,IN,IOUT)
C
c          do 100 N=1,NEQ
c          write(IOUT,*) (JA(J),J=IA(N),IA(N+1)-1)
c 100    continue
      RETURN
      END
      SUBROUTINE IZREAD(IZONE,NEQ,INZN1,IOUT)
C     ******************************************************************
C     ROUTINE TO INPUT ZONE MATRIX, IZONE
C       INZN1 IS INPUT UNIT
C       IOUT IS OUTPUT UNIT
C     ******************************************************************
C        SPECIFICATIONS:
      DIMENSION IZONE(NEQ)
      CHARACTER*80 NAME,LINE
      CHARACTER*24 ANAME
C     ------------------------------------------------------------------
      IPRN=0
C
      READ(INZN1,*) N
      IF(N.NE.NEQ) THEN
         WRITE(*,*) 'MISMATCH BETWEEN NUMBER OF NODES IN DISU FILE AND',
     1 ' ZONE FILE:'
         WRITE(*,*) 'NODES IN ZONE FILE:',N
         WRITE(*,*) 'NODES IN DISU FILE:',NEQ
         STOP
      END IF
C
C-----READ RECORDS
      ANAME='ZONES'
      CALL U1DINT(IZONE,ANAME,NEQ,0,INZN1,IOUT)
C      READ(INZN1,'(20I2)') (IZONE(J),J=1,NEQ)
C
C-----CHECK FOR NEGATIVE IZONE VALUES
320   DO 400 N=1,NEQ
      IF(IZONE(N).LT.0) THEN
         WRITE(*,*) ' NEGATIVE ZONE AT EQUATION):',N
         STOP
      END IF
400   CONTINUE
C
C-----IF PRINT CODE (IPRN) =>0 THEN PRINT ARRAY VALUES.
      IF(IPRN.LT.0) GO TO 1000
C
C-----PRINT EACH ROW IN THE ARRAY.
c      WRITE(IOUT,'(A)') 'Equation   Zone'
c      DO 430 N=1,NEQ
c      WRITE(IOUT,423) N,IZONE(N)
c423   FORMAT(1X,I10,I5)
c430   CONTINUE
C
C
C-----RETURN
1000  RETURN
      END
      SUBROUTINE ZONCOUNT(NZDIM,LSTZON,MXZONE,IZONE,NEQ,IOUT)
C     ******************************************************************
C     Create Zone list
C     ******************************************************************
      DIMENSION LSTZON(0:MXZONE),IZONE(NEQ)
C     ------------------------------------------------------------------
      LSTZON(0)=-1
      NZDIM=0
      DO 100 N=1,NEQ
      IZ=IZONE(N)
      IF(IZ.EQ.0) THEN
        LSTZON(0)=0
      ELSE
        IF(NZDIM.EQ.0) THEN
          NZDIM=NZDIM+1
          LSTZON(NZDIM)=IZ
        ELSE
          DO 70 L=1,NZDIM
          IF(IZ.EQ.LSTZON(L)) THEN
             GO TO 100
          ELSE IF(IZ.LT.LSTZON(L)) THEN
C  Found a new zone
             DO 60 M=NZDIM,L,-1
             LSTZON(M+1)=LSTZON(M)
60           CONTINUE
             LSTZON(L)=IZ
             NZDIM=NZDIM+1
             GO TO 100
          END IF
70        CONTINUE
          NZDIM=NZDIM+1
          LSTZON(NZDIM)=IZ
        END IF
      END IF
100   CONTINUE
C
      WRITE(*,*)
      WRITE(*,*) NZDIM,' zones.'
      WRITE(IOUT,*) NZDIM,' zones.'
      IF(NZDIM.EQ.0) THEN
         WRITE(*,*) ' Stopping because there are no zones'
         STOP
      END IF
      WRITE(*,195) (LSTZON(M),M=1,NZDIM)
195   FORMAT(20I5)
      WRITE(IOUT,195) (LSTZON(M),M=1,NZDIM)
C
C  Change IZONE to the zone index number
      DO 300 N=1,NEQ
        DO 250 M=0,NZDIM
        IF(IZONE(N).EQ.LSTZON(M)) THEN
          IZONE(N)=M
          GO TO 300
        END IF
250     CONTINUE
300   CONTINUE
C
      RETURN
      END
      SUBROUTINE INCOMP(ICOMP,NZWCZ,MXCOMP,MXZWCZ,NCOMP,INZN1,
     1                  LSTZON,NZDIM,IOUT,NAMCOMP)
C     ******************************************************************
C     READ COMPOSITE ZONES
C     ******************************************************************
C       SPECIFICATIONS:
      DIMENSION ICOMP(MXZWCZ,MXCOMP),NZWCZ(MXCOMP),LSTZON(0:NZDIM)
      CHARACTER*1000 LINE
      CHARACTER*10 NAMCOMP(MXCOMP)
C     ------------------------------------------------------------------
C
C-----READ THE COMPOSITE ZONES
      DO 10 I=1,MXCOMP
      READ(INZN1,'(A)',END=20) LINE
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,0,IDUM,RDUM,IOUT,INZN1)
      IF(LINE(ISTART:ISTART).GE.'0' .AND.
     1              LINE(ISTART:ISTART).LE.'9') THEN
        NAMCOMP(I)=' '
        WRITE(NAMCOMP(I),2) I
2       FORMAT('CZ',I3.3)
        LLOC=1
      ELSE
        NAMCOMP(I)=LINE(ISTART:ISTOP)
      END IF
      DO 3 J=1,MXZWCZ
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICOMP(J,I),RDUM,IOUT,INZN1)
      IF(ICOMP(J,I).LE.0) GO TO 10
3     CONTINUE
10    CONTINUE
      I=MXCOMP+1
20    NCOMP=I-1
      IF(NCOMP.EQ.0) RETURN
C
C-----FIND HOW MANY ZONES MAKE UP EACH COMPOSITE ZONE
      DO 40 I=1,NCOMP
        DO 30 J=1,MXZWCZ
          IF(ICOMP(J,I).LE.0) GO TO 35
          DO 25 M=1,NZDIM
            IF(ICOMP(J,I).EQ.LSTZON(M)) THEN
              ICOMP(J,I)=M
              GO TO 30
            END IF
25        CONTINUE
          WRITE(IOUT,26) NAMCOMP(I),ICOMP(J,I)
26        FORMAT(1X,'Nonexistent zone specified for Composite Zone ',
     1                A,':',I5)
          GO TO 35
30      CONTINUE
        J=MXZWCZ+1
35      NZWCZ(I)=J-1
        IF(NZWCZ(I).EQ.0) THEN
           NCOMP=I-1
           IF(NCOMP.EQ.0) RETURN
           GO TO 50
        END IF
40    CONTINUE
C
C-----WRITE THE COMPOSITE ZONES
50    WRITE(IOUT,*)
      WRITE(IOUT,52) NCOMP
52    FORMAT(1X,I3,' Composite Zones:')
      DO 60 I=1,NCOMP
      WRITE(IOUT,54) NAMCOMP(I),(LSTZON(ICOMP(J,I)),J=1,NZWCZ(I))
54    FORMAT(1X,'Composite Zone ',A,':',15I4/(27X,15I4))
60    CONTINUE
C
      RETURN
      END
      SUBROUTINE ACCM(IZONE,ICH,VBNM,VBVL,VBZNFL,
     1                MSUM,TEXT,NTRDIM,NZDIM,MSUMCH,
     2                ITYPE,NLIST,INBUD,NVAL,NREAL)
C     ******************************************************************
C     ACCUMULATE VOLUMETRIC BUDGET FOR ZONES
C     ******************************************************************
      USE ZONBUDMODULE
      DIMENSION VBVL(2,NTRDIM,NZDIM),
     1  VBZNFL(2,0:NZDIM,0:NZDIM),IZONE(NEQ),
     2  ICH(NEQ)
      DOUBLE PRECISION VBVL,VBZNFL,DBUFF
      CHARACTER*16 VBNM(NTRDIM),TEXT
      DIMENSION VAL(20)
      DOUBLE PRECISION VALD(20),DZERO
C     ------------------------------------------------------------------
      DZERO=0.0
C
C-----CHECK FOR INTERNAL FLOW TERMS, WHICH ARE USED TO CALCULATE FLOW
C-----BETWEEN ZONES, AND CONSTANT-HEAD TERMS
      IF(TEXT.EQ.'   CONSTANT HEAD') GO TO 200
      IF(ADJUSTL(TEXT).EQ.'FLOW JA FACE') GO TO 300
C
C-----NOT AN INTERNAL FLOW TERM, SO MUST BE A SOURCE TERM OR STORAGE
C-----ACCUMULATE THE FLOW BY ZONE
      IF(ITYPE.EQ.2 .OR. ITYPE.EQ.5) THEN
C  LIST
         IF(NLIST.GT.0) THEN
            DO 80 N=1,NLIST
            IF(IPREC.EQ.1) THEN
              READ(INBUD) ICELL,(VAL(I),I=1,NVAL)
              DO 45 I=1,NVAL
              VALD(I)=VAL(I)
45            CONTINUE
            ELSE
              READ(INBUD) ICELL,(VALD(I),I=1,NVAL)
            END IF
            NZ=IZONE(ICELL)
            IF(NZ.EQ.0) GO TO 80
            DBUFF=VALD(1)
            IF(DBUFF.EQ.DZERO) THEN
            ELSE IF(DBUFF.LT.DZERO) THEN
               VBVL(2,MSUM,NZ)=VBVL(2,MSUM,NZ)-DBUFF
            ELSE
               VBVL(1,MSUM,NZ)=VBVL(1,MSUM,NZ)+DBUFF
            END IF
80          CONTINUE
         END IF
      ELSE
C  ARRAY -- BUFFD already has the data
         DO 100 N=1,NEQ
         NZ=IZONE(N)
         IF(NZ.EQ.0) GO TO 100
         DBUFF=BUFFD(N)
         IF(DBUFF.EQ.DZERO) THEN
         ELSE IF(DBUFF.LT.DZERO) THEN
            VBVL(2,MSUM,NZ)=VBVL(2,MSUM,NZ)-DBUFF
         ELSE
            VBVL(1,MSUM,NZ)=VBVL(1,MSUM,NZ)+DBUFF
         END IF
  100    CONTINUE
      END IF
C
C-----SAVE THE TERM NAME AND KEEP TRACK OF THE NUMBER OF TERMS
      VBNM(MSUM)=TEXT
      MSUM=MSUM+1
      RETURN
C
C-----CONSTANT-HEAD FLOW -- DON'T ACCUMULATE THE CELL-BY-CELL VALUES FOR
C-----CONSTANT-HEAD FLOW BECAUSE THEY MAY INCLUDE PARTIALLY CANCELING
C-----INS AND OUTS.  USE CONSTANT-HEAD TERM TO IDENTIFY WHERE CONSTANT-
C-----HEAD CELLS ARE AND THEN USE FACE FLOWS TO DETERMINE THE AMOUNT OF
C-----FLOW.  STORE CONSTANT-HEAD LOCATIONS IN ICH ARRAY.
200   IF(ITYPE.EQ.2 .OR. ITYPE.EQ.5) THEN
         DO 240 N=1,NEQ
          ICH(N)=0
240      CONTINUE
         IF(NLIST.GT.0) THEN
            DO 250 N=1,NLIST
            IF(IPREC.EQ.1) THEN
              READ(INBUD) ICELL,(VAL(I),I=1,NVAL)
            ELSE
              READ(INBUD) ICELL,(VALD(I),I=1,NVAL)
            END IF
            ICH(ICELL)=1
250         CONTINUE
         END IF
      ELSE
         DO 260 N=1,NEQ
         ICH(N)=0
         IF(BUFFD(N).NE.DZERO) ICH(N)=1
260      CONTINUE
      END IF
      VBNM(MSUM)=TEXT
      MSUMCH=MSUM
      MSUM=MSUM+1
      RETURN
C
C-----"FLOW JA FACE"  COMPUTE FLOW BETWEEN ZONES AND CH FLOW.
C-----COMPUTE FLOW ONLY BETWEEN A ZONE AND A HIGHER ZONE -- FLOW FROM
C-----ZONE 4 TO 3 IS THE NEGATIVE OF FLOW FROM 3 TO 4.
300   DO 340 N=1,NEQ
      NZ=IZONE(N)
        DO 330 M=IA(N)+1,IA(N+1)-1
C  Flow between zones.
        JL=JA(M)
        NZL=IZONE(JL)
        IF(NZL.GT.NZ) THEN
C  Don't include CH to CH flow (can occur if CHTOCH option is used)
          IF(ICH(N).EQ.0 .OR. ICH(JL).EQ.0) THEN
             DBUFF=BUFFD(M)
             IF(DBUFF.LT.DZERO) THEN
                VBZNFL(2,NZ,NZL)=VBZNFL(2,NZ,NZL)-DBUFF
             ELSE
                VBZNFL(1,NZ,NZL)=VBZNFL(1,NZ,NZL)+DBUFF
             END IF
          END IF
        END IF
C Constant head
        IF(ICH(N).EQ.1 .AND. NZ.NE.0 .AND. ICH(JL).NE.1) THEN
          DBUFF=BUFFD(M)
          IF(DBUFF.EQ.DZERO) THEN
          ELSE IF(DBUFF.LT.DZERO) THEN
             VBVL(1,MSUMCH,NZ)=VBVL(1,MSUMCH,NZ)-DBUFF
          ELSE
             VBVL(2,MSUMCH,NZ)=VBVL(2,MSUMCH,NZ)+DBUFF
          END IF
        END IF
  330 CONTINUE
  340 CONTINUE
C
      RETURN
C
      END
      SUBROUTINE SUBPR(KSTP,KPER,VBNM,VBVL,VBZNFL,MSUM,IOUT,
     1               NTRDIM,LSTZON,NZDIM,TITLE)
C     ******************************************************************
C     COMPUTE TOTALS AND DIFFERENCES FOR ZONES AND PRINT BUDGET
C     ******************************************************************
C       SPECIFICATIONS:
      DIMENSION VBVL(2,NTRDIM,NZDIM),VBZNFL(2,0:NZDIM,0:NZDIM),
     1          LSTZON(0:NZDIM)
      DOUBLE PRECISION VBVL,VBZNFL,TOTOUT,TOTIN,TOTBD,DHUN,DTWO,ZERO
      CHARACTER*16 VBNM(NTRDIM)
      CHARACTER*80 TITLE
C     ------------------------------------------------------------------
      ZERO=0.0
C
C-----THE NUMBER OF FLOW TERMS OTHER THAN FLOW BETWEEN ZONES IS MSUM-1
      MTOT=MSUM-1
      DHUN=100.
      DTWO=2.
C
C-----FOR EACH ZONE, CALCULATE BUDGET TOTALS AND PRINT
      DO 500 N=1,NZDIM
C
C
C-----COMPUTE TOTAL INS AND OUTS
      TOTOUT=ZERO
      TOTIN=ZERO
      DO 100 I=1,MTOT
      TOTIN=TOTIN+VBVL(1,I,N)
      TOTOUT=TOTOUT+VBVL(2,I,N)
100   CONTINUE
      DO 150 I=0,NZDIM
      TOTIN=TOTIN+VBZNFL(1,N,I)
      TOTOUT=TOTOUT+VBZNFL(2,N,I)
150   CONTINUE
C
C-----CALCULATE THE DIFFERENCE BETWEEN IN AND OUT AND THE PERCENT ERROR
      IF(TOTIN.EQ.ZERO .AND. TOTOUT.EQ.ZERO) GO TO 500
      TOTBD=TOTIN-TOTOUT
      PERCNT=DHUN*TOTBD/((TOTIN+TOTOUT)/DTWO)
C
C
C     ---PRINT BUDGET---
C
C-----PRINT THE TITLE
      WRITE(IOUT,'(1H1,/1X,A)') TITLE
      WRITE(IOUT,*)
      WRITE(IOUT,601) LSTZON(N),KSTP,KPER
C
C-----PRINT THE IN TERMS
      WRITE(IOUT,*)
      WRITE(IOUT,602)
      DO 200 I=1,MTOT
      WRITE(IOUT,603) VBNM(I),VBVL(1,I,N)
200   CONTINUE
      DO 250 I=0,NZDIM
      IF(VBZNFL(1,N,I).NE.ZERO .OR. VBZNFL(2,N,I).NE.ZERO)
     1              WRITE(IOUT,609) LSTZON(I),LSTZON(N),VBZNFL(1,N,I)
250   CONTINUE
      WRITE(IOUT,*)
      WRITE(IOUT,604) TOTIN
C
C-----PRINT THE OUT TERMS
      WRITE(IOUT,*)
      WRITE(IOUT,605)
      DO 300 I=1,MTOT
      WRITE(IOUT,603) VBNM(I),VBVL(2,I,N)
300   CONTINUE
      DO 350 I=0,NZDIM
      IF(VBZNFL(1,N,I).NE.ZERO .OR. VBZNFL(2,N,I).NE.ZERO)
     1              WRITE(IOUT,609) LSTZON(N),LSTZON(I),VBZNFL(2,N,I)
350   CONTINUE
      WRITE(IOUT,*)
      WRITE(IOUT,606) TOTOUT
C
C-----PRINT IN-OUT AND THE PERCENT ERROR
      WRITE(IOUT,*)
      WRITE(IOUT,607) TOTBD
      WRITE(IOUT,*)
      WRITE(IOUT,608) PERCNT
C
  500 CONTINUE
C
      RETURN
C
C    ---FORMATS---
C
  601 FORMAT(5X,'Flow Budget for Zone',I3,
     1  ' at Time Step',I4,' of Stress Period',I4/5X,61('-'))
  602 FORMAT(23X,'Budget Term',5X,'Flow (L**3/T)'/
     1     23X,29('-')//13X,'IN:'/13X,'---')
  603 FORMAT(18X,A,' =',G14.5)
  604 FORMAT(26X,'Total IN =',G14.5)
  605 FORMAT(13X,'OUT:'/13X,4('-'))
  606 FORMAT(25X,'Total OUT =',G14.5)
  607 FORMAT(26X,'IN - OUT =',G14.5)
  608 FORMAT(15X,'Percent Discrepancy =',F20.2)
  609 FORMAT(19X,'Zone',I4,' to',I4,' =',G14.5)
      END
      SUBROUTINE COMPPR(KSTP,KPER,VBNM,VBVL,VBZNFL,MSUM,IOUT,NTRDIM,
     1   NZDIM,TITLE,ICOMP,NZWCZ,NCOMP,MXCOMP,MXZWCZ,LSTZON,NAMCOMP)
C     ******************************************************************
C     COMPUTE BUDGET TOTALS FOR COMPOSITE ZONES AND PRINT BUDGET
C     ******************************************************************
C       SPECIFICATIONS:
      DIMENSION VBVL(2,NTRDIM,NZDIM),VBZNFL(2,0:NZDIM,0:NZDIM)
      DIMENSION ICOMP(MXZWCZ,MXCOMP),NZWCZ(MXCOMP),LSTZON(0:NZDIM)
      DOUBLE PRECISION VBVL,VBZNFL
      DOUBLE PRECISION TOTOUT,TOTIN,TOTBD,DHUN,DTWO,TSUM1,TSUM2
      CHARACTER*16 VBNM(NTRDIM)
      CHARACTER*80 TITLE
      CHARACTER*10 NAMCOMP(NCOMP)
C     ------------------------------------------------------------------
      DHUN=100.
      DTWO=2.
C
C-----THE NUMBER OF FLOW TERMS OTHER THAN FLOW BETWEEN ZONES IS MSUM-1
      MTOT=MSUM-1
C
C-----FOR EACH COMPOSITE ZONE, CALCULATE BUDGET TOTALS AND PRINT
      DO 500 M=1,NCOMP
C
C
C-----COMPUTE TOTAL INS AND OUTS
      TOTOUT=0.
      TOTIN=0.
C
C-----TOTAL THE BUDGET TERMS
      DO 100 I=1,MTOT
      DO 100 J=1,NZWCZ(M)
      N=ICOMP(J,M)
      TOTIN=TOTIN+VBVL(1,I,N)
      TOTOUT=TOTOUT+VBVL(2,I,N)
100   CONTINUE
C
C-----TOTAL THE FLOW ACROSS ZONE BOUNDARIES
      DO 150 I=0,NZDIM
C
C-----SKIP FLOW TO ANY ZONES THAT ARE PART OF COMPOSITE ZONE
      DO 130 J=1,NZWCZ(M)
      N=ICOMP(J,M)
      IF(I.EQ.N) GO TO 150
130   CONTINUE
      DO 140 J=1,NZWCZ(M)
      N=ICOMP(J,M)
      TOTIN=TOTIN+VBZNFL(1,N,I)
      TOTOUT=TOTOUT+VBZNFL(2,N,I)
140   CONTINUE
150   CONTINUE
C
C-----CALCULATE THE DIFFERENCE BETWEEN IN AND OUT AND THE PERCENT ERROR
      IF(TOTIN.EQ.0. .AND. TOTOUT.EQ.0.) GO TO 500
      TOTBD=TOTIN-TOTOUT
      PERCNT=DHUN*TOTBD/((TOTIN+TOTOUT)/DTWO)
C
C
C-----PRINT BUDGET---
C
C-----PRINT THE TITLE
      WRITE(IOUT,'(1H1,/1X,A)') TITLE
      WRITE(IOUT,*)
      WRITE(IOUT,601) NAMCOMP(M),KSTP,KPER
      WRITE(IOUT,*)
      WRITE(IOUT,611) NAMCOMP(M),(LSTZON(ICOMP(J,M)),J=1,NZWCZ(M))
C
C-----TOTAL AND PRINT THE IN BUDGET TERMS
      WRITE(IOUT,*)
      WRITE(IOUT,602)
      DO 200 I=1,MTOT
      TSUM1=0.
      DO 180 J=1,NZWCZ(M)
      N=ICOMP(J,M)
      TSUM1=TSUM1+VBVL(1,I,N)
180   CONTINUE
      WRITE(IOUT,603) VBNM(I),TSUM1
200   CONTINUE
C
C-----TOTAL AND PRINT THE IN FLOW ACROSS ZONE BOUNDARIES
      DO 250 I=0,NZDIM
      DO 230 J=1,NZWCZ(M)
      N=ICOMP(J,M)
      IF(N.EQ.I) GO TO 250
      TSUM1=0.
      TSUM2=0.
230   CONTINUE
      DO 240  J=1,NZWCZ(M)
      N=ICOMP(J,M)
      TSUM1=TSUM1+VBZNFL(1,N,I)
      TSUM2=TSUM2+VBZNFL(2,N,I)
240   CONTINUE
      IF(TSUM1.NE.0. .OR. TSUM2.NE.0.)
     1            WRITE(IOUT,609) LSTZON(I),NAMCOMP(M),TSUM1
250   CONTINUE
C
C-----WRITE THE TOTALS OF ALL INS
      WRITE(IOUT,*)
      WRITE(IOUT,604) TOTIN
C
C-----TOTAL AND PRINT THE OUT BUDGET TERMS
      WRITE(IOUT,*)
      WRITE(IOUT,605)
      DO 300 I=1,MTOT
      TSUM2=0.
      DO 280 J=1,NZWCZ(M)
      N=ICOMP(J,M)
      TSUM2=TSUM2+VBVL(2,I,N)
280   CONTINUE
      WRITE(IOUT,603) VBNM(I),TSUM2
300   CONTINUE
C
C-----TOTAL AND PRINT THE OUT FLOW ACROSS ZONE BOUNDARIES
      DO 350 I=0,NZDIM
      DO 330 J=1,NZWCZ(M)
      N=ICOMP(J,M)
      IF(N.EQ.I) GO TO 350
330   CONTINUE
      TSUM1=0.
      TSUM2=0.
      DO 340  J=1,NZWCZ(M)
      N=ICOMP(J,M)
      TSUM1=TSUM1+VBZNFL(1,N,I)
      TSUM2=TSUM2+VBZNFL(2,N,I)
340   CONTINUE
      IF(TSUM1.NE.0. .OR. TSUM2.NE.0.)
     1            WRITE(IOUT,610) NAMCOMP(M),LSTZON(I),TSUM2
350   CONTINUE
C
C-----WRITE TOTAL OUTS
      WRITE(IOUT,*)
      WRITE(IOUT,606) TOTOUT
C
C-----PRINT IN-OUT AND THE PERCENT ERROR
      WRITE(IOUT,*)
      WRITE(IOUT,607) TOTBD
      WRITE(IOUT,*)
      WRITE(IOUT,608) PERCNT
C
  500 CONTINUE
C
      RETURN
C
C    ---FORMATS---
C
  601 FORMAT(5X,'Flow Budget for Composite Zone ',A,
     1  ' at Time Step',I4,' of Stress Period',I4/5X,79('-'))
  602 FORMAT(23X,'Budget Term',5X,'Flow (L**3/T)'/
     1     23X,29('-')//13X,'IN:'/13X,'---')
  603 FORMAT(18X,A,' =',G14.5)
  604 FORMAT(26X,'Total IN =',G14.5)
  605 FORMAT(13X,'OUT:'/13X,4('-'))
  606 FORMAT(25X,'Total OUT =',G14.5)
  607 FORMAT(26X,'IN - OUT =',G14.5)
  608 FORMAT(15X,'Percent Discrepancy =',F20.2)
  609 FORMAT(12X,'Zone',I4,' to ',A,' =',G14.5)
  610 FORMAT(12X,A,' to Zone',I4,' =',G14.5)
  611 FORMAT(5X,'Composite Zone ',A,
     1      ' consists of the following numeric zones:'/(5X,15I4))
      END
      SUBROUTINE CSVSUBPR(KSTP,KPER,VBNM,VBVL,VBZNFL,MSUM,IUCSV,
     1               NTRDIM,NZDIM,TITLE,TOTIMD,LSTZON)
C     ******************************************************************
C     COMPUTE TOTALS AND DIFFERENCES FOR ZONES AND WRITE CSV FILE
C     ******************************************************************
C       SPECIFICATIONS:
      DIMENSION VBVL(2,NTRDIM,NZDIM),VBZNFL(2,0:NZDIM,0:NZDIM),
     1          LSTZON(0:NZDIM)
      DOUBLE PRECISION VBVL,VBZNFL,DHUN,DTWO,ZERO
      DOUBLE PRECISION TOTOUT(NZDIM),TOTIN(NZDIM)
      CHARACTER*16 VBNM(NTRDIM)
      CHARACTER*80 TITLE
      DOUBLE PRECISION TOTIMD
      CHARACTER*16 FIELD(0:NZDIM)
C     ------------------------------------------------------------------
      ZERO=0.0
C
      DO 5 K=1,NZDIM
      TOTIN(K)=ZERO
      TOTOUT(K)=ZERO
5     CONTINUE
C
C-----THE NUMBER OF FLOW TERMS OTHER THAN FLOW BETWEEN ZONES IS MSUM-1
      MTOT=MSUM-1
      DHUN=100.
      DTWO=2.
C
C-----PRINT THE TITLE
      IF(TOTIMD.LT.0.) THEN
        WRITE(IUCSV,601) KSTP,KPER,TRIM(TITLE)
  601   FORMAT('Time Step,',I3,',Stress Period,',I3,',',A,',')
      ELSE
        WRITE(IUCSV,602) KSTP,KPER,TOTIMD,TRIM(TITLE)
  602   FORMAT('Time Step,',I4,',Stress Period,',I4,
     1              ',Sim. Time,',1PE13.6,',',A,',')
      END IF
C
C-----GENERATE and PRINT each Row for all zones
C-----Zone Numbers
      FIELD(0)=' '
      DO 10 K=1,NZDIM
      WRITE(FIELD(K),7) LSTZON(K)
    7 FORMAT('  ZONE',I4,'      ')
10    CONTINUE
      WRITE(IUCSV,11) (FIELD(J),J=0,NZDIM)
   11 FORMAT(2000(A,','))
C
C-----IN Labels
      FIELD(0)=' '
      DO 20 K=1,NZDIM
      FIELD(K)= '    IN'
20    CONTINUE
      WRITE(IUCSV,11) (FIELD(J),J=0,NZDIM)
C
C-----ALL BUDGET TERM INFLOWS
      DO 40 M=1,MTOT
      FIELD(0)=VBNM(M)
      DO 30 K=1,NZDIM
      TOTIN(K)=TOTIN(K)+VBVL(1,M,K)
      WRITE(FIELD(K),81) VBVL(1,M,K)
   81 FORMAT(1P,E16.6)
30    CONTINUE
      WRITE(IUCSV,11) (FIELD(J),J=0,NZDIM)
40    CONTINUE
C
C-----Inflows from other zones
      DO 60 N=0,NZDIM
      IF(N.EQ.0 .AND. LSTZON(N).LT.0) GO TO 60
      WRITE(FIELD(0),41) LSTZON(N)
   41 FORMAT('   FROM ZONE',I4)
      DO 50 K=1,NZDIM
      TOTIN(K)=TOTIN(K)+VBZNFL(1,K,N)
      WRITE(FIELD(K),81) VBZNFL(1,K,N)
50    CONTINUE
      WRITE(IUCSV,11) (FIELD(J),J=0,NZDIM)
60    CONTINUE
C
C-----Total inflow
      FIELD(0)='Total IN        '
      DO 70 K=1,NZDIM
      WRITE(FIELD(K),81) TOTIN(K)
70    CONTINUE
      WRITE(IUCSV,11) (FIELD(J),J=0,NZDIM)
C
C-----OUT Labels
      FIELD(0)=' '
      DO 200 K=1,NZDIM
      FIELD(K)= '   OUT'
200   CONTINUE
      WRITE(IUCSV,11) (FIELD(J),J=0,NZDIM)
C
C-----ALL BUDGET TERM OUTFLOWS
      DO 240 M=1,MTOT
      FIELD(0)=VBNM(M)
      DO 230 K=1,NZDIM
      TOTOUT(K)=TOTOUT(K)+VBVL(2,M,K)
      WRITE(FIELD(K),81) VBVL(2,M,K)
230   CONTINUE
      WRITE(IUCSV,11) (FIELD(J),J=0,NZDIM)
240   CONTINUE
C
C-----Outflows to other zones
      DO 260 N=0,NZDIM
      IF(N.EQ.0 .AND. LSTZON(N).LT.0) GO TO 260
      WRITE(FIELD(0),242) LSTZON(N)
  242 FORMAT('     TO ZONE',I4)
      DO 250 K=1,NZDIM
      TOTOUT(K)=TOTOUT(K)+VBZNFL(2,K,N)
      WRITE(FIELD(K),81) VBZNFL(2,K,N)
250   CONTINUE
      WRITE(IUCSV,11) (FIELD(J),J=0,NZDIM)
260   CONTINUE
C
C-----Total outflow
      FIELD(0)='Total OUT       '
      DO 270 K=1,NZDIM
      WRITE(FIELD(K),81) TOTOUT(K)
270   CONTINUE
      WRITE(IUCSV,11) (FIELD(J),J=0,NZDIM)
C
C-----IN-OUT
      FIELD(0)=' IN-OUT          '
      DO 280 K=1,NZDIM
      WRITE(FIELD(K),81) TOTIN(K)-TOTOUT(K)
280   CONTINUE
      WRITE(IUCSV,11) (FIELD(J),J=0,NZDIM)
C
C-----Percent error
      FIELD(0)='Percent Error   '
      DO 290 K=1,NZDIM
      WRITE(FIELD(K),81) DHUN*(TOTIN(K)-TOTOUT(K))/
     1                       ((TOTIN(K)+TOTOUT(K))/DTWO)
290   CONTINUE
      WRITE(IUCSV,11) (FIELD(J),J=0,NZDIM)
      WRITE(IUCSV,'(A)') ','
C
      RETURN
      END
      SUBROUTINE CSVSUBPR2(KSTP,KPER,VBNM,VBVL,VBZNFL,MSUM,IUCSV,
     1               NTRDIM,NZDIM,TITLE,TOTIMD,LSTZON,IOUT)
C     ******************************************************************
C     COMPUTE TOTALS AND DIFFERENCES FOR ZONES AND WRITE CSV FILE
C     ******************************************************************
C       SPECIFICATIONS:
      DIMENSION VBVL(2,NTRDIM,NZDIM),VBZNFL(2,0:NZDIM,0:NZDIM),
     1         LSTZON(0:NZDIM)
      DOUBLE PRECISION VBVL,VBZNFL,TOTOUT,TOTIN,TOTBD,DHUN,DTWO,ZERO
      DOUBLE PRECISION TOTZONIN,TOTZONOUT
      CHARACTER*16 VBNM(NTRDIM)
      CHARACTER*80 TITLE
      DOUBLE PRECISION TOTIMD
      CHARACTER*16 ZONINNAM(0:NZDIM)
      CHARACTER*16 ZONOUTNAM(0:NZDIM)
      CHARACTER*16 FIELD(NZDIM*2+NTRDIM+10)
      INTEGER,SAVE    ::IFIRST=1
C     ------------------------------------------------------------------
      ZERO=0.0
C
C-----THE NUMBER OF FLOW TERMS OTHER THAN FLOW BETWEEN ZONES IS MSUM-1
      MTOT=MSUM-1
      DHUN=100.
      DTWO=2.
C
C ----VERIFY THAT THE BUDGET ENTRIES HAVE NOT CHANGED
      CALL VBNMCHK(IFIRST,IOUT,IUCSV,MTOT,NTRDIM,VBNM)
C
C-----Create Zone labels
      DO 2 I=0,NZDIM
      WRITE(ZONINNAM(I),11) LSTZON(I)
   11 FORMAT('   FROM ZONE',I4)
      WRITE(ZONOUTNAM(I),12) LSTZON(I)
   12 FORMAT('     TO ZONE',I4)
    2 CONTINUE
C
C-----PRINT THE HEADINGS ONLY FOR THE FIRST TIME STEP
      IF(IFIRST.EQ.1) THEN
        IFIRST=0
C
C-----GENERATE ZONE FLOW VALUES GOING FROM ONE ZONE INTO A LOWER ZONE --
C-----EG GOING FROM B TO A WHERE A IS A LOWER ZONE THAN B
C-----THESE ARE JUST THE SAME AS THE CORRESPONDING VALUES GOING
C-----FROM A TO B EXCEPT THAT INS AND OUTS ARE REVERSED
        DO 50 K=0,NZDIM-1
        DO 50 J=K+1,NZDIM
        DO 50 I=1,2
        VBZNFL(I,J,K)=VBZNFL(3-I,K,J)
50      CONTINUE
C
C-----PRINT COLUMN HEADERS
        NFIELD=1
        FIELD(NFIELD)='TOTIM'
        NFIELD=NFIELD+1
        FIELD(NFIELD)='  PERIOD'
        NFIELD=NFIELD+1
        FIELD(NFIELD)='   STEP'
        NFIELD=NFIELD+1
        FIELD(NFIELD)='   ZONE         '
C  Add storage term if none exists -- necessary in case 1st stress period
C  is steady state and others are transient
        IF(VBNM(1).EQ.'   CONSTANT HEAD') THEN
          NFIELD=NFIELD+1
          FIELD(NFIELD)='         STORAGE'
        END IF
        DO 72 M=1,MTOT
        NFIELD=NFIELD+1
        FIELD(NFIELD)=VBNM(M)
   72   CONTINUE
        NFIELD=NFIELD+1
        FIELD(NFIELD)='From Other Zones'
        NFIELD=NFIELD+1
        FIELD(NFIELD)='Total IN        '
C  Add storage term if none exists -- necessary in case 1st stress period
C  is steady state and others are transient
        IF(VBNM(1).EQ.'   CONSTANT HEAD') THEN
          NFIELD=NFIELD+1
          FIELD(NFIELD)='         STORAGE'
        END IF
        DO 73 M=1,MTOT
        NFIELD=NFIELD+1
        FIELD(NFIELD)=VBNM(M)
   73   CONTINUE
        NFIELD=NFIELD+1
        FIELD(NFIELD)='To Other Zones'
        NFIELD=NFIELD+1
        FIELD(NFIELD)='Total Out       '
        NFIELD=NFIELD+1
        FIELD(NFIELD)=' IN-OUT          '
        NFIELD=NFIELD+1
        FIELD(NFIELD)='Percent Error   '
C  Put zone fields twice -- once for IN and once for OUT
        IF(LSTZON(0).EQ.0) THEN
          NFIELD=NFIELD+1
          FIELD(NFIELD)=ZONINNAM(0)
        END IF
        DO 74 K=1,NZDIM
        NFIELD=NFIELD+1
        FIELD(NFIELD)=ZONINNAM(K)
   74   CONTINUE
        IF(LSTZON(0).EQ.0) THEN
          NFIELD=NFIELD+1
          FIELD(NFIELD)=ZONOUTNAM(0)
        END IF
        DO 75 K=1,NZDIM
        NFIELD=NFIELD+1
        FIELD(NFIELD)=ZONOUTNAM(K)
   75   CONTINUE
        WRITE(IUCSV,7) (FIELD(I),I=1,NFIELD)
    7   FORMAT(1000(A,','))
      END IF
C
C-----FOR EACH ZONE, CALCULATE BUDGET TOTALS AND PRINT
      DO 500 N=1,NZDIM
C
C
C-----COMPUTE TOTAL INS AND OUTS
      TOTOUT=ZERO
      TOTIN=ZERO
      TOTZONIN=ZERO
      TOTZONOUT=ZERO
      DO 100 I=1,MTOT
      TOTIN=TOTIN+VBVL(1,I,N)
      TOTOUT=TOTOUT+VBVL(2,I,N)
100   CONTINUE
      DO 150 I=0,NZDIM
      IF(LSTZON(N).LT.0) GO TO 150
      TOTZONIN=TOTZONIN+VBZNFL(1,N,I)
      TOTZONOUT=TOTZONOUT+VBZNFL(2,N,I)
150   CONTINUE
      TOTIN=TOTIN+TOTZONIN
      TOTOUT=TOTOUT+TOTZONOUT
C
C-----CALCULATE THE DIFFERENCE BETWEEN IN AND OUT AND THE PERCENT ERROR
      IF(TOTIN.EQ.ZERO .AND. TOTOUT.EQ.ZERO) GO TO 500
      TOTBD=TOTIN-TOTOUT
      PERCNT=DHUN*TOTBD/((TOTIN+TOTOUT)/DTWO)
C
C
C     ---PRINT BUDGET---
C
      NFIELD=1
      IF(TOTIMD.GE.0.) THEN
         WRITE(FIELD(NFIELD),81) TOTIMD
      ELSE
         WRITE(FIELD(NFIELD),'(A)') 'UNDEFINED'
      END IF
      NFIELD=NFIELD+1
      WRITE(FIELD(NFIELD),'(I16)') KPER
      NFIELD=NFIELD+1
      WRITE(FIELD(NFIELD),'(I16)') KSTP
      NFIELD=NFIELD+1
      WRITE(FIELD(NFIELD),'(I16)') LSTZON(N)
C  Print storage term if none exists -- necessary in case 1st stress period
C  is steady state and others are transient
      IF(VBNM(1).EQ.'   CONSTANT HEAD') THEN
        NFIELD=NFIELD+1
        WRITE(FIELD(NFIELD),81) 0.0
      END IF
      DO 82 I=1,MTOT
      NFIELD=NFIELD+1
      WRITE(FIELD(NFIELD),81) VBVL(1,I,N)
   81 FORMAT(1P,E16.6)
   82 CONTINUE
      NFIELD=NFIELD+1
      WRITE(FIELD(NFIELD),81) TOTZONIN
      NFIELD=NFIELD+1
      WRITE(FIELD(NFIELD),81) TOTIN
C  Print storage term if none exists -- necessary in case 1st stress period
C  is steady state and others are transient
      IF(VBNM(1).EQ.'   CONSTANT HEAD') THEN
        NFIELD=NFIELD+1
        WRITE(FIELD(NFIELD),81) 0.0
      END IF
      DO 84 I=1,MTOT
      NFIELD=NFIELD+1
      WRITE(FIELD(NFIELD),81) VBVL(2,I,N)
   84 CONTINUE
      NFIELD=NFIELD+1
      WRITE(FIELD(NFIELD),81) TOTZONOUT
      NFIELD=NFIELD+1
      WRITE(FIELD(NFIELD),81) TOTOUT
      NFIELD=NFIELD+1
      WRITE(FIELD(NFIELD),81) TOTBD
      NFIELD=NFIELD+1
      WRITE(FIELD(NFIELD),81) PERCNT
      DO 83 I=0,NZDIM
      IF(LSTZON(I).GE.0) THEN
        NFIELD=NFIELD+1
        WRITE(FIELD(NFIELD),81) VBZNFL(1,N,I)
      END IF
   83 CONTINUE
      DO 85 I=0,NZDIM
      IF(LSTZON(I).GE.0) THEN
        NFIELD=NFIELD+1
        WRITE(FIELD(NFIELD),81) VBZNFL(2,N,I)
      END IF
   85 CONTINUE
      WRITE(IUCSV,7) (FIELD(I),I=1,NFIELD)
C
  500 CONTINUE
C
      RETURN
      END
      SUBROUTINE BUDGETPRECISION(IU,NCOL,NROW,NLAY)
C     ******************************************************************
C     Determine single or double precision file type for a MODFLOW
C     budget file:  0=unrecognized, 1=single, 2=double.
C     ******************************************************************
      USE ZONBUDMODULE
      DOUBLE PRECISION DELTD,PERTIMD,TOTIMD,VALD
      CHARACTER*16 TEXT1,TEXT2
C
C  Default is unrecognized file
      IPREC=0
C
C  SINGLE check
      READ(IU,ERR=100,END=100) KSTP,KPER,TEXT1,NCOL,NROW,NLAY
      ICODE=0
      IF(NLAY.LT.0) THEN
        NLAY=-NLAY
        READ(IU,ERR=50,END=50) ICODE,DELT,PERTIM,TOTIM
      END IF
      IF(NCOL.LT.1 .OR. NROW.NE.1 .OR. NLAY.NE.1) GO TO 100
C
C  Read data depending on ICODE.  ICODE 0,1, or 2 are the only allowed
C  values because the first budget terms must be from the internal
C  flow package (BCF,LPF, or HUF).
      IF(ICODE.EQ.0 .OR. ICODE.EQ.1) THEN
         READ(IU,ERR=50,END=50) (BUFF(I),I=1,NEQ)
      ELSE IF(ICODE.EQ.2) THEN
         READ(IU,ERR=50,END=50) NLST
         IF(NLST.LT.0) GO TO 50
         IF(NLST.GT.0) THEN
            DO 22 N=1,NLST
            READ(IU,END=50,ERR=50) ICELL,VAL
            IF(ICELL.LE.0 .OR. ICELL.GT.NCOL) GO TO 50
22          CONTINUE
         END IF
      ELSE
         GO TO 100
      END IF
C
C  Read 2nd header and check for valid type.
      READ(IU,ERR=50,END=50) KSTP,KPER,TEXT2
      IF(TEXT1.EQ.'         STORAGE' .AND.
     1   TEXT2.EQ.'   CONSTANT HEAD') THEN
           IPREC=1
           GO TO 100
      ELSE IF(TEXT1.EQ.'   CONSTANT HEAD' .AND.
     1        TEXT2.EQ.'   FLOW JA FACE ') THEN
           IPREC=1
           GO TO 100
      END IF
C
C  DOUBLE check
50    REWIND(IU)
      READ(IU,ERR=100,END=100) KSTP,KPER,TEXT1,NC,NR,NL
      ICODE=0
      IF(NL.LT.0) THEN
        NL=-NL
        READ(IU,ERR=100,END=100) ICODE,DELTD,PERTIMD,TOTIMD
      END IF
C
C  Read data depending on ICODE.  ICODE 0,1, or 2 are the only allowed
C  values because the first budget terms must be from the internal
C  flow package (BCF,LPF, or HUF).
      IF(ICODE.EQ.0 .OR. ICODE.EQ.1) THEN
         READ(IU,ERR=100,END=100) (BUFFD(I),I=1,NEQ)
      ELSE IF(ICODE.EQ.2) THEN
         READ(IU,ERR=100,END=100) NLST
         IF(NLST.LT.0) GO TO 100
         IF(NLST.GT.0) THEN
            DO 72 N=1,NLST
            READ(IU,END=100,ERR=100) ICELL,VALD
            IF(ICELL.LE.0 .OR. ICELL.GT.NCOL) GO TO 100
72          CONTINUE
         END IF
      ELSE
         GO TO 100
      END IF
C
C  Read 2nd header and check for valid type.
      READ(IU,ERR=100,END=100) KSTP,KPER,TEXT2
      IF(TEXT1.EQ.'         STORAGE' .AND.
     1   TEXT2.EQ.'   CONSTANT HEAD') THEN
           IPREC=2
      ELSE IF(TEXT1.EQ.'   CONSTANT HEAD' .AND.
     1        TEXT2.EQ.'   FLOW JA FACE ') THEN
           IPREC=2
      END IF
C
100   REWIND(IU)
      RETURN
      END
      SUBROUTINE VBNMCHK(IFIRST,IOUT,IUCSV,MTOT,NTRDIM,VBNM)
C     ******************************************************************
C     CHECK TO SEE IF THE NUMBER OR TYPE OF ENTRIES IN THE BUDGET FILE 
C     HAS CHANGED.  FOR THE FIRST CALL, STORE NON-STORAGE ENTRIES IN
C     VBNM0.  TERMINATE WITH AN ERROR IF VBNM DIFFERS FROM VBNM0 IN 
C     SUBSEQUENT CALLS.
C     ******************************************************************
C       SPECIFICATIONS:
      CHARACTER*16 VBNM(NTRDIM)
      CHARACTER*16,ALLOCATABLE,SAVE :: VBNM0(:)
      INTEGER,SAVE    ::MTOT0       
C     ------------------------------------------------------------------
C
C-----SET MSTART VALUE FOR WHERE TO START COMPARING IN VBNM.  SET MLEN
C-----EQUAL TO NUMBER OF ENTRIES NOT EQUAL TO 'STORAGE'.
      MSTART=1
      IF(VBNM(1).EQ.'         STORAGE') MSTART=2
      MLEN=MTOT-MSTART+1
C
C-----IF THIS IS FIRST TIME, THEN SAVE VBNM ENTRIES INTO VBNM0.
C-----EXCLUDE 'STORAGE' AS IT IS ALWAYS ACCOUNTED FOR IN ZONEBUDGET.
      IF(IFIRST.EQ.1) THEN
        MTOT0=MTOT-MSTART+1
        ALLOCATE(VBNM0(MTOT0))
        VBNM0(:)=VBNM(MSTART:MTOT)
      ENDIF
C
C-----IF IT IS NOT THE FIRST TIME, THEN COMPARE VBNM0 WITH VBNM. SET
C-----IERR=1 IF ENTRIES HAVE CHANGED.
      IERR=0
      IF(IFIRST.EQ.0) THEN
C
C-------VERIFY THAT NUMBER OF ENTRIES ARE THE SAME
        IF(MLEN.NE.MTOT0) THEN
          IERR=1
        ELSE
C
C---------VERIFY THAT EACH ENTRY IS THE SAME
          M=MSTART
          DO M0=1,MTOT0
            IF(VBNM(M).NE.VBNM0(M0)) THEN
              IERR=1
              EXIT
            ENDIF
            M=M+1
          ENDDO
        ENDIF
      ENDIF
C
C-----IF ENTRIES DO NOT COMPARE, THEN TERMINATE WITH AN ERROR
      IF(IERR.GT.0) THEN
         WRITE(*,5)
         WRITE(*,*) 'Entries first detected in budget file: ', VBNM0(:)
         WRITE(*,*) 'Entries currently detected in budget file: ', 
     1               VBNM(MSTART:MTOT)
C
         WRITE(IUCSV,5)
         WRITE(IUCSV,*) 'Entries first detected in budget file: ', 
     1                   VBNM0(:)
         WRITE(IUCSV,*) 'Entries currently detected in budget file: ', 
     1                   VBNM(MSTART:MTOT)
C
         WRITE(IOUT,5)
         WRITE(IOUT,*) 'Entries first detected in budget file: ', 
     1                  VBNM0(:)
         WRITE(IOUT,*) 'Entries currently detected in budget file: ', 
     1                  VBNM(MSTART:MTOT)
C
    5    FORMAT(' Stopping -- the entries in the budget file have ',
     1          'changed.  Use of the COMPACT BUDGET option in ',
     2          'the Output Control File of MODFLOW will often fix ',
     3          'this problem.')
         STOP
      ENDIF
C
      RETURN
      END
