      SUBROUTINE SDIS2CLN1AR(IUCLN)
C     ******************************************************************
C     ALLOCATE SPACE AND READ NODE AND CONNECTIVITY INFORMATION FOR CLN DOMAIN
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CLN1MODULE
      USE GLOBAL, ONLY: IUNIT,IOUT,NEQS,NODES,NROW,NCOL,IFREFM,IUNSTR,
     *                  INCLN
      USE GWFBASMODULE, ONLY: IHEDUN,IDDNUN,IBOUUN
      CHARACTER*24 ANAME
      CHARACTER*200 LINE
      DATA ANAME /'   NODES PER CLN SEGMENT'/
      DOUBLE PRECISION FRAD
C     ------------------------------------------------------------------
C
C1------IDENTIFY PACKAGE.
        INCLN = IUNIT(IUCLN)
        WRITE(IOUT,1)INCLN
    1   FORMAT(1X,/1X,'CLN -- CONNECTED LINE NETWORK DISCRETIZATION ',
     1    'PROCESS, VERSION 1, 3/3/2012 INPUT READ FROM UNIT ',I4)
C
C2------ALLOCATE SCALAR VARIABLES AND INITIALIZE.
      ALLOCATE(NCLN,ICLNCB,ICLNHD,ICLNDD,ICLNIB,NCLNNDS,NCLNGWC)
      ALLOCATE(NCONDUITYP) !OTHER CLN TYPES CAN BE DIMENSIONED HERE
      NCLN = 0
C
C3------READ MAXIMUM NUMBER OF CLN NODES AND UNIT OR FLAGS FOR CLN
C3------DOMAIN OUTPUT OF HEAD, DRAWDOWN AND CELL-BY-CELL FLOW TERMS.
      CALL URDCOM(INCLN,IOUT,LINE)
      IF(IFREFM.EQ.0) THEN
        READ(LINE,'(8I10)') NCLN,ICLNNDS,ICLNCB,ICLNHD,ICLNDD,
     1    ICLNIB,NCLNGWC,NCONDUITYP
        LLOC=31
      ELSE
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NCLN,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICLNNDS,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICLNCB,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICLNHD,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICLNDD,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICLNIB,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NCLNGWC,R,IOUT,INCLN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NCONDUITYP,R,IOUT,INCLN)
CADD----ADD NUMBER OF OTHER CLN NODE TYPES HERE TO CATALOGUE THEM        
      END IF
C---------------------------------------------------------------------------
C3A-----REFLECT FLAGS IN OUTPUT LISTING FILE
      WRITE(IOUT,3) NCLN,ICLNNDS,NCLNGWC
    3 FORMAT(1X,'MAXIMUM NUMBER OF CONDUITS (NCLN) =',I7
     1  /1X,'FLAG (-VE) OR NUMBER OF CONDUIT DOMAIN NODES (+VE)',
     1  1X,'(ICLNNDS) =',I7
     1  /1X,'NUMBER OF LINEAR NODE TO MATRIX GRID CONNECTIONS',
     1  ' (NCLNGWC) =',I7/)
C
      IF(ICLNCB.LT.0) WRITE(IOUT,7)
    7 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL',
     1   ' IS NOT 0 (FLAG ICLNCB IS LESS THAN ZERO)')
      IF(ICLNCB.GT.0) WRITE(IOUT,8) ICLNCB
    8 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I5,
     1  '(FLAG ICLNCB IS GREATER THAN ZERO)')
      IF(ICLNCB.EQ.0) WRITE(IOUT,6)
    6 FORMAT(1X,'CELL-BY-CELL FLOWS WILL NOT BE SAVED OR PRINTED',
     1  1X,'(FLAG ICLNCB IS EQUAL TO ZERO)')
C
      IF(ICLNHD.LT.0) WRITE(IOUT,9)
    9 FORMAT(1X,'CLN HEAD OUTPUT WILL BE SAVED TO THE SAME UNIT',1X,
     1   'NUMBER (IHEDUN) AS USED FOR HEAD OUTPUT FOR POROUS MATRIX'
     2   1X,'(FLAG ICLNHD IS LESS THAN ZERO)')
      IF(ICLNHD.GT.0) WRITE(IOUT,10) ICLNHD
   10 FORMAT(1X,'CLN HEAD OUTPUT WILL BE SAVED ON UNIT ',I4,
     1  '(FLAG ICLNHD IS GREATER THAN ZERO)')
      IF(ICLNHD.EQ.0) WRITE(IOUT,31)
   31 FORMAT(1X,'CLN HEAD OUTPUT WILL NOT BE SAVED OR PRINTED',
     1  1X,'(FLAG ICLNHD IS EQUAL TO ZERO)')
        IF(ICLNHD.LT.0) ICLNHD = IHEDUN
C
      IF(ICLNDD.LT.0) WRITE(IOUT,12)
   12 FORMAT(1X,'CLN DDN OUTPUT WILL BE SAVED TO THE SAME UNIT',1X,
     1   'NUMBER (IDDNUN) AS USED FOR DDN OUTPUT FOR POROUS MATRIX'
     2   1X,'(FLAG ICLNDD IS LESS THAN ZERO)')
      IF(ICLNDD.GT.0) WRITE(IOUT,13) ICLNDD
   13 FORMAT(1X,'CLN DDN OUTPUT WILL BE SAVED ON UNIT ',I4,
     1  '(FLAG ICLNDD IS GREATER THAN ZERO)')
      IF(ICLNDD.EQ.0) WRITE(IOUT,14)
   14 FORMAT(1X,'CLN DDN OUTPUT WILL NOT BE SAVED OR PRINTED',
     1  1X,'(FLAG ICLNDD IS EQUAL TO ZERO)')
      IF(ICLNDD.LT.0) ICLNDD = IDDNUN
C
      IF(ICLNIB.LT.0) WRITE(IOUT,32)
   32 FORMAT(1X,'CLN IBOUND OUTPUT WILL BE SAVED TO THE SAME UNIT',1X,
     1   'NUMBER (IBOUUN) AS USED FOR DDN OUTPUT FOR POROUS MATRIX'
     2   1X,'(FLAG ICLNIB IS LESS THAN ZERO)')
      IF(ICLNIB.GT.0) WRITE(IOUT,33) ICLNIB
   33 FORMAT(1X,'CLN IBOUND OUTPUT WILL BE SAVED ON UNIT ',I4,
     1  '(FLAG ICLNIB IS GREATER THAN ZERO)')
      IF(ICLNIB.EQ.0) WRITE(IOUT,17)
   17 FORMAT(1X,'CLN IBOUND OUTPUT WILL NOT BE SAVED OR PRINTED',
     1  1X,'(FLAG ICLNIB IS EQUAL TO ZERO)')
      IF(ICLNDD.LT.0) ICLNIB = IBOUUN
C--------------------------------------------------------------------------------
C
C4------DIMENSION AND READ ARRAY THAT CONTAINS NUMBER OF NODES PER CLN SEGMENT
      ALLOCATE(NNDCLN(0:NCLN))
      K = 0
      CALL U1DINT(NNDCLN(1),ANAME,NCLN,K,INCLN,IOUT)
      NNDCLN(0) = 0
C
C5------MAKE NNDCLN ARRAY CUMULATIVE
      DO I = 1,NCLN
        NNDCLN(I) = NNDCLN(I) + NNDCLN(I-1)
      ENDDO
      NCLNCONS = NNDCLN(NCLN)
C------------------------------------------------------------------------------
C6------FILL CLNCON WITH CONNECTIVITY OF ADJACENT CLN NODES
      IF(ICLNNDS.LT.0)THEN
C6A-------FILL CLN CONNECTIONS SEQUENTIALLY WITH GLOBAL NODE NUMBERS
        NCLNNDS = NNDCLN(NCLN)
        ALLOCATE(CLNCON(NCLNNDS))
        DO I=1,NCLNNDS
          CLNCON(I) = NODES + I
        ENDDO
      ELSE
C6B-----SET NUMBER OF CLN NODES AND READ CONNECTION ARRAY FOR EACH CLN SEGMENT
        NCLNNDS = ICLNNDS
        ALLOCATE(CLNCON(NCLNCONS))
        DO I=1,NCLN
          IF(IFREFM.EQ.0) THEN
            READ(INCLN,'(200I10)') (CLNCON(J),J=NNDCLN(I-1)+1,NNDCLN(I))
          ELSE
            READ(INCLN,*) (CLNCON(J),J=NNDCLN(I-1)+1,NNDCLN(I))
          ENDIF
        ENDDO
C6C-------CONVERT CLN-NODE NUMBER TO GLOBAL NODE NUMBER
        DO I=1,NCLNCONS
          CLNCON(I) = NODES + CLNCON(I)
        ENDDO
      ENDIF
C----------------------------------------------------------------------
C7------ALLOCATE SPACE FOR CLN PROPERTY ARRAYS
      ALLOCATE(ACLNNDS(NCLNNDS,6))
      ALLOCATE(IFLINCLN(NCLNNDS))      
      ALLOCATE(ICCWADICLN(NCLNNDS))
      ALLOCATE(ICGWADICLN(NCLNGWC))
C
C8------PREPARE TO REFLECT INPUT PROPERTIES INTO LISTING FILE
      WRITE(IOUT,21)
21    FORMAT(/20X,' CONNECTED LINE NETWORK INFORMATION'/
     1  20X,40('-')/5X,'CLN-NODE NO.',1X,'CLNTYP',1X,'ORIENTATION',2X,
     1  'CLN LENGTH',4X,'BOT ELEVATION',9X,'FANGLE',9X,'IFLIN',11X,
     1  'ICCWADI'/5X,11('-'),2X,6('-'),1X,11('-'),1X,11('-'),4X,13('-'),
     1   4X,11('-'),8X,6('-'),4X,7('-'))
C
C9-------READ BASIC PROPERTIES FOR ALL CLN NODES AND FILL ARRAYS
      DO I = 1,NCLNNDS
        CALL URDCOM(INCLN,IOUT,LINE)
        IF(IFREFM.EQ.0) THEN
          READ(LINE,'(3I10,3F10.3,2I10)') IFNO,IFTYP,IFDIR,FLENG,FELEV,
     1        FANGLE,IFLIN,ICCWADI
          LLOC=71
        ELSE
          LLOC=1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFTYP,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFDIR,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FLENG,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FELEV,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FANGLE,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFLIN,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICCWADI,R,IOUT,INCLN)
        END IF
C9A--------FOR ANGLED PIPE, IF DEPTH OF FLOW IS LESS THAN DIAMETER MAKE HORIZONTAL
        IF(IFDIR.EQ.2)THEN
          FDPTH = FLENG * SIN(FANGLE)
          IC=IFTYP
          CALL CLNR(IC,FRAD)
          IF(FDPTH.LT.2.0*FRAD) IFDIR = 1
        ENDIF
        WRITE(IOUT,22)IFNO,IFTYP,IFDIR,FLENG,FELEV,FANGLE,IFLIN,ICCWADI
22      FORMAT(5X,I10,1X,I6,1X,I10,3(1X,E15.6),1X,I10,1X,I10)
C9B--------FILL PROPERTY ARRAYS WITH READ AND PREPARE INFORMATION
        ACLNNDS(I,1) = IFNO + NODES ! GLOBAL NODE NUMBER FOR CLN-CELL 
        ACLNNDS(I,2) = IFTYP
        ACLNNDS(I,3) = IFDIR
        ACLNNDS(I,4) = FLENG
        ACLNNDS(I,5) = FELEV
        ACLNNDS(I,6) = FANGLE
        IFLINCLN(I) = IFLIN
        ICCWADICLN(I) = ICCWADI
      ENDDO
C----------------------------------------------------------------------------------------
C10------ALLOCATE SPACE FOR CLN TO GW PROPERTY ARRAYS
      ALLOCATE(ACLNGWC(NCLNGWC,6))
C----------------------------------------------------------------------------------------
C11------READ CONNECTING SUBSURFACE NODE AND ASSOCIATED PARAMETERS
      IF(IUNSTR.EQ.0)THEN
C
C11A-----FOR STRUCTURED GRID READ SUBSURFACE NODE IN IJK FORMATS
C11A-----AND OTHER CLN SEGMENT PROPERTY INFORMATION
        CALL SCLN2DIS1SR
      ELSE
C
C11B-----FOR UNSTRUCTURED GRID READ SUBSURFACE NODE NUMBER OF
C11B-----CONNECTION AND OTHER CLN SEGMENT PROPERTY INFORMATION
        CALL SCLN2DIS1UR
      ENDIF
C----------------------------------------------------------------------------------------
C12------ALLOCATE SPACE AND FILL PROPERTIES FOR ALL CONDUIT TYPE CLNs
      IF(NCONDUITYP.GT.0)THEN
        CALL SCLN2COND1RP
      ENDIF
C----------------------------------------------------------------------------------------
C13------ALLOCATE SPACE AND FILL PROPERTIES FOR OTHER CLN TYPES HERE
CADD------ADD OTHER CLN TYPE READ AND PREPARE INFORMATION HERE
C----------------------------------------------------------------------------------------
C14-----RETURN
      RETURN
      END
C ---------------------------------------------------------------------
      SUBROUTINE SCLN2DIS1SR
C     ******************************************************************
C      READ PROPERTIES FOR CLN DOMAIN FOR A STRUCTURED GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CLN1MODULE
      USE GLOBAL, ONLY: IUNIT,IOUT,NEQS,NODES,NROW,NCOL,IFREFM,IUNSTR,
     *                  INCLN
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
      PI = 3.1415926
C1------PREPARE TO REFLECT INPUT INTO LISTING FILE
      WRITE(IOUT,21)
21    FORMAT(/20X,' CLN TO 3-D GRID CONNECTION INFORMATION'/
     1  20X,40('-')/5X,'F-NODE NO.',6X,'LAYER',8X,'ROW',5X,'COLUMN',
     2  2X,'EQTN. TYPE',5X,'      FSKIN',11X,'FLENG',10X,
     4  'FANISO',3X,'ICGWADI'/5X,10('-'),6X,5('-'),8X,3('-'),5X,
     5  6('-'),2X,11('-'),3X,12('-'),2X,14('-'),4X,12('-'),3X,7('-'))
C2-------READ PROPERTIES AND SUBSURFACE CONNECTION INFORMATION FOR ALL CLN NODES
      DO I = 1,NCLNGWC
        CALL URDCOM(INCLN,IOUT,LINE)
        IF(IFREFM.EQ.0) THEN
          READ(LINE,'(5I10,3F10.3,I10)') IFNO,IFLAY,IFROW,IFCOL,IFCON,
     1      FSKIN,FLENG,FANISO,ICGWADI
          LLOC=71
        ELSE
          LLOC=1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFLAY,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFROW,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFCOL,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFCON,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FSKIN,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FLENG,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FANISO,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICGWADI,R,IOUT,INCLN)
        END IF
C3--------SET SKIN PARAMETER AND REFLECT INPUT IN LST FILE
        IF(IFCON.EQ.0)FSKIN = 0.0
        WRITE(IOUT,22)IFNO,IFLAY,IFROW,IFCOL,IFCON,FSKIN,FLENG,FANISO,
     1        ICGWADI
22      FORMAT(5X,I10,3(1X,I10),2X,I10,3(1X,E15.6),1X,I9)
C4--------FILL CLN AND GW NODE NUMBERS AND CONNECTION PROPERTY MATRIX
        ACLNGWC(I,1) = IFNO
        IFNOD = (IFLAY-1)*NROW*NCOL + (IFROW-1)*NCOL + IFCOL
        ACLNGWC(I,2) = IFNOD
        ACLNGWC(I,3) = IFCON
        ACLNGWC(I,4) = FSKIN
        ACLNGWC(I,5) = FANISO
        ACLNGWC(I,6) = FLENG
        ICGWADICLN(I) = ICGWADI
      ENDDO
C5-----RETURN
      RETURN
      END
C ---------------------------------------------------------------------
      SUBROUTINE SCLN2DIS1UR
C     ******************************************************************
C      READ PROPERTIES FOR CLN DOMAIN FOR A UNSTRUCTURED GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CLN1MODULE
      USE GLOBAL, ONLY: IUNIT,IOUT,NEQS,NODES,NROW,NCOL,IFREFM,IUNSTR,
     *                  INCLN
      CHARACTER*200 LINE
C     ------------------------------------------------------------------
      PI = 3.1415926
C1------PREPARE TO REFLECT INPUT INTO LISTING FILE
      WRITE(IOUT,23)
23    FORMAT(/20X,' CLN TO 3-D GRID CONNECTION INFORMATION'/
     1    20X,40('-')/5X,'F-NODE NO.',1X,'GW-NODE NO',2X,
     2    'EQTN. TYPE',2X,'      FSKIN',11X,
     4    'FLENG',9X,'FANISO'3X,'ICGWADI'/5X,10('-'),1X,10('-'),
     5    1X,11('-'),5X,11('-'),1X,17('-'),1X,15('-'),3X,10('-'))
C2-------READ PROPERTIES AND SUBSURFACE CONNECTION INFORMATION FOR ALL CLN NODES
      DO I = 1,NCLNGWC
        CALL URDCOM(INCLN,IOUT,LINE)
        IF(IFREFM.EQ.0) THEN
          READ(LINE,'(3I10,3F10.3,I10)') IFNO,IFNOD,IFCON,FSKIN,FLENG,
     1          FANISO,ICGWADI
          LLOC=51
        ELSE
          LLOC=1
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNO,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFNOD,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFCON,R,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FSKIN,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FLENG,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,FANISO,IOUT,INCLN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ICGWADI,R,IOUT,INCLN)
        END IF
C3--------SET SKIN PARAMETER AND REFLECT INPUT IN LST FILE
        IF(IFCON.EQ.0)FSKIN = 0.0
        WRITE(IOUT,24)IFNO,IFNOD,IFCON,FSKIN,FLENG,FANISO,ICGWADI
24      FORMAT(5X,I10,1X,I10,2X,I10,3(1X,E15.6),1X,I9)
C4--------FILL CLN AND GW NODE NUMBERS AND CONNECTION PROPERTY MATRIX
        ACLNGWC(I,1) = IFNO
        ACLNGWC(I,2) = IFNOD
        ACLNGWC(I,3) = IFCON
        ACLNGWC(I,4) = FSKIN
        ACLNGWC(I,5) = FANISO
        ACLNGWC(I,6) = FLENG
        ICGWADICLN(I) = ICGWADI
      ENDDO
C5-----RETURN
      RETURN
      END
C
C ---------------------------------------------------------------------
      SUBROUTINE CLN2BAS1AR
C     ******************************************************************
C     READ IBOUND AND STARTING HEADS AND PREPARE KADI, Sn AND PGF ARRAYS FOR CLN
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CLN1MODULE, ONLY: NCLNNDS,NCLNGWC,ACLNNDS,IFLINCLN,
     1    ICCWADICLN,ICGWADICLN
      USE GLOBAL, ONLY: IOUT,IBOUND,NODES,STRT,HNEW,Sn,So,INCLN,IWADI,
     * IWADICLN
      USE GWFBASMODULE, ONLY: HNOFLO
      REAL, DIMENSION(:),ALLOCATABLE  ::HTMP1
      CHARACTER*24 ANAME(2)
      DATA ANAME(1) /' CONDUIT BOUNDARY ARRAY'/
      DATA ANAME(2) /'   CONDUIT INITIAL HEAD'/
      DOUBLE PRECISION HD,THCK,BBOT
C     ------------------------------------------------------------------
C
C1------IDENTIFY PACKAGE.
      WRITE(IOUT,1)INCLN
    1 FORMAT(1X,/1X,'CLN -- CONDUIT DOMAIN FLOW PACKAGE, VERSION 1,',
     1  ' 5/17/2010 INPUT READ FROM UNIT ',I4)
C
C2-------READ IBOUND FOR CLN NODES
      CALL U1DINT(IBOUND(NODES+1),ANAME(1),NCLNNDS,0,INCLN,IOUT)
C3-------READ INITIAL HEADS FOR CLN NODES
      ALLOCATE(HTMP1(NCLNNDS))
      CALL U1DREL(HTMP1,ANAME(2),NCLNNDS,0,INCLN,IOUT)
      DO N=1,NCLNNDS
        HNEW(NODES+N) = HTMP1(N)
        STRT(NODES+N) = HTMP1(N)
        IF(IBOUND(NODES+N).EQ.0) HNEW(NODES+N)=HNOFLO
      ENDDO
      DEALLOCATE(HTMP1)
C
C4-----SET VOLUMETRIC FRACTIONS FOR CLN-NODES IN SATURATION ARRAY
      DO  IFN=1,NCLNNDS
        N = ACLNNDS(IFN,1)
        IFLIN = IFLINCLN(IFN)
        IF(IBOUND(N).NE.0.AND.IFLIN.EQ.0) THEN
C---------CALCULATE INITIAL SATURATED THICKNESS.
          HD=HNEW(N)
          BBOT = ACLNNDS(IFN,5)
          CALL CLN_THIK(IFN,HD,BBOT,THCK)
          Sn(N)=THCK
          So(N) = Sn(N)
        ENDIF
      ENDDO
C--------------------------------------------------------------------------------
C5-------FILL PGF ARRAY FOR CLN FLOW AND ITS CONNECTION WITH POROUS MATRIX
      CALL SFILLPGF_CLN
C----------------------------------------------------------------------------------------
C12A------ESTABLISH WADI CONDITION FOR CLN  
        IWADICLN = 0
        DO I = 1,NCLNNDS
           IF(ICCWADICLN(I).NE.0) IWADICLN = 1 
        ENDDO    
        DO I = 1,NCLNGWC
          IF(ICGWADICLN(I).NE.0) IWADICLN = 1 
        ENDDO
        IF(IWADICLN.EQ.1) IWADI = 1
C
C6------RETURN
      RETURN
      END
C
C ---------------------------------------------------------------------
      SUBROUTINE SFILLIA_CLN
C     ******************************************************************
C     INCLUDE CLN CONNECTIVITIES IN ROW INDICATOR MATRIX IA
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE CLN1MODULE, ONLY: NCLNGWC,ACLNGWC,NNDCLN,NCLN,CLNCON,ACLNNDS
      USE GLOBAL, ONLY: IA
C     ------------------------------------------------------------------
C
C1------ADD CONNECTION OF CLN NODE WITH MATRIX
      DO IFN = 1,NCLNGWC
        I1 = ACLNGWC(IFN,1)
        ND1 = ACLNNDS(I1,1)
        IA(ND1) = IA(ND1) + 1
        ND2 = ACLNGWC(IFN,2)
        IA(ND2) = IA(ND2) + 1
      ENDDO
C2------ADD CONNECTION WITH NEXT CONDUIT-NODE OF A CONDUIT SEGMENT
      DO IFR = 1,NCLN
        NN = NNDCLN(IFR)
        NNM1 = NNDCLN(IFR-1)
        DO INOD = NNM1+1,NN-1
          ND1 = CLNCON(INOD)
          ND2 = CLNCON(INOD+1)
          IA(ND1) = IA(ND1) + 1
          IA(ND2) = IA(ND2) + 1
        ENDDO
      ENDDO
C
C3------RETURN
      RETURN
      END
C
C -----------------------------------------------------------------------
      SUBROUTINE FILLJA_CLN
C     ******************************************************************
C     FILL JA ARRAY FOR CONDUIT DOMAIN FLOW NODES.
C     ******************************************************************
      USE GLOBAL, ONLY:JA,IA,NODES,NEQS
      USE CLN1MODULE, ONLY:NCLN,NCLNGWC,ACLNGWC,NNDCLN,CLNCON,NCLNNDS,
     1    ACLNNDS
C-----------------------------------------------------------------------
C
C1------DIAGONAL IN FIRST LOCATION
      DO I=NODES+1,NODES+NCLNNDS
        JA(IA(I)) = I
      ENDDO
C2-------CONNECTION WITH MATRIX
      DO IFN = 1,NCLNGWC
        I1 = ACLNGWC(IFN,1)
        ND1 = ACLNNDS(I1,1)
        ND2 = ACLNGWC(IFN,2)
        CALL FINDJA(ND1,ND2)
        CALL FINDJA(ND2,ND1)
      ENDDO
C3-------CONNECTION WITH NEXT CONDUIT-NODE OF A CONDUIT SETMENT
      DO IFR = 1,NCLN
        NN = NNDCLN(IFR)
        NB = NNDCLN(IFR-1)+1
        DO N = NB,NN-1
          ND1 = CLNCON(N)
          ND2 = CLNCON(N+1)
          CALL FINDJA(ND1,ND2)
          CALL FINDJA(ND2,ND1)
        ENDDO
      ENDDO
C4-----RETURN
      RETURN
      END
C -----------------------------------------------------------------------
      SUBROUTINE SFILLPGF_CLN
C     ******************************************************************
C     COMPUTE AND FILL CONSTANT TERMS INTO PGF ARRAY FOR CONDUIT DOMAIN FLOW
C     ALSO FILL AREA IN FAHL, FILL CL1, AND CL2 FOR CLN NODES
C     AND SET IVC = 3 OR 4 FOR CONDUIT-CONDUIT OR CONDUIT-MATRIX CONNECTIONS
C     ******************************************************************
      USE GLOBAL, ONLY:NODES,NJA,IA,PGF,FAHL,TOP,BOT,CL1,CL2,NODES,NLAY,
     1            NODLAY,IBOUND,JA,JAS,IVC,ISYM,AREA,IDEALLOC_HY
      USE CLN1MODULE, ONLY: ACLNNDS,NCLNNDS,NNDCLN,ACLNGWC,NCLNGWC,
     1                  NCLN,CLNCON
      USE GWFBCFMODULE,ONLY:HK,CV,LAYCON,LAYAVG,IKVFLAG
      DOUBLE PRECISION FK,AREAF,EL,RADFSQ,CWCn,RO,ROD,CWND,DEX,DEXN,DEZ,
     1  AREAF1,AREAF2,FK1,FK2,FPER,FRAD,FANISO,EL1,EL2,AKVAL
C
C--------------------------------------------------------------------------------------
C1-----CONNECT CLN NODES TO EACH OTHER
C--------------------------------------------------------------------------------------
      PI = 3.1415926
C1A-----loop over all CLN segments
      DO IFR = 1,NCLN
        NN = NNDCLN(IFR)
        NB = NNDCLN(IFR-1)+1
        DO N = NB,NN-1
C1B---------for all nodes of a CLN segment connect with next CLN-node
          ND1 = CLNCON(N)
          ND2 = CLNCON(N+1)
          NC1 = ND1 - NODES
          NC2 = ND2 - NODES
          IC1 = ACLNNDS(NC1,2)     !CLN TYPE FOR NODE 1
          IC2 = ACLNNDS(NC2,2)     !CLN TYPE FOR NODE 2
C1C---------FIND LOWER ROW NUMBER TO FILL UPPER DIAGONAL OF PGF
          IF(ND2.GT.ND1)THEN
            NL = ND1
            NH = ND2
          ELSE
            NL = ND2
            NH = ND1
          ENDIF
C--------------------------------------------------------------------------------------
C2---------COMPUTE AND FILL CONDUCTANCE TERM FOR CLN-CLN CONNECTION IN PGF
          DO II = IA(NL)+1,IA(NL+1)-1
            JJ = JA(II)
            IF(JJ.NE.NH) CYCLE
            IIS = JAS(II)
C
            CALL CLNA(IC1,AREAF1)
            CALL CLNK(IC1,FK1)
            CALL CLNA(IC2,AREAF2)
            CALL CLNK(IC2,FK2)
            AREAF = MIN(AREAF1,AREAF2)             ! MIN OF AREAS
            EL1 = ACLNNDS(NC1,4)
            EL2 = ACLNNDS(NC2,4)
            EL = 0.5*(EL1+EL2)
            FK = (EL1+EL2)*(FK1 * FK2) / (FK1*EL1 + FK2*EL2)     ! LENGTH-WEIGHTED HARMONIC MEAN OF CONDUCTIVITY
            CWCn = AREAF * FK / EL
            IF(CWCn.GT.1.0E7) CWCn = 1.0E7
            PGF(IIS) = CWCn
C--------------------------------------------------------------------------------------
C3-----------ALSO FILL AREA, IVC, CL1, CL2 - NEEDED FOR TRANSPORT
            FAHL(IIS) = AREAF
            IVC(IIS) = 3
            CL1(IIS) = ACLNNDS(NC1,4)  !DIVIDE BY 2?
            CL2(IIS) = ACLNNDS(NC2,4)  !DIVIDE BY 2?
          ENDDO
        ENDDO
      ENDDO
C
C--------------------------------------------------------------------------------------
C4-----CONNECT CLN NODES WITH POROUS MATRIX
C-------------------------------------------------------------------------------------
C4A-----loop over all conduit node to GW connections
      DO IFN = 1,NCLNGWC
        IH = ACLNGWC(IFN,1)
        NH = ACLNNDS(IH,1)
        NL = ACLNGWC(IFN,2)
        DO II = IA(NL)+1,IA(NL+1)-1
          JJ = JA(II)
          IF(JJ.NE.NH) CYCLE
          IIS = JAS(II)
C--------------------------------------------------------------------------------------
C4B---------FIRST COMPUTE EFFECTIVE CELL RADIUS FOR THIS CONNECTION
          FANISO = ACLNGWC(IFN,5)
          IFNC = ACLNGWC(IFN,1)
          IFDIR = ACLNNDS(IFNC,3)
          IF(IFDIR.EQ.0)THEN
C4B1---------GET RO FOR VERTICAL WELL USING ISOTROPIC THIEM EQUATION,
C4B1--------USE ANISOTROPY FOR CONDUCTANCE (PGF)
            RO = 0.0
            DEXN = 0.0
            DO IJ = IA(NL)+1,IA(NL+1)-1
              JJ1 = JA(IJ)
              IF(JJ1.GT.NODES)CYCLE
              IJS = JAS(IJ)
              IF(IVC(IJS).EQ.1) CYCLE
              RO = RO + CL1(IJS)**2
              DEXN = DEXN + 1.0
            ENDDO
            RO = 0.28 * SQRT(2.0*RO/DEXN)
          ELSE
C4B2--------GET RO FOR HORIZONTAL WELL USING ANISOTROPIC EQUATION
            DEX = 0.0
            DEXN = 0.0
            DO IJ = IA(NL)+1,IA(NL+1)-1
              JJ = JA(IJ)
              IF(JJ.GT.NODES)CYCLE
              IJS = JAS(IJ)
              IF(IVC(IJS).EQ.1) CYCLE
              DEX = DEX + CL1(IJS)
              DEXN = DEXN + 1.0
            ENDDO
            DEX = 2.0 * DEX / DEXN
            DEZ = TOP(NL) - BOT(NL)
            RO = DEX**2 * SQRT(1.0/FANISO) + DEZ**2 * SQRT(FANISO)
            ROD = (1.0/FANISO)**0.25 + FANISO**0.25
            RO = 0.28 * SQRT(RO) / ROD
          ENDIF
C--------------------------------------------------------------------------------------
C5---------COMPUTE CONDUCTANCE TERM FOR THE DIFFERENT CLN-MATRIX CONNECTION TYPES
          IFCON = ACLNGWC(IFN,3)
          FSKIN = ACLNGWC(IFN,4)
          IFTYP = ACLNNDS(IFNC,2)
          CALL CLNP(IFTYP,FPER)
          IF(IFCON.EQ.3)THEN
C
C5A-----------CONNECTION IS ACROSS A LEAKANCE TERM LIKE CONDUIT FLOW PROCESS OF MF2K5, COMPUTE LEAKANCE
            FLENG = ACLNGWC(IFN,6)
            CWCn =  FSKIN * FPER * FLENG/ FANISO
          ELSEIF(IFCON.EQ.2)THEN
C
C5B-----------CONNECTION IS ACROSS A LEAKANCE TERM LIKE CONDUIT FLOW PROCESS PACKAGE, LEAKANCE IS INPUT
            CWCn = FSKIN
          ELSEIF(IFCON.EQ.0.OR.IFCON.EQ.1)THEN
C
C5C-----------CONNECTION USES THIEM EQUATION LIKE MULTI-NODE WELL PACKAGE
            FLENG = ACLNGWC(IFN,6)
            CALL CLNR(IFTYP,FRAD)
            CWND = LOG(RO / FRAD) + FSKIN
C5C1------------COMPUTE THE CONDUCTANCE TERM            
            CWCn = 2.0*PI*HK(NL) * SQRT(1.0/FANISO)*FLENG / CWND
          ENDIF
          PGF(IIS) = CWCn
C--------------------------------------------------------------------------------------
C6-----------ALSO FILL AREA, IVC, CL1, CL2 - NEEDED FOR TRANSPORT
          FAHL(IIS) = FPER * FLENG
          IVC(IIS) = 4
          CL1(IIS) = FRAD
          CL2(IIS) = RO
        ENDDO
      ENDDO
C-----------------------------------------------------------------------------------
C7------------FILL X-SECTIONAL AREA NEEDED FOR TRANSPORT AND STORAGE TERM
      DO IFNC=1,NCLNNDS
        IFTYP = ACLNNDS(IFNC,2)
        N = ACLNNDS(IFNC,1)
        CALL CLNA(IFTYP,AREAF)
        AREA(N) = AREAF
      ENDDO
C
      IF(IDEALLOC_HY.EQ.2)  DEALLOCATE(HK)
C7------RETURN
      RETURN
      END
C--------------------------------------------------------------------------------------
      SUBROUTINE ADDIAJA_CLN
C     ******************************************************************
C     ADD IA AND JA OF CLN NODES TO THE SUBSURFACE IA AND JA ARRAYS
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
C
      USE GLOBAL,     ONLY:NODES,NEQS,NJA,IA,JA
      INTEGER, SAVE,    DIMENSION(:),ALLOCATABLE  ::IAT
      INTEGER, SAVE,    DIMENSION(:),ALLOCATABLE  ::JAT
C
C     ------------------------------------------------------------------
C
C1--------ALLOCATE AND FILL TEMPORARY IA AND JA ARRAYS FOR SUBSURFACE DOMAIN
      ALLOCATE(IAT(NODES+1))
      ALLOCATE(JAT(NJA))
      DO N=1,NODES+1
        IAT(N) = IA(N)
      ENDDO
      DO IJA=1,NJA
        JAT(IJA) = JA(IJA)
      ENDDO
      DEALLOCATE(JA)
C
C2-------CONVERT IA TO INDICATE CONNECTIONS PER ROW OF MATRIX
        DO N=1,NODES
          IA(N) = IA(N+1) - IA(N)
        ENDDO
        IA(NODES+1) = 0
C3-------ADD CLN NODES TO IA
        CALL SFILLIA_CLN
C4-------ADJUST DIAGONAL FOR CLN NODES
        DO N = NODES+1,NEQS
          IA(N) = IA(N) + 1
        ENDDO
C
C5-------RE-COMPUTE CUMULATIVE OF CONNECTIONS PER ROW IN IA
        DO II=2,NEQS+1
          IA(II) = IA(II) + IA(II-1)
        ENDDO
C---------IA(N+1) IS CUMULATIVE_IA(N) + 1
        DO II=NEQS+1,2,-1
          IA(II) = IA(II-1) + 1
        ENDDO
        IA(1) = 1
C6---------GET NEW NJA AND ALLOCATE NEW JA ACCORDINGLY
        NJA = IA(NEQS+1) - 1
        ALLOCATE(JA(NJA))
        JA = 0
C7--------FILL SUBSURFACE BLOCK TERMS INTO JA ARRAY AS PER NEW IA
        DO N=1,NODES
          IJA = IA(N)
          DO IT = IAT(N),IAT(N+1)-1
            JA(IJA) = JAT(IT)
            IJA = IJA + 1
          ENDDO
        ENDDO
C
C8--------FILL JA TERMS FOR CLN DOMAIN
        CALL FILLJA_CLN
C9------DEALLOCATE TEMPORATY ARRAYS
      DEALLOCATE(IAT,JAT)

C10------RETURN
      RETURN
      END
C------------------------------------------------------------------------
      SUBROUTINE CLN1FM(KPER)
C     ******************************************************************
C     COMPUTE CONDUCTANCE TERM IN AMAT AND ADD STORAGE TO AMAT AND RHS
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:ISSFLG,IWADICLN
C     ------------------------------------------------------------------
C
C1-----CALCULATE CONDUIT-CONDUIT AND CONDUIT-MATRIX CONDUCTANCES IN AMAT
      CALL SCLN1H4
C
C2------SET STEADY-STATE FLAG
      ISS=ISSFLG(KPER)
C
C3------IF THE SIMULATION IS TRANSIENT ADD STORAGE TO DIAGONAL AND RHS
      IF(ISS.NE.0) GO TO 201
      CALL SCLN1S4
 201  CONTINUE      
C4------PROVIDE VERTICAL FLOW CORRECTION
      IF(IWADICLN.NE.0) CALL SCLN1WADI
C5------RETURN
      RETURN
      END
C------------------------------------------------------------------------
      SUBROUTINE SCLN1WADI
C     ******************************************************************
C     COMPUTE VERTICAL FLOW CORRECTION FOR CLN-CLN AND CLN-GW FLOW
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:NODES,NLAY,IBOUND,HNEW,BUFF,BOT,TOP,ISYM,IOUT,
     1    NODLAY,AMAT,RHS,IA,JA,JAS,PGF
      USE CLN1MODULE, ONLY:  NCLN,NNDCLN,NCLNNDS,ACLNNDS,CLNCON,
     1    ACLNGWC,NCLNGWC,IFLINCLN,HWADICC,HWADICG,ICCWADICLN,ICGWADICLN
      USE GWFBCFMODULE,ONLY:IWDFLG,WETFCT,IHDWET,IWETIT,LAYCON,HDRY,
     1                      HK,WETDRY,LAYAVG
      USE SMSMODULE, ONLY: EPSILON
C
      DOUBLE PRECISION HD,BBOT,TTOP,THCK,ZERO,PERIF,PERIW,SILLBOT,
     1           TOTTHICK,X,Y
C     ------------------------------------------------------------------
C1------FILL HWADICC TERM FOR EACH CLN DOMAIN NODE WITH FLOW CORRECTION
      ALLOCATE( HWADICC(NCLNNDS))
      HWADICC = 0.0
      DO I=1,NCLNNDS
        N = I + NODES
        IF(ICCWADICLN(I).NE.0)THEN
          X = HNEW(N) - ACLNNDS(I,5)
          CALL WADIFN(X,Y)
          HWADICC(I) = Y + ACLNNDS(I,5)
        ELSE
          HWADICC(I) = HNEW(N)
        ENDIF    
      ENDDO    
C----------------------------------------------------------------------------
C2------LOOP OVER ALL CLN-CLN CONNECTIONS FOR LEAKAGE CORRECTION
      DO IFR = 1,NCLN
        NN = NNDCLN(IFR)
        NB = NNDCLN(IFR-1)+1
        DO N = NB,NN-1
C2A---------CONNECTION WITH NEXT CLN-NODE OF A SEGMENT
          ND1 = CLNCON(N)
          ND2 = CLNCON(N+1)
          IF(IBOUND(ND1).EQ.0.OR.IBOUND(ND2).EQ.0) CYCLE
C2B---------FIND LOWER ROW NUMBER FOR UPPER DIAGONAL OF PGF
          IF(ND2.GT.ND1)THEN
            NL = ND1
            NH = ND2
          ELSE
            NL = ND2
            NH = ND1
          ENDIF
C2C---------COMPUTE AND FILL CORRECTION TERM FOR CLN-CLN CONNECTION
          DO II = IA(NL)+1,IA(NL+1)-1
            JJ = JA(II)
            IF(JJ.NE.NH) CYCLE
            IIS = JAS(II)
C2D---------FIND UPSTREAM AND DOWNSTREAM NODES
            IUP = NL
            IF(HNEW(NH).GT.HNEW(NL)) IUP = NH
            IDN = NL
            IF(IUP.EQ.NL) IDN = NH
C2E---------SKIP CORRECTION IF DOWNSTREAM NODE DOES NOT NEED CORRECTION
            IDNL = IDN - NODES
            IF(ICCWADICLN(IDNL).EQ.0) CYCLE
C2F-----------FIND MATRIX LOCATION OF DOWNSTREAM NODE          
            ILOC = II  !MATRIX LOCATION FOR NL BEING DOWNSTREAM NODE
            IF(NH.EQ.IDN) ILOC = ISYM(II)            
C2G---------FILL CORRECTION FOR CONNECTION 
            RHS(IDN) = RHS(IDN) + AMAT(ILOC)*(HWADICC(IDNL) - HNEW(IDN))
            RHS(IUP) = RHS(IUP) - AMAT(ILOC)*(HWADICC(IDNL) - HNEW(IDN))
          ENDDO
        ENDDO
      ENDDO
C-----------------------------------------------------------------------------
C3------FILL HWADICG TERM FOR CLN-GWF CONNECTIONS WITH D/S FLOW CORRECTION
C-----------------------------------------------------------------------------
      ALLOCATE( HWADICG(NCLNGWC))
      HWADICG = 0.0
C3A------LOOP OVER ALL CLN-GWF CONNECTIONS FOR LEAKAGE CORRECTION      
      DO IFN=1,NCLNGWC
        IH = ACLNGWC(IFN,1)          
        NH = ACLNNDS(IH,1)
        NL = ACLNGWC(IFN,2)
        IF(IBOUND(NH).EQ.0.OR.IBOUND(NL).EQ.0) CYCLE          
C3B---------FIND UPSTREAM AND DOWNSTREAM NODES
        IUP = NL
        IF(HNEW(NH).GT.HNEW(NL)) IUP = NH
        IDN = NL
        IF(IUP.EQ.NL) IDN = NH
C3C------FILL THE CORRECTED HEAD OF THE DOWNSTREAM CELL OF CLN-GW CONNECTION IN HWADICG
        IF(ICGWADICLN(IFN).NE.0)THEN
          SILLBOT = ACLNNDS(IH,5)
          IF(BOT(NL).GT.SILLBOT) SILLBOT = BOT(NL)
          X = HNEW(IDN) - SILLBOT
          CALL WADIFN(X,Y)
          HWADICG(IFN) = Y + SILLBOT
        ELSE
          HWADICG(IFN) = HNEW(IDN)
        ENDIF
C3E---------COMPUTE AND FILL FLOW CORRECTION FOR CLN-GWF CONNECTION
        DO II = IA(NL)+1,IA(NL+1)-1
          JJ = JA(II)
          IF(JJ.NE.NH) CYCLE
C3F-----------FIND MATRIX LOCATION OF DOWNSTREAM NODE          
          ILOC = II  !MATRIX LOCATION FOR NL BEING DOWNSTREAM NODE
          IF(NH.EQ.IDN) ILOC = ISYM(II)
C3G-------FILL CORRECTION FOR CONNECTION 
          RHS(IDN) = RHS(IDN) + AMAT(ILOC)*(HWADICG(IFN) - HNEW(IDN)) 
          RHS(IUP) = RHS(IUP) - AMAT(ILOC)*(HWADICG(IFN) - HNEW(IDN))
        ENDDO
      ENDDO
C
C4-----RETURN.
      RETURN
      END
C------------------------------------------------------------------------
      SUBROUTINE SCLN1H4
C     ******************************************************************
C     COMPUTE CONDUCTANCE FOR CLN FROM PGF AND SATURATED THICKNESS, IN AMAT
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:NODES,NLAY,IBOUND,HNEW,BUFF,BOT,TOP,ISYM,IOUT,
     1    NODLAY,AMAT,RHS,IA,JA,JAS,PGF,ICONCV,Sn,AKRC,AKR,iunsat
      USE CLN1MODULE, ONLY:  NCLN,NNDCLN,NCLNNDS,ACLNNDS,CLNCON,
     1    ACLNGWC,NCLNGWC,IFLINCLN
      USE GWFBCFMODULE,ONLY:IWDFLG,WETFCT,IHDWET,IWETIT,LAYCON,HDRY,
     1                      HK,WETDRY,LAYAVG
      USE SMSMODULE, ONLY: EPSILON
C
      DOUBLE PRECISION HD,BBOT,TTOP,THCK,ZERO,PERIF,PERIW,
     1           TOTTHICK
C     ------------------------------------------------------------------
C1------SET CONSTANTS
      ZERO=0.
C----------------------------------------------------------------------------
C2------LOOP THROUGH EACH CLN CELL AND COMPUTE FRACTION SATURATED
C
      DO 200 ICLN=1,NCLNNDS
        N = ACLNNDS(ICLN,1)
        IFLIN = IFLINCLN(ICLN)
        IF(IBOUND(N).EQ.0.OR.IFLIN.EQ.1) CYCLE
C
C2A------CALCULATE SATURATED THICKNESS.
        HD=HNEW(N)
        BBOT = ACLNNDS(ICLN,5)
        CALL CLN_THIK(ICLN,HD,BBOT,THCK)
C
C2B-----STORE IN Sn ARRAY AND MOVE TO NEXT NODE.
        Sn(N)=THCK
        AKR(N) = THCK
  200 CONTINUE
C----------------------------------------------------------------------------
C3------FILL AKRC WITH UPSTREAM KR OF THE CONNECTION FOR ALL CLN-CLN CONNECTIONS
      DO IFR = 1,NCLN
        NN = NNDCLN(IFR)
        NB = NNDCLN(IFR-1)+1
        DO N = NB,NN-1
C2A---------CONNECTION WITH NEXT CLN-NODE OF A SEGMENT
          ND1 = CLNCON(N)
          ND2 = CLNCON(N+1)
          IF(IBOUND(ND1).EQ.0.OR.IBOUND(ND2).EQ.0) CYCLE
C2B---------FIND LOWER ROW NUMBER FOR UPPER DIAGONAL OF PGF
          IF(ND2.GT.ND1)THEN
            NL = ND1
            NH = ND2
          ELSE
            NL = ND2
            NH = ND1
          ENDIF
C2C---------COMPUTE AND FILL AKRC TERM FOR CLN-CLN CONNECTION
          DO II = IA(NL)+1,IA(NL+1)-1
            JJ = JA(II)
            IF(JJ.NE.NH) CYCLE
            IIS = JAS(II)
C2D---------FIND UPSTREAM NODE AND HIGHER BOT NODE
            IUPS = NL
            IF(HNEW(JJ).GT.HNEW(NL)) IUPS = JJ
            IHBOT = NL
            BNL = ACLNNDS(NL-NODES,5)
            BJJ = ACLNNDS(JJ-NODES,5)
            IF(BJJ.GT.BNL) IHBOT = JJ
C2E---------FILL AKRC FOR CONNECTION
            INDK = 0
            IF(IUPS.EQ.IHBOT) INDK = 1
            IF(ABS(BJJ-BNL).LT.0.01) INDK = 1
            IF(INDK.EQ.1)THEN
              AKRC(IIS) = AKR(IUPS)
            ELSE
              IFLIN = IFLINCLN(IUPS-NODES)
              IF(IFLIN.EQ.1) CYCLE
              HD=HNEW(IUPS)
              ICLN= IHBOT-NODES
              BBOT = ACLNNDS(ICLN,5)
              CALL CLN_THIK(ICLN,HD,BBOT,THCK)
              AKRC(IIS) = THCK
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C-----------------------------------------------------------------------------
C3------FILL AKRC WITH UPSTREAM AKR OF THE CONNECTION FOR ALL CLN-GWF CONNECTIONS
C-----------------------------------------------------------------------------
      DO IFN = 1,NCLNGWC
        IH = ACLNGWC(IFN,1)
        NH = ACLNNDS(IH,1)
        NL = ACLNGWC(IFN,2)
        IF(IBOUND(NH).EQ.0.OR.IBOUND(NL).EQ.0) CYCLE
C3A---------COMPUTE AND FILL AKRC TERM FOR CLN-GWF CONNECTION
        DO II = IA(NL)+1,IA(NL+1)-1
          JJ = JA(II)
          IF(JJ.NE.NH) CYCLE
          IIS = JAS(II)
C3B---------FIND UPSTREAM NODE AND HIGHER BOT NODE
          IUPS = NL
          IF(HNEW(JJ).GT.HNEW(NL)) IUPS = JJ
          IHBOT = NL
          BNH = ACLNNDS(IH,5)
          BNL = BOT(NL)
          IF(BNH.GT.BNL) IHBOT = NH
          IF(ACLNNDS(IH,3).EQ.1)THEN
C3C---------FILL AKRC FOR HORIZONTAL CLN CELL (USE UPSTREAM WETTED PERIMETER)
              IFLIN = IFLINCLN(IUPS-NODES)
              IF(IFLIN.EQ.1) CYCLE
              BBOT = ACLNNDS(IH,5)
              IF(HNEW(IUPS).GT.(BBOT-EPSILON))THEN !OTHERWISE AKRC IS ZERO FOR THE CONNECTION
                HD=HNEW(IUPS)
                IFTYP = ACLNNDS(IH,2)
                CALL CLNP (IFTYP,PERIF)
                CALL CLNPW (IH,HD,PERIW)
                THCK = PERIW/PERIF
                AKRC(IIS) = THCK
              ENDIF
          ELSE
C3D---------FILL ARKC FOR VERTICAL CLN CELL (USE UPSTREAM SATURATIONS)
            INDK = 0
            IF(IUPS.EQ.IHBOT) INDK = 1
            IF(ABS(BNH-BNL).LT.0.01) INDK = 1
            IF(INDK.EQ.1)THEN
              AKRC(IIS) = AKR(IUPS)
            ELSE
              HD=HNEW(IUPS)
              IF(IUPS.EQ.NH) THEN !CLN CELL IS UPSTREAM, GWF CELL HAS HIGHER BOT
                IFLIN = IFLINCLN(IUPS-NODES)
                IF(IFLIN.EQ.1) CYCLE
                BBOT = BOT(IHBOT)
                CALL CLN_THIK(IH,HD,BBOT,THCK)
              ELSE !GWF CELL IS UPSTREAM, CLN CELL HAS HIGHER BOT
                BBOT=ACLNNDS(NH-NODES,5)
                TTOP=TOP(NL)
                TOTTHICK = TTOP - BBOT
                CALL SAT_THIK(N,HD,TOTTHICK,BBOT,THCK)
              ENDIF
              AKRC(IIS) = THCK
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
C----------------------------------------------------------------------------
C3-----COMPUTE CONDUIT-CONDUIT CONDUCTANCE FROM AKRC  AND PGF
C3A----LOOP OVER ALL CONDUIT SEGMENTS
      DO IFR = 1,NCLN
        NN = NNDCLN(IFR)
        NB = NNDCLN(IFR-1)+1
        DO N = NB,NN-1
C3B---------CONNECTION WITH NEXT CONDUIT-NODE OF A CONDUIT SEGMENT
          ND1 = CLNCON(N)
          ND2 = CLNCON(N+1)
          IF(IBOUND(ND1).EQ.0.OR.IBOUND(ND2).EQ.0) CYCLE
C3C---------FIND LOWER ROW NUMBER FOR UPPER DIAGONAL OF PGF
          IF(ND2.GT.ND1)THEN
            NL = ND1
            NH = ND2
          ELSE
            NL = ND2
            NH = ND1
          ENDIF
C3D---------COMPUTE AND FILL AMAT TERM FOR CONDUIT-CONDUIT CONNECTION
          DO II = IA(NL)+1,IA(NL+1)-1
            JJ = JA(II)
            IF(JJ.NE.NH) CYCLE
            IIS = JAS(II)
            IUPS = JJ
            IF(HNEW(JJ).LT.HNEW(NL)) IUPS = NL
C3D1--------FILL OFFDIAGONAL TERMS IN ROWS NL AND NH
            AMAT(II) = PGF(IIS)*AKRC(IIS)
            AMAT(ISYM(II)) = PGF(IIS)*AKRC(IIS)
C3D2--------ADD TO DIAGONAL TERMS IN ROWS NL AND NH
            AMAT(IA(NL)) = AMAT(IA(NL)) - PGF(IIS)*AKRC(IIS)
            AMAT(IA(NH)) = AMAT(IA(NH)) - PGF(IIS)*AKRC(IIS)
          ENDDO
C
        ENDDO
      ENDDO
C----------------------------------------------------------------------------
C4----COMPUTE CONDUIT-MATRIX CONDUCTANCE FROM AKRC AND PGF
C4A---Loop over all conduit nodes
      DO IFN = 1,NCLNGWC
        IH = ACLNGWC(IFN,1)
        NH = ACLNNDS(IH,1)
        NL = ACLNGWC(IFN,2)
        IF(IBOUND(NH).EQ.0.OR.IBOUND(NL).EQ.0) CYCLE
        DO II = IA(NL)+1,IA(NL+1)-1
          JJ = JA(II)
          IF(JJ.NE.NH) CYCLE
          IIS = JAS(II)
C
          IUPS = JJ
          IF(HNEW(JJ).LT.HNEW(NL)) IUPS = NL
C4B---------FILL OFFDIAGONAL TERMS IN ROWS NL AND NH
          AMAT(II) = PGF(IIS)*AKRC(IIS)
          AMAT(ISYM(II)) = PGF(IIS)*AKRC(IIS)
C4C---------ADD TO DIAGONAL TERMS IN ROWS NL AND NH
          AMAT(IA(NL)) = AMAT(IA(NL)) - PGF(IIS)*AKRC(IIS)
          AMAT(IA(NH)) = AMAT(IA(NH)) - PGF(IIS)*AKRC(IIS)
        ENDDO
      ENDDO
C
C5-----RETURN.
      RETURN
      END
C
C -----------------------------------------------------------------------
      SUBROUTINE SCLN1S4
C     ******************************************************************
C     FILL STORAGE TERMS IN AMAT AND RHS FOR CLN DOMAIN.
C     ******************************************************************
      USE GLOBAL, ONLY:JA,IA,NODES,NEQS,AMAT,RHS,IBOUND,Sn,So,HNEW,AREA
      USE CLN1MODULE, ONLY:NCLN,NCLNNDS,ACLNNDS,NNDCLN,CLNCON,IFLINCLN
      USE GWFBASMODULE,ONLY:DELT
      DOUBLE PRECISION SATO,SATN,EPS,HD,THCK,DS,RHO2,TLED,BBOT
C----------------------------------------------------------------------
C1------LOOP OVER ALL CLN NODES
      TLED = 1.0/DELT
      DO ICLN=1,NCLNNDS
        N = ACLNNDS(ICLN,1)
        IFLIN = IFLINCLN(ICLN)
        IF(IBOUND(N).EQ.0.OR.IFLIN.EQ.1) CYCLE
C2--------COMPUTE TERM USING NEWTON LINEARIZATION
        SATO = So(N)
        SATN = Sn(N)
        EPS = 1.0E-3
        HD=HNEW(N)+ EPS
        BBOT = ACLNNDS(ICLN,5)
        CALL CLN_THIK(ICLN,HD,BBOT,THCK)
        DS = (THCK - SATN)/EPS
        IF(DS.LT.1.0E-7) DS = 1.0E-7
        RHO2  = AREA(N) * ACLNNDS(ICLN,4) * TLED
        AMAT(IA(N)) = AMAT(IA(N)) - RHO2 * DS
        RHS(N) = RHS(N) - RHO2*DS*HNEW(N) + RHO2 * (SATN-SATO)
      ENDDO
C3-----RETURN
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE CLN_THIK(ICLN,HD,BBOT,THCK)
C     ******************************************************************
C     COMPUTE FRACTION OF TOTAL VOLUME THAT IS SATURATED
C     FOR CONDUIT NODE AND STORE IN THCK -
C     FRACTION SATURATED DEPENDS ON CONDUIT ORIENTATION
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:BOT,TOP,ISSFLG,NODES
      USE CLN1MODULE, ONLY: ACLNNDS,ACLNCOND
      DOUBLE PRECISION THCK,HD,BBOT,TTOP,X,S,V,COF1,COF2,FACTOR1,FACTOR2
     *  ,eps,acof,y,TOTTHICK,FRAD,PI,DEPTH,AREAF,AREAW
C     ------------------------------------------------------------------
C1------GET DIRECTION OF LINE SEGMENT
      IFDIR = ACLNNDS(ICLN,3)
      I = ACLNNDS(ICLN,1)
C--------------------------------------------------------
      IF(IFDIR.EQ.0)THEN
C2-------VERTICAL LINE SEGMENT
        TOTTHICK = ACLNNDS(ICLN,4)
        CALL SAT_THIK(I,HD,TOTTHICK,BBOT,THCK)
      ELSEIF(IFDIR.EQ.1)THEN
C3-------HORIZONTAL LINE SEGMENT CONDUIT
        IC = ACLNNDS(ICLN,2)
        CALL CLNA(IC,AREAF)
        CALL CLNAW(ICLN,HD,AREAW)
        THCK = AREAW / AREAF
      ELSEIF(IFDIR.EQ.2)THEN
C4-------ANGLED CONDUIT
        FANGLE = ACLNNDS(ICLN,6)
        TOTTHICK = ACLNNDS(ICLN,4) * SIN(FANGLE)
        BBOT = ACLNNDS(ICLN,5)
        I = ACLNNDS(ICLN,1)
        CALL SAT_THIK(I,HD,TOTTHICK,BBOT,THCK)
      ENDIF
      IF(THCK.LT.1.0E-7) THCK = 1.0E-7
C
C5------RETURN.
      RETURN
      END
C--------------------------------------------------------------------------------
      SUBROUTINE CLNV(ICLN,THCK)
C     ******************************************************************
C     COMPUTE VERTICAL GRID DIMENSION FOR CLN NODE AND STORE IN THCK
C     ******************************************************************
C
C      SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:BOT,TOP,ISSFLG,NODES
      USE CLN1MODULE, ONLY: ACLNNDS,ACLNCOND
      DOUBLE PRECISION THCK,HD,BBOT,TTOP,X,S,V,COF1,COF2,FACTOR1,FACTOR2
     *  ,eps,acof,y,TOTTHICK,Aw,FRAD,PI,DEPTH
C     ------------------------------------------------------------------
C1------GET DIRECTION OF LINE SEGMENT
      IFDIR = ACLNNDS(ICLN,3)
      I = ACLNNDS(ICLN,1)
C--------------------------------------------------------
      IF(IFDIR.EQ.0)THEN
C2-------VERTICAL LINE SEGMENT
        THCK = ACLNNDS(ICLN,4)
      ELSEIF(IFDIR.EQ.1)THEN
C3-------HORIZONTAL LINE SEGMENT CONDUIT
        PI = 3.1415926
        IC = ACLNNDS(ICLN,2)
        CALL CLNR(IC,FRAD)
        THCK = 2.0 * PI * FRAD
      ELSEIF(IFDIR.EQ.2)THEN
C4-------ANGLED CONDUIT
        FANGLE = ACLNNDS(ICLN,6)
        THCK = ACLNNDS(ICLN,4) * SIN(FANGLE)
      ENDIF
C
C5------RETURN.
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE CLN1BDS(KSTP,KPER)
C     ******************************************************************
C     COMPUTE STORAGE BUDGET FLOW TERM FOR CLN.
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,  ONLY:NCOL,NROW,NLAY,ISSFLG,IBOUND,HNEW,HOLD,
     1             BUFF,TOP,IOUT,NODES,NODLAY,IUNSTR,Sn,So,AREA
      USE CLN1MODULE, ONLY: ACLNNDS,NCLNNDS,ICLNCB,IFLINCLN
      USE GWFBASMODULE,ONLY:MSUM,ICBCFL,VBVL,VBNM,DELT,PERTIM,TOTIM
C
      CHARACTER*16 TEXT
      DOUBLE PRECISION STOIN,STOUT,SSTRG,SIN,SOUT,TLED,HSING,STRG,
     *  RHO,RHO1,RHO2,SNEW,SOLD,ONE,SATN,SATO,HD,THCK,BBOT
C
      DATA TEXT /'     CLN STORAGE'/
C     ------------------------------------------------------------------
C1------IF STEADY STATE, STORAGE TERM IS ZERO SO RETURN
      ISS=ISSFLG(KPER)
      IF(ISS.NE.0) RETURN
C
C2------INITIALIZE BUDGET ACCUMULATORS AND 1/DELT.
      ZERO=0.
      STOIN=ZERO
      STOUT=ZERO
      ONE=1.0
      TLED=ONE/DELT
C
C3------IF CELL-BY-CELL FLOWS WILL BE SAVED, SET FLAG IBD.
      IBD=0
      IF(ICLNCB.GT.0) IBD=ICBCFL
C
C4------CLEAR BUFFER.
      DO 210 N=1,NCLNNDS
      BUFF(N)=ZERO
210   CONTINUE
C
C5------LOOP THROUGH EVERY CLN CELL IN THE GRID.
      DO ICLN=1,NCLNNDS
        N = ACLNNDS(ICLN,1)
        IFLIN = IFLINCLN(ICLN)
        IF(IBOUND(N).EQ.0.OR.IFLIN.EQ.1) CYCLE
        HD=HNEW(N)
        SATO = So(N)
        BBOT = ACLNNDS(ICLN,5)
        CALL CLN_THIK(ICLN,HD,BBOT,THCK)
        SATN = THCK
C6--------COMPUTE STORAGE
        STRG  = AREA(N) * ACLNNDS(ICLN,4) * TLED * (SATO-SATN)
C
C7-------STORE STORAGE TERM IN BUFFER AND ADD TO ACCUMULATORS.
        BUFF(ICLN)=STRG
        SSTRG=STRG
        IF(STRG.LT.ZERO) THEN
          STOUT=STOUT-SSTRG
        ELSE
          STOIN=STOIN+SSTRG
        END IF
      ENDDO
C
  300 CONTINUE
C
C8-----IF IBD FLAG IS SET RECORD THE CONTENTS OF THE BUFFER.
        IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,
     1                       ICLNCB,BUFF,NCLNNDS,1,1,IOUT)
        IF(IBD.EQ.2) CALL UBDSV1(KSTP,KPER,TEXT,ICLNCB,
     1            BUFF,NCLNNDS,1,1,IOUT,DELT,PERTIM,TOTIM,IBOUND)
C
C9------ADD TOTAL RATES AND VOLUMES TO VBVL & PUT TITLE IN VBNM.
  400 CONTINUE
      SIN=STOIN
      SOUT=STOUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+SIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+SOUT*DELT
      VBVL(3,MSUM)=SIN
      VBVL(4,MSUM)=SOUT
      VBNM(MSUM)=TEXT
      MSUM=MSUM+1
C
C10----RETURN.
      RETURN
      END
      SUBROUTINE CLN1BDADJ(KSTP,KPER)
C     ******************************************************************
C     COMPUTE FLOW BETWEEN CLN-CLN AND CLN-MATRIX
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,BUFF,AMAT,NODLAY,
     1           BOT,IOUT,NODES,NJA,IA,JA,JAS,IUNSTR,ISYM,ITRNSP,TMPA,
     1           IWADICLN
      USE CLN1MODULE, ONLY: ICLNCB,NCLN,NNDCLN,CLNCON,NCLNNDS,ACLNNDS,
     1           ACLNGWC,ICCWADICLN,ICGWADICLN,NCLNGWC
      USE GWFBASMODULE,ONLY:ICBCFL,DELT,PERTIM,TOTIM,ICHFLG
      USE GWFBCFMODULE,ONLY:IBCFCB,LAYCON
      USE GWTBCTMODULE, ONLY: CBCF
      USE SMSMODULE, ONLY: AMATFL
C
      DOUBLE PRECISION HD,TMP,HDIFF,HDN,X,Y,SILLBOT
      REAL, DIMENSION(:),ALLOCATABLE :: TMPCLN(:),TMPCP(:)
C     ------------------------------------------------------------------
C
C1------IF CELL-BY-CELL FLOWS WILL BE SAVED IN A FILE, SET FLAG IBD.
C1------RETURN IF FLOWS ARE NOT BEING SAVED OR RETURNED.
      ZERO=0.
      IBD=0
      IF(ICLNCB.GT.0) IBD=ICBCFL
      IF(ICLNCB.LT.0) IBD = -1
      IF(ITRNSP.GT.0.AND.IBD.EQ.0) IBD = 999
      IF(IBD.EQ.0) RETURN
C
C2-----COMPUTE FOR ALL CLN NODES
      DO ICLN = 1,NCLNNDS
        N = ACLNNDS(ICLN,1)
        IF(IBOUND(N).EQ.0) CYCLE
C3---------COMPUTE AND FILL FLOW TERM FROM CLN NODE TO ALL ITS CONNECTIONS
        DO II = IA(N)+1,IA(N+1)-1
          JJ = JA(II)
          IF(IBOUND(JJ).EQ.0) CYCLE
          IF(ICHFLG.EQ.0) THEN
            IF((IBOUND(N).LE.0) .AND. (IBOUND(JJ).LE.0)) CYCLE
          END IF
C
C4---------CALCULATE FLOW THROUGH THIS FACE INTO THE ADJACENT CELL.
          HD=HNEW(JJ)
          HDIFF=HNEW(N)-HD
C5---------TAKE CARE OF WADI TERMS
          IF(IWADICLN.NE.0)THEN
            IDN = JJ
            IUP = N
            IF(HNEW(N).LT.HD)THEN 
                IDN = N
                IUP = JJ
            ENDIF
            IDNCLN = IDN-NODES
            IF(JJ.GT.NODES)THEN  
C5A--------FOR CLN-CLN CONNECTION
              IF(ICCWADICLN(IDNCLN).NE.0)THEN
                X = HNEW(IDN) - ACLNNDS(IDNCLN,5)
                CALL WADIFN(X,Y)
                HDN = Y + ACLNNDS(IDNCLN,5)
                HDIFF = HNEW(IUP) - HDN
                IF(IUP.EQ.JJ) HDIFF = - HDIFF
              ENDIF
            ELSE  
C5A--------FOR CLN-GW CONNECTION
C5A1---------FIND CLN-GW CONNECTION NUMBER
              DO IFN = 1,NCLNGWC
                IGW = ACLNGWC(IFN,2)
                IF(IGW.EQ.JJ) GO TO 10
              ENDDO
10            CONTINUE
C5A2---------COMPUTE HDIFF FOR CONNECTION              
              IF(ICGWADICLN(IFN).NE.0)THEN
                NL = ACLNGWC(IFN,2)
                IH = ACLNGWC(IFN,1)
                SILLBOT = ACLNNDS(IH,5)
                IF(BOT(NL).GT.SILLBOT) SILLBOT = BOT(NL)
                X = HNEW(IDN) - SILLBOT
                CALL WADIFN(X,Y)
                HDN = Y + SILLBOT
                HDIFF = HNEW(IUP) - HDN
                IF(IUP.EQ.JJ) HDIFF = - HDIFF
              ENDIF
            ENDIF
          ENDIF
C6---------COMPUTE FLUX IN TMP ARRAY
          TMPA(II)= HDIFF*AMATFL(II)
        ENDDO
C
      ENDDO
C
      RETURN
      END
C---------------------------------------------------------------------------------------------
      SUBROUTINE CLN1BDWR(KSTP,KPER)
C     ******************************************************************
C     WRITE FLOW BETWEEN CLN-CLN AND CLN-MATRIX
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:NCOL,NROW,NLAY,IBOUND,HNEW,BUFF,AMAT,NODLAY,
     1    TOP,IOUT,NODES,NJA,IA,JA,JAS,IUNSTR,ISYM,ITRNSP,TMPA
      USE CLN1MODULE, ONLY: ICLNCB,NCLN,NNDCLN,CLNCON,NCLNNDS,ACLNNDS,
     1    NCLNGWC,ACLNGWC
      USE GWFBASMODULE,ONLY:ICBCFL,DELT,PERTIM,TOTIM,ICHFLG
      USE GWFBCFMODULE,ONLY:IBCFCB,LAYCON
      USE GWTBCTMODULE, ONLY: CBCF
C
      CHARACTER*16 TEXT(3)
      DOUBLE PRECISION HD,TMP,HDIFF
      REAL, DIMENSION(:),ALLOCATABLE :: TMPCLN(:),TMPCP(:)
C
      DATA TEXT(1) /'   FLOW CLN FACE'/
      DATA TEXT(2) /'      CLN TO GWF'/
      DATA TEXT(3) /'       CLN FLOWS'/
C     ------------------------------------------------------------------
C
C1------RETURN IF FLOWS ARE NOT BEING SAVED OR RETURNED.
      IBD=0
      IF(ICLNCB.GT.0) IBD=ICBCFL
      IF(ICLNCB.LT.0) IBD = -1
      IF(ITRNSP.GT.0.AND.IBD.EQ.0) IBD = 999
      IF(IBD.EQ.0) RETURN
      ZERO = 0.
C2------ALLOCATE TEMPORARY ARRAY FOR FLOW ACCUMULATIONS
      LCLN = NNDCLN(NCLN)
      ALLOCATE(TMPCLN(LCLN))
      ALLOCATE(TMPCP(NCLNGWC))
C
C3------INITIALIZE FLOW ACCUMULATION ARRAYS
      DO IJ=1,LCLN
        TMPCLN(IJ)=ZERO
      ENDDO
      DO IJ=1,NCLNNDS
        TMPCP(IJ)=ZERO
      ENDDO
C----------------------------------------------------------------------------
C4-----MOVE CLN-CLN FLOW INTO TEMPORARY ARRAY
      ICLN = 0
      DO IFR = 1,NCLN
        NN = NNDCLN(IFR)
        NB = NNDCLN(IFR-1)+1
        DO N = NB,NN-1
          ICLN = ICLN + 1
C4A---------FIND CONNECTION WITH NEXT CLN-NODE OF A CLN SEGMENT
          ND1 = CLNCON(N)
          ND2 = CLNCON(N+1)
          IF(IBOUND(ND1).EQ.0.OR.IBOUND(ND2).EQ.0) CYCLE
C4B---------FILL FLOW TERM FOR CLN-CLN CONNECTION
          DO II = IA(ND1)+1,IA(ND1+1)-1
            JJ = JA(II)
            IF(JJ.NE.ND2) CYCLE
            IF(ICHFLG.EQ.0) THEN
              IF((IBOUND(N).LE.0) .AND. (IBOUND(JJ).LE.0)) CYCLE
            END IF
            IIS = JAS(II)
            TMPCLN(ICLN)= TMPA(II)
            IF(ITRNSP.GT.0) CBCF(IIS) = TMPCLN(ICLN)
          ENDDO
        ENDDO
      ENDDO
C4D------RECORD CLN-CLN FLOW
      IF(IBD.EQ.1)
     1   CALL UBUDSVU(KSTP,KPER,TEXT(1),ICLNCB,TMPCLN,LCLN,IOUT,
     1         PERTIM,TOTIM)
      IF(IBD.EQ.2) CALL UBDSV1U(KSTP,KPER,TEXT(1),ICLNCB,TMPCLN,LCLN,
     1     IOUT,DELT,PERTIM,TOTIM,IBOUND,NODES)
      IF(IBD.EQ.-1)THEN
C4E-----WRITE FLOWS TO OUTPUT FILE
      IF(IOUT.GT.0) WRITE(IOUT,1) TEXT(1),KSTP,KPER
1     FORMAT(/1X,'WRITING "',A16,'" BELOW',1X,
     1     'AT TIME STEP',I7,', STRESS PERIOD',I7)
      NLAG = 0
      DO IFR = 1,NCLN
        NN = NNDCLN(IFR)
        NB = NNDCLN(IFR-1)+1
        WRITE(IOUT,2) IFR
2       FORMAT(/1X,'FLOW THROUGH CONDUIT NUMBER:',I10,' ='/1X,40('-'))
        WRITE(IOUT,3)(TMPCLN(N),N=NB-NLAG,NN-1-NLAG)
        NLAG = NLAG + 1
      ENDDO
3     FORMAT(12E15.6)
      ENDIF
C
C----------------------------------------------------------------------------
C5-----MOVE CLN-MATRIX FLOW FOR EACH CLN NODE INTO TEMPORARY ARRAY
      DO NN = 1,NCLNGWC
        IH = ACLNGWC(NN,1)
        ND1 = ACLNNDS(IH,1)
        NL = ACLNGWC(NN,2)
        IF(IBOUND(ND1).EQ.0.OR.IBOUND(NL).EQ.0) CYCLE
C5B-------FIND CLN-MATRIX CONNECTION
        DO II = IA(ND1)+1,IA(ND1+1)-1
          JJ = JA(II)
          IF(JJ.NE.NL) CYCLE
          IIS = JAS(II)
          IF(ICHFLG.EQ.0) THEN
            IF((IBOUND(ND1).LE.0) .AND. (IBOUND(JJ).LE.0)) CYCLE
          END IF
C
C5C-------FILL FLOW THROUGH THIS FACE INTO THE ADJACENT CELL.
          TMPCP(NN) = TMPA(II)
          IF(ITRNSP.GT.0) CBCF(IIS) = TMPCP(NN)
        ENDDO
      ENDDO
C5D-----RECORD CLN-MATRIX FLOW
      IF(IBD.EQ.1)
     1   CALL UBUDSVU(KSTP,KPER,TEXT(2),ICLNCB,TMPCP,NCLNGWC,IOUT,
     1         PERTIM,TOTIM)
      IF(IBD.EQ.2) CALL UBDSV1U(KSTP,KPER,TEXT(2),ICLNCB,TMPCP,NCLNGWC,
     1     IOUT,DELT,PERTIM,TOTIM,IBOUND,NODES)
      IF(IBD.EQ.-1)THEN
C5E-----WRITE FLOWS TO OUTPUT FILE
      IF(IOUT.GT.0) WRITE(IOUT,4) TEXT(2),KSTP,KPER
4     FORMAT(/1X,'WRITING "',A16,'" BELOW',1X,
     1     'AT TIME STEP',I7,', STRESS PERIOD',I7)
      WRITE(IOUT,3)(TMPCP(N),N=1,NCLNGWC)
      ENDIF
C6------DEALLOCATE ALL TEMPORARY ARRAYS
      DEALLOCATE(TMPCLN)
      DEALLOCATE(TMPCP)
C7-------RETURN
      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE SCLN1D(KSTP,KPER,IPFLG,ISA)
C     ******************************************************************
C     CALCULATE, PRINT, AND SAVE DRAWDOWNS IN CLN CELLS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY: HNEW,STRT,IBOUND,IOUT,NODES
      USE CLN1MODULE, ONLY: NCLNNDS,ICLNDD
      USE GWFBASMODULE, ONLY: PERTIM,TOTIM,IDDNFM,IDDNUN,LBDDSV,
     2                        CDDNFM,IOFLG
C
      CHARACTER*16 TEXT
      DOUBLE PRECISION SSTRT
      REAL,          SAVE,    DIMENSION(:),    ALLOCATABLE ::BUFF
C
      DATA TEXT /'    CLN DRAWDOWN'/
C     ------------------------------------------------------------------
      ALLOCATE(BUFF(NCLNNDS))
C
C1------FOR EACH CLN NODE CALCULATE DRAWDOWN IF PRINT OR SAVE IS REQUESTED.
      DO 59 N=1,NCLNNDS
C
C2------CALCULATE DRAWDOWN FOR THE NODE
      NG = N+NODES
      BUFF(N)=HNEW(NG)
      SSTRT=STRT(NG)
      IF(IBOUND(NG).NE.0) BUFF(N)=SSTRT-HNEW(NG)
   59 CONTINUE
C
C3------CALL ULAPRS OR ULAPRW TO PRINT DRAWDOWN.
      IF(ISA.NE.0) THEN
        IF(IOFLG(1,2).NE.0) THEN
          IF(IDDNFM.LT.0) CALL ULAPRS(BUFF(1),TEXT,KSTP,KPER,
     1                  NCLNNDS,1,1,-IDDNFM,IOUT)
          IF(IDDNFM.GE.0) CALL ULAPRW(BUFF(1),TEXT,KSTP,KPER,
     1                  NCLNNDS,1,1,IDDNFM,IOUT)
          IPFLG=1
        ENDIF
C
      END IF
C
C4------DETERMINE IF DRAWDOWN SHOULD BE SAVED.
C4------IF SO THEN CALL A ULASAV OR ULASV2 TO RECORD DRAWDOWN.
      IFIRST=1
      IF(ICLNDD.LE.0) GO TO 80
      IF(IOFLG(1,4).EQ.0) GO TO 80
        NSTRT = NODES+1
        IF(IFIRST.EQ.1) WRITE(IOUT,74) ICLNDD,KSTP,KPER
   74   FORMAT(1X,/1X,'CLN DRAWDOWN WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I3,', STRESS PERIOD ',I4)
        IFIRST=0
        IF(CDDNFM.EQ.' ') THEN
           CALL ULASAV(BUFF(1),TEXT,KSTP,KPER,PERTIM,TOTIM,NCLNNDS,
     1                1,1,ICLNDD)
        ELSE
           CALL ULASV2(BUFF(1),TEXT,KSTP,KPER,PERTIM,TOTIM,NCLNNDS,
     1                1,1,ICLNDD,CDDNFM,LBDDSV,IBOUND(NSTRT))
        END IF
C
80    CONTINUE
      DEALLOCATE(BUFF)

C
C5------RETURN.
      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE SCLN1H(KSTP,KPER,IPFLG,ISA)
C     ******************************************************************
C     PRINT AND SAVE HEADS IN CLN CELLS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:HNEW,IBOUND,IOUT,NODES
      USE CLN1MODULE, ONLY:  NCLNNDS,ICLNHD
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,IHEDFM,IHEDUN,LBDDSV,
     2                      CDDNFM,IOFLG
C
      CHARACTER*16 TEXT
      DOUBLE PRECISION SSTRT
      REAL,          SAVE,    DIMENSION(:),    ALLOCATABLE ::BUFF
C
      DATA TEXT /'       CLN HEADS'/
C     ------------------------------------------------------------------
      ALLOCATE(BUFF(NCLNNDS))
C
C1------FOR EACH CLN NODE PUT HEADS IN BUFF IF PRINT OR SAVE IS REQUESTED.
      DO 59 N=1,NCLNNDS
C
C2------Save heads in buffer array BUFF
        NG = N+NODES
        BUFF(N)=HNEW(NG)
   59 CONTINUE
C
C3------CALL ULAPRS OR ULAPRW TO PRINT heads.
      IF(ISA.NE.0) THEN
        IF(IOFLG(1,1).NE.0) THEN
          IF(IHEDFM.LT.0) CALL ULAPRS(BUFF(1),TEXT,KSTP,KPER,
     1                  NCLNNDS,1,1,-IHEDFM,IOUT)
          IF(IHEDFM.GE.0) CALL ULAPRW(BUFF(1),TEXT,KSTP,KPER,
     1                  NCLNNDS,1,1,IHEDFM,IOUT)
          IPFLG=1
        ENDIF
C
      END IF
C
C4------DETERMINE IF HEAD SHOULD BE SAVED.
C4------IF SO THEN CALL A ULASAV OR ULASV2 TO RECORD HEAD.
      IFIRST=1
      IF(ICLNHD.LE.0) GO TO 80
        NSTRT = NODES+1
        IF(IOFLG(1,3).EQ.0) GO TO 80
        IF(IFIRST.EQ.1) WRITE(IOUT,74) ICLNHD,KSTP,KPER
   74   FORMAT(1X,/1X,'CLN HEAD WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I3,', STRESS PERIOD ',I4)
        IFIRST=0
        IF(CDDNFM.EQ.' ') THEN
           CALL ULASAV(BUFF(1),TEXT,KSTP,KPER,PERTIM,TOTIM,NCLNNDS,
     1                1,1,ICLNHD)
        ELSE
           CALL ULASV2(BUFF(1),TEXT,KSTP,KPER,PERTIM,TOTIM,NCLNNDS,
     1                1,1,ICLNHD,CDDNFM,LBDDSV,IBOUND(NSTRT))
        END IF
C
80    CONTINUE
      DEALLOCATE(BUFF)

C
C5------RETURN.
      RETURN
      END
C----------------------------------------------------------------------------
      SUBROUTINE SCLN1IB(KSTP,KPER)
C     ******************************************************************
C     RECORD IBOUND OF CLN NODES
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY: IBOUND,IOUT
      USE CLN1MODULE, ONLY:  NCLNNDS,ICLNIB
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,IBOUUN,LBBOSV,CBOUFM,IOFLG
C
      INTEGER,  SAVE,    DIMENSION(:),    ALLOCATABLE ::ITEMP
      CHARACTER*16 TEXT
      DATA TEXT /'          IBOUND'/
C     ------------------------------------------------------------------
      IF(ICLNIB.LE.0) RETURN
      ALLOCATE (ITEMP(NCLNNDS))
C
C1------FOR EACH CLN NODE PUT IBOUND IN ITMP IF SAVE IS REQUESTED.
      DO 59 N=1,NCLNNDS
        NG = N+NODES
        ITEMP(N)=IBOUND(NG)
   59 CONTINUE
C
C2------SAVE IBOUND WHEN REQUESTED.
      IF(IOFLG(1,7).EQ.0) GO TO 79
      WRITE(IOUT,74) ICLNIB,KSTP,KPER
   74 FORMAT(1X,/1X,'CLN IBOUND WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I3,', STRESS PERIOD ',I4)
C
      CALL ULASV3(ITEMP(1),TEXT,KSTP,KPER,PERTIM,TOTIM,NCLNNDS,
     1                1,1,ICLNIB,CBOUFM,LBBOSV)
   79   CONTINUE
C
C3------RETURN.
      DEALLOCATE(ITEMP)
      RETURN
      END
C
C -----------------------------------------------------------------------
      SUBROUTINE CLN1DA
C  DEALLOCATE CLN DATA
      USE CLN1MODULE
C
        DEALLOCATE(NCLN,ICLNCB,ICLNHD,ICLNDD,ICLNIB,NCLNNDS)
        DEALLOCATE(NNDCLN,CLNCON,ACLNNDS)
        DEALLOCATE(IFLINCLN,ICCWADICLN,ICGWADICLN,ACLNGWC,ACLNCOND)
C
      RETURN
      END
