      MODULE GWTDPTMODULE
        INTEGER,SAVE,POINTER::IDPTCB,IDPTCON,IC_IBNDIN_FLG,IADSORBIM,
     *  IDISPIM,IZODIM,IFODIM
        INTEGER, SAVE, ALLOCATABLE, DIMENSION (:) :: ICBUNDIM
        REAL, SAVE, ALLOCATABLE, DIMENSION(:) ::DDTTR
C
        DOUBLE PRECISION,SAVE,ALLOCATABLE,DIMENSION(:,:) :: CONCIM
        DOUBLE PRECISION,SAVE,ALLOCATABLE,DIMENSION(:,:) :: CONCOIM
        REAL, SAVE,ALLOCATABLE,DIMENSION(:) :: PRSITYIM,BULKDIM,DLIM
       REAL, SAVE, ALLOCATABLE,   DIMENSION(:,:) ::ADSORBIM,FLICHIM,
     1  ZODRWIM,FODRWIM,ZODRSIM,FODRSIM
        DOUBLE PRECISION, SAVE,ALLOCATABLE,DIMENSION(:) :: MASSBCTIM
        DOUBLE PRECISION, SAVE,ALLOCATABLE,DIMENSION(:) :: DIADDT,RDDT
        DOUBLE PRECISION, SAVE,ALLOCATABLE,DIMENSION(:) :: OFFDDTM
        DOUBLE PRECISION, SAVE,ALLOCATABLE,DIMENSION(:) :: OFFDDTIM
      END MODULE GWTDPTMODULE
C
C-----------------------------------------------------------------------
      SUBROUTINE GWT2DPTU1AR(IN)
C     ******************************************************************
C     INITIALIZE VARIABLES AND READ DATA FOR DUAL POROSITY TRANSPORT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:IFREFM,IOUT,NODES,NLAY,IUNSAT,NODLAY,IDPT,IDPF,
     1    IBOUND,TOP,BOT,AREA,ISSFLG,PGF,IA,JAS,JA,Sn
      USE GWTDPTMODULE
      USE GWTBCTMODULE, ONLY: MCOMP,ICBUND,ZODRW,ZODRS,FODRW,FODRS,
     1  PRSITY,ADSORB,IZOD,IFOD,IADSORB,CINACT
      USE GWFBCFMODULE, ONLY: SC1,SC2,HK
      USE GWFDPFMODULE, ONLY: PHIF,SnIM,SoIM,SC1IM,SC2IM,HNEWIM
      CHARACTER*200 LINE
      DOUBLE PRECISION MASLOCW,MASLOCS,MASLOCC,REFHD
      INTEGER IFRAHK,IMSAT
C
      REAL, DIMENSION(:),ALLOCATABLE  ::TEMPC
      CHARACTER*24 ANAME(14)
      DATA ANAME(1) /'      DPT BOUNDARY ARRAY'/
      DATA ANAME(2) /'       FRACTURE POROSITY'/
      DATA ANAME(3) /'IMMOBILE DOMAIN POROSITY'/
      DATA ANAME(4) /'         DD BULK DENSITY'/
      DATA ANAME(5) /'    DD DISPERSION COEFF.'/
      DATA ANAME(6) /'   DD MASS TRANSFER RATE'/
      DATA ANAME(7) /'      DD ADSORPTION COEF'/
      DATA ANAME(8) /'  DD FREUNDLICH EXPONENT'/
      DATA ANAME(9) /'DD ZERO-ORDER DECAY WAT.'/
      DATA ANAME(10) /'DD ZERO-ORDER DECAY SOIL'/
      DATA ANAME(11) /'DD 1-ST ORDER DECAY WAT.'/
      DATA ANAME(12) /'DD 1-ST ORDER DECAY SOIL'/
      DATA ANAME(13) /'        DPT INITIAL CONC'/
      DATA ANAME(14) /'     IMMOBILE DOMAIN SAT'/
C     ------------------------------------------------------------------
C
C1------IDENTIFY PACKAGE
      IDPT = 1 ! FLAG FOR DUAL POROSITY TRANSPORT IS ON
      WRITE(IOUT,1) IN
    1 FORMAT(1X,/1X,'DPT -- DUAL POROSITY TRANSPORT PACKAGE, VERSION 1',
     1',12/26/2012',/,9X,'INPUT READ FROM UNIT',I3)
C
C-----------------------------------------------------------------------
C2------ALLOCATE VARIABLES
      ALLOCATE(IDPTCB,IDPTCON,IC_IBNDIN_FLG,IADSORBIM,
     1  IDISPIM,IZODIM,IFODIM)
C
C-----------------------------------------------------------------------
C3------READ GENERAL DPT INFORMATION AND FLAGS
      IF(IFREFM.EQ.0) THEN
        READ(IN,2)IDPTCB,IDPTCON,IC_BNDIM_FLG,IADSORBIM,IDISPIM,IZODIM,
     1    IFODIM
        LLOC=71
      ELSE
        CALL URDCOM(IN,IOUT,LINE)
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDPTCB,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDPTCON,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IC_BNDIM_FLG,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IADSORBIM,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IDISPIM,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IZODIM,R,IOUT,IN)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IFODIM,R,IOUT,IN)
      ENDIF
2     FORMAT(7I10)
C      
C3B-----PRINT VALUES
      WRITE(IOUT,11) IDPTCB,IDPTCON,IC_BNDIM_FLG,IADSORBIM,
     *  IDISPIM,IZODIM,IFODIM
   11 FORMAT(1X,'C-B-C FLUX FLAG OR UNIT NUMBER (IDPTCB)',27X,' =',I3
     *  /1X,'IMMOBILE DOMAIN CONC SAVE FLAG OR UNIT NUMBER (IDPTCON)',
     *  11X,' =',I3,
     *  /1X,'ACTIVE IMMOBILE DOMAIN SAME AS FRACTURE DOMAIN FLAG',1X,
     *  '(IC_BNDIM_FLG) =',I3
     *  /1X,'IMMOBILE DOMAIN ADSORPTION FLAG (IADSORBIM)',23X,' =',I3
     *  /1X,'IMMOBILE DOMAIN DISPERSION FLAG (IDISPIM)',25X,' =',I3,
     */1X,'IMMOBILE DOMAIN ZERO-ORDER DECAY FLAG (IZODIM)',20X,' =',I3,
     */1X,'IMMOBILE DOMAIN FIRST-ORDER DECAY FLAG (IFODIM)',19X,' =',I3)
C
      IF(IDPTCB.GT.0) WRITE(IOUT,9) IDPTCB
    9 FORMAT(1X,'DUAL POROSITY CELL-BY-CELL MASS FLUX WILL BE SAVED ON',
     1 1X,'UNIT IDPTCB  =',I3)
      IF(IDPTCON.GT.0) WRITE(IOUT,10) IDPTCON
10    FORMAT(1X,'IMMOBILE DOMAIN CONC WILL BE SAVED ON UNIT IDPTCON',
     1  14X,'=',I3)
C-------------------------------------------------------------------------------
C3C--------GET OPTIONS FOR WHEN FLOW IS NOT DUAL POROSITY
      IF(IDPF.EQ.0)THEN
        IFRAHK = 0
        IMSAT = 1
100     CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
        IF(LINE(ISTART:ISTOP).EQ.'FRAHK') THEN
C3D-------SET FLAG FOR HK TO BE FRACTURE-DOMAIN VALUE.   
          IFRAHK = 1
        ENDIF 
C3E-------SET FLAG FOR IMMOBILE DOMAIN SATURATION TREATMENT.          
        IF(LINE(ISTART:ISTOP).EQ.'MOBILESAT') THEN
C3F-------SET FLAG.   
          IMSAT = 2
        ELSEIF(LINE(ISTART:ISTOP).EQ.'INPUTSAT') THEN
C3G-------SET FLAG.   
          IMSAT = 3
        ENDIF      
        IF(LLOC.LT.200) GO TO 100 
C3H----------PRINT OPTION FLAG VALUES
        IF(IFRAHK.EQ.1) WRITE(IOUT,12) IFRAHK
12      FORMAT(1X,'CONDUCTANCE ARE FOR FRACTURE VOLUME; (IFRAHK)',12X,
     1   '=',I4)
        IF(IFRAHK.EQ.0) WRITE(IOUT,13) IFRAHK
13      FORMAT(1X,'CONDUCTANCE ARE FOR TOTAL (FRACTURE + MATRIX)', 
     1            1X,'DOMAIN; (IFRAHK)  =',I4)
        IF(IMSAT.EQ.1) WRITE(IOUT,14) IMSAT
14      FORMAT(1X,'IMMOBILE DOMAIN SATURATIONS SET TO UNITY)', 
     1            12X, '(IMSAT)  =',I4)
        IF(IMSAT.EQ.2) WRITE(IOUT,15) IMSAT
15      FORMAT(1X,'IMMOBILE DOMAIN SATURATIONS SET SAME AS FOR MOBILE', 
     1            1X, 'DOMAIN (IMSAT)  =',I4)        
        IF(IMSAT.EQ.3) WRITE(IOUT,16) IMSAT
16      FORMAT(1X,'IMMOBILE DOMAIN SATURATIONS ARE INPUT)', 
     1            12X, '(IMSAT)  =',I4)        
      ENDIF
C-----------------------------------------------------------------------
C4-----ALLOCATE ARRAYS AND INITIALIZE
      ALLOCATE(DDTTR(NODES),CONCIM(NODES,MCOMP),CONCOIM(NODES,MCOMP),
     1  ICBUNDIM(NODES),PRSITYIM(NODES),BULKDIM(NODES),MASSBCTIM(MCOMP))
      ALLOCATE(DIADDT(NODES),RDDT(NODES),OFFDDTM(NODES),OFFDDTIM(NODES))
      IF(IADSORBIM.NE.0) ALLOCATE (ADSORBIM(NODES,MCOMP))
      IF(IADSORBIM.EQ.2) ALLOCATE (FLICHIM(NODES,MCOMP))
      IF(IDISPIM.GT.0)THEN
        ALLOCATE(DLIM(NODES))
      ENDIF
      IF(IZODIM.EQ.1)THEN
        ALLOCATE(ZODRWIM(NODES,MCOMP))
        ZODRW = 0.
      ELSEIF(IADSORBIM.GT.0)THEN
        IF(IZODIM.EQ.2)THEN
           ALLOCATE(ZODRSIM(NODES,MCOMP))
           ZODRSIM = 0.
        ELSEIF(IZODIM.EQ.3)THEN
          ALLOCATE(ZODRWIM(NODES,MCOMP))
          ZODRWIM = 0.
          ALLOCATE(ZODRSIM(NODES,MCOMP))
          ZODRSIM = 0.
        ENDIF
      ENDIF
      IF(IFODIM.EQ.1)THEN
        ALLOCATE(FODRWIM(NODES,MCOMP))
        FODRWIM = 0.
      ELSEIF(IADSORBIM.GT.0)THEN
        IF(IFODIM.EQ.2)THEN
           ALLOCATE(FODRSIM(NODES,MCOMP))
           FODRSIM = 0.
        ELSEIF(IFODIM.EQ.3)THEN
          ALLOCATE(FODRWIM(NODES,MCOMP))
          FODRWIM = 0.
          ALLOCATE(FODRSIM(NODES,MCOMP))
          FODRSIM = 0.
        ENDIF
      ENDIF
C------------------------------------------------------------------
C5------FILL BOUNDARY ARRAY(ICBUNDIM).
      IF(IC_BNDIM_FLG.EQ.0)THEN
C5A-----READ ICBUNDIM ARRAY IF FLAG IS SET
        DO K = 1,NLAY
          KK = K
          NNDLAY = NODLAY(K)
          NSTRT = NODLAY(K-1)+1
          NDSLAY = NNDLAY - NODLAY(K-1)
          CALL U1DINT(ICBUNDIM(NSTRT),ANAME(1),NDSLAY,K,IN,IOUT)
        ENDDO
C
        DO N=1,NODES
          IF(ICBUND(N).EQ.0) ICBUNDIM(N) = 0
        ENDDO
      ELSE
C5B-------SET ICBUNDIM ARRAY TO ICBUND IF FLAG IS NOT SET
        DO N=1,NODES
          ICBUNDIM(N) = ICBUND(N)
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C6------READ MATERIAL PROPERTIES FOR IMMOBILE DOMAIN.
C-----------------------------------------------------------------------
C6A-----READ FRACTURE DOMAIN FRACTION INTO ARRAY PHIF IF FLOW WAS SINGLE DOMAIN
      IF(IDPF.EQ.0)THEN
        ALLOCATE(PHIF(NODES))
        DO K = 1,NLAY
          NNDLAY = NODLAY(K)
          NSTRT = NODLAY(K-1)+1
          NDSLAY = NNDLAY - NODLAY(K-1)
          CALL U1DREL(PHIF(NSTRT),ANAME(2),NDSLAY,K,IN,IOUT)
        ENDDO
      ENDIF
c
C6B-----READ IMMOBILE DOMAIN POROSITY INTO ARRAY PRSITYIM
      DO K = 1,NLAY
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
        NDSLAY = NNDLAY - NODLAY(K-1)
        CALL U1DREL(PRSITYIM(NSTRT),ANAME(3),NDSLAY,K,IN,IOUT)
      ENDDO
C
C6C-----READ BULK DENSITY INTO ARRAY TEMP IF ADSORPTION.
      IF(IADSORBIM.NE.0)THEN
        DO K = 1,NLAY
          NNDLAY = NODLAY(K)
          NSTRT = NODLAY(K-1)+1
          NDSLAY = NNDLAY - NODLAY(K-1)
          CALL U1DREL(BULKDIM(NSTRT),ANAME(4),NDSLAY,K,IN,IOUT)
        ENDDO
      ENDIF
C
C6D-----READ DISPERSION COEFFICIENT IF DP FLOW WAS ON
      IF(IDPF.NE.0)THEN
        DO K = 1,NLAY
          NNDLAY = NODLAY(K)
          NSTRT = NODLAY(K-1)+1
          NDSLAY = NNDLAY - NODLAY(K-1)
          CALL U1DREL(DLIM(NSTRT),ANAME(5),NDSLAY,K,IN,IOUT)
        ENDDO
      ENDIF
C6E-----READ IMMOBILE DOMAIN MASS TRANSFER RATE INTO ARRAY DDTTR
      DO K = 1,NLAY
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
        NDSLAY = NNDLAY - NODLAY(K-1)
        CALL U1DREL(DDTTR(NSTRT),ANAME(6),NDSLAY,K,IN,IOUT)
      ENDDO
C6F-----SET OR READ IMMOBILE DOMAIN SATURATIONS IF DUAL DOMAIN FLOW IS NOT SIMULATED      
      IF(IDPF.EQ.0)THEN
C6F1-----ALLOCATE ARRAY
        ALLOCATE(SnIM(NODES))
        SoIM => SnIM
        IF(IMSAT.EQ.1)THEN
C6F2---------IMMOBILE DOMAIN SATURATIONS SET TO UNITY            
          DO N=1,NODES
            SNIM(N) = 1.0
          ENDDO
        ELSEIF(IMSAT.EQ.2)THEN
C6F3---------IMMOBILE DOMAIN SATURATIONS SET SAME AS FOR MOBILE DOMAIN            
          DO N=1,NODES
            SNIM(N) = Sn(N)
          ENDDO  
        ELSEIF(IMSAT.EQ.3)THEN
C6F4---------IMMOBILE DOMAIN SATURATIONS ARE INPUT
          DO K=1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL(TEMPC(NSTRT),ANAME(14),NDSLAY,K,IN,IOUT)
          ENDDO
          DO N = 1, NODES
            SNIM(N) = TEMPC(N)
          ENDDO
        ENDIF
      ENDIF   
C
C-----------------------------------------------------------------------
C7------READ SPECIES DEPENDENT ARRAYS FOR ALL LAYERS.
C-----------------------------------------------------------------------
      ALLOCATE(TEMPC(Nodes))
      DO ICOMP=1,MCOMP
        WRITE(IOUT,20) ICOMP
20      FORMAT(80('-')/1X,'THE FOLLOWING ARRAYS ARE READ FOR SPECIES ',
     *    1X,'NUMBER ',I3/80('-'))
C
C---------------------------------------------------------
C8------READ ADSORPTION COEFFICIENT FOR EACH SPECIES
      IF(IADSORBIM.NE.0)THEN
        DO K=1,NLAY
          NNDLAY = NODLAY(K)
          NSTRT = NODLAY(K-1)+1
          NDSLAY = NNDLAY - NODLAY(K-1)
          CALL U1DREL(TEMPC(NSTRT),ANAME(7),NDSLAY,K,IN,IOUT)
        ENDDO
C8A-------PREPARE CONSTANT PART OF ADSORPTION TERM
        DO N = 1, NODES
          ADSORBIM(N,ICOMP) = TEMPC(N)
          ADSORBIM(N,ICOMP) = ADSORBIM(N,ICOMP) * BULKDIM(N)
        ENDDO
C---------------------------------------------------------
C9-----READ ADSORPTION EXPONENT FOR EACH SPECIES
        IF(IADSORBIM.EQ.2)THEN
          DO K=1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL(TEMPC(NSTRT),ANAME(8),NDSLAY,K,IN,IOUT)
          ENDDO
          DO N = 1, NODES
            FLICHIM(N,ICOMP) = TEMPC(N)
          ENDDO
        ENDIF
      ENDIF
C---------------------------------------------------------
C10-----READ ZERO ORDER DECAY COEFFICIENTS
      IF(IZODIM.EQ.1)THEN
C10A---ZERO ORDER DECAY IN WATER IF FLAG IZODIM=1
        DO K=1,NLAY
          NNDLAY = NODLAY(K)
          NSTRT = NODLAY(K-1)+1
          NDSLAY = NNDLAY - NODLAY(K-1)
          CALL U1DREL(TEMPC(NSTRT),ANAME(9),NDSLAY,K,IN,IOUT)
        ENDDO
        DO N = 1, NODES
          ZODRWIM(N,ICOMP) = TEMPC(N)
        ENDDO
      ELSEIF(IADSORBIM.GT.0)THEN
        IF(IZODIM.EQ.2)THEN
C10B------ZERO ORDER DECAY ON SOIL IF FLAG IZODIM=2
          DO K=1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL(TEMPC(NSTRT),ANAME(10),NDSLAY,K,IN,IOUT)
          ENDDO
          DO N = 1, NODES
            ZODRSIM(N,ICOMP) = TEMPC(N)
          ENDDO
        ELSEIF(IZODIM.EQ.3)THEN
C10C-------ZERO ORDER DECAY IN WATER AND ON SOIL IF FLAG IZODIM=3
          DO K=1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL(TEMPC(NSTRT),ANAME(9),NDSLAY,K,IN,IOUT)
          ENDDO
          DO N = 1, NODES
            ZODRWIM(N,ICOMP) = TEMPC(N)
          ENDDO
          DO K=1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL(TEMPC(NSTRT),ANAME(10),NDSLAY,K,IN,IOUT)
          ENDDO
          DO N = 1, NODES
            ZODRSIM(N,ICOMP) = TEMPC(N)
          ENDDO
        ENDIF
      ENDIF
C---------------------------------------------------------
C11-----READ FIRST ORDER DECAY COEFFICIENTS
      IF(IFODIM.EQ.1)THEN
C11A----FIRST ORDER DECAY IN WATER IF FLAG IFODIM=1
        DO K=1,NLAY
          NNDLAY = NODLAY(K)
          NSTRT = NODLAY(K-1)+1
          NDSLAY = NNDLAY - NODLAY(K-1)
          CALL U1DREL(TEMPC(NSTRT),ANAME(11),NDSLAY,K,IN,IOUT)
        ENDDO
        DO N = 1, NODES
          FODRWIM(N,ICOMP) = TEMPC(N)
        ENDDO
      ELSEIF(IADSORBIM.GT.0)THEN
        IF(IFODIM.EQ.2)THEN
C11B------FIRST ORDER DECAY ON SOIL IF FLAG IFODIM=2
          DO K=1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL(TEMPC(NSTRT),ANAME(12),NDSLAY,K,IN,IOUT)
          ENDDO
          DO N = 1, NODES
            FODRSIM(N,ICOMP) = TEMPC(N)
          ENDDO
        ELSEIF(IFODIM.EQ.3)THEN
C11C-----FIRST ORDER DECAY IN WATER AND ON SOIL IF FLAG IFODIM=3
          DO K=1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL(TEMPC(NSTRT),ANAME(11),NDSLAY,K,IN,IOUT)
          ENDDO
          DO N = 1, NODES
            FODRWIM(N,ICOMP) = TEMPC(N)
          ENDDO
          DO K=1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            NDSLAY = NNDLAY - NODLAY(K-1)
            CALL U1DREL(TEMPC(NSTRT),ANAME(12),NDSLAY,K,IN,IOUT)
          ENDDO
          DO N = 1, NODES
            FODRSIM(N,ICOMP) = TEMPC(N)
          ENDDO
        ENDIF
      ENDIF
C
C---------------------------------------------------------
C12------CONCENTRATION OF EACH SPECIES
      DO K=1,NLAY
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
        NDSLAY = NNDLAY - NODLAY(K-1)
        CALL U1DREL(TEMPC(NSTRT),ANAME(13),NDSLAY,K,IN,IOUT)
      ENDDO
      DO N = 1,NODES
        CONCIM(N,ICOMP) = TEMPC(N)
        IF(ICBUNDIM(N).EQ.0) CONCIM(N,ICOMP)=CINACT
        CONCOIM(N,ICOMP) = CONCIM(N,ICOMP)
      ENDDO
C
C---------------------------------------------------------
      ENDDO  ! END SPECIES DO LOOP ICOMP
C---------------------------------------------------------
C13   DEALLOCATE UNWANTED ARRAYS
      IF(IADSORBIM.NE.0) DEALLOCATE(BULKDIM)
      DEALLOCATE (TEMPC)
C-----------------------------------------------------------------------
C14-----PREPARE ARRAYS OF PARAMETERS
C-----------------------------------------------------------------------
C15-----MULTIPLY DUAL DOMAIN RATE TERM BY VOLUME OF GRID BLOCK TO GIVE TOTAL TRANSFER FOR CELL
      CALL SGWF2LPFU1SC(DDTTR(1),1)
C
C-----------------------------------------------------------------------
C16-----IF FLOW IS DUAL POROSITY THEN SATURATIONS AND STORAGE TERMS ARE ADJUSTED IN DPT ELSE ADJUST HERE
      IF(IDPF.EQ.0)THEN
C16B-----SCALE STORAGE COEFFICIENTS BY MOBILE FRACTION 
        DO N=1,NODES
          SC1(N) = SC1(N) * PHIF(N)
        ENDDO
        IF(NCNVRT.GT.0)THEN
          DO N=1,NODES
            SC2(N) = SC2(N) * PHIF(N)
            SC2IM(N) = SC2IM(N) * (1.0 - PHIF(N))
          ENDDO
        ENDIF
C16C----SCALE CONDUCTIVITY IF PROVIDED FOR FRACTURES ONLY        
        IF(IFRAHK.EQ.1)THEN
          DO K=1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
C1B---------loop over all nodes within each layer
            DO N=NSTRT,NNDLAY
C2------------loop over all connections of node N and fill upper triangle with PGF term
              DO II = IA(N)+1,IA(N+1)-1
                JJ = JA(II)
C3--------------only for upper triangle of porous medium nodes
                IF(JJ.LE.N.OR.JJ.GT.NODES) CYCLE
                IIS = JAS(II)
                PGF(IIS) = PGF(IIS) * 0.5 * (PHIF(N) + PHIF(JJ))
              ENDDO
            ENDDO
          ENDDO
          DO N = 1,NODES
            HK(N) = HK(N) * PHIF(N)
          ENDDO
        ENDIF        
      ENDIF
C-----------------------------------------------------------------------      
C17----FOR ZERO ORDER DECAY IN WATER
      IF(IZOD.EQ.1.OR.IZOD.EQ.3)THEN
        DO ICOMP = 1,MCOMP
        DO N = 1,NODES
          ZODRW(N,ICOMP) = ZODRW(N,ICOMP) * PHIF(N)
          ZODRWIM(N,ICOMP)=ZODRWIM(N,ICOMP)*PRSITYIM(N)*(1.0 - PHIF(N))
        ENDDO
        ENDDO
      ENDIF
C18----FOR ZERO ORDER DECAY IN SOIL
      IF((IZOD.EQ.2.OR.IZOD.EQ.3).AND.IADSORB.NE.0)THEN
        DO ICOMP = 1,MCOMP
        DO N = 1,NODES
          ZODRS(N,ICOMP) = ZODRS(N,ICOMP) * PHIF(N)
          ZODRSIM(N,ICOMP) = ZODRSIM(N,ICOMP) * (1.0 - PRSITYIM(N))
     1          *(1.0 - PHIF(N))
        ENDDO
        ENDDO
      ENDIF
C19----FOR FIRST ORDER DECAY IN WATER
      IF(IFOD.EQ.1.OR.IFOD.EQ.3)THEN
        DO ICOMP = 1,MCOMP
        DO N = 1,NODES
          FODRW(N,ICOMP) = FODRW(N,ICOMP) * PHIF(N)
          FODRWIM(N,ICOMP)=FODRWIM(N,ICOMP)*PRSITYIM(N)*(1.0 - PHIF(N))
        ENDDO
        ENDDO
      ENDIF
C20----FOR FIRST ORDER DECAY ON SOIL DOES NOT NEED PHIF SINCE ADSORB TERM HAS IT 
C      IF((IFOD.EQ.2.OR.IFOD.EQ.3).AND.IADSORB.NE.0)THEN
C        DO ICOMP = 1,MCOMP
C        DO N = 1,NODES
C          FODRS(N,ICOMP) = FODRS(N,ICOMP) * PHIF(N)
C          FODRSIM(N,ICOMP) = FODRSIM(N,ICOMP) * (1.0 - PHIF(N))
C        ENDDO
C        ENDDO
C      ENDIF
C21----FOR POROSITY FRACTIONED INTO MOBILE AND IMMOBILE DOMAINS
      DO N = 1,NODES
        PRSITY(N) = PRSITY(N) * PHIF(N)
        PRSITYIM(N) = PRSITYIM(N) * (1.0 - PHIF(N))
      ENDDO
C22----FOR KD VALUES ON SOIL
      IF(IADSORB.NE.0)THEN
        DO ICOMP = 1,MCOMP
        DO N = 1,NODES
          ADSORB(N,ICOMP) = ADSORB(N,ICOMP) * PHIF(N)
          ADSORBIM(N,ICOMP) = ADSORBIM(N,ICOMP) * (1.0 - PHIF(N))
        ENDDO
        ENDDO
      ENDIF
C  ---------------------------------------------------------------------
C10------RETURN
      RETURN
      END
C
C-----------------------------------------------------------------------
      SUBROUTINE INITMASSIM
C     ******************************************************************
C      COMPUTE THE INITIAL MASS OF IMMOBILE COMPONENTS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:IOUT,NODES,IDPT,IDPF,TOP,BOT,AREA,ISSFLG
      USE GWTDPTMODULE
      USE GWTBCTMODULE, ONLY: MCOMP,ICBUND,CINACT
      USE GWFDPFMODULE, ONLY: PHIF,SoIM,SC1IM,SC2IM,HOLDIM
      DOUBLE PRECISION MASLOCW,MASLOCS,MASLOCC,REFHD
C     --------------------------------------------------------------------
C1-----COMPUTE INITIAL MASS FOR MOBILE COMPONENTS OF IMMOBILE DOMAIN
      WRITE(IOUT,12)
12    FORMAT(/5X,'INITIAL MASS OF COMPONENTS IN IMMOBILE DOMAIN'/
     *       5X,'----------------------------------------------'/
     *       5X,'COMPONENT INDEX',5X,'INITIAL MASS'/
     *       5X,'---------------',5X,'------------')
      DO ICOMP = 1,MCOMP
        MASSBCTIM (ICOMP) = 0.0
      ENDDO
      MASLOCW = 0.0
      MASLOCS = 0.0
      MASLOCC = 0.0
      ISS=ISSFLG(1)
C2-----COMPUTE MASS FOR EACH ACTIVE NODE
      DO N = 1,NODES
        IF(ICBUNDIM(N).EQ.0) CYCLE
C3-------COMPUTE VOLUME OF NODE
        ALENG = TOP(N) - BOT(N)
        VOLU = AREA(N) * ALENG
C4-------FOR EACH COMPONENT
        DO ICOMP = 1,MCOMP
C5-------COMPUTE MASS IN WATER
          IF(ISS.EQ.1)THEN
            MASLOCW = PRSITYIM(N) * SoIM(N) * CONCIM(N,ICOMP) * VOLU
          ELSE
C6---------INITIAL MASS IN PORE STORAGE
            MASLOCW=(PRSITYIM(N)*VOLU-SC2IM(N)*ALENG+
     *        SC2IM(N)*ALENG*SoIM(N))*CONCIM(N,ICOMP)
C7-----------INITIAL MASS IN COMPRESSIBLE STORAGE ASSUMED FROM TOP OF CELL
            REFHD = HOLDIM(N) - TOP(N)
            IF(REFHD.LT.0.0) REFHD = 0.0
            MASLOCC = SC1IM(N) * REFHD * CONCIM(N,ICOMP)
          ENDIF
C8---------COMPUTE MASS ON SOIL
          IF(IADSORBIM.NE.0)THEN
            ETA = 1.0
            IF(IADSORBIM.EQ.2) ETA = FLICHIM(N,ICOMP)
            MASLOCC = ADSORBIM(N,ICOMP) * CONCIM(N,ICOMP)**ETA * VOLU
          ENDIF
          MASSBCTIM(ICOMP) = MASSBCTIM(ICOMP)+MASLOCW+MASLOCS+MASLOCC
        ENDDO
      ENDDO
C9-----WRITE MASS TO OUTPUT FILE
      DO ICOMP = 1,MCOMP
        WRITE(IOUT,13) ICOMP, MASSBCTIM(ICOMP)
      ENDDO
13    FORMAT(I15,5X,E15.6)
C  ---------------------------------------------------------------------
C10------RETURN
      RETURN
      END
C
C-----------------------------------------------------------------------
      SUBROUTINE GWT2DPTU1FM(KPER,ICOMP,ISS)
C     ******************************************************************
C     FORMULATE DUAL POROSITY TERMS FOR TRANSPORT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,  ONLY:NODES,NLAY,IUNSAT,NODLAY,AMAT,IA,HNEW,AKR,
     1  TOP,BOT,IBOUND,ISSFLG,Sn,IDPF
      USE GWTDPTMODULE
      USE GWFDPFMODULE, ONLY: HNEWIM,SnIM,CBCFIM
      USE GWTBCTMODULE, ONLY: ICBUND
      USE GWFBASMODULE, ONLY: DELT
      USE GWFBCFMODULE, ONLY: ISFAC,SC1,SC2,LAYCON
      USE SMSMODULE, ONLY: NONMETH,DKDH
      DOUBLE PRECISION DDFLOW,RHO,TLED,TTOP,BBOT, TOTTHICK,EPS,HD,
     *  DS,RHO2,RHO1,THCK,SATN,SATO,FTERM,DFTERM,SW,EKR,AKRCIMN,
     *  DIAGS,RTERMS,DIAGD,RTERMD,QIJ,UPSAT
C
C     ------------------------------------------------------------------
C1--------INITALIZE ARRAYS
      DO N=1,NODES
        DIADDT(N) = 0.0
        RDDT(N) = 0.0
        OFFDDTM(N) = 0.0
        OFFDDTIM(N) = 0.0
      ENDDO
C-----------------------------------------------------------------------------
C2------FILL STORAGE TERM ON DIAGONAL AND RHS OF IMMOBILE DOMAIN EQUATION
C-----------------------------------------------------------------------------
      TLED=1.0D0/DELT
      DO 200 K=1,NLAY
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
C-----------------------------------------------------------------------------
        DO 140 N=NSTRT,NNDLAY
          IF(ICBUNDIM(N).EQ.0) CYCLE
          CALL GWT2DPT1STOIM(N,ICOMP,DIAGS,RTERMS,ISS)
          CALL GWT2DPT1DCYIM(N,ICOMP,DIAGD,RTERMD)
          DIADDT(N)=DIADDT(N) + DIAGS + DIAGD
          RDDT(N)=RDDT(N) + RTERMS + RTERMD
  140   CONTINUE
200   CONTINUE
C
C-----------------------------------------------------------------------------
C5------FILL ADVECTIVE/DISPERSIVE MASS TRANSFER TERM ON OFF-DIAGONALS AND DIAGONALS
C-----------------------------------------------------------------------------
      IF(IDPF.NE.0) THEN  !DUAL POROSITY FLOW
        DO 220 N = 1, NODES
          IF(ICBUNDIM(N).EQ.0) CYCLE
C---------FILL UPSTREAM TERM
          QIJ = CBCFIM(N)
          IF(QIJ.LE.0)THEN ! MOBILE DOMAIN IS UPSTREAM
            AMAT(IA(N)) = AMAT(IA(N)) + QIJ
            OFFDDTIM(N) = OFFDDTIM(N) - QIJ
          ELSE ! IMMOBILE DOMAIN IS UPSTREAM
            DIADDT(N) = DIADDT(N) + QIJ
            OFFDDTM(N) = OFFDDTM(N) - QIJ
          ENDIF
220     CONTINUE
      ENDIF
C-----------------------------------------------------------------------------
C5------FILL DIFFUSIVE MASS TRANSFER TERM ON OFF-DIAGONALS AND DIAGONALS
C-----------------------------------------------------------------------------
      DO 300 K=1,NLAY
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
        IF(LAYCON(K).EQ.0)THEN
C-----------------------------------------------------------------------------
C6-------NON-CONVERTIBLE LAYER, SO COPY CONSTANT TERM TO OFF-DIAGONALS AND ADD TO DIAGONALS
          DO 240 N=NSTRT,NNDLAY
            IF(ICBUNDIM(N).NE.0.AND.ICBUND(N).NE.0) THEN
              DDFLOW = DDTTR(N)
              OFFDDTM(N) = OFFDDTM(N) + DDFLOW
              OFFDDTIM(N) = OFFDDTIM(N) + DDFLOW
              AMAT(IA(N)) =  AMAT(IA(N)) - DDFLOW
              DIADDT(N) = DIADDT(N) - DDFLOW
            ENDIF
240       CONTINUE
        ELSE
C-----------------------------------------------------------------------------
C7-------CONVERTIBLE LAYER IS NONLINEAR, COMPUTE NONLINEAR TERMS
          DO 250 N=NSTRT,NNDLAY
            IF(ICBUNDIM(N).EQ.0.OR.ICBUND(N).EQ.0) CYCLE
C
C8------------FIND UPSTREAM SATURATION BETWEEN M AND IM DOMAINS
            IF(HNEW(N).GT.HNEWIM(N))THEN
C8A-------------MOBILE DOMAIN IS UPSTREAM
              UPSAT = Sn(N)
            ELSE
C8B-------------IMMOBILE DOMAIN IS UPSTREAM
              UPSAT = SnIM(N)
            ENDIF
C
C9--------------FILL NONLINEAR TERMS IN OFF-DIAGONALS AND ADD TO DIAGONALS
            DDFLOW = DDTTR(N) * UPSAT
            OFFDDTM(N) = OFFDDTM(N) + DDFLOW
            OFFDDTIM(N) = OFFDDTIM(N) + DDFLOW
            AMAT(IA(N)) =  AMAT(IA(N)) - DDFLOW
            DIADDT(N) = DIADDT(N) - DDFLOW
250       CONTINUE
        ENDIF
300   CONTINUE
C
C11------RETURN
      RETURN
        END
C----------------------------------------------------------------------
      SUBROUTINE GWT2DPT1STOIM(N,ICOMP,DTERMS,RTERMS,ISS)
C     ******************************************************************
C     FORMULATE STORAGE TERM FOR EACH NODE AND COMPONENT IN IMMOBILE DOMAIN
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,TOP,NODLAY,ISYM,
     1 HNEW,AMAT,RHS,HOLD,ISSFLG,IA,JA,NJA,NODES,AREA,BOT,TOP,Sn,So
      USE GWTBCTMODULE, ONLY: ICT
      USE GWFDPFMODULE, ONLY: PHIF,SnIM,SoIM
      USE CLN1MODULE, ONLY: ACLNNDS
      USE GWFBASMODULE,ONLY:DELT
      USE GWTDPTMODULE, ONLY: CONCIM,CONCOIM,IADSORBIM,PRSITYIM,
     * ADSORBIM,FLICHIM,IZODIM,IFODIM,ZODRWIM,FODRWIM,
     1  ZODRSIM,FODRSIM
      DOUBLE PRECISION VODT,ADSTERM,CW,FL,CWO,DT,RT,ALENG,VOLU,
     1  X,Y,EPSS,EPS,CT,CEPS,QA,QEPS,DQ,OMP,DTERMS,RTERMS
C     ------------------------------------------------------------------
      DTERMS = 0.0
      RTERMS = 0.0
      IF(N.LE.NODES)THEN
        ALENG = TOP(N) - BOT(N)
      ELSE
        ALENG = ACLNNDS(N-NODES,5)
      ENDIF
      VOLU = AREA(N) * ALENG
      VODT = VOLU / DELT
C----------------------------------------------------------------------
      IF(ICT.EQ.0)THEN  !----------WATER PHASE CONCENTRATION FORMULATION
C-------ADD STORAGE TERMS ON SOIL
        IF(N.LE.NODES.AND.IADSORBIM.EQ.1)THEN
C---------LINEAR ISOTHERM
          ADSTERM = ADSORBIM(N,ICOMP) * VODT
          DTERMS = DTERMS - ADSTERM
          RTERMS = RTERMS - ADSTERM * CONCOIM(N,ICOMP)
C-----------------------------------------------------------------------
        ELSEIF(N.LE.NODES.AND.IADSORBIM.EQ.2)THEN
C---------NONLINEAR FREUNDLICH ISOTHERM FILLED AS MODIFIED PICARD
          ADSTERM = ADSORBIM(N,ICOMP) * VODT
          FL = FLICHIM(N,ICOMP)
          CW = CONCIM(N,ICOMP)
          CWO = CONCOIM(N,ICOMP)
          DT = 0.0
          RT = 0.0
          IF(CWO.GT.0.0)THEN
            RT = -CWO**FL
          ENDIF
          IF(CW.GT.0.0)THEN
            DT = ADSTERM*FL * CW**(FL - 1.0)
            RT = RT + CW**FL
          ENDIF
          RT = RT * ADSTERM
          DTERMS = DTERMS - DT
          RTERMS = RTERMS - DT * CW + RT
        ENDIF
C-----------------------------------------------------------------------
C-------ADD STORAGE TERM IN WATER
        CALL GWT2BCT1STOIMW (N,ICOMP,DTERMS,RTERMS,VODT,VOLU,ALENG,ISS)
      ELSE       !-----------------------TOTAL CONCENTRATION FORMULATION
C-------NET STORAGE TERM FOR TOTAL CONCENTRATION FORMULATION
        DTERMS = DTERMS - VODT
        RTERMS = RTERMS - VODT * CONCOIM(N,ICOMP)
      ENDIF
C
C9------RETURN
      RETURN
      END
C---------------------------------------------------------------------------
      SUBROUTINE GWT2BCT1STOIMW(N,ICOMP,DTERMS,RTERMS,VODT,VOLU,ALENG,
     *  ISS)
C     ******************************************************************
C     FORMULATE STORAGE TERM FOR EACH NODE AND COMPONENT IN WATER
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,TOP,NODLAY,ISYM,
     1 ISSFLG,IA,JA,NJA,NODES,AREA,BOT,TOP,LAYNOD
      USE CLN1MODULE, ONLY: ACLNNDS
      USE GWFBASMODULE,ONLY:DELT
      USE GWFBCFMODULE,ONLY:LAYCON
      USE GWTBCTMODULE, ONLY: MCOMP,CONC,CONCO,ICT,IADSORB,PRSITY,
     * ADSORB,FLICH
      USE GWFDPFMODULE, ONLY: PHIF,SnIM,SoIM,SC1IM,SC2IM,HNEWIM,HOLDIM
      USE GWTDPTMODULE, ONLY: CONCIM,CONCOIM,IADSORBIM,PRSITYIM,
     * ADSORBIM
      DOUBLE PRECISION VODT,ADSTERM,CW,FL,CWO,DT,RT,ALENG,VOLU,
     1  X,Y,EPSS,EPS,CT,CEPS,QA,QEPS,DQ,OMP,DTERMS,RTERMS
C-------------------------------------------------------------------------------------
      IF(ISS.EQ.1.OR.N.GT.NODES) THEN
C-------USE TRADITIONAL STORAGE TERM FOR CLN DOMAIN OR SS FLOW
        IF(N.LE.NODES) VODT = VODT * PRSITYIM(N)
        DTERMS = DTERMS - VODT * SnIM(N)
        RTERMS = RTERMS - VODT * SoIM(N) * CONCOIM(N,ICOMP)
      ELSE
        K = LAYNOD(N)
        LC=LAYCON(K)
        IF(LC.EQ.4.OR.LC.EQ.5)THEN !
C---------TWO STORAGES FOR UPSTREAM WEIGHTING FORMULATION
          DTERMS = DTERMS -
     *      ((PRSITYIM(N)*VOLU-SC2IM(N)*ALENG) + ALENG*SC2IM(N)*SnIM(N)
     *      +SnIM(N)*SC1IM(N)*HNEWIM(N)) / DELT
          RTERMS = RTERMS -
     *      ((PRSITYIM(N)*VOLU-SC2IM(N)*ALENG) + ALENG*SC2IM(N)*SoIM(N)
     *      + SnIM(N)*SC1IM(N)*HOLDIM(N)) * CONCOIM(N,ICOMP) / DELT
        ELSEIF(LC.EQ.0.OR.LC.EQ.2) THEN
C---------ONE CONFINED STORAGE CAPACITY FOR FLOW
            DTERMS = DTERMS - (PRSITYIM(N)*VOLU+SC1IM(N)*HNEWIM(N))/DELT
            RTERMS = RTERMS - (PRSITYIM(N)*VOLU+SC1IM(N)*HOLDIM(N))/DELT
     *       * CONCOIM(N,ICOMP)
        ELSEIF(LC.EQ.1) THEN ! IN BCF, IF LAYCON = 1, THE SPECIRFIC YIELD IS READ IN SC2 ?
C---------ONE UNCONFINED STORAGE CAPACITY FOR FLOW
          DTERMS = DTERMS -
     *   ((PRSITYIM(N)*VOLU-SC2IM(N)*ALENG)+SC2IM(N)*ALENG*SnIM(N))/DELT
          RTERMS = RTERMS -
     *   ((PRSITYIM(N)*VOLU-SC2IM(N)*ALENG)+SC2IM(N)*ALENG*SoIM(N))/DELT
     *     * CONCOIM(N,ICOMP)
        ELSEIF(LC.EQ.3) THEN
C---------TWO STORAGE CAPACITIES FOR FLOW
          TP=TOP(N)
          IF(HOLDIM(N).GT.TP.AND.HNEWIM(N).GT.TP) THEN
C-----------CELL IS CONFINED
            DTERMS = DTERMS - (PRSITYIM(N)*VOLU+SC1IM(N)*HNEWIM(N))/DELT
            RTERMS = RTERMS - (PRSITYIM(N)*VOLU+SC1IM(N)*HOLDIM(N))/DELT
     *       * CONCOIM(N,ICOMP)
          ELSEIF (HOLDIM(N).LT.TP.AND.HNEWIM(N).LT.TP) THEN
C-----------CELL IS UNCONFINED
            DTERMS = DTERMS -
     *   ((PRSITYIM(N)*VOLU-SC2IM(N)*ALENG)+SC2IM(N)*ALENG*SnIM(N))/DELT
            RTERMS = RTERMS -
     *   ((PRSITYIM(N)*VOLU-SC2IM(N)*ALENG)+SC2IM(N)*ALENG*SoIM(N))/DELT
     *     * CONCOIM(N,ICOMP)
          ELSE
C-----------CELL HAS CONVERTED THIS TIME STEP
            IF(HNEWIM(N).GT.HOLDIM(N)) THEN
C-------------WLEs RAISED - BECOMES CONFINED
              H1 = HNEWIM(N)
              H2 = TP
            ELSE
C-------------WLEs LOWERED - BECOMES UNCONFINED
              H1 = TP
              H2 = HOLDIM(N)
            ENDIF
            DTERMS = DTERMS - ((PRSITYIM(N)*VOLU-SC2IM(N)*ALENG) +
     *       SC2IM(N)*ALENG*SnIM(N) + SC1IM(N)*H1) / DELT
            RTERMS = RTERMS - ((PRSITYIM(N)*VOLU-SC2IM(N)*ALENG) +
     *       SC2IM(N)*ALENG*SoIM(N) + SC1IM(N)*H2) / DELT
     *       * CONCOIM(N,ICOMP)
          ENDIF
        ENDIF
      ENDIF
C
C9------RETURN
      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE GWT2DPT1DCYIM(N,ICOMP,DTERMD,RTERMD)
C     ******************************************************************
C     FORMULATE STORAGE TERM FOR EACH NODE AND COMPONENT IN IMMOBILE DOMAIN
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,TOP,NODLAY,ISYM,
     1 HNEW,AMAT,RHS,HOLD,ISSFLG,IA,JA,NJA,NODES,AREA,BOT,TOP,Sn,So
      USE GWTBCTMODULE, ONLY: ICT
      USE GWFDPFMODULE, ONLY: PHIF,SnIM,SoIM
      USE CLN1MODULE, ONLY: ACLNNDS
      USE GWFBASMODULE,ONLY:DELT
      USE GWTDPTMODULE, ONLY: CONCIM,CONCOIM,IADSORBIM,PRSITYIM,
     * ADSORBIM,FLICHIM,IZODIM,IFODIM,ZODRWIM,FODRWIM,
     1  ZODRSIM,FODRSIM
      DOUBLE PRECISION ADSTERM,CW,FL,CWO,DT,RT,ALENG,VOLU,
     1  X,Y,EPSS,EPS,CT,CEPS,QA,QEPS,DQ,OMP,DTERMD,RTERMD
C     ------------------------------------------------------------------
      DTERMD = 0.0
      RTERMD = 0.0
      IF(N.LE.NODES)THEN
        ALENG = TOP(N) - BOT(N)
      ELSE
        ALENG = ACLNNDS(N-NODES,5)
      ENDIF
      VOLU = AREA(N) * ALENG
C----------------------------------------------------------------------
      IF(ICT.EQ.0)THEN  !----------WATER PHASE CONCENTRATION FORMULATION
C------- ADD DECAY TERMS ON SOIL (NO ADSORPTION ON CLN)
        IF(N.LE.NODES)THEN
C---------ZERO ORDER DECAY ON SOIL - APPLY NEWTON EXPANSION OF SUPPLY-DEMAND FUNCTION
          IF(IZODIM.GE.2.AND.IADSORBIM.GT.0)THEN
            CT = - VOLU * ZODRSIM(N,ICOMP)
            EPS = 0.01
            CEPS = MAX(0.0,CONCIM(N,ICOMP))
            X = CEPS /EPS
            CALL SMOOTH(X,Y)
            QA =  CT * Y
C-----------CALCULATE DQ/DH
            EPSS = 0.001 * EPS
            CEPS = MAX(0.0,CONCIM(N,ICOMP)+EPSS)
            X = (CEPS)/EPS
            CALL SMOOTH(X,Y)
            QEPS = CT * Y
            DQ = (QEPS - QA) / EPSS
            DTERMD = DTERMD + DQ
            RTERMD = RTERMD - QA + DQ*CONCIM(N,ICOMP)
          ENDIF
C
C---------FIRST ORDER DECAY ON SOIL
          IF(IFODIM.GE.2.AND.IADSORBIM.GT.0)THEN
            CT = -ADSORBIM(N,ICOMP) * VOLU * FODRSIM(N,ICOMP)
            IF(IADSORBIM.EQ.1)THEN
C--------------FOR LINEAR ADSORPTION
              DTERMD = DTERMD + CT
            ELSE
C--------------FOR NON-LINEAR ADSORPTION FILL AS NEWTON
              ETA = FLICHIM(N,ICOMP)
              QA =  CT * CONCIM(N,ICOMP) ** ETA
C--------------CALCULATE DQ/DH
              EPSS = 0.00001
              CEPS = CONCIM(N,ICOMP)+EPSS
              QEPS = CT * CEPS ** ETA
              DQ = (QEPS - QA) / EPSS
              DTERMD = DTERMD + DQ
              RTERMD = RTERMD - QA + DQ*CONCIM(N,ICOMP)
            ENDIF
          ENDIF
        ENDIF
C-----------------------------------------------------------------------
C---------ZERO ORDER DECAY IN WATER - APPLY NEWTON EXPANSION OF SUPPLY-DEMAND FUNCTION
        IF(IZODIM.EQ.1.OR.IZODIM.EQ.3)THEN
          CT = -SnIM(N)* VOLU * ZODRWIM(N,ICOMP)
          EPS = 0.01
          CEPS = MAX(0.0,CONCIM(N,ICOMP))
          X = CEPS /EPS
          CALL SMOOTH(X,Y)
          QA =  CT * Y
C-----------CALCULATE DQ/DH
          EPSS = 0.001 * EPS
          CEPS = MAX(0.0,CONCIM(N,ICOMP)+EPSS)
          X = (CEPS)/EPS
          CALL SMOOTH(X,Y)
          QEPS = CT * Y
          DQ = (QEPS - QA) / EPSS
          DTERMD = DTERMD + DQ
          RTERMD = RTERMD - QA + DQ*CONCIM(N,ICOMP)
        ENDIF
C
C---------FIRST ORDER DECAY IN WATER
        IF(IFODIM.EQ.1.OR.IFODIM.EQ.3)THEN
          CT =  -SnIM(N)* VOLU * FODRWIM(N,ICOMP)
          DTERMD = DTERMD + CT
        ENDIF
      ELSE       !-----------------------TOTAL CONCENTRATION FORMULATION
C
C-------NET DECAY TERM FOR TOTAL CONCENTRATION FORMULATION
CSP TO DO
      ENDIF
C
C9------RETURN
      RETURN
      END
C-------------------------------------------------------------------------
      SUBROUTINE GWT2STOIM1BD(KSTP,KPER,ICOMP)
C     ******************************************************************
C     CALCULATE MASS BUDGET TERMS FOR IMMOBILE DOMAIN FOR ALL TRANSPORT CELLS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,BUFF,NODES,IUNSTR,
     1  AMAT,IA,JA,TOP,BOT,AREA,Sn,So
      USE CLN1MODULE, ONLY: ACLNNDS,NCLNNDS
      USE GWFBASMODULE,ONLY:MSUM,ISPCFL,IAUXSV,DELT,PERTIM,TOTIM,DELT
      USE GWTBCTMODULE, ONLY: MSUMT,VBVLT,VBNMT,ICT
      USE GWTDPTMODULE, ONLY: ICBUNDIM,CONCIM,CONCOIM,IDPTCB,
     1 IADSORBIM,ADSORBIM,FLICHIM,PRSITYIM,CONCOIM
      USE GWFDPFMODULE, ONLY: PHIF,SnIM,SoIM
C
      CHARACTER*16 TEXT(2)
      DOUBLE PRECISION RATIN,RATOUT,QQ,VODT,ADSTERM,FL,CW,CWO,ALENG,
     *  DTERMS,RTERMS,VOLU
      DATA TEXT(1) /' IM MASS STORAGE'/
      DATA TEXT(2) /'IM CLN MASS STRG'/
C     ------------------------------------------------------------------
C
C1------CLEAR RATIN AND RATOUT ACCUMULATORS, AND SET CELL-BY-CELL
C1------BUDGET FLAG.
      ZERO=0.
      RATIN=ZERO
      RATOUT=ZERO
      IBD=0
      IF(IDPTCB.LT.0 .AND. ISPCFL.NE.0) IBD=-1
      IF(IDPTCB.GT.0) IBD=ISPCFL
      IBDLBL=0
C
C3------CLEAR THE BUFFER.
      DO 50 N=1,NODES
      BUFF(N)=ZERO
50    CONTINUE
C
C5------LOOP THROUGH EACH NODE AND CALCULATE STORAGE
      DO 100 N=1,NODES
C
C5B-----IF THE CELL IS NOT PCB OR WRONG COMPONENT SPECIES, IGNORE IT.
      IF(ICBUNDIM(N).EQ.0)GO TO 99
C
      ALENG = TOP(N) - BOT(N)
      VOLU = AREA(N) * ALENG
      VODT = VOLU / DELT
      QQ = 0.0
      IF(ICT.EQ.0)THEN
C-------STORAGE TERM ON SOIL
        IF(N.LE.NODES.AND.IADSORBIM.EQ.1)THEN
          ADSTERM = ADSORBIM(N,ICOMP) * VODT
          QQ = ADSTERM * (CONCIM(N,ICOMP) - CONCOIM(N,ICOMP))
        ELSEIF(N.LE.NODES.AND.IADSORBIM.EQ.2)THEN
          ADSTERM = ADSORBIM(N,ICOMP) * VODT
          FL = FLICHIM(N,ICOMP)
          CW = 0.0
          CWO = 0.0
          IF(CONCIM(N,ICOMP).GT.0.0) CW = CONCIM(N,ICOMP)
          IF(CONCOIM(N,ICOMP).GT.0.0) CWO = CONCOIM(N,ICOMP)
          QQ = ADSTERM * (CW**FL - CWO**FL)
        ENDIF
C-------STORAGE TERM IN WATER
CSP        IF(N.LE.NODES) VODT = VODT * PRSITYIM(N)
CSP        QQ = QQ + VODT * SnIM(N) * CONCIM(N,ICOMP)
CSP     *          - VODT * SoIM(N) * CONCOIM(N,ICOMP)
        DTERMS = 0.0
        RTERMS = 0.0
        CALL GWT2BCT1STOIMW(N,ICOMP,DTERMS,RTERMS,VODT,VOLU,ALENG,ISS)
        QQ = QQ - DTERMS * CONCIM(N,ICOMP) + RTERMS
      ELSE
        QQ = QQ + VODT * CONCIM(N,ICOMP)
     *          - VODT * CONCOIM(N,ICOMP)
      ENDIF
      QQ = - QQ  ! STORAGE TERM NEGATIVE IS INFLOW AS PER MODFLOW CONVENTION
      Q = QQ
C
C5D-----PRINT FLOW RATE IF REQUESTED.
      IF(IBD.LT.0) THEN
         IF(IBDLBL.EQ.0) WRITE(IOUT,61) TEXT,KPER,KSTP
   61    FORMAT(1X,/1X,A,'   PERIOD ',I4,'   STEP ',I3)
        IF(IUNSTR.EQ.0.AND.N.LE.NODES)THEN
          IL = (N-1) / (NCOL*NROW) + 1
          IJ = N - (IL-1)*NCOL*NROW
          IR = (IJ-1)/NCOL + 1
          IC = IJ - (IR-1)*NCOL
           WRITE(IOUT,62) L,IL,IR,IC,Q
   62    FORMAT(1X,'CBC  ',I6,'   LAYER ',I3,'   ROW ',I5,'   COL ',I5,
     1       '   FLUX ',1PG15.6)
        ELSE
           WRITE(IOUT,63) L,N,Q
   63    FORMAT(1X,'CBC  ',I6,'    NODE ',I8,'   FLUX ',1PG15.6)
        ENDIF
        IBDLBL=1
      END IF
C
C5E-----ADD FLOW RATE TO BUFFER.
      BUFF(N)=BUFF(N)+Q
C
C5F-----SEE IF FLUX IS POSITIVE OR NEGATIVE.
      IF(QQ.GE.ZERO) THEN
C
C5G-----POSITIVE FLOW RATE. ADD IT TO RATIN
        RATIN=RATIN+QQ
      ELSE
C
C5H-----NEGATIVE FLOW RATE. ADD IT TO RATOUT
        RATOUT=RATOUT-QQ
      END IF
   99 CONTINUE
C
100   CONTINUE
C
C6------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A 3-D ARRAY,
C6------CALL UBUDSV TO SAVE THEM.
      IF(IBD.EQ.1)THEN
        IF(IUNSTR.EQ.0)THEN
          CALL UBUDSV(KSTP,KPER,TEXT(1),IDPTCB,BUFF,NCOL,NROW,
     1                          NLAY,IOUT)
        ELSE
          CALL UBUDSVU(KSTP,KPER,TEXT(1),IDPTCB,BUFF,NODES,
     1                          IOUT,PERTIM,TOTIM)
        ENDIF
      ENDIF
C
C7------MOVE RATES, VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
  200 RIN=RATIN
      ROUT=RATOUT
      VBVLT(3,MSUMT,ICOMP)=RIN
      VBVLT(4,MSUMT,ICOMP)=ROUT
      VBVLT(1,MSUMT,ICOMP)=VBVLT(1,MSUMT,ICOMP)+RATIN*DELT
      VBVLT(2,MSUMT,ICOMP)=VBVLT(2,MSUMT,ICOMP)+RATOUT*DELT
      VBNMT(MSUMT,ICOMP)=TEXT(1)
C
C8------INCREMENT BUDGET TERM COUNTER(MSUM).
      MSUMT=MSUMT+1
C
C9------RETURN
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE GWT2DCYIM1BD(KSTP,KPER,ICOMP)
C     ******************************************************************
C     CALCULATE MASS DECAY TERMS FOR ALL TRANSPORT CELLS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,   ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,BUFF,NODES,IUNSTR,
     1  AMAT,IA,JA,TOP,BOT,AREA,Sn,So
      USE CLN1MODULE, ONLY: ACLNNDS,NCLNNDS
      USE GWFBASMODULE,ONLY:MSUM,ISPCFL,IAUXSV,DELT,PERTIM,TOTIM,DELT
      USE GWTBCTMODULE, ONLY: MSUMT,VBVLT,VBNMT,ICT
      USE GWTDPTMODULE, ONLY: ICBUNDIM,CONCIM,IDPTCB,
     1 IADSORBIM,ADSORBIM,FLICHIM,PRSITYIM,CONCOIM,IZODIM,IFODIM,
     1 ZODRWIM,FODRWIM,ZODRSIM,FODRSIM
      USE GWFDPFMODULE, ONLY: PHIF,SnIM
C
      CHARACTER*16 TEXT(2)
      DOUBLE PRECISION RATIN,RATOUT,QQ,VOLU,ADSTERM,FL,CW,CWO,ALENG,X,Y,
     1  CEPS,EPS,CT
      DATA TEXT(1) /'   IM MASS DECAY'/
      DATA TEXT(2) /'IMCLN MASS DECAY'/
C     ------------------------------------------------------------------
C1------RETURN IF NO DECAY IN SIMULATION
      IF(IZODIM.EQ.0.AND.IFODIM.EQ.0) RETURN
C2------CLEAR RATIN AND RATOUT ACCUMULATORS, AND SET CELL-BY-CELL
C2------BUDGET FLAG.
      ZERO=0.
      RATIN=ZERO
      RATOUT=ZERO
      IBD=0
      IF(IDPTCB.LT.0 .AND. ISPCFL.NE.0) IBD=-1
      IF(IDPTCB.GT.0) IBD=ISPCFL
      IBDLBL=0
C
C3------CLEAR THE BUFFER.
      DO 50 N=1,NODES
      BUFF(N)=ZERO
50    CONTINUE
C
C5------LOOP THROUGH EACH NODE AND CALCULATE STORAGE
      DO 100 N=1,NODES
C
C5B-----IF THE CELL IS NOT PCB OR WRONG COMPONENT SPECIES, IGNORE IT.
      IF(ICBUNDIM(N).EQ.0)GO TO 99
C
      ALENG = TOP(N) - BOT(N)
      VOLU = AREA(N) * ALENG
      QQ = 0.0
C-----------------------------------------------------------------------------
      IF(ICT.EQ.0)THEN  !----------WATER PHASE CONCENTRATION FORMULATION
C-----------------------------------------------------------------------------
C-------DECAY TERM ON SOIL (NO ADSORPTION ON CLN)
        IF(N.LE.NODES)THEN
C
C---------ZERO ORDER DECAY ON SOIL
            IF(IZODIM.GE.2.AND.IADSORBIM.GT.0)THEN
            CT = -(1.0-PRSITYIM(N)) * VOLU * ZODRSIM(N,ICOMP)
            EPS = 0.01
            CEPS = MAX(0.0,CONCIM(N,ICOMP))
            X = CEPS /EPS
            CALL SMOOTH(X,Y)
            QQ =  CT * Y
          ENDIF
C
C---------FIRST ORDER DECAY ON SOIL
          IF(IFODIM.GE.2.AND.IADSORBIM.GT.0)THEN
            CT = -ADSORBIM(N,ICOMP) * VOLU * FODRSIM(N,ICOMP) ! * (1.0-PRSITYIM(N)) ???
            IF(IADSORBIM.EQ.1)THEN
C--------------FOR LINEAR ADSORPTION
              QQ = QQ + CT * CONCIM(N,ICOMP)
            ELSE
C--------------FOR NON-LINEAR ADSORPTION FILL AS NEWTON
              ETA = FLICHIM(N,ICOMP)
              QQ =  CT * CONCIM(N,ICOMP) ** ETA
            ENDIF
          ENDIF
          ENDIF
C-----------------------------------------------------------------------------          
C-------DECAY TERM IN WATER
C-----------------------------------------------------------------------------
C-------ZERO ORDER DECAY IN WATER 
        IF(IZODIM.EQ.1.OR.IZODIM.EQ.3)THEN
          CT = -SnIM(N)* VOLU * ZODRWIM(N,ICOMP)
          EPS = 0.01
          CEPS = MAX(0.0,CONCIM(N,ICOMP))
          X = CEPS /EPS
          CALL SMOOTH(X,Y)
          QQ =  QQ + CT * Y
        ENDIF
C
C-------FIRST ORDER DECAY IN WATER
        IF(IFODIM.EQ.1.OR.IFODIM.EQ.3)THEN
          CT =  -SnIM(N)* VOLU * FODRWIM(N,ICOMP)
          QQ = QQ + CT * CONCIM(N,ICOMP)
        ENDIF
      ELSE
CSP FINISH TOTAL CONCENTRATION FORMULATION
      ENDIF
      Q = QQ
C
C5D-----PRINT FLOW RATE IF REQUESTED.
      IF(IBD.LT.0) THEN
         IF(IBDLBL.EQ.0) WRITE(IOUT,61) TEXT(1),KPER,KSTP
   61    FORMAT(1X,/1X,A,'   PERIOD ',I4,'   STEP ',I3)
        IF(IUNSTR.EQ.0.AND.N.LE.NODES)THEN
          IL = (N-1) / (NCOL*NROW) + 1
          IJ = N - (IL-1)*NCOL*NROW
          IR = (IJ-1)/NCOL + 1
          IC = IJ - (IR-1)*NCOL
           WRITE(IOUT,62) L,IL,IR,IC,Q
   62    FORMAT(1X,'CBC  ',I6,'   LAYER ',I3,'   ROW ',I5,'   COL ',I5,
     1       '  DECAY ',1PG15.6)
        ELSE
           WRITE(IOUT,63) L,N,Q
   63    FORMAT(1X,'CBC  ',I6,'    NODE ',I8,'  DECAY ',1PG15.6)
        ENDIF
        IBDLBL=1
      END IF
C
C5E-----ADD FLOW RATE TO BUFFER.
      BUFF(N)=BUFF(N)+Q
C
C5F-----SEE IF FLUX IS POSITIVE OR NEGATIVE.
      IF(QQ.GE.ZERO) THEN
C
C5G-----POSITIVE FLOW RATE. ADD IT TO RATIN
        RATIN=RATIN+QQ
      ELSE
C
C5H-----NEGATIVE FLOW RATE. ADD IT TO RATOUT
        RATOUT=RATOUT-QQ
      END IF
   99 CONTINUE
C
100   CONTINUE
C
C6------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A 3-D ARRAY,
C6------CALL UBUDSV TO SAVE THEM.
      IF(IBD.EQ.1)THEN
        IF(IUNSTR.EQ.0)THEN
          CALL UBUDSV(KSTP,KPER,TEXT(1),IDPTCB,BUFF,NCOL,NROW,
     1                          NLAY,IOUT)
        ELSE
          CALL UBUDSVU(KSTP,KPER,TEXT(1),IDPTCB,BUFF,NODES,
     1                          IOUT,PERTIM,TOTIM)
        ENDIF
      ENDIF
C
C7------MOVE RATES, VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
  200 RIN=RATIN
      ROUT=RATOUT
      VBVLT(3,MSUMT,ICOMP)=RIN
      VBVLT(4,MSUMT,ICOMP)=ROUT
      VBVLT(1,MSUMT,ICOMP)=VBVLT(1,MSUMT,ICOMP)+RATIN*DELT
      VBVLT(2,MSUMT,ICOMP)=VBVLT(2,MSUMT,ICOMP)+RATOUT*DELT
      VBNMT(MSUMT,ICOMP)=TEXT(1)
C
C8------INCREMENT BUDGET TERM COUNTER(MSUM).
      MSUMT=MSUMT+1
C
C9------RETURN
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE SSMS2DPT1RED(KPER)
C     ******************************************************************
C     REDUCE THE IMMOBILE DOMAIN EQUATION INTO THE MOBILE DOMAIN EQUATION
C     AT EACH DUAL POROSITY CELL FOR TRANSPORT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY: NODES,AMAT,RHS,IA,ISSFLG
      USE GWTDPTMODULE, ONLY: DIADDT, RDDT, OFFDDTM,OFFDDTIM,ICBUNDIM
C
C     ------------------------------------------------------------------
C-----------------------------------------------------------------------------
C1------LOOP OVER NODES TO REDUCE EQUATIONS
C-----------------------------------------------------------------------------
      DO N=1,NODES
        IF(ICBUNDIM(N).EQ.0) CYCLE
        IPIV = IA(N)
        AMAT(IPIV) = AMAT(IPIV) - OFFDDTIM(N)/DIADDT(N)*OFFDDTM(N)
        RHS(N) = RHS(N) - RDDT(N)/DIADDT(N)*OFFDDTM(N)
      ENDDO
C
C11----RETURN.
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE SSMS2DPT1BKS(KPER,ICOMP)
C     ******************************************************************
C     BACK-SUBSTITUTE INTO IMMOBILE DOMAIN EQUATION TO GET ITS CONC
C     AT EACH DUAL POROSITY CELL
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:NODES,AMAT,RHS,IA,HNEW,ISSFLG
      USE GWTBCTMODULE, ONLY: CONC
      USE GWTDPTMODULE,ONLY:DIADDT,RDDT,OFFDDTM,OFFDDTIM,CONCIM,ICBUNDIM
C
C     ------------------------------------------------------------------
C-----------------------------------------------------------------------------
C1------LOOP OVER NODES TO FILL CONCIM
C-----------------------------------------------------------------------------
      DO N=1,NODES
        IF(ICBUNDIM(N).EQ.0) CYCLE
C1B-----COMPUTE CONCIM FOR TRANSIENT STRESS PERIOD
        CONCIM(N,ICOMP) = RDDT(N)/DIADDT(N) -
     1        OFFDDTIM(N)*CONC(N,ICOMP)/DIADDT(N)
      ENDDO
C
C2----RETURN.
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE GWT2DPT1AD
C     ******************************************************************
C     COPY NEW INTO OLD VARIABLES FOR DUAL POROSITY TRANSPORT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:NODES
      USE GWTDPTMODULE, ONLY:CONCOIM,CONCIM
      USE GWTBCTMODULE, ONLY: MCOMP
C     ------------------------------------------------------------------
C
C1----COPY NEW TO OLD VARIABLES
      DO ICOMP=1,MCOMP
      DO N=1,NODES
        CONCOIM(N,ICOMP) = CONCIM(N,ICOMP)
      ENDDO
      ENDDO
C
C4------RETURN
      RETURN
      END
C -----------------------------------------------------------------------
      SUBROUTINE GWT2DPT1OT(KSTP,KPER,ISA)
C     ******************************************************************
C     OUTPUT CONC OF IMMOBILE DOMAIN
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:ITMUNI,IOUT,IUNSTR
      USE GWFBASMODULE,ONLY:DELT,PERTIM,TOTIM,ISPCFL,IBUDFL
      USE GWTBCTMODULE, ONLY: MCOMP,VBVLT,VBNMT,MSUMT
C     ------------------------------------------------------------------
C
C
C1------CLEAR PRINTOUT FLAG (IPFLG)
      IPFLG=0
      DO ICOMP = 1,MCOMP
        WRITE(IOUT,1)ICOMP
1       FORMAT(5X,'TRANSPORT SOLUTION COMPLETE FOR COMPONENT SPECIES',
     *    1X,'NUMBER',I5,' IN IMMOBILE DOMAIN'/5X,80('-'))
C
C3------IF CONCENTRATION PRINT FLAG (ISPCFL) IS SET WRITE
C3------CONCENTRATION, AND ICBUND IN ACCORDANCE WITH FLAGS IN IOFLG.
        IF(ISPCFL.EQ.0) GO TO 100
C
        IF(IUNSTR.EQ.0)THEN ! WRITE M2K5 STYLE FOR STRUCTURED GRID
          CALL SGWT2DPT1C(KSTP,KPER,IPFLG,ISA,ICOMP)
        ELSE
          CALL SGWT2DPT1CU(KSTP,KPER,IPFLG,ISA,ICOMP)
        ENDIF
        IPFLG = 1
C
  100   CONTINUE

C4------PRINT TOTAL BUDGET IF REQUESTED
CPS      IF(IBUDFL.EQ.0) GO TO 120
CPS        CALL SGWF2BAS7V(MSUMT,VBNMT(1,ICOMP),VBVLT(1,1,ICOMP),
CPS     *  KSTP,KPER,IOUT)
CPS        IPFLG=1
CPS
C5------END PRINTOUT WITH TIME SUMMARY AND FORM FEED IF ANY PRINTOUT
C5------WILL BE PRODUCED.
  120   IF(IPFLG.EQ.0) GO TO 99
        CALL SGWF2BAS7T(KSTP,KPER,DELT,PERTIM,TOTIM,ITMUNI,IOUT)
        WRITE(IOUT,101)
  101   FORMAT('1')

      ENDDO
C
C6------RETURN
99    CONTINUE
      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE SGWT2DPT1C(KSTP,KPER,IPFLG,ISA,ICOMP)
C     ******************************************************************
C     PRINT AND RECORD IMMOBILE DOMAIN CONCS FOR STRUCTURED GWF GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IXSEC,HNEW,NODLAY,
     1                      IBOUND,IOUT
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,IOFLG,ISPCFM,LBHDSV,
     2                      CSPCFM,IOFLG
      USE GWTDPTMODULE, ONLY: CONCIM,IDPTCON,ICBUNDIM
C
      REAL,          SAVE,    DIMENSION(:,:,:),    ALLOCATABLE ::BUFF
      CHARACTER*16 TEXT
      DATA TEXT /'   IMMOBILE CONC'/
C     ------------------------------------------------------------------
      ALLOCATE(BUFF(NCOL,NROW,NLAY))
C
C1------FOR EACH LAYER MOVE CONCIM TO BUFF IF PRINT OR SAVE IS REQUESTED.
      DO 59 K=1,NLAY
C
C2------IS CONC NEEDED FOR THIS LAYER?
      KL=K
      IF(IXSEC.NE.0) KL=1
      IF(IOFLG(KL,5).EQ.0 .AND. IOFLG(KL,6).EQ.0) GO TO 59
C
C3------MOVE CONCIM TO BUFF FOR THE LAYER.
      DO 58 I=1,NROW
      DO 58 J=1,NCOL
      N = (K-1)*NROW*NCOL + (I-1)*NCOL + J
      BUFF(J,I,K)=CONCIM(N,ICOMP)
   58 CONTINUE
   59 CONTINUE
C
C4------FOR EACH LAYER: DETERMINE IF CONC SHOULD BE PRINTED.
C4------IF SO THEN CALL ULAPRS OR ULAPRW TO PRINT CONC.
      IF(ISA.NE.0) THEN
         IF(IXSEC.EQ.0) THEN
           DO 69 K=1,NLAY
           KK=K
           IF(IOFLG(K,5).EQ.0) GO TO 69
           IF(ISPCFM.LT.0) CALL ULAPRS(BUFF(1,1,K),TEXT,KSTP,KPER,
     1               NCOL,NROW,KK,-ISPCFM,IOUT)
           IF(ISPCFM.GE.0) CALL ULAPRW(BUFF(1,1,K),TEXT,KSTP,KPER,
     1               NCOL,NROW,KK,ISPCFM,IOUT)
           IPFLG=1
   69      CONTINUE
C
C4A-----PRINT CONC FOR CROSS SECTION.
         ELSE
           IF(IOFLG(1,5).NE.0) THEN
             IF(ISPCFM.LT.0) CALL ULAPRS(BUFF,TEXT,KSTP,KPER,
     1                 NCOL,NLAY,-1,-ISPCFM,IOUT)
             IF(ISPCFM.GE.0) CALL ULAPRW(BUFF,TEXT,KSTP,KPER,
     1                 NCOL,NLAY,-1,ISPCFM,IOUT)
             IPFLG=1
           END IF
         END IF
      END IF
C
C5------FOR EACH LAYER: DETERMINE IF CONC SHOULD BE SAVED ON DISK.
C5------IF SO THEN CALL ULASAV OR ULASV2 TO SAVE CONC.
      IFIRST=1
      IF(IDPTCON.LE.0) GO TO 80
      IF(IXSEC.EQ.0) THEN
        DO 79 K=1,NLAY
        NSTRT = NODLAY(K-1)+1
        KK=K
        IF(IOFLG(K,6).EQ.0) GO TO 79
        IF(IFIRST.EQ.1) WRITE(IOUT,74) IDPTCON,KSTP,KPER
   74   FORMAT(1X,/1X,'IMMOBILE DOMAIN CONC WILL BE SAVED ON UNIT ',I4,
     1      ' AT END OF TIME STEP ',I3,', STRESS PERIOD ',I4)
        IFIRST=0
        IF(CSPCFM.EQ.' ') THEN
           CALL ULASAV(BUFF(1,1,K),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,KK,IDPTCON)
        ELSE
           CALL ULASV2(BUFF(1,1,K),TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NROW,KK,IDPTCON,CSPCFM,LBHDSV,ICBUNDIM(NSTRT))
        END IF
   79   CONTINUE
C
C5A-----SAVE CONC FOR CROSS SECTION.
      ELSE
        IF(IOFLG(1,6).NE.0) THEN
          WRITE(IOUT,74) IDPTCON,KSTP,KPER
          IF(CSPCFM.EQ.' ') THEN
             CALL ULASAV(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                NLAY,-1,IDPTCON)
          ELSE
             CALL ULASV2(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NCOL,
     1                  NLAY,-1,IDPTCON,CSPCFM,LBHDSV,ICBUNDIM)
          END IF
        END IF
      END IF
80    CONTINUE
      DEALLOCATE(BUFF)
C
C6------RETURN.
      RETURN
      END
C----------------------------------------------------------------------
      SUBROUTINE SGWT2DPT1CU(KSTP,KPER,IPFLG,ISA,ICOMP)
C     ******************************************************************
C     PRINT AND RECORD IMMOBILE DOMAIN CONCS FOR UNSTRUCTURED GWF GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IXSEC,HNEW,NODLAY,
     1                      IBOUND,IOUT,NODES,BUFF
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,IOFLG,ISPCFM,LBHDSV,
     2                      CSPCFM,IOFLG
      USE GWTDPTMODULE, ONLY: CONCIM,IDPTCON,ICBUNDIM
C
      CHARACTER*16 TEXT
      DATA TEXT /'  IMMOBILE CONCU'/
C     ------------------------------------------------------------------
C
C1------FOR EACH LAYER MOVE CONC TO BUFF IF PRINT OR SAVE IS REQUESTED.
      DO 59 K=1,NLAY
C
C2------IS CONC NEEDED FOR THIS LAYER?
      KL=K
      IF(IXSEC.NE.0) KL=1
      IF(IOFLG(KL,5).EQ.0 .AND. IOFLG(KL,6).EQ.0) GO TO 59
C
C3------MOVE CONC TO BUFF FOR THE LAYER.
      NNDLAY = NODLAY(K)
      NSTRT = NODLAY(K-1)+1
      DO 58 N=NSTRT,NNDLAY
      BUFF(N)=CONCIM(N,ICOMP)
   58 CONTINUE
   59 CONTINUE
C
C4------FOR EACH LAYER: DETERMINE IF CONC SHOULD BE PRINTED.
C4------IF SO THEN CALL ULAPRU TO PRINT CONC.
      IF(ISA.NE.0) THEN
         IF(IXSEC.EQ.0) THEN
           DO 69 K=1,NLAY
           KK=K
           IF(IOFLG(K,5).EQ.0) GO TO 69
           NNDLAY = NODLAY(K)
           NSTRT = NODLAY(K-1)+1
           CALL ULAPRU(BUFF,TEXT,KSTP,KPER,
     1           NSTRT,NNDLAY,KK,IABS(ISPCFM),IOUT,PERTIM,TOTIM,NODES)
           IPFLG=1
   69      CONTINUE
C
C4A-----PRINT HEAD FOR CROSS SECTION.
         ELSE
           IF(IOFLG(1,5).NE.0) THEN
           IF(ISPCFM.NE.0) CALL ULAPRU(BUFF,TEXT,KSTP,KPER,
     1           NSTRT,NNDLAY,-1,IABS(ICONFM),IOUT,PERTIM,TOTIM,NODES)
             IPFLG=1
C
           END IF
         END IF
      END IF
C
C5------FOR EACH LAYER: DETERMINE IF CONC SHOULD BE SAVED ON DISK.
C5------IF SO THEN CALL ULASAV OR ULASV2 TO SAVE HEAD.
      IFIRST=1
      IF(IDPTCON.LE.0) GO TO 80
      IF(IXSEC.EQ.0) THEN
        DO 79 K=1,NLAY
        KK=K
        IF(IOFLG(K,6).EQ.0) GO TO 79
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
        IF(IFIRST.EQ.1) WRITE(IOUT,74) IDPTCON,KSTP,KPER
   74   FORMAT(1X,/1X,'IMMOBILE CONC WILL BE SAVED ON UNIT ',I8,
     1      ' AT END OF TIME STEP ',I8,', STRESS PERIOD ',I8)
        IFIRST=0
        IF(CSPCFM.EQ.' ') THEN
           CALL ULASAVU(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NSTRT,
     1                NNDLAY,KK,IDPTCON,NODES)
        ELSE
           CALL ULASV2U(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NSTRT,
     1            NNDLAY,KK,IDPTCON,CSPCFM,LBHDSV,ICBUNDIM(NSTRT),NODES)
        END IF
        IPFLG=1
   79   CONTINUE
C
C5A-----SAVE CONC FOR CROSS SECTION.
      ELSE
        IF(IOFLG(1,6).NE.0) THEN
          WRITE(IOUT,74) IDPTCON,KSTP,KPER
          IF(CSPCFM.EQ.' ') THEN
             CALL ULASAVU(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NSTRT,
     1                NNDLAY,-1,IDPTCON,NODES)
          ELSE
             CALL ULASV2U(BUFF,TEXT,KSTP,KPER,PERTIM,TOTIM,NSTRT,
     1                  NNDLAY,-1,IDPTCON,CSPCFM,LBHDSV,ICBUNDIM,NODES)
          END IF
          IPFLG=1
        END IF
      END IF
C
C6------RETURN.
   80 CONTINUE
      RETURN
C
      END
