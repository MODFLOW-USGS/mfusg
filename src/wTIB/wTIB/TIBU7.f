      SUBROUTINE GWF2TIB1RP(IN)
C     ******************************************************************
C     READ TRANSIENT IBOUND PACKAGE DATA FOR STRESS PERIOD
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,IFREFM,IUNSTR,IBOUND,HNEW,IA,JA
      USE GWFBASMODULE, ONLY: HNOFLO
      INTEGER, DIMENSION(:),ALLOCATABLE  ::ITEMP
C
      CHARACTER*24 ANAME
      CHARACTER(LEN=200) line
C
      DATA ANAME /'     ZEROED IBOUND CELLS'/
C     ------------------------------------------------------------------
C
C1------IDENTIFY PACKAGE.
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'TIB -- TRANSIENT IBOUND PACKAGE, VERSION 1, ',
     1' 12/24/2012, INPUT READ FROM UNIT ',I4)
C2------READ FIRST LINE
      CALL URDCOM(In, Iout, line)
      LLOC = 1
C3------READ FLAGS      
      IF(IFREFM.EQ.0)THEN
        READ(LINE,'(3I10)') NIB0,NIB1,NIBM1
      ELSE
        CALL URWORD(line, lloc, istart, istop, 2, NIB0, r, Iout, In)
        CALL URWORD(line, lloc, istart, istop, 2, NIB1, r, Iout, In)
        CALL URWORD(line, lloc, istart, istop, 2, NIBM1, r, Iout, In)
      END IF
C------------------------------------------------------------------------
C4------CHECK IF IBOUND IS TO BE ZEROED OUT.
      IF(NIB0.LE.0) THEN
C
C4A-----NIB0=<0, SO NO CELLS INACTIVATED.
        WRITE(IOUT,3)
    3   FORMAT(1X,/1X,'NO CELLS INACTIVATED FROM LAST STRESS PERIOD')
      ELSE
C
C4B-----NIB0>0, SO READ LIST OF INACTIVATED CELLS AND SET IBOUND TO ZERO
        ALLOCATE (ITEMP(NIB0))
        CALL U1DINT(ITEMP,ANAME,NIB0,0,IN,IOUT)   
        DO I=1,NIB0
          ICELL = ITEMP(I)
          IBOUND(ICELL) = 0 
          HNEW(ICELL) = HNOFLO  
        ENDDO
        DEALLOCATE(ITEMP)
      ENDIF
C------------------------------------------------------------------------
C5------CHECK IF IBOUND IS TO BE ACTIVATED.
      IF(NIB1.LE.0) THEN
C
C5A-----NIB1=<0, SO NO CELLS ACTIVATED.
        WRITE(IOUT,4)
4       FORMAT(1X,/1X,'NO CELLS ACTIVATED FROM LAST STRESS PERIOD')
      ELSE
C
C5B-----NIB1>0, SO READ LIST OF ACTIVATED CELLS AND SET IBOUND TO 1
        DO IB=1,NIB1
C5C-------READ CELL NUMBER  
          CALL URDCOM(In, Iout, line)
          LLOC = 1
          IF(IFREFM.EQ.0)THEN
            READ(LINE,'(I10)') ICELL
          ELSE
            CALL URWORD(line, lloc, istart, istop, 2, ICELL, r,Iout, In)
          END IF              
C5D--------GET OPTIONS 
          IAVHEAD=0
          IHEAD = 0
          IF(LINE(ISTART:ISTOP).EQ.'HEAD') THEN
C5D1---------READ KEYWORD OPTION FOR HEAD TO BE READ.   
            IHEAD = 1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,HEAD,IOUT,IN)
          ELSEIF(LINE(ISTART:ISTOP).EQ.'AVHEAD') THEN
C5D2----------READ KEYWORD OPTION FOR AVERAGE HEAD TO BE READ.          
             IAVHEAD=1
          ENDIF
C5E-----------SET IBOUND AND HEADS
          IBOUNDKP = IBOUND(ICELL)
          IBOUND(ICELL) = 1
          IF(IHEAD.EQ.1)THEN
C5E1--------HEAD IS SET TO GIVEN VALUE              
            HNEW(ICELL) = HEAD  
          ELSEIF(IAVHEAD.EQ.1)THEN
C5E2--------HEAD IS SET TO AVERAGE OF CONNECTING ACTIVE CELLS              
            HEAD = 0.0
            ISUM = 0
            DO I=IA(ICELL)+1,IA(ICELL+1)-1
              IF(IBOUND(I).NE.0) THEN
                JJ = JA(I)
                HEAD = HEAD + HNEW(JJ)  
                ISUM = ISUM + 1
              ENDIF
            ENDDO
            HEAD = HEAD / ISUM
          ELSE
C5E3--------CHECK TO SEE IF NODE WAS PREVIOUSLY INACTIVE
            IF(IBOUNDKP.EQ.0)THEN
              WRITE(IOUT,11)ICELL
11            FORMAT(1X,'*** NEED TO SET HEAD IF INACTIVE CELL IS MADE'
     1        1X,'ACTIVE FOR CELL ',I9,', STOPPING ***')
              STOP
            ENDIF  
          ENDIF
        ENDDO
      ENDIF      
C------------------------------------------------------------------------
C6------CHECK IF IBOUND IS TO BE MADE MINUS ONE (PRESCRIBED HEAD).
      IF(NIBM1.LE.0) THEN
C
C5A-----NIBM1=<0, SO NO CELLS MADE PRESCRIBED HEAD.
        WRITE(IOUT,5)
5       FORMAT(/1X,'NO CELLS PRESCRIBED HEAD FROM LAST STRESS PERIOD')
      ELSE
C
C5B-----NIBM1>0, SO READ LIST OF PRESCRIBED HEAD CELLS AND SET IBOUND TO -1
        DO IB=1,NIBM1
C5C-------READ CELL NUMBER  
          CALL URDCOM(In, Iout, line)
          LLOC = 1
          IF(IFREFM.EQ.0)THEN
            READ(LINE,'(I10)') ICELL
          ELSE
            CALL URWORD(line, lloc, istart, istop, 2, ICELL, r,Iout, In)
          END IF              
C5D--------GET OPTIONS 
          IAVHEAD=0
          IHEAD = 0
          IF(LINE(ISTART:ISTOP).EQ.'HEAD') THEN
C5D1---------READ KEYWORD OPTION FOR HEAD TO BE READ.   
            IHEAD = 1
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,HEAD,IOUT,IN)
          ELSEIF(LINE(ISTART:ISTOP).EQ.'AVHEAD') THEN
C5D2----------READ KEYWORD OPTION FOR AVERAGE HEAD TO BE READ.          
             IAVHEAD=1
          ENDIF
C5E-----------SET IBOUND AND HEADS
          IBOUNDKP = IBOUND(ICELL)
          IBOUND(ICELL) = -1
          IF(IHEAD.EQ.1)THEN
C5E1--------HEAD IS SET TO GIVEN VALUE              
            HNEW(ICELL) = HEAD  
          ELSEIF(IAVHEAD.EQ.1)THEN
C5E2--------HEAD IS SET TO AVERAGE OF CONNECTING ACTIVE CELLS              
            HEAD = 0.0
            ISUM = 0
            DO I=IA(ICELL)+1,IA(ICELL+1)-1
              IF(IBOUND(ICELL).NE.0) THEN
                HEAD = HEAD + HNEW(ICELL)  
                ISUM = ISUM + 1
              ENDIF
            ENDDO
            HEAD = HEAD / ISUM
          ELSE
C5E3--------CHECK TO SEE IF NODE WAS PREVIOUSLY INACTIVE
            IF(IBOUNDKP.EQ.0)THEN
              WRITE(IOUT,12)ICELL
12            FORMAT(1X,'*** NEED TO SET HEAD IF INACTIVE CELL IS MADE'
     1        1X,'PRESCRIBED HEAD FOR CELL ',I9,', STOPPING ***')
              STOP
            ENDIF  
          ENDIF
        ENDDO
      ENDIF            
C
C9------RETURN
      RETURN
C
      END         
