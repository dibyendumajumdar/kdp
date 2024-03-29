      PROGRAM CONVERT
      IMPLICIT NONE
      CHARACTER CMDLINE*127,AA*20,FROMFILE*12,TOFILE*12,ITEM*80
      CHARACTER A1*8,TYPE*5
      INTEGER NLINES,I,FLAG1,A1TAG,J
      REAL R1,D1
      AA='                    '
      CMDLINE=AA//AA//AA//AA//AA//AA//'       '
      CALL GETCL(CMDLINE)
      FROMFILE = CMDLINE(1:12)
      OPEN(UNIT=10,ACCESS='SEQUENTIAL',BLANK='NULL'
     1,FORM='FORMATTED',FILE=FROMFILE
     2,STATUS='UNKNOWN')
      OPEN(UNIT=11,ACCESS='SEQUENTIAL',BLANK='NULL'
     1,FORM='FORMATTED',FILE='TEMP.DAT'
     2,STATUS='UNKNOWN')
      REWIND(11)
      READ(UNIT=10,FMT=100,ERR=999,END=200) ITEM
      REWIND(10)
      TOFILE=ITEM(2:13)
      OPEN(UNIT=20,ACCESS='SEQUENTIAL',BLANK='NULL'
     1,FORM='FORMATTED',FILE=TOFILE
     2,STATUS='UNKNOWN')
100   FORMAT(A80)
101   FORMAT('*',A79)
                        NLINES=0
                        DO I=1,99999
      READ(UNIT=10,FMT=100,ERR=999,END=200) ITEM
      ITEM=TRIM(ITEM)
                       DO J=1,20
      IF(ITEM(1:1).EQ.' ') ITEM(1:80)=ITEM(2:80)//' '
                       END DO
      WRITE(UNIT=11,FMT=100) ITEM
                        NLINES=I
                        END DO
200                     CONTINUE
      REWIND(10)
      REWIND(11)
C     WE NOW HAVE NLINES IN THE FROMFILE, PROCESS THEM
                        DO I=1,NLINES
      READ(11,100,END=2000) ITEM
      IF(ITEM(1:1).EQ.'!') THEN
      WRITE(20,101) ITEM(2:80)
                        ELSE
C     NOT A COMMENT
                        DO J=1,20
                IF(ITEM(J:J).EQ.' ') THEN
                A1TAG=J-1
                        GO TO 300
                        END IF
300                     CONTINUE
                        END DO
      BACKSPACE(11)
      A1=AA
      R1=0.0
      D1=0.0
      FLAG1=0
      READ(11,*,END=2000) A1(1:A1TAG),R1,D1,FLAG1
      IF(FLAG1.EQ.0) TYPE='CV/CX'
      IF(FLAG1.EQ.1) TYPE='__/CX'
      IF(FLAG1.EQ.-1) TYPE='CV/__'
      WRITE(20,*)R1,' ',D1,' ',FLAG1,' ',TYPE
                        END IF
                        END DO
2000  CONTINUE
      REWIND(10)
      REWIND(11)
      REWIND(20)
      CLOSE(10,STATUS='KEEP')
      CLOSE(11,STATUS='DELETE')
      CLOSE(20,STATUS='KEEP')
                        STOP
999   PRINT*,'ERROR IN FILE READ, FIX IT AND CONVERT AGAIN'
      CLOSE(10,STATUS='KEEP')
      CLOSE(11,STATUS='DELETE')
      CLOSE(20,STATUS='KEEP')
                        STOP
      END