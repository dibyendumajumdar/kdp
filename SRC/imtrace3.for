C
C       IMTRACE3
        IF(WC.EQ.'IMTRACE3') THEN
        IF(SQ.NE.0.OR.SST.NE.0.OR.SN.EQ.1) THEN
        WRITE(OUTLYNE,*)
     1  WC,' TAKES NO ADDITIONAL INPUT'
        CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
        NCRAYS=INT(DSQRT(W1))
        IF(SYSTEM(30).GT.2.0D0) THEN
        WRITE(OUTLYNE,*)
     1  WC,' REQUIRES "FOCAL" OR "UFOCAL" MODE'
        CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
        IF(OBJNX.EQ.0.OR.OBJNY.EQ.0) THEN
        WRITE(OUTLYNE,*) 'NO OBJECT PLANE ARRAY EXISTS'
        CALL SHOWIT(1)
        WRITE(OUTLYNE,*) 'NO IMAGE TRACES CAN BE PERFORMED'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
         IF(IMGNX.EQ.0.OR.IMGNY.EQ.0) THEN
         WRITE(OUTLYNE,*) 'NO IMAGE PLANE ARRAY EXISTS'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*) 'NO IMAGE TRACES CAN BE PERFORMED'
         CALL SHOWIT(1)
                         CALL MACFAL
                         RETURN
                         END IF
C
C       CREATE A PSF AT THE CONTROL WAVELENGTH FROM THE ON-AXIS
C       OBJECT POSITION
C
C       INDEX CREATION
C       INDEX 1,1 IS LOCATED AT THE -X,-Y LIMIT OF THE
C       CENTRAL HALF OF THE IMAGE X AND Y ARRAYS
C
        WC='FOB'
        SQ=0
        SST=0
        STI=0
        DF1=0
        DF2=0
        DF3=0
        DF4=0
        DF5=1
        SN=1
        S1=1
        S2=1
        S3=1
        S4=1
        S5=0
        W3=0.0D0
        XCORR=IIMAGEX2(1,1)-(IDELX/2.0)
        YCORR=IIMAGEY2(1,1)-(IDELY/2.0)

                        SYSSUM=0.0D0
                        DO K=1,NUMCOLORS
                SYSSUM=SYSSUM+SYSTEM(30+K)
                        END DO
                SYSSUM=SYSSUM/DBLE(K)
                        DO K=1,NUMCOLORS
        WRITE(OUTLYNE,*) 'TRACING RAYS I COLOR # ',K
        CALL SHOWIT(1)
                        DO J=1,OBJNY
                        DO I=1,OBJNX

        IF(K.EQ.1) WAVLN=SYSTEM(7)
        IF(K.EQ.2) WAVLN=SYSTEM(11)
        IF(K.EQ.3) WAVLN=SYSTEM(8)
        IF(K.EQ.1) KKK=3
        IF(K.EQ.2) KKK=1
        IF(K.EQ.3) KKK=2
        W1=(IOBJECTY(I,J))/DABS(IOBJECTY(1,1))
        W2=(IOBJECTX(I,J))/DABS(IOBJECTX(1,1))
        W4=WAVLN
        CALL FASTFFOB(IOBJECTV(I,J,K))
        X2=REFRY(1,INT(SYSTEM(20)))
        Y2=REFRY(2,INT(SYSTEM(20)))
        SAVE_KDP(26)=SAVEINPT(26)
        INPUT='OUT NULL'
        CALL PROCES
        INPUT='PSFPLOT NO'
        CALL PROCES
        INPUT='PSFWRITE NO'
        CALL PROCES
        WRITE(INPUT,*) 'PSF,,',WAVLN
        CALL PROCES
        WRITE(OUTLYNE,*) 'PSF DONE'
        CALL SHOWIT(1)
        INPUT='PSFPLOT YES'
        CALL PROCES
        INPUT='PSFWRITE YES'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        REST_KDP(26)=RESTINPT(26)
C       NEGATIVE CORNER OF THE PSF IS:
        PSFXCORNER=-((DBLE(PGR)-1.0D0)/2.0D0)*GRI
        PSFYCORNER=-((DBLE(PGR)-1.0D0)/2.0D0)*GRI
C       APPLI THE STORED PSF
                        DO L=1,PGR
                        DO M=1,PGR
C       REFERENCED TO THE CENTER OF THE IMAGE PLANE (PSF CENTERED ON CHIEF RAY)
        PSFX=X2+(PSFXCORNER+((L-1)*GRI))
        PSFY=Y2+(PSFYCORNER+((M-1)*GRI))
        IX=INT(((PSFX-XCORR)/IDELX)+1)
        IY=INT(((PSFY-YCORR)/IDELY)+1)
        IF(IX.LE.IMGNX.AND.IY.LE.IMGNY.AND.IX.GE.1.AND.
     1  IY.GE.1) THEN
        IIMAGEV(IX,IY,K,1)=IIMAGEV(IX,IY,K,1)+
     1  (FIMG(L,M)*(REFRY(25,INT(SYSTEM(20))))*(SYSTEM(30+KKK)/SYSSUM))
        END IF
                        END DO
                        END DO
                        END DO
                        END DO
                        END DO
         IIMAGEX(1:IMGNX,1:IMGNY)=IIMAGEX2(1:IMGNX,1:IMGNY)
         IIMAGEY(1:IMGNX,1:IMGNY)=IIMAGEY2(1:IMGNX,1:IMGNY)
        PEAKVAL=-1.0D30
        PITVAL=1.0D30
                        DO J=1,IMGNY
                        DO I=1,IMGNX
                        DO M=1,3
        IF(IIMAGEV(I,J,M,1).LT.PITVAL) PITVAL=IIMAGEV(I,J,M,1)
                        END DO
                        END DO
                        END DO
C
                        DO M=1,3
        IIMAGEV(1:IMGNX,1:IMGNY,1:M,1)=IIMAGEV(1:IMGNX,1:IMGNY,1:M,1)
     1  -PITVAL
                        END DO
C
                        DO J=1,IMGNY
                        DO I=1,IMGNX
                        DO M=1,3
        IF(IIMAGEV(I,J,M,1).GT.PEAKVAL) PEAKVAL=IIMAGEV(I,J,M,1)
                        END DO
                        END DO
                        END DO
                        DO J=1,IMGNY
                        DO I=1,IMGNX
                        DO M=1,3
        IIMAGEV(I,J,M,1)=((IIMAGEV(I,J,M,1))/PEAKVAL)*255.0D0
                        END DO
                        END DO
                        END DO
        WRITE(OUTLYNE,*) 'IMTRACE3 TRACING DONE'
        CALL SHOWIT(1)
                        END IF
