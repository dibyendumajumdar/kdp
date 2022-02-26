!  edfile
!
!
!     #include "medrc.rc"
!
!  is added to the .RC file for programs which call WEditFile. This is
!  necessary in order for keyboard shortcuts on the File and Search menus
!  to work. You should also ensure that no menu strcutures have an identifier
!  value of 32101 since this will cause a conflict with the accelerator table
!  in editor.rc. This line is not necessary for other compilers, but can be
!  safely included if you need to build your program with multiple compilers.
!
      PROGRAM MEDFILE
!
! Use of the WINTERACTER module is compulsory
!
      USE WINTERACTER
      IMPLICIT NONE
!
      INTEGER  IFLG,ITEM,IW1,I
	LOGICAL EXISF,OPENF
	INCLUDE 'RESOURCE.INC'
!
! Declare window-type and message variables
!
      TYPE(WIN_STYLE)    :: WINDOW
      TYPE(WIN_MESSAGE)  :: MESSAGE,MESSAGEB
      INTEGER            :: ITYPE,JTYPE,IIFONT
!
      CHARACTER(LEN=260) :: FILENAME ! Initial filename
      INTEGER            :: IEDITWIN ! Editor window handle
!
	EXISF=.FALSE.
	OPENF=.FALSE.
	INQUIRE(FILE='FONTSAVE.DAT',EXIST=EXISF)
	INQUIRE(FILE='FONTSAVE.DAT',OPENED=OPENF)	
	IF(EXISF) THEN
      OPEN(UNIT=110,ACCESS='SEQUENTIAL',BLANK='NULL' ,FORM='FORMATTED',FILE='FONTSAVE.DAT',STATUS='UNKNOWN')
      REWIND(UNIT=110)
      READ(110,*) I
      CLOSE(UNIT=110,STATUS='KEEP')
	IF(I.LT.2.OR.I.GT.36) I=8
			ELSE
			I=8
			END IF

! Initialise Winteracter
!
      CALL WInitialise("")
!
!  Create root window. Since we only want the editor visible and since the
!  editor must use a Child window then we will hide the root window.
!
      WINDOW%FLAGS  = HideRoot
      WINDOW%X      = -1
      WINDOW%Y      = -1
      WINDOW%WIDTH  = 0
      WINDOW%HEIGHT = 0
      WINDOW%MENUID = 0
      WINDOW%TITLE  = ""
!
      CALL WindowOpen(WINDOW)
      ITEM=INFOOPSYSTEM(16)
!
!  Now open a Child window. This will be converted into a text editor later.
!  The Flags specified for this window control certain aspects of the editor,
!  such as the presence or absence of a Status Bar.
!
      WINDOW%FLAGS  = SysMenuOn + MinButton + MaxButton + StatusBar
      WINDOW%TITLE  = "KDP-2 Macro Editor"
!
      CALL WindowOpenChild(WINDOW,IEDITWIN)
!
!  Check command line for a filename.
!
      CALL IOsArgument(1,FILENAME)
!
!  Now convert the Child window into a text editor. We will start with no
!  file loaded.
!  Since we want to extend the editor's menu we must use a Modeless or
!  Semi-Modeless editor.
         OPEN(UNIT=86,FILE='EDITTING.DAT',STATUS='UNKNOWN')
         WRITE(86,*) 'HI'
         CLOSE(UNIT=86,STATUS='KEEP')
!
      IFLG=256+512
      FILENAME(1:260)=FILENAME(2:260)//' '
      CALL WEditFile(FILENAME,Modeless,IDR_MENU1,IFLG)
	CALL WEditFont(4,I)

!  Main message loop
!
      DO                                 ! Loop until user terminates
          CALL WMessage(ITYPE, MESSAGE)
          SELECT CASE (ITYPE)
          CASE (MenuSelect)

!  Menu item selected. This message reports both our extensions to the menu
!  and certain options from the editor's built in menu. In the case of the
!  built in options the editor will already have perfomed the appropriate
!  action and the message is for information purposes only.
!      
                      
          IF (MESSAGE%VALUE1 == ID_HELP_ABOUT) THEN
          CALL WDialogLoad(IDD_ABOUT)
          CALL WDialogShow(-1,-1,0,Modal)
                                        END IF
          IF (MESSAGE%VALUE1 == ID_ITEM4) THEN
          IF(ITEM.EQ.3)&
          CALL IOsCommand('START MACPRMPT.PDF',1)
          IF(ITEM.EQ.4)&
          CALL IOsCommand('CMD.EXE /c MACPRMPT.PDF',1)
                                        END IF
          IF (MESSAGE%VALUE1 == ID_HELP_MANUAL) THEN
          IF(ITEM.EQ.3)&
          CALL IOsCommand('START MANUAL.PDF',1)
          IF(ITEM.EQ.4)&
          CALL IOsCommand('CMD.EXE /c MANUAL.PDF',1)
                                        END IF


          CASE (CloseRequest)
              CALL WindowCloseChild(IEDITWIN)
         OPEN(UNIT=86,FILE='EDITTING.DAT',STATUS='UNKNOWN')
         CLOSE(UNIT=86,STATUS='DELETE')
         OPEN(UNIT=88,FILE='RESOLVE.DAT',STATUS='UNKNOWN')
         WRITE(88,*) 'HI'
         CLOSE(UNIT=88,STATUS='KEEP')
              EXIT
          END SELECT
      END DO
!
!  Terminate Winteracter
!
      CALL WindowClose()
      STOP
      END PROGRAM MEDFILE
