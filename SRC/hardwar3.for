      SUBROUTINE SETTEXTSCREEN
C     THIS SETS UP AND INITIALIZES THE SCROLLING WINDOW WHEN THE PROGRAM
C     STARTS WHEN NOT STARTED IN "BATCH" MODE
C
C     Use of the WINTERACTER module is compulsory
C
      USE WINTERACTER
C
      IMPLICIT NONE
C
      CHARACTER KKDP*3
C
      INCLUDE 'RESOURCE.INC'
C
C  Parameters for new line and tab codes in editor
C
      CHARACTER(LEN=2), PARAMETER :: NEWLIN = CHAR(13)//CHAR(10)
      CHARACTER(LEN=1), PARAMETER :: TAB    = CHAR(9)
C
C Declare window-type and message variables
C
      TYPE(WIN_STYLE)     :: WINDOW
      TYPE(WIN_STYLE)     :: WINDOW1
      TYPE(WIN_MESSAGE)   :: MESSAGE
      INTEGER             :: ITYPE,I
      LOGICAL EXISF,OPENF
C
C Entered commands and displayed output
C
      CHARACTER(LEN=1024) :: COMMAND,OUTPUT
C
      INTEGER             :: IPOS
      INTEGER             :: IEDITWIN
C

C Initialise Winteracter
C
      CALL WInitialise('WINTER.INI')
C
C  Create root window. Since we only want the editor window visible and since
C  the editor must use a Child window then we will hide the root window.
C
      WINDOW%FLAGS  = HideRoot
c     WINDOW%X      = -1
c     WINDOW%Y      = -1
c     WINDOW%WIDTH  = -1
c     WINDOW%HEIGHT = -1
c     WINDOW%MENUID = 0
c     WINDOW%TITLE  = ' '
C
        KKDP='CMD'
C
      CALL WindowOpen(WINDOW)
C
C  Now open a Child window. This will be converted into a text editor later.
C  The Flags specified for this window control certain aspects of the editor,
C  such as the presence or absence of a Status Bar.
C
      WINDOW1%FLAGS=SysMenuOn + MinButton + MaxButton
      WINDOW1%X      = -1
      WINDOW1%Y      = 0
      WINDOW1%WIDTH  = -1
      WINDOW1%HEIGHT = -1
      WINDOW1%TITLE=
     1'Klein''s Optical Design Program - 2,  (KDP-2)'

      CALL WindowOpenChild(WINDOW1,IEDITWIN)
C
C  Now convert the Child window into a text browser.
C  Since we want the editor window to have a command line we must must use a
C  Modeless or Semi-Modeless editor.
C
      CALL WEditFile(' ',
     1Modeless,
     2IDR_MENU1,
     3ViewOnly+WordWrap+NoFileNewOpen+CommandHistory,
     4SystemFixed)
      EXISF=.FALSE.
      OPENF=.FALSE.
      INQUIRE(FILE='FONTSAVE.DAT',EXIST=EXISF)
      INQUIRE(FILE='FONTSAVE.DAT',OPENED=OPENF)
      IF(EXISF) THEN
      OPEN(UNIT=110,ACCESS='SEQUENTIAL',BLANK='NULL' ,
     1FORM='FORMATTED',FILE='FONTSAVE.DAT',STATUS='UNKNOWN')
      REWIND(UNIT=110)
      READ(110,*) I
      CLOSE(UNIT=110,STATUS='KEEP')
      IF(I.LT.2.OR.I.GT.36) I=8
      ELSE
      I=8
      END IF
      CALL WEditFont(4,I)
      CALL SAVEFONT(I)
C
C  Add prompt to command line
C
      CALL WEditPrompt(KKDP)
C
C  Place a default command into the editor's command line
C
      CALL WEditPutCommand('')
C     Load and display About box
C
                        RETURN
                        END
