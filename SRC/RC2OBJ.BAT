@ECHO OFF
rem *********************************
rem Automake batch file for compiling
rem program resource script
RC /i\wint\include "C:\winopt\src\prgres.rc"
RES2OBJ "C:\winopt\src\prgres.res" "C:\winopt\src\prgres.obj"
