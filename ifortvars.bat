@echo off
Rem Intel(R) Visual Fortran Compiler Build Environment for 32-bit applications

echo.
echo Intel(R) Visual Fortran Compiler 9.1.024 Build Environment for 32-bit applications
echo Copyright (C) 1985-2006 Intel Corporation. All rights reserved.
echo.

@call "C:\Program Files\Microsoft Visual Studio 8\VC\Bin\Vcvars32.bat"
title Intel(R) Visual Fortran Compiler 9.1.024 Build Environment for 32-bit applications

echo.

SET IFORT_COMPILER91=C:\Program Files\Intel\Compiler\Fortran\9.1

SET INTEL_SHARED=C:\Program Files\Common Files\Intel\Shared Files

SET INTEL_LICENSE_FILE=C:\Program Files\Common Files\Intel\Licenses

SET PATH=%IFORT_COMPILER91%\Ia32\Bin;%INTEL_SHARED%\Ia32\Bin;%PATH%

SET LIB=C:\Program Files\Intel\Compiler\Fortran\9.1\IA32\LIB;C:\Program Files\Microsoft Visual Studio 8\VC\lib;%IFORT_COMPILER91%\Ia32\Lib;%LIB%

SET INCLUDE=%IFORT_COMPILER91%\Ia32\Include;%INCLUDE%

if exist "C:\Program Files\Intel\Compiler\Fortran\9.1\ia32\Bin\imsl.bat" call "C:\Program Files\Intel\Compiler\Fortran\9.1\ia32\Bin\imsl.bat"

