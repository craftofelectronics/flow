@ECHO OFF

echo. Setting up variables

set src=C:\Documents and Settings\Administrator\My Documents\GitHub\flow
set build=%src%\build
set ddname=flow
set dest=%src%\build\%ddname%
set raco="C:\Program Files\Racket\raco.exe"
set zip="C:\Program Files\7-Zip\7z.exe"

REM REMOVE THE DESTINATION IF IT EXISTS
cd "%build%"
rm -rf %ddname%

REM MAKE DESTINATION DIRECTORY
echo. Making destination directories.
mkdir "%dest%"

REM MAKE TEMP DIRECTORY IN DESTINATION
mkdir "%dest%\temp"

REM BUILD EXECUTABLE
echo. Building executable.
cd %build%
%raco% exe -o flow.exe "%src%\server\gui.rkt"

REM MAKE IT DISTRIBUTABLE
echo. Making executable distributable.
%raco% distribute "%dest%" flow.exe

REM COPY NEEDED DIRECTORIES
call:xcopy_sd bin
call:xcopy_sd interface
call:xcopy_sd occam

REM ZIP EVERYTHING
echo. Zipping things up.
%zip% a -r %ddname%.zip %ddname%

GOTO:EOF

REM End of script
goto:eof

:xcopy_sd
  echo. Copying %1
  xcopy /S /F /Y /I "%src%\%1" "%dest%\%1"
GOTO:EOF