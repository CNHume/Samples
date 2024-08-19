@echo off
if "%~1" equ "" goto FolderNameEmpty
@echo Cleaning %1
if exist %1\bin rmdir /s /q %1\bin
if exist %1\obj rmdir /s /q %1\obj
exit /b
:FolderNameEmpty
@echo Project folder must be specified.
