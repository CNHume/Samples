@echo off
for /f "delims=" %%i in (SolutionProjectFolders.txt) do @CleanProject "%%i"
