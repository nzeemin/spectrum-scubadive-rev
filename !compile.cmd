@echo off
if exist scubacoda.bin del scubacoda.bin
if exist scubacoda.txt del scubacoda.txt
if exist scubacodb.bin del scubacodb.bin
if exist scubacodb.txt del scubacodb.txt
if exist scubacodc.bin del scubacodc.bin
if exist scubacodc.txt del scubacodc.txt
if exist scuba.tap del scuba.tap

rem Define ESCchar to use in ANSI escape sequences
rem https://stackoverflow.com/questions/2048509/how-to-echo-with-different-colors-in-the-windows-command-line
for /F "delims=#" %%E in ('"prompt #$E# & for %%E in (1) do rem"') do set "ESCchar=%%E"

@echo on
tools\bas2tap.exe -a1 -sSCUBA basloader.bas basloader.tap
@if errorlevel 1 goto Failed
tools\pasmo scubacoda.asm scubacoda.bin scubacoda.txt
@if errorlevel 1 goto Failed
tools\pasmo scubacodb.asm scubacodb.bin scubacodb.txt
@if errorlevel 1 goto Failed
tools\pasmo scubacodc.asm scubacodc.bin scubacodc.txt
@if errorlevel 1 goto Failed
@echo off

dir /-c scubacod?.bin|findstr /R /C:"scubacod"

tools\bin2tap scubacoda.bin -o scubacoda.tap -a 16384
@if errorlevel 1 goto Failed

tools\bin2tap scubacodb.bin -o scubacodb.tap -a 24576
@if errorlevel 1 goto Failed

tools\bin2tap scubacodc.bin -o scubacodc.tap -a 55552
@if errorlevel 1 goto Failed

@echo on
copy /B /Y basloader.tap+scubacoda.tap+scubacodb.tap+scubacodc.tap scuba.tap
@if errorlevel 1 goto Failed
@echo off

echo %ESCchar%[92mSUCCESS%ESCchar%[0m
exit

:Failed
@echo off
echo %ESCchar%[91mFAILED%ESCchar%[0m
exit /b
