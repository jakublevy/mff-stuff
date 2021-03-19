@echo off
:: Load compilation environment
call "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\bin\vcvars32.bat"
:: Invoke compiler with any options passed to this batch file
"C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\bin\cl.exe" %*