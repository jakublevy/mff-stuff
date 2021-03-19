:: Autor: Jakub Levý
:: Vytvořeno pro DBA @ MFF ZS 19/20
::
:: Tento skript spustí postupně všechny soubory z adresáře vytvorit/ na lokálním MSSQL serveru 

for /f "delims=" %%f in ('dir /b /o:n .\vytvorit') do sqlcmd /S . /d master /E /f 65001 /I /m 1 /i ".\vytvorit\%%f"
pause