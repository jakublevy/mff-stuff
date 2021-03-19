:: Autor: Jakub Levý
:: Vytvořeno pro DBA @ MFF ZS 19/20
::
:: Tento skript spustí postupně všechny soubory z adresáře smazat/ na lokálním MSSQL serveru 

for /f "delims=" %%f in ('dir /b /o:n .\smazat') do sqlcmd /S . /d master /E /f 65001 /I /m 1 /i ".\smazat\%%f"
pause