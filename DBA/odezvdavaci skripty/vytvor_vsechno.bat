:: Autor: Jakub Levý
:: Vytvořeno pro DBA @ MFF ZS 19/20
::
:: Tento skript spustí postupně všechny soubory z adresáře vytvorit/ na lokálním MSSQL serveru 

set "oldpath=%cd%"
cd vytvorit
forfiles /s /m *.sql /c "cmd /c sqlcmd /S . /d master /E /f 65001 /I /m 1 /i @path"
chdir /d %oldpath% 
pause