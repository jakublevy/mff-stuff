/*
Autor: Jakub Levý
Vytvořeno pro DBA @ MFF ZS 19/20
*/

USE [Levý_Fotbal] 
GO

--Smazání funkcí včetně závislostí
alter table Kontakt
drop column Využit
drop FUNCTION dbo.Je_Kontakt_Využit

drop FUNCTION dbo.Zapsal_Kontakt_Soupisku
drop FUNCTION dbo.Je_Kontakt_Rozhodčím
drop FUNCTION dbo.Je_Kontakt_Pověřenou_Osobou_Klubu
drop FUNCTION dbo.Je_Kontakt_Hráčem

alter table Hráč
drop constraint CK_Hráč_D_Narození
drop FUNCTION dbo.Věk

alter table Adresa
drop column Využita
drop FUNCTION dbo.Je_Adresa_Využita

drop FUNCTION dbo.Je_Adresa_Adresou_Utkání
drop FUNCTION dbo.Je_Adresa_Adresou_Klubu





--Smazání tabulek
drop TABLE dbo.Hráč_Soupiska
drop TABLE dbo.Hostování
drop TABLE dbo.Hráč
drop TABLE dbo.Utkání 
drop TABLE dbo.Ml_Kategorie 
drop TABLE dbo.Soupiska 
drop TABLE dbo.Rozhodčí 
drop TABLE dbo.Klub 
drop TABLE dbo.Adresa
drop TABLE dbo.Kontakt 
drop TABLE dbo.Tel 
drop TABLE dbo.Sezóna 


--Smazání DB
use [master]

ALTER DATABASE [Levý_Fotbal]
SET SINGLE_USER
WITH ROLLBACK IMMEDIATE;
drop database [Levý_Fotbal] 
GO

--Vypnutí clr
sp_configure 'show advanced options', 1;
GO
RECONFIGURE;
GO
sp_configure 'clr enabled', 0;
GO
RECONFIGURE;
GO
sp_configure 'show advanced options', 0;
GO
RECONFIGURE;