/*
Autor: Jakub Levý
Vytvořeno pro DBA @ MFF ZS 19/20
*/

USE Levý_Fotbal 
GO

drop FUNCTION dbo.Soubor_Existuje
drop assembly CLR

drop function dbo.Aktuální_Sezóna
drop function dbo.Doma_Venku
drop function dbo.Ml_Kategorie_Formátované
drop function dbo.Bit_Pohlaví
drop function dbo.Má_Platné_Hostování
drop function dbo.Počet_Zápisů_Na_Soupisce
drop function dbo.Počet_Zápisů_Na_Soupisce_Náhradník
drop function dbo.Počet_Zápisů_Na_Soupisce_V_Základu
drop function dbo.Sídlí_Klub
drop function dbo.Soupiska_Počet_Lidí
drop function dbo.Tel_Číslo
drop function dbo.Urči_Ml_Kategorii
drop function dbo.Urči_Ml_Kategorii_Pohlaví
drop function dbo.Vlastním_Hráče
drop function dbo.Skóre
drop function dbo.Ml_Kategorie_Pohlaví
drop function dbo.Pohlaví_Bit

--tyto zbyli v DB - jsou nutné pro constraints tabulek
--jejich smazani je ve skriptu 7. zruseni_db_tabulky

--FUNCTION dbo.Je_Adresa_Adresou_Klubu
--FUNCTION dbo.Je_Adresa_Adresou_Utkání
--FUNCTION dbo.Je_Adresa_Využita
--FUNCTION dbo.Věk
--FUNCTION dbo.Je_Kontakt_Hráčem
--FUNCTION dbo.Je_Kontakt_Pověřenou_Osobou_Klubu
--FUNCTION dbo.Je_Kontakt_Rozhodčím
--FUNCTION dbo.Zapsal_Kontakt_Soupisku
--FUNCTION dbo.Je_Kontakt_Využit