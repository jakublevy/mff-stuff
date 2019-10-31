/*
Autor: Jakub Levý
Vytvořeno pro DBA @ MFF ZS 19/20
*/

USE [Levý_Fotbal] 
GO

drop statistics Adresa.STAT_Adresa_Ulice
drop statistics Adresa.STAT_Adresa_Č_p 
drop statistics Adresa.STAT_Adresa_Město
drop statistics Adresa.STAT_Adresa_Psč 

drop statistics Hostování.STAT_Hostování_Od
drop statistics Hostování.STAT_Hostování_Do
drop statistics Hostování.STAT_Hostování_Cena_Za_Sezónu

drop statistics Hráč.STAT_Hráč_Muž

drop statistics Hráč_Soupiska.STAT_Hráč_Soupiska_Číslo
drop statistics Hráč_Soupiska.STAT_Hráč_Soupiska_Náhradník

drop statistics Kontakt.STAT_Kontakt_Jméno 
drop statistics Kontakt.STAT_Kontakt_Příjmení

drop statistics Ml_Kategorie.STAT_Ml_Kategorie_Délka_Zápasu_Minut

drop statistics Utkání.STAT_Utkání_Ml_Kategorie_Muži
drop statistics Utkání.STAT_Utkání_Góly_My
drop statistics Utkání.STAT_Utkání_Góly_Soupeř
drop statistics Utkání.STAT_Utkání_Góly_My_Poločas
drop statistics Utkání.STAT_Utkání_Góly_Soupeř_Poločas