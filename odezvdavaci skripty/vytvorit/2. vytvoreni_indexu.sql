/*
Autor: Jakub Levý
Vytvořeno pro DBA @ MFF ZS 19/20

Všechny indexy s název UQ* (unique keys) byly vytvořeny kvůli logickému významu unikátnosti.
Tyto indexy jsou součástí definic jednotlivých tabulek.
(Až na výjimku UQ_Kontakt_Email a UQ_Kontakt_Tel, {které nejdou/nevím jak} vytvořit při vytváření tabulky)
Např. UQ_Klub_Adresa_Id (Jeden klub má právě jednu adresu)
Zároveň tyto indexy jsou využity některými dotazy, které jsou součásti DB (pohledy, procedůry, ...)

Všechny ostatní indexy (nacházející se v tomto souboru) byly vytvořeny z důvodů optimalizace dotazů. (Nikoliv foreign key = index).
Zbytečný index se musí neustále aktualizovat a neměl by žádné využití. Mimo jiné pokud by se naskytl dotaz, který by
potřeboval nový index a na jehož rychlosti by záleželo, není problém takový index přidat.
*/

USE Levý_Fotbal 
GO

CREATE NONCLUSTERED INDEX IX_Hostování_Členství_Id ON dbo.Hostování(Členství_Id ASC) 
	INCLUDE (	Hráč_Reg_Id
	          , Od
			  , Do
			  , Cena_Za_Sezónu
		    )

CREATE NONCLUSTERED INDEX IX_Hostování_Hráč_Reg_Id ON dbo.Hostování(Hráč_Reg_Id ASC) 
	INCLUDE (	Od
			  , Do
		    )


CREATE NONCLUSTERED INDEX IX_Hráč_Soupiska_Hráč_Reg_Id ON dbo.Hráč_Soupiska(Hráč_Reg_Id) 
CREATE NONCLUSTERED INDEX IX_Hráč_Soupiska_Soupiska_Id ON dbo.Hráč_Soupiska(Soupiska_Id) INCLUDE(Náhradník) 

CREATE NONCLUSTERED INDEX IX_Klub_Pověřená_Osoba_Id ON dbo.Klub(Pověřená_Osoba_Id) 

--Jeden kontakt má unikátní email a telefon, s výjimkou NULL
CREATE UNIQUE NONCLUSTERED INDEX UQ_Kontakt_Email ON dbo.Kontakt(Email) WHERE (Email IS NOT NULL)
CREATE UNIQUE NONCLUSTERED INDEX UQ_Kontakt_Tel ON dbo.Kontakt(Tel_Id) WHERE (Tel_Id IS NOT NULL)

CREATE NONCLUSTERED INDEX IX_Sezóna_Konec_Rok ON dbo.Sezóna(Konec_Rok)
CREATE NONCLUSTERED INDEX IX_Sezóna_Start_Rok ON dbo.Sezóna(Start_Rok)

CREATE NONCLUSTERED INDEX IX_Soupiska_Zapsal_Id ON dbo.Soupiska(Zapsal_Id)

CREATE NONCLUSTERED INDEX IX_Utkání_Místo_Konání_Id ON dbo.Utkání(Místo_Konání_Id) 
CREATE NONCLUSTERED INDEX IX_Utkání_Ml_Kategorie ON dbo.Utkání(Ml_Kategorie_Název) INCLUDE(Ml_Kategorie_Muži)
CREATE NONCLUSTERED INDEX IX_Utkání_Rozhodčí_Id ON dbo.Utkání(Rozhodčí_Id)
CREATE NONCLUSTERED INDEX IX_Utkání_Sezóna_Start ON dbo.Utkání(Sezóna_Start)

CREATE NONCLUSTERED INDEX IX_Utkání_Soupeř_Id ON dbo.Utkání(Soupeř_Id) 
	INCLUDE (	Góly_My
			  , Góly_Soupeř
	          , Góly_My_Poločas
	     	  , Góly_Soupeř_Poločas
	     	  , Místo_Konání_Id
	     	  , Sezóna_Start
	     	  , Rozhodčí_Id
	     	  , Soupiska_Id
	     	  , Ml_Kategorie_Název
	     	  , Ml_Kategorie_Muži
		    ) 

CREATE NONCLUSTERED INDEX IX_Utkání_Soupiska_Id ON dbo.Utkání(Soupiska_Id)