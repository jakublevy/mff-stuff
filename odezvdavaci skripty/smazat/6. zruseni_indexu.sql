/*
Autor: Jakub Levý
Vytvořeno pro DBA @ MFF ZS 19/20
*/

USE Levý_Fotbal 
GO

drop INDEX IX_Hostování_Členství_Id ON dbo.Hostování 
drop INDEX IX_Hostování_Hráč_Reg_Id ON dbo.Hostování 
drop INDEX IX_Hráč_Soupiska_Hráč_Reg_Id ON dbo.Hráč_Soupiska 
drop INDEX IX_Hráč_Soupiska_Soupiska_Id ON dbo.Hráč_Soupiska  
drop INDEX IX_Klub_Pověřená_Osoba_Id ON dbo.Klub
drop INDEX UQ_Kontakt_Email ON dbo.Kontakt
drop INDEX UQ_Kontakt_Tel ON dbo.Kontakt
drop INDEX IX_Sezóna_Konec_Rok ON dbo.Sezóna
drop INDEX IX_Sezóna_Start_Rok ON dbo.Sezóna
drop INDEX IX_Soupiska_Zapsal_Id ON dbo.Soupiska
drop INDEX IX_Utkání_Místo_Konání_Id ON dbo.Utkání
drop INDEX IX_Utkání_Ml_Kategorie ON dbo.Utkání 
drop INDEX IX_Utkání_Rozhodčí_Id ON dbo.Utkání 
drop INDEX IX_Utkání_Sezóna_Start ON dbo.Utkání 
drop INDEX IX_Utkání_Soupeř_Id ON dbo.Utkání 
drop INDEX IX_Utkání_Soupiska_Id ON dbo.Utkání 