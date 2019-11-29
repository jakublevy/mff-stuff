/*
Autor: Jakub Levý
Vytvořeno pro DBA @ MFF ZS 19/20

Tento soubor obsahuje testovací demo předvádějící hlavní funkcionalitu DB.
Předpokládáme, že v DB jsou již testovací data obsažena v souboru '1. testovaci_data.sql'

Přeskočeny byly zejména zřejmé a nezajímavé části (procedůry Přidej_*, Smaž_*, ...)
Přeskočené části jsou buďto zdokumentovány, nebo je jasná jejich funkcionalita z kódu
a žádnou dokumentaci nevyžadují.
*/

USE Levý_Fotbal
GO

--Test constraints

--Constraints tabulky Adresa
--Ani jeden z následujících příkazů pro vložení neprojde přes omezení
insert into Adresa (Ulice, Č_p, Město, Psč) values ('17. listopadu','2k19', 'Praha', '10001')
insert into Adresa (Ulice, Č_p, Město, Psč) values ('$17. listopadu','2/19', 'Praha', '10001')
insert into Adresa (Ulice, Č_p, Město, Psč) values ('17. listopadu','2/19', 'Praha@', '10002')
insert into Adresa (Ulice, Č_p, Město, Psč) values ('17. listopadu','2/19', 'Praha', '00002')

--Tento příkaz projde
insert into Adresa (Ulice, Č_p, Město, Psč) values ('17. listopadu','2/19', 'Praha', '10002')

--Kontrola
select * from Adresa where Id = SCOPE_IDENTITY()

--Constraint tabulky Hostování
declare @machov int
select @machov = Id from Klub where Název = 'VEBA Machov'
--Hostování nemůže skončit dříve než začně
exec Přidej_Hostování '10A45XXX12', '2020-02-10', '2020-01-10', 5000, '1650655', @machov
--Neprošlo

--Constraints tabulky Hráč
--Uvnitř procedůry Přidej_Hráče se nachází transakce, 
--při selhání DB zůstane neupravena díky využítí transakce
--Neprojde - neplatný formát FAČR registračního čísla
exec Přidej_Hráče '1434A59', 'Milan', 'Konečný', 'mužské', '1989-11-05', 'mkonecny@email.cz', '744389520'

--A do DB nebyla žádná osoba se jménem Milan Konečný přidána
select * from Kontakt where Jméno = 'Milan' and Příjmení = 'Konečný'

--Neprojde - hráči musí být alespoň pět let před registrací
exec Přidej_Hráče '1434159', 'Milan', 'Konečný', 'mužské', '2017-11-05', 'mkonecny@email.cz', '744389520'

--Constraints tabulky Hráč_Soupiska
--Neprojde - číslo dresu musí být > 0 a < 100
begin transaction
begin try
	declare @soupiska int
	exec Vytvoř_Prázdnou_Soupisku @soupiska_id = @soupiska output
	exec Přidej_Hráče_Na_Soupisku '1811787', @soupiska, @cislo_dresu = 150, @nahradnik = 0
	commit transaction
end try
begin catch
	--znovu vyhození výjimky, která nastala ve try
    declare @ErrorMessage nvarchar(max), @ErrorSeverity int, @ErrorState int;
    select @ErrorMessage = ERROR_MESSAGE() + ' Line ' + cast(ERROR_LINE() as nvarchar(5)), @ErrorSeverity = ERROR_SEVERITY(), @ErrorState = ERROR_STATE();

	rollback transaction
	raiserror (@ErrorMessage, @ErrorSeverity, @ErrorState);
end catch

--Constraint tabulky Klub
--Neprojde - neplatný název klubu
exec Přidej_Klub '007Kacau007', 'Bří. Čapků', '13', 'Dolany', '30012'

--Constraints tabulky Kontakt
--Neprojde - neplatné jméno/přijmení
exec Přidej_Kontakt 'James 007', 'Bond', 'james@007.uk', '773903128', '+44'
exec Přidej_Kontakt 'James', 'Bond 007', 'james@007.uk', '773903128', '+44'

--Neprojde - email nebo tel. číslo vyžadováno
exec Přidej_Kontakt 'James', 'Bond', @predvolba = '+44'

--Toto již projde
exec Přidej_Kontakt 'James', 'Bond', @tel_cislo = '007007007', @predvolba = '+44' --UK

--Constrains tabulky Ml_Kategorie
--Neprojde - fotbalové utkání nemůže mít 100 minut
insert into Ml_Kategorie (Název, Muži, Délka_Zápasu_Minut) VALUES ('Kojenci', 1, 100)

--Constraints tabulky Sezóna
--Neprojde
exec Přidej_Sezónu '2022-10-23', '2024-01-02' --moc dlouhá sezóna
exec Přidej_Sezónu '2024-09-29', '2023-08-15' --kon < zac

--Constraints tabulky Tel
--Neprojde
exec Přidej_Kontakt 'Jaromír', 'Soukup', 'jarda@barrandov.cz', '77471Z855' --neplatné tel. číslo
exec Přidej_Kontakt 'Jaromír', 'Soukup', 'jarda@barrandov.cz', '774715855', @predvolba = '-420' --neplatná tel. předvolba

--Constraints tabulky Utkání
--Neprojde ani jeden příkaz
exec Přidej_Utkání 101, 5, 1, 1, 1, 1, 'Mladší dorost' --nemůže padnou 101 gólu ve fotbale v rámci jednoho utkání

--O poločase víc gólu než na konci utkání
exec Přidej_Utkání 5, 3, 1, 1, 1, 1, 'Starší žáci', @goly_my_polocas = 2, @goly_souper_polocas = 4 -- 5 : 3 (2 : 4) je neplatné skóre


--Test triggerů

--Trigger na tabulce Hostování
--V DB je hráč s reg. č. '1915044' s platným hostováním od '2013-03-15' do '2013-12-12'
select * from Hráči_Hostování where [Reg. č.] = '1915044'

--Pokusíme se tomuto hráči přidat další hostování, které má neprázdný průnik s již existujícím hostováním
--Příkaz neprojde
declare @c1 int
select @c1 = Id from Klub where Název = 'VEBA Machov'
exec Přidej_Hostování 'JAM1001EX25', '2013-10-27', '2014-07-17', 35000, '1915044', @c1

--Hostování, která mají prázdný průnik však projdou
declare @c2 int
select @c2 = Id from Klub where Název = 'VEBA Machov'
exec Přidej_Hostování 'JAM1001EX25', '2009-10-27', '2011-07-17', 35000, '1915044', @c2

--Přidání můžeme ověřit
select * from Hráči_Hostování where [Reg. č.] = '1915044'

--Trigger Kontrola_Využití_Soupisky na tabulce Hráč_Soupiska
--Vybereme si nějakou soupisku, která je již přiřazena k existujícímu utkání a smažeme z ní všechny hráče
--Logicky vznikne soupiska, která nemůže být přiřazena k žádnému utkání
--Následující příkazy neprojdou
declare @vyuzita_soupiska int
select top 1 @vyuzita_soupiska = Soupiska_Id from Utkání 
delete from Hráč_Soupiska where Soupiska_Id = @vyuzita_soupiska

--Trigger Kontrola_Soupisky na tabulce Hráč_Soupiska
--duplicitní dresy
declare @ns int
exec Vytvoř_Prázdnou_Soupisku @soupiska_id = @ns output
--oba hráčí mají číslo dresu 5, toto neprojde
exec Přidej_Hráče_Na_Soupisku '0001820', @ns, 5, 0
exec Přidej_Hráče_Na_Soupisku '0004530', @ns, 5, 0

--Na druhou stranu toto již projde
declare @ns2 int
exec Vytvoř_Prázdnou_Soupisku @soupiska_id = @ns2 output
--jeden hráč má číslo dresu 5 a druhý 6
exec Přidej_Hráče_Na_Soupisku '0001820', @ns2, 5, 0
exec Přidej_Hráče_Na_Soupisku '0004530', @ns2, 6, 0

--Kontrola
select * from Hráči_Na_Soupisce where [Id soupisky] in (select max(Id) from Soupiska)

--Maximální počet náhradníku (7)
--Pokusíme se přidat 8 náhradníků na novou soupisku
declare @ns3 int
exec Vytvoř_Prázdnou_Soupisku @soupiska_id = @ns3 output
exec Přidej_Hráče_Na_Soupisku '0001820', @ns3, 1, 1
exec Přidej_Hráče_Na_Soupisku '0004530', @ns3, 2, 1
exec Přidej_Hráče_Na_Soupisku '0033513', @ns3, 3, 1
exec Přidej_Hráče_Na_Soupisku '0046815', @ns3, 4, 1
exec Přidej_Hráče_Na_Soupisku '0095357', @ns3, 5, 1
exec Přidej_Hráče_Na_Soupisku '0102630', @ns3, 6, 1
exec Přidej_Hráče_Na_Soupisku '0471068', @ns3, 7, 1
exec Přidej_Hráče_Na_Soupisku '0434013', @ns3, 8, 1 --Tento již má smůlu, prošlo jich prvních 7

--Kontrola
select * from Hráči_Na_Soupisce where [Id soupisky] IN (select max(Id) from Soupiska)

--Kontrola výskytu hráčů jedné kategorie (věk, pohlaví)

--Hráč s reg. č. '0095357' spadá do kategorie Muži
--Hráč s reg. č. '1811787' spadá do kategorie Starší dorost
--Kontrola
select * from Hráči where [Reg. č.] = '0095357' or [Reg. č.] = '1811787'

--Pokusíme se je přidat na jednu soupisku (neprojde)
declare @ns4 int
exec Vytvoř_Prázdnou_Soupisku @soupiska_id = @ns4 output
exec Přidej_Hráče_Na_Soupisku '0095357', @ns4, 5, 0
exec Přidej_Hráče_Na_Soupisku '1811787', @ns4, 10, 0

--Na soupisku se přidal pouze první hráč, ten určil kategorii soupisky
select * from Hráči_Na_Soupisce where [Id soupisky] IN (select max(Id) from Soupiska)

--Kontrola maximálního počtu hráčů na soupisce (18)
--Nejprve budeme potřebovat přidat další hráče do DB
--Pokusíme se přidat > 18 hráčů novou soupisku (přesněji řečeno 20 hráčů)
exec Přidej_Hráče '3128202', 'Kamil', 'Brodský', 'mužské', '1990-10-01', 'kamil@brodak.cz', '601958933'
exec Přidej_Hráče '4328223', 'Roman', 'Loskot', 'mužské', '1993-04-22', 'roman.loskot@uhk.cz', '894453270'
exec Přidej_Hráče '5163443', 'Radek', 'Cvejn', 'mužské', '1989-07-31', 'radek.cvejn@uhk.cz', '123421343'
exec Přidej_Hráče '1299451', 'Yoshiko', 'Nagai', 'mužské', '1991-03-22', 'sekai.wa.nagai@hontoni.jp', '478439098', '+81'

declare @ns5 int
exec Vytvoř_Prázdnou_Soupisku @soupiska_id = @ns5 output
exec Přidej_Hráče_Na_Soupisku '3128202', @ns5, 1, 0
exec Přidej_Hráče_Na_Soupisku '4328223', @ns5, 2, 0
exec Přidej_Hráče_Na_Soupisku '5163443', @ns5, 3, 0
exec Přidej_Hráče_Na_Soupisku '1299451', @ns5, 4, 0
exec Přidej_Hráče_Na_Soupisku '0001820', @ns5, 5, 0
exec Přidej_Hráče_Na_Soupisku '0004530', @ns5, 6, 0
exec Přidej_Hráče_Na_Soupisku '0033513', @ns5, 7, 0
exec Přidej_Hráče_Na_Soupisku '0046815', @ns5, 8, 0
exec Přidej_Hráče_Na_Soupisku '0095357', @ns5, 9, 0
exec Přidej_Hráče_Na_Soupisku '0102630', @ns5, 10, 0
exec Přidej_Hráče_Na_Soupisku '0471068', @ns5, 11, 0
exec Přidej_Hráče_Na_Soupisku '0434013', @ns5, 12, 1
exec Přidej_Hráče_Na_Soupisku '0374901', @ns5, 13, 1
exec Přidej_Hráče_Na_Soupisku '0549405', @ns5, 14, 1
exec Přidej_Hráče_Na_Soupisku '0593646', @ns5, 15, 1
exec Přidej_Hráče_Na_Soupisku '0645778', @ns5, 16, 1
exec Přidej_Hráče_Na_Soupisku '0668429', @ns5, 17, 1
exec Přidej_Hráče_Na_Soupisku '0996668', @ns5, 18, 1
exec Přidej_Hráče_Na_Soupisku '1094895', @ns5, 19, 1 --má smůlu, na soupisku se již nevejde
exec Přidej_Hráče_Na_Soupisku '1937981', @ns5, 20, 1 --má smůlu, na soupisku se již nevejde

--Kontrola, na soupisce je 18 hráčů, poslední dva se na ní nevyskytují
select * from Hráči_Na_Soupisce where [Id soupisky] IN (select max(Id) from Soupiska)

--Kontrola, že poslední dva hráči tam opravdu nejsou
select * from Hráči_Na_Soupisce where [Id soupisky] IN (select max(Id) from Soupiska) and ([Reg. č.] = '1094895' or [Reg. č.] = '1937981')


--Trigger na tabulce Klub
--Na adrese klubu VEBA Machov se odehrály dvě utkání
select * from Klub_Adresa_Počet_Utkání where [Název klubu] = 'VEBA Machov'

--Vzhledem k tomu by z DB nemělo být možné odstranit klub VEBA Machov
--A také není, následující dotaz neprojde
delete from Klub where Název = 'VEBA Machov'
--(Error, který dostaneme nebude ten z triggeru, 
--protože klub VEBA Machov také nelze odstranit, 
--protože existuje utkání, v kterém je VEBA Machov soupeřem)
--Pokud by se však utkání odehrálo na adrese klubu, jenž není soupeřem
--v žádném utkání, pak bychom dostali error z triggeru - kód je jednoduchý
--a je zřejmé, že implementace funguje

--Trigger Smaž_Tel na tabulce Kontakt
--Pokud jsme spustili všechny předchozí příklady, které měli projít
--měli bychom v DB mít jeden neužitečný kontakt (James Bond)
select * from Nevyužité_Kontakty

--Pokud chybí, tak spustit: exec Přidej_Kontakt 'James', 'Bond', @tel_cislo = '007007007', @predvolba = '+44'
--V tabulce Tel je uloženo telefonní číslo Jamese Bonda
select * from Tel where Předvolba = '+44'

--Smažeme Jamese Bonda z DB
delete from Kontakt where Jméno = 'James' and Příjmení = 'Bond'
--Ve výpisu:
--(1 row affected)
--(1 row affected)
--Smazal se kontakt i jeho telefonní informace

--Kontrola
select * from Tel where Předvolba = '+44'

--Trigger Smaž_Tel_Pokud_Změněn na tabulce Kontakt
--Vraťme se do původní situace, přidejme si opět Jamese Bonda do DB
exec Přidej_Kontakt 'James', 'Bond', @tel_cislo = '007007007', @predvolba = '+44'

--Opět se v tabulce Tel nachází jeho telefonní číslo
select * from Tel where Předvolba = '+44'

--Nyní se ho pokusíme změnit na nové číslo (divným způsobem, aby staré číslo zůstalo v DB)
insert into Tel (Předvolba, Číslo) VALUES ('+44', '111111111')
update Kontakt SET Tel_Id = SCOPE_IDENTITY() where Jméno = 'James' and Příjmení = 'Bond'

--Opět byli upraveny 2 řádky v DB
--Staré číslo by již nebylo vázáno na existující Kontakt a tak bylo smazáno z DB
select * from Tel where Předvolba = '+44'

--Trigger na tabulce Utkání
--Nemůžeme vytvořit utkání se soupiskou hráčů, která neobsahuje alespoň minimální počet hráčů (7)
--Vytvoříme si soupisku s jenom šesti hráči a zkusíme vytvořit utkání
declare @ns6 int
exec Vytvoř_Prázdnou_Soupisku @soupiska_id = @ns6 output
exec Přidej_Hráče_Na_Soupisku '3128202', @ns6, 1, 0
exec Přidej_Hráče_Na_Soupisku '4328223', @ns6, 2, 0
exec Přidej_Hráče_Na_Soupisku '5163443', @ns6, 3, 0
exec Přidej_Hráče_Na_Soupisku '1299451', @ns6, 4, 0
exec Přidej_Hráče_Na_Soupisku '0004530', @ns6, 5, 0
exec Přidej_Hráče_Na_Soupisku '0033513', @ns6, 6, 0
declare @r1 int
select top 1 @r1 = Id from Rozhodčí
declare @s1 int, @a1 int
select @s1 = Id, @a1 = Klub.Adresa_Id from Klub where Název = 'RSCM Rozkoš'
exec Přidej_Utkání 5, 3, @s1, @a1, @r1, @ns6, 'Muži'

--Neprošlo, zkusíme to znovu, pouze s jedním hráčem navíc
declare @sa int 
select @sa = max(Id) from Soupiska
exec Přidej_Hráče_Na_Soupisku '0996668', @sa, 7, 0
declare @r2 int
select top 1 @r2 = Id from Rozhodčí
declare @s2 int, @a2 int
select @s2 = Id, @a2 = Klub.Adresa_Id from Klub where Název = 'RSCM Rozkoš'
exec Přidej_Utkání 5, 3, @s2, @a2, @r2, @sa, 'Muži'
--Prošlo

--Kontrola
select * from Zápas where [Id zápasu] IN (select max(Id) from Utkání)

--Kategorie, pro kterou je určené utkání musí být stejné jako kategorie hráčů na soupisce
--Využijeme validní (čítající pouze 7 hráčů) soupisku z předchozího, jenom zkusíme změnit kategorii z Muži na cokoliv jiného
declare @sb int 
select @sb = max(Id) from Soupiska
declare @r3 int
select top 1 @r3 = Id from Rozhodčí
declare @s3 int, @a3 int
select @s3 = Id, @a3 = Klub.Adresa_Id from Klub where Název = 'RSCM Rozkoš'
exec Přidej_Utkání 5, 3, @s3, @a3, @r3, @sb, 'Mladší žáci'

--Neprošlo protože hráčí na soupisce spadají pod kategorii Muži, což si můžeme ověřit:
select * from Hráči_Na_Soupisce where [Id soupisky] IN (select max(Id) from Soupiska)

--Kontrola, zda-li na adrese, kde se koná utkání sídlí nějaký klub
--Utkání se jinde konat nemůže -- kde nesídlí klub, není hřiště :)
--Vytvoříme si novou adresu, která nebude asociována s žádným klubem a zkusíme na ní vytvořit utkání
declare @a4 int
exec Přidej_Adresu 'Národní', '23', 'Praha', '10002', @adresa_id = @a4 output
declare @sc int 
select @sc = max(Id) from Soupiska
declare @r4 int
select top 1 @r4 = Id from Rozhodčí
declare @s4 int
select @s4 = Id from Klub where Název = 'RSCM Rozkoš'
exec Přidej_Utkání 5, 3, @s4, @a4, @r4, @sc, 'Muži'
--Neprošlo

--Kontrola, zda-li hráči na soupisce mají validní hostování v sezóně utkání
--Opět využijeme předchozí soupisku, jenom jednomu hráči přidáme hostování, které již nebude
--aktuální a zkusíme přidat nové utkání
declare @sol int
select @sol = Id from Klub where Název = 'SK Solnice'
exec Přidej_Hostování 'XC25OOM1010', '2012-05-17', '2016-08-08', 50000, '3128202', @sol
declare @sd int 
select @sd = max(Id) from Soupiska
declare @r5 int
select top 1 @r5 = Id from Rozhodčí
declare @s5 int, @a5 int
select @s5 = Id, @a5 = Klub.Adresa_Id from Klub where Název = 'RSCM Rozkoš'
exec Přidej_Utkání 5, 3, @s5, @a5, @r5, @sd, 'Muži', @sezona_start = '2019-10-08'
--Neprošlo

--Test procedůr

--Hostování_Mateřský_Klub
--Přidáme si do DB dvě nové hráčky, jedné přidáme hostování z TJ Koněpůlek a druhé z SK Solnice
exec Přidej_Hráče '8742367', 'Kamila', 'Smutná', 'ženské', '2006-02-27', 'k.smutna@deckonachod.cz', '847302298'
exec Přidej_Hráče '9732367', 'Anna', 'Kutinová', 'ženské', '2006-05-13', 'anicka@kutinova.cz', '273489478'
declare @par1 int, @sol1 int
select @par1 = Id from Klub where Název = 'TJ Koněpůlky'
select @sol1 = Id from Klub where Název = 'SK Solnice'
exec Přidej_Hostování '45AA502500C', '2012-07-17', '2014-05-15', 5000, '8742367', @par1
exec Přidej_Hostování '2500X50CC12', '2018-03-21', '2021-01-12', 75000, '9732367', @sol1

--Podíváme se na hráče s hostováním pocházející z klubu TJ Koněpůlky
exec Hostování_Mateřský_Klub 'TJ Koněpůlky'
--ve výpisu je Kamila Smutná

--a SK Solnice 
exec Hostování_Mateřský_Klub 'SK Solnice'
--ve výpisu je Anna Kutinová

--Hráči_Podle_Kategorie
--Vrátí všechny hráče spadající pod kategorii Starší přípravka
exec Hráči_Podle_Kategorie 'Starší dorost'

--Neprojde, máme na mysli dívčí nebo chlapeckou kategorii?
exec Hráči_Podle_Kategorie 'Starší přípravka'

--Tohle už projde
exec Hráči_Podle_Kategorie 'Starší přípravka', 'ž'

--Koupit_Hráče
--Z testovacích dat máme v DB Vladana Vomáčku, který má již prošlé hostování
--Tedy soupiska obsahující Vladana nemůže být soupiskou utkání
--Kontrola
select * from Hráči_Prošlé_Hostování

--Avšak bychom, chtěli aby s námi Vladan mohl hrát, štědrá FAČR nám přispěla
--a tak máme na nákup -- koupíme si Vladana
declare @vladan_reg int
select @vladan_reg = [Reg. č.] from Hráči_Prošlé_Hostování where Jméno = 'Vladan' and Příjmení = 'Vomáčka'
exec Koupit_Hráče @vladan_reg

--Nyní již Vladan není v seznamu hráčů s prošlým hostováním
select * from Hráči_Prošlé_Hostování

--Smaž_Nevyužité_Adresy, Smaž_Nevyužité_Kontakty, Smaž_Prázdné_Soupisky
--Procedůry sloužící v vyčištění sirotků z DB
--Nevyužitá adresa = adresa, na které nesídlí klub
--Neyužitý kontakt = kontakt, který nezapsal soupisku, není hráčem, není rozhodčím, ani není správce klubu
--Prázdná soupiska = soupiska neobsahující hráče
--Předvedeme pouze Smaž_Nevyužité adresy (ostatní jsou obdobné, nezajímavé)
--Nejprve se ujistíme se, že v DB nemáme žádné zbytečné adresy
exec Smaž_Nevyužité_Adresy --to poznáme tak, že se nám již žádný sloupec z DB nesmaže po spuštění příkazu

--Nyní přidáme 3 adresy, přičemž jednu z nich asociujeme k novému klubu
exec Přidej_Adresu 'Tyršova', '118/26', 'Broumov', '70003' 
exec Přidej_Adresu 'Pleskotova', '12', 'Nymburk', '53478'
exec Přidej_Klub 'FK Jaroměř', 'Dlouhá', '1055', 'Jaroměř', '58972' --čímž jsme zároveň přidali klub i adresu

--Podíváme se na adresy v DB
select * from Adresa where Ulice = 'Tyršova' or Ulice = 'Pleskotova' or Ulice = 'Dlouhá'
--V computed column Využita ihned vidíme, že adresa klubu FK Jaroměř je využita

--Můžeme alternativně použít pohled a podívat se na nevyužité adresy
select * from Nevyužité_Adresy
--Adresa FK Jaroměře mezi nimi není

--Vyčistíme adresy
exec Smaž_Nevyužité_Adresy

--A zkusíme znovu najít v DB naši trojci adres
select * from Adresa where Ulice = 'Tyršova' or Ulice = 'Pleskotova' or Ulice = 'Dlouhá'
--a zbyla tam jen jediná -- ta patřící klubu Jaroměře

--Alternativně
select * from Nevyužité_Adresy
--Prázdno

--Utkání_Proti
--Podíváme se, na hřišti kterého klubu se odehrálo nejvíce utkání
--(Sice nemusel hrát klub, kterému hriště patří, ale dá se to předpokládat)
select top 1 * from Klub_Adresa_Počet_Utkání
where [Název klubu] <> 'SEPA Žabáci' --vyřadíme naše hriště
order by [Počet utkání na hřišti klubu] desc
--2 zápasy se odehrály na hřišti klubu VEBA Machov

--Teď se podíváme na utkání, která jsme proti tomuto klubu hráli my
declare @kl nvarchar(50)
select top 1 @kl = [Název klubu] from Klub_Adresa_Počet_Utkání
where [Název klubu] <> 'SEPA Žabáci' --vyřadíme naše hriště
order by [Počet utkání na hřišti klubu] desc
exec Utkání_Proti @kl
--Jej, oba dva zápasy jsme hráli my a oba jsme prohráli

--Ještě jeden malý příklad, v testovacích datech jsme přidali dva zápasy proti
--klubu SK Babí (druhý byl odvetný). Podíváme se na všechny utkání proti SK Babí
exec Utkání_Proti 'SK Babí'

--Test funkcí
--Součástí DB je jedna table-valued funkce, ostatní funkce jsou scalar-valued

--Table valued function dbo.Urči_Ml_Kategorii
--Přidáme si hráčku, které je aktuálně 13 let ==> spadá pod kategorii starších žákyň
exec Přidej_Hráče '9837244', 'Jana', 'Nováková', 'ženské', '2006-07-18', 'janca.n@outlook.cz', '748098111'

--Nyní zkusíme zavolat funkci dbo.Urči_Ml_Kategorii, měla by nám vrátít dvojci
-- Název kategorie, bit značící pohlaví
--( Starší žákyně ,       0)
select * from dbo.Urči_Ml_Kategorii_Pohlaví('9837244')

--Následují některé scalar-valued funkce

--dbo.Ml_Kategorie_Formátované
--Vrátí název kategorie (přidá pohlaví do textového řetězce, pokud není zřejmé z názvu kategorie)

declare @kat2 nvarchar(25)
exec @kat2 = dbo.Ml_Kategorie_Formátované 'Mladší přípravka', 1
print @kat2
--OK

declare @kat3 nvarchar(25)
exec @kat3 = dbo.Ml_Kategorie_Formátované 'Mladší přípravka', 0
print @kat3
--OK

declare @kat4 nvarchar(25)
exec @kat4 = dbo.Ml_Kategorie_Formátované 'Mladší žákyně'
print @kat4
--OK

declare @kat1 nvarchar(25)
exec @kat1 = dbo.Ml_Kategorie_Formátované 'Mladší přípravka'
print @kat1
--ERR (děvčata nebo chlapci?)

--dbo.Ml_Kategorie_Pohlaví
--Vrátí bit značící pohlaví kategorie
--(pro kategorie z jejichž názvu není možné zjistit pohlaví nastane výjimka)
select dbo.Ml_Kategorie_Pohlaví('Mladší dorost')
--OK 1

select dbo.Ml_Kategorie_Pohlaví('Ženy')
--OK 0

select dbo.Ml_Kategorie_Pohlaví('Starší přípravka')
--ERR

--dbo.Skóre
--Bude nejprve předvedeno za pomocí pohledu Zápas, který
--pro zobrazení skóre využívá tuto funkci
--Vybereme si utkání proti klubu klubu SK Babí
declare @k_id int
select @k_id = Id from Klub where Název = 'SK Babí'
select * from Utkání where Soupeř_Id = @k_id
select * from Zápas where Soupeř = 'SK Babí'
--Pozn. výstup funkce dbo.Skóre závisí zda-li
--se jedná o utkání odehrané doma nebo venku
--Pro utkání doma platí formát: naše_skóre : skóre_soupeře (naše_skóre_poločas : skóre_soupeře_poločas)
--Pro utkání venku platí formát: skóre_soupeře : naše_skóre (skóre_soupeře_poločas : naše_skóre_poločas)

--Přímé volání funkce dbo.Skóre
--Zápas doma, o poločase jsme vedli 1 : 0, utkání skončilo 5 : 3
select dbo.Skóre('Doma', 5, 3, 1, 0)

--To samé, ale zápas se odehrál venku
select dbo.Skóre('Venku', 5, 3, 1, 0)

--dbo.Tel_Číslo
--Spojí předvolbu telefoního čísla s telefoním číslem samotným
--Pokud je předvolba nevyplněna implicitně se předpokládá česká +420

--Test s předvolbou
select top 1 * from Tel where Předvolba is not null
declare @tel1_id int
select top 1 @tel1_id = Id from Tel where Předvolba is not null
select dbo.Tel_Číslo(@tel1_id)

--Test bez předvolby
select top 1 * from Tel where Předvolba is null
declare @tel2_id int
select top 1 @tel2_id = Id from Tel where Předvolba is null
select dbo.Tel_Číslo(@tel2_id)

--dbo.Urči_Ml_Kategorii
--Vrátí kategorii, pod kterou hráč spadá
--Přidáme si alespoň 3 hráče:
	-- Prvním bude Milan Baroš nar. dne 28. 10. 1981 ==> kategorie muži
	-- Druhý bude Jan Veber, kterému je 12 let ==> kategorie mladší žáci
	-- Třetí bude Marta Jirmanová, které je 16 let ==> kategorie mladší dorostenky
--a zkontrolujeme funkčnost funkce dbo.Urči_Ml_Kategorii
exec Přidej_Hráče '8989123', 'Milan', 'Baroš', 'mužské', '10-28-1981', 'milan.baros@email.cz', '238972939'
exec Přidej_Hráče '8324922', 'Jan', 'Veber', 'mužské', '08-29-2007', 'jan.veber@seznam.cz', '983247892'
exec Přidej_Hráče '2347682', 'Marta', 'Jirmanová', 'ženské', '10-14-2003', 'marta@jermanova.cz', '327962348'

select dbo.Urči_Ml_Kategorii('8989123')
--OK Muži

select dbo.Urči_Ml_Kategorii('8324922')
--OK Mladší žáci

select dbo.Urči_Ml_Kategorii('2347682') 
--OK Mladší dorostenky

--dbo.Věk
--Dostane datum narození, vrátí věk
select dbo.Věk('2011-09-19') --aktuálně 8 let
select dbo.Věk('1959-11-12') --aktuálně 60 let
select dbo.Věk('2019-07-06') --aktuálně 0 let

--dbo.Vlastním_Hráče
--Tuto funkci využívá pohled Hráči_Vlastnění
--Vlastním pouze hráče, kteří nemají žádný záznam (myšleno i prošlý) v tabulce Hostování

--Přidáme si nového hráče a nepřidáme mu žádné hostování
exec Přidej_Hráče '9328417', 'Ferda', 'Mravenec', 'mužské', '1933-01-01', 'f.mravenec@sejkora.cz'

--Ferda Mravenec je mezi mnou vlastněnými hráči
select * from Hráči_Vlastnění where [Reg. č.] = '9328417'

--Jakmile přidáme jakékoliv hostování, přestanu být vlastníkem Ferdy Mravence
declare @lck int
select @lck = Id from Klub where Název = 'Lokomotiva Červený Kostelec'
exec Přidej_Hostování '50BD234DE2525', '2016-09-12', '2018-03-22', 65000, '9328417', @lck

--Kontrola
select * from Hráči_Vlastnění where [Reg. č.] = '9328417'

--Po odstranění hostování jsme opět vlastníkem
--delete from Hostování where Kontrakt_Id = '50BD234DE2525'
--nebo lépe
exec Koupit_Hráče '9328417'

--Kontrola
select * from Hráči_Vlastnění where [Reg. č.] = '9328417'

--Pohledy
--Následují některé pohledy, které ještě nebyly předvedeny při předvedení ostatních částí DB

--(Již byli ukázány)
--Hráči_Hostování
--Hráči_Na_Soupisce
--Hráči
--Klub_Adresa_Počet_Utkání
--Nevyužité_Adresy
--Nevyužité_Kontakty
--Zápas
--Hráči_Prošlé_Hostování
--Hráči_Vlastnění

--Hráči_Číslo
--Hráči s číslem dresu a počtem soupisek s daným číslem
select * from Hráči_Číslo

--Podíváme se třeba na hráče 1094895
select * from Hráči_Číslo where [Reg. č.] = '1094895'
--Marek Hron je zapsán na jedné soupisce s číslem dresu 2 a na druhé soupisce s číslem dresu 15
--Na žádné další soupisce není zapsán

--Vytvoříme dvě nové soupisky, na jednu zapíšeme Marka Hrona s číslem dresu 2 a na druhou
--ho zapíšeme s číslem dresu 77
declare @s6 int, @s7 int
exec Vytvoř_Prázdnou_Soupisku @soupiska_id = @s6 output
exec Vytvoř_Prázdnou_Soupisku @soupiska_id = @s7 output
exec Přidej_Hráče_Na_Soupisku '1094895', @s6, 2, @nahradnik = 0
exec Přidej_Hráče_Na_Soupisku '1094895', @s7, 77, @nahradnik = 1

--Podíváme se opět na Marka Hrona
select * from Hráči_Číslo where [Reg. č.] = '1094895'
--Tentokrát 2x s číslem 2, 1x s číslem 15 a 1x s číslem 77

--Hráči_Platné_Hostování, Hráči_Prošlé_Hostování
--Všichni hráči, kteří mají podle aktuálního času platné / prošlé hostování
select * from Hráči_Platné_Hostování

select * from Hráči_Prošlé_Hostování

--Mělo by být prázdné
select * from Hráči_Platné_Hostování where [Reg. č.] IN (select [Reg. č.] from Hráči_Prošlé_Hostování)

--Mělo by být prázdné
select * from Hráči_Prošlé_Hostování where [Reg. č.] IN (select [Reg. č.] from Hráči_Platné_Hostování)

--Hráči_Počet_Zápisů_Na_Soupisce
--Opět využijeme Marka Hrona
select * from Hráči_Počet_Zápisů_Na_Soupisce where [Reg. č.] = '1094895'
--Počet zápisů = x, Z toho náhradníkem = y

--Přidáme ho na dvě soupisky, jednou do základu a jednou jako náhradníka
declare @s8 int, @s9 int
exec Vytvoř_Prázdnou_Soupisku @soupiska_id = @s8 output
exec Vytvoř_Prázdnou_Soupisku @soupiska_id = @s9 output
exec Přidej_Hráče_Na_Soupisku '1094895', @s8, @cislo_dresu = 11, @nahradnik = 0
exec Přidej_Hráče_Na_Soupisku '1094895', @s9, @cislo_dresu = 17, @nahradnik = 1

--Opět se podíváme, co nám vrátil pohled
select * from Hráči_Počet_Zápisů_Na_Soupisce where [Reg. č.] = '1094895'
--Počet zápisů = x+2, Z toho náhradníkem = y+1

--Hráči_Zároveň_Rozhodčí
--Vrátí hráče, kteří zároveň mohou být rozhodčím utkání
--Z testovacích dat víme, že jediným hráčem, který má hotové zkoušky rozhodčího je Vladan Vomáčka
select * from Hráči_Zároveň_Rozhodčí

--Přidáme-li pouze nového hráče, pohled nám vrátí stejná data
exec Přidej_Hráče '3894393', 'Jaromír', 'Novohradčanský', 'mužské', '1988-02-15', 'jarda@hradec.cz', '128394234'
select * from Hráči_Zároveň_Rozhodčí

--Přidáme-li novému hráči status rozhodčího přibyde do výpisu pohledu
declare @k2_id int
select @k2_id = Kontakt_Id from Hráč
insert into Rozhodčí (Kontakt_Id) VALUES (@k2_id)
select * from Hráči_Zároveň_Rozhodčí

--Hráčské_Kategorie
--Jednotlivé hráčské kategorie s délkou utkání
select * from Hráčské_Kategorie
--Využívá funkci, kterou jsme již viděli (dbo.Ml_Kategorie_Formátované)

--Pro porovnání:
--Původní tabulka
select * from Ml_Kategorie

--Klub_Adresa_Správce
select * from Klub_Adresa_Správce

--Mezi kluby ve výpisu nám chybí pouze případ, kdy by správce měl pouze email, nikoliv pouze tel. číslo
--Přidáme si takový klub
exec Přidej_Klub 'TJ Sokol Stárkov', 'Kohoutova', '3', 'Stárkov', '59433', 'Pavel', 'Mikuška', 'p.mikuska@outlook.cz'

select * from Klub_Adresa_Správce
--Stárkov se objevil mezi ostatními kluby

--Kontakt_Zapisovatel_Počet_Zapsaných_Soupisek
--Vrátí kontakty osob s počtem zapsaných soupisek
--Kontakt se objeví ve výpisu pouze pokud zapsal alespoň 1 soupisku
select * from Kontakt_Zapisovatel_Počet_Zapsaných_Soupisek

--Přidáme nový kontakt, který vytvoří novou soupisku
--a František Majer vytvoří novou soupisku
declare @fn int, @fm int
exec Přidej_Kontakt 'Filip', 'Nguyen', 'filip@slovan.liberec.cz', '928348329', @kontakt_id = @fn output
select @fm = Id from Kontakt where Příjmení = 'Majer'
exec Vytvoř_Prázdnou_Soupisku @fn
exec Vytvoř_Prázdnou_Soupisku @fm

--Podíváme se opět na výstup pohledu
select * from Kontakt_Zapisovatel_Počet_Zapsaných_Soupisek
--Filip Nguyen se nám ve výpisu objevil jakožto nový zapisovatel soupisek s jednou soupiskou
--Františku Majerovi jedna soupiska přibyla

--Ml_Kategorie_Počet_Odehraných_Zápasů
--Jednotlivé kategorie s počtem odehraných utkání
select * from Ml_Kategorie_Počet_Odehraných_Zápasů

--Konkrétněji Starší dorost má momentálně odehráno x utkání
select * from Ml_Kategorie_Počet_Odehraných_Zápasů where Název = 'Starší dorost'

--Vybereme nějakou validní soupisku Staršího dorostu a vytvoříme nové utkání s touto soupiskou
declare @soup int
select top 1 @soup = [Id soupisky] from Hráči_Na_Soupisce
where [Hráčská kat.] = 'Starší dorost'
group by [Id soupisky]
having COUNT(*) > 6
declare @ohk int, @ohka int, @r int
select @ohk = Id, @ohka = Adresa_Id from Klub where Název = 'Olympia Hradec Králové'
select top 1 @r = Id from Rozhodčí
exec Přidej_Utkání 0, 0, @ohk, @ohka, @r, @soup, 'Starší dorost'

--Nyní bude mít Starší dorost odehráno x+1 utkání
select * from Ml_Kategorie_Počet_Odehraných_Zápasů where Název = 'Starší dorost'

--Počet odehraných utkání u ostatních kategorií zůstal beze změny
select * from Ml_Kategorie_Počet_Odehraných_Zápasů


--Prázdné_Soupisky
--Vrátí všechny soupisky neobsahující hráče
select * from Prázdné_Soupisky
--(Kontaktní informace, pokud jsou k dispozici, se vážou k osobě, která zapsala soupisku)

--Podíváme se jestli jsou vrácené soupisky opravdu prázdné pomocí funkce dbo.Soupiska_Počet_Lidí
--(Pokud jsme vůbec nějaké prázdné soupisky vytvořili, např. spuštěním některého testovacího kódu výše)
select [Id soupisky], dbo.Soupiska_Počet_Lidí([Id soupisky]) as 'Počet zapsaných hráčů', Jméno, Příjmení, Email, [Tel. č.] from Prázdné_Soupisky
--Na všech soupiskách je Počet zapsaných hráčů roven 0

--Protože nám tyto soupisky k ničemu moc nejsou můžeme se jich zbavit
exec Smaž_Prázdné_Soupisky

--Nyní již žádné prázdné soupisky v DB nemáme
select * from Prázdné_Soupisky

--Rozhodčí_Místo_Počet_Odpískaných_Utkání
--Rozhodčí, klub s počtem odpískaných utkání
select * from Rozhodčí_Místo_Počet_Odpískaných_Utkání

--Nově přidaný rozhodčí bude mít 0 odpískaných utkání
exec Přidej_Rozhodčího 'Radek', 'Zítka', 'radek.zitka@komna.cz', '123452344'
select * from Rozhodčí_Místo_Počet_Odpískaných_Utkání where Jméno = 'Radek'

--Necháme Radka Zítku jeden zápas odpískat
declare @r_id int
select @r_id = max(Id) from Rozhodčí
update Utkání SET Rozhodčí_Id = @r_id
where Id IN (select top 1 Id from Utkání)

--Teď už má jeden zápas odpískaný
select * from Rozhodčí_Místo_Počet_Odpískaných_Utkání where Jméno = 'Radek'

--Bližší pohled na utkání, které Radek Zítka odpískal
select * from Zápas where [Jméno rozh.] = 'Radek' and [Příjmení rozh.] = 'Zítka'

--Rozhodčí_Počet_Odpískaných_Utkání
--Obdobný pohled jako předchozí, neseskupuje počty podle rozhodčího a místa, ale pouze podle rozhodčího
select * from Rozhodčí_Počet_Odpískaných_Utkání

--Sezóna_Počet_Utkání
--Vrátí jednotlivé sezóny s počtem odehraných utkání
--Všimneme si že v sezóně 2011/2012 se odehrála 2 utkání a 2015/2016 se žádné utkání neodehrálo
select * from Sezóna_Počet_Utkání

--V sezoně 2011/2012 se odehrála 2 utkání
--Podíváme se na všechna utkání sezóny 2011/2012 skrze pohled Zápas
select * from Zápas where [Sez. start] = '2011-03-06'
--Vskutku se jedná o dvě utkání

--V sezóně 2015/2016 se neodehrálo žádné utkání
select * from Zápas where [Sez. start] = '2015-11-11'
--Výstup je prázdný

--Soupiska_Kontakt_Zapisovatel
--Vrátí všechny soupisky s kontatními informacemi zapisovatele a počtem zapsaných hráčů
select * from Soupiska_Kontakt_Zapisovatel

--Vytvoříme si prázdnou soupisku, kterou zapsal Jiří Nedočkavý a prázdnou soupisku o níž nevím, kdo ji zapsal
declare @kj int
select @kj = Id from Kontakt where Jméno = 'Jiří' and Příjmení = 'Nedočkavý'
exec Vytvoř_Prázdnou_Soupisku @kj
exec Vytvoř_Prázdnou_Soupisku

select * from Soupiska_Kontakt_Zapisovatel where [Id soupisky] IN (select top 2 Id from Soupiska order by Id desc)
--Máme je mezi soupiskami, aktuálně na nich je 0 hráčů

--Na poslední přidanou soupisku přidáme tři hráčky
declare @ls int, @reg int, @cislo int = 4
select @ls = max(Id) from Soupiska
declare cur CURSOR LOCAL for select top 3 [Reg. č.] from Hráči where [Hráčská kat.] = 'Starší žákyně'
open cur
fetch next from cur into @reg
while @@FETCH_STATUS = 0 
begin
	exec Přidej_Hráče_Na_Soupisku @reg, @ls, @cislo, 0
	set @cislo = @cislo + 1
	fetch next from cur into @reg
end
close cur
deallocate cur

--Kontrola
select * from Soupiska_Kontakt_Zapisovatel where [Id soupisky] IN (select top 2 Id from Soupiska order by Id desc)
--Korektně na poslední soupisku přidali 3 hráčky, druhá nově přidaná soupiska zůstala beze změny

