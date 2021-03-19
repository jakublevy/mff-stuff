/*
Autor: Jakub Levý
Vytvořeno pro DBA @ MFF ZS 19/20

Tento soubor obsahuje testovací data, která umožní předvést funkcionalitu DB.
Je nutné spustit celý soubor jako skript (nikoliv po částech).
*/

USE Levý_Fotbal 
GO

--Kluby s adresami proti kterým hrajeme utkání
declare @babi int, @konepulky int, @rozkos int, @machov int, @hradec int, @nachod int, @kostelec int, @solnice int;
exec Přidej_Klub 'VEBA Machov', 'Machovská', '2', 'Machov', '54377', 'Jiří', 'Drtina', 'jir.drtina@post.cz', '779321701', @klub_id = @machov output
exec Přidej_Klub 'Lokomotiva Červený Kostelec', 'Ke Kostelu', '22a/8', 'Červený Kostelec', '54941', 'Stanislav', 'Luděk', 'standa.fočus@seznam.cz', '589219007', @klub_id = @kostelec output
exec Přidej_Klub 'RSCM Rozkoš', 'Uždilova', '22', 'Česká Skalice', '54441', 'Stanislav', 'Skalický', 'skalicky@centrum.cz', '783029571', @klub_id = @rozkos output
exec Přidej_Klub 'SK Solnice', '17. listopadu', '41', 'Solnice', '49321', @klub_id = @solnice output --klub SK Solnice nemá momentálně správce klubu
exec Přidej_Klub 'FC Náchod', 'Náchodská', '9', 'Náchod', '54901', 'František', 'Panenka', @tel_cislo = '777382958', @klub_id = @nachod output --Správce klubu FC Náchod nemá email
exec Přidej_Klub 'Olympia Hradec Králové', 'Pražská', '891', 'Hradec Králové', '50002', 'Milan', 'Dlouhý', 'milan@hradec.cz', '787392819', @klub_id = @hradec output
exec Přidej_Klub 'SK Babí', 'Vyšehrad', '111', 'Náchod', '54701', 'Josef', 'Stránský', 'josef.stransky@fotbalbabi.cz', '783738215', @klub_id = @babi output
exec Přidej_Klub 'TJ Koněpůlky', 'Pardubická', '12', 'Pardubice', '40001', 'František', 'Malý', 'franta@pardubice.cz', '749432804', @klub_id = @konepulky output



--Ml. kategorie
insert into Ml_Kategorie (Název, Muži, Délka_Zápasu_Minut)
VALUES
	('Mladší dorost', 1, 80)
  , ('Mladší dorostenky', 0, 80)
  , ('Mladší přípravka', 0, 80)
  , ('Mladší přípravka', 1, 70)
  , ('Mladší žáci', 1, 60)
  , ('Mladší žákyně', 0, 60)
  , ('Muži', 1, 90)
  , ('Starší dorost', 1, 90)
  , ('Starší dorostenky', 0, 90)
  , ('Starší přípravka', 0, 40)
  , ('Starší přípravka', 1, 60)
  , ('Starší žáci', 1, 70)
  , ('Starší žákyně', 0, 70)
  , ('Ženy', 0, 90)



--Rozhodčí
declare @r1 int, @r2 int, @r3 int
exec Přidej_Rozhodčího 'Milan', 'Šedivý', 'm.sedivy@facr.cz', '604666881', @rozhodci_id = @r1 output
exec Přidej_Rozhodčího 'Pawel', 'Adamowicz', 'pawel.adamowicz@email.pl', '748394123', '+48', @rozhodci_id = @r2 output 
exec Přidej_Rozhodčího 'Peter', 'Nagy', 'peter.nagy@fotbal.sk', '483990389', '+421', @rozhodci_id = @r3 output



--Sezóna
insert into Sezóna (Start, Konec)
VALUES
	('2011-03-06', '2012-01-15')
  , ('2014-03-07', '2015-01-19')
  , ('2015-11-11', '2016-09-24')
  , ('2017-03-08', '2018-01-22')
  , ('2018-05-09', '2019-04-01')
  , ('2019-10-08', '2020-09-18')
  , ('2020-10-10', '2021-11-11')


  
--Hráči

--družstvo kategorie mužů
exec Přidej_Hráče '0001820', 'Josef', 'Grabowski', 'mužské', '1983-12-21', 'josef.grabowski@email.pl', '348920394', '+48' --na hostování z SK Babí
exec Přidej_Hráče '0004530', 'František', 'Ducháč', 'mužské', '1992-03-03', 'frantisek.duchac@email.cz', '892574839'
exec Přidej_Hráče '0033513', 'Pavel', 'Braha', 'mužské', '1991-01-10', 'pavel.braha@email.cz', '784849367'
exec Přidej_Hráče '0046815', 'Miloš', 'Kozák', 'mužské', '1959-06-09', 'milos.kozak@centrum.cz', '783289021'
exec Přidej_Hráče '0095357', 'Antonín', 'Adámek', 'mužské', '1992-11-08', 'antonin.adamek@yahoo.com', '145873948'
exec Přidej_Hráče '0102630', 'Bořivoj', 'Hataš', 'mužské', '1984-06-25', 'borivoj.hatas@volny.cz', '603498490'
exec Přidej_Hráče '0471068', 'Jakub', 'Bálek', 'mužské', '1977-11-05', 'jbalek@protonmail.ch', '948304210'
exec Přidej_Hráče '0434013', 'Mirai', 'Voborník', 'mužské', '1985-04-14', 'mirai@meeru.jp', '493091483', '+81' --na hostování z TJ Koněpůlek
exec Přidej_Hráče '0374901', 'Milan', 'Hron', 'mužské', '1977-12-03', 'milan.hron@seznam.cz', '601602991'
exec Přidej_Hráče '0549405', 'Petr', 'Schrötter', 'mužské', '1951-10-01', 'petr.schrotter@tiscali.cz', '602666119'
exec Přidej_Hráče '0593646', 'Petr', 'Bryksí', 'mužské', '1993-12-03', 'petr.bryksi@gmail.com', '490483901'
exec Přidej_Hráče '0645778', 'Pavel', 'Bryksí', 'mužské', '1993-12-04', 'pavel.bryksi@outlook.cz', '491431111'
exec Přidej_Hráče '0668429', 'Josef', 'Alexa', 'mužské', '1981-11-12', 'pepa.alexa@hotmail.cz', '775030123'
exec Přidej_Hráče '0996668', 'Lukáš', 'Zelinka', 'mužské', '1989-11-02', 'l-u-k-a-s--z-e-l-i-n-k-a@seznam.cz', '771034012'
exec Přidej_Hráče '1094895', 'Marek', 'Hron', 'mužské', '1991-01-01', 'mhron@seznam.cz', '771050610'
exec Přidej_Hráče '1937981', 'Pawol', 'Nowak', 'mužské', '1994-01-01', 'nowak@focus.pl', '669110506', '+48' --na hostování z SK Babí

--družstvo kategorie starší přípravka (devčata)
exec Přidej_Hráče '2004779', 'Martina', 'Čermáková', 'ženské', '2010-09-07', 'martina.cermakova@seznam.cz', '941324321'
exec Přidej_Hráče '1565121', 'Michaela', 'Rousková', 'ženské', '2010-08-27', 'michaela@rouskova.cz', '721750904'
exec Přidej_Hráče '1650655', 'Eliška', 'Prouzová', 'ženské', '2010-06-08', 'e.prouzova@seznam.cz', '123487943'
exec Přidej_Hráče '1734537', 'Anna', 'Vodáková', 'ženské', '2010-10-22', 'anna.v@gymnachod.cz', '734829198'
exec Přidej_Hráče '1535958', 'Nikola', 'Malá', 'ženské', '2010-10-25', 'nikola.mala@ssaobhk.cz', '839234109'
exec Přidej_Hráče '1569064', 'Kristýna', 'Středová', 'ženské', '2010-10-02', 'kry.st@obec-horni-radechova.cz', '324900990'
exec Přidej_Hráče '1707151', 'Alena', 'Kunstová', 'ženské', '2010-11-04', 'alena@kunstik.cz', '834142399'
exec Přidej_Hráče '1219555', 'Saki', 'Yamamoto', 'ženské', '2010-06-25', 'saki@yamamoto.jp', '231423897', '+81' --na hostování z RSCM Rozkoš
exec Přidej_Hráče '0562290', 'Sakura', 'Takahashi', 'ženské', '2010-07-29', 'sakura@quickmail.jp', '843910039', '+81' --na hostování z VEBA Machov
exec Přidej_Hráče '0769472', 'Marie', 'Honzerová', 'ženské', '2010-06-02', 'marie.honzerova@nazskom.cz', '791823942'
exec Přidej_Hráče '0988559', 'Lucie', 'Černá', 'ženské', '2010-05-11', 'l.cerna@mail.com', '213849090'
exec Přidej_Hráče '1095706', 'Helena', 'Vondřejcová', 'ženské', '2010-09-30', 'helenka@vondrejcovi.cz', '723148994'
exec Přidej_Hráče '0486604', 'Ivana', 'Dobrá', 'ženské', '2010-08-31', 'ivana.dobra@email.cz', '158795413'
exec Přidej_Hráče '0424448', 'Božena', 'Polská', 'ženské', '2010-11-13', 'bozenka@zpolska.pl', '373284193', '+48' --na hostování z Olympia Hradec Králové
exec Přidej_Hráče '0368681', 'Veronika', 'Volná', 'ženské', '2010-11-15', 'veronika@volny.cz', '314897432'


--družstvo kategorie starší dorost
exec Přidej_Hráče '1002706', 'Karel', 'Hrnčíř', 'mužské', '2002-07-13', 'kaja@hrnec.cz', '771234987'
exec Přidej_Hráče '1113836', 'Michal', 'Novotný', 'mužské', '2002-06-11', 'michal.novotny@post.cz', '601603150'
exec Přidej_Hráče '1308728', 'Viktor', 'Fidler', 'mužské', '2002-06-22', 'viktor.filder@email.cz', '602150144'
exec Přidej_Hráče '0219444', 'Viktor', 'Skalický', 'mužské', '2002-08-01', 'skalicky.v@volny.cz', '711150441'
exec Přidej_Hráče '1575479', 'Daniel', 'Umlauf', 'mužské', '2002-09-21', 'daniel@umlauf.cz', '606313945'
exec Přidej_Hráče '1639407', 'Bohuslav', 'Hynek', 'mužské', '2002-10-02', 'bohuslav.hynek@atlas.cz', '603591091'
exec Přidej_Hráče '1673927', 'Matěj', 'Ansorge', 'mužské', '2002-11-13', 'matej.ansorge@outlook.com', '601777515'
exec Přidej_Hráče '1739793', 'Michael', 'Svoboda', 'mužské', '2002-10-07', 'michael.s@apple.com', '715094912'
exec Přidej_Hráče '1740292', 'Jakub', 'Vladyka', 'mužské', '2002-08-30', 'jvladyka@microsoft.com', '747950147'
exec Přidej_Hráče '1781253', 'Nikolas', 'Šarišský', 'mužské', '2002-07-01', 'niki.sar@post.cz', '606150999'
exec Přidej_Hráče '1790985', 'Tomáš', 'Škripko', 'mužské', '2002-08-08', 'tom.skripko@gmail.com', '607750987'
exec Přidej_Hráče '1804178', 'Jan', 'Železo', 'mužské', '2002-05-31', 'jan.zelezo@seznam.cz', '849301534'
exec Přidej_Hráče '1809151', 'Roman', 'Regent', 'mužské', '2002-06-30', 'rr@centrum.cz', '750740730'
exec Přidej_Hráče '1853901', 'Šlitr', 'Martin', 'mužské', '2002-09-09', 'slitr.martin@email.cz', '723489478'
exec Přidej_Hráče '1811787', 'Bayajavkhlan', 'Ganzorikt', 'mužské', '2002-11-10', 'b.ganzorikt@yandex.ru', '213489243', '+7' --na hostování z RSCM Rozkoš
exec Přidej_Hráče '1915044', 'Vladan', 'Vomáčka', 'mužské', '2002-10-21', 'vladan.omacka@vomacka.cz', '123434321' --ten už vůbec není naším hráčem, má prošlé hostování

--zároveň však si Vladan Vomáčka udělal zkoušky rozhodčího a tedy je hráčem (bývalým) i rozhodčním
declare @vladan_kontakt_id int
select @vladan_kontakt_id = Kontakt_Id from Hráč where Reg_Id = '1915044'
insert into Rozhodčí (Kontakt_Id) VALUES (@vladan_kontakt_id)
declare @r4 int
set @r4 = SCOPE_IDENTITY()



--Hostování
exec Přidej_Hostování '10AA55', '2017-03-10', '2021-05-10', 20000, '0001820', @babi
exec Přidej_Hostování '10BB66', '2018-10-10', '2022-06-11', 50000, '0434013', @konepulky
exec Přidej_Hostování 'XX25BC25', '2016-10-10', '2020-12-31', 5000, '1937981', @babi

exec Přidej_Hostování 'KLBB050505', '2017-01-10', '2019-01-31', 30000, '1219555', @rozkos
exec Přidej_Hostování 'KLAAA1010', '2019-02-27', '2022-03-13', 30000, '1219555', @rozkos
exec Přidej_Hostování 'MAZZ55AA', '2018-06-16', '2021-01-01', 25000, '0562290', @machov
exec Přidej_Hostování '10DOKONIITA10', '2016-10-10', '2020-07-07', 35000, '0424448', @hradec

exec Přidej_Hostování 'X50YAC56', '2019-03-31', '2023-07-31', 80000, '1811787', @rozkos
exec Přidej_Hostování 'X40YAC45', '2010-01-27', '2013-03-15', 75000, '1811787', @rozkos
exec Přidej_Hostování 'A150C251', '2013-03-15', '2013-12-12', 150000, '1915044', @hradec



--Zapisoval soupisek
--V našem klubu obvykle soupisky zapisuje František Majer nebo Jiří Nedočkavý
declare @z1 int, @z2 int
exec Přidej_Kontakt 'František', 'Majer', 'franta@majeru.cz', '273483091', @kontakt_id = @z1 output
exec Přidej_Kontakt 'Jiří', 'Nedočkavý', 'jiri.nedockavy@mail.com', '172384998', @kontakt_id = @z2 output

--Soupisky
--Každé z naších jednotlivých družstev má právě 2 soupisky ==> dohromady máme 6 soupisek
declare @s1 int, @s2 int, @s3 int, @s4 int, @s5 int, @s6 int
exec Vytvoř_Prázdnou_Soupisku @z1, @soupiska_id = @s1 output
exec Vytvoř_Prázdnou_Soupisku @z2, @soupiska_id = @s2 output
exec Vytvoř_Prázdnou_Soupisku @z2, @soupiska_id = @s3 output
exec Vytvoř_Prázdnou_Soupisku @z2, @soupiska_id = @s4 output
exec Vytvoř_Prázdnou_Soupisku @z1, @soupiska_id = @s5 output
exec Vytvoř_Prázdnou_Soupisku @z2, @soupiska_id = @s6 output

exec Přidej_Hráče_Na_Soupisku '0001820', @s1, 1, 0
exec Přidej_Hráče_Na_Soupisku '0004530', @s1, 2, 0
exec Přidej_Hráče_Na_Soupisku '0033513', @s1, 3, 0
exec Přidej_Hráče_Na_Soupisku '0046815', @s1, 4, 0
exec Přidej_Hráče_Na_Soupisku '0095357', @s1, 5, 0
exec Přidej_Hráče_Na_Soupisku '0102630', @s1, 6, 0
exec Přidej_Hráče_Na_Soupisku '0471068', @s1, 7, 0
exec Přidej_Hráče_Na_Soupisku '0434013', @s1, 8, 0
exec Přidej_Hráče_Na_Soupisku '0374901', @s1, 9, 0
exec Přidej_Hráče_Na_Soupisku '0549405', @s1, 10, 0
exec Přidej_Hráče_Na_Soupisku '0593646', @s1, 11, 0
exec Přidej_Hráče_Na_Soupisku '0645778', @s1, 12, 1
exec Přidej_Hráče_Na_Soupisku '0668429', @s1, 13, 1
exec Přidej_Hráče_Na_Soupisku '0996668', @s1, 14, 1
exec Přidej_Hráče_Na_Soupisku '1094895', @s1, 15, 1
exec Přidej_Hráče_Na_Soupisku '1937981', @s1, 16, 1

exec Přidej_Hráče_Na_Soupisku '0001820', @s2, 16, 1
exec Přidej_Hráče_Na_Soupisku '0004530', @s2, 15, 1
exec Přidej_Hráče_Na_Soupisku '0033513', @s2, 14, 1
exec Přidej_Hráče_Na_Soupisku '0046815', @s2, 13, 1
exec Přidej_Hráče_Na_Soupisku '0095357', @s2, 12, 1
exec Přidej_Hráče_Na_Soupisku '0102630', @s2, 11, 0
exec Přidej_Hráče_Na_Soupisku '0471068', @s2, 10, 0
exec Přidej_Hráče_Na_Soupisku '0434013', @s2, 9, 0
exec Přidej_Hráče_Na_Soupisku '0374901', @s2, 8, 0
exec Přidej_Hráče_Na_Soupisku '0549405', @s2, 7, 0
exec Přidej_Hráče_Na_Soupisku '0593646', @s2, 6, 0
exec Přidej_Hráče_Na_Soupisku '0645778', @s2, 5, 0
exec Přidej_Hráče_Na_Soupisku '0668429', @s2, 4, 0
exec Přidej_Hráče_Na_Soupisku '0996668', @s2, 3, 0
exec Přidej_Hráče_Na_Soupisku '1094895', @s2, 2, 0
exec Přidej_Hráče_Na_Soupisku '1937981', @s2, 1, 0

exec Přidej_Hráče_Na_Soupisku '2004779', @s3, 1, 0
exec Přidej_Hráče_Na_Soupisku '1565121', @s3, 2, 0
exec Přidej_Hráče_Na_Soupisku '1650655', @s3, 8, 0
exec Přidej_Hráče_Na_Soupisku '1734537', @s3, 4, 0
exec Přidej_Hráče_Na_Soupisku '1535958', @s3, 5, 0
exec Přidej_Hráče_Na_Soupisku '1569064', @s3, 6, 0
exec Přidej_Hráče_Na_Soupisku '1707151', @s3, 7, 0
exec Přidej_Hráče_Na_Soupisku '1219555', @s3, 3, 0
exec Přidej_Hráče_Na_Soupisku '0562290', @s3, 9, 0
exec Přidej_Hráče_Na_Soupisku '0769472', @s3, 10, 0
exec Přidej_Hráče_Na_Soupisku '0988559', @s3, 11, 0
exec Přidej_Hráče_Na_Soupisku '1095706', @s3, 12, 1
exec Přidej_Hráče_Na_Soupisku '0486604', @s3, 13, 1
exec Přidej_Hráče_Na_Soupisku '0424448', @s3, 14, 1
exec Přidej_Hráče_Na_Soupisku '0368681', @s3, 15, 1

exec Přidej_Hráče_Na_Soupisku '2004779', @s4, 15, 1
exec Přidej_Hráče_Na_Soupisku '1565121', @s4, 14, 0
exec Přidej_Hráče_Na_Soupisku '1650655', @s4, 13, 1
exec Přidej_Hráče_Na_Soupisku '1734537', @s4, 12, 1
exec Přidej_Hráče_Na_Soupisku '1535958', @s4, 11, 0
exec Přidej_Hráče_Na_Soupisku '1569064', @s4, 10, 0
exec Přidej_Hráče_Na_Soupisku '1707151', @s4, 9, 0
exec Přidej_Hráče_Na_Soupisku '1219555', @s4, 3, 1
exec Přidej_Hráče_Na_Soupisku '0562290', @s4, 7, 0
exec Přidej_Hráče_Na_Soupisku '0769472', @s4, 6, 0
exec Přidej_Hráče_Na_Soupisku '0988559', @s4, 5, 0
exec Přidej_Hráče_Na_Soupisku '1095706', @s4, 4, 0
exec Přidej_Hráče_Na_Soupisku '0486604', @s4, 8, 0
exec Přidej_Hráče_Na_Soupisku '0424448', @s4, 2, 0
exec Přidej_Hráče_Na_Soupisku '0368681', @s4, 1, 0

exec Přidej_Hráče_Na_Soupisku '1002706', @s5, 1, 0
exec Přidej_Hráče_Na_Soupisku '1113836', @s5, 2, 0
exec Přidej_Hráče_Na_Soupisku '1308728', @s5, 3, 0
exec Přidej_Hráče_Na_Soupisku '0219444', @s5, 4, 0
exec Přidej_Hráče_Na_Soupisku '1575479', @s5, 5, 0
exec Přidej_Hráče_Na_Soupisku '1639407', @s5, 6, 0
exec Přidej_Hráče_Na_Soupisku '1673927', @s5, 7, 0
exec Přidej_Hráče_Na_Soupisku '1739793', @s5, 8, 0
exec Přidej_Hráče_Na_Soupisku '1740292', @s5, 9, 0
exec Přidej_Hráče_Na_Soupisku '1781253', @s5, 10, 0
exec Přidej_Hráče_Na_Soupisku '1790985', @s5, 11, 0
exec Přidej_Hráče_Na_Soupisku '1804178', @s5, 12, 1
exec Přidej_Hráče_Na_Soupisku '1809151', @s5, 13, 1
exec Přidej_Hráče_Na_Soupisku '1853901', @s5, 14, 1
exec Přidej_Hráče_Na_Soupisku '1811787', @s5, 15, 1

exec Přidej_Hráče_Na_Soupisku '1002706', @s6, 15, 1
exec Přidej_Hráče_Na_Soupisku '1113836', @s6, 14, 1
exec Přidej_Hráče_Na_Soupisku '1308728', @s6, 13, 1
exec Přidej_Hráče_Na_Soupisku '0219444', @s6, 12, 0
exec Přidej_Hráče_Na_Soupisku '1575479', @s6, 11, 0
exec Přidej_Hráče_Na_Soupisku '1639407', @s6, 10, 0
exec Přidej_Hráče_Na_Soupisku '1673927', @s6, 9, 0
exec Přidej_Hráče_Na_Soupisku '1739793', @s6, 8, 1
exec Přidej_Hráče_Na_Soupisku '1740292', @s6, 7, 0
exec Přidej_Hráče_Na_Soupisku '1781253', @s6, 6, 0
exec Přidej_Hráče_Na_Soupisku '1790985', @s6, 5, 0
exec Přidej_Hráče_Na_Soupisku '1804178', @s6, 4, 0
exec Přidej_Hráče_Na_Soupisku '1809151', @s6, 3, 0
exec Přidej_Hráče_Na_Soupisku '1853901', @s6, 2, 0
exec Přidej_Hráče_Na_Soupisku '1811787', @s6, 1, 0


--náš klub, jehož správcem je zapisovatel soupisek Jiří Nedočkavý
declare @klub_nas int, @adresa_nase int
exec Přidej_Klub 'SEPA Žabáci', 'Pavlišovská', '53', 'Slavíkov', '55041', @klub_id = @klub_nas output
update Klub set Pověřená_Osoba_Id = @z2 where Id = @klub_nas
select @adresa_nase = Adresa_Id from Klub where Id = @klub_nas

--adresy ostatních klubů, kde se utkání odehrála
declare @babi_adresa int, @hradec_adresa int, @nachod_adresa int, @kostelec_adresa int, @konepulky_adresa int, @solnice_adresa int, @rozkos_adresa int, @machov_adresa int
select @babi_adresa = Adresa_Id from Klub where Id = @babi
select @hradec_adresa = Adresa_Id from Klub where Id = @hradec
select @nachod_adresa = Adresa_Id from Klub where Id = @nachod
select @kostelec_adresa = Adresa_Id from Klub where Id = @kostelec
select @konepulky_adresa = Adresa_Id from Klub where Id = @konepulky
select @solnice_adresa = Adresa_Id from Klub where Id = @solnice
select @rozkos_adresa = Adresa_Id from Klub where Id = @rozkos
select @machov_adresa = Adresa_Id from Klub where Id = @machov

--Utkání
exec Přidej_Utkání  5, 3, @babi, @babi_adresa, @r1, @s1, 'Muži', @sezona_start = '2018-05-09', @goly_my_polocas = 1, @goly_souper_polocas = 1
exec Přidej_Utkání  1, 2, @machov, @machov_adresa, @r3, @s1, 'Muži', @sezona_start = '2019-10-08', @goly_my_polocas = 0, @goly_souper_polocas = 1
exec Přidej_Utkání 3, 3, @babi, @adresa_nase, @r2, @s2, 'Muži', @sezona_start = '2018-05-09', @goly_my_polocas = 2, @goly_souper_polocas = 2
exec Přidej_Utkání 0, 0, @kostelec, @kostelec_adresa, @r2, @s2, 'Muži', @sezona_start = '2018-05-09', @goly_my_polocas = 0, @goly_souper_polocas = 0

exec Přidej_Utkání 1, 10, @hradec, @adresa_nase, @r4, @s3, 'Starší přípravka', 0, '2019-10-08', @goly_my_polocas = 0, @goly_souper_polocas = 7
exec Přidej_Utkání 2, 3, @nachod, @adresa_nase, @r3, @s3, 'Starší přípravka', 0, '2019-10-08', @goly_my_polocas = 0, @goly_souper_polocas = 0
exec Přidej_Utkání 5, 0, @konepulky, @konepulky_adresa, @r1, @s4, 'Starší přípravka', 0, '2018-05-09' --neznáme stav o poločase
exec Přidej_Utkání 1, 4, @solnice, @solnice_adresa, @r1, @s4, 'Starší přípravka', 0, '2018-05-09' --neznáme stav o poločase

exec Přidej_Utkání 3, 3, @rozkos, @rozkos_adresa, @r4, @s5, 'Starší dorost', @sezona_start = '2011-03-06', @goly_my_polocas = 2, @goly_souper_polocas = 2
exec Přidej_Utkání 3, 4, @machov, @machov_adresa, @r3, @s5, 'Starší dorost', @sezona_start = '2011-03-06', @goly_my_polocas = 2, @goly_souper_polocas = 0
exec Přidej_Utkání 2, 5, @nachod, @adresa_nase, @r1, @s6, 'Starší dorost', @sezona_start = '2019-10-08' --neznáme stav o poločase
exec Přidej_Utkání 1,6, @hradec, @adresa_nase, @r2, @s6, 'Starší dorost', @sezona_start = '2019-10-08', @goly_my_polocas = 0, @goly_souper_polocas = 5
