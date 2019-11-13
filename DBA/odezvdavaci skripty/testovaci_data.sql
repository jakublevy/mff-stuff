/*
Autor: Jakub Levý
Vytvořeno pro DBA @ MFF ZS 19/20
*/

--Kluby s adresami
exec Přidej_Klub 'VEBA Machov', 'Machovská', '2', 'Machov', '54377', 'Jiří', 'Drtina', 'jir.drtina@post.cz', '779321701'
exec Přidej_Klub 'Lokomotiva Červený Kostelec', 'Ke Kostelu', '22a/8', 'Červený Kostelec', '54941', 'Stanislav', 'Luděk', 'standa.fočus@seznam.cz', '589219007'
exec Přidej_Klub 'TJ Rozkoš', 'Uždilova', '22', 'Česká Skalice', '54441', 'Stanislav', 'Skalický', 'skalicky@centrum.cz', '783029571'
exec Přidej_Klub 'SK Solnice', '17. listopadu', '41', 'Solnice', '49321' --SK Solnice nemá momentálně správce klubu
exec Přidej_Klub 'FC Náchod', 'Náchodská', '9', 'Náchod', '54901', 'František', 'Panenka', @tel_cislo = '777382958' --Správce FC Náchod nemá email
exec Přidej_Klub 'Olympia Hradec Králové', 'Pražská', '891', 'Hradec Králové', '50002', 'Milan', 'Dlouhý', 'milan@hradec.cz', '787392819'
exec Přidej_Klub 'SK Babí', 'Vyšehrad', '111', 'Náchod', '54701', 'Josef', 'Stránský', 'josef.stransky@fotbalbabi.cz', '783738215'
exec Přidej_Klub 'TJ Koněpůlky', 'Pardubická', '12', 'Pardubice', '40001', 'František', 'Malý', 'franta@pardubice.cz', '749432804' 