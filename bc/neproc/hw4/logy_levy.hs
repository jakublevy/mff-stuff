import qualified Data.Set as Set
import qualified Data.List as List

-- cabal/stack install data-stringmap
import qualified Data.StringMap as StringMap


type Castka = Integer

data Operace
  = Prihlaseni
  | Odhlaseni
  | Vyber Castka
  | Pripis Castka
  deriving (Show, Read, Eq)

type Cas = Integer

type Uzivatel = String

data Zaznam =
  Zaznam Cas
         Uzivatel
         Operace
  deriving (Show, Read, Eq)

type Zaznamy = [Zaznam]

main = do
  log <- (map read . lines <$> readFile "banka.log") :: IO [Zaznam] --nacteni a rozparsovani logu
  let result cmt f --pomocna funkce na vypisovani vysledku
       = do
        putStrLn (cmt ++ ":")
        print (f log)
        putChar '\n'
  {- pocitani a vypisovani vysledku zacina zde -}
  result
    "DEMO -- jmeno prvniho uzivatele v souboru se smichanymi zaznamy"
    demoPrvniZaznam
  result "Seznam uzivatelu serazenych podle abecedy" serazeniUzivatele
  result "Casy top 10 nejvetsich vyberu" top10vyber
  result "Jmena uzivatelu 10 nejmensich pripisu" top10pripis
  result "Nejaktivnejsi uzivatel" topUzivatel
  result "Uzivatel ktery vydelal nejvic penez" topPrirustek
  result "Prumerna vybrana castka uzivatelu zacinajicich od J" prumerVyberuJ
  result
    "Uzivatel s nejdelsi posloupnosti akci nerusenou v logu jinymi uzivateli"
    nejdelsiSingleRun
  result
    "Pocet vyberu pri kterych uzivatele s 5-pismennymi jmeny nebyli prokazatelne prihlaseni"
    neprihlaseneVybery

{- Priklad -}
demoPrvniZaznam :: Zaznamy -> Uzivatel
demoPrvniZaznam (Zaznam _ jm _:_) = jm

{- Ukol zacina tady. Misto `undefined` dodejte definice funkci, ktere z logu
 - vytahnou pozadovany vysledek. -}
serazeniUzivatele :: Zaznamy -> [Uzivatel]
serazeniUzivatele zs = Set.toList mnozinaUzivatelu
                                          where mnozinaUzivatelu = foldr (\(Zaznam c j o) mnozina -> Set.insert j mnozina) Set.empty zs

jeVyber :: Operace -> Bool
jeVyber(Vyber x) = True
jeVyber o = False

top10vyber :: Zaznamy -> [Cas]
top10vyber zs = map (\(Zaznam c _ _) -> c) (take 10 vyberySerazene)
                         where
                          vybery = filter (\(Zaznam _ _ o) -> jeVyber o) zs
                          vyberySerazene = List.sortBy (\(Zaznam _ _ (Vyber v1)) (Zaznam _ _ (Vyber v2))  -> compare v2 v1) vybery

jePripis :: Operace -> Bool
jePripis(Pripis x) = True
jePripis o = False

top10pripis :: Zaznamy -> [Uzivatel]
top10pripis zs = map (\(Zaznam _ j _) -> j) (take 10 pripisySerazene)
                          where
                            pripisy = filter (\(Zaznam _ _ o) -> jePripis o) zs
                            pripisySerazene = List.sortBy (\(Zaznam _ _ (Pripis v1)) (Zaznam _ _ (Pripis v2))  -> compare v1 v2) pripisy

topUzivatel :: Zaznamy -> Uzivatel
topUzivatel zs = fst $ head $ List.sortBy (\(k,v) (k',v') -> compare v' v) (StringMap.toList mapaUzivatelu)
                          where mapaUzivatelu = foldr (\(Zaznam c j o) mapa -> StringMap.insertWith (\n s -> s+1) j 1 mapa) StringMap.empty zs

zpracuj :: StringMap.Key -> Operace -> StringMap.StringMap Castka -> StringMap.StringMap Castka
zpracuj u (Vyber x) mapa = StringMap.insertWith (\n s -> s - x) u (-x) mapa
zpracuj u (Pripis x) mapa = StringMap.insertWith (\n s -> s + x) u x mapa
zpracuj u o mapa = StringMap.insertWith (\n s -> s) u 0 mapa

topPrirustek :: Zaznamy -> Uzivatel
topPrirustek zs = fst $ head $ List.sortBy (\(k,v) (k',v') -> compare v' v) (StringMap.toList mapaUzivatelu)
                        where mapaUzivatelu = foldr (\(Zaznam c j o) mapa -> zpracuj j o mapa) StringMap.empty zs

jmenoOdJ :: Uzivatel -> Bool
jmenoOdJ j = head j == 'J'

-- prumer vyberu spocitejte zaokrouhleny dolu
prumerVyberuJ :: Zaznamy -> Castka
prumerVyberuJ zs = floor $ fromIntegral (sum $ map snd uzivateleVybery) / fromIntegral (List.genericLength uzivateleVybery)
                            where uzivateleVybery = map (\(Zaznam c j (Vyber x)) -> (j,x)) $ filter (\(Zaznam c j o) -> jmenoOdJ j && jeVyber o) zs

seradVzestupnePodleCasu :: Zaznamy -> Zaznamy
seradVzestupnePodleCasu = List.sortBy (\(Zaznam c _ _) (Zaznam c' _ _) ->compare c c') 

seradSestupnePodleCasu :: Zaznamy -> Zaznamy
seradSestupnePodleCasu = List.sortBy (\(Zaznam c _ _) (Zaznam c' _ _) ->compare c' c) 

nejdelsiSingleRun :: Zaznamy -> Uzivatel
nejdelsiSingleRun zs = fst $ head serazenaJmenaVyskyt
                                  where 
                                    zaznamyPodleCasu = seradVzestupnePodleCasu zs
                                    jmenaPodleCasu = map (\(Zaznam _ j _) -> j) zaznamyPodleCasu
                                    jmenaGrouped = List.groupBy (\j j' -> j == j') jmenaPodleCasu
                                    jmenaVyskyt = Set.toList $ foldr (\group mnozina -> Set.insert (head group, length group) mnozina) Set.empty jmenaGrouped
                                    serazenaJmenaVyskyt = List.sortBy (\(j,v) (j',v') -> compare v' v) jmenaVyskyt

petiPismeneJmeno :: Uzivatel -> Bool
petiPismeneJmeno j = length j == 5

jePrihlaseni :: Operace -> Bool
jePrihlaseni Prihlaseni = True
jePrihlaseni o = False

jeOdhlaseni :: Operace -> Bool
jeOdhlaseni Odhlaseni = True
jeOdhlaseni o = False

extractCas :: Maybe Zaznam -> Cas
extractCas (Just (Zaznam c _ _)) = c

jePrihlasen :: Cas -> Uzivatel -> [Zaznam] -> [Zaznam] -> Bool
jePrihlasen c j prihlaseni odhlaseni | posledniPrihlaseni == Nothing      = False
                                                               | posledniOdhlaseni == Nothing      = True
                                                               | otherwise                                           =  extractCas posledniPrihlaseni < extractCas posledniOdhlaseni
                                  where 
                                    posledniPrihlaseni = List.find (\(Zaznam c' j' _) -> j == j' && c' < c) prihlaseni
                                    posledniOdhlaseni = List.find (\(Zaznam c' j' _) -> j == j' && c' < c) odhlaseni

neprihlaseneVybery :: Zaznamy -> Integer
neprihlaseneVybery zs = pocetNeprihlasenychVyberu
                                    where 
                                      zaznamyPetPodleCasu = filter (\(Zaznam c j o) -> petiPismeneJmeno j) (seradSestupnePodleCasu zs)
                                      prihlaseni = filter (\(Zaznam c j o) -> jePrihlaseni o) zaznamyPetPodleCasu
                                      odhlaseni = filter (\(Zaznam c j o) -> jeOdhlaseni o) zaznamyPetPodleCasu
                                      vybery = filter (\(Zaznam c j o) -> jeVyber o) zaznamyPetPodleCasu
                                      pocetNeprihlasenychVyberu = foldr (\(Zaznam c j _) a -> 
                                        case not(jePrihlasen c j prihlaseni odhlaseni) of
                                                True -> a + 1 --neni prihlasen
                                                False -> a      --je prihlasen
                                        ) 0 vybery
