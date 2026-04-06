# szakdolgozat
R kódok a szakdolgozatomhoz: SZÓTÁRALAPÚ MEGKÖZELÍTÉSEK A POPULISTA RETORIKA VIZSGÁLATÁRA AZ 1998-2018 KÖZÖTT ELHANGZOTT PARLAMENTI FELSZÓLALÁSOK KVANTITATÍV ELEMZÉSÉVEL

## A repository felépítése 

Az elemzés logikai sorrendben az alábbi fájlokra van bontva:

* `00_adatbazisok_egyesitese.R`: Különböző forrásból származó (témakódolt és pártkódolt) nyers parlamenti korpuszok összekapcsolása URL fragmentumok alapján.
* `01_adattisztitas.R`: A nyers korpusz szűrése, független és nemzetiségi képviselők, valamint technikai napirendi pontok eltávolítása.
* `02_deskriptiv_stat.R`: Alapvető leíró statisztikák (felszólalások száma pártok és ciklusok szerint).
* `03_szovegelemzes_es_cossu.R`: EU populista diskurzus azonosítása és a kontextus elemzés (EU említése +/- 2 szavas ablakkal).
* `04_eu_kritikus_diskurzus_es_temak.R`: Negatív kontextusú EU-s beszédek szűrése és vizsgálata a különböző szakpolitikai témákban (topics).
* `05_fidesz_diskurzus.R`: A Fidesz-specifikus populista kifejezések intenzitásának elemzése a korpuszon belül.
* `06_temak_es_migracio.R`: A diskurzus alakulása a különböző témákban, különös tekintettel a migrációs vitákra (2010 utáni ciklusok).
* `07_elitellenesseg_es_pauwels.R`: Az elitellenes és a Pauwels-féle populizmus szótár lefuttatása, sűrűség (intenzitás) számítása 1000 szóra vetítve.

## Reprodukálhatóság és futtatás

A kódok futtatásához a következő R csomagok telepítése szükséges:
`tidyverse`, `scales`, `lubridate`, `tidytext`, `stringr`, `stopwords`, `dbplyr`

**Adatstruktúra és források:**
A kódok relatív elérési utakat használnak. Az elemzéshez használt parlamenti adatbázisok publikusan elérhetőek, a pontos források és a hivatkozások a szakdolgozatban találhatóak. 

A fenti kódok reprodukálásához a letöltött adatbázist `vegleges.rds` néven egy létrehozott `data/` mappába kell helyezni, az alábbi struktúra szerint:
```text
project_mappa/
├── data/
│   └── vegleges.rds    
├── 00_adatbazisok_egyesitese.R
├── 01_adattisztitas.R
...
```
