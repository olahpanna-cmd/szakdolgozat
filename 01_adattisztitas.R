# --- Könyvtárak betöltése ---
library(tidyverse)
library(scales) 
library(lubridate) 

# --- Adatok beolvasása ---
df <- readRDS("data/HU_parlament_vegleges_link_alapjan.rds")

# --- Technikai szűrőfeltételek definiálása ---
technikai_kifejezesek <- paste(
  c("ügyrendi", 
    "mandátumigazolás tárgyalása", 
    "ülésnap megnyitása", 
    "eskütétel", 
    "Az ülés napirendjének megállapítása", 
    "Bejelentések", 
    "Küldöttség üdvözlése"),
  collapse = "|"
)

# --- Adattisztítási pipeline ---
df_clean <- df %>%
  filter(
    !is.na(party),                                                          # Hiányzó pártadatok kiszűrése
    party != "független",                                                   # Függetlenek kiszűrése
    !str_detect(party, "(?i)nemzetiségi"),                                  # Nemzetiségi képviselők kiszűrése
    !str_detect(agenda, regex(technikai_kifejezesek, ignore_case = TRUE))   # Technikai napirendi pontok eldobása
  )

# --- Tisztított adatbázis mentése ---
saveRDS(df_clean, "data/vegleges.rds")