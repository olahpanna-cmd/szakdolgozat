# ==============================================================================
# FIDESZ-SPECIFIKUS SZÓTÁR ÉS TARTALOMELEMZÉS
# ==============================================================================

# --- Könyvtárak betöltése ---
library(tidyverse)
library(scales)

# --- Tisztított adatok beolvasása ---
df <- readRDS("data/vegleges.rds")


# ==============================================================================
# 1. SZÓTÁRAK ÉS ADATELŐKÉSZÍTÉS (EGYSZERI, GYORSÍTOTT SZÁMOLÁS)
# ==============================================================================

# --- A Fidesz szótár beállítása ---
szent_formak <- "\\b(szent|szentek|szentet|szentnek|szentté|szentül|szenthez|szentből|szentben|szenteknek|szenteket)\\b"
tovabbi_fidesz_szavak <- c("kedvesebb", "úriember", "húsz", "elmúlt", 
                           "csendőrség", "vasárnap", "idegenforgalom", 
                           "jövőbeli", "megtámadták")

# Itt hozzuk létre a fideszszolista objektumot
fideszszolista <- paste(c(szent_formak, tovabbi_fidesz_szavak), collapse = "|")

# --- Téma címkék betöltése ---
topic_labels <- c(
  "1" = "Makrogazdaság", "2" = "Polgári jogok", "3" = "Egészségügy",
  "4" = "Mezőgazdaság", "5" = "Munkaügy", "6" = "Oktatás",
  "7" = "Környezetvédelem", "8" = "Energia", "9" = "Migráció",
  "10" = "Közlekedés", "12" = "Jog és bűnözés", "13" = "Szociális ellátás",
  "14" = "Lakhatás", "15" = "Belföldi kereskedelem", "16" = "Védelem / hadügy",
  "17" = "Technológia", "18" = "Külkereskedelem", "19" = "Külpolitika",
  "20" = "Kormányzati működés", "21" = "Közterületek", "23" = "Kultúra"
)

# --- Alapmutatók kiszámítása a TELJES adatbázisra ---
df <- df %>%
  mutate(
    # Szószám kiszámítása minden beszédre
    word_count = str_count(speech_text, "\\S+"),
    
    # Fidesz kifejezések számolása (kisbetűsítve) a fideszszolista alapján
    fidesz_db = str_count(str_to_lower(speech_text), fideszszolista),
    
    # Flag: van-e benne egyáltalán?
    fidesz_flag = fidesz_db > 0,
    
    # Témák nevesítése (ha van major_topic oszlop)
    topic_name = if("major_topic" %in% names(.)) topic_labels[as.character(major_topic)] else NA
  )


# ==============================================================================
# 2. KERESZT-PÁRT STATISZTIKÁK (MINDEN PÁRT)
# ==============================================================================

print("--- FIDESZ CSOMAG TELJES STATISZTIKA PÁRTONKÉNT (Összes év) ---")
party_total_stat <- df %>%
  group_by(party) %>%
  summarise(
    osszes_beszed_szoszam = sum(word_count, na.rm = TRUE),  
    osszes_talalat_db = sum(fidesz_db, na.rm = TRUE)              
  ) %>%
  mutate(intenzitas_1000_szora = round((osszes_talalat_db / osszes_beszed_szoszam) * 1000, 2)) %>%
  arrange(desc(intenzitas_1000_szora)) 
print(party_total_stat)

print("--- FIDESZ-CSOMAG INTENZITÁSA PÁRTONKÉNT ÉS CIKLUSONKÉNT (db / 1000 szó) ---")
party_freq_cycle <- df %>%
  filter(!is.na(electoral_cycle)) %>% 
  group_by(party, electoral_cycle) %>%
  summarise(
    osszes_szo = sum(word_count, na.rm = TRUE),
    talalatok = sum(fidesz_db, na.rm = TRUE),
    .groups = 'drop' 
  ) %>%
  mutate(freq_per_1k = round((talalatok / osszes_szo) * 1000, 2)) %>%
  arrange(electoral_cycle, desc(freq_per_1k)) 
print(party_freq_cycle)


# ==============================================================================
# 3. FIDESZ-SPECIFIKUS STATISZTIKÁK (CSAK FIDESZ)
# ==============================================================================

df_fidesz <- df %>% filter(party == "Fidesz")

print("--- FIDESZ CSOMAG ÖSSZESÍTÉS (Csak Fidesz beszédek) ---")
fidesz_summary <- df_fidesz %>%
  summarise(
    fidesz_beszed_db = sum(fidesz_flag, na.rm = TRUE), 
    osszes_fidesz_beszed = n(), 
    fidesz_szazalek = round(fidesz_beszed_db / osszes_fidesz_beszed * 100, 2)
  )
print(fidesz_summary)

print("--- FIDESZ CSOMAG CIKLUSONKÉNTI ARÁNY ÉS INTENZITÁS ---")
fidesz_cycle_stat <- df_fidesz %>%
  filter(!is.na(electoral_cycle)) %>%
  group_by(electoral_cycle) %>%
  summarise(
    osszes_beszed = n(),
    erintett_beszed_db = sum(fidesz_flag, na.rm = TRUE),
    jelenlet_szazalek = round(erintett_beszed_db / osszes_beszed * 100, 2),
    osszes_szo = sum(word_count, na.rm = TRUE),
    talalatok_db = sum(fidesz_db, na.rm = TRUE),
    intenzitas_1000_szora = round((talalatok_db / osszes_szo) * 1000, 2)
  )
print(fidesz_cycle_stat)


# ==============================================================================
# 4. TÉMA-SPECIFIKUS STATISZTIKÁK (FIDESZ BESZÉDEK)
# ==============================================================================

print("--- FIDESZ-CSOMAG STATISZTIKÁK TÉMÁNKÉNT ---")
fidesz_topic_stats <- df_fidesz %>%
  filter(!is.na(topic_name)) %>%
  group_by(topic_name) %>%
  summarise(
    osszes_beszed = n(),
    erintett_beszed = sum(fidesz_flag, na.rm = TRUE),
    jelenlet_szazalek = round(erintett_beszed / osszes_beszed * 100, 2),
    osszes_talalat = sum(fidesz_db, na.rm = TRUE),
    osszes_szo = sum(word_count, na.rm = TRUE),
    intenzitas_1000 = round((osszes_talalat / osszes_szo) * 1000, 2)
  ) %>%
  arrange(desc(intenzitas_1000))
print(fidesz_topic_stats)


# ==============================================================================
# 5. MIGRÁCIÓS FÓKUSZ (FIDESZ, 2010 UTÁN)
# ==============================================================================

df_fidesz_migracio <- df_fidesz %>%
  filter(topic_name == "Migráció", electoral_cycle > 2010)

print("--- FIDESZ-CSOMAG: MIGRÁCIÓS TÉMA ÖSSZESÍTVE (2010 UTÁN) ---")
fidesz_migracio_total_stat <- df_fidesz_migracio %>%
  summarise(
    osszes_beszed = n(),
    erintett_beszed = sum(fidesz_flag, na.rm = TRUE),
    jelenlet_szazalek = round((erintett_beszed / osszes_beszed) * 100, 2),
    osszes_szo = sum(word_count, na.rm = TRUE),
    talalatok = sum(fidesz_db, na.rm = TRUE),
    intenzitas_1000 = round((talalatok / osszes_szo) * 1000, 2)
  )
print(fidesz_migracio_total_stat)

print("--- FIDESZ-CSOMAG: MIGRÁCIÓS TÉMA CIKLUSONKÉNT (2010 UTÁN) ---")
fidesz_migracio_ciklusonkent <- df_fidesz_migracio %>%
  group_by(electoral_cycle) %>%
  summarise(
    osszes_beszed = n(),
    erintett_beszed = sum(fidesz_flag, na.rm = TRUE),
    jelenlet_szazalek = round((erintett_beszed / osszes_beszed) * 100, 2),
    osszes_szo = sum(word_count, na.rm = TRUE),
    talalatok = sum(fidesz_db, na.rm = TRUE),
    intenzitas_1000 = round((talalatok / osszes_szo) * 1000, 2),
    .groups = 'drop'
  )
print(fidesz_migracio_ciklusonkent)