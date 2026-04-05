# ==============================================================================
# EU-KRITIKUS DISKURZUS ÉS TÉMA-ELEMZÉS
# ==============================================================================

# --- Könyvtárak betöltése ---
library(tidyverse)
library(scales) 

# --- Tisztított adatok beolvasása ---
df <- readRDS("data/vegleges.rds")


# ==============================================================================
# 1. FLAGELÉS ÉS ÚJ KORPUSZ LÉTREHOZÁSA (CSAK EU-S BESZÉDEK)
# ==============================================================================

# --- Szólista és regex minták ---
eunegativszolista <- c("akadályok", "adósság", "vészhelyzet", "bűn") 
eu_pattern <- "\\b(eu|unió)" 

# --- Keresés és szűkített korpusz (Bázis) létrehozása ---
df_eu_context <- df %>%
  mutate(
    # 1. Alap EU keresés
    eu_basic_flag = str_detect(speech_text, regex(eu_pattern, ignore_case = TRUE)),
    
    # 2. EU + NEGATÍV együttes jelenléte
    eu_negativ_flag = str_detect(str_to_lower(speech_text), 
                                 str_to_lower(paste(eunegativszolista, collapse = "|"))) & eu_basic_flag
  ) %>%
  # Innentől minden elemzés csak ezen a szűrt táblán fut
  filter(eu_basic_flag == TRUE)

print("--- ÚJ KORPUSZ (EU KONTEXTUS) MÉRETEI ---")
print(paste("Eredeti beszédek száma:", nrow(df)))
print(paste("Szűrt (EU-s) beszédek száma (Bázis):", nrow(df_eu_context)))
print("------------------------------------------------")


# ==============================================================================
# 2. EU NEGATÍV ELEMZÉS (A szűkített korpuszon belül)
# ==============================================================================

print("--- Általános EU-kritikus statisztika ---")
eu_negativ_summary <- df_eu_context %>% 
  summarise(
    osszes_eu_beszed = n(), 
    eu_negativ_beszed_db = sum(eu_negativ_flag, na.rm = TRUE), 
    eu_negativ_szazalek = round(eu_negativ_beszed_db / osszes_eu_beszed * 100, 2)
  )
print(eu_negativ_summary)

print("--- EU-kritikus beszédek aránya ciklusonként ---")
eu_negativ_cycle <- df_eu_context %>% 
  filter(!is.na(electoral_cycle)) %>% 
  group_by(electoral_cycle) %>%
  summarise(
    osszes_eu_beszed = n(), 
    eu_negativ_beszed_db = sum(eu_negativ_flag, na.rm = TRUE), 
    eu_negativ_arany = mean(eu_negativ_flag, na.rm = TRUE)
  ) %>%
  mutate(eu_negativ_szazalek = round(eu_negativ_arany * 100, 2))
print(eu_negativ_cycle)

print("--- EU-kritikus beszédek aránya pártonként ---")
eu_negativ_party_share <- df_eu_context %>% 
  group_by(party) %>%
  summarise(
    party_all_eu_speeches = n(),
    party_eu_negativ_count = sum(eu_negativ_flag, na.rm = TRUE)
  ) %>%
  mutate(
    eu_negativ_eloszlas_a_partok_kozott = party_eu_negativ_count / sum(party_eu_negativ_count),
    eu_negativ_belso_arany = party_eu_negativ_count / party_all_eu_speeches
  ) %>%
  arrange(desc(party_all_eu_speeches))

# Kiírás a konzolra formázott százalékokkal
eu_negativ_party_share_print <- eu_negativ_party_share %>%
  mutate(
    eloszlas_pct = scales::percent(eu_negativ_eloszlas_a_partok_kozott, accuracy = 0.1),
    belso_arany_pct = scales::percent(eu_negativ_belso_arany, accuracy = 0.1)
  )
print(eu_negativ_party_share_print)


# ==============================================================================
# 3. TOPIC ELEMZÉS ("EU NEGATÍV")
# ==============================================================================

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

eu_negativ_pattern <- paste(eunegativszolista, collapse = "|")

# --- Adatok előkészítése a témastatisztikához ---
df_eu_topics <- df_eu_context %>%
  filter(major_topic != 9999) %>%
  mutate(
    topic_name = topic_labels[as.character(major_topic)],
    word_count = str_count(speech_text, "\\S+"),
    eu_negativ_count = str_count(speech_text, regex(eu_negativ_pattern, ignore_case = TRUE))
  ) %>%
  filter(!is.na(topic_name))

# --- Téma statisztikák kiszámítása ---
print("--- 'EU NEGATÍV' STATISZTIKÁK TÉMÁNKÉNT ---")
eu_negativ_stats <- df_eu_topics %>%
  group_by(topic_name) %>%
  summarise(
    osszes_beszed = n(),
    
    # Jelenlét (Hány beszédben fordult elő)
    erintett_beszed = sum(eu_negativ_flag, na.rm = TRUE),
    jelenlet_szazalek = round(erintett_beszed / osszes_beszed * 100, 2),
    
    # Intenzitás (Hányszor fordult elő 1000 szóra vetítve)
    osszes_talalat = sum(eu_negativ_count, na.rm = TRUE),
    osszes_szo = sum(word_count, na.rm = TRUE),
    intenzitas_1000 = round((osszes_talalat / osszes_szo) * 1000, 2)
  ) %>%
  arrange(desc(jelenlet_szazalek))

print(eu_negativ_stats)