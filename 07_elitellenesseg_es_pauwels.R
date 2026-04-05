# ==============================================================================
# ELITELLENESSÉG ÉS PAUWELS-FÉLE POPULIZMUS ELEMZÉSE
# ==============================================================================

# --- Könyvtárak betöltése ---
library(tidyverse)
library(scales)

# --- Tisztított adatok beolvasása ---
df <- readRDS("data/vegleges.rds")


# ==============================================================================
# 1. SZÓTÁRAK ÉS ADATELŐKÉSZÍTÉS (KÖZÖS SZÁMOLÁS)
# ==============================================================================

# --- Közös kifejezések (mindkét szótár használja) ---
elit_formak <- "\\b(elit)"
csal_formak <- "\\b(csalás|csalnak|csaló|csal|csalással|csalást|csalni|csalók|csaltak|csalásra|csalási|csalt|csalókat)\\b"

# --- 1. Elitellenes szótár felépítése ---
elit_tovabbi_szavak <- c("konszenzus", "demokráciaellenes", "korrupt", "propaganda", 
                         "elárul", "szégyen", "botrány", "igazság", "hazug", 
                         "népszavazás", "tisztességtelen", "megtéveszt", "becsap", "politikus")
elitellenesszolista <- paste(c(elit_formak, csal_formak, elit_tovabbi_szavak), collapse = "|")

# --- 2. Pauwels szótár felépítése ---
nep_formak <- "\\b(nép|népnek|népet|népe|népét|néppel|népünk|néptől|néphez|népben|népünket|népért|népről|népünknek|népem|népemet)\\b"
osztaly_formak <- "\\b(osztály|osztályok|osztályban|osztályt|osztályba)\\b"
kaszt_formak <- "\\b(kaszt)"
iger_formak <- "\\b(ígér)"
pauwels_tovabbi_szavak <- c("elárul", "abszurd", "arrogáns", "pártokrácia", "párturalom", 
                            "korrupt", "közvetlen", "fennálló rendszer", "uralkodó", 
                            "maffia", "szólásszabadság", "demokráciaellenes", 
                            "politikus", "propaganda", "népszavazás", "rezsim", 
                            "szégyentelen", "szégyen", "beismer", "hagyomány")
pauwelsszolista <- paste(c(nep_formak, csal_formak, osztaly_formak, elit_formak, kaszt_formak, iger_formak, pauwels_tovabbi_szavak), collapse = "|")

# --- Témák címkézése ---
topic_labels <- c(
  "1" = "Makrogazdaság", "2" = "Polgári jogok", "3" = "Egészségügy",
  "4" = "Mezőgazdaság", "5" = "Munkaügy", "6" = "Oktatás",
  "7" = "Környezetvédelem", "8" = "Energia", "9" = "Migráció",
  "10" = "Közlekedés", "12" = "Jog és bűnözés", "13" = "Szociális ellátás",
  "14" = "Lakhatás", "15" = "Belföldi kereskedelem", "16" = "Védelem / hadügy",
  "17" = "Technológia", "18" = "Külkereskedelem", "19" = "Külpolitika",
  "20" = "Kormányzati működés", "21" = "Közterületek", "23" = "Kultúra"
)

df <- df %>%
  mutate(
    word_count = str_count(speech_text, "\\S+"),
    topic_name = if("major_topic" %in% names(.)) topic_labels[as.character(major_topic)] else NA,
    
    # Elitellenes mutatók
    elit_db = str_count(str_to_lower(speech_text), elitellenesszolista),
    elit_flag = elit_db > 0,
    
    # Pauwels mutatók
    pauwels_db = str_count(str_to_lower(speech_text), pauwelsszolista),
    pauwels_flag = pauwels_db > 0
  )


# ==============================================================================
# 2. ELITELLENESSÉG ELEMZÉSE (Alap és Intenzitás)
# ==============================================================================

print("========== ELITELLENESSÉG EREDMÉNYEK ==========")

print("--- 1. ÖSSZESÍTÉS (Elitellenes) ---")
print(df %>% summarise(
  elit_beszed_db = sum(elit_flag, na.rm = TRUE), 
  osszes_beszed = n(), 
  elit_szazalek = round(elit_beszed_db / osszes_beszed * 100, 2)
))

print("--- 2. PÁRTONKÉNTI ARÁNY ÉS INTENZITÁS (Elitellenes) ---")
print(df %>%
        group_by(party) %>%
        summarise(
          osszes_beszed = n(),
          elit_beszed_db = sum(elit_flag, na.rm = TRUE),
          jelenlet_szazalek = round((elit_beszed_db / osszes_beszed) * 100, 2),
          intenzitas_1000 = round((sum(elit_db, na.rm = TRUE) / sum(word_count, na.rm = TRUE)) * 1000, 2)
        ) %>% arrange(desc(intenzitas_1000))
)

print("--- 3. CIKLUSONKÉNTI ARÁNY ÉS INTENZITÁS (Elitellenes) ---")
print(df %>%
        filter(!is.na(electoral_cycle)) %>%
        group_by(electoral_cycle) %>%
        summarise(
          osszes_beszed = n(),
          elit_beszed_db = sum(elit_flag, na.rm = TRUE),
          jelenlet_szazalek = round((elit_beszed_db / osszes_beszed) * 100, 2),
          intenzitas_1000 = round((sum(elit_db, na.rm = TRUE) / sum(word_count, na.rm = TRUE)) * 1000, 2)
        )
)

print("--- 4. TÉMÁNKÉNTI INTENZITÁS (Elitellenes) ---")
print(df %>%
        filter(major_topic != 9999, !is.na(topic_name)) %>%
        group_by(topic_name) %>%
        summarise(
          osszes_beszed = n(),
          erintett_beszed = sum(elit_flag, na.rm = TRUE),
          jelenlet_szazalek = round((erintett_beszed / osszes_beszed) * 100, 2),
          intenzitas_1000 = round((sum(elit_db, na.rm = TRUE) / sum(word_count, na.rm = TRUE)) * 1000, 2)
        ) %>% arrange(desc(intenzitas_1000))
)


# ==============================================================================
# 3. PAUWELS POPULIZMUS ELEMZÉSE (Alap és Intenzitás)
# ==============================================================================

print("========== PAUWELS-FÉLE POPULIZMUS EREDMÉNYEK ==========")

print("--- 1. ÖSSZESÍTÉS (Pauwels) ---")
print(df %>% summarise(
  pauwels_beszed_db = sum(pauwels_flag, na.rm = TRUE), 
  osszes_beszed = n(), 
  pauwels_szazalek = round(pauwels_beszed_db / osszes_beszed * 100, 2)
))

print("--- 2. PÁRTONKÉNTI ARÁNY ÉS INTENZITÁS (Pauwels) ---")
print(df %>%
        group_by(party) %>%
        summarise(
          osszes_beszed = n(),
          pauwels_beszed_db = sum(pauwels_flag, na.rm = TRUE),
          jelenlet_szazalek = round((pauwels_beszed_db / osszes_beszed) * 100, 2),
          intenzitas_1000 = round((sum(pauwels_db, na.rm = TRUE) / sum(word_count, na.rm = TRUE)) * 1000, 2)
        ) %>% arrange(desc(intenzitas_1000))
)

print("--- 3. CIKLUSONKÉNTI ARÁNY ÉS INTENZITÁS (Pauwels) ---")
print(df %>%
        filter(!is.na(electoral_cycle)) %>%
        group_by(electoral_cycle) %>%
        summarise(
          osszes_beszed = n(),
          pauwels_beszed_db = sum(pauwels_flag, na.rm = TRUE),
          jelenlet_szazalek = round((pauwels_beszed_db / osszes_beszed) * 100, 2),
          intenzitas_1000 = round((sum(pauwels_db, na.rm = TRUE) / sum(word_count, na.rm = TRUE)) * 1000, 2)
        )
)

print("--- 4. TÉMÁNKÉNTI INTENZITÁS (Pauwels) ---")
print(df %>%
        filter(major_topic != 9999, !is.na(topic_name)) %>%
        group_by(topic_name) %>%
        summarise(
          osszes_beszed = n(),
          erintett_beszed = sum(pauwels_flag, na.rm = TRUE),
          jelenlet_szazalek = round((erintett_beszed / osszes_beszed) * 100, 2),
          intenzitas_1000 = round((sum(pauwels_db, na.rm = TRUE) / sum(word_count, na.rm = TRUE)) * 1000, 2)
        ) %>% arrange(desc(intenzitas_1000))
)