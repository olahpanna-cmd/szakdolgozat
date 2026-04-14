library(tidyverse)
library(scales)

df <- readRDS("data/vegleges.rds")


eurpaiuniszolista <- paste(c("keresztény Európa", "EU- alapok", "európai alapok", "európai székhelyű", 
                             "közép- európai", "millió euró", "közép európaiak", "európai közlekedés", 
                             "európai országok", "uniós jogszabályok"), collapse = "|")

eunegativszolista <- paste(c("akadályok", "adósság", "vészhelyzet", "bűn"), collapse = "|")

fidesz_szavak <- c("\\b(szent|szentek|szentet|szentnek|szentté|szentül|szenthez|szentből|szentben|szenteknek|szenteket)\\b",
                   "kedvesebb", "úriember", "húsz", "elmúlt", "csendőrség", "vasárnap", 
                   "idegenforgalom", "jövőbeli", "megtámadták")
fideszszolista <- paste(fidesz_szavak, collapse = "|")

eu_pattern <- "\\b(eu|európai unió|európai|európa|unió|uniós)\\b"

topic_labels <- c(
  "1" = "Makrogazdaság", "2" = "Polgári jogok", "3" = "Egészségügy",
  "4" = "Mezőgazdaság", "5" = "Munkaügy", "6" = "Oktatás",
  "7" = "Környezetvédelem", "8" = "Energia", "9" = "Migráció",
  "10" = "Közlekedés", "12" = "Jog és bűnözés", "13" = "Szociális ellátás",
  "14" = "Lakhatás", "15" = "Belföldi kereskedelem", "16" = "Védelem / hadügy",
  "17" = "Technológia", "18" = "Külkereskedelem", "19" = "Külpolitika",
  "20" = "Kormányzati működés", "21" = "Közterületek", "23" = "Kultúra"
)


df_topics <- df %>%
  filter(major_topic != 9999) %>%
  mutate(
    topic_name = topic_labels[as.character(major_topic)],
    word_count = str_count(speech_text, "\\S+")
  ) %>%
  filter(!is.na(topic_name))

df_eu_topics <- df_topics %>%
  mutate(eu_basic_flag = str_detect(speech_text, regex(eu_pattern, ignore_case = TRUE))) %>%
  filter(eu_basic_flag == TRUE) %>%
  mutate(
    eupozitiv_count = str_count(str_to_lower(speech_text), eupozitivszolista),
    eunegativ_count = str_count(str_to_lower(speech_text), eunegativszolista),
    has_eupozitiv = eupozitiv_count > 0,
    has_eunegativ = eunegativ_count > 0
  )

print("--- EU POZITÍV STATISZTIKÁK TÉMÁNKÉNT (Bázis: EU-s beszédek) ---")
eupozitiv_stats <- df_eu_topics %>%
  group_by(topic_name) %>%
  summarise(
    osszes_beszed = n(),
    erintett_beszed = sum(has_eupozitiv, na.rm = TRUE),
    jelenlet_szazalek = round(erintett_beszed / osszes_beszed * 100, 2),
    intenzitas_1000 = round((sum(eupozitiv_count, na.rm = TRUE) / sum(word_count, na.rm = TRUE)) * 1000, 2)
  ) %>% arrange(desc(jelenlet_szazalek))
print(eupozitiv_stats)

print("--- EU NEGATÍV STATISZTIKÁK TÉMÁNKÉNT (EU-s beszédek) ---")
eunegativ_stats <- df_eu_topics %>%
  group_by(topic_name) %>%
  summarise(
    osszes_beszed = n(),
    erintett_beszed = sum(has_eunegativ, na.rm = TRUE),
    jelenlet_szazalek = round(erintett_beszed / osszes_beszed * 100, 2),
    intenzitas_1000 = round((sum(eunegativ_count, na.rm = TRUE) / sum(word_count, na.rm = TRUE)) * 1000, 2)
  ) %>% arrange(desc(jelenlet_szazalek))
print(eunegativ_stats)


df_migration_2010 <- df_topics %>%
  filter(
    topic_name == "Migráció",
    !party %in% c("FKgP", "MDF", "SZDSZ"),
    !is.na(electoral_cycle),
    !electoral_cycle %in% c("1998-2002", "2002-2006", "2006-2010")
  )

df_mig_populista <- df_migration_2010 %>%
  mutate(
    eu_count = str_count(str_to_lower(speech_text), eurpaiuniszolista),
    has_eu = eu_count > 0
  )

print("--- SIMA EU (POPULISTA) A MIGRÁCIÓBAN PÁRTONKÉNT (2010 után) ---")
print(df_mig_populista %>%
        group_by(party) %>%
        summarise(
          osszes_beszed = n(),
          jelenlet_szazalek = round(sum(has_eu, na.rm=TRUE) / osszes_beszed * 100, 2),
          intenzitas_1000 = round((sum(eu_count, na.rm=TRUE) / sum(word_count, na.rm=TRUE)) * 1000, 2)
        ) %>% arrange(desc(jelenlet_szazalek)))

print("--- SIMA EU (POPULISTA) A MIGRÁCIÓBAN CIKLUSONKÉNT (2010 után) ---")
print(df_mig_populista %>%
        group_by(electoral_cycle) %>%
        summarise(
          osszes_beszed = n(),
          jelenlet_szazalek = round(sum(has_eu, na.rm=TRUE) / osszes_beszed * 100, 2),
          intenzitas_1000 = round((sum(eu_count, na.rm=TRUE) / sum(word_count, na.rm=TRUE)) * 1000, 2)
        ))

df_mig_eunegativ <- df_migration_2010 %>%
  mutate(eu_basic_flag = str_detect(speech_text, regex(eu_pattern, ignore_case = TRUE))) %>%
  filter(eu_basic_flag == TRUE) %>%
  mutate(
    eunegativ_count = str_count(str_to_lower(speech_text), eunegativszolista),
    has_eunegativ = eunegativ_count > 0
  )

print("--- EU NEGATÍV A MIGRÁCIÓBAN PÁRTONKÉNT (2010 után, EU) ---")
print(df_mig_eunegativ %>%
        group_by(party) %>%
        summarise(
          osszes_beszed = n(),
          jelenlet_szazalek = round(sum(has_eunegativ, na.rm=TRUE) / osszes_beszed * 100, 2),
          intenzitas_1000 = round((sum(eunegativ_count, na.rm=TRUE) / sum(word_count, na.rm=TRUE)) * 1000, 2)
        ) %>% arrange(desc(jelenlet_szazalek)))


df_mig_fidesz <- df_migration_2010 %>%
  filter(party == "Fidesz") %>%
  mutate(
    fidesz_count = str_count(str_to_lower(speech_text), fideszszolista),
    has_fidesz = fidesz_count > 0
  )

print("--- FIDESZ-CSOMAG A MIGRÁCIÓBAN CIKLUSONKÉNT (2010 után) ---")
print(df_mig_fidesz %>%
        group_by(electoral_cycle) %>%
        summarise(
          osszes_beszed = n(),
          jelenlet_szazalek = round(sum(has_fidesz, na.rm=TRUE) / osszes_beszed * 100, 2),
          intenzitas_1000 = round((sum(fidesz_count, na.rm=TRUE) / sum(word_count, na.rm=TRUE)) * 1000, 2)
        ))

print("--- FIDESZ-CSOMAG A MIGRÁCIÓBAN ÖSSZESÍTVE (2010 után) ---")
print(df_mig_fidesz %>%
        summarise(
          osszes_beszed = n(),
          jelenlet_szazalek = round(sum(has_fidesz, na.rm=TRUE) / osszes_beszed * 100, 2),
          intenzitas_1000 = round((sum(fidesz_count, na.rm=TRUE) / sum(word_count, na.rm=TRUE)) * 1000, 2)
        ))
