library(tidyverse)
library(tidytext)
library(stringr)
library(stopwords)

df <- readRDS("data/vegleges.rds")

eurpaiuniszolista <- c(
  "keresztény Európa", "EU- alapok", "európai alapok", "európai székhelyű", 
  "közép- európai", "millió euró", "közép európaiak", "európai közlekedés", 
  "európai országok", "uniós jogszabályok"
)


df <- df %>%
  mutate(
    eu_flag = str_detect(str_to_lower(speech_text), 
                         str_to_lower(paste(eurpaiuniszolista, collapse = "|")))
  )

print("--- Összesített EU diskurzus ---")
eu_summary <- df %>%
  summarise(
    eu_beszed = sum(eu_flag, na.rm = TRUE), 
    osszes_beszed = n(), 
    eu_szazalek = round(eu_beszed / osszes_beszed * 100, 2)
  )
print(eu_summary)

print("--- EU diskurzus aránya pártonként ---")
eu_party_share <- df %>%
  group_by(party) %>%
  summarise(
    osszes_beszed = n(), 
    eu_beszed_db = sum(eu_flag, na.rm = TRUE), 
    eu_arany = eu_beszed_db / osszes_beszed
  ) %>%
  mutate(eu_szazalek = round(eu_arany * 100, 2)) %>% 
  arrange(desc(eu_arany))
print(eu_party_share)

print("--- EU diskurzus aránya ciklusonként ---")
eu_cycle <- df %>%
  filter(!is.na(electoral_cycle)) %>% 
  group_by(electoral_cycle) %>%
  summarise(
    eu_beszed_db = sum(eu_flag, na.rm = TRUE), 
    osszes_beszed = n(), 
    eu_arany = mean(eu_flag, na.rm = TRUE)
  ) %>%
  mutate(eu_szazalek = round(eu_arany * 100, 2))
print(eu_cycle)

stop_hu <- c(stopwords("hu"), "is", "ha")

tokens <- df %>%
  mutate(doc_id = row_number()) %>%
  unnest_tokens(word, speech_text) %>%
  filter(!word %in% stop_hu) %>%
  group_by(doc_id) %>%
  mutate(pos = row_number()) %>%
  ungroup()

eu_anchors <- c("eu", "unió")
eu_pattern <- paste0("\\b(", paste(eu_anchors, collapse = "|"), ")")

hits <- tokens %>%
  filter(str_detect(word, eu_pattern)) %>% 
  arrange(doc_id, pos) %>%
  group_by(doc_id) %>%
  mutate(
    diff = pos - lag(pos, default = -999),
    new_hit = diff > 1,
    hit_id = cumsum(new_hit)
  ) %>%
  ungroup()

hit_clusters <- hits %>%
  group_by(doc_id, hit_id, party, electoral_cycle) %>%
  summarise(
    start_pos = min(pos),
    end_pos = max(pos),
    eu_phrase = paste(word, collapse = " "), 
    .groups = "drop"
  )

windows <- hit_clusters %>%
  inner_join(tokens, by = c("doc_id", "party", "electoral_cycle"), relationship = "many-to-many") %>%
  filter(
    pos >= start_pos - 2,    
    pos <= end_pos + 2,      
    !(pos >= start_pos & pos <= end_pos) 
  ) %>%
  rename(word_ctx = word, pos_ctx = pos)

windows_text <- windows %>%
  arrange(doc_id, hit_id, pos_ctx) %>%
  group_by(doc_id, hit_id, party, electoral_cycle) %>%
  summarise(
    context = paste(word_ctx, collapse = " "), 
    .groups = "drop"
  ) %>%
  distinct(doc_id, hit_id, .keep_all = TRUE)

cossu_targets <- c("keresztény", "alapok", "székhelyű", "közép", "millió", "közlekedés", "országok", "jogszabályok")
cossu_pattern <- paste(cossu_targets, collapse = "|")

eu_nem_populista_szolista <- c("szocialista", "országok", "kelet", "szélesebb", "forgalom", "ügyész", "dimenzió", "vidéki", "nyelvek", "horvát")
nem_pop_pattern <- paste(eu_nem_populista_szolista, collapse = "|")

windows_text <- windows_text %>%
  mutate(
    cossu_hit = str_detect(context, cossu_pattern),
    nem_pop_hit = str_detect(context, nem_pop_pattern) 
  )

rates <- windows_text %>%
  group_by(party, electoral_cycle) %>%
  summarise(
    eu_mentions = n(),                  
    cossu_mentions = sum(cossu_hit),    
    cossu_rate = mean(cossu_hit),
    nem_pop_mentions = sum(nem_pop_hit), 
    nem_pop_rate = mean(nem_pop_hit),    
    .groups = "drop"
  )

print("--- COSSU Eredmények táblázata ---")
print(rates)

summary_stats <- rates %>%
  filter(party %in% c("Fidesz", "MSZP")) %>%
  group_by(party) %>%
  summarise(
    osszes_eu_emlites = sum(eu_mentions),
    osszes_cossu_emlites = sum(cossu_mentions),
    osszes_nem_pop_emlites = sum(nem_pop_mentions) 
  )

print("--- Összesített COSSU statisztika (Fidesz & MSZP) ---")
print(summary_stats)

top_words <- windows %>%
  filter(!str_detect(word_ctx, eu_pattern)) %>% 
  count(party, word_ctx, sort = TRUE)

top20 <- top_words %>%
  group_by(party) %>%
  slice_max(n, n = 20) %>%
  ungroup()

print("--- Leggyakoribb kontextus-szavak (Top 20 pártonként) ---")
print(top20, n = 40)
