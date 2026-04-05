# --- Könyvtárak betöltése ---
library(tidyverse)

# --- Tisztított adatok beolvasása ---
df <- readRDS("data/vegleges.rds")

# ==============================================================================
# DESKRIPTÍV STATISZTIKA
# ==============================================================================

# --- 1. A korpusz mérete ---
osszes_beszed <- nrow(df)
print(paste("A tisztított korpusz mérete:", osszes_beszed, "db felszólalás."))

# --- 2. Ciklusonkénti megoszlás ---
stat_ciklus <- df %>%
  count(electoral_cycle, name = "db")

print("--- Felszólalások ciklusonként ---")
print(stat_ciklus)

# --- 3. Pártonkénti megoszlás ---
stat_part <- df %>%
  count(party, name = "db") %>%
  arrange(desc(db)) 

print("--- Felszólalások pártonként ---")
print(stat_part)