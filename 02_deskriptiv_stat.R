library(tidyverse)

df <- readRDS("data/vegleges.rds")

osszes_beszed <- nrow(df)
print(paste("A tisztított korpusz mérete:", osszes_beszed, "db felszólalás."))

stat_ciklus <- df %>%
  count(electoral_cycle, name = "db")

print("--- Felszólalások ciklusonként ---")
print(stat_ciklus)

stat_part <- df %>%
  count(party, name = "db") %>%
  arrange(desc(db)) 

print("--- Felszólalások pártonként ---")
print(stat_part)
