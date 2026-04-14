library(tidyverse)
library(scales) 
library(lubridate) 

df <- readRDS("data/HU_parlament_vegleges_link_alapjan.rds")

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

df_clean <- df %>%
  filter(
    !is.na(party),                                                          
    party != "független",                                                   
    !str_detect(party, "(?i)nemzetiségi"),                                  
    !str_detect(agenda, regex(technikai_kifejezesek, ignore_case = TRUE))   
  )

saveRDS(df_clean, "data/vegleges.rds")
