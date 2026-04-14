library(tidyverse) 

df <- readRDS("data/HU_speeches.rds") %>% 
  filter(
    electoral_cycle %in% c("1998-2002", "2002-2006", "2006-2010", "2010-2014", "2014-2018"),
    chair == 0 # Elnökök kiszűrése
  ) %>% 
  # A link végének levágása a 'PairProxy_INSTANCE' mentén a párosításhoz
  mutate(link_end = str_split_i(link, 'PairProxy_INSTANCE', 3))


df2 <- readRDS("data/Corpus_speeches_hungary.RDS") %>%
  as_tibble() %>%
  filter(
    period %in% c("1998-2002", "2002-2006", "2006-2010", "2010-2014", "2014-2018"),
    chair == FALSE
  ) %>%
  mutate(link_end = str_split_i(link, 'PairProxy_INSTANCE', 3))


merged_df <- df %>%
  inner_join(
    df2, 
    by = "link_end", 
    suffix = c("_df1", "_df2") 
  )


print(paste("Sikeresen párosított sorok száma:", nrow(merged_df)))

print("--- Minta az egyesített adatokból ---")
set.seed(123) 
check_sample <- merged_df %>%
  slice_sample(n = 5) %>%
  select(link_end, speaker_df2, party, major_topic) 
print(check_sample)

final_export <- merged_df %>%
  select(
    date = date_df2,        
    electoral_cycle,        
    speaker = speaker_df2,  
    party = party,          
    partyfacts_ID,
    agenda = agenda_df2,    
    major_topic,            
    speech_text = text,     
    link_end                
  )

saveRDS(final_export, "data/HU_parlament_vegleges_link_alapjan.rds")
print("A végleges, egyesített adatbázis sikeresen elmentve!")
