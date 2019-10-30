## PREP POKEMON DATA FOR ANALYSIS AND OTHER THINGS ##

#### PACKAGES ####
library(here)
library(tidyverse)


#### READ IN DATA ####
# from https://www.kaggle.com/rounakbanik/pokemon
pokemon_t000 <- read_csv(here("data", "pokemon.csv"))

#### INITIAL PREP ####
pokemon_t001 <- pokemon_t000 %>%
         # misspelling
  rename(classification = classfication) %>%
         # pokemon with incorrect/extra types due to regional variation
  mutate(type1 = case_when(TRUE ~ type1),
         type2 = case_when(pokedex_number == 19 ~ NA_character_,
                           TRUE ~ type2),
         # remove duplicate types
         type2 = case_when(type2 == type1 ~ NA_character_, 
                           TRUE ~ type2),
         # create dummy variables for types
         is_bug = case_when(type1 == "bug" ~ 1, 
                            type2 == "bug" ~ 1,
                            TRUE ~ 0),
         is_dark = case_when(type1 == "dark" ~ 1,
                             type2 == "dark" ~ 1,
                             TRUE ~ 0),
         is_dragon = case_when(type1 == "dragon" ~ 1,
                               type2 == "dragon" ~ 1,
                               TRUE ~ 0),
         is_electric = case_when(type1 == "electric" ~ 1,
                                 type2 == "electric" ~ 1,
                                 TRUE ~ 0),
         is_fairy = case_when(type1 == "fairy" ~ 1,
                              type2 == "fairy" ~ 1,
                              TRUE ~ 0),
         is_fighting = case_when(type1 == "fighting" ~ 1,
                                 type2 == "fighting" ~ 1,
                                 TRUE ~ 0),
         is_fire = case_when(type1 == "fire" ~ 1,
                             type2 == "fire" ~ 1,
                             TRUE ~ 0),
         is_flying = case_when(type1 == "flying" ~ 1,
                               type2 == "flying" ~ 1,
                               TRUE ~ 0),
         is_ghost = case_when(type1 == "ghost" ~ 1,
                              type2 == "ghost" ~ 1,
                              TRUE ~ 0),
         is_grass = case_when(type1 == "grass" ~ 1,
                              type2 == "grass" ~ 1,
                              TRUE ~ 0),
         is_ground = case_when(type1 == "ground" ~ 1,
                               type2 == "ground" ~ 1,
                               TRUE ~ 0),
         is_ice = case_when(type1 == "ice" ~ 1,
                            type2 == "ice" ~ 1,
                            TRUE ~ 0),
         is_normal = case_when(type1 == "normal" ~ 1,
                               type2 == "normal" ~ 1,
                               TRUE ~ 0),
         is_poison = case_when(type1 == "poison" ~ 1,
                               type2 == "poison" ~ 1,
                               TRUE ~ 0),
         is_psychic = case_when(type1 == "psychic" ~ 1,
                                type2 == "psychic" ~ 1,
                                TRUE ~ 0),
         is_rock = case_when(type1 == "rock" ~ 1,
                             type2 == "rock" ~ 1,
                             TRUE ~ 0),
         is_steel = case_when(type1 == "steel" ~ 1,
                              type2 == "steel" ~ 1,
                              TRUE ~ 0),
         is_water = case_when(type1 == "water" ~ 1,
                              type2 == "water" ~ 1,
                              TRUE ~ 0), 
         # count number of types
         types_count = case_when(is.na(type2) ~ 1,
                                 TRUE ~ 2),
         # convert capture_rate to numeric & deal with Minior/774
         capture_rate = as.numeric(capture_rate),
         capture_rate = case_when(pokedex_number == 774 ~ 30,
                                  TRUE ~ capture_rate),
         capture_rate2 = case_when(pokedex_number == 774 ~ 255),
         
         # pokemon missing heights/weights due to regional variation
         height_m = case_when(pokedex_number == 19 ~ 0.3,
                              TRUE ~ height_m),
         weight_kg = case_when(pokedex_number == 19 ~ 3.5,
                               TRUE ~ weight_kg)) %>%
          # make sure data is in order of pokedex number
  arrange(pokedex_number)

#### ABILITIES ####
# separate abilities into separate columns
pokemon_t002 <- pokemon_t001 %>% 
  pull(abilities) %>% 
  as.list() %>%
  str_replace_all("\\[|\\]", "") %>%
  as.list() %>%
  str_replace_all("\\'|\\'", "") %>%
  as.list() %>%
  str_split(", ") %>%
  plyr::ldply(rbind) %>% 
  as_tibble() %>%
  # add row id column (this will be the pokedex number)
  rowid_to_column() %>%
         # convert to character
  mutate(`1` = as.character(`1`),
         `2` = as.character(`2`),
         `3` = as.character(`3`),
         `4` = as.character(`4`),
         `5` = as.character(`5`),
         `6` = as.character(`6`),
         # deal with repeated abilities
         `4` = case_when(rowid == 50 ~ `5`,
                         rowid == 51 ~ `5`,
                         rowid == 52 ~ `6`,
                         rowid == 88 ~ `6`,
                         rowid == 89 ~ `6`,
                         rowid == 103 ~ NA_character_,
                         rowid == 745 ~ `6`,
                         TRUE ~ `4`),
         `5` = case_when(rowid == 19 ~ `6`,
                         rowid == 20 ~ `6`,
                         rowid == 50 ~ NA_character_,
                         rowid == 51 ~ NA_character_,
                         rowid == 52 ~ NA_character_,
                         rowid == 53 ~ `6`,
                         rowid == 74 ~ `6`,
                         rowid == 75 ~ `6`,
                         rowid == 76 ~ `6`,
                         rowid == 105 ~ NA_character_,
                         TRUE ~ `5`),
         `6` = case_when(rowid == 19 ~ NA_character_,
                         rowid == 20 ~ NA_character_,
                         rowid == 50 ~ NA_character_,
                         rowid == 51 ~ NA_character_,
                         rowid == 52 ~ NA_character_,
                         rowid == 53 ~ NA_character_,
                         rowid == 74 ~ NA_character_,
                         rowid == 75 ~ NA_character_,
                         rowid == 76 ~ NA_character_,
                         rowid == 88 ~ NA_character_,
                         rowid == 89 ~ NA_character_,
                         rowid == 105 ~ NA_character_,
                         rowid == 745 ~ NA_character_,
                         TRUE ~ `6`),
         # count abilities
         abilities_count = case_when(is.na(`2`) ~ 1,
                                     is.na(`3`) ~ 2,
                                     is.na(`4`) ~ 3,
                                     is.na(`5`) ~ 4,
                                     is.na(`6`) ~ 5,
                                     TRUE ~ 6)) %>%
  select(-c(`6`))

# add column names
colnames(pokemon_t002) <- c("pokedex_number", "ability1", "ability2", "ability3", 
                             "ability4", "ability5", "abilities_count")
  

#### FINAL DATA ####
# join pokemon and abilities data by pokedex number
pokemon_final <- full_join(pokemon_t002, pokemon_t001, by = "pokedex_number") %>%
  # reorder variables
  select(pokedex_number, name, generation, classification, type1, type2, types_count,
         height_m, weight_kg, 
         base_total, hp, speed, attack, defense, sp_attack, sp_defense,
         is_legendary,
         ability1, ability2, ability3, ability4, ability5, abilities_count,
         everything()) %>%
  # remove abilities column
  select(-c(abilities)) %>%
  #### TO DO ####
  # fix abilities for pokemon with regional variants
  # filter gen 7 pokemon & remove gen 7-only variables because gen 7 data is incomplete
  filter(generation != 7) %>% select(-c(capture_rate2))

saveRDS(pokemon_final, here("output", "pkm_data.rds"))
