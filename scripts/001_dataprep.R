## PREP POKEMON DATA FOR ANALYSIS AND OTHER THINGS ##

#### PACKAGES ####
library(here)
library(tidyverse)


#### READ IN DATA ####
# from https://www.kaggle.com/rounakbanik/pokemon
pokemon_t000 <- read_csv(here("data", "pokemon.csv"))

#### INITIAL PREP ####
pokemon_final <- pokemon_t000 %>%
         # misspelling
  rename(classification = classfication) %>%
         # pokemon with incorrect/extra types due to regional variation
  mutate(type1 = case_when(pokedex_number == 27 ~ "ground",
                           pokedex_number == 28 ~ "ground",
                           TRUE ~ type1),
         type2 = case_when(pokedex_number == 19 ~ NA_character_,
                           pokedex_number == 20 ~ NA_character_,
                           pokedex_number == 27 ~ NA_character_,
                           pokedex_number == 28 ~ NA_character_,
                           pokedex_number == 37 ~ NA_character_,
                           pokedex_number == 38 ~ NA_character_,
                           pokedex_number == 52 ~ NA_character_,
                           pokedex_number == 53 ~ NA_character_,
                           pokedex_number == 105 ~ NA_character_,
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
                               TRUE ~ weight_kg),
         # deal with abilities column
         abilities = str_replace_all(abilities, c("\\[|\\]" = "", "\\'|\\'" = ""))) %>%
  # separate abilities column into multiple columns
  separate(col = abilities, 
           sep = ", ",
           into = c('ability1', 'ability2', 'ability3', 'ability4', 
                    'ability5', 'ability6')) %>%
         # fix repeated abilities
  mutate(ability4 = case_when(pokedex_number == 50 ~ ability5,
                              pokedex_number == 51 ~ ability5,
                              pokedex_number == 52 ~ ability6,
                              pokedex_number == 88 ~ ability6,
                              pokedex_number == 89 ~ ability6,
                              pokedex_number == 103 ~ NA_character_,
                              pokedex_number == 745 ~ ability6,
                              TRUE ~ ability4),
         ability5 = case_when(pokedex_number == 19 ~ ability6,
                              pokedex_number == 20 ~ ability6,
                              pokedex_number == 50 ~ NA_character_,
                              pokedex_number == 51 ~ NA_character_,
                              pokedex_number == 52 ~ NA_character_,
                              pokedex_number == 53 ~ ability6,
                              pokedex_number == 74 ~ ability6,
                              pokedex_number == 75 ~ ability6,
                              pokedex_number == 76 ~ ability6,
                              pokedex_number == 105 ~ NA_character_,
                              TRUE ~ ability5),
         ability6 = case_when(pokedex_number == 19 ~ NA_character_,
                              pokedex_number == 20 ~ NA_character_,
                              pokedex_number == 50 ~ NA_character_,
                              pokedex_number == 51 ~ NA_character_,
                              pokedex_number == 52 ~ NA_character_,
                              pokedex_number == 53 ~ NA_character_,
                              pokedex_number == 74 ~ NA_character_,
                              pokedex_number == 75 ~ NA_character_,
                              pokedex_number == 76 ~ NA_character_,
                              pokedex_number == 88 ~ NA_character_,
                              pokedex_number == 89 ~ NA_character_,
                              pokedex_number == 105 ~ NA_character_,
                              pokedex_number == 745 ~ NA_character_,
                              TRUE ~ ability6),
         # count abilities
         abilities_count = case_when(is.na(ability2) ~ 1,
                                     is.na(ability3) ~ 2,
                                     is.na(ability4) ~ 3,
                                     is.na(ability5) ~ 4,
                                     is.na(ability6) ~ 5,
                                     TRUE ~ 6)) %>%
  # reorder variables
  select(pokedex_number, name, generation, classification, type1, type2, types_count,
         height_m, weight_kg, 
         base_total, hp, speed, attack, defense, sp_attack, sp_defense,
         is_legendary,
         ability1, ability2, ability3, ability4, ability5, abilities_count,
         everything()) %>%
  # remove ability6 column
  select(-c(ability6)) %>%
  #### TO DO ####
  # fix abilities for pokemon with regional variants
  # filter gen 7 pokemon & remove gen 7-only variables because gen 7 data is incomplete
  filter(generation != 7) %>% select(-c(capture_rate2))

saveRDS(pokemon_final, here("output", "pkm_data.rds"))
