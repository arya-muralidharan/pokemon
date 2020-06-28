## PRELIMINARY ANALYSES/VIZ OF POKEMON DATA ##

#### SETUP ####

#### packages ####
library(here)
library(grid)
library(tidyverse)

#### load data ####
# pokemon_final <- read_rds(here("output", "pkm_data.rds"))

#### functions and stuff ####
primeFactorization <- function(n){
  p <- c()
  i = 2
  x = n
  while(prod(p) != n){
    if(!x%%i){
      p <- c(p, i)
      x = x/i
      i = 1
    }
    i = i+1
  }
  list(p)
}


#### SUMMARY TABLES ####

#### overall ####
pokemon_byall <- pokemon_final %>%
  summarise(label = "overall",
            total_pkmn = n(),
            starting_pokedex_num = min(pokedex_number),
            ending_pokedex_num = max(pokedex_number),
            legendary_pkmn = sum(is_legendary, na.rm = T),
            single_types = sum(is.na(type2)),
            dual_types = sum(!is.na(type2)),
            
            bug_types = sum(is_bug, na.rm = T),
            dark_types = sum(is_dark, na.rm = T),
            dragon_types = sum(is_dragon, na.rm = T),
            electric_types = sum(is_electric, na.rm = T),
            fairy_types = sum(is_fairy, na.rm = T),
            fighting_types = sum(is_fighting, na.rm = T),
            fire_types = sum(is_fire, na.rm = T),
            flying_types = sum(is_flying, na.rm = T),
            ghost_types = sum(is_ghost, na.rm = T),
            grass_types = sum(is_grass, na.rm = T),
            ground_types = sum(is_ground, na.rm = T),
            ice_types = sum(is_ice, na.rm = T),
            normal_types = sum(is_normal, na.rm = T),
            poison_types = sum(is_poison, na.rm = T),
            psychic_types = sum(is_psychic, na.rm = T),
            rock_types = sum(is_rock, na.rm = T),
            steel_types = sum(is_steel, na.rm = T),
            water_types = sum(is_water, na.rm = T))

#### generation ####
pokemon_bygen <- pokemon_final %>%
  group_by(generation) %>%
  summarise(total_pkmn = n(),
            starting_pokedex_num = min(pokedex_number),
            ending_pokedex_num = max(pokedex_number),
            
            avg_height_m = mean(height_m),
            avg_weight_kg = mean(weight_kg),
            avg_base_stats = mean(base_total),
            avg_hp = mean(hp),
            avg_speed = mean(speed),
            avg_def = mean(defense),
            avg_att = mean(attack),
            avg_sp_def = mean(sp_defense),
            avg_sp_att = mean(sp_attack),
            
            legendary_pkmn = sum(is_legendary, na.rm = T),
            
            single_types = sum(is.na(type2)),
            dual_types = sum(!is.na(type2)),
            
            bug_types = sum(is_bug, na.rm = T),
            dark_types = sum(is_dark, na.rm = T),
            dragon_types = sum(is_dragon, na.rm = T),
            electric_types = sum(is_electric, na.rm = T),
            fairy_types = sum(is_fairy, na.rm = T),
            fighting_types = sum(is_fighting, na.rm = T),
            fire_types = sum(is_fire, na.rm = T),
            flying_types = sum(is_flying, na.rm = T),
            ghost_types = sum(is_ghost, na.rm = T),
            grass_types = sum(is_grass, na.rm = T),
            ground_types = sum(is_ground, na.rm = T),
            ice_types = sum(is_ice, na.rm = T),
            normal_types = sum(is_normal, na.rm = T),
            poison_types = sum(is_poison, na.rm = T),
            psychic_types = sum(is_psychic, na.rm = T),
            rock_types = sum(is_rock, na.rm = T),
            steel_types = sum(is_steel, na.rm = T),
            water_types = sum(is_water, na.rm = T))


#### TYPES ####

pokemon_dualtypes <- pokemon_final %>%
  group_by(type1, type2) %>%
  summarise(total_pkmn = n()) %>%
  mutate(type2 = case_when(is.na(type2) ~ type1,
                           TRUE ~ type2),
         # assign a unique prime number to each type
         ta = case_when(type1 == "bug" ~ 2,
                        type1 == "dark" ~ 3,
                        type1 == "dragon" ~ 5,
                        type1 == "electric" ~ 7,
                        type1 == "fairy" ~ 11,
                        type1 == "fighting" ~ 13,
                        type1 == "fire" ~ 17,
                        type1 == "flying" ~ 19,
                        type1 == "ghost" ~ 23,
                        type1 == "grass" ~ 29,
                        type1 == "ground" ~ 31,
                        type1 == "ice" ~ 37,
                        type1 == "normal" ~ 41,
                        type1 == "poison" ~ 43,
                        type1 == "psychic" ~ 47,
                        type1 == "rock" ~ 53,
                        type1 == "steel" ~ 59,
                        type1 == "water" ~ 61),
         tb = case_when(type2 == "bug" ~ 2,
                        type2 == "dark" ~ 3,
                        type2 == "dragon" ~ 5,
                        type2 == "electric" ~ 7,
                        type2 == "fairy" ~ 11,
                        type2 == "fighting" ~ 13,
                        type2 == "fire" ~ 17,
                        type2 == "flying" ~ 19,
                        type2 == "ghost" ~ 23,
                        type2 == "grass" ~ 29,
                        type2 == "ground" ~ 31,
                        type2 == "ice" ~ 37,
                        type2 == "normal" ~ 41,
                        type2 == "poison" ~ 43,
                        type2 == "psychic" ~ 47,
                        type2 == "rock" ~ 53,
                        type2 == "steel" ~ 59,
                        type2 == "water" ~ 61),
         # create product column
         product = ta*tb) %>%
  # group type pairs together
  group_by(product) %>%
  summarise(total_pkmn = sum(total_pkmn, na.rm = T),
              pf = suppressWarnings(primeFactorization(product))) %>%
  mutate(b = map(pf, ~ as_tibble(split(., letters[2:3])))) %>%
  unnest(b) %>%
  mutate(t1 = case_when(b == 2 ~ "bug",
                        b == 3 ~ "dark",
                        b == 5 ~ "dragon",
                        b == 7 ~ "electric",
                        b == 11 ~ "fairy",
                        b == 13 ~ "fighting",
                        b == 17 ~ "fire",
                        b == 19 ~ "flying",
                        b == 23 ~ "ghost",
                        b == 29 ~ "grass",
                        b == 31 ~ "ground",
                        b == 37 ~ "ice",
                        b == 41 ~ "normal",
                        b == 43 ~ "poison",
                        b == 47 ~ "psychic",
                        b == 53 ~ "rock",
                        b == 59 ~ "steel",
                        b == 61 ~ "water"),
         t2 = case_when(c == 2 ~ "bug",
                        c == 3 ~ "dark",
                        c == 5 ~ "dragon",
                        c == 7 ~ "electric",
                        c == 11 ~ "fairy",
                        c == 13 ~ "fighting",
                        c == 17 ~ "fire",
                        c == 19 ~ "flying",
                        c == 23 ~ "ghost",
                        c == 29 ~ "grass",
                        c == 31 ~ "ground",
                        c == 37 ~ "ice",
                        c == 41 ~ "normal",
                        c == 43 ~ "poison",
                        c == 47 ~ "psychic",
                        c == 53 ~ "rock",
                        c == 59 ~ "steel",
                        c == 61 ~ "water")) %>%
  select(t1, t2, total_pkmn) %>%
  # add 0s for unused dual types
  rbind(tribble(~t1, ~t2, ~total_pkmn, 
                "bug", "dark", 0,
                "bug", "dragon", 0,
                "bug", "fairy", 0,
                "bug", "ice", 0,
                "bug", "normal", 0,
                "bug", "psychic", 0,
                "dark", "electric", 0,
                "dark", "fairy", 0,
                "dragon", "fairy", 0,
                "dragon", "fighting", 0,
                "dragon", "grass", 0,
                "dragon", "normal", 0,
                "electric", "fighting", 0,
                "electric", "fire", 0,
                "electric", "grass", 0,
                "electric", "ice", 0,
                "electric", "poison", 0,
                "electric", "psychic", 0,
                "electric", "rock", 0,
                "fairy", "fighting", 0,
                "fairy", "fire", 0,
                "fairy", "ghost", 0,
                "fairy", "ground", 0,
                "fairy", "ice", 0,
                "fairy", "poison", 0,
                "fighting", "ghost", 0,
                "fighting", "ground", 0,
                "fighting", "ice", 0,
                "fighting", "normal", 0,
                "fire", "grass", 0,
                "fire", "poison", 0,
                "ghost", "normal", 0,
                "ghost", "rock", 0,
                "ice", "normal", 0,
                "ice", "poison", 0,
                "ice", "steel", 0,
                "normal", "poison", 0,
                "normal", "steel", 0,
                "normal", "rock", 0,
                "poison", "psychic", 0,
                "poison", "rock", 0,
                "poison", "steel", 0)) %>%
  arrange(t1, t2)

ggplot(data = pokemon_dualtypes) +
  geom_raster(aes(x = t1, y = t2, fill = total_pkmn)) +
  geom_text(aes(x = t1, y = t2, label = total_pkmn)) +
  labs(title = "Pokémon Types, Generations I-VI",
       # x = "Type",
       # y = "Type",
       caption = "Single-type pokémon appear on the diagonal. 
                  721 pokémon are included (no mega evolutions).",
       fill = "Number of Pokémon") + 
  scale_fill_gradient(limits = c(0, 60), low = "#ffffff", high = "#cc0000") +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_line(color = "transparent"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size = 13),
        axis.title = element_blank(),
        axis.text = element_text(size = 7),
        axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1),
        legend.position = "none",
        # legend.title = element_text(size = 20, hjust = 0.5),
        # legend.text = element_text(size = 15),
        # legend.key.height = unit(7, "line"),
        # legend.key.width = unit(3, "line"),
        plot.caption = element_text(size = 6)) 
  # annotate("text", x = 9, y = 9, 
  #          label = "DRAFT", col="#bbbbbb", cex = 50,
  #          fontface = "bold", alpha = 0.8)
ggsave(here("output", "types.png"), width = 5, height = 5, bg= "#ffffff")

##### notes ####

# only one pure flying pokemon: tornadus (gen v)
# only four pure steel pokemon: registeel (gen iii); klink, klang, klinklang (gen v)