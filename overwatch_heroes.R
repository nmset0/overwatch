# Packages:
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(knitr)
library(stringr)

# LAST UPDATED 21 OCTOBER 2024 #

# Import dataset
ow_data = read.csv("~/Data Raw/Overwatch2_HeroPatches_raw - Sheet1.csv")

# Exporting dataset
#write.csv(ow_data, file = "C:/Users/natha/OneDrive/Documents/Data Clean/OverwatchHeroData_inProgress.csv")

# Data cleaning 
ow_data = ow_data %>% mutate(id = 1:nrow(ow_data), .before = year) %>% mutate(month_num = match(month, month.name), .after = month) %>% # Adding new column with 
  mutate(ow_data$`ow_data$skins == as.numeric(ow_data$skins)`) %>%                     # month's numbers, i.e. "January" -> 1, etc.
  mutate(change = gsub("sec", "seconds", ow_data$change)) %>%  # undoing abbreviation 
  mutate(hero = tolower(hero)) %>% # changing uppercase to lowercase
  mutate(ability = tolower(ability)) %>%
  mutate(change = tolower(change)) %>%
  mutate(skins = case_when(  # Adding number of skins for each hero
    hero == 'ana' ~ 38,
    hero == 'ashe' ~ 31,
    hero == 'baptiste' ~ 27,
    hero == 'bastion' ~ 34,
    hero == 'brigitte' ~ 32, 
    hero == 'cassidy' ~ 35,
    hero == 'dva' ~ 37, 
    hero == 'doomfist' ~ 31, 
    hero == 'echo' ~ 26,
    hero == 'genji' ~ 44,
    hero == 'hanzo' ~ 36,
    hero == 'illari' ~ 16,
    hero == 'junker queen' ~ 18,
    hero == 'junkrat' ~ 32,
    hero == 'juno' ~ 5,
    hero == 'kiriko' ~ 25,
    hero == 'lifeweaver' ~ 17,
    hero == 'lucio' ~ 38,
    hero == 'mauga' ~ 12,
    hero == 'mei' ~ 35,
    hero == 'mercy' ~ 44,
    hero == 'moira' ~ 30,
    hero == 'orisa' ~ 29,
    hero == 'pharah' ~ 35, 
    hero == 'ramattra' ~ 18,
    hero == 'reaper' ~ 42,
    hero == 'reinhardt' ~ 40,
    hero == 'roadhog' ~ 35,
    hero == 'sigma' ~ 27,  
    hero == 'sojourn' ~ 21,
    hero == 'soldier: 76' ~ 38,
    hero == 'sombra' ~ 36,
    hero == 'symmetra' ~ 34,
    hero == 'torbjorn' ~ 32,
    hero == 'tracer' ~ 40,
    hero == 'venture' ~ 9,
    hero == 'widowmaker' ~ 41,
    hero == 'winston' ~ 29,
    hero == 'wrecking ball' ~ 29,
    hero == 'zarya' ~ 35,
    hero == 'zenyatta' ~ 34,
    TRUE ~ skins
  )) %>% 
  mutate(days_since_release = case_when(
    hero == 'ana' ~ 749,
    hero == 'ashe' ~ 749,
    hero == 'baptiste' ~ 749,
    hero == 'bastion' ~ 749,
    hero == 'brigitte' ~ 749, 
    hero == 'cassidy' ~ 749,
    hero == 'dva' ~ 749, 
    hero == 'doomfist' ~ 749, 
    hero == 'echo' ~ 749,
    hero == 'genji' ~ 749,
    hero == 'hanzo' ~ 749,
    hero == 'illari' ~ 439,
    hero == 'junker queen' ~ 749,
    hero == 'junkrat' ~ 749,
    hero == 'juno' ~ 55,
    hero == 'kiriko' ~ 749,
    hero == 'lifeweaver' ~ 558,
    hero == 'lucio' ~ 749,
    hero == 'mauga' ~ 321,
    hero == 'mei' ~ 749,
    hero == 'mercy' ~ 749,
    hero == 'moira' ~ 749,
    hero == 'orisa' ~ 749,
    hero == 'pharah' ~ 749, 
    hero == 'ramattra' ~ 686,
    hero == 'reaper' ~ 749,
    hero == 'reinhardt' ~ 749,
    hero == 'roadhog' ~ 749,
    hero == 'sigma' ~ 749,
    hero == 'sojourn' ~ 749,
    hero == 'soldier: 76' ~ 749,
    hero == 'sombra' ~ 749,
    hero == 'symmetra' ~ 749,
    hero == 'torbjorn' ~ 749,
    hero == 'tracer' ~ 749,
    hero == 'venture' ~ 188,
    hero == 'widowmaker' ~ 749,
    hero == 'winston' ~ 749,
    hero == 'wrecking ball' ~ 749,
    hero == 'zarya' ~ 749,
    hero == 'zenyatta' ~ 749,
    TRUE ~ days_since_release 
  )) 

# ow_data$days_since_release = ow_data$days_since_release + [Number of days since 21 October 2024]


skin = subset(ow_data, select = c("hero", "skins"))
  skin_count = unique(skin)
    skin_sum = sum(skin_count$skins)
      ow_data = mutate(ow_data, skin_ratio = skins/skin_sum)


row = nrow(ow_data) # total number of hero changes
# Get number of changes for each hero
num_change = ow_data %>% count(str_to_title(hero))
# Calculate ratio of changes and add to data
ow_data = ow_data %>%
  mutate(patch_ratio = case_when(
    hero == 'ana' ~ num_change[1,2]/row,
    hero == 'ashe' ~ num_change[2,2]/row,
    hero == 'baptiste' ~ num_change[3,2]/row,
    hero == 'bastion' ~ num_change[4,2]/row,
    hero == 'brigitte' ~ num_change[5,2]/row, 
    hero == 'cassidy' ~ num_change[6,2]/row,
    hero == 'dva' ~ num_change[8,2]/row, 
    hero == 'doomfist' ~ num_change[7,2]/row, 
    hero == 'echo' ~ num_change[9,2]/row,
    hero == 'genji' ~ num_change[10,2]/row,
    hero == 'hanzo' ~ num_change[11,2]/row,
    hero == 'illari' ~ num_change[12,2]/row,
    hero == 'junker queen' ~ num_change[13,2]/row,
    hero == 'junkrat' ~ num_change[14,2]/row,
    hero == 'juno' ~ num_change[15,2]/row,
    hero == 'kiriko' ~ num_change[16,2]/row,
    hero == 'lifeweaver' ~ num_change[17,2]/row,
    hero == 'lucio' ~ num_change[18,2]/row,
    hero == 'mauga' ~ num_change[19,2]/row,
    hero == 'mei' ~ num_change[20,2]/row,
    hero == 'mercy' ~ num_change[21,2]/row,
    hero == 'moira' ~ num_change[22,2]/row,
    hero == 'orisa' ~ num_change[23,2]/row,
    hero == 'pharah' ~ num_change[24,2]/row, 
    hero == 'ramattra' ~ num_change[25,2]/row,
    hero == 'reaper' ~ num_change[26,2]/row,
    hero == 'reinhardt' ~ num_change[27,2]/row,
    hero == 'roadhog' ~ num_change[28,2]/row,
    hero == 'sigma' ~ num_change[29,2]/row,
    hero == 'sojourn' ~ num_change[30,2]/row,
    hero == 'soldier: 76' ~ num_change[31,2]/row,
    hero == 'sombra' ~ num_change[32,2]/row,
    hero == 'symmetra' ~ num_change[33,2]/row,
    hero == 'torbjorn' ~ num_change[34,2]/row,
    hero == 'tracer' ~ num_change[35,2]/row,
    hero == 'venture' ~ num_change[36,2]/row,
    hero == 'widowmaker' ~ num_change[37,2]/row,
    hero == 'winston' ~ num_change[38,2]/row,
    hero == 'wrecking ball' ~ num_change[39,2]/row,
    hero == 'zarya' ~ num_change[40,2]/row,
    hero == 'zenyatta' ~ num_change[41,2]/row 
  ))

# Get ratios of how many buffs and nerfs each hero has
ow_data = ow_data %>%
  group_by(hero) %>%
  mutate(
    nerf_ratio = sum(is_nerf) / n(),
    buff_ratio = sum(is_buff) / n()
  ) %>%
  ungroup()

hero_buff_nerf_ratios = ow_data %>%
  group_by(hero) %>%
  summarise(
    nerf_count = sum(is_nerf),
    buff_count = sum(is_buff),
    nerf_ratio = nerf_count / n(),
    buff_ratio = buff_count / n(),
    .groups = 'drop'
  )
write.csv(hero_buff_nerf_ratios, file = "C:/Users/natha/OneDrive/Documents/Data Clean/hero_buff_nerf_ratios.csv")


role_buff_nerf_ratios = ow_data %>%
  group_by(role) %>%
  summarise(
    nerf_count = sum(is_nerf),
    buff_count = sum(is_buff),
    nerf_ratio = nerf_count / n(),
    buff_ratio = buff_count / n(),
    .groups = 'drop'
  )
write.csv(role_buff_nerf_ratios, file = "C:/Users/natha/OneDrive/Documents/Data Clean/role_buff_nerf_ratios.csv")


ow_data = ow_data %>%
  group_by(hero) %>%
  mutate(number_of_changes = n(), .before = patch_ratio) %>%
  ungroup()



# Ratios of each hero's skins to total number of skins in Overwatch 2
skin_ratios = unique(ow_data %>% group_by(hero) %>% select(hero, skins, skin_ratio)) %>% arrange(hero)
write.csv(skin_ratios, file = "C:/Users/natha/OneDrive/Documents/Data Clean/skin_ratios.csv")



# Some kable tables
hero_changes = ow_data %>% count(str_to_title(hero)) %>% kable(col.names = c("Hero", "Number of Changes"))
hero_changes

# Number of buffs and nerfs each role has and the ratios to total number of buffs and nerfs
buffs_per_role = ow_data %>% subset(ow_data$is_buff == 1) %>% count(str_to_title(role)) %>% kable(col.names = c("Role", "Times Buffed"))
  nerfs_per_role = ow_data %>% subset(ow_data$is_nerf == 1) %>% count(str_to_title(role)) %>% kable(col.names = c("Role", "Times Nerfed")) 
    buffs_per_role
      nerfs_per_role
        role_buff_nerf_table = kable(role_buff_nerf_ratios)

# Number of buffs and nerfs each hero has and the ratios to total number of buffs and nerfs
buffs_per_hero = ow_data %>% subset(ow_data$is_buff == 1) %>% count(str_to_title(hero)) %>% kable(col.names = c("Hero", "Times Buffed"))
  nerfs_per_hero = ow_data %>% subset(ow_data$is_nerf == 1) %>% count(str_to_title(hero)) %>% kable(col.names = c("Hero", "Times Nerfed"))
    buffs_per_hero
      nerfs_per_hero
        hero_buff_nerf_table = kable(hero_buff_nerf_ratios)

# Number of skins each hero has and the ratio to total skins
skin_count = arrange(skin_count, desc(skins))
  skin_count_table = skin_count %>% kable(col.names = c("Hero", "Number of Skins")) # Table of only number of skins per hero
    skin_ratio_table = kable(skin_ratios)


