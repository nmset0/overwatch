# Packages:
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(knitr)
library(stringr)

# LAST UPDATED 30 OCTOBER 2024 #

# Import dataset
ow_data = read_csv("Datasets/Overwatch2_HeroPatches_raw - Sheet1.csv")

# Exporting dataset
# write.csv(ow_data, file = "Datasets/OverwatchHeroData_clean.csv")

# Data cleaning 
n_days = 758
increment = 0
# increment = [Days since last updated]
# n_days = ow_data$days_since_release + increment


ow_data = ow_data %>% mutate(id = 1:nrow(ow_data), .before = year) %>% 
  mutate(month_num = match(month, month.name), .after = month) %>% # Adding new column with 
  mutate(ow_data$`ow_data$skins = as.numeric(ow_data$skins)`) %>%  # undoing abbreviation 
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
    hero == 'illari' ~ 455,
    hero == 'juno' ~ 71 + increment,
    hero == 'lifeweaver' ~ 574 + increment,
    hero == 'mauga' ~ 321 + increment,
    hero == 'ramattra' ~ 702 + increment,
    hero == 'venture' ~ 204 + increment,
    TRUE ~ n_days
  )) # Counting days since hero was released; heroes in the game at release are the same age as the game


# adding the number of skins for each hero to the dataset
skin = subset(ow_data, select = c("hero", "skins"))
  skin_count = unique(skin)
    skin_sum = sum(skin_count$skins)
      ow_data = mutate(ow_data, skin_ratio = skins/skin_sum, .after = skins)


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
    indiv_nerf_ratio = sum(is_nerf) / n(),
    indiv_buff_ratio = sum(is_buff) / n()
  ) %>%
  ungroup()


# nerf_ratio for each hero = proportions of changes to this hero that have been nerfs
# buff_ratio for each hero = proportions of changes to this hero that have been buffs 
hero_buff_nerf_ratios = ow_data %>%
  group_by(hero) %>%
  summarise(
    nerf_count = sum(is_nerf),
    buff_count = sum(is_buff),
    nerf_ratio = nerf_count / n(),
    buff_ratio = buff_count / n(),
    .groups = 'drop'
  )


# Total nerf/buff ratios
total_buffs = sum(ow_data$is_buff, na.rm = TRUE)
total_nerfs = sum(ow_data$is_nerf, na.rm = TRUE)

aggregate_hero_buffnerf_ratios = ow_data %>%
  group_by(hero) %>%
  summarise(
    buffs = sum(is_buff, na.rm = TRUE),
    nerfs = sum(is_nerf, na.rm = TRUE),
    total_buff_ratio = buffs / total_buffs,
    total_nerf_ratio = nerfs / total_nerfs
  )

aggregate_hero_buffnerf_ratios %>% arrange(desc(buffs)) %>% kable()
aggregate_hero_buffnerf_ratios %>% arrange(desc(nerfs)) %>% kable()

write.csv(aggregate_hero_buffnerf_ratios, file = "Datasets/hero_buff_nerf_ratios.csv")


# Adding aggregate ratios to dataset
ow_data = ow_data %>% left_join(aggregate_hero_buffnerf_ratios %>% select(hero, total_buff_ratio, total_nerf_ratio), by = "hero")



# Calculate total buffs and nerfs across all roles
total_counts = ow_data %>% summarise( total_nerfs = sum(is_nerf), total_buffs = sum(is_buff))

# Calculate role-based buff and nerf counts and ratios relative to overall counts
aggregate_role_buffnerf_ratios = ow_data %>% group_by(role) %>%
  summarise(
    nerf_count = sum(is_nerf),
    buff_count = sum(is_buff),
    nerf_ratio = nerf_count / total_counts$total_nerfs,
    buff_ratio = buff_count / total_counts$total_buffs,
    .groups = 'drop'
  )

aggregate_role_buffnerf_ratios = aggregate_role_buffnerf_ratios %>% 
  bind_rows(summarise(aggregate_role_buffnerf_ratios, role = "Total", 
                                                    nerf_count = sum(nerf_count),
                                                    buff_count = sum(buff_count), 
                                                    nerf_ratio = sum(nerf_ratio),
                                                    buff_ratio = sum(buff_ratio)))

write.csv(aggregate_role_buffnerf_ratios, file = "Datasets/role_buff_nerf_ratios.csv")


# Adding the number of changes to each hero to the dataset
ow_data = ow_data %>%
  group_by(hero) %>%
  mutate(number_of_changes = n(), .before = patch_ratio) %>% ungroup()


# Ratios of each hero's skins to total number of skins in Overwatch 2
skin_ratios = unique(ow_data %>% group_by(hero) %>% select(hero, skins, skin_ratio)) %>% arrange(hero)
write.csv(skin_ratios, file = "Datasets/skin_ratios.csv")




# number of changes to each hero
hero_changes = ow_data %>% count(str_to_title(hero)) %>% arrange(n)
hero_changes %>% kable(col.names = c("Hero", "Number of Changes")) 

# Number of buffs and nerfs each role has and the ratios to total number of buffs and nerfs
buffs_per_role = ow_data %>% subset(ow_data$is_buff == 1) %>% count(str_to_title(role)) 
  nerfs_per_role = ow_data %>% subset(ow_data$is_nerf == 1) %>% count(str_to_title(role))  
    buffs_per_role %>% kable(col.names = c("Role", "Times Buffed"))
      nerfs_per_role %>% kable(col.names = c("Role", "Times Nerfed"))
        role_buff_nerf_table = aggregate_role_buffnerf_ratios %>% kable(col.names = c("Role", "Nerfs", "Buffs", "Overall Nerf Ratio", "Overall Buff Ratio"))
          role_buff_nerf_table

# Number of buffs and nerfs each hero has and the ratios to total number of buffs and nerfs
buffs_per_hero = ow_data %>% subset(ow_data$is_buff == 1) %>% count(str_to_title(hero)) %>% arrange(desc(n))
  nerfs_per_hero = ow_data %>% subset(ow_data$is_nerf == 1) %>% count(str_to_title(hero)) %>% arrange(desc(n))
    buffs_per_hero %>% kable(col.names = c("Hero", "Buff Count"))
      nerfs_per_hero %>% kable(col.names = c("Hero", "Nerf Count"))
        aggregate_hero_buffnerf_ratios %>% kable(col.names = c("Hero", "Buffs", "Nerfs", "Buff Ratio All Heroes", "Nerf Ratio All Heroes"))

# Number of skins each hero has and the ratio to total skins
skin_count = arrange(skin_count, desc(skins))
  skin_count_table = skin_count %>% kable(col.names = c("Hero", "Number of Skins")) # Table of only number of skins per hero
    skin_ratio_table = kable(skin_ratios)


