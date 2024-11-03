# Packages:
library(ggplot2)
library(readr)
library(dplyr)

OverwatchHeroData_clean = read_csv("Datasets/OverwatchHeroData_clean.csv")
role_buff_nerf_ratios = read_csv("Datasets/role_buff_nerf_ratios.csv")
hero_buff_nerf_ratios = read_csv("Datasets/hero_buff_nerf_ratios.csv")
skin_ratios = read_csv("Datasets/skin_ratios.csv")
mythics = read_csv("Datasets/ow_mythicskins.csv")
atviANDmsft = read_csv("Datasets/msft_and_atvi_stocks.csv")