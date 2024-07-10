library(tidyr)
library(dplyr)
library(tibble)
library(readxl)
library(xml2)
library(XML)

GWP <- read.csv("data/ghg_GWP.csv") %>%
  rename(ghg = GHG_gases)

target_ghg <- read_excel("data/Emissions.xlsx", sheet = "target") %>%
  rename(ghg_target = value) %>%
  mutate(year = as.numeric(year),
         ghg_target = as.numeric(ghg_target)) %>%
  mutate(ghg_target = round(ghg_target, 2))

#-----------------------------------

co2 <- read_excel("data/Emissions.xlsx", sheet = "CO2") %>%
  gather(year, value, -sce, -region, -ghg, -unit) %>%
  group_by(unit, sce, region, year) %>%
  summarise(value = sum(value)) %>%
  ungroup %>%
  mutate(ghg = "CO2",
         unit = "MTC")

luc <- read_excel("data/Emissions.xlsx", sheet = "LUC_reg") %>%
  gather(year, value, -scenario, -region, -ghg, -unit) %>%
  group_by(unit, scenario, region, year) %>%
  summarise(value = sum(value)) %>%
  ungroup %>%
  mutate(ghg = "CO2LUC",
         unit = "MTC") %>%
  rename(sce = scenario) 

ghg_total <- read_excel("data/Emissions.xlsx", sheet = "nonCO2_reg") %>%
  gather(year, value, -sce, -region, -ghg, -unit) %>%
  bind_rows(luc) %>%
  bind_rows(co2) %>%
  filter(year >= 2015) %>%
  left_join(GWP, by = c("ghg")) %>%
  mutate(value = value * GWP,
         Units = "MtCO2e") %>%
  group_by(sce, region, year, Units) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  filter(year > 2015,
         year <= 2050) %>%
  rename(scenario = sce) %>%
  mutate(scenario = if_else(grepl("Gini25", scenario), "Gini25", scenario),
         scenario = if_else(grepl("Gini50", scenario), "Gini50", scenario),
         scenario = if_else(grepl("MinHist", scenario), "MinHist", scenario),
         scenario = if_else(grepl("RegGHGPol", scenario), "Narayan_etal_2023", scenario)) %>%
  #mutate(value = round(value, 0)) %>%
  pivot_wider(names_from = "scenario",
              values_from = "value") %>%
  mutate(year = as.numeric(year)) %>%
  left_join(target_ghg, by = join_by(region, year)) 



check_policy <- ghg_total %>%
  filter(region != "European Free Trade Association") %>%
  as_tibble() %>%
  mutate(diff_PCA = ghg_target - Narayan_etal_2023,
         diff_Gini25 = ghg_target - Gini25 ,
         diff_Gini50 = ghg_target - Gini50,
         diff_MinHist = ghg_target - MinHist) %>%
  select(region, year, Units, starts_with("diff")) %>%
  pivot_longer(names_to = "scenario",
               values_to = "diff_target_sce",
               cols = c("diff_PCA", "diff_Gini25", "diff_Gini50", "diff_MinHist")) %>%
  mutate(scenario = gsub("diff_", "", scenario)) %>%
  filter(diff_target_sce < 0) %>%
  gcamdata::left_join_error_no_match(target_ghg, by = join_by(region, year)) %>%
  filter(ghg_target > 0) %>%
  mutate(diff_target_sce = round(diff_target_sce, 2),
         diff_target_sce_perc = diff_target_sce /ghg_target,
         diff_target_sce_perc = round(diff_target_sce_perc, 2)) %>%
  filter(abs(diff_target_sce_perc) > 0.02)

  write.csv(check_policy,"check_policy.csv", row.names = F)
  
  
  
  



