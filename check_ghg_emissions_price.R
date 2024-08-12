library(tidyr)
library(dplyr)
library(tibble)
library(readxl)
library(xml2)
library(XML)

GWP <- read.csv("data/ghg_GWP.csv") %>%
  rename(ghg = GHG_gases)

target_ghg <- read_excel("data/Emissions_price.xlsx", sheet = "target") %>%
  rename(ghg_target = value) %>%
  mutate(year = as.numeric(year),
         ghg_target = as.numeric(ghg_target)) %>%
  mutate(ghg_target = round(ghg_target, 2))

#-----------------------------------

co2 <- read_excel("data/Emissions_price.xlsx", sheet = "CO2") %>%
  gather(year, value, -sce, -region, -ghg, -unit) %>%
  group_by(unit, sce, region, year) %>%
  summarise(value = sum(value)) %>%
  ungroup %>%
  mutate(ghg = "CO2",
         unit = "MTC")

luc <- read_excel("data/Emissions_price.xlsx", sheet = "LUC_reg") %>%
  gather(year, value, -scenario, -region, -ghg, -unit) %>%
  group_by(unit, scenario, region, year) %>%
  summarise(value = sum(value)) %>%
  ungroup %>%
  mutate(ghg = "CO2LUC",
         unit = "MTC") %>%
  rename(sce = scenario) 

ghg_total <- read_excel("data/Emissions_price.xlsx", sheet = "nonCO2_reg") %>%
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
         scenario = if_else(grepl("RegGHGPol", scenario), "Narayan_etal_2023", scenario)) %>%
  #mutate(value = round(value, 0)) %>%
  pivot_wider(names_from = "scenario",
              values_from = "value") %>%
  mutate(year = as.numeric(year)) %>%
  left_join(target_ghg, by = join_by(region, year)) 



check_policy <- ghg_total %>%
  filter(region != "European Free Trade Association") %>%
  as_tibble() %>%
  select(-Narayan_etal_2023) %>%
  pivot_longer(names_to = "scenario",
               values_to = "GHG",
               cols = c("Gini25", "Gini50", "ghg_target")) %>%
  filter(year == 2050) %>%
  mutate(GHG = GHG / 1000,
         Units = "GtCO2e",
         scenario = gsub("ghg_target", "Target", scenario),
         region = gsub("_", " ", region))
  


ggplot(check_policy, aes(x = GHG, y = region, colour = scenario, shape = scenario, size = scenario)) + 
  geom_point() +
  labs(x = "", y = "GtCO2e") + 
  theme_bw()+
  theme(panel.grid.major = element_line(colour = "gray80"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  scale_shape_manual(values=c(19, 19, 73)) +
  scale_size_manual(values=c(2, 2, 4)) +
  scale_colour_manual(values = c("dodgerblue1", "forestgreen", "firebrick2")) +
  scale_y_discrete(limits=rev)

ggsave(paste0(here::here(), "/figures/Price_target.tiff"),last_plot(), "tiff")  
  
  
  



