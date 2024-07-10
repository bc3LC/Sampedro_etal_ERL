# Load libraries
# --------

library(rgcam)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(tibble)
library(gcamdata)
library(rmap)
library(here)
library(countrycode)

# SECTION 0:  SCENARIO FEATURES ----

#---
# 0.1 - Compare Ginis for a selected suite of countries ----
# TODO Add Gini MinHIst (minimum Gini in the time series)

ginis <- read.csv(paste0(here::here(), "/data/Compare Ginis.csv")) %>%
  mutate(iso = toupper(iso)) %>%
  mutate(continent = countrycode::countrycode(sourcevar = iso, origin = "iso3c", destination = "continent")) %>%
  mutate(country.name = countrycode::countrycode(sourcevar = iso, origin = "iso3c", destination = "country.name")) %>%
  select(!contains("scal"), Gini_2015) %>%
  rename(SSP2_Ineq = Narayan_etal_2023) %>%
  pivot_longer(cols = c("SSP2_Ineq", "Gini25", "Gini50"),
               names_to = "scenario",
               values_to = "gini")


# Select a representative subset of countries
selected_countries <- c("ETH", "MAR", "ZAF", "BRA", "USA", "ARG", "CHN", "IND", "JPN", "SAU",
                        "DEU", "ESP", "SWE", "AUS", "RUS", "IDN")


ggplot(ginis %>% filter(iso %in% selected_countries), aes(x = as.numeric(year), y = gini, color = factor(scenario, levels = c("SSP2_Ineq", "Gini25", "Gini50")))) +
  geom_line() + 
  facet_wrap(~country.name) + 
  theme_bw() + 
  labs(x = "", y = "Gini") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        legend.text = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12)) + 
  scale_color_manual(values = c("orange", "dodgerblue1", "forestgreen"))

ggsave(paste0(here::here(), "/figures/ginis_sce_selectedISO_year.tiff"),last_plot(), device = "tiff")

#---
# 0.2 - Compare income shares for a subset of representative regions ----
gcam_regions <- read.csv(paste0(here::here(), "/data/GCAM_region_names.csv"))

selected_gcam_regions <- c("Africa_Eastern", "Brazil", "Australia_NZ", "Canada", "China", "EU-15",
                          "India", "Japan", "Middle East", "Russia", "South Africa", "USA")


shares <- tibble::as_tibble(bind_rows(
  read.csv(paste0(here::here(), "/data/Rao_multimodel_income_deciles.csv")) %>%
    filter(model == "PCA algorithm (Two Components)") %>%
    mutate(model = "Baseline"),
  read.csv(paste0(here::here(), "/data/GiniRed_25pct.csv")),
           read.csv(paste0(here::here(), "/data/GiniRed_50pct.csv"))
)) %>%
  filter(sce == "SSP2") %>%
  left_join(gcam_regions, by = join_by(GCAM_region_ID)) %>%
  filter(complete.cases(.)) %>%
  filter(region %in% selected_gcam_regions)

ggplot(shares %>%   filter(region %in% selected_gcam_regions, year == 2050), 
       aes(x = factor(category, levels = c("d1", "d2", "d3", "d4", "d5",
                                           "d6", "d7", "d8", "d9", "d10")),
           y = shares, color = factor(model, levels = c("Baseline", "Gini25", "Gini50")))) +
  geom_point() + 
  facet_wrap(~region) + 
  theme_bw() + 
  labs(x = "", y = "Income share") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        legend.text = element_text(size = 11),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12)) + 
  scale_color_manual(values = c("orange","dodgerblue1", "forestgreen"))

ggsave(paste0(here::here(), "/figures/IncShares_sce_selectedGCAMReg_2050.tiff"),last_plot(), "tiff")

#---
# 0.3: Check how the Gini reduction erradicate poverty and extreme poverty  ----
pov <- read.csv(paste0(here::here(), "/data/poverty.csv")) %>%
  filter(!grepl("MinHist", sce)) %>%
  rename(scenario = sce) %>%
  mutate(scenario = if_else(grepl("Gini25", scenario), "Gini25", scenario),
         scenario = if_else(grepl("Gini50", scenario), "Gini50", scenario),
         scenario = if_else(grepl("Ref_SSP2", scenario), "Baseline", scenario)) %>%
  mutate(decile = gsub("resid_", "", decile)) %>%
  gather(year, pcGDP, -scenario, -region, -decile) %>%
  mutate(year = gsub("X", "", year)) %>%
  dplyr::rename(subRegion = region,
              value = pcGDP) %>%
  filter(year %in% c(2050),
         decile %in% c("d1","d5", "d10")) %>%
  mutate(scenario = factor(scenario, levels = c("Baseline", "Gini25", "Gini50")),
         decile_fin = factor(decile, levels = c("d1","d5", "d10")),
         sce = scenario) %>%
  select(-decile, -scenario) %>%
  rename(decile = decile_fin)

pov_palette <- c("firebrick4", "firebrick1", "yellow" , "forestgreen")

map_povx <- rmap::map(data = pov,
                          shape = mapGCAMReg32,
                          folder ="figures/maps/pov",
                          legendFixedBreaks=c(1, 2.17, 10, 30, 50, 70, 100, 500, 1000),
                          palette = pov_palette,
                          row = "sce",
                          col = "decile",
                          #legendType = "continuous",
                          background  = T)
  
map_pov_2050 <- map_povx$map_param_FIXED +
  theme(strip.text.y = element_text(size = 9),
        strip.text.x = element_text(size = 11),
        plot.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8))

ggsave(paste0(here::here(), "/figures/Poverty_GCAMReg_2050.tiff"),last_plot(), "tiff")

# =====================================================
# RESULTS PROCESSING
 
## First, load the rgcam project
prj <- loadProject("Paper_inequality_new.dat")
listScenarios(prj)
QUERY_LIST <- listQueries(prj)

# Select some pollutants (for emissions) and scenarios
sel_gas<-c("BC","OC","HFCs","NOx","SO2","CO2")
sel_sce <- c("RegGHGPol_Gini25_Ineq", "RegGHGPol_Gini50_Ineq", "RegGHGPol_Ineq")

#Create some palletes
my_pal<-c("gray20","gray50","#ad440c","#ef8e27","#d01c2a","darkorchid3","#507fab","deepskyblue1","#11d081", "#00931d")
my_pal_energy<-c("#00931d","gray20","thistle2","gold2","deepskyblue1","peachpuff2","#d01c2a","#11d081")
my_pal_scen<-c("darkorchid3","forestgreen")
my_pal_ssp<-c("forestgreen","dodgerblue3","darkgoldenrod3","firebrick3","black")


#---

# 1 - ENERGY SYSTEM ----

# 1.1 Final energy demand (FEN) ----

# First, calculate the region-level changes in energy demand by sector 
fen.bld <- getQuery(prj,"building final energy by service and fuel") %>%
  mutate(sector = if_else(grepl("comm", sector), "Commercial Buildings", sector),
         sector = if_else(grepl("resid heating", sector), "Residential heating", sector),
         sector = if_else(grepl("resid cooling", sector), "Residential cooling", sector),
         sector = if_else(grepl("resid other", sector), "Residential non-thermal services", sector)) %>%
  group_by(scenario, sector, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() 

fen.ind <- getQuery(prj,"industry total final energy by service") %>%
  group_by(scenario, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(sector = "Industry")

fen.trn <- getQuery(prj,"transport final energy by mode and fuel" ) %>%
  mutate(sector = if_else(grepl("trn_aviation", sector), "International Aviation (pass)", sector),
         sector = if_else(grepl("trn_pass_", sector), "Passenger transport", sector),
         sector = if_else(grepl("trn_freight", sector), "Freight transport", sector),
         sector = if_else(grepl("trn_shipping_intl", sector), "International shipping (freight)", sector)) %>%
  group_by(scenario, sector, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() 

fen <- bind_rows(fen.bld, fen.ind, fen.trn) %>%
  mutate(scenario = if_else(grepl("Gini25", scenario), "Gini25", scenario),
         scenario = if_else(grepl("Gini50", scenario), "Gini50", scenario),
         scenario = if_else(grepl("RegGHGPol_Ineq", scenario), "Baseline", scenario)) %>%
  pivot_wider(names_from = "scenario",
              values_from = "value") %>%
  mutate(diff_Gini25 = Gini25 - Baseline,
         diff_Gini50 = Gini50 - Baseline) %>%
  select(year, sector, Units, starts_with("diff")) %>%
  pivot_longer(cols = c("diff_Gini25", "diff_Gini50"),
               names_to = "scenario",
               values_to = "value") %>%
  mutate(scenario = gsub("diff_", "", scenario)) %>%
  filter(year <= 2050,
         year > 2015)

ggplot(fen, aes(x = year, y = value, fill = factor(sector, levels = c("Commercial Buildings", 
                                                                      "Residential cooling", "Residential heating", "Residential non-thermal services",
                                                                      "Industry",
                                                                      "Passenger transport", "International Aviation (pass)",
                                                                      "Freight transport", "International shipping (freight)"))))+
  geom_col() +
  facet_grid( ~ scenario) + 
  theme_bw() +
  labs(x = "", y = "EJ") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 12)) + 
  guides(fill = guide_legend(ncol = 3)) + 
  scale_fill_manual(values = c("yellow2", 
                               "salmon", "tomato3", "tomato",
                                "violet",
                                 "yellowgreen","chartreuse2",
                                "deepskyblue1", "deepskyblue3"))

ggsave(paste0(here::here(), "/figures/Fen_global_sce_sct.tiff"),last_plot(), "tiff")

# ---
# Then add deciles to assess "within-region effects"
fen.bld.dec <- getQuery(prj,"building final energy by service and fuel") %>%
  filter(grepl("resid", sector)) %>%
  mutate(sector = sub("_([^_]*)$", "_split_\\1", sector)) %>%
  tidyr::separate(sector, into = c("sector", "decile"), sep = "_split_", extra = "merge", fill = "right") %>%
  mutate(sector = if_else(grepl("resid heating", sector), "Residential heating", sector),
         sector = if_else(grepl("resid cooling", sector), "Residential cooling", sector),
         sector = if_else(grepl("resid other", sector), "Residential non-thermal services", sector)) %>%
  group_by(scenario, region, decile, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(sector = "Residential buidlings") 

fen.trn.dec <- getQuery(prj,"transport final energy by mode and fuel" ) %>%
  filter(grepl("trn_pass", sector) | grepl("trn_aviation", sector)) %>%
  mutate(sector = sub("_([^_]*)$", "_split_\\1", sector)) %>%
  tidyr::separate(sector, into = c("sector", "decile"), sep = "_split_", extra = "merge", fill = "right") %>%
  mutate(sector = if_else(grepl("trn_aviation", sector), "International Aviation (pass)", sector),
         sector = if_else(grepl("trn_pass_", sector), "Passenger transport", sector)) %>%
  group_by(scenario, region, decile, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(sector = "Passenger transport") 


fen.dec <- bind_rows(fen.bld.dec, fen.trn.dec) %>%
  mutate(scenario = if_else(grepl("Gini25", scenario), "Gini25", scenario),
         scenario = if_else(grepl("Gini50", scenario), "Gini50", scenario),
         scenario = if_else(grepl("RegGHGPol_Ineq", scenario), "Baseline", scenario)) %>%
  pivot_wider(names_from = "scenario",
              values_from = "value") %>%
  mutate(diff_Gini25 = Gini25 - Baseline,
         diff_Gini50 = Gini50 - Baseline) %>%
  select(year, region, decile, sector, Units, starts_with("diff")) %>%
  pivot_longer(cols = c("diff_Gini25", "diff_Gini50"),
               names_to = "scenario",
               values_to = "value") %>%
  left_join_error_no_match(read.csv("data/map_GCAM_reg.csv"), by = join_by(region)) %>%
  group_by(scenario, region_agg, year, decile, sector) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  rename(region = region_agg) %>%
  mutate(scenario = gsub("diff_", "", scenario),
         region = gsub("_", " ", region),
         region = if_else(grepl("Caribbean", region), "CAC", region),
         region = if_else(grepl("Trade", region), "EFTA", region)) %>%
  filter(year <= 2050,
         year > 2015) 


ggplot(fen.dec %>% filter(year == 2050), aes(x = region, y = value, fill = factor(decile, levels = c("d1", "d2", "d3", "d4", "d5",
                                                                          "d6", "d7", "d8", "d9", "d10")) )) +
  geom_col() +
  facet_grid(factor(sector, levels = c("Residential buidlings",
                                       "Passenger transport")) ~ scenario) + 
  theme_bw() + 
  labs(x = "", y = "EJ") + 
  theme(legend.position = "right",
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        axis.text.x = element_text(size = 7, angle = 90, hjust = 1, vjust = .5),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 12)) + 
  scale_fill_manual(values = my_pal)

ggsave(paste0(here::here(), "/figures/Fen_dec_sce_sct_2050.tiff"),last_plot(), "tiff")

# 1.2 Renewable energy and CDR  ----

en <- getQuery(prj, "primary energy consumption by region (avg fossil efficiency)") %>%
  filter(fuel != "regional biomass") %>%
  mutate(scenario = if_else(grepl("Gini25", scenario), "Gini25", scenario),
         scenario = if_else(grepl("Gini50", scenario), "Gini50", scenario),
         scenario = if_else(grepl("RegGHGPol_Ineq", scenario), "Baseline", scenario)) %>%
  pivot_wider(names_from = "scenario",
              values_from = "value") %>%
  mutate(diff_Gini25 = Gini25 - Baseline,
         diff_Gini50 = Gini50 - Baseline) %>%
  select(year, region, fuel, Units, starts_with("diff")) %>%
  pivot_longer(cols = c("diff_Gini25", "diff_Gini50"),
               names_to = "scenario",
               values_to = "value") %>%
  mutate(scenario = gsub("diff_", "", scenario)) %>%
  filter(year <= 2050,
         year > 2015)

techs = c("a oil","b natural gas","c coal",
          "d biomass","e nuclear","f hydro","g wind","h solar","i geothermal",
          "j traditional biomass","H2 industrial")

my_pal_energy = jgcricolors::jgcricol()$pal_all

ggplot(en %>% filter(year == 2050), aes(x = region, y = value, fill = fuel)) +
  geom_col() +
  facet_grid( ~ scenario) + 
  theme_bw() + 
  labs(x = "", y = "kcal/yr") + 
  theme(legend.position = "right",
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        axis.text.x = element_text(size = 7, angle = 90, hjust = 1, vjust = .5),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 12)) + 
  scale_fill_manual(values = my_pal_energy[names(my_pal_energy) %in% techs]) 

ggsave(paste0(here::here(), "/figures/Food_sce_dec_Check.tiff"),last_plot(), "tiff")

# ---

# 2 - FOOD DEMAND  ----

# 2.1 Regional changes in food consumption ----
food <- getQuery(prj, "food demand") %>%
  separate(`gcam-consumer`, c("adj", "decile")) %>%
  separate(input, c("adj2", "demand")) %>%
  mutate(decile = gsub("Group", "d", decile)) %>%
  select(scenario, region, year, decile, demand, value, Units) %>%
  mutate(scenario = if_else(grepl("Gini25", scenario), "Gini25", scenario),
         scenario = if_else(grepl("Gini50", scenario), "Gini50", scenario),
         scenario = if_else(grepl("RegGHGPol_Ineq", scenario), "Baseline", scenario)) %>%
  pivot_wider(names_from = "scenario",
              values_from = "value") %>%
  mutate(diff_Gini25 = Gini25 - Baseline,
         diff_Gini50 = Gini50 - Baseline) %>%
  select(year, region, demand, decile, Units, starts_with("diff")) %>%
  pivot_longer(cols = c("diff_Gini25", "diff_Gini50"),
               names_to = "scenario",
               values_to = "value") %>%
  mutate(scenario = gsub("diff_", "", scenario)) %>%
  filter(year <= 2050,
         year > 2015)


food.map <- food %>%
  rename(sce = scenario) %>%
  group_by(sce, subRegion = region, year, demand, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup()
  
  
  map_food <- rmap::map(data = food.map %>% filter(year == 2050),
                        shape = mapGCAMReg32,
                        folder ="figures/maps/food",
                        legendType = "kmeans",
                        palette = "pal_div_BrGn",
                        row = "sce",
                        col = "demand",
                        #legendType = "continuous",
                        background  = T)

map_food_2050 <- map_food$map_param_KMEANS +
  theme(strip.text.y = element_text(size = 11),
        strip.text.x = element_text(size = 11),
        plot.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8))  

ggsave(paste0(here::here(), "/figures/Food_map_diffSce_2050.tiff"),map_food_2050, "tiff")

#---
# Calculate changes by food group
food.sub <- getQuery(prj, "food consumption by type (specific)") %>%
  mutate(subsector = if_else(grepl("Animal", `subsector...4`), "Animal protein", subsector...3),
         subsector = if_else(grepl("Plant", `subsector...4`), "Plant protein", subsector),
         subsector = if_else(grepl("FiberCrop", `subsector...4`), "Other crops", subsector),
         subsector = if_else(grepl("MiscCrop", `subsector...4`), "Other crops", subsector),
         subsector = if_else(grepl("OtherGrain", `subsector...4`), "Other crops", subsector),
         subsector = if_else(grepl("Root", `subsector...4`), "Other crops", subsector),
         subsector = if_else(grepl("Sugar", `subsector...4`), "Other crops", subsector),
         subsector = if_else(grepl("Oil", `subsector...4`), "Other crops", subsector),) %>%
  group_by(scenario, region, subsector, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  filter(year <= 2050,
         year > 2015) %>%
  mutate(scenario = if_else(grepl("Gini25", scenario), "Gini25", scenario),
         scenario = if_else(grepl("Gini50", scenario), "Gini50", scenario),
         scenario = if_else(grepl("RegGHGPol_Ineq", scenario), "Baseline", scenario)) %>%
  pivot_wider(names_from = "scenario",
              values_from = "value") %>%
  mutate(diff_Gini25 = Gini25 - Baseline,
         diff_Gini50 = Gini50 - Baseline) %>%
  select(year, region, subsector, Units, starts_with("diff")) %>%
  pivot_longer(cols = c("diff_Gini25", "diff_Gini50"),
               names_to = "scenario",
               values_to = "value") %>%
  mutate(scenario = gsub("diff_", "", scenario))

food.sub.map <- food.sub %>%
  rename(sce = scenario) %>%
  group_by(sce, subRegion = region, year, subsector, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup()

map_food.sub <- rmap::map(data = food.sub.map %>% filter(year == 2050),
                      shape = mapGCAMReg32,
                      folder ="figures/maps/foodSubsector",
                      legendType = "kmeans",
                      palette = "pal_div_BrGn",
                      row = "sce",
                      col = "subsector",
                      #legendType = "continuous",
                      background  = T)

map_food_sub_2050 <- map_food.sub$map_param_KMEANS +
  theme(strip.text.y = element_text(size = 6),
        strip.text.x = element_text(size = 6),
        plot.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7))  +
  theme(legend.key.size = unit(0.4, "cm"))

ggsave(paste0(here::here(), "/figures/FoodSub_map_diffSce_2050.tiff"),map_food_sub_2050, "tiff")

# 2.2 Within-region changes in calorie consumption ----
pop <- getQuery(prj, "Population by region") %>%
  mutate(value = value * 1E3) %>%
  as_tibble() %>%
  repeat_add_columns(tibble(decile = unique(food$decile))) %>%
  mutate(pop = value / 10) %>%
  select(-value, -Units) %>%
  mutate(scenario = if_else(grepl("Gini25", scenario), "Gini25", scenario),
         scenario = if_else(grepl("Gini50", scenario), "Gini50", scenario),
         scenario = if_else(grepl("RegGHGPol_Ineq", scenario), "Baseline", scenario)) %>%
  left_join_error_no_match(read.csv("data/map_GCAM_reg.csv"), by = join_by(region)) %>%
  group_by(scenario, region_agg, decile, year) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  rename(region = region_agg)

food_pc <- getQuery(prj, "food demand") %>%
  separate(`gcam-consumer`, c("adj", "decile")) %>%
  separate(input, c("adj2", "demand")) %>%
  mutate(decile = gsub("Group", "d", decile)) %>%
  select(scenario, region, year, decile, demand, value, Units) %>%
  mutate(scenario = if_else(grepl("Gini25", scenario), "Gini25", scenario),
         scenario = if_else(grepl("Gini50", scenario), "Gini50", scenario),
         scenario = if_else(grepl("RegGHGPol_Ineq", scenario), "Baseline", scenario))  %>%
  left_join_error_no_match(read.csv("data/map_GCAM_reg.csv"), by = join_by(region)) %>%
  group_by(scenario, region_agg, decile, demand, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  rename(region = region_agg) %>%
  left_join_error_no_match(pop, by = join_by(year, region, decile, scenario)) %>%
  mutate(value = value * 1000000000000 / pop /365,
         Units = "kcal/day") %>%
  pivot_wider(names_from = "scenario",
              values_from = "value") %>%
  mutate(diff_Gini25 = Gini25 - Baseline,
         diff_Gini50 = Gini50 - Baseline) %>%
  select(year, region, demand, decile, Units, starts_with("diff")) %>%
  pivot_longer(cols = c("diff_Gini25", "diff_Gini50"),
               names_to = "scenario",
               values_to = "value") %>%
  mutate(scenario = gsub("diff_", "", scenario)) %>%
  filter(year <= 2050,
         year > 2015) %>%
  mutate(region = gsub("_", " ", region),
         region = if_else(grepl("Caribbean", region), "CAC", region),
         region = if_else(grepl("Trade", region), "EFTA", region))


ggplot(food_pc %>% filter(year == 2050), aes(x = region, y = value, fill = factor(decile, levels = c("d1", "d2", "d3", "d4", "d5",
                                                                          "d6", "d7", "d8", "d9", "d10")) )) +
  geom_col() +
  facet_grid(demand ~ scenario) + 
  theme_bw() + 
  labs(x = "", y = "kcal/pc/day") + 
  theme(legend.position = "right",
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        axis.text.x = element_text(size = 7, angle = 90, hjust = 1, vjust = .5),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 12)) + 
  scale_fill_manual(values = my_pal)

ggsave(paste0(here::here(), "/figures/Food_sce_dec_Check.tiff"),last_plot(), "tiff")

# 2.3 Changes in nutrition ----




# 3 - DEMAND INEQUALITIES ----

# ---
# HOUSEHOLD ENERGY
hh.energy <- getQuery(prj, "building final energy by service and fuel") %>%
  filter(scenario %in% sel_sce) %>%
  mutate(scenario = if_else(grepl("Gini25", scenario), "Gini25", scenario),
         scenario = if_else(grepl("Gini50", scenario), "Gini50", scenario),
         scenario = if_else(scenario == "RegGHGPol_Ineq", "Baseline", scenario)) %>%
  # filter modern fuels
  filter(grepl("modern", sector)) %>%
  separate(sector, c("sector", "decile"), sep = "_") %>%
  mutate(sector = if_else(grepl("cooling", sector), "Resid cooling", sector),
         sector = if_else(grepl("heating", sector), "Resid heating", sector),
         sector = if_else(grepl("others", sector), "Resid non-thermal", sector)) %>%
  group_by(scenario, region, sector, decile, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  pivot_wider(names_from = "decile",
              values_from = "value") %>%
  mutate(palma = d10 / (d1 + d2 + d3 + d4)) %>%
  select(scenario, region, sector, year, palma)

# ---
# TRN ENERGY
trn.energy <- getQuery(prj, "transport service output by tech") %>%
  filter(scenario %in% sel_sce) %>%
  mutate(scenario = if_else(grepl("Gini25", scenario), "Gini25", scenario),
         scenario = if_else(grepl("Gini50", scenario), "Gini50", scenario),
         scenario = if_else(scenario == "RegGHGPol_Ineq", "Baseline", scenario)) %>%
  filter(grepl("trn_pass_d", sector) | grepl("aviat", sector)) %>%
  mutate(sector = sub("_([^_]*)$", "_split_\\1", sector)) %>%
  tidyr::separate(sector, into = c("sector", "decile"), sep = "_split_", extra = "merge", fill = "right") %>%
  group_by(scenario, region, sector, decile, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(sector = if_else(grepl("aviation", sector), "International aviation", "Domestic transportation")) %>%
  pivot_wider(names_from = "decile",
              values_from = "value") %>%
  mutate(palma = d10 / (d1 + d2 + d3 + d4)) %>%
  select(scenario, region, sector, year, palma)
  
# ---
# FOOD
 food <- getQuery(prj, "food demand") %>%
  filter(scenario %in% sel_sce) %>%
  mutate(scenario = if_else(grepl("Gini25", scenario), "Gini25", scenario),
         scenario = if_else(grepl("Gini50", scenario), "Gini50", scenario),
         scenario = if_else(scenario == "RegGHGPol_Ineq", "Baseline", scenario)) %>%
  filter(grepl("NonStaples", input)) %>%
  separate( `gcam-consumer`, c("sector", "decile"), sep = "_") %>%
  mutate(decile = gsub("Group", "d", decile),
         sector = "Non-staple food") %>%
  group_by(scenario, region, sector, decile, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  pivot_wider(names_from = "decile",
              values_from = "value") %>%
  mutate(palma = d10 / (d1 + d2 + d3 + d4)) %>%
  select(scenario, region, sector, year, palma)

 # ---
# Figure
demand_ineq <- bind_rows(hh.energy, trn.energy, food) %>%
  dplyr::rename(subRegion = region,
                value = palma,
                class = sector) %>%
  filter(year %in% c(2030, 2050)) %>%
  mutate(scenario = factor(scenario, levels = c("Baseline", "Gini25", "Gini50")),
         class = factor(class, levels = c("Resid cooling", "Resid heating", "Resid non-thermal",
                                            "Domestic transportation", "International aviation",
                                          "Non-staple food")))

demand_ineq_an <- demand_ineq %>%
  pivot_wider(names_from = "scenario",
              values_from = "value") %>%
  mutate(diff_Gini25 = Gini25 / Baseline,
         diff_Gini50 = Gini50 / Baseline)

mapx <- rmap::map(data = demand_ineq,
          shape = mapGCAMReg32,
          folder ="figures/maps/palma",
          legendType = "kmeans",
          #palette = "Reds",
          #legendType = "continuous",
          background  = T)

map_2030 <- mapx$map_param_2030_KMEANS +
  theme(strip.text.y = element_text(size = 6),
        strip.text.x = element_text(size = 6),
        plot.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8))

map_2050 <- mapx$map_param_2050_KMEANS +
  theme(strip.text.y = element_text(size = 6),
        strip.text.x = element_text(size = 6),
        plot.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8))

ggsave(paste0(here::here(), "/figures/map_2030.tiff"), map_2030, "tiff")
ggsave(paste0(here::here(), "/figures/map_2050.tiff"), map_2050, "tiff")


# 4 - CLIMATE CHANGE MITIGATION: CARBON PICES -----
c.price <- getQuery(prj, "CO2 prices") %>%
  filter(!grepl("FUG", market),
         !grepl("LUC", market),
         year >= 2015,
         year <= 2050) %>%
  mutate(value = value * gdp_deflator(2015, 1990),
         Units = "2015$/tC") %>%
  mutate(scenario = if_else(grepl("Gini25", scenario), "Gini25", scenario),
         scenario = if_else(grepl("Gini50", scenario), "Gini50", scenario),
         scenario = if_else(scenario == "RegGHGPol_Ineq", "Baseline", scenario)) %>%
  mutate(region = gsub("CO2", "", market)) %>%
  select(-market) %>%
  mutate(region = gsub("Middle_East", "Middle East", region),
         region = gsub("South_Africa", "South Africa", region))


ggplot(c.price %>%
         filter(region %in% selected_gcam_regions), aes(x = year, y = value, color = factor(scenario, levels = c("Narayan_etal_2023",  "Gini25", "Gini50")))) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~region, scales = "free") + 
  theme_bw() + 
  labs(x = "", y = "CPrice (2015$/tC)") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 13),
        plot.title = element_text(size = 18, hjust = .5)) + 
  scale_color_manual(values = c("orange", "dodgerblue1", "forestgreen")) + 
  ggtitle("Regional carbon tax")
ggsave(paste0(here::here(), "/figures/Reg_CPrice.tiff"),last_plot(), "tiff")

#---

diff.c.price <- c.price %>%
  # adjust region names to rmap names
  mutate(
    region = if_else(region == "CAC", "Central America and Caribbean", region),
    region = if_else(region == "Central_Asia", "Central Asia", region),
    region = if_else(region == "EFTA", "European Free Trade Association", region),
    region = if_else(region == "EU-12", "EU_12", region),
    region = if_else(region == "EU-15", "EU_15", region),
    region = if_else(region == "Middle_East", "Middle East", region),
    region = if_else(region == "SAN", "South America_Northern", region),
    region = if_else(region == "SAS", "South America_Southern", region),
    region = if_else(region == "South_Africa", "South Africa", region),
    region = if_else(region == "South_Asia", "South Asia", region),
    region = if_else(region == "South_Korea", "South Korea", region),
    region = if_else(region == "Southeast_Asia", "Southeast Asia", region)
  ) %>%
  pivot_wider(names_from = "scenario",
              values_from = "value") %>%
  mutate(Gini25 = (Gini25 - Baseline),
         Gini50 = (Gini50 - Baseline)) %>%
  select(-Baseline, -Units) %>%
  pivot_longer(cols = c("Gini25", "Gini50"),
               names_to = "scenario",
               values_to = "diff") %>%
  replace_na(list(diff = 0)) %>%
  mutate(units = "2015$") 

diff.c.price_perc <- c.price %>%
  # adjust region names to rmap names
  mutate(
    region = if_else(region == "CAC", "Central America and Caribbean", region),
    region = if_else(region == "Central_Asia", "Central Asia", region),
    region = if_else(region == "EFTA", "European Free Trade Association", region),
    region = if_else(region == "EU-12", "EU_12", region),
    region = if_else(region == "EU-15", "EU_15", region),
    region = if_else(region == "Middle_East", "Middle East", region),
    region = if_else(region == "SAN", "South America_Northern", region),
    region = if_else(region == "SAS", "South America_Southern", region),
    region = if_else(region == "South_Africa", "South Africa", region),
    region = if_else(region == "South_Asia", "South Asia", region),
    region = if_else(region == "South_Korea", "South Korea", region),
    region = if_else(region == "Southeast_Asia", "Southeast Asia", region)
  ) %>%
  pivot_wider(names_from = "scenario",
              values_from = "value") %>%
  mutate(Gini25 = (Gini25 - Baseline) / Baseline,
         Gini50 = (Gini50 - Baseline) / Baseline) %>%
  select(-Baseline, -Units) %>%
  pivot_longer(cols = c( "Gini25", "Gini50"),
               names_to = "scenario",
               values_to = "diff") %>%
  replace_na(list(diff = 0)) %>%
  mutate(units = "2015$") 

  
map.diff.c.price  <- diff.c.price %>%
  rename(value = diff,
         subRegion = region) %>%
  mutate(units = "2015$/tCO2-eq",
         year = as.numeric(as.character(year))) %>%
  filter(year %in% c(2050))

map.diff.c.price_perc  <- diff.c.price_perc %>%
  rename(value = diff,
         subRegion = region) %>%
  mutate(units = "%",
         year = as.numeric(as.character(year)),
         value = value * 100) %>%
  filter(year %in% c(2050))


map_crpiceX <- rmap::map(data = map.diff.c.price,
          shape = rmap::mapGCAMReg32,
          folder = paste0(here::here(), "/figures/maps/cPrice"),
          legendType = "pretty",
          palette = "pal_div_BlRd",
          background  = T,
          animate = T)

map_cPrice_perc2050<- rmap::map(data = map.diff.c.price_perc,
                         shape = rmap::mapGCAMReg32,
                         folder = paste0(here::here(), "/figures/maps/cPrice_perc"),
                         legendType = "kmeans",
                         palette = "pal_div_BlRd",
                         background  = T,
                         animate = T)

map_Cprice_2050 <- map_crpiceX$map_param_PRETTY +
  ggtitle("A)") + 
  theme(strip.text.y = element_text(size = 10),
        plot.title = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10))

map_Cprice_perc_2050 <- map_cPrice_perc2050$map_param_KMEANS +
  ggtitle("B)") + 
  theme(strip.text.y = element_text(size = 10),
        plot.title = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10))


ggsave(paste0(here::here(), "/figures/map_Cprice_2050.tiff"), map_Cprice_2050, "tiff")
ggsave(paste0(here::here(), "/figures/map_Cprice_perc_2050.tiff"), map_Cprice_perc_2050, "tiff")

# =======================================================================
# Check emission increases with CPrices ----

# Load project with emissions
prj_em <- loadProject("Paper_inequality_emissions.dat")
listScenarios(prj_em)
listQueries(prj_em)

# Read GWP
GWP <- read.csv("data/ghg_GWP.csv") %>%
  rename(ghg = GHG_gases)

# Load GHG
co2 <- getQuery(prj_em, "CO2 emissions by region") %>%   mutate(ghg = "CO2")
luc <- getQuery(prj_em, "LUC emissions by region") %>% select(-landleaf) %>% mutate(ghg = "CO2LUC")

ghg <- getQuery(prj_em, "nonCO2 emissions by region") %>%
  bind_rows(luc) %>%
  bind_rows(co2) %>%
  filter(year >= 2015) %>%
  left_join(GWP, by = c("ghg")) %>%
  mutate(value = value * GWP,
         Units = "MtCO2e") %>%
  group_by(scenario, region, year, Units) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  filter(year > 2015,
         year <= 2050) %>%
  mutate(scenario = if_else(grepl("Gini25", scenario), "Gini25", scenario),
         scenario = if_else(grepl("Gini50", scenario), "Gini50", scenario),
         scenario = if_else(grepl("RegGHGPol", scenario), "Base", scenario))

ghg_an <- ghg %>%
  pivot_wider(names_from = "scenario",
              values_from = "value") %>%
  mutate(year = as.numeric(year)) %>%
  mutate(diff_Gini25 = (Gini25 - Base) / Base,
         diff_Gini50 = (Gini50 - Base) / Base)


ghg_gas <- getQuery(prj_em, "nonCO2 emissions by region") %>%
  bind_rows(luc) %>%
  bind_rows(co2) %>%
  filter(year >= 2015) %>%
  left_join(GWP, by = c("ghg")) %>%
  mutate(value = value * GWP,
         Units = "MtCO2e") %>%
  mutate(group = if_else(ghg == "CO2LUC", "CO2_LUC", "a"),
         group = if_else(ghg == "CO2", "CO2", group),
         group = if_else(grepl("CH4", ghg), "CH4", group),
         group = if_else(group == "a", "F-gases", group)) %>%
  group_by(scenario, region, group, year, Units) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  filter(year > 2015,
         year <= 2050) %>%
  mutate(scenario = if_else(grepl("Gini25", scenario), "Gini25", scenario),
         scenario = if_else(grepl("Gini50", scenario), "Gini50", scenario),
         scenario = if_else(grepl("RegGHGPol", scenario), "Base", scenario))

  

ggplot(ghg %>% filter(region %in% selected_gcam_regions), aes(x = as.numeric(year), y = value, color = scenario))+
  geom_line() +
  facet_wrap(~ region, scales = "free") + 
  theme_bw() + 
  labs(x = "", y = "MtCO2e") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        legend.text = element_text(size = 11),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12)) + 
  scale_color_manual(values = c("orange", "dodgerblue1", "forestgreen"))

ggplot(ghg_gas %>% filter(region %in% c("Africa_Eastern", "Africa_Northern", "India", "South Africa")), aes(x = as.numeric(year), y = value, color = scenario))+
  geom_line() +
  facet_grid(group ~ region, scales = "free") + 
  theme_bw() + 
  labs(x = "", y = "MtCO2e") + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 10),
        legend.text = element_text(size = 11),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12)) + 
  scale_color_manual(values = c("orange", "dodgerblue1", "forestgreen"))


