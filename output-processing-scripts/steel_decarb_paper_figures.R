# script to process data and generate figures for the steel decarbonization paper

library(plyr)
library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(devtools)
library(cowplot)
library(RColorBrewer)
library(readxl)
library(jgcricolors)
library(ggsci)
library(zoo)
library(extrafont)
loadfonts(quiet = T)
options(scipen=999)

# LOAD DATA AND SET DIRECTORIES ---------------

# set directories
run_dir <- "C:/Users/spei632/Documents/GCAM_industry/data/steel_decarb/steel_decarb_run_9-30-22/"
fig_dir <- paste0(run_dir,"/figures_till_2050_paper")
#fig_dir <- paste0(run_dir,"/figures_till_2100")
#fig_dir <- paste0(run_dir,"/figures_no_EU_new_gas")
if (!dir.exists(fig_dir)) {dir.create(fig_dir)}
results_dir <- fig_dir
setwd("C:/Users/spei632/Documents/GCAM_industry/R/steel_decarb_paper")

# set years to plot
plot_years <- c(seq(2015,2050))
#plot_years <- c(seq(2015,2100))

# load functions
source("functions_final/functions.R")
source("functions_final/diag_util_functions.R")

# load mapping files
region_mapping <- read.csv("mappings_final/steel_region_mapping.csv")
regions_aggregated <- unique(region_mapping$steel_region)
CO2_sector_mapping <- read.csv("mappings_final/CO2_nonbio_sector_mapping.csv")
all_ghg_sector_mapping <- read.csv("mappings_final/all_ghg_CO2_nobio_sector_mapping.csv")
hydrogen_mapping <- read.csv("mappings_final/hydrogen_production_mapping.csv")
elec_mapping <-read.csv("mappings_final/elec_mapping.csv")
GWP_AR5 <- read.csv("mappings_final/GWP_AR5.csv")

# load constants
C_to_CO2 <- 44/12
EJ_to_TWh <- 277.778
USD1990_to_2015 <- 1.64753738
USD1975_to_2015 <- 3.508771929
THOUS_to_MILL <- 0.001

# load data
queries <- list.files(run_dir, pattern = 'queryoutall')

for (i in queries) {
  filename <- gsub('.csv','', i) %>% gsub('queryoutall_','', .)
  assign(filename, readr::read_csv(paste0(run_dir, "/", i), skip = 1))
}

# load IPCC data
AR6_data <- readr::read_csv("AR6_Scenarios_Database_World_v1p0.csv")
AR6_metadata <- readr::read_csv("AR6_Scenarios_Database_metadata_indicators_v1.0_meta.csv")

# PREPARE DATA -----------------

# tidy data and add global
electricity <- electricity %>% parse_output_scenario %>% add_global_sum()
CO2_emissions <- CO2_emissions %>% parse_output_scenario %>% add_global_sum()
CO2_emissions_sector_nobio <- CO2_emissions_sector_nobio_v2 %>% parse_output_scenario %>% add_global_sum()
LU_CO2_emissions <- LU_CO2_emissions %>% parse_output_scenario %>% add_global_sum()
final_ene_sect_fuel <- final_ene_sect_fuel %>% parse_output_scenario %>% add_global_sum()
industry_energy_tech_fuel <- industry_energy_tech_fuel %>% parse_output_scenario %>% add_global_sum()
ironsteel_production <- ironsteel_production %>% parse_output_scenario %>% add_global_sum()
ironsteel_production_tech <- ironsteel_production_tech %>% parse_output_scenario %>% add_global_sum()
ironsteel_input_tech <- ironsteel_input_tech %>% parse_output_scenario %>% add_global_sum()
nonCO2_em_sector <- nonCO2_em_sector %>% parse_output_scenario() %>% add_global_sum()
hydrogen_inputs_tech <- hydrogen_inputs_tech %>% parse_output_scenario() %>% add_global_sum()
nonCO2_emissions_resource_prod <- nonCO2_emissions_resource_prod %>% parse_output_scenario() %>% add_global_sum()
CO2_squestration_tech <- CO2_squestration_tech %>% parse_output_scenario() %>% add_global_sum()
global_mean_temp <- global_mean_temp %>% parse_output_scenario()

# convert emissions C to CO2
CO2_emissions <- CO2_emissions %>% dplyr::mutate(value = if_else(Units == "MTC", value * C_to_CO2, value),
                                                 Units = if_else(Units == "MTC", "MTCO2", Units))
LU_CO2_emissions <- LU_CO2_emissions %>% dplyr::mutate(value = if_else(Units == "MtC/yr", value * C_to_CO2, value),
                                                       Units = if_else(Units == "MtC/yr", "MtCO2/yr", Units))
CO2_emissions_sector_nobio <- CO2_emissions_sector_nobio %>% dplyr::mutate(value = if_else(Units == "MTC", value * C_to_CO2, value),
                                                                           Units = if_else(Units == "MTC", "MTCO2", Units))

# aggregate to deep dive regions and ROW
electricity <- electricity %>% aggregate_regions(region_mapping, colname = "steel_region")
CO2_emissions <- CO2_emissions %>% aggregate_regions(region_mapping, colname = "steel_region")
CO2_emissions_sector_nobio <- CO2_emissions_sector_nobio %>% aggregate_regions(region_mapping, colname = "steel_region")
LU_CO2_emissions <- LU_CO2_emissions %>% aggregate_regions(region_mapping, colname = "steel_region")
final_ene_sect_fuel <- final_ene_sect_fuel %>% aggregate_regions(region_mapping, colname = "steel_region")
industry_energy_tech_fuel <- industry_energy_tech_fuel %>% aggregate_regions(region_mapping, colname = "steel_region")
ironsteel_production <- ironsteel_production %>% aggregate_regions(region_mapping, colname = "steel_region")
ironsteel_production_tech <- ironsteel_production_tech %>% aggregate_regions(region_mapping, colname = "steel_region")
ironsteel_input_tech <- ironsteel_input_tech %>% aggregate_regions(region_mapping, colname = "steel_region")
hydrogen_production_tech <- hydrogen_production_tech %>% parse_output_scenario %>% add_global_sum()%>% aggregate_regions(region_mapping, colname = "steel_region")
nonCO2_em_sector <- nonCO2_em_sector %>% aggregate_regions(region_mapping, colname = "steel_region")
hydrogen_inputs_tech <- hydrogen_inputs_tech %>% aggregate_regions(region_mapping, colname = "steel_region")
nonCO2_emissions_resource_prod <- nonCO2_emissions_resource_prod %>% aggregate_regions(region_mapping, colname = "steel_region")
CO2_squestration_tech <- CO2_squestration_tech %>% aggregate_regions(region_mapping, colname = "steel_region")

# SET PLOT THEMES AND COLORS ---------------
# plot themes
plot_theme <- theme_bw() +
  theme(legend.text = element_text(size = 15, vjust = 0.5)) +
  theme(legend.title = element_text(size = 15, vjust = 2)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title = element_text(size = 15, face = "bold")) +
  theme(plot.title = element_text(size = 15, face = "bold", vjust = 1)) +
  theme(plot.subtitle = element_text(size = 15, face = "bold", vjust = 1))+
  theme(strip.text = element_text(size = 15))+
  theme(strip.text.x = element_text(size = 15, face = "bold"))+
  #theme(legend.position = "bottom")+
  theme(legend.text = element_text(size = 15))+
  theme(legend.title = element_text(size = 15,color = "black",face="bold"))+
  theme(axis.text.x= element_text(angle=60,hjust=1))+
  theme(legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())+
  theme(text=element_text(family="Calibri", size=16))

pie_theme <- theme_bw() +
  theme(legend.text = element_text(size = 14, vjust = 0.5)) +
  theme(legend.title = element_text(size = 14, vjust = 2)) +
  theme(axis.text = element_text(size = 14)) +
  theme(axis.title = element_text(size = 14, face = "bold")) +
  theme(plot.title = element_text(size = 14, face = "bold", vjust = 1)) +
  theme(plot.subtitle = element_text(size = 14, face = "bold", vjust = 1))+
  theme(strip.text = element_text(size = 14))+
  #theme(strip.text.x = element_text(size = 9, face = "bold"))+
  #theme(legend.position = "bottom")+
  theme(legend.text = element_text(size = 14))+
  theme(legend.title = element_text(size = 14,color = "black",face="bold"))+
  #theme(axis.text.x= element_text(angle=60,hjust=1))+
  theme(legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))+
  theme(text=element_text(family="Calibri", size=16))

blank_theme <- pie_theme+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_rect(colour="black",size=1),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.title=element_text(size=14, face="bold"),
  )

# plot colors 
# add missing colors
# input_colors  <- c(pal_all,"delivered biomass" = "#00931d",
#                    "delivered coal" = "gray20",
#                    "H2 enduse" = "darkgoldenrod3",
#                    "hydrogen" = "peachpuff2",
#                    "refined liquids industrial" = "#d01c2a",
#                    "gas" = "#d01c2a",
#                    "gas with CCS" ="peachpuff2",
#                    "refined liquids" = "#756bb1",
#                    "refined liquids with CCS" = "#bcbddc",
#                    "scrap" = "#666666",
#                    "coal" = "grey20",
#                    "coal with CCS" = "grey85")

scenario_colors <- c("Ref"= "#E31A1C",
                     "1p5 delay" = "#3182bd",
                     "1p5" = "#33A02C",
                     "1p5 no CCS" = "334FFF")

scenario_colors_IPCC_comp <- c(scenario_colors,
                               "AR6 reference" = "tan",
                               "AR6 1.5C" = "gray")

scenario_colors_no_new_EU_gas <- c("1p5" = "#33A02C",
                                   "1p5 no CCS" = "334FFF",
                                   "1p5 no new EU gas" = "#3182bd")

# tech_colors  <- c("BF-CCUS" = "#FB9A99",
#                   "BF-BOF" = "#E31A1C",
#                   "EAF-scrap" = "#756bb1",
#                   "DRI-EAF-CCUS" = "#9ecae1",
#                   "DRI-EAF-Fossil" = "#3182bd",
#                   "BF-biomass" = "#33A02C",
#                   "DRI-EAF-H2" = "#FCD581",
#                   "BF-H2" = "darkgoldenrod3")

tech_colors  <- c("BF-biomass" = "#33A02C",
                  "BF-BOF" = "#E31A1C",
                  "BF-CCUS" = "#FB9A99",
                  "BF-H2" = "darkgoldenrod3",
                  "DRI-EAF-Fossil" = "#3182bd",
                  "DRI-EAF-CCUS" = "#9ecae1",
                  "EAF-scrap" = "#756bb1",
                  "DRI-EAF-H2" = "#FCD581")

mat_eff_colors <- c("Direct reuse" = "#33A02C" , 
                    'Optimized building design' ="#E31A1C", 
                    'High-strength steel'="#FB9A99", 
                    'Lifetime extension'="darkgoldenrod3", 
                    'Post-use recycling'="#3182bd", 
                    'Lightweighting'="#9ecae1",
                    'Improved semi-manufacturing yields'="#756bb1",
                    'Improved product manufacturing yields'="#FCD581")

regions <- c("China" = "#E31A1C","Europe"="#FB9A99","India"="#756bb1",
             "Japan"="#9ecae1","ROW"="#3182bd","South Korea"="#33A02C",
             "US"="#FCD581")

fuel_colors <- c("biomass" = "#33A02C", "coal" = "gray27",
                 "coal with CCS" = "gray57", "electricity" = "#756bb1","gas" = "#3182bd", 
                 "gas with CCS" = "#9ecae1","hydrogen" = "#FCD581",
                 "refined liquids" = "#E31A1C", "refined liquids with CCS" = "#FB9A99")                

hydrogen_pal <- c("blue"="#3182bd", "grey"="gray57", "green"="#33A02C")

hydrogen_tech_pal <- c("biomass to H2" = "#33A02C", "biomass to H2 CCS" = "#3182bd",
                       "coal chemical CCS" = "gray57", "electrolysis" = "#756bb1", 
                       "natural gas steam reforming" = "#E31A1C",
                       "natural gas steam reforming CCS" = "#FB9A99", 
                       "thermal splitting" = "#FCD581")

sector_colors <- c("agriculture" = "#3182bd", "buildings" = "#9ecae1", "electricity"= "#756bb1", 
                   "hydrogen" = "#FCD581", "industry" = "#E31A1C",
                   "iron and steel" = "#FB9A99", "LULUCF" = "#33A02C", 
                   "oil and gas" = "gray57", "other energy supply" = "334FFF", 
                   "transportation" = "darkgoldenrod3")
sector_colors_CO2 <- c("agriculture" = "#3182bd", "buildings" = "#9ecae1", 
                   "industry" = "#E31A1C", "iron and steel" = "#FB9A99", 
                   "oil and gas" = "gray57", 
                   "transportation" = "darkgoldenrod3", "other energy supply" = "334FFF", 
                   "LULUCF" = "#33A02C", "hydrogen" = "#FCD581", "electricity"= "#756bb1", 
                   "CO2 removal" = "black")

electricity_tech_pal <- c("biomass" = "#33A02C", "biomass (CCS)" = "lightgreen", 
                          "coal" = "gray27", "coal (CCS)" = "gray57",
                          "concentrated solar power" = "darkgoldenrod3", "gas" = "#3182bd", 
                          "gas (CCS)" = "#9ecae1", "geothermal" = "#756bb1",
                          "hydro" = "334FFF", "nuclear generation" = "orange4",
                          "PV" = "#FCD581", "refined liquids" = "#E31A1C", "refined liquids (CCS)" = "#FB9A99", 
                          "wind" = "plum")


# SET SCENARIO LABELS ----------
scenarios = c("Reference_1","1p5C_12", "1p5C_13","1p5C_delay_14")
scenario_labels = c("Ref","1p5", "1p5 no CCS", "1p5 delay")
scenarios_no_new_EU_gas = c("1p5C_12", "1p5C_16_no_new_EU_gas_DRI", "1p5C_13")
scenario_labels_no_new_EU_gas = c("1p5", "1p5 no new EU gas", "1p5 no CCS")

# GLOBAL NET CO2 EMISSIONS --------------------
# calculate net CO2 emissions 
net_co2 <- CO2_emissions %>%
  bind_rows(LU_CO2_emissions %>% filter(year %in% unique(CO2_emissions$year))) %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()

net_co2$scenario <- factor(net_co2$scenario, levels = scenarios, labels=scenario_labels)

ggplot(data=filter(net_co2, year %in% plot_years,scenario!="NA", region!="ROW", region!="Global",scenario!="NA"),aes(x=year, y=value / 1000, color=scenario))+
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~region, scales = "free") +
  labs(title = expression(bold(Net~CO[2]~emissions)), x="", y=bquote(Gt~CO[2])) +
  scale_color_manual(values = scenario_colors, name = "Scenario") +
  plot_theme 
ggsave(paste0(fig_dir, "/net_co2_emissions_all_regions.png"), height = 6, width = 9, units = "in")


for (i in regions_aggregated) {
  ggplot(data=filter(net_co2, region == i, year %in% plot_years, region!="NA",scenario!="NA"),
         aes(x=year, y=value / 1000, color=scenario)) +
    geom_line(size = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
    labs(title = bquote(bold(.(i)~net~CO[2]~emissions)), x="", y=bquote(Gt~CO[2])) +
    scale_color_manual(values = scenario_colors, name = "Scenario") +
    plot_theme 
  ggsave(paste0(fig_dir, "/net_co2_emissions_", i, ".png"), height = 6, width = 9, units = "in")
}

# plot without no CCS pathway, just global
ggplot(data=filter(net_co2, region == "Global", year %in% plot_years, region!="NA",scenario!="NA", 
                   scenario!= "1p5 no CCS"),
       aes(x=year, y=value / 1000, color=scenario)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  labs(title = bquote(bold(Global~net~CO[2]~emissions)), x="", y=bquote(Gt~CO[2])) +
  scale_color_manual(values = scenario_colors, name = "Scenario", limits = force, drop = TRUE) +
  plot_theme 
ggsave(paste0(fig_dir, "/net_co2_emissions_Global_without_no_CCS.png"), height = 6, width = 9, units = "in")


# save data 
net_co2_data <- filter(net_co2, region %in% regions_aggregated,
                       year %in% plot_years,scenario!="NA")
write.csv(net_co2_data, paste0(results_dir, '/net_co2.csv'))

# STEEL CO2 EMISSIONS BY REGION PIE AND BAR CHART --------------
CO2_emissions_sector_nobio$scenario <- factor(CO2_emissions_sector_nobio$scenario,
                                              levels= scenarios, labels = scenario_labels)

ironsteel_CO2 <- filter(CO2_emissions_sector_nobio, sector=="iron and steel",
                        scenario!="NA", region != "Global")

ironsteel_CO2_total <- ironsteel_CO2 %>% 
  group_by(year, scenario) %>% 
  summarize(sum = sum(value))

ironsteel_CO2 <- ironsteel_CO2 %>%
  left_join(ironsteel_CO2_total, by = c("year","scenario"))%>%
  mutate(share = value/sum*100)

pie_years <- c(2020,2030,2050)
ggplot(filter(ironsteel_CO2, year%in%pie_years, scenario=="1p5"), 
       aes(x= "", y = share , fill = region)) +
  geom_bar(stat = "identity", width = 1) + 
  scale_fill_manual(values = regions, name = "Region") +
  facet_wrap(~year)+
  coord_polar("y", start = 0) + 
  plot_theme +
  theme(axis.text.x=element_blank()) +
  blank_theme + 
  labs(title = expression(bold(paste(Regional~contributions~to~total~iron~and~steel~CO[2]~emissions,", 1p5 scenario"))))

ggsave(paste0(fig_dir, "/ironsteel_CO2_pie",  ".png"), height = 6, width = 9, units = "in")

# save underlying pie chart data as csv 
write.csv(ironsteel_CO2, paste0(results_dir, "/ironsteel_CO2_pie.csv"))

# bar chart just for ref and 1.5
steel_CO2_share <- ggplot(filter(ironsteel_CO2, year%in%plot_years, (scenario == "Ref" | scenario == "1p5")), 
       aes(x= year, y = share , fill = region)) +
  geom_col() + 
  scale_fill_manual(values = regions, name = "Region") +
  facet_wrap(~scenario, nrow = 1)+
  labs(title = expression(bold(paste(Regional~contributions~to~total~emissions))),
       x = "",
       y = expression("Share of total emissions")) + 
  plot_theme

ggsave(paste0(fig_dir, "/ironsteel_CO2_bar_share.png"), plot = steel_CO2_share, 
       height = 6, width = 9, units = "in")


# MATERIAL EFFICIENCY PIE CHART ----------------

mat_eff <- data.frame(cbind(c("Direct reuse", 'Optimized building design', 'High-strength steel', 'Lifetime extension', 'Post-use recycling', 'Lightweighting',
                              'Improved semi-manufacturing yields', 'Improved product manufacturing yields'),
                            c(.15,.09,.05,.17,.17,.18,.07,.12)))
colnames(mat_eff) <- c("Material efficiency measure", "value")
mat_eff <- mat_eff %>%
  mutate(label_num = as.numeric(value) * 100,
         label_num = paste0(label_num, "%"))
mat_eff_pie <- ggplot(filter(mat_eff),
       aes(x= "", y = value , fill = `Material efficiency measure`)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_manual(values = mat_eff_colors) +
  coord_polar("y", start = 0) +
  plot_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(label = label_num), position = position_stack(vjust = 0.5)) +
  blank_theme +
  labs(title = "Material efficiency measures")

ggsave(paste0(fig_dir, "/mat_eff_pie",  ".png"), plot = mat_eff_pie, height = 6, width = 9, units = "in")

# save as bar as well
mat_eff_bar <- ggplot(filter(mat_eff),
                      aes(x= "", y = value , fill = `Material efficiency measure`)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_manual(values = mat_eff_colors, name = "Material efficiency measures") +
  plot_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(label = label_num), position = position_stack(vjust = 0.5)) +
  blank_theme

# REFERENCE STEEL PRODUCTION STACKED LINE CHART ---------------
ironsteel_production$scenario <- factor(ironsteel_production$scenario,levels= scenarios, labels = scenario_labels)

ggplot(data=filter(ironsteel_production, year %in% plot_years, scenario=="Ref", region!="Global"),
       aes(x=year, y=value / 1000, fill=region)) +
  geom_area() +
  labs(title = "Iron and steel production, ref scenario", x="", y="Mt") +
  scale_fill_manual(values = regions, name = "Region")+
  plot_theme 
ggsave(paste0(fig_dir, "/iron-steel-prod-stacked.png"), height = 6, width = 6, units = "in")


ironsteel_production_data <- filter(ironsteel_production, 
                                    region %in% regions_aggregated, 
                                    year %in% plot_years, scenario != "NA")

write.csv(ironsteel_production_data, paste0(results_dir, "/ironsteel_production_data.csv"))

# STEEL PRODUCTION BY TECHNOLOGY -------------------
ironsteel_production_tech$scenario <- factor(ironsteel_production_tech$scenario, levels = scenarios, labels=scenario_labels)
ironsteel_production_tech$technology <- factor(ironsteel_production_tech$technology, 
                                               levels = unique(ironsteel_production_tech$technology),
                                               labels = c('BF-biomass',
                                                          'BF-BOF','BF-CCUS','BF-H2','DRI-EAF-Fossil','DRI-EAF-CCUS',
                                                          'EAF-scrap','DRI-EAF-H2'))

for (i in regions_aggregated) {
  ggplot(data=filter(ironsteel_production_tech, region == i,
                     year %in% plot_years, scenario != "NA"),
         aes(x=year, y=value, fill=technology)) +
    geom_col()+
    facet_wrap(~scenario, nrow = 1) +
    labs(title = paste(i, "iron and steel production by technology"), x="", y="Mt") +
    scale_fill_manual(values = tech_colors, name = "Technology")+
    plot_theme
  
  ggsave(paste0(fig_dir, "/ironsteel_production_tech_", i, ".png"), height = 7, width = 10, units = "in")
}

production_tech_1p5_regions <- ggplot(data=filter(ironsteel_production_tech,region %in% regions_aggregated, 
                   year %in% plot_years, scenario!="NA",
                   scenario=="1p5", region!="Global", region!="ROW"),
       aes(x=year, y=value, fill=technology)) +
  geom_col()+
  facet_wrap(~region, scale="free") +
  labs(title = paste("Iron and steel production by technology, 1p5 scenario"), x="", y="Mt") +
  scale_fill_manual(values = tech_colors, name = "Technology")+
  plot_theme

ggsave(paste0(fig_dir, "/ironsteel_production_tech_all_regions", ".png"), plot = production_tech_1p5_regions, height = 6, width = 10, units = "in")

# save data to csv 
ironsteel_production_tech_data <- filter(ironsteel_production_tech, region %in% regions_aggregated, 
                                         year %in% plot_years, scenario!="NA")
write.csv(ironsteel_production_tech_data , paste0(results_dir, "/ironsteel_production_tech_data.csv"))

# also save data with shares
ironsteel_production_tech_share_data <- ironsteel_production_tech_data %>%
  group_by(region, scenario, year) %>%
  mutate(total = sum(value),
         share = value / total * 100) %>%
  ungroup()

write.csv(ironsteel_production_tech_share_data, paste0(results_dir, "/ironsteel_production_tech_share_data.csv"))

# save the global production by tech figure for combining with other figures later
production_tech_global <- ggplot(data=filter(ironsteel_production_tech, region == "Global",
                   year %in% plot_years, scenario != "NA"),
       aes(x=year, y=value, fill=technology)) +
  geom_col()+
  facet_wrap(~scenario, nrow = 1) +
  labs(title = "Global steel production by technology", x="", y="Mt") +
  scale_fill_manual(values = tech_colors, name = "Technology")+
  plot_theme

# STEEL ENERGY USE BY FUEL --------------------

# separate coal with CCS
industry_energy_tech_fuel$scenario <- factor(industry_energy_tech_fuel$scenario,
                                             levels = scenarios, labels = scenario_labels)

x <- filter(industry_energy_tech_fuel, sector=="iron and steel")

# rename fuels to recognizable names, rename fuels with CCS 
x <- x %>% mutate(input = if_else(input == "delivered coal", "coal", input))
x <- x %>% mutate(input = if_else(input == "delivered biomass", "biomass", input))
x <- x %>% mutate(input = if_else(input == "elect_td_ind", "electricity", input))
x <- x %>% mutate(input = if_else(input == "H2 industrial", "hydrogen", input))
x <- x %>% mutate(input = if_else(input == "refined liquids industrial", "refined liquids", input))
x <- x %>% mutate(input = if_else(input == "wholesale gas", "gas", input))
x <- x %>% mutate(input2 = if_else((technology == "BLASTFUR CCS" & input != "biomass" & input != "electricity" & input != "hydrogen")|
                                     (technology=="EAF with DRI CCS" & input != "biomass" & input != "electricity" & input != "hydrogen"), 
                                   paste0(input, " with CCS"), input))
x <- x %>% mutate(input = input2)

for (i in regions_aggregated) {
  ggplot(data=filter(x, region == i, year %in% plot_years,sector == "iron and steel", 
                     scenario!="NA"),aes(x=year, y=value, fill=input)) +
    geom_col() +
    facet_wrap(~scenario, nrow = 1) +
    labs(title = paste(i, "iron and steel energy use by fuel"), x="", y="EJ") +
    scale_fill_manual(values = fuel_colors, name = "Fuel") +
    plot_theme

  ggsave(paste0(fig_dir, "/ironsteel_energy_fuel_", i, ".png"), height = 6, width = 9, units = "in")
}

energy_fuel_1p5_regions <- ggplot(data=filter(x, region %in% regions_aggregated, region!="Global", 
                   region!="ROW", year %in% plot_years,sector == "iron and steel",
                   scenario=="1p5"),aes(x=year, y=value, fill=input)) +
  geom_col() +
  facet_wrap(~region, ncol=3, scale="free") +
  labs(title = paste("Iron and steel energy use by fuel, 1p5 scenario"), x="", y="EJ") +
  scale_fill_manual(values = fuel_colors, name = "Fuel") +
  plot_theme
ggsave(paste0(fig_dir, "/ironsteel_energy_fuel_all_regions",  ".png"), plot = energy_fuel_1p5_regions, height = 6, width = 10, units = "in")


# ggplot(data=filter(x, region=="Global", year %in% plot_years,sector == "iron and steel",
#                    scenario!="NA"),aes(x=year, y=value, fill=input)) +
#   geom_col() +
#   facet_wrap(~scenario, ncol=3) +
#   labs(title = paste("Global iron and steel energy use by fuel"), x="", y="EJ") +
#   scale_fill_manual(values = fuel_colors, name = "Fuel") +
#   plot_theme
# ggsave(paste0(fig_dir, "/ironsteel_energy_fuel_global",  ".png"), height = 7, width = 11, units = "in")

# save data 
ironsteel_energy_tech_fuel_data <- filter(x, region %in% regions_aggregated, 
                                         year %in% plot_years, sector == "iron and steel",
                                         scenario != "NA") %>%
  select(-input2)

write.csv(ironsteel_energy_tech_fuel_data, paste0(results_dir, "/ironsteel_energy_tech_fuel_data.csv"))

# calculate the share from each fuel
ironsteel_energy_fuel_data <- ironsteel_energy_tech_fuel_data %>%
  select(-technology) %>%
  group_by(region, year, scenario, input)%>%
  summarize(value = sum(value)) %>%
  group_by(scenario, year, region) %>%
  mutate(total_energy = sum(value),
         share = value / total_energy * 100) %>%
  ungroup()

# save 
write.csv(ironsteel_energy_fuel_data, paste0(results_dir, "/ironsteel_fuel_use.csv"))

# just (non CCS) coal for the coal phaseout 
ironsteel_coal_use <- filter(ironsteel_energy_fuel_data, input == "coal")
write.csv(ironsteel_coal_use, paste0(results_dir, "/ironsteel_coal_use.csv"))


# save the global energy use figure for combining with other figures later
energy_fuel_global <- ggplot(data=filter(x, region == "Global", year %in% plot_years,sector == "iron and steel", 
                   scenario!="NA"),aes(x=year, y=value, fill=input)) +
  geom_col() +
  facet_wrap(~scenario, nrow = 1) +
  labs(title = paste("Global steel energy use by fuel"), x="", y="EJ") +
  scale_fill_manual(values = fuel_colors, name = "Fuel") +
  plot_theme

# STEEL INDIRECT/DIRECT EMISSIONS BREAKDOWN -------------------
final_ene_sect_fuel$scenario <- factor(final_ene_sect_fuel$scenario,levels = scenarios, labels = scenario_labels)
final_ene_sect_fuel <- filter(final_ene_sect_fuel, scenario!="NA")
CO2_emissions_sector_nobio <- filter(CO2_emissions_sector_nobio, scenario!="NA")

# get total fuel use in all sectors 
final_ene_sect_fuel_total <- final_ene_sect_fuel %>%
  group_by(region, year, scenario, input) %>%
  summarize(total = sum(value)) %>%
  ungroup()

# filter out portion of iron and steel electricity use
# first obtain iron and steel fuel use and share of total
ironsteel_fuel <- final_ene_sect_fuel %>%
  filter(sector == "iron and steel") %>%
  group_by(region, scenario, input, year) %>%
  ungroup()

# calculate share of fuel use from iron/steel 
ironsteel_fuel <- ironsteel_fuel %>%
  left_join(final_ene_sect_fuel_total, by = c("region","year","scenario","input")) %>%
  mutate(share = value/total) %>%
  ungroup()

# get CO2 emissions by aggregated sector
CO2_emissions_sector_nobio_assigned <- CO2_emissions_sector_nobio %>%
  left_join(CO2_sector_mapping, by = c("sector"="GCAM_sector"))%>%
  mutate(sector=sector_mapping) %>%
  group_by(region, scenario, sector, year) %>%
  summarize(total_emissions = sum(value)) %>%
  ungroup()

# get electricity negative emissions 
electricity_emissions <- filter(CO2_emissions_sector_nobio_assigned, 
                                sector=="electricity", total_emissions<0)

write.csv(electricity_emissions, paste0(results_dir,"/electricity_neg_emissions.csv"))

colnames(CO2_emissions_sector_nobio_assigned) <- c("region","scenario",
                                                   "input","year", "total_emissions")

# multiply iron steel portion of electricity use * electricity emissions 
ironsteel_fuel <- ironsteel_fuel %>% 
  left_join(CO2_emissions_sector_nobio_assigned, 
            by = c("region","scenario","year","input")) %>%
  mutate(indirect = share*total_emissions) %>%
  filter(total_emissions!="NA")

ironsteel_indirect_emissions <- ironsteel_fuel %>% 
  filter(indirect !="NaN") %>%
  select(c("year","region","indirect","scenario")) %>%
  group_by(year, region, scenario) %>%
  summarize(total_indirect = sum(indirect)) %>%
  ungroup() 

# get direct CO2 emissions and combine with indirect
CO2_dir_indir_emissions_ironsteel <- filter(CO2_emissions_sector_nobio, 
                                   sector=="iron and steel") %>%
  left_join(ironsteel_indirect_emissions, by=c("year", "scenario", "region"))

CO2_dir_indir_emissions_ironsteel <- gather(CO2_dir_indir_emissions_ironsteel,
                                             emission, value, 6:7) %>%
  mutate(`year `= as.character(year))

# plot 
for (i in regions_aggregated) {
  ggplot(data=filter(CO2_dir_indir_emissions_ironsteel, region == i, year %in% c(2020,2030,2050), scenario=="1p5" | scenario=="Ref"),
         aes(x=`year `, y=value / 1000 , fill=emission)) +
    geom_bar(stat = "identity", width =.7) +
    facet_wrap(~scenario, nrow = 1)+
    scale_fill_manual(values = c("value"="#E31A1C", "total_indirect"="#FB9A99"), labels = c("direct","indirect")) +
    plot_theme +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    labs(y=expression(Gt~CO[2]), x = "", title = bquote(bold(.(i)~iron~and~steel~direct~and~indirect~CO[2]~emissions))) +
    theme(legend.title = element_blank())
  ggsave(paste0(fig_dir, "/co2_direct_indirect_1p5_ref_", i, ".png"), height = 6, width = 9, units = "in")
}

write.csv(CO2_dir_indir_emissions_ironsteel, paste0(results_dir,"/ironsteel_direct_indirect_emissions.csv"))

# save plot of global direct and indirect emissions for combining with others later
direct_indirect_CO2_steel_global <- ggplot(data=filter(CO2_dir_indir_emissions_ironsteel, region == "Global", 
                                                       year %in% plot_years, scenario=="1p5" | scenario=="Ref"),
                                           aes(x=`year `, y=value / 1000 , fill=emission)) +
  geom_col() +
  facet_wrap(~scenario, nrow = 1)+
  scale_fill_manual(values = c("value"="#E31A1C", "total_indirect"="#FB9A99"), labels = c("direct","indirect")) +
  plot_theme +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(y=expression(Gt~CO[2]), x = "", title = bquote(bold(Direct~and~indirect~emissions))) +
  theme(legend.title = element_blank()) + 
  scale_x_discrete(breaks = c(2020, 2030, 2040, 2050))

# STEEL DIRECT EMISSIONS LINE PLOT ---------------
for (i in regions_aggregated) {
  ggplot(data=filter(CO2_emissions_sector_nobio, sector == "iron and steel", region == i, year %in% plot_years, scenario!="NA"),
         aes(x=year, y=value/1000, color=scenario)) +
    geom_line(size = 1.4) +
    labs(title = bquote(bold(.(i)~iron~and~steel~direct~CO[2]~emissions)), x="", y=bquote(Gt~CO[2])) +
    scale_color_manual(values = scenario_colors, name = "Scenario") +
    plot_theme
  
  ggsave(paste0(fig_dir, "/ironsteel_direct_co2_emissions_", i, ".png"), height = 6, width = 8, units = "in")
}

ggplot(data=filter(CO2_emissions_sector_nobio, sector == "iron and steel", region !="Global", region!="ROW", year %in% plot_years, scenario!="NA"),
       aes(x=year, y=value/1000, color=scenario)) +
  geom_line(size = 1.2) +
  facet_wrap(~region, scale="free", ncol=2)+
  labs(title = expression(bold(Iron~and~steel~direct~CO[2]~emissions)), x="", y=bquote(Gt~CO[2])) +
  scale_color_manual(values = scenario_colors, name = "Scenario") +
  plot_theme

ggsave(paste0(fig_dir, "/ironsteel_direct_co2_emissions_all_regions.png"), height = 6, width = 6, units = "in")

ironsteel_CO2_data <- filter(CO2_emissions_sector_nobio, sector == "iron and steel",
                             region %in% regions_aggregated, year %in% plot_years, scenario!="NA")

# save the global figure for grouped plotting later
steel_CO2_total_global <- ggplot(data=filter(CO2_emissions_sector_nobio, sector == "iron and steel", region == "Global", year %in% plot_years, scenario!="NA"),
                                 aes(x=year, y=value/1000, color=scenario)) +
  geom_line(size = 1.4) +
  labs(title = bquote(bold(Global~steel~CO[2]~emissions)), x="", y=bquote(Gt~CO[2])) +
  scale_color_manual(values = scenario_colors, name = "Scenario") +
  plot_theme

# STEEL EMISSIONS REDUCTIONS --------------------
ironsteel_CO2_data_1p5 <- filter(ironsteel_CO2_data, scenario == "1p5")

ironsteel_CO2_data_1p5_2020 <- filter(ironsteel_CO2_data_1p5, year == 2020) %>%
  select(-year)

ironsteel_CO2_data_1p5 <- ironsteel_CO2_data_1p5 %>%
  left_join(ironsteel_CO2_data_1p5_2020 %>% rename(value_2020 = value), 
            by = c("region", "scenario","sector","Units")) %>%
  mutate(percent_reduction = (value - value_2020)/value_2020*100) %>%
  mutate(absolute_reduction = value - value_2020) %>%
  filter(year > 2020)

write.csv(ironsteel_CO2_data_1p5, paste0(results_dir, "/ironsteel_co2_reductions.csv"))

# plot
ggplot(data=ironsteel_CO2_data_1p5, aes(x=year, y = percent_reduction, color=region))+
  geom_line(size = 1) +
  scale_color_manual(values = regions, name = "Region") + 
  labs(title = expression(bold(Steel~sector~CO[2]~emissions~percent~reduction)), x="", y="% reduction") +
  plot_theme 
ggsave(paste0(results_dir, "/ironsteel_CO2_percent_reduction.png"), height = 6, width = 9, units = "in")


# STEEL EMISSIONS INTENSITY ----------------
emissions_intensity <-  ironsteel_CO2_data %>%
  left_join(ironsteel_production_data,
            by = c("scenario","year","region", "sector")) %>%
  mutate(em_intensity = (value.x/value.y)) %>%
  filter(scenario=="1p5", year > 2015)

emissions_intensity_2020 <- filter(emissions_intensity, year == 2020) %>%
  select(c(em_intensity, region))

colnames(emissions_intensity_2020) <- c("em_intensity_2020", "region")

emissions_intensity <- emissions_intensity %>%
  left_join(emissions_intensity_2020, by = "region") %>%
  mutate(abs_reduction = em_intensity_2020 - em_intensity,
         perc_reduction =  (em_intensity - em_intensity_2020)/em_intensity_2020 * 100,
         Units = "tCO2/t steel")

emissions_intensity <- emissions_intensity %>%
  select(region, year, scenario, em_intensity,
         em_intensity_2020, abs_reduction, perc_reduction, Units)

write.csv(emissions_intensity, paste0(results_dir, "/ironsteel_emissions_intensity.csv"))

ggplot(data=emissions_intensity, aes(x=year, y = em_intensity, color=region))+
  geom_line(size = 1) +
  scale_color_manual(values = regions, name = "Region") + 
  labs(title = expression(bold(Iron~and~steel~CO[2]~emissions~intensity)), x="", y=expression(tCO[2]/tsteel)) +
  plot_theme 
ggsave(paste0(results_dir, "/ironsteel_emissions_intensity.png"), height = 6, width = 9, units = "in")

# CUMULATIVE DIRECT CO2 EMISSIONS FROM STEEL ---------------
cumulative_ironsteel_co2_2020to2100 <- CO2_emissions_sector_nobio %>%
  filter(sector=="iron and steel" & scenario!="NA") %>%
  group_by(scenario, region) %>%
  complete(year = seq(2010, 2100)) %>%
  mutate(value = na.approx(value),
         value = value / 1000,
         Units = "GtCO2") %>%
  filter(year >= 2020) %>%
  mutate(cum_value = cumsum(value)) %>%
  ungroup() 

write.csv(cumulative_ironsteel_co2_2020to2100, paste0(results_dir,"/ironsteel_cumulative_CO2_emissions.csv"))

# make bar chart of cumulative emissions in max year
steel_CO2_total_cum_global <- ggplot(cumulative_ironsteel_co2_2020to2100 %>% 
                                       filter(year == max(plot_years) & region == "Global"),
                                     aes(x = scenario, y = cum_value, fill = scenario)) + 
  geom_col() + 
  scale_fill_manual(values = scenario_colors, name = "Scenario") + 
  labs(title = paste0("Cumulative emissions, 2020-",max(plot_years)), 
       x="", y=bquote(Gt~CO[2])) +
  plot_theme + 
  scale_x_discrete(breaks = NULL)

# HYDROGEN PRODUCTION BY TECHNOLOGY AND COLOR ----------------
# first by color - hydrogen green, blue, grey calculation
hydrogen_production_tech$scenario <- factor(hydrogen_production_tech$scenario, levels = scenarios, labels=scenario_labels)
hydrogen_production <- filter(hydrogen_production_tech, scenario!="NA")
hydrogen_production_color <- hydrogen_production %>% left_join(hydrogen_mapping, by=c("technology"="technology"))

for (i in regions_aggregated) {
  ggplot(data=filter(hydrogen_production_color, region == i, year %in% plot_years, scenario!="NA"),
         aes(x=year, y=value, fill=color)) +
    labs(fill="Type of hydrogen")+
    geom_col()+
    facet_wrap(~scenario, nrow = 1) +
    labs(title = paste(i, "hydrogen by type of production method"), x="", y="EJ") +
    scale_fill_manual(values = hydrogen_pal)+
    plot_theme 
  ggsave(paste0(fig_dir, "/hydrogen_production_color_", i, ".png"), height = 6, width = 10, units = "in")
}

# save data with shares green, blue, grey hydrogen 
hydrogen_production_color_2 <- hydrogen_production_color %>%
  group_by(year, region, scenario) %>%
  summarize(total = sum(value)) %>%
  ungroup()

hydrogen_production_color_data <- hydrogen_production_color %>%
  group_by(year, region, scenario, color) %>%
  summarize(value=sum(value)) %>%
  left_join(hydrogen_production_color_2, by=c("year", "region","scenario")) %>%
  mutate(share = value/total*100) %>%
  filter(year > 2015)

write.csv(hydrogen_production_color_data, paste0(results_dir, "/hydrogen_production_color.csv"))

# now plot by technology
for (i in regions_aggregated) {
  ggplot(data=filter(hydrogen_production, region == i, year %in% plot_years, scenario!="NA"),
         aes(x=year, y=value, fill=technology)) +
    labs(fill="Hydrogen technology")+
    geom_col()+
    facet_wrap(~scenario, nrow = 1) +
    labs(title = paste(i, "hydrogen by production method"), x="", y="EJ") +
    scale_fill_manual(values = hydrogen_tech_pal)+
    plot_theme 
  ggsave(paste0(fig_dir, "/hydrogen_production_tech_", i, ".png"), height = 6, width = 11, units = "in")
}

ggplot(data=filter(hydrogen_production, region !="Global", year %in% plot_years, scenario == "1p5"),
       aes(x=year, y=value, fill=technology)) +
  labs(fill="Hydrogen technology")+
  geom_col() +
  facet_wrap(~region, scale="free", ncol=3)+
  labs(title = "Hydrogen production by technology, 1p5 scenario", x="", y="EJ") +
  scale_fill_manual(values = hydrogen_tech_pal)+
  plot_theme

ggsave(paste0(fig_dir, "/hydrogen_production_tech_all_regions.png"), height = 6, width = 11, units = "in")

# save data for production by technology
hydrogen_production_data <- hydrogen_production %>%
  group_by(year, region, scenario) %>%
  mutate(share = value*100 / sum(value)) %>%
  filter(year > 2015) %>%
  ungroup()

write.csv(hydrogen_production_data, paste0(results_dir, "/hydrogen_production_tech.csv"))

# ELECTRICITY GENERATION ----------

colnames(elec_mapping) <- c("technology","type", "generation technology")

electricity <- electricity %>%
  left_join(elec_mapping)

# look at electricity generation by tech
elec_gen_by_tech <- electricity %>%
  group_by(region, scenario, year, `generation technology`, Units) %>%
  dplyr::summarize(value = sum(value)) %>%
  group_by(region, scenario, year) %>%
  mutate(total = sum(value),
         share = value / total * 100) %>%
  ungroup()

# look at electricity generation by type
elec_gen_by_type <- electricity %>%
  group_by(region, scenario, year, type, Units) %>%
  dplyr::summarize(value = sum(value)) %>%
  group_by(region, scenario, year) %>%
  mutate(total = sum(value),
         share = value / total * 100) %>%
  ungroup()

elec_gen_by_tech$scenario <- factor(elec_gen_by_tech$scenario, levels = scenarios, labels=scenario_labels)
elec_gen_by_type$scenario <- factor(elec_gen_by_type$scenario, levels = scenarios, labels=scenario_labels)

# plot electricity generation by tech
for (i in regions_aggregated) {
  ggplot(data=filter(elec_gen_by_tech, region == i, year %in% plot_years, scenario!="NA"),
         aes(x=year, y=value, fill=`generation technology`)) +
    labs(fill="technology")+
    geom_col()+
    facet_wrap(~scenario, nrow = 1) +
    labs(title = paste(i, "electricity by production technology"), x="", y="EJ") +
    scale_fill_manual(values = electricity_tech_pal, name = "Technology")+
    plot_theme 
  ggsave(paste0(fig_dir, "/electricity_production_tech_", i, ".png"), height = 6, width = 11, units = "in")
}

ggplot(data=filter(elec_gen_by_tech, region !="Global", year %in% plot_years, scenario == "1p5"),
       aes(x=year, y=value, fill=`generation technology`)) +
  labs(fill="technology")+
  geom_col() +
  facet_wrap(~region, scale="free", ncol=3)+
  labs(title = "Electricity production by technology, 1p5 scenario", x="", y="EJ") +
  scale_fill_manual(values = electricity_tech_pal, name = "Technology")+
  plot_theme

ggsave(paste0(fig_dir, "/electricity_production_tech_all_regions.png"), height = 6, width = 11, units = "in")


write.csv(elec_gen_by_tech, paste0(results_dir,"/elec_gen_by_tech.csv"))
write.csv(elec_gen_by_type, paste0(results_dir,"/elec_gen_by_type.csv"))

# GREENHOUSE GAS EMISSIONS BY SECTOR AND GAS -------------------
# set scenario labels for relevant queries
nonCO2_em_sector$scenario <- factor(nonCO2_em_sector$scenario,
                                    levels = scenarios, labels=scenario_labels)
nonCO2_emissions_resource_prod$scenario <- factor(nonCO2_emissions_resource_prod$scenario,
                                    levels = scenarios, labels=scenario_labels)
LU_CO2_emissions$scenario <- factor(LU_CO2_emissions$scenario,
                                                  levels = scenarios, labels=scenario_labels)


# get all GHG emissions by assigned sector
all_ghg_em_sector_assigned <- nonCO2_em_sector %>%
  bind_rows(nonCO2_emissions_resource_prod %>% mutate(sector = resource)) %>%
  # join GWP values and filter to just gases that have a GWP (i.e., GHGs)
  left_join(GWP_AR5 %>% rename(GHG = ghg)) %>%
  mutate(Mt_CO2e = value * GWP) %>%
  filter(!is.na(Mt_CO2e)) %>%
  # exclude CO2 since we will use the no bio query output for CO2
  filter(GHG != "CO2") %>%
  # use no bio CO2 query
  bind_rows(CO2_emissions_sector_nobio %>% mutate(Mt_CO2e = value, GHG = "CO2")) %>%
  # add sector mapping
  left_join(all_ghg_sector_mapping, by = c("sector" = "GCAM_sector")) %>%
  # add LUC emissions
  bind_rows(LU_CO2_emissions %>% filter(year %in% unique(CO2_emissions$year)) %>%
              mutate(sector_mapping = "LULUCF",
                     GHG = "CO2",
                     Mt_CO2e = value)) %>%
  group_by(region, scenario, year, GHG, sector_mapping) %>%
  summarize(Mt_CO2e = sum(Mt_CO2e)) %>%
  rename(sector = sector_mapping) %>%
  ungroup()

# group the GHGs a bit more
all_ghg_em_sector_assigned_ghg_grouped <- all_ghg_em_sector_assigned %>%
  mutate(GHG = case_when(grepl("CH4", GHG) ~ "CH4",
                               grepl("HFC", GHG) ~ "HFC",
                               grepl("N2O", GHG) ~ "N2O",
                               TRUE ~ GHG)) %>%
  group_by(region, scenario, year, GHG, sector) %>%
  summarize(Mt_CO2e = sum(Mt_CO2e)) %>%
  group_by(region, scenario, year, sector) %>%
  mutate(share_of_sector = Mt_CO2e / sum(Mt_CO2e) * 100) %>%
  ungroup()

# plot
for (i in regions_aggregated) {
  ggplot(data=filter(all_ghg_em_sector_assigned_ghg_grouped, region == i, year %in% plot_years, scenario == "1p5"),
         aes(x=year, y=Mt_CO2e, fill= GHG)) +
    labs(fill="GHG")+
    geom_col()+
    facet_wrap(~sector, ncol=4, scale = "free") +
    labs(title = paste(i, "GHG emissions by sector and gas, 1p5 scenario"), x="", y = expression(Mt~CO[2]~e)) +
#    scale_fill_manual(values = hydrogen_tech_pal) +
    plot_theme
  ggsave(paste0(fig_dir, "/ghg_emis_sector_gas_1p5_", i, ".png"), height = 6, width = 12, units = "in")
}

# plot just non-CO2
for (i in regions_aggregated) {
  ggplot(data=filter(all_ghg_em_sector_assigned_ghg_grouped, GHG != "CO2", region == i, year %in% plot_years, scenario == "1p5"),
         aes(x=year, y=Mt_CO2e, fill= GHG)) +
    labs(fill="GHG")+
    geom_col()+
    facet_wrap(~sector, ncol=4, scale = "free") +
    labs(title = bquote(bold(.(i)~ "GHG (non-" ~ CO[2] ~ ") emissions by sector and gas, 1p5 scenario")), x="", y = expression(Mt~CO[2]~e)) +
    #    scale_fill_manual(values = hydrogen_tech_pal) +
    plot_theme
  ggsave(paste0(fig_dir, "/ghg_no_co2_emis_sector_gas_1p5_", i, ".png"), height = 6, width = 12, units = "in")
}

# get total GHG emissions
total_ghg_em <- all_ghg_em_sector_assigned_ghg_grouped %>%
  group_by(scenario, year, region) %>%
  summarize(Mt_CO2e = sum(Mt_CO2e)) %>%
  ungroup()

ggplot(data=filter(total_ghg_em, year %in% plot_years,scenario!="NA"),aes(x=year, y=Mt_CO2e / 1000, color=scenario))+
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~region, scales = "free") +
  labs(title = expression(bold(GHG~emissions)), x="", y=bquote(Gt~CO[2]~e)) +
  scale_color_manual(values = scenario_colors, name = "Scenario") +
  plot_theme 
ggsave(paste0(fig_dir, "/ghg_emissions_all_regions.png"), height = 6, width = 9, units = "in")

# get total GHG emissions by sector (excluding CO2)
non_CO2_em_sector_assigned <- all_ghg_em_sector_assigned_ghg_grouped %>%
  filter(GHG != "CO2") %>%
  group_by(scenario, year, region, sector) %>%
  summarize(Mt_CO2e = sum(Mt_CO2e)) %>%
  ungroup()

# plot
for (i in regions_aggregated) {
  ggplot(data=filter(non_CO2_em_sector_assigned, region == i, scenario != "NA", year %in% plot_years),
         aes(x=year, y=Mt_CO2e / 1000 , fill= sector)) +
    geom_bar(stat = "identity") +
    facet_wrap(~scenario, nrow = 1)+
    scale_fill_manual(values = sector_colors, name = "Sector") + 
    plot_theme +
    labs(y=expression(Gt~CO[2]~e), title = bquote(bold(.(i)~GHG~(non-~CO[2])~emissions~by~sector)), x = "")
  ggsave(paste0(fig_dir, "/ghg_no_co2_emis_sector_", i, ".png"), height = 6, width = 9, units = "in")
}

# total GHG emissions by sector including CO2
all_ghg_em_sector_assigned <- all_ghg_em_sector_assigned_ghg_grouped %>%
  group_by(scenario, year, region, sector) %>%
  summarize(Mt_CO2e = sum(Mt_CO2e)) %>%
  ungroup()


# plot
for (i in regions_aggregated) {
  ggplot(data=filter(all_ghg_em_sector_assigned, region == i, scenario != "NA", year %in% plot_years),
         aes(x=year, y=Mt_CO2e / 1000 , fill= sector)) +
    geom_bar(stat = "identity") +
    facet_wrap(~scenario, nrow = 1)+
    scale_fill_manual(values = sector_colors_CO2, name = "Sector") + 
    plot_theme +
    labs(y=expression(Gt~CO[2]~e), title = bquote(bold(.(i)~total~GHG~emissions~by~sector)), x = "")
  ggsave(paste0(fig_dir, "/ghg_emis_sector_", i, ".png"), height = 6, width = 9, units = "in")
}

# total CO2 emissions by sector
all_co2_em_sector_assigned <- all_ghg_em_sector_assigned_ghg_grouped %>%
  filter(GHG == "CO2") %>%
  group_by(scenario, year, region, sector) %>%
  summarize(Mt_CO2e = sum(Mt_CO2e)) %>%
  ungroup()


# plot
for (i in regions_aggregated) {
  ggplot(data=filter(all_co2_em_sector_assigned, region == i, scenario != "NA", year %in% plot_years),
         aes(x=year, y=Mt_CO2e / 1000 , fill= sector)) +
    geom_bar(stat = "identity") +
    facet_wrap(~scenario, nrow = 1)+
    scale_fill_manual(values = sector_colors_CO2, name = "Sector") + 
    plot_theme +
    labs(y=expression(Gt~CO[2]), title = bquote(bold(.(i)~total~CO[2]~emissions~by~sector)), x = "")
  ggsave(paste0(fig_dir, "/co2_emis_sector_", i, ".png"), height = 6, width = 9, units = "in")
}


# save data
write.csv(all_ghg_em_sector_assigned_ghg_grouped, paste0(results_dir,"/ghg_emis_by_sector_gas.csv"))
write.csv(total_ghg_em, paste0(results_dir,"/ghg_emis_total.csv"))
write.csv(non_CO2_em_sector_assigned, paste0(results_dir,"/ghg_emis_no_CO2_by_sector.csv"))
write.csv(all_ghg_em_sector_assigned, paste0(results_dir,"/ghg_emis_by_sector.csv"))

# CALCULATIONS FOR COMPARISONS TO OTHER STUDIES ------------
# STEEL ELECTRICITY USE - RAW
ironsteel_elec_use_global_1p5 <- ironsteel_energy_fuel_data %>%
  filter(input == "electricity" & scenario == "1p5" & region == "Global")

# get surge in electricity use
ironsteel_elec_use_global_1p5 <- ironsteel_elec_use_global_1p5 %>%
  left_join(ironsteel_elec_use_global_1p5 %>%
               filter(year == 2020) %>%
              dplyr::select(region, scenario, value) %>%
              rename(value_2020 = value)) %>%
  mutate(perc_increase_from_2020 = 100 * (value - value_2020) / value_2020)

# STEEL ELECTRICITY USE WITH H2 ELECTRICITY
# include electricity used to generate hydrogen for iron and steel production
# this electricity is calculated as:
# (total electricity used in hydrogen production via electrolysis) * (H2 fuel use in iron and steel) / (total H2 fuel use across all sectors)
# first get iron and steel hydrogen use
ironsteel_h2_use_global_1p5 <- ironsteel_energy_fuel_data %>%
  filter(input == "hydrogen" & scenario == "1p5" & region == "Global") %>%
  dplyr::select(region, year, scenario, input, value)

# get total hydrogen use across all sectors
all_sectors_h2_use_global_1p5 <- final_ene_sect_fuel_total %>%
  filter(input == "hydrogen" & scenario == "1p5" & region == "Global")

# get fraction of hydrogen used for steel
ironsteel_h2_use_global_1p5 <- ironsteel_h2_use_global_1p5 %>%
  left_join(all_sectors_h2_use_global_1p5) %>%
  mutate(frac = value / total)

# get electricity used for hydrogen production
hydrogen_inputs_tech$scenario <- factor(hydrogen_inputs_tech$scenario, levels = scenarios, labels=scenario_labels)

elec_use_for_h2_global_1p5 <- hydrogen_inputs_tech %>%
  filter(scenario == "1p5" & region == "Global" & 
           technology == "electrolysis" & 
           fuel %in% c("elect_td_ind", "global solar resource", "onshore wind resource")) %>%
  group_by(scenario, region, year, Units) %>%
  summarize(total_H2_elec_use = sum(value))

# add electricity use for hydrogen production to steel electricity use
ironsteel_elec_use_global_1p5_w_h2_elec <- ironsteel_elec_use_global_1p5 %>%
  left_join(ironsteel_h2_use_global_1p5 %>% 
              dplyr::select(region, year, scenario, frac) %>% 
              rename(steel_H2_frac = frac)) %>%
  left_join(elec_use_for_h2_global_1p5) %>%
  mutate(steel_H2_elec = replace_na(steel_H2_frac * total_H2_elec_use, 0),
         steel_elec_use_w_H2_elec = value + steel_H2_elec)

ironsteel_elec_use_global_1p5_w_h2_elec <- ironsteel_elec_use_global_1p5_w_h2_elec %>%
  left_join(ironsteel_elec_use_global_1p5_w_h2_elec %>%
              filter(year == 2020) %>%
              dplyr::select(region, scenario, steel_elec_use_w_H2_elec) %>%
              rename(steel_elec_use_w_H2_elec_2020 = steel_elec_use_w_H2_elec)) %>%
  mutate(perc_change_from_2020 = 100 * (steel_elec_use_w_H2_elec - steel_elec_use_w_H2_elec_2020) / steel_elec_use_w_H2_elec_2020)

# GLOBAL PRODUCTION OF STEEL
# get increase relative to 2020 in production
ironsteel_production_data_global_1p5 <- ironsteel_production_data %>%
  filter(region == "Global" & scenario == "1p5")

ironsteel_production_data_global_1p5 <- ironsteel_production_data_global_1p5 %>%
  left_join(ironsteel_production_data_global_1p5 %>%
              filter(year == 2020) %>%
              rename(value_2020 = value) %>%
              dplyr::select(-year)) %>%
  mutate(perc_change_from_2020 = 100 * (value - value_2020) / value_2020)

# COAL USE FOR STEEL
# get decrease relative to 2020
ironsteel_coal_use_no_CCS_global_1p5 <- ironsteel_coal_use %>%
  filter(region == "Global" & scenario == "1p5")

ironsteel_coal_use_no_CCS_global_1p5 <- ironsteel_coal_use_no_CCS_global_1p5 %>%
  left_join(ironsteel_coal_use_no_CCS_global_1p5 %>%
              filter(year == 2020) %>%
              rename(value_2020 = value) %>%
              dplyr::select(region, scenario, value_2020)) %>%
  mutate(perc_change_from_2020 = 100 * (value - value_2020) / value_2020)

# do the same for coal use, including coal both with and without CCS
ironsteel_coal_use_inc_with_CCS_global_1p5 <- ironsteel_energy_fuel_data %>%
  filter(region == "Global" & scenario == "1p5" & input %in% c("coal", "coal with CCS")) %>%
  group_by(region, year, scenario) %>%
  summarize(value = sum(value)) %>%
  ungroup()

ironsteel_coal_use_inc_with_CCS_global_1p5 <- ironsteel_coal_use_inc_with_CCS_global_1p5 %>%
  left_join(ironsteel_coal_use_inc_with_CCS_global_1p5 %>%
              filter(year == 2020) %>%
              rename(value_2020 = value) %>%
              dplyr::select(region, scenario, value_2020)) %>%
  mutate(perc_change_from_2020 = 100 * (value - value_2020) / value_2020)

# HYDROGEN USE IN STEEL, converted to Mt
# hydrogen conversion factor to Mt from EJ
conv_H2_EJ_to_Mt = 7

ironsteel_h2_use_global_1p5_Mt <- ironsteel_h2_use_global_1p5 %>%
  mutate(H2_Mt = value * conv_H2_EJ_to_Mt)

# SCALE-UP ESTIMATE: Considering what our results would like like if total production were 2500 Mt
# and 70% of hydrogen were produced via electrolysis, specifically for in 2050.
# This calculation assumes that the extra production has the same fractional breakdown across technologies
# and fuel inputs as the original production estimate in 2050.

total_prod_scale_up <- 2500 
frac_H2_electrolysis_scale_up <- 0.7

# first calculate current fraction of hydrogen production via electrolysis
hydrogen_production_global_1p5 <- hydrogen_production %>%
  filter(region == "Global" & scenario == "1p5") %>%
  group_by(region, scenario, year, technology, Units) %>%
  summarize(value = sum(value)) %>%
  group_by(scenario, year, region) %>%
  mutate(tech_frac = value / sum(value)) %>%
  ungroup()

hydrogen_production_global_1p5_electrolysis_frac_2050 <- (hydrogen_production_global_1p5 %>%
  filter(technology == "electrolysis" & year == "2050"))$tech_frac

# get desired fractional increase in electrolysis
frac_electrolysis_inc <- frac_H2_electrolysis_scale_up / hydrogen_production_global_1p5_electrolysis_frac_2050

# get fractional increase in production
frac_prod_inc <- total_prod_scale_up / (ironsteel_production_data_global_1p5 %>% filter(year == 2050))$value

# obtain the new production relative to 2020 values
ironsteel_production_data_global_1p5 <- ironsteel_production_data_global_1p5 %>% 
  mutate(scale_up_prod = ifelse(year == 2050, total_prod_scale_up, NA),
         scale_up_prod_perc_change_from_2020 = 100 * (scale_up_prod - value_2020) / value_2020)

# get corresponding result for reduction in coal use, calculating new coal use as old coal use times the increase in production
ironsteel_coal_use_no_CCS_global_1p5 <- ironsteel_coal_use_no_CCS_global_1p5 %>%
  mutate(scale_up_coal_use = ifelse(year == 2050, value * frac_prod_inc, NA),
         scale_up_coal_use_perc_change_from_2020 = 100 * (scale_up_coal_use - value_2020) / value_2020)

ironsteel_coal_use_inc_with_CCS_global_1p5 <- ironsteel_coal_use_inc_with_CCS_global_1p5 %>%
  mutate(scale_up_coal_use = ifelse(year == 2050, value * frac_prod_inc, NA),
         scale_up_coal_use_perc_change_from_2020 = 100 * (scale_up_coal_use - value_2020) / value_2020)

# get corresponding result for increase in hydrogen use
ironsteel_h2_use_global_1p5_Mt <- ironsteel_h2_use_global_1p5_Mt %>%
  mutate(scale_up_H2_use_Mt = ifelse(year == 2050, H2_Mt * frac_prod_inc, NA),
         scale_up_frac = ifelse(year == 2050, (value * frac_prod_inc) / (total + value * frac_prod_inc - value), NA))

# obtain new electricity use values - need to scale up both by the increase in electricity used due to more
# production and also by the increase in hydrogen production via electrolysis
ironsteel_elec_use_global_1p5_w_h2_elec <- ironsteel_elec_use_global_1p5_w_h2_elec %>%
  left_join(ironsteel_h2_use_global_1p5_Mt %>% dplyr::select(region, scenario, year, scale_up_frac) %>% 
              rename(scale_up_steel_H2_frac = scale_up_frac)) %>%
  mutate(scale_up_total_H2_elec_use = ifelse(year == 2050, total_H2_elec_use * frac_electrolysis_inc, NA),
         scale_up_steel_elec_use = ifelse(year == 2050, value * frac_prod_inc, NA),
         scale_up_steel_elec_use_w_H2_elec = scale_up_steel_elec_use + scale_up_total_H2_elec_use * scale_up_steel_H2_frac,
         scale_up_steel_elec_use_w_H2_elec_perc_change_from_2020 = 100 * (scale_up_steel_elec_use_w_H2_elec - steel_elec_use_w_H2_elec_2020) / steel_elec_use_w_H2_elec_2020)

# save results
write.csv(ironsteel_elec_use_global_1p5_w_h2_elec, paste0(results_dir,"/ironsteel_elec_use_global_1p5_w_h2_elec_w_scale_up.csv"))
write.csv(ironsteel_coal_use_no_CCS_global_1p5, paste0(results_dir,"/ironsteel_coal_use_no_CCS_global_1p5_w_scale_up.csv"))
write.csv(ironsteel_coal_use_inc_with_CCS_global_1p5, paste0(results_dir,"/ironsteel_coal_use_inc_with_CCS_global_1p5_w_scale_up.csv"))
write.csv(ironsteel_production_data_global_1p5, paste0(results_dir, "/ironsteel_production_data_global_1p5_w_scale_up.csv"))
write.csv(ironsteel_h2_use_global_1p5_Mt, paste0(results_dir, "/ironsteel_h2_use_global_1p5_Mt_w_scale_up.csv"))


# IPCC COMPARISON -----------------------
## Comparison to IPCC AR6 scenario categories -----------
# calculate the values for the comparison to the IPCC scenarios, if we are looking at results to 2100 only
if (2100 %in% plot_years) {
  # prep temp data
  global_mean_temp <- unique(global_mean_temp)
  global_mean_temp$scenario <- factor(global_mean_temp$scenario, 
                                      levels = scenarios, 
                                      labels = scenario_labels)
  
  # perform for both 1p5 and 1p5 delay
  ipcc_comparison_final <- c()
  
  for (sce in c("1p5", "1p5 delay")) {
    # select just 1.5C global results and interpolate them
    total_ghg_em_1p5_global <- total_ghg_em %>% filter(scenario == sce & region == "Global") %>%
      complete(year = plot_years) %>%
      arrange(year) %>%
      mutate(Mt_CO2e_interp = approx(year, Mt_CO2e, xout = year)$y)
    
    net_co2_data_1p5_global <- net_co2_data %>% 
      filter(scenario == sce & region == "Global") %>%
      complete(year = plot_years) %>%
      arrange(year) %>%
      mutate(Mt_CO2e_interp = approx(year, value, xout = year)$y)
    
    # interpolate temp data
    temp_1p5 <- global_mean_temp %>%
      filter(scenario == sce & region == "Global") %>%
      complete(year = plot_years) %>%
      arrange(year) %>%
      mutate(deg_C_interp = approx(year, value, xout = year)$y)
    
    ipcc_comparison_labels <- c()
    ipcc_comparison_results <- c()
    
    # peak CO2 + GHG emissions year
    peak_CO2_year <- (net_co2_data_1p5_global %>% filter(Mt_CO2e_interp == max(Mt_CO2e_interp)))$year[1]
    peak_GHG_year <- (total_ghg_em_1p5_global %>% filter(Mt_CO2e_interp == max(Mt_CO2e_interp)))$year[1]
    ipcc_comparison_labels <- c(ipcc_comparison_labels, "peak CO2 year", "peak GHG year")
    ipcc_comparison_results <- c(ipcc_comparison_results, peak_CO2_year, peak_GHG_year)
    
    # net zero CO2 + GHG emissions year
    net_zero_CO2_year <- (net_co2_data_1p5_global %>% filter(Mt_CO2e_interp <= 0))$year[1]
    net_zero_GHG_year <- (total_ghg_em_1p5_global %>% filter(Mt_CO2e_interp <= 0))$year[1]
    ipcc_comparison_labels <- c(ipcc_comparison_labels, "net zero CO2 year", "net zero GHG year")
    ipcc_comparison_results <- c(ipcc_comparison_results, net_zero_CO2_year, net_zero_GHG_year)
    
    # cumulative CO2 emissions, 2020 till net zero CO2 year
    # calculate cumulative emissions
    net_co2_data_1p5_global_cum_from_2020 <- net_co2_data_1p5_global %>%
      filter(year >= 2020) %>%
      mutate(Gt_CO2e_interp_cum = cumsum(Mt_CO2e_interp) / 1000,
             # column with just negative emissions
             Mt_CO2e_interp_neg = ifelse(Mt_CO2e_interp <= 0 , Mt_CO2e_interp, 0),
             Gt_CO2e_interp_neg_cum = cumsum(Mt_CO2e_interp_neg) / 1000)
    
    cum_emissions_2020_net_zero_CO2 <- (net_co2_data_1p5_global_cum_from_2020 %>% 
                                          filter(year == net_zero_CO2_year))$Gt_CO2e_interp_cum
    ipcc_comparison_labels <- c(ipcc_comparison_labels, "cumulative CO2 emissions from 2020 to net zero year (Gt CO2e)")
    ipcc_comparison_results <- c(ipcc_comparison_results, cum_emissions_2020_net_zero_CO2)
    
    # cumulative CO2 emissions, 2020 to 2050
    cum_emissions_2020_2050 <- (net_co2_data_1p5_global_cum_from_2020 %>% 
                                  filter(year == 2050))$Gt_CO2e_interp_cum
    ipcc_comparison_labels <- c(ipcc_comparison_labels, "cumulative CO2 emissions from 2020 to 2050 (Gt CO2e)")
    ipcc_comparison_results <- c(ipcc_comparison_results, cum_emissions_2020_2050)
    
    # cumulative CO2 emissions, 2020 till 2100
    cum_emissions_2020_2100 <- (net_co2_data_1p5_global_cum_from_2020 %>% 
                                  filter(year == 2100))$Gt_CO2e_interp_cum
    ipcc_comparison_labels <- c(ipcc_comparison_labels, "cumulative CO2 emissions from 2020 to 2100 (Gt CO2e)")
    ipcc_comparison_results <- c(ipcc_comparison_results, cum_emissions_2020_2100)
    
    # cumulative net negative CO2 emissions from net zero till 2100
    cum_net_neg_emissions <- (net_co2_data_1p5_global_cum_from_2020 %>% 
                                filter(year == 2100))$Gt_CO2e_interp_neg_cum
    ipcc_comparison_labels <- c(ipcc_comparison_labels, "cumulative net negative CO2 emissions (Gt CO2e)")
    ipcc_comparison_results <- c(ipcc_comparison_results, cum_net_neg_emissions)
    
    # GHG emissions in 2030, 2040, 2050
    ghg_2030 <- (total_ghg_em_1p5_global %>% filter(year == 2030))$Mt_CO2e / 1000
    ghg_2040 <- (total_ghg_em_1p5_global %>% filter(year == 2040))$Mt_CO2e / 1000
    ghg_2050 <- (total_ghg_em_1p5_global %>% filter(year == 2050))$Mt_CO2e / 1000
    ipcc_comparison_labels <- c(ipcc_comparison_labels, "2030 GHG emissions (Gt CO2e)", 
                                "2040 GHG emissions (Gt CO2e)", "2050 GHG emissions (Gt CO2e)")
    ipcc_comparison_results <- c(ipcc_comparison_results, ghg_2030, ghg_2040, ghg_2050)
    
    # GHG emissions reductions from 2019 to 2030, 2040, 2050
    ghg_2020 <- (total_ghg_em_1p5_global %>% filter(year == 2020))$Mt_CO2e
    total_ghg_em_1p5_global <- total_ghg_em_1p5_global %>%
      mutate(perc_change_from_2020 = (Mt_CO2e_interp - ghg_2020) / ghg_2020 * 100)
    
    ghg_perc_change_2030_2020 <- (total_ghg_em_1p5_global %>% filter(year == 2030))$perc_change_from_2020
    ghg_perc_change_2040_2020 <- (total_ghg_em_1p5_global %>% filter(year == 2040))$perc_change_from_2020
    ghg_perc_change_2050_2020 <- (total_ghg_em_1p5_global %>% filter(year == 2050))$perc_change_from_2020
    ipcc_comparison_labels <- c(ipcc_comparison_labels, "2030 GHG emissions percent change from 2020", 
                                "2040 GHG emissions percent change from 2020", 
                                "2050 GHG emissions percent change from 2020")
    ipcc_comparison_results <- c(ipcc_comparison_results, ghg_perc_change_2030_2020, 
                                 ghg_perc_change_2040_2020, ghg_perc_change_2050_2020)
    
    # temp change at peak warming and 2100
    temp_change_peak_warming <- (temp_1p5 %>% filter(deg_C_interp == max(deg_C_interp)))$deg_C_interp
    temp_change_2100 <- (temp_1p5 %>% filter(year == 2100))$deg_C_interp
    ipcc_comparison_labels <- c(ipcc_comparison_labels, "temp change at peak warming (deg C)", "temp change in 2100 (deg C)")
    ipcc_comparison_results <- c(ipcc_comparison_results, temp_change_peak_warming, temp_change_2100)
    
    # year of reaching 1.5C
    year_of_1p5 <- (temp_1p5 %>% filter(deg_C_interp >= 1.5))$year[1]
    ipcc_comparison_labels <- c(ipcc_comparison_labels, "year of reaching 1.5C")
    ipcc_comparison_results <- c(ipcc_comparison_results, year_of_1p5)
    ipcc_comparison <- tibble(ipcc_comparison_labels, ipcc_comparison_results) %>%
      mutate(scenario = sce)
    ipcc_comparison_final <- rbind(ipcc_comparison_final, ipcc_comparison)
    
  }
  
  write.csv(ipcc_comparison_final, paste0(results_dir, "/ipcc_comparison.csv"))
}

## Comparison of steel and industry emissions to scenarios in the IPCC AR6 database -------

# function to select data from the AR6 database - reference and 1.5C consistent scenarios
sel_AR6_data_1p5_ref <- function(input_data) {
  output_data <- input_data %>%
    left_join(AR6_metadata %>% dplyr::select(Model, Scenario, Category_name_FaIRv1.6.2, Policy_category_name)) %>%
    filter(grepl("Below 1.5", Category_name_FaIRv1.6.2) |
             Policy_category_name == "P1a: Baseline") %>%
    pivot_longer(-c(Model, Scenario, Region, Variable, Category_name_FaIRv1.6.2,
                    Policy_category_name, Unit), names_to = "year", values_to = "value") %>%
    mutate(scenario_type = case_when(Policy_category_name == "P1a: Baseline" ~ "AR6 reference",
                                     grepl("Below 1.5", Category_name_FaIRv1.6.2) ~ "AR6 1.5C"),
           year = as.numeric(year),
           value = as.numeric(value))
}

# get emissions from all industry
AR6_industry_sel <- AR6_data %>%
  filter(Variable == "Emissions|CO2|Energy|Demand|Industry" |
           Variable == "Emissions|CO2|Industrial Processes" | 
           Variable == "Emissions|CO2|Energy|Demand|Industry|Steel" | 
           Variable == "Emissions|CO2|Industrial Processes|Steel") %>%
  sel_AR6_data_1p5_ref() %>%
  filter(!is.na(value)) %>%
  ungroup()

# get just the steel emissions
AR6_steel <- AR6_industry_sel %>%
  filter(Variable == "Emissions|CO2|Energy|Demand|Industry|Steel" | 
           Variable == "Emissions|CO2|Industrial Processes|Steel") %>%
  group_by(year, Model, Scenario, Region, Category_name_FaIRv1.6.2, Policy_category_name, scenario_type, Unit) %>%
  summarize(value = sum(value))%>%
  ungroup()

# get the industry emissions
AR6_all_industry <- AR6_industry_sel %>%
  filter(Variable == "Emissions|CO2|Energy|Demand|Industry" |
           Variable == "Emissions|CO2|Industrial Processes") %>%
  group_by(year, Model, Scenario, Region, Category_name_FaIRv1.6.2, Policy_category_name, scenario_type, Unit) %>%
  summarize(value = sum(value))%>%
  ungroup()

# add to other data
industry_steel_CO2_em_global_w_IPCC <- AR6_all_industry %>%
  dplyr::select(year, CO2_Mt = value, Model, Scenario, scenario_type) %>%
  mutate(sector = "all industry") %>%
  bind_rows(AR6_steel %>%
              dplyr::select(year, CO2_Mt = value, Model, Scenario, scenario_type) %>%
              mutate(sector = "iron and steel"),
            # add GCAM scenarios - just steel and then industry including steel
            all_co2_em_sector_assigned %>%
              filter(sector %in% c("iron and steel") & region == "Global") %>%
              rbind(all_co2_em_sector_assigned %>%
                      filter(sector %in% c("industry", "iron and steel") & region == "Global") %>%
                      group_by(scenario, year, region) %>%
                      summarize(Mt_CO2e = sum(Mt_CO2e)) %>%
                      mutate(sector = "all industry")) %>%
              dplyr::select(-region) %>%
              mutate(scenario_type = scenario, Scenario = scenario, Model = "GCAM") %>%
              rename(CO2_Mt = Mt_CO2e)) %>%
  mutate(Model_Scenario = paste(Model, Scenario))

industry_steel_CO2_em_global_w_IPCC <- industry_steel_CO2_em_global_w_IPCC %>%
  mutate(Model_Scenario = factor(Model_Scenario,
                                 levels = c(unique((industry_steel_CO2_em_global_w_IPCC %>% 
                                                      filter(grepl("AR6", scenario_type)) %>%
                                                      arrange(scenario_type))$Model_Scenario),
                                            paste0("GCAM ", scenario_labels[2:4]), paste0("GCAM ",  scenario_labels[1]))),
         scenario_type = factor(scenario_type, levels = c("AR6 reference", "AR6 1.5C", scenario_labels)))

# plot
ggplot(data=filter(industry_steel_CO2_em_global_w_IPCC, year %in% plot_years),
       aes(x=year, y=CO2_Mt / 1000, group = Model_Scenario, color = scenario_type, size = scenario_type, alpha = scenario_type)) +
  facet_wrap(~sector) +
  geom_line() +
  scale_alpha_manual(values = c(0.5, 0.5, 1, 1, 1, 1), guide = "none") +
  scale_size_manual(values = c(0.5, 0.5, 1, 1, 1, 1), guide = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "slategray") +
  scale_color_manual(values = scenario_colors_IPCC_comp, name = "Scenario") +
  plot_theme +
  labs(y=expression(Gt~CO[2]), title = bquote(bold(Global~total~CO[2]~emissions)), x = "")
ggsave(paste0(fig_dir, "/co2_emis_industry_steel_global_IPCC_comp.png"), height = 6, width = 9, units = "in")

## Comparison of overall emissions to scenarios in the IPCC database -----------------

# get AR6 data for CO2 emissions
AR6_CO2 <- AR6_data %>%
  filter(Variable ==  "Emissions|CO2") %>%
  sel_AR6_data_1p5_ref()

# get AR6 data for GHGs (Kyoto gases)
AR6_GHG <- AR6_data %>%
  filter(Variable == "Emissions|Kyoto Gases") %>% 
  sel_AR6_data_1p5_ref()

# combine with our scenarios data
all_net_CO2 <- AR6_CO2 %>%
  arrange(Model, Scenario, Region, Variable, Unit, Category_name_FaIRv1.6.2, Policy_category_name, scenario_type, year) %>%
  group_by(Model, Scenario, Region, Variable, Unit, Category_name_FaIRv1.6.2, Policy_category_name, scenario_type) %>%
  mutate(value = gcamdata::approx_fun(year, value)) %>%
  ungroup() %>%
  mutate(CO2_Gt = value / 1000) %>%
  bind_rows(net_co2 %>%
              filter(region == "Global" & !is.na(scenario)) %>%
              mutate(value = value / 1000) %>%
              dplyr::select(scenario, year, CO2_Gt = value) %>%
              mutate(scenario_type = scenario, Scenario = scenario,
                     Model = "GCAM", Variable = "GCAM CO2")) %>%
  mutate(Model_Scenario = paste(Model, Scenario)) %>%
  ungroup()

all_net_CO2 <- all_net_CO2 %>%
  mutate(Model_Scenario = factor(Model_Scenario,
                                 levels = c(unique((all_net_CO2 %>% filter(grepl("AR6", scenario_type)))$Model_Scenario),
                                            paste0("GCAM ", scenario_labels[2:4]), paste0("GCAM ",  scenario_labels[1]))),
         scenario_type = factor(scenario_type, levels = c("AR6 reference", "AR6 1.5C", scenario_labels)))

all_total_ghg_em <- AR6_GHG %>%
  filter(year %in% unique(total_ghg_em$year)) %>%
  arrange(Model, Scenario, Region, Variable, Unit, Category_name_FaIRv1.6.2, Policy_category_name, scenario_type, year) %>%
  group_by(Model, Scenario, Region, Variable, Unit, Category_name_FaIRv1.6.2, Policy_category_name, scenario_type) %>%
  mutate(value = gcamdata::approx_fun(year, value)) %>%
  ungroup() %>%
  mutate(CO2e_Gt = value / 1000) %>%
  bind_rows(total_ghg_em %>%
              filter(region == "Global" & !is.na(scenario)) %>%
              mutate(CO2e_Gt = Mt_CO2e / 1000) %>%
              dplyr::select(scenario, year, CO2e_Gt) %>%
              mutate(scenario_type = scenario, Scenario = scenario,
                     Model = "GCAM", Variable = "GCAM GHG")) %>%
  mutate(Model_Scenario = paste(Model, Scenario)) %>%
  ungroup()

all_total_ghg_em <- all_total_ghg_em %>%
  mutate(Model_Scenario = factor(Model_Scenario,
                                 levels = c(unique((all_total_ghg_em %>% filter(grepl("AR6", scenario_type)))$Model_Scenario),
                                            paste0("GCAM ", scenario_labels[2:4]), paste0("GCAM ",  scenario_labels[1]))),
         scenario_type = factor(scenario_type, levels = c("AR6 reference", "AR6 1.5C", scenario_labels)))


# plot 
CO2_plot_AR6 <- ggplot(all_net_CO2 %>% filter(year >= 2005 & year %in% plot_years),
                       aes(x = year, y = CO2_Gt, group = Model_Scenario, 
                           color = scenario_type, size = scenario_type, alpha = scenario_type)) +
  geom_line() +
  scale_alpha_manual(values = c(0.5, 0.5, 1, 1, 1, 1), guide = "none") +
  scale_size_manual(values = c(0.5, 0.5, 1, 1, 1, 1), guide = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = scenario_colors_IPCC_comp, name = "Scenario") +
  labs(title =bquote(bold(Global~CO[2]~emissions)),
       x="", y=bquote(Gt~CO[2])) +
  plot_theme + 
  theme(legend.position = "none")

GHG_plot_AR6 <- ggplot(all_total_ghg_em %>% filter(year >= 2005 & year %in% plot_years),
                       aes(x = year, y = CO2e_Gt, 
                           group = Model_Scenario, color = scenario_type, 
                           size = scenario_type, alpha = scenario_type)) +
  geom_line() +
  scale_alpha_manual(values = c(0.5, 0.5, 1, 1, 1, 1), guide = "none") +
  scale_size_manual(values = c(0.5, 0.5, 1, 1, 1, 1), guide = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = scenario_colors_IPCC_comp, name = "Scenario") +
  labs(title ="Global GHG emissions",
       x="", y=bquote(Gt~CO[2]~e)) +
  plot_theme

plot_grid(CO2_plot_AR6, GHG_plot_AR6,
          ncol = 2, nrow = 1, labels = c("a", "b"), label_size = 13,
          rel_widths = c(0.65, 1))

ggsave(paste0(fig_dir, "/co2_ghg_emis_total_global_IPCC_comp.png"), height = 6, width = 11, units = "in")


# NO NEW EU GAS SENSITIVITY ---------------------
# re-read in the raw data for steel production by tech
ironsteel_production_tech_eu_gas_sens <- 
  readr::read_csv(paste0(run_dir, "/queryoutall_ironsteel_production_tech.csv"), 
                  skip = 1)

ironsteel_production_tech_eu_gas_sens <- ironsteel_production_tech_eu_gas_sens %>% 
  parse_output_scenario %>% 
  add_global_sum() %>%
  aggregate_regions(region_mapping, colname = "steel_region")

ironsteel_production_tech_eu_gas_sens$scenario <- factor(ironsteel_production_tech_eu_gas_sens$scenario, 
                                                         levels = scenarios_no_new_EU_gas, 
                                                         labels=scenario_labels_no_new_EU_gas)
ironsteel_production_tech_eu_gas_sens$technology <- factor(ironsteel_production_tech_eu_gas_sens$technology, 
                                               levels = unique(ironsteel_production_tech_eu_gas_sens$technology),
                                               labels = c('BF-biomass',
                                                          'BF-BOF','BF-CCUS','BF-H2','DRI-EAF-Fossil','DRI-EAF-CCUS',
                                                          'EAF-scrap','DRI-EAF-H2'))

ggplot(data=filter(ironsteel_production_tech_eu_gas_sens, region == "Europe",
                   year %in% plot_years, scenario != "NA"),
       aes(x=year, y=value, fill=technology)) +
  geom_col()+
  facet_wrap(~scenario, nrow = 1) +
  labs(title = "Europe iron and steel production by technology", x="", y="Mt") +
  scale_fill_manual(values = tech_colors, name = "Technology")+
  plot_theme

ggsave(paste0(fig_dir, "/ironsteel_production_tech_Europe_no_new_EU_gas.png"), height = 6, width = 10, units = "in")

# save data to csv 
ironsteel_production_tech_eu_gas_sens_data <- filter(ironsteel_production_tech_eu_gas_sens, region %in% regions_aggregated, 
                                         year %in% plot_years, scenario!="NA")
write.csv(ironsteel_production_tech_eu_gas_sens_data , paste0(results_dir, "/ironsteel_production_tech_eu_gas_sens_data.csv"))

# also save data with shares
ironsteel_production_tech_eu_gas_sens_share_data <- ironsteel_production_tech_eu_gas_sens_data %>%
  group_by(region, scenario, year) %>%
  mutate(total = sum(value),
         share = value / total * 100) %>%
  ungroup()

write.csv(ironsteel_production_tech_eu_gas_sens_share_data, paste0(results_dir, "/ironsteel_production_tech_eu_gas_sens_share_data.csv"))


# COALESCED FIGURES FOR PAPER -----------------------

## Emissions from steel and reduction contributions waterfall ----------
# get waterfall chart and MEF production chart by running other script
source("steel_decarb_waterfall_chart_final_function.R")
waterfall_output <- steel_decarb_waterfall_chart()
waterfall_chart <- waterfall_output[[1]]
MEF_chart <- waterfall_output[[2]]

steel_CO2_plots_1 <- plot_grid(steel_CO2_total_global + theme(legend.position = "none"), 
                               steel_CO2_total_cum_global,
                               ncol = 2, nrow = 1, align = "hv", axis = "tb",
                               rel_widths = c(1, 1), labels = c("a", "b"), label_size = 13)
steel_CO2_plots_2 <- plot_grid(direct_indirect_CO2_steel_global, 
                               steel_CO2_share,
                               ncol = 2, nrow = 1, align = "hv", axis = "tb", 
                               rel_widths = c(1, 1), labels = c("c", "d"), label_size = 13)
plot_grid(steel_CO2_plots_1, steel_CO2_plots_2, 
          waterfall_chart + plot_theme + theme(axis.text.x = element_text(angle = 25, hjust = 1)), 
          ncol = 1, nrow = 3, labels = c("", "", "e"), 
          label_size = 13, rel_heights = c(1, 1, 1.5))
ggsave(paste0(fig_dir, "/main_fig_emissions_steel.png"), height = 15, width = 11, units = "in")

## Material efficiency, production by tech, and energy use; global, all scenarios ---------
# combine material efficiency pie chart and production charts
# MEF_charts_comb <- plot_grid(MEF_chart + theme(legend.position = "bottom"),
#                              mat_eff_bar, 
#                              nrow = 1, ncol = 2, align = "v", axis = "tb",
#                              labels = c("a", "b"), label_size = 13)
# 
MEF_charts_comb <- plot_grid(MEF_chart,
                             mat_eff_pie,
                             nrow = 2, ncol = 1, align = "h", axis = "lr",
                             labels = c("a", "b"), label_size = 13)

prod_en_fuel_global <- plot_grid(production_tech_global, energy_fuel_global, nrow = 2, ncol = 1,
                                 labels = c("c", "d"), label_size = 13, align = "hv", axis = "lr")

# plot_grid(MEF_charts_comb, prod_en_fuel_global, nrow = 2, ncol = 1, rel_heights = c(1, 1.5))

plot_grid(MEF_chart, mat_eff_pie, prod_en_fuel_global, 
          nrow = 3, ncol = 1, labels = c("a", "b", ""), label_size = 13, rel_heights = c(1, 1, 2))
ggsave(paste0(fig_dir, "/main_fig_mateff_prod_tech_energy_fuel_global.png"), height = 15, width = 11, units = "in")

ggsave(paste0(fig_dir, "/main_fig_prod_tech_energy_fuel_global.png"), 
       plot = plot_grid(production_tech_global, energy_fuel_global, nrow = 2, ncol = 1,
                        labels = c("a", "b"), label_size = 13, align = "hv", axis = "lr"), 
       height = 8, width = 11, units = "in")

## Production by tech and energy use, regional, 1p5 scenario -------------

plot_grid(production_tech_1p5_regions, energy_fuel_1p5_regions,
          labels = c("a", "b"), nrow = 2, ncol = 1, label_size = 13, 
          align = "hv", axis = "lrtb")
ggsave(paste0(fig_dir, "/main_fig_prod_tech_energy_fuel_1p5_regions.png"),  
       height = 13, width = 11, units = "in")

