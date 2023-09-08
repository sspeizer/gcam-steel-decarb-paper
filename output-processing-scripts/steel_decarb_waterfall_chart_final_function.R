# script to process data and generate the waterfall chart for the steel decarbonization paper,
# as well as material efficiency production figures - as a function to return the waterfall chart

steel_decarb_waterfall_chart <- function(run_dir) {

library(waterfalls)
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
library(rgcam)
library(jgcricolors)
library(ggsci)
library(zoo)

library(extrafont)
loadfonts(quiet = T)
options(scipen=999)

# LOAD DATA AND SET DIRECTORIES ---------------

# set directories
fig_dir <- paste0(run_dir,"/figures_waterfall_final_function")
if (!dir.exists(fig_dir)) {dir.create(fig_dir)}
setwd("C:/Users/spei632/Documents/GCAM_industry/R/steel_decarb_paper")

# load functions
source("functions_final/functions.R")
source("functions_final/diag_util_functions.R")

# load mapping files
region_mapping <- read.csv("mappings_final/steel_region_mapping.csv")
regions_aggregated <- unique(region_mapping$steel_region)

# load constants
C_to_CO2 <- 44/12

# load data
# queries <- list.files(run_dir, pattern = 'queryoutall')
# 
# for (i in queries) {
#   filename <- gsub('.csv','', i) %>% gsub('queryoutall_','', .)
#   assign(filename, readr::read_csv(paste0(run_dir, "/", i), skip = 1))
# }
CO2_emissions_sector_nobio_v2 <- read_csv(paste0(run_dir, "/queryoutall_CO2_emissions_sector_nobio_v2.csv"), skip = 1)
ironsteel_production <- read_csv(paste0(run_dir, "/queryoutall_ironsteel_production.csv"), skip = 1)
CO2_emissions_subsector <- read_csv(paste0(run_dir, "/queryoutall_CO2_emissions_subsector.csv"), skip = 1)
ironsteel_production_tech <- read_csv(paste0(run_dir, "/queryoutall_ironsteel_production_tech.csv"), skip = 1)
CO2_emissions_tech <- read_csv(paste0(run_dir, "/queryoutall_CO2_emissions_tech.csv"), skip = 1)
CO2_squestration_subsector <- read_csv(paste0(run_dir, "/queryoutall_CO2_squestration_subsector.csv"), skip = 1)

# SET SCENARIOS AND SCENARIO LABELS ---------------
scenarios = c("Reference_1", "1p5C_12", "Reference_2", "Reference_3")
# scenarios = c("Reference_1", "1p5C_10", "Reference_2", "Reference_3")
scenario_labels = c("Reference", "1.5C", "ref_advEE", "ref_MEF")

# PREPARE DATA --------------

# import total emissions data and rename scenarios
CO2_emissions_sector_nobio <- CO2_emissions_sector_nobio_v2 %>% parse_output_scenario %>% add_global_sum()
CO2_emissions_sector_nobio <- CO2_emissions_sector_nobio %>% dplyr::mutate(value = if_else(Units == "MTC", value * C_to_CO2, value),
                                                                           Units = if_else(Units == "MTC", "MTCO2", Units))

CO2_emissions_sector_nobio <- CO2_emissions_sector_nobio %>% aggregate_regions(region_mapping, colname = "steel_region")

CO2_emissions_sector_nobio$scenario  <- factor(CO2_emissions_sector_nobio$scenario, levels = scenarios, labels=scenario_labels)
CO2_emissions_sector_nobio <- filter(CO2_emissions_sector_nobio, sector=="iron and steel")

# import production and rename scenarios
ironsteel_production <- ironsteel_production %>% parse_output_scenario %>% add_global_sum()
ironsteel_production <- ironsteel_production %>% aggregate_regions(region_mapping, colname = "steel_region")
ironsteel_production$scenario  <- factor(ironsteel_production$scenario, levels = scenarios, labels=scenario_labels)

# import emissions by subsector data and rename scenarios
CO2_emissions_subsector <- CO2_emissions_subsector %>% parse_output_scenario %>% add_global_sum()%>% dplyr::mutate(value = if_else(Units == "MTC", value * C_to_CO2, value),
                                                                                                                   Units = if_else(Units == "MTC", "MTCO2", Units))
CO2_emissions_subsector <- CO2_emissions_subsector %>% aggregate_regions(region_mapping, colname = "steel_region")
CO2_emissions_subsector$scenario  <- factor(CO2_emissions_subsector$scenario, levels = scenarios, labels=scenario_labels)

# import production by tech
ironsteel_production_tech <- ironsteel_production_tech %>% parse_output_scenario %>% add_global_sum()
ironsteel_production_tech <- ironsteel_production_tech %>% aggregate_regions(region_mapping, colname = "steel_region")
ironsteel_production_tech$scenario  <- factor(ironsteel_production_tech$scenario, levels = scenarios, labels=scenario_labels)

# import emissions by tech
CO2_emissions_tech <- CO2_emissions_tech %>% parse_output_scenario %>% add_global_sum() %>% dplyr::mutate(value = if_else(Units == "MTC", value * C_to_CO2, value),
                                                                                                          Units = if_else(Units == "MTC", "MTCO2", Units))
CO2_emissions_tech <- CO2_emissions_tech %>% aggregate_regions(region_mapping, colname = "steel_region")
CO2_emissions_tech$scenario  <- factor(CO2_emissions_tech$scenario, levels = scenarios, labels=scenario_labels)

# import CCS sequestration data
CO2_squestration_subsector <- CO2_squestration_subsector %>% parse_output_scenario %>% add_global_sum() %>% dplyr::mutate(value = if_else(Units == "MTC", value * C_to_CO2, value),
                                                                                                                          Units = if_else(Units == "MTC", "MTCO2", Units))
CO2_squestration_subsector <- CO2_squestration_subsector %>% aggregate_regions(region_mapping, colname = "steel_region")
CO2_squestration_subsector$scenario  <- factor(CO2_squestration_subsector$scenario, levels = scenarios, labels=scenario_labels)


# SET PLOT THEMES AND COLORS ---------------
plot_theme <- theme_bw() +
  theme(legend.text = element_text(size = 15, vjust = 0.5)) +
  theme(legend.title = element_text(size = 15, vjust = 2)) +
  theme(axis.text = element_text(size = 15)) +
  #theme(axis.title = element_text(size = 15, face = "bold")) +
  theme(axis.title = element_text(size = 15)) +
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

# CALCULATE EMISSIONS CONTRIBUTIONS ---------------

results4 <- c("Mitigation measure", "region","reduction","year")

for (selected_year in seq(2020, 2050, 5)){
  results <- c("Reference emissions", "Energy efficiency contribution",
               "Material efficiency contribution", "Price-induced contribution",
               "High scrap use contribution", "H2 use contribution", 
               "CCS contribution", "1.5C emissions")
  for (i in regions_aggregated) {
    #set region
    selected_region <- i
    
    #ref emissions and 1.5C emissions
    ref_emissions <- filter(CO2_emissions_sector_nobio, region == selected_region, year == selected_year, scenario=="Reference")$value
    emissions_1p5 <- filter(CO2_emissions_sector_nobio, region == selected_region, year == selected_year, scenario=="1.5C")$value
    total_reduction <- emissions_1p5 - ref_emissions
    
    #1
    #a.	Contribution of adv EE to emissions reduction: emissions reduction from ref + adv EE (see below)
    
    #calculate adv EE emissions
    adv_EE_emissions <- filter(CO2_emissions_sector_nobio, region == selected_region, year == selected_year, scenario=="ref_advEE")$value
    adv_EE_contribution <- adv_EE_emissions - ref_emissions
    
    #b.	Contribution of adv MEF to emissions reduction: emissions reduction from ref + adv MEF (see below)
    adv_MEF_emissions <- filter(CO2_emissions_sector_nobio, region == selected_region, year == selected_year, scenario=="ref_MEF")$value
    adv_MEF_contribution <- adv_MEF_emissions - ref_emissions
    
    #c.	Contribution of price-induced demand reduction to emissions reduction: (price-induced demand reduction/MEF)* emissions reduction from adv MEF (bullet point b)
    # alternatively explained: (MEF reduction in emissions) / (MEF reduction in production) * (price induced reduction in production)
    MEF_production <- filter(ironsteel_production, region == selected_region, year == selected_year, scenario == "ref_MEF")$value
    ref_production <- filter(ironsteel_production, region == selected_region, year == selected_year, scenario == "Reference")$value
    MEF_reduction <- ref_production - MEF_production
    
    price_induced_production <- filter(ironsteel_production, region == selected_region, year == selected_year, scenario == "1.5C")$value
    price_induced_reduction <-  price_induced_production - MEF_production
    
    price_induced_contribution <- (price_induced_reduction / MEF_reduction) * adv_MEF_contribution * -1
    
    #d.	Contribution of high scrap to emissions reduction:     
    # (production from scrap in 1.5 - production from scrap in ref) * (average emissions intensity of scrap in 1.5 - emissions intensity of other techs in ref)
    scrap_1p5 <- filter(ironsteel_production_tech, region == selected_region, year == selected_year, scenario == "1.5C", subsector == "EAF with scrap")$value
    scrap_REF <- filter(ironsteel_production_tech, region == selected_region, year == selected_year, scenario == "Reference", subsector == "EAF with scrap")$value
    
    other_tech_REF<- filter(ironsteel_production_tech, region == selected_region, year == selected_year, scenario == "Reference", subsector != "EAF with scrap")
    emissions_other_tech <- filter(CO2_emissions_subsector, region == selected_region, year == selected_year, scenario == "Reference", subsector != "EAF with scrap", sector == "iron and steel")
    other_tech_emissions_intensity <- sum(emissions_other_tech$value)/sum(other_tech_REF$value)
    
    emissions_scrap_1p5 <- filter(CO2_emissions_subsector, region == selected_region, year == selected_year, scenario == "1.5C", subsector == "EAF with scrap")$value
    scrap_emissions_intensity <- emissions_scrap_1p5 / scrap_1p5
    
    high_scrap_contribution <- (scrap_1p5 - scrap_REF) * (scrap_emissions_intensity - other_tech_emissions_intensity)
    
    #e.	Contribution of hydrogen: separate H2-DRI and H2-BF
    # H2-DRI: (steel production from H2-DRI in 1.5) * (average emission intensity in ref - emissions intensity of H2-DRI 
    # which is assumed to be zero)
    prod_HDRI_1p5 <-  filter(ironsteel_production_tech, region == selected_region, year == selected_year, scenario == "1.5C", technology == "Hydrogen-based DRI")$value
    emissions_average <- filter(CO2_emissions_subsector, region == selected_region, year == selected_year, scenario == "Reference", sector == "iron and steel")
    average_emissions_intensity <- sum(emissions_average$value) / ref_production
    
    H2_DRI_contribution <- prod_HDRI_1p5 * average_emissions_intensity * -1
    
    # H2-BF: (difference in emissions intensity between BF in ref and H2-BF in 1.5C)*(H2-BF production in 1.5C)
    H2_BF_prod_1p5 <- (ironsteel_production_tech %>% filter(region == selected_region & year == selected_year & scenario == "1.5C" & 
                                                              technology == "BLASTFUR with hydrogen"))$value
    H2_BF_emissions_1p5 <- (CO2_emissions_tech %>% filter(region == selected_region & year == selected_year & scenario == "1.5C" & 
                                                            technology == "BLASTFUR with hydrogen"))$value
    H2_BF_emissions_intensity_1p5 <- H2_BF_emissions_1p5 / H2_BF_prod_1p5
    BF_std_prod <- (ironsteel_production_tech %>% filter(region == selected_region & year == selected_year & scenario == "Reference" & 
                                                           technology == "BLASTFUR"))$value
    BF_std_emissions <- (CO2_emissions_tech %>% filter(region == selected_region & year == selected_year & scenario == "Reference" & 
                                                         technology == "BLASTFUR"))$value
    BF_std_emissions_intensity <- BF_std_emissions / BF_std_prod
    H2_BF_contribution <- (H2_BF_emissions_intensity_1p5 - BF_std_emissions_intensity) * H2_BF_prod_1p5
    if(is.na(H2_BF_contribution)) {H2_BF_contribution <- 0}
    
    H2_contribution <- H2_DRI_contribution + H2_BF_contribution
    
    # f.	Contribution of CCS - actual carbon sequestration amount
    # edit - need to group by sector and summarize
    CCS_contribution <- sum(filter(CO2_squestration_subsector, sector == "iron and steel", region ==  selected_region, scenario == "1.5C", year == selected_year)$value) * -1
    
    
    val <- c(ref_emissions, adv_EE_contribution, adv_MEF_contribution, price_induced_contribution,
             high_scrap_contribution, H2_contribution, CCS_contribution, emissions_1p5)
    
    results <- data.frame(cbind(results, val))
  }
  colnames(results)<-c("Mitigation measure", regions_aggregated)
  results2 <- gather(results, region, reduction, US:Global)
  results3 <- results2 %>% mutate(year=selected_year)
  results4 <- rbind(results4, results3)
}

# ADJUST CALCULATIONS TO MATCH ACTUAL EMISSIONS REDUCTIONS QUANTITY ---------------
# make sure that the reduction contributions from each of the mitigation measures
# sum to the accurate total reduction quantity, by finding the share of each calculated
# mitigation measure's reduction to the sum of all the calculated mitigation measure's reductions,
# and applying that share to the actual emissions reduction in 1.5C relative to reference
results4 <- results4[-1,] %>%
  mutate(reduction = abs(as.numeric(reduction)))

results2 <- results4 

# get just 1.5 and reference emissions
spread <- spread(results2, `Mitigation measure`, reduction) %>%
  select(c(`1.5C emissions`,`Reference emissions`, region, year))

# get total emissions reductions calculated via our methodology and how they compare to actual reductions
total <- results2 %>%
  group_by(region, year)%>%
  filter(`Mitigation measure` != "1.5C emissions" & `Mitigation measure` != "Reference emissions") %>%
  dplyr::summarize(total = sum(reduction)) %>%
  left_join(spread, by = c("region","year"))

# obtain the share of each mitigation measure to the sum of calculated mitigation measure reductions
results_final <- results2 %>%
  left_join(total, by =c("region","year")) %>%
  mutate(difference = `Reference emissions` - `1.5C emissions` ) %>%
  mutate(share = reduction / total) %>%
  # apply the share to the actual overall total emissions reduction in 1.5
  mutate(adjusted = share * difference) %>%
  mutate(adjusted = if_else(`Mitigation measure` == "1.5C emissions", `1.5C emissions`, adjusted)) %>%
  mutate(adjusted = if_else(`Mitigation measure` == "Reference emissions", `Reference emissions`, adjusted))%>%
  mutate(reduction = adjusted) %>%
  select(-adjusted)

# MAKE WATERFALL PLOT ----------------
# first save the data
write.csv(results_final, paste0(fig_dir, "/waterfall_data.csv"))
# write.csv(results_final, paste0(fig_dir, "/waterfall_data_1p5_low_CCS.csv"))

results_final$`Mitigation measure` <- factor(results_final$`Mitigation measure`, levels = c("Energy efficiency contribution",
                                                                                            "Material efficiency contribution", "Price-induced contribution",
                                                                                            "High scrap use contribution", "H2 use contribution", 
                                                                                            "CCS contribution",
                                                                                            "1.5C emissions","Reference emissions"))

reduction_colors <- c( "Energy efficiency contribution" = "gray50",
                       "Material efficiency contribution" = "darkturquoise",
                       "Price-induced contribution" = "#33A02C",
                       "High scrap use contribution" = "#756bb1",
                       "H2 use contribution" = "darkgoldenrod3",
                       "CCS contribution" ="#3182bd",
                       "1.5C emissions" = "#E31A1C")

for (i in regions_aggregated) {
  ggplot(data=filter(results_final, region == i, `Mitigation measure` != "Reference emissions"),
         aes(x=as.numeric(year), y=abs(reduction), fill=`Mitigation measure`)) +
    geom_area()+
    labs(title = bquote(bold(.(i)~CO[2]~emissions~reduction~contributions~from~mitigation~measures)), x="", y=bquote(MtCO[2])) +
    scale_fill_manual(values = reduction_colors)+
    plot_theme
  
  ggsave(paste0(fig_dir, "/reduction_contributions_", i, ".png"), height = 6, width = 11, units = "in")
}

results_final_edit <- results_final %>% 
  mutate(reduction = if_else(`Mitigation measure` == "Reference emissions", 
                             reduction, reduction *-1))

for (i in regions_aggregated){
  waterfall_data <- data.frame(filter(results_final_edit,region==i,`Mitigation measure` != "1.5C emissions", year == 2050)) %>%
    # add color
    mutate(color = reduction_colors[Mitigation.measure])
  waterfall(select(waterfall_data, c(Mitigation.measure, reduction)), calc_total = TRUE,
            total_rect_text_color = "black", total_axis_text = "1.5C emissions",
            fill_colours = c(reduction_colors["1.5C emissions"], waterfall_data$color[2:7]), 
            fill_by_sign = FALSE, rect_text_labels = c("", round(waterfall_data$share[2:7]*100)), total_rect_text = "")+
    theme_minimal()+
    labs(title = paste0(i, " contributions from mitigation measures in 2050"),
         y=bquote(MtCO[2]), x=" " )+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    theme(text = element_text(size=18))
  
  ggsave(paste0(fig_dir, "/waterfall_chart_values_", i, ".png"), height = 7, width = 9, units = "in")
}

# MAKE PLOT OF PRODUCTION AND CONTRIBUTIONS FROM MATERIAL EFFICIENCY AND PRICE INDUCED -------------
for (i in regions_aggregated) {
  ggplot(data=filter(ironsteel_production, region == i, year %in% c(2015:2050), scenario=="ref_MEF" | scenario=="Reference"|scenario=="1.5C"),
         aes(x=year, y=value, color = scenario)) +
    geom_line(size = 1.2) +
    labs(title = paste(i, "iron and steel production"), x="", y="Mt") +
    scale_y_continuous(limits = c(0, NA)) +
    scale_color_manual(labels = c("Reference","Material efficiency","Material efficiency and price increase") , values = c("Reference" = "#E31A1C", "ref_MEF"="#3182bd", "1.5C"="#33A02C")) +
    plot_theme
  
  ggsave(paste0(fig_dir, "/ironsteel_production_MEF_1p5_ref", i, ".png"), height = 8, width = 10, units = "in")
}

# save data
write.csv(ironsteel_production, paste0(fig_dir, "/ironsteel_production_MEF_1p5_ref.csv"))


# return the global waterfall chart and production figure in the different scenarios
waterfall_data_global <- data.frame(filter(results_final_edit,
                                           region=="Global",
                                           `Mitigation measure` != "1.5C emissions", 
                                           year == 2050)) %>%
  # add color
  mutate(color = reduction_colors[Mitigation.measure])

return_plot <- waterfall(select(waterfall_data_global %>% mutate(reduction = reduction / 1000), c(Mitigation.measure, reduction)), calc_total = TRUE,
          total_rect_text_color = "black", total_axis_text = "1.5C emissions",
          fill_colours = c(reduction_colors["1.5C emissions"], waterfall_data_global$color[2:7]), 
          fill_by_sign = FALSE, rect_text_labels = c("", paste0(round(waterfall_data_global$share[2:7]*100), "%")), 
          total_rect_text = "", rect_text_size = 1.8)+
  theme_minimal()+
  labs(title = "Global contributions to emissions reductions from mitigation measures in 2050",
       y= bquote(Gt~CO[2]), x=" ")+
  theme(axis.text.x = element_text(angle = 20, hjust = 1))+
  theme(text = element_text(size=18))

# waterfall(select(waterfall_data_global %>% mutate(reduction = reduction / 1000), c(Mitigation.measure, reduction)), calc_total = TRUE,
#                          total_rect_text_color = "black", total_axis_text = "1.5C emissions",
#                          fill_colours = c(reduction_colors["1.5C emissions"], waterfall_data_global$color[2:7]),
#                          fill_by_sign = FALSE, rect_text_labels = c("", paste0(round(waterfall_data_global$share[2:7]*100), "%")),
#                          total_rect_text = "", rect_text_size = 1.8)+
#   theme_minimal()+
#   labs(title = "Global contributions to emissions reductions from mitigation measures in 2050\n1.5C EE+MEF+recycling+H2 scenario",
#        y= bquote(Gt~CO[2]), x=" ")+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   theme(text = element_text(size=18))
# ggsave(paste0(fig_dir, "/waterfall_chart_values_Global_1p5_low_CCS.png"), height = 7, width = 11, units = "in")

# also get plot to return for MEF
return_plot_MEF <- ggplot(data=filter(ironsteel_production, region == "Global", year %in% c(2015:2050), scenario=="ref_MEF" | scenario=="Reference"|scenario=="1.5C") %>%
                            mutate(scenario = factor(scenario, levels = c("1.5C", "ref_MEF", "Reference"))),
                          aes(x=year, y=value, color = scenario)) +
  geom_line(size = 1.2) +
  labs(title = "Global steel production and consumption", x="", y="Mt") +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(labels = c("Reference","Material efficiency","Material efficiency and price increase") , 
                     values = c("Reference" = "#E31A1C", "ref_MEF"="#3182bd", "1.5C"="#33A02C"),
                     name = "") +
  plot_theme

return(list(return_plot, return_plot_MEF))
}