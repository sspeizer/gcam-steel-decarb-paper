# file to generate an xml with advanced hydrogen assumptions for the iron and
# steel decarbonization paper, based on the hydrogen setup in GCAM6.0.
# implements lower cost of production for hydrogen

library(readr)
library(tidyr)
library(dplyr)
library(plotly)
library(gcamdata)

# load data

# data that goes into the xml for GCAM6.0 for hydrogen global tech costs and share weights
# these were obtained by saving the outputs from zchunk_L225.hydrogen.R.
L225.GlobalTechCost_h2 <- read_csv("C:/Users/spei632/Documents/GCAM_industry/data/GCAM6p0_hydrogen_pre-xml_data/L225.GlobalTechCost_h2.csv")
L225.GlobalTechShrwt_h2 <- read_csv("C:/Users/spei632/Documents/GCAM_industry/data/GCAM6p0_hydrogen_pre-xml_data/L225.GlobalTechShrwt_h2.csv")
L225.StubTechCost_h2 <- read_csv("C:/Users/spei632/Documents/GCAM_industry/data/GCAM6p0_hydrogen_pre-xml_data/L225.StubTechCost_h2.csv")

# relevant data produced by zchunk_L223.electricity.R - advanced capital costs for renewables
# these were obtained by saving the values from zchunk_L223.electricity.R
L225.GlobalTechCoef_h2 <- read_csv("C:/Users/spei632/Documents/GCAM_industry/data/GCAM6p0_hydrogen_pre-xml_data/L225.GlobalTechCoef_h2.csv")
L223.StubTechCapFactor_elec <- read_csv("C:/Users/spei632/Documents/GCAM_industry/data/GCAM6p0_hydrogen_pre-xml_data/L223.StubTechCapFactor_elec.csv")
L223.GlobalIntTechOMfixed_elec <- read_csv("C:/Users/spei632/Documents/GCAM_industry/data/GCAM6p0_hydrogen_pre-xml_data/L223.GlobalIntTechOMfixed_elec.csv")
L223.GlobalIntTechCapital_elec_adv <- read_csv("C:/Users/spei632/Documents/GCAM_industry/data/GCAM6p0_hydrogen_pre-xml_data/L223.GlobalIntTechCapital_elec_adv.csv")

# constants and other useful values
source("C:/Users/spei632/Documents/repositories/testing/gcam-core/input/gcamdata/R/constants.R")


# COSTS - global tech database

# ASSUMPTIONS - these came from Patrick's advanced H2 assumptions, the methodology
# is also adapted from him as well
# advanced improvement rate in costs is set to be 2*(std improvement rate);
# maximum improvement rate in costs is set to be 0.25+(std max improvement) (where
# max improvement is the 2100 value relative to 2015 value) unless the new value
# exceeds 75% cost reduction (then use 75%);
# advanced = standard through 2020

# assumptions for adjusting improvement rate and max improvement
adj_impvt_rate_factor = 2
adj_max_impvt = 0.25
max_impvt_limit = 0.75

# select just the relevant sectors from the H2 global tech costs - only want
# production costs.
# also not applying this to compression and storage costs for forecourt 
# production, only to production costs.
# global_tech_cost_sel <- L225.GlobalTechCost_h2 %>%
#   filter(subsector.name %in% c("biomass", "electricity", "gas", "nuclear", "coal", 
#                                "forecourt production") & 
#            minicam.non.energy.input %in% c("non-energy", "production"))
global_tech_cost_sel <- L225.GlobalTechCost_h2 %>%
  filter(sector.name == "H2 central production"  | 
           (subsector.name == "forecourt production" & 
              minicam.non.energy.input %in% c("non-energy", "production")))

# calculate improvement rate till 2040 for each in the standard, for non-CCS
global_tech_cost_impvt_no_CCS <- global_tech_cost_sel %>%
  filter(!grepl("CCS", technology)) %>%
  pivot_wider(names_from = year, values_from = `input.cost`) %>%
  mutate(impvt_rate_2015_2040 = 1 - (`2040`/`2015`)^(1/5),
         max_impvt_2015_2100 = 1 - `2100` / `2015`,
         adv_impvt_rate = impvt_rate_2015_2040 * adj_impvt_rate_factor,
         adv_max_impvt_2015_2100 = max_impvt_2015_2100 + adj_max_impvt,
         adv_max_impvt_2015_2100 = ifelse(adv_max_impvt_2015_2100 > max_impvt_limit,
                                          max_impvt_limit, 
                                          adv_max_impvt_2015_2100),
         # calculate the actual maximum improved value
         adv_max_impvt_value =  (1 - adv_max_impvt_2015_2100) * `2015`)

# calculate the new values for global tech costs, again non CCS only
global_tech_cost_advanced_no_CCS <- global_tech_cost_sel %>%
  filter(!grepl("CCS", technology)) %>%
  left_join(global_tech_cost_impvt_no_CCS %>% 
              rename(cost_2020 = `2020`) %>%
              dplyr::select(c(sector.name, subsector.name, technology,
                              minicam.non.energy.input, units, cost_2020, 
                              adv_impvt_rate, adv_max_impvt_2015_2100,
                              adv_max_impvt_value))) %>%
  # calculate new costs - improvement rate ^(years since 2020)/5 * 2020 cost
  # advanced values are same as standard through 2020, adjustment starts in 2025
  mutate(input.cost_adv = case_when(year <= 2020 ~ input.cost,
                                    year > 2020 ~ 
                                      cost_2020*(1 - adv_impvt_rate)^((year - 2020)/5)),
         # if the new calculated cost is such that it exceeds the max improvement, 
         # set to the maximum improved value
         input.cost_adv = ifelse(input.cost_adv < adv_max_impvt_value,
                                 adv_max_impvt_value,
                                 input.cost_adv))

# for CCS techs, calculate the additional cost in each period due to CCS;
# this is added to the advanced cost of the non-CCS tech to calculate the
# new advanced cost of the tech with CCS (so the CCS portion of the cost does
# not change)
global_tech_cost_CCS_extra <- global_tech_cost_sel %>%
  mutate(technology_edit = gsub(" CCS", "", technology)) %>%
  filter(technology_edit %in% c("biomass to H2", 
                                "natural gas steam reforming",
                                "coal chemical")) %>%
  group_by(sector.name, subsector.name, minicam.non.energy.input, 
           technology_edit, year) %>%
  summarize(dif_cost_w_CCS = max(input.cost) - min(input.cost)) %>%
  filter(dif_cost_w_CCS > 0)

# now add the CCS technologies
global_tech_cost_advanced_CCS <- global_tech_cost_advanced_no_CCS %>%
  left_join(global_tech_cost_CCS_extra %>% rename(technology = technology_edit)) %>%
  filter(!is.na(dif_cost_w_CCS)) %>%
  # add the CCS additional cost to the advanced tech cost, and relabel as the 
  # tech with CCS
  mutate(input.cost_adv = input.cost_adv + dif_cost_w_CCS,
         technology = paste0(technology, " CCS")) %>%
  dplyr::select(-dif_cost_w_CCS)
  
# combine all
global_tech_cost_advanced <- rbind(global_tech_cost_advanced_no_CCS, 
                                   global_tech_cost_advanced_CCS)

# plot just to see what they look like
plot_ly(global_tech_cost_advanced %>%
          mutate(plot_label = paste0(sector.name, subsector.name, technology, 
                                     minicam.non.energy.input))) %>%
  add_trace(type = "scatter",
            mode = "lines",
            x = ~year,
            y = ~input.cost_adv,
            color = ~plot_label)


# add back in unchanged values
L225.GlobalTechCost_h2_adv <- global_tech_cost_advanced %>%
  dplyr::select(sector.name, subsector.name, technology, minicam.non.energy.input, 
                units, input.cost_adv, year) %>%
  rename(input.cost = input.cost_adv) %>%
  rbind(L225.GlobalTechCost_h2 %>%
          filter(sector.name != "H2 central production"  & 
                   (subsector.name != "forecourt production" | 
                      minicam.non.energy.input == "compression and storage")))
  

# plot summarized by sector-subsector-technology
plot_ly(L225.GlobalTechCost_h2_adv %>%
          group_by(sector.name, subsector.name, technology, year) %>%
          summarize(total.input.cost = sum(input.cost)) %>%
          mutate(plot_label = paste0(sector.name, subsector.name, technology))) %>%
  add_trace(type = "scatter",
            mode = "lines",
            x = ~year,
            y = ~total.input.cost,
            color = ~plot_label)

# plot split also by non energy input
plot_ly(L225.GlobalTechCost_h2_adv %>%
          mutate(plot_label = paste0(sector.name, "-", subsector.name, "-", 
                                     technology, "-", minicam.non.energy.input))) %>%
  add_trace(type = "scatter",
            mode = "lines",
            x = ~year,
            y = ~input.cost,
            color = ~plot_label) %>%
  layout(title = "Advanced")

# compare to previous values
plot_ly(L225.GlobalTechCost_h2 %>%
          mutate(plot_label = paste0(sector.name, "-", subsector.name, "-", 
                                     technology, "-", minicam.non.energy.input))) %>%
  add_trace(type = "scatter",
            mode = "lines",
            x = ~year,
            y = ~input.cost,
            color = ~plot_label) %>%
  layout(title = "Standard")

# ===================================================

# COSTS - stub technology - solar and wind

# for the electrolyzer costs, apply the same methodology as above, but at a
# regional level
stub_tech_cost_electrolyzer <- L225.StubTechCost_h2 %>%
  filter(minicam.non.energy.input == "electrolyzer")
  
# calculate improvement rate till 2040 for each in the standard case
stub_tech_cost_electrolyzer_impvt <- stub_tech_cost_electrolyzer %>%
  pivot_wider(names_from = year, values_from = `input.cost`) %>%
  mutate(impvt_rate_2015_2040 = 1 - (`2040`/`2015`)^(1/5),
         max_impvt_2015_2100 = 1 - `2100` / `2015`,
         adv_impvt_rate = impvt_rate_2015_2040 * adj_impvt_rate_factor,
         adv_max_impvt_2015_2100 = max_impvt_2015_2100 + adj_max_impvt,
         adv_max_impvt_2015_2100 = ifelse(adv_max_impvt_2015_2100 > max_impvt_limit,
                                          max_impvt_limit, 
                                          adv_max_impvt_2015_2100),
         # calculate the actual maximum improved value
         adv_max_impvt_value =  (1 - adv_max_impvt_2015_2100) * `2015`)

# calculate the new values for stub tech electrolyzer costs
stub_tech_cost_electrolyzer_adv <- stub_tech_cost_electrolyzer %>%
  left_join(stub_tech_cost_electrolyzer_impvt %>% 
              rename(cost_2020 = `2020`) %>%
              dplyr::select(c(region, supplysector, subsector, stub.technology, 
                              minicam.non.energy.input, cost_2020, 
                              adv_impvt_rate, adv_max_impvt_2015_2100,
                              adv_max_impvt_value))) %>%
  # calculate new costs - improvement rate ^(years since 2020)/5 * 2020 cost
  # advanced values are same as standard through 2020, adjustment starts in 2025
  mutate(input.cost_adv = case_when(year <= 2020 ~ input.cost,
                                    year > 2020 ~ 
                                      cost_2020*(1 - adv_impvt_rate)^((year - 2020)/5)),
         # if the new calculated cost is such that it exceeds the max improvement, 
         # set to the maximum improved value
         input.cost_adv = ifelse(input.cost_adv < adv_max_impvt_value,
                                 adv_max_impvt_value,
                                 input.cost_adv))


# for the wind turbine and solar panel costs, use the advanced wind and solar 
# assumptions from the electricity files (specifically, 
# L223.GlobalIntTechCapital_elec_adv from zchunk_L223.electricity.R) to input into 
# the standard processing code from zchunk_L225.hydrogen.R - lines 246 to 273.
# substituting L223.GlobalIntTechCapital_elec_adv for L223.GlobalIntTechCapital_elec
# in the code from that section

# Estimate the wind turbine and solar panel related aspects of the costs of direct renewable hydrogen electrolysis
# These are estimated from the capital costs of wind and solar technologies (generic, global), the region-specific
# capacity factors, efficiencies of hydrogen production, and size of the representative hydrogen plant
L225.RenewElec_cost_adv <- L223.GlobalIntTechCapital_elec_adv %>%
  filter(intermittent.technology %in% c("wind", "PV")) %>%
  left_join(L223.GlobalIntTechOMfixed_elec, by = c("sector.name", "subsector.name", "intermittent.technology", "year")) %>%
  mutate(cost_75USD_kW_yr = capital.overnight * fixed.charge.rate + OM.fixed) %>%
  select(subsector.name, intermittent.technology, year, cost_75USD_kW_yr)

L225.RenewElec_eff <- filter(L225.GlobalTechCoef_h2,
                             subsector.name %in% c("solar", "wind") & !grepl("water", minicam.energy.input)) %>%
  mutate(kWh_elec_per_kgH2 = coefficient * CONV_GJ_KGH2 / CONV_KWH_GJ) %>%
  select(subsector.name, year, kWh_elec_per_kgH2)

L225.RenewElec_cost_adv %>%
  left_join_error_no_match(L225.RenewElec_eff, by = c("subsector.name", "year")) %>%
  left_join(L223.StubTechCapFactor_elec,
            by = c("subsector.name" = "subsector", "intermittent.technology" = "stub.technology", "year")) %>%
  mutate(minicam.non.energy.input = if_else(subsector.name == "solar", "solar panels", "wind turbines"),
         output_kgh2_d = if_else(subsector.name == "solar", energy.SOLAR_ELECTROLYSIS_KGH2_D, energy.WIND_ELECTROLYSIS_KGH2_D),
         cost_75USD_kgH2 = cost_75USD_kW_yr * kWh_elec_per_kgH2 * output_kgh2_d / CONV_DAY_HOURS /
           (output_kgh2_d * capacity.factor / CONV_DAYS_YEAR),
         input.cost = cost_75USD_kgH2 / CONV_GJ_KGH2) %>%
  select(region, subsector.name, year, minicam.non.energy.input, input.cost) %>%
  left_join(select(L225.GlobalTechCost_h2, -input.cost), by = c("subsector.name", "year", "minicam.non.energy.input")) %>%
  rename(supplysector = sector.name, subsector = subsector.name, stub.technology = technology) %>%
#  select(LEVEL2_DATA_NAMES[["StubTechCost"]]) ->
  select(c("region", "supplysector", "subsector", "stub.technology", "year", "minicam.non.energy.input", "input.cost")) ->
  L225.StubTechCost_h2_renewables_adv


# combine electrolyzer and other costs
L225.StubTechCost_h2_adv <- rbind(L225.StubTechCost_h2_renewables_adv,
                                  stub_tech_cost_electrolyzer_adv %>% 
                                    dplyr::select(c("region", "supplysector", "subsector", 
                                                    "stub.technology", "year", 
                                                    "minicam.non.energy.input", "input.cost_adv")) %>%
                                    rename(input.cost = input.cost_adv))

# plot standard vs advanced
plot_ly(L225.StubTechCost_h2_adv %>%
          mutate(plot_label = paste0(region, "-", subsector, "-", minicam.non.energy.input))) %>%
  add_trace(type = "scatter",
            mode = "lines+markers",
            x = ~year,
            y = ~input.cost,
            color = ~plot_label,
            symbol = ~subsector) %>%
  layout(title = "Advanced")

plot_ly(L225.StubTechCost_h2_adv %>%
          mutate(plot_label = paste0(region, "-", subsector, "-", minicam.non.energy.input),
                 plot_label_2 = paste0(subsector, "-", minicam.non.energy.input))) %>%
  add_trace(type = "scatter",
            mode = "lines+markers",
            x = ~year,
            y = ~input.cost,
            color = ~plot_label_2,
            name = ~plot_label) %>%
  layout(title = "Advanced")

plot_ly(L225.StubTechCost_h2 %>%
          mutate(plot_label = paste0(region, "-", subsector, "-", minicam.non.energy.input))) %>%
  add_trace(type = "scatter",
            mode = "lines",
            x = ~year,
            y = ~input.cost,
            color = ~plot_label) %>%
  layout(title = "Standard")

plot_ly(L225.StubTechCost_h2 %>%
          mutate(plot_label = paste0(region, "-", subsector, "-", minicam.non.energy.input),
                 plot_label_2 = paste0(subsector, "-", minicam.non.energy.input))) %>%
  add_trace(type = "scatter",
            mode = "lines+markers",
            x = ~year,
            y = ~input.cost,
            color = ~plot_label_2,
            name = ~plot_label) %>%
  layout(title = "Standard")

# ===================================================

# make xml
gcamdata::create_xml("hydrogen_adv_GCAM6p0.xml") %>%
  gcamdata::add_xml_data(L225.StubTechCost_h2_adv, "StubTechCost") %>%
  gcamdata::add_xml_data(L225.GlobalTechCost_h2_adv, "GlobalTechCost") %>%
  gcamdata::run_xml_conversion()

# # SHARE WEIGHTS - adjusting global tech share weights, specifically making the 
# # fossil fuel-based production techs have share weights of 0.25 in 2100 (decline
# # from 1 in 2020 to 0.25 in 2100)
# 
# # 2100 share weight value 
# shrwt_2100_ff_techs <- 0.25
# 
# L225.GlobalTechShrwt_h2_adv <- L225.GlobalTechShrwt_h2 %>%
#   filter(technology %in% c("natural gas steam reforming", "coal chemical")) %>%
#   mutate(share.weight_adv = case_when(year == 2100 ~ shrwt_2100_ff_techs, 
#                                   (year > 2020 & year < 2100) ~ as.double(NA),
#                                   year <= 2020 ~ share.weight))