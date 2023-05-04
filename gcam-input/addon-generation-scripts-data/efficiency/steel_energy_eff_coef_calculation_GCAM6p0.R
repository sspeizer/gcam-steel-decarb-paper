# file to generate an XML for the updated energy efficiency assumptions for iron
# and steel technologies for the steel decarbonization paper, as well as an XML
# add-on that modifies the coefficients for biomass-based BLASTFUR for 2020
# onwards to be continuation of their 2015 values (for use in the reference
# scenario)

library(tidyr)
library(dplyr)
library(xml2)
library(readr)
library(plotly)

setwd('C:/Users/spei632/Documents/GCAM_industry/R/steel_decarb_paper')

# LOAD DATA
# IEA best available technology data, from 
# https://www.iea.org/articles/driving-energy-efficiency-in-heavy-industries
iea_bat <- read_csv("efficiency_calcs/iea_steel_bat.csv")

# World Best Practice technology data, from Worrell et al. 2008
# https://eta-publications.lbl.gov/sites/default/files/industrial_best_practice_en.pdf
# the world best practices values, converting to GJ/kg
wbp_bf_bof <- 14.8 / 1000
wbp_eaf_dri <- 16.9 / 1000
wbp_eaf_scrap <- 2.6 / 1000
wbp_cast_roll <- 0.2 / 1000 # casting and rolling lowest intensity possible (GJ/kg)

# regional coefficients from the detailed industry branch
coef <- read_csv("efficiency_calcs/L2323.StubTechCoef_iron_steel.csv")
# global tech database coefficients from the detailed industry branch
coef_gtech <- read_csv("efficiency_calcs/L2323.GlobalTechCoef_iron_steel.csv")

# OBTAIN BAT ESTIMATES
# as the BAT estimates, we use the world best practices value for BLASTFUR and 
# EAF with DRI. for EAF with scrap, we use the IEA estimate and add the world
# best practices value for the energy intensity of casting and rolling, as this
# is not included in the IEA data
blastfur_bat_est <- wbp_bf_bof
eaf_dri_bat_est <- wbp_eaf_dri
eaf_scrap_bat_est <- (iea_bat %>% 
                        # divide by 1000 to convert to GJ/kg
                        filter(tech == "Scrap-based EAF"))$intensity[1]/1000 + wbp_cast_roll

# PROCESS COEFFICIENT DATA
# find which values are not present in the regional values, that we need to
# pull from the global tech database to calculate the total coefficient accurately

# first need to add region names to global tech database and expand appropriately
region_names <- unique(coef$region)
coef_gtech_expanded <- c()
for (i in 1:length(region_names)) {
  coef_gtech_expanded <- coef_gtech_expanded %>%
    bind_rows(coef_gtech %>%
                mutate(region = region_names[i],
                       market.name = region_names[i]))
}

# join global tech and regional values, and define the coefficient for each fuel 
# as the regional value if it exists and the global tech value if it does not
coef_edit <- coef %>% 
  rename(coefficient_regional = coefficient) %>% 
  full_join(coef_gtech_expanded %>% 
              rename(coefficient_gtech = coefficient, 
                     stub.technology = technology,
                     subsector = subsector.name,
                     supplysector = sector.name)) %>%
  mutate(coefficient = ifelse(is.na(coefficient_regional), 
                              coefficient_gtech, 
                              coefficient_regional))

# summarize to get the total coefficient aggregated across all energy inputs
# exclude scrap from the computation
coef_total <- coef_edit %>%
  filter(minicam.energy.input != "scrap") %>%
  group_by(region, subsector, stub.technology, year) %>%
  summarize(total_coefficient = sum(coefficient, na.rm = TRUE)) 
# need to remove NAs which are present if that fuel isn't used in that tech

# OBTAIN TIME SERIES OF COEFFICIENTS FOR ENERGY EFFICIENCY ASSUMPTIONS
# we will assume that regional coefficients scale down from their values in 2015 
# to converge at the BAT coefficients, or a coefficient that is 20% more efficient
# than their historical values, whichever is higher (least efficient). the scaling
# down happens linearly from 2020-2050, with 2020 coefficients the same as 2015.
# for regions that have 2015 coefficients that are already lower (i.e., more 
# efficient) than the BAT values, we will assume that their coefficients stay
# constant in the future.
# for all regions, coefficients are then constant for 2050 onwards.
# note that I assume that the relative contributions of each type of fuel to the
# overall coefficient remain constant (i.e., the 2015 proportions are maintained
# into the future) while the overall total coefficient scales down to the BAT 
# value.
# also note that I assume that the overall efficiency achievable with the 
# hydrogen and CCS technologies is the same as the standard methods.

# efficiency increase fraction
eff_inc_frac <- 0.2

# get the values for 2015 for all regions and technologies, and exclude scrap
coef_2015 <- coef_edit %>%
  filter(year == 2015 & minicam.energy.input != "scrap") %>%
  # join with the total coefficient data (summarized over all fuels) to get the 
  # fraction of the total energy use that each fuel represents
  left_join(coef_total %>% filter(year == 2015)) %>%
  mutate(fuel_frac_of_total_coef = coefficient / total_coefficient) %>%
  # if the total coefficient is 0, replace with the total coefficient from the 
  # global technology database and use the global tech database coefficient
  # breakdowns as well
  left_join(coef_gtech %>% 
              rename(stub.technology = technology,
                     subsector = subsector.name,
                     supplysector = sector.name) %>% 
              filter(year == 2015 & minicam.energy.input != "scrap") %>%
              group_by(stub.technology, year, subsector) %>%
              summarize(total_coefficient_gtech = sum(coefficient, 
                                                      na.rm = TRUE)) %>%
              ungroup()) %>%
  mutate(total_coefficient_edit = ifelse(total_coefficient == 0, 
                                         total_coefficient_gtech, 
                                         total_coefficient),
         fuel_frac_of_total_coef = ifelse(total_coefficient == 0, 
                                          coefficient_gtech / total_coefficient_gtech, 
                                          fuel_frac_of_total_coef)) %>%
  # exclude fuels not used for a particular technology
  filter(!is.na(coefficient)) %>%
  # add column for the BAT estimate
  mutate(bat_coefficient = case_when(subsector == "BLASTFUR" ~ blastfur_bat_est,
                                     subsector == "EAF with DRI" ~ eaf_dri_bat_est,
                                     subsector == "EAF with scrap" ~ eaf_scrap_bat_est),
         # calculate the 20% efficiency increase coefficient
         eff_inc_coef = total_coefficient_edit * (1 - eff_inc_frac),
         # also add column for what the terminal (total) coefficient is - if 
         # the 2015 coefficient is less than the BAT estimate, use that, 
         # otherwise if the coefficient with the efficiency increase is greater
         # than the BAT estimate, use that, and otherwise use the BAT estimate
         terminal_total_coefficient = case_when(total_coefficient_edit <= bat_coefficient ~ 
                                                  total_coefficient_edit, 
                                                eff_inc_coef >= bat_coefficient ~ eff_inc_coef,
                                                TRUE ~ bat_coefficient)) %>%
  dplyr::select(region, supplysector, subsector, stub.technology,
                minicam.energy.input, market.name, year, coefficient, 
                total_coefficient_edit, fuel_frac_of_total_coef, 
                terminal_total_coefficient)

# expand to all years
# get all future years
future_years <- unique((coef %>% filter(year >= 2020))$year)
coef_future_bat <- coef_2015
# expand
for (yr in future_years) {
  coef_future_bat <- coef_future_bat %>%
    bind_rows(coef_2015 %>%
                mutate(year = yr,
                       # set the coefficient and total coefficient to be NA for
                       # future periods (except 2020, which we assume take the
                       # 2015 values)
                       coefficient = ifelse(year > 2020, NA,
                                            coefficient),
                       total_coefficient_edit = ifelse(year > 2020, NA,
                                                       total_coefficient_edit)))
}

# set the coefficient in 2050 and beyond to be the terminal coefficient 
# (either BAT estimate or the 2015 coefficient, if better than the BAT estimate), 
# and then linearly interpolate for the years in between
coef_future_bat_full <- coef_future_bat %>%
  mutate(eff_total_coefficient = ifelse(year >= 2050, 
                                        terminal_total_coefficient, 
                                        total_coefficient_edit),
         # coefficient for each fuel is the fraction of the total coefficient 
         # that comes from that fuel (using 2015 ratios)
         eff_fuel_coefficient = eff_total_coefficient * fuel_frac_of_total_coef) %>%
  arrange(region, subsector, stub.technology, minicam.energy.input, year) %>%
  group_by(region, subsector, stub.technology, minicam.energy.input) %>%
  mutate(eff_fuel_coefficient_final = approx(year, eff_fuel_coefficient, 
                                             xout = year, rule = 2)$y)

# aggregate to get total coefficients
coef_future_bat_summarized <- coef_future_bat_full %>%
  group_by(region, subsector, stub.technology, year) %>%
  summarize(eff_total_coefficient_final = sum(eff_fuel_coefficient_final, 
                                              na.rm = TRUE))

# set the final values with appropriate column labels
final_eff_coef_regional <- coef_future_bat_full %>%
  dplyr::select(region, supplysector, subsector, stub.technology, 
                minicam.energy.input, market.name, year, 
                eff_fuel_coefficient_final) %>%
  rename(coefficient = eff_fuel_coefficient_final)

# PLOTS OF RESULTS
plot_ly(coef_future_bat_summarized %>% filter(stub.technology == "BLASTFUR" & 
                                                year <= 2055)) %>%
  add_trace(type = "scatter",
            mode = "markers",
            x = ~year,
            y = ~eff_total_coefficient_final,
            color = ~region,
            legendgroup = ~region,
            showlegend = TRUE) %>%
  add_trace(type = "scatter",
            mode = "lines",
            x = ~year,
            y = ~eff_total_coefficient_final,
            color = ~region,
            legendgroup = ~region,
            showlegend = FALSE) %>%
  # add line for BAT for reference
  add_trace(type = "scatter",
            mode = "lines",
            x = ~year,
            y = blastfur_bat_est,
            color = I("black"),
            name = "BAT estimate",
            line = list(dash = "dash")) %>%
  layout(yaxis = list(title = "Coefficient, GJ/kg", rangemode = "tozero"),
         xaxis = list(title = "Year"),
         annotations = list(text = "BLASTFUR",
                            x = 0.5,
                            y = 1,
                            xanchor = "center",
                            showarrow = FALSE,
                            xref = "paper",
                            yref = "paper"))

plot_ly(coef_future_bat_summarized %>% filter(stub.technology == "BLASTFUR CCS" & 
                                                year <= 2055)) %>%
  add_trace(type = "scatter",
            mode = "markers",
            x = ~year,
            y = ~eff_total_coefficient_final,
            color = ~region,
            legendgroup = ~region,
            showlegend = TRUE) %>%
  add_trace(type = "scatter",
            mode = "lines",
            x = ~year,
            y = ~eff_total_coefficient_final,
            color = ~region,
            legendgroup = ~region,
            showlegend = FALSE) %>%
  # add line for BAT for reference
  add_trace(type = "scatter",
            mode = "lines",
            x = ~year,
            y = blastfur_bat_est,
            color = I("black"),
            name = "BAT estimate",
            line = list(dash = "dash")) %>%
  layout(yaxis = list(title = "Coefficient, GJ/kg", rangemode = "tozero"),
         xaxis = list(title = "Year"),
         annotations = list(text = "BLASTFUR CCS",
                            x = 0.5,
                            y = 1,
                            xanchor = "center",
                            showarrow = FALSE,
                            xref = "paper",
                            yref = "paper"))

plot_ly(coef_future_bat_summarized %>% filter(stub.technology == "Biomass-based" &
                                                year <= 2055)) %>%
  add_trace(type = "scatter",
            mode = "markers",
            x = ~year,
            y = ~eff_total_coefficient_final,
            color = ~region,
            legendgroup = ~region,
            showlegend = TRUE) %>%
  add_trace(type = "scatter",
            mode = "lines",
            x = ~year,
            y = ~eff_total_coefficient_final,
            color = ~region,
            legendgroup = ~region,
            showlegend = FALSE) %>%
  # add line for BAT for reference
  add_trace(type = "scatter",
            mode = "lines",
            x = ~year,
            y = blastfur_bat_est,
            color = I("black"),
            name = "BAT estimate",
            line = list(dash = "dash")) %>%
  layout(yaxis = list(title = "Coefficient, GJ/kg", rangemode = "tozero"),
         xaxis = list(title = "Year"),
         annotations = list(text = "BLASTFUR, biomass-based",
                            x = 0.5,
                            y = 1,
                            xanchor = "center",
                            showarrow = FALSE,
                            xref = "paper",
                            yref = "paper"))

plot_ly(coef_future_bat_summarized %>% filter(stub.technology == "BLASTFUR with hydrogen" & 
                                                year <= 2055)) %>%
  add_trace(type = "scatter",
            mode = "markers",
            x = ~year,
            y = ~eff_total_coefficient_final,
            color = ~region,
            legendgroup = ~region,
            showlegend = TRUE) %>%
  add_trace(type = "scatter",
            mode = "lines",
            x = ~year,
            y = ~eff_total_coefficient_final,
            color = ~region,
            legendgroup = ~region,
            showlegend = FALSE) %>%
  # add line for BAT for reference
  add_trace(type = "scatter",
            mode = "lines",
            x = ~year,
            y = blastfur_bat_est,
            color = I("black"),
            name = "BAT estimate",
            line = list(dash = "dash")) %>%
  layout(yaxis = list(title = "Coefficient, GJ/kg", rangemode = "tozero"),
         xaxis = list(title = "Year"),
         annotations = list(text = "BLASTFUR with hydrogen",
                            x = 0.5,
                            y = 1,
                            xanchor = "center",
                            showarrow = FALSE,
                            xref = "paper",
                            yref = "paper"))

plot_ly(coef_future_bat_summarized %>% filter(stub.technology == "EAF with DRI" & 
                                                year <= 2055)) %>%
  add_trace(type = "scatter",
            mode = "markers",
            x = ~year,
            y = ~eff_total_coefficient_final,
            color = ~region,
            legendgroup = ~region,
            showlegend = TRUE) %>%
  add_trace(type = "scatter",
            mode = "lines",
            x = ~year,
            y = ~eff_total_coefficient_final,
            color = ~region,
            legendgroup = ~region,
            showlegend = FALSE) %>%
  # add line for BAT for reference
  add_trace(type = "scatter",
            mode = "lines",
            x = ~year,
            y = eaf_dri_bat_est,
            color = I("black"),
            name = "BAT estimate",
            line = list(dash = "dash")) %>%
  layout(yaxis = list(title = "Coefficient, GJ/kg", rangemode = "tozero"),
         xaxis = list(title = "Year"),
         annotations = list(text = "EAF with DRI",
                            x = 0.5,
                            y = 1,
                            xanchor = "center",
                            showarrow = FALSE,
                            xref = "paper",
                            yref = "paper"))

plot_ly(coef_future_bat_summarized %>% filter(stub.technology == "EAF with DRI CCS" & 
                                                year <= 2055)) %>%
  add_trace(type = "scatter",
            mode = "markers",
            x = ~year,
            y = ~eff_total_coefficient_final,
            color = ~region,
            legendgroup = ~region,
            showlegend = TRUE) %>%
  add_trace(type = "scatter",
            mode = "lines",
            x = ~year,
            y = ~eff_total_coefficient_final,
            color = ~region,
            legendgroup = ~region,
            showlegend = FALSE) %>%
  # add line for BAT for reference
  add_trace(type = "scatter",
            mode = "lines",
            x = ~year,
            y = eaf_dri_bat_est,
            color = I("black"),
            name = "BAT estimate",
            line = list(dash = "dash")) %>%
  layout(yaxis = list(title = "Coefficient, GJ/kg", rangemode = "tozero"),
         xaxis = list(title = "Year"),
         annotations = list(text = "EAF with DRI CCS",
                            x = 0.5,
                            y = 1,
                            xanchor = "center",
                            showarrow = FALSE,
                            xref = "paper",
                            yref = "paper"))

plot_ly(coef_future_bat_summarized %>% filter(stub.technology == "Hydrogen-based DRI" & 
                                                year <= 2055)) %>%
  add_trace(type = "scatter",
            mode = "markers",
            x = ~year,
            y = ~eff_total_coefficient_final,
            color = ~region,
            legendgroup = ~region,
            showlegend = TRUE) %>%
  add_trace(type = "scatter",
            mode = "lines",
            x = ~year,
            y = ~eff_total_coefficient_final,
            color = ~region,
            legendgroup = ~region,
            showlegend = FALSE) %>%
  # add line for BAT for reference
  add_trace(type = "scatter",
            mode = "lines",
            x = ~year,
            y = eaf_dri_bat_est,
            color = I("black"),
            name = "BAT estimate",
            line = list(dash = "dash")) %>%
  layout(yaxis = list(title = "Coefficient, GJ/kg", rangemode = "tozero"),
         xaxis = list(title = "Year"),
         annotations = list(text = "Hydrogen-based DRI",
                            x = 0.5,
                            y = 1,
                            xanchor = "center",
                            showarrow = FALSE,
                            xref = "paper",
                            yref = "paper"))

plot_ly(coef_future_bat_summarized %>% filter(stub.technology == "EAF with scrap" &
                                                year <= 2055)) %>%
  add_trace(type = "scatter",
            mode = "markers",
            x = ~year,
            y = ~eff_total_coefficient_final,
            color = ~region,
            legendgroup = ~region,
            showlegend = TRUE) %>%
  add_trace(type = "scatter",
            mode = "lines",
            x = ~year,
            y = ~eff_total_coefficient_final,
            color = ~region,
            legendgroup = ~region,
            showlegend = FALSE) %>%
  # add line for BAT for reference
  add_trace(type = "scatter",
            mode = "lines",
            x = ~year,
            y = eaf_scrap_bat_est,
            color = I("black"),
            name = "BAT estimate",
            line = list(dash = "dash")) %>%
  layout(yaxis = list(title = "Coefficient, GJ/kg", rangemode = "tozero"),
         xaxis = list(title = "Year"),
         annotations = list(text = "EAF with scrap",
                            x = 0.5,
                            y = 1,
                            xanchor = "center",
                            showarrow = FALSE,
                            xref = "paper",
                            yref = "paper"))


# ADJUST GLOBAL TECH COEFFICIENTS
# also need to apply the same assumptions to the global technology database
# values for the coefficients
# note that the global tech coefficients for BLASTFUR CCS and hydrogen-based DRI
# are already lower than the BAT coefficients for BLASTFUR and EAF-DRI, so I
# just continue the global tech values from 2015 for those technologies into the
# future, with no modifications

# first calculate overall global tech coefficients for each technology
coef_total_gtech <- coef_gtech %>%
  filter(minicam.energy.input != "scrap") %>%
  group_by(subsector.name, technology, year) %>%
  summarize(total_coefficient = sum(coefficient, na.rm = TRUE))

# join to global tech full data
coef_future_bat_gtech <- coef_gtech %>%
  filter(minicam.energy.input != "scrap" & year >= 2015) %>%
  left_join(coef_total_gtech) %>%
  mutate(fuel_frac_of_total_coef = coefficient / total_coefficient) %>%
  # add column for the BAT estimate
  mutate(bat_coefficient = case_when(subsector.name == "BLASTFUR" ~ blastfur_bat_est,
                                     subsector.name == "EAF with DRI" ~ eaf_dri_bat_est,
                                     subsector.name == "EAF with scrap" ~ eaf_scrap_bat_est),
         # calculate the 20% efficiency increase coefficient
         eff_inc_coef = total_coefficient * (1 - eff_inc_frac),
         # also add column for what the terminal (total) coefficient is - if 
         # the 2015 coefficient is less than the BAT estimate, use that, 
         # otherwise if the coefficient with the efficiency increase is greater
         # than the BAT estimate, use that, and otherwise use the BAT estimate
         terminal_total_coefficient = case_when(total_coefficient <= bat_coefficient ~ 
                                                  total_coefficient, 
                                                eff_inc_coef >= bat_coefficient ~ eff_inc_coef,
                                                TRUE ~ bat_coefficient),
         # set efficient coefficient to be NA for future periods beyond 2020
         eff_coefficient = ifelse(year > 2020, NA, coefficient),
         # set the efficient total coefficient to be historical values through
         # 2020, NA between 2025-2050 and the terminal coefficient after 2050
         eff_total_coefficient = case_when(year <= 2020 ~ total_coefficient,
                                           year >= 2050 ~ terminal_total_coefficient),
         # coefficient for each fuel is the fraction of the total coefficient 
         # that comes from that fuel
         eff_coefficient = eff_total_coefficient * fuel_frac_of_total_coef) %>%
  arrange(subsector.name, technology, minicam.energy.input, year) %>%
  # interpolate
  group_by(subsector.name, technology, minicam.energy.input) %>%
  mutate(eff_coefficient_final = approx(year, eff_coefficient, 
                                        xout = year, rule = 2)$y)


# aggregate to get total coefficients
coef_future_bat_gtech_summarized <- coef_future_bat_gtech %>%
  group_by(subsector.name, technology, year) %>%
  summarize(eff_total_coefficient_final = sum(eff_coefficient_final, 
                                              na.rm = TRUE))

# set the final values with appropriate column labels
final_eff_coef_gtech <- coef_future_bat_gtech %>%
  dplyr::select(sector.name, subsector.name, technology, year,
                minicam.energy.input, eff_coefficient_final) %>%
  rename(coefficient = eff_coefficient_final)

# PRODUCE EFFICIENCY ASSUMPTIONS ADD ON XML
gcamdata::create_xml("efficiency_calcs/steel_efficiency_coefs_bat_2050_or_20pct_improvement_GCAM6p0_final.xml") %>%
  gcamdata::add_xml_data(final_eff_coef_regional, "StubTechCoef")%>%
  gcamdata::add_xml_data(final_eff_coef_gtech, "GlobalTechCoef") %>%
  gcamdata::run_xml_conversion()
