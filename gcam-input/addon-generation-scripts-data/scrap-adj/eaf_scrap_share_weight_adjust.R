# file to generate the add-on xml to reduce the scrap share weights for the no CCS
# scenarios to ensure that scrap-based production does not exceed the global 
# scrap limit

# load packages
library(readr)
library(tidyr)
library(dplyr)
library(plotly)
library(gcamdata)

# set directory
dir <- "C:/Users/spei632/Documents/GCAM_industry/R/steel_decarb_paper"
data_dir <- "C:/Users/spei632/Documents/GCAM_industry/data/steel_decarb"

# load scrap share weights without adjustment, from an output no CCS scenario run
scrap_shrwts <- read_csv(paste0(data_dir, "/1p5_no_CCS_scrap_shareweights_steel_decarb_run_9-15-22_regional_costs_TZ_2019.csv"))

# adjustment factor to apply
adj_factor <- 0.5
shrwt_year_fillout <- 1975
interp_fxn <- "linear"
interp_from <- 2015
interp_to = 2100

# apply the adjustment to the 2100 share weights
scrap_shrwts_adj <- scrap_shrwts %>% 
  dplyr::select(region, sector, subsector, `2100`) %>%
  mutate(share.weight = `2100` * adj_factor,
         year.fillout = shrwt_year_fillout) %>%
  rename(supplysector = sector) %>%
  dplyr::select(-`2100`)

# also make interpolation rules for them
scrap_interp_adj <- scrap_shrwts_adj %>%
  dplyr::select(region, supplysector, subsector) %>%
  mutate(apply.to = "share-weight",
         interpolation.function = interp_fxn,
         from.year = interp_from,
         to.year = interp_to)

# generate XML 
gcamdata::create_xml(paste0(data_dir, "/iron_steel_scrap_adj_for_no_CCS_v1.xml")) %>%
  gcamdata::add_xml_data(scrap_shrwts_adj, "SubsectorShrwtFllt") %>%
  gcamdata::add_xml_data(scrap_interp_adj, "SubsectorInterp") %>%
  gcamdata::run_xml_conversion()

# ADD DELETE = 1 TO THE RESULTING XML!