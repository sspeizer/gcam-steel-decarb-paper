# file to generate the add-on xmls for the share weights and retirements for 
# the steel report - the tech adjustment xml, the tech adjustment no CCS xml,
# the retirement xml, and the adjustments for the reference scenario for BF share weights

# load packages
library(readr)
library(tidyr)
library(dplyr)
library(plotly)
library(gcamdata)

# set directory
dir <- "C:/Users/spei632/Documents/GCAM_industry/R/steel_decarb_paper"
data_dir <- "C:/Users/spei632/Documents/GCAM_industry/data/steel_decarb"

# load GCAM region names
GCAM_region_names <- read_csv(paste0(dir, "/GCAM_region_names.csv"), skip = 6)

# load region OECD mapping
GCAM_region_OECD <- read_csv(paste0(dir, "/mapping_oecd.csv"))

# load steel techs
steel_techs <- read_csv(paste0(dir, "/steel_techs.csv"), skip = 1)

# load scenarios share weights assumptions
subsector_shrwt_adj <- read_csv(paste0(data_dir, "/steel_decarb_subsector_shrwt_adj.csv"), skip = 1)
tech_shrwt_adj <- read_csv(paste0(data_dir, "/steel_decarb_tech_shrwt_adj.csv"), skip = 1)
# load retirement information
retirement_adj <- read_csv(paste0(data_dir, "/steel_decarb_retirement_adj.csv"), skip = 1)
# load reference scenario adjustments to make to base assumptions for a few regions
subsector_shrwt_adj_ref <- read_csv(paste0(data_dir, "/steel_decarb_subsector_shrwt_adj_ref.csv"), skip = 1)

# SUBSECTOR-LEVEL ---------------------

# expand the default values to all regions
shrwt_year_fillout <- 1975

# first for the share weight fillout values
subsector_shrwt_fillout <- tibble(region = GCAM_region_names$region,
                                  supplysector = "iron and steel",
                                  year.fillout = shrwt_year_fillout) %>%
  repeat_add_columns(subsector_shrwt_adj %>%
                       filter(region == "default") %>%
                       dplyr::select(-region)) %>%
  dplyr::select(region, supplysector, subsector, 
                share_weight_fillout_value, year.fillout) %>%
  rename(share.weight = share_weight_fillout_value) %>%
  unique()

# now for the interp rules
subsector_shrwt_interp <- tibble(region = GCAM_region_names$region,
                                 supplysector = "iron and steel",
                                 apply.to = "share-weight") %>%
  repeat_add_columns(subsector_shrwt_adj %>%
                       filter(region == "default") %>%
                       dplyr::select(-region)) %>%
  dplyr::select(region, supplysector, subsector, apply.to, interp_from, 
                interp_to, interp_fxn) %>%
  rename(interpolation.function = interp_fxn, 
         from.year = interp_from, 
         to.year = interp_to)

# adjust regions specified to have different values from the defaults
regions_to_adjust_subsectors <- unique((subsector_shrwt_adj %>% 
                                          filter(region != "default"))$region)

for (curr_region in regions_to_adjust_subsectors) {
  # get this region's data to adjust
  curr_region_data <- subsector_shrwt_adj %>% filter(region == curr_region)
  curr_region_subsectors_to_adj <- curr_region_data$subsector
  
  # adjust for necessary subsectors
  for (curr_subsector in curr_region_subsectors_to_adj) {
    # first fillout
    subsector_shrwt_fillout <- subsector_shrwt_fillout %>%
      filter(!(region == curr_region & subsector == curr_subsector)) %>%
      rbind(tibble(region = curr_region,
                   supplysector = "iron and steel",
                   subsector = curr_subsector,
                   year.fillout = shrwt_year_fillout,
                   share.weight = (curr_region_data %>% 
                                     filter(subsector == curr_subsector))$share_weight_fillout_value) %>%
              unique())
    
    # next interp rule
    subsector_shrwt_interp <- subsector_shrwt_interp %>%
      filter(!(region == curr_region & subsector == curr_subsector)) %>%
      rbind(tibble(region = curr_region,
                   supplysector = "iron and steel",
                   subsector = curr_subsector,
                   apply.to = "share-weight",
                   from.year = (curr_region_data %>% 
                                  filter(subsector == curr_subsector))$interp_from,
                   to.year = (curr_region_data %>% 
                                filter(subsector == curr_subsector))$interp_to,
                   interpolation.function = (curr_region_data %>% 
                                               filter(subsector == curr_subsector))$interp_fxn))
  }
  
}


# TECHNOLOGY-LEVEL -------------------

# first global tech database
global_tech_shrwt <- tech_shrwt_adj %>%
  filter(region == "global_tech") %>%
  dplyr::select(year, subsector, technology, share_weight) %>%
  rename(share.weight = share_weight, subsector.name = subsector) %>%
  mutate(sector.name = "iron and steel")

# regional values
regional_tech_shrwt <- tech_shrwt_adj %>%
  filter(region != "global_tech" & !is.na(share_weight)) %>%
  dplyr::select(region, year, subsector, technology, share_weight) %>%
  rename(share.weight = share_weight) %>%
  mutate(supplysector = "iron and steel")


# GENERATE XML -------------------

gcamdata::create_xml(paste0(data_dir, "/iron_steel_techAdj_v7.xml")) %>%
  gcamdata::add_xml_data(subsector_shrwt_fillout, "SubsectorShrwtFllt") %>%
  gcamdata::add_xml_data(subsector_shrwt_interp, "SubsectorInterp") %>%
  gcamdata::add_xml_data(global_tech_shrwt, "GlobalTechShrwt") %>%
  gcamdata::add_xml_data(regional_tech_shrwt, "TechShrwt") %>%
#  gcamdata::add_xml_data(regional_tech_s_curve, "TechSCurve") %>%
  gcamdata::run_xml_conversion()

# don't forget to add delete = "1" to this XML for the subsectors manually!

# NO CCS XML ---------------------

# this is the same as the standard techAdj xml, but with all the CCS techs
# having share weight of 0
global_tech_shrwt_no_CCS <- global_tech_shrwt %>%
  mutate(share.weight = ifelse(grepl("CCS", technology), 0, share.weight))

regional_tech_shrwt_no_CCS <- regional_tech_shrwt %>%
  mutate(share.weight = ifelse(grepl("CCS", technology), 0, share.weight))

gcamdata::create_xml(paste0(data_dir, "/iron_steel_techAdj_noCCS_v7.xml")) %>%
  gcamdata::add_xml_data(subsector_shrwt_fillout, "SubsectorShrwtFllt") %>%
  gcamdata::add_xml_data(subsector_shrwt_interp, "SubsectorInterp") %>%
  gcamdata::add_xml_data(global_tech_shrwt_no_CCS, "GlobalTechShrwt") %>%
  gcamdata::add_xml_data(regional_tech_shrwt_no_CCS, "TechShrwt") %>%
#  gcamdata::add_xml_data(regional_tech_s_curve, "TechSCurve") %>%
  gcamdata::run_xml_conversion()

# don't forget to add delete = "1" to this XML for the subsectors manually!

# REFERENCE XML ------------------
# first for the share weight fillout values
subsector_shrwt_fillout_ref <- tibble(region = subsector_shrwt_adj_ref$region,
                                  supplysector = "iron and steel",
                                  year.fillout = shrwt_year_fillout,
                                  subsector = subsector_shrwt_adj_ref$subsector,
                                  share.weight = subsector_shrwt_adj_ref$share_weight_fillout_value)

# now for the interp rules
subsector_shrwt_interp_ref <- tibble(region = subsector_shrwt_adj_ref$region,
                                 supplysector = "iron and steel",
                                 apply.to = "share-weight",
                                 subsector = subsector_shrwt_adj_ref$subsector,
                                 interpolation.function = subsector_shrwt_adj_ref$interp_fxn,
                                 from.year = subsector_shrwt_adj_ref$interp_from,
                                 to.year = subsector_shrwt_adj_ref$interp_to)

gcamdata::create_xml(paste0(data_dir, "/iron_steel_ref_adj_v1.xml")) %>%
  gcamdata::add_xml_data(subsector_shrwt_fillout_ref, "SubsectorShrwtFllt") %>%
  gcamdata::add_xml_data(subsector_shrwt_interp_ref, "SubsectorInterp") %>%
  gcamdata::run_xml_conversion()

# don't forget to add delete = "1" to this XML for the subsectors manually!


# RETIREMENT XML -----------------
# make XML to set retirement values to be different for non-OECD regions

# retirement values for non-OECD regions, excluding China
# first get regions to use adjusted retirement values for
non_OECD_no_retirement_adj <- "China"
regions_to_adjust_retirement <- GCAM_region_OECD %>%
  filter(club == "NonOECD" & !(region %in% non_OECD_no_retirement_adj))

# expand retirement curves to relevant regions and all techs
regional_tech_s_curve <- retirement_adj %>%
  mutate(supplysector = "iron and steel") %>%
  repeat_add_columns(regions_to_adjust_retirement) %>%
  dplyr::select(-club) %>%
  repeat_add_columns(steel_techs)

gcamdata::create_xml(paste0(data_dir, "/iron_steel_retirementAdj_v1.xml")) %>%
  gcamdata::add_xml_data(regional_tech_s_curve, "TechSCurve") %>%
  gcamdata::run_xml_conversion()
