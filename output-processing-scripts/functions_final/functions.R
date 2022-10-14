
# functions

parse_output_scenario <- function(df) {
  
  #remove the duplicated headers and columnn names
  df <- df[df$scenario != "scenario",]
  
  #Remove last column with all NA
  df <- df[, !apply(is.na(df), 2, all)]  
  
  #separate scenario name and date
  df <- separate(df, col = "scenario", into = c("scenario", "date"), sep = c(","))
  
  #tidy data
  YEARS <- as.character(intersect(colnames(df), c(1975:2100)))
  df <- gather(df, all_of(YEARS), key = "year", value = "value")
  df <- dplyr::mutate(df, year = as.integer(year))
  
  #remove rows with NA
  df <- na.omit(df)
  
  # remove date column
  df <- select(df, -date)
  
  return (df)
}


parse_vintages <- function(df) {
  df <- separate(df, col =  "technology", into = c("technology", "year"), sep = c(","))
  df <- separate(df, col =  "year", into = c("year", "vintage"), sep = c("="))
  df$year = NULL
  return (df)
}

aggregate_rows <- function(df, filter_var, var_name, filter_group, ...) {
  group_var <- quos(...)
  filter_var <- enquo(filter_var)
  filter_var_name <- quo_name(filter_var)
  df %>%
    filter(!!filter_var %in% filter_group) %>%
    group_by(!!!group_var) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::mutate(!!filter_var_name := !!var_name)
}
