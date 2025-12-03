# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958


counties_by_state <- function(states) {
  county_to_fips<-data.table::fread("data/Region_to_fips_mapping_dup_fips.csv")
  county_to_fips$countyfips<-str_pad(as.character(county_to_fips$countyfips), width = 5, pad = "0", side = "left")
  pattern <- paste0("^(", paste(states, collapse = "|"), ")_")
  df <- county_to_fips |> filter(str_detect(Region, pattern))  |>
    arrange(Region)
  return(df$Region)
}

counties_from_fips <- function(fips) {
  county_to_fips<-data.table::fread("data/Region_to_fips_mapping_dup_fips.csv")
  county_to_fips$countyfips<-str_pad(as.character(county_to_fips$countyfips), width = 5, pad = "0", side = "left")
  county_to_fips[CJ(countyfips = fips), on="countyfips", Region]
}

add_fips<-function(data){
  county_to_fips<-data.table::fread("data/Region_to_fips_mapping_dup_fips.csv")
  county_to_fips$countyfips<-str_pad(as.character(county_to_fips$countyfips), width = 5, pad = "0", side = "left")
  county_to_fips<-county_to_fips|> rename(region="Region")
  data<-inner_join(data,county_to_fips,by="region")
  return(data)

}


 
# function takes data frame and string date column name
# and returns frame with that column replaced with the
# the date. Speed up, (relative to using sapply on
# the above function) grows with bigger datasets.
wk_to_date <- function(df, date_col) {
  d = df$date |> unique()
  ndf = names(df)
  dlu = data.table(
    sapply(str_split(d,"-",), \(f) MMWRweek2Date(as.numeric(f[1]), as.numeric(f[2]),1)) |> as.IDate(),
    d
  ) |> setnames(new=c("_x", date_col))
  df = df[dlu, on=c(date_col)]
  df[, .SD, .SDcols = c("_x", ndf[-1])] |> setnames(new=ndf)
}

## LOAD ADJACENCY MATRICES
load_adj_matrix <- function(path) {
  if(tools::file_ext(path) == "csv") {
    return(read_adj_matrix_from_csv(path))
  }
  else if(tools::file_ext(path) == "rds") {
    return(readRDS(path))
  }
  else {
    cli::cli_abort("Only `rds` or `csv` allowed")
  }
}

read_adj_matrix_from_csv <- function(path) {
  am = data.table::fread(path, drop = 1, header=TRUE)
  as.matrix(am, rownames=names(am))
}

# read_mobility_adj_mat <- function(path = "data/mobility_adj_mat.csv") {
#   am = data.table::fread(path, drop = 1, header=TRUE)
#   as.matrix(am, rownames=names(am))
# }
# read_physical_adj_mat <- function(path = "data/us_county_adjacency.csv") {
#   am = data.table::fread(path, drop = 1, header=TRUE)
#   as.matrix(am, rownames=names(am))
# }


############################################
## ESSENCE QUERY TOOLS
############################################


get_county_codes <- function(){
  file_path<-"data/fips.json"
  # Load required package
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required. Please install it with install.packages('jsonlite').")
  }
  
  # Read and parse the JSON file
  json_data <- jsonlite::fromJSON(file_path)
  
  # Extract the "values" list and convert to data frame
  df <- as.data.frame(json_data$values, stringsAsFactors = FALSE)
  
  return(df)
}


make_table_builder_url<-function(
    start_date,end_date,time_resolution,geo_resolution,state_filter=NULL,county_filter=NULL,med_group_sys, categ_info=NULL){
  
  base_url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?"
  start_date<-format(as.Date(start_date), "%d%b%Y")
  end_date<-format(as.Date(end_date), "%d%b%Y")
  url<-paste0(base_url,"endDate=",end_date,"&startDate=",start_date)
  
  url<-paste0(url,"&aqtTarget=TableBuilder")
  url<-paste0(url,"&datasource=va_er")
  url<-paste0(url,"&detector=nodetectordetector")
  
  if (time_resolution=="daily"){
    url<-paste0(url,"&timeResolution=daily")
  } else if (time_resolution == "weekly"){
    url<-paste0(url,"&timeResolution=weekly")
  } else if (time_resolution == "monthly"){
    url<-paste0(url,"&timeResolution=monthly")
  } else if (time_resolution == "yearly"){
    url<-paste0(url,"&timeResolution=yearly")
  } else {
    stop(paste0("invalid time resoution:",time_resolution))
  }
  url<-paste0(url,"&rowFields=timeResolution")
  
  if (geo_resolution =="county"){
    url<-paste0(url,"&geographySystem=region")
    url<-paste0(url,"&columnField=geographyregion")
    
    if (!is.null(state_filter)) {
      if(is.null(county_filter)) {
        county_filter <- counties_by_state(state_filter)
      } else {
        county_filter <- counties_from_fips(county_filter)
      }
      print(paste0("Filtering to ", paste0(county_filter,collapse=", ")))
      url <- paste0(url,paste0("&geography=", gsub(" ","%20",tolower(county_filter)), collapse = ""))
    } 
  } else if (geo_resolution =="state"){
    url<-paste0(url,"&geographySystem=state")
    url<-paste0(url,"&columnField=geographystate")
    if (!is.null(state_filter)){
      #print(paste0("Filtering to ", paste0(state_filter,collapse=", ")))
      url <- paste0(url,paste0("&geography=", tolower(state_filter), collapse = ""))
    }
  } else {
    stop(paste0("invalid geo resoution:",geo_resolution))
  }
  
  url<-paste0(url,"&medicalGroupingSystem=", med_group_sys)
  
  if (!is.null(categ_info)){

    url<-paste0(url,"&", categ_info[["cat_class"]], "=",categ_info[["cat_value"]])
  }
  return(url)
}

reshape_and_join <- function(df_single, df_all) {
  df_single_long <- df_single |>
    pivot_longer(-timeResolution, names_to = "region", values_to = "target")
  
  df_all_long <- df_all |>
    pivot_longer(-timeResolution, names_to = "region", values_to = "overall")
  
  df_joined <- df_single_long |>
    inner_join(df_all_long, by = c("timeResolution", "region")) |>
    rename("date"="timeResolution")
  
  return(df_joined)
}

reshape_and_join_dt <- function(df_single, df_all) {
  merge(
    melt(df_single, id="timeResolution", value.name="target", variable.name="region"),
    melt(df_all, id="timeResolution", value.name="overall", variable.name="region"),
    by=c("timeResolution", "region")
  ) |> setnames(new= c("date", "region", "target", "overall"))
}

get_data<-function(sd,ed,time_res,geo_res,state_filter=NULL,county_filter, med_group_sys, categ_info, profile){
  url_all <- make_table_builder_url(
    start_date=sd,
    end_date=ed,
    time_resolution=time_res,
    geo_resolution=geo_res,
    state_filter=state_filter,
    county_filter=county_filter,
    med_group_sys = med_group_sys
  )
  url_single <- make_table_builder_url(
    start_date=sd,
    end_date=ed,
    time_resolution=time_res,
    geo_resolution=geo_res,
    state_filter=state_filter,
    county_filter=county_filter,
    med_group_sys = med_group_sys,
    categ_info = categ_info
  )
  # Data Pull from ESSENCE
  data_all <- get_api_data(url_all, fromCSV = TRUE, profile=profile)
  data_single <- get_api_data(url_single, fromCSV = TRUE, profile=profile)
  #merged<-reshape_and_join(df_single=data_single,df_all=data_all)
  setDT(data_all)
  setDT(data_single)
  merged <- reshape_and_join_dt(data_single, data_all)
  
  return(list(data = merged, url_all = url_all, url_single = url_single))
}

map_table_names_to_display <- function(names, title_case = FALSE) {
  map = list(
    "Date" = c("date"),
    "Region" = c("region"),
    "County FIPS" = c("countyfips"),
    "ED Visits (Target)" = c("target", "cases"),
    "ED Visits (Overall)" = c("overall"),
    "ED Visits (Expected)" = c("expected"), 
    "Denominator Source" = c("denominator_source")
  )
  # convert map to datatable for fast lookup via join
  map = rbindlist(lapply(map, data.table), id="display_name")
  # join, but retain order of initial names, so that we can return in that order
  result = map[data.table(V1=names)[, i:=.I], on="V1"]
  # return display name only
  result = result[is.na(display_name), display_name:=V1][order(i), display_name]
  
  # any names that have underscores should be converte
  result = sapply(result, \(r) gsub("_", " ", r) |> tools::toTitleCase(),USE.NAMES = FALSE)
  
  # convert to title case if requested (this is mainly useful for other columns)
  if(title_case) result = tools::toTitleCase(result)
  
  result
}


# Given a data frame, identify which columns are those that should be 
# can be rounded, and which can be left as is because are integer. This
# is useful for feeding to formatRound() in DT. Will return column indices;
# set names to TRUE to get names instead of indices. Note that indices are 
# useful, because the colnames() parameter might have been used on the DT before
# formatRound() call, and so in this case it is better to use indices. 
non_integer_cols_to_round <- function(d, names=FALSE) {
  
  is_integer_vector <- \(x) {
    all(x==floor(x)) && all(abs(x)<=.Machine$integer.max)
  }

  cols_to_convert <- which(sapply(d, \(col) is.numeric(col) && !is_integer_vector(col), USE.NAMES = FALSE))
  if(names) cols_to_convert <- names(d)[cols_to_convert]
  
  cols_to_convert
}


load_saved_model_file <- function(path) {

  # unpack the archive
  archive <- load_saved_object_from_file(path)
  
  # return a list of objects (model object, model values)
  return(list(
    "model_object" = archive[["rds"]],
    "model_vals" = archive[["json"]]
  ))
  
}

load_saved_query_file <- function(path) {
  
  # unpack the archive
  archive <- load_saved_object_from_file(path)
  
  # return a list of objects (data, query_values)
  return(list(
    "data" = archive[["rds"]],
    "query_values" = archive[["json"]]
  ))
  
}

# Given a path to a bsm_model zip file (created from this app),
# read to temp location, unzip, and return
load_saved_object_from_file <- function(path) {
  # create temp folder
  tmpdir <- tempfile()
  dir.create(tmpdir)
  
  # unizip the path to the tempdir
  unzip(path, exdir = tmpdir)
  
  # get files from unzipped archive
  files <- list.files(tmpdir, full.names = TRUE)
  
  # identify the rds file (model object) and json file (model values)
  rds_file  <- files[grepl("\\.rds$", files, ignore.case = TRUE)]
  json_file <- files[grepl("\\.json$", files, ignore.case = TRUE)]
  
  # ensure that unzipped archive contains the expected files
  validate(
    need(length(rds_file) > 0, "No RDS file found in zip"),
    need(length(json_file) > 0, "No JSON file found in zip")
  )
  return(list(
    rds = readRDS(rds_file[1]),
    json=jsonlite::read_json(json_file[1], simplifyVector = TRUE)
  ))
  
}
  
# # Function to place info circle tool tip on a input label
# # l must be a two element list, first element holds the label
# # 2nd element holds the tool tip message
labeltt <- function(l, ...) {
  tt <- bslib::tooltip(
    trigger = list(
      l[[1]],
      bsicons::bs_icon(name = "info-circle-fill", class = "text-primary")
    ),
    p(l[[2]], style = "text-align:left;"),
    ...
  )
  
  # lets convert the result of tooltip function to character/HTML
  tt #htmltools::HTML(as.character(tt))
}

add_button_hover <- function(title,button) {
  div(title=title,button)
}
