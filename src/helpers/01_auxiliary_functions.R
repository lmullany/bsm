counties_by_state <- function(states) {
  county_to_fips<-data.table::fread("data/Region_to_fips_mapping_dup_fips.csv")
  county_to_fips$countyfips<-str_pad(as.character(county_to_fips$countyfips), width = 5, pad = "0", side = "left")
  pattern <- paste0("^(", paste(states, collapse = "|"), ")_")
  df <- county_to_fips |> filter(str_detect(Region, pattern))  |>
    arrange(Region)
  return(df$Region)
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



read_mobility_adj_mat <- function(path = "data/mobility_adj_mat.csv") {
  am = data.table::fread(path, drop = 1, header=TRUE)
  as.matrix(am, rownames=names(am))
}
read_physical_adj_mat <- function(path = "data/us_county_adjacency.csv") {
  am = data.table::fread(path, drop = 1, header=TRUE)
  as.matrix(am, rownames=names(am))
}







############################################
## PLOTTING TOOLS
############################################
make_timeseries_plots<-function(res_data,date_col = "date", use_prop=FALSE,add_temporal=TRUE,add_rolling=TRUE,add_rescaled=TRUE){
  groups <-res_data |> group_split(countyfips)
  
  plots = list()
  
  for (i in seq_along(groups)) {
    group <- groups[[i]]
    if (use_prop){
      group$target=group$target/group$overall
      group$predicted_median = group$predicted_median/group$overall
      group$predicted_lower = group$predicted_lower/group$overall
      group$predicted_upper = group$predicted_upper/group$overall
      if (add_temporal){
        group$predicted_median_temporal = group$predicted_median_temporal/group$overall
        group$predicted_lower_temporal = group$predicted_lower_temporal/group$overall
        group$predicted_upper_temporal = group$predicted_upper_temporal/group$overall
      }
      if (add_rolling){
        group$rolling_avg_target = group$rolling_avg_target/group$overall
      }
      if (add_rescaled){
        group$rescaled_aggregate_trend = group$rescaled_aggregate_trend/group$overall
      }
    }
    #group_name <- paste0("location_",unique(group$countyfips))
    group_name <- as.character(unique(na.omit(group$region)))
    plt<-ggplot(group, aes(x = .data[[date_col]], y = target)) +
      geom_point(size=0.5) +
      geom_line(aes(y=predicted_median,color='spatio-temporal'),linewidth=0.1) +
      geom_ribbon(aes(ymin=predicted_lower,ymax=predicted_upper, fill='spatio-temporal'),alpha=0.3) + 
      labs(title = group_name, x = "Date", y = ifelse(use_prop==TRUE, "Proportion of Visits", "Count"), fill="", color="") +
      theme_minimal() + 
      theme(legend.position = "bottom")
    
    
    if (add_temporal){
      plt<-plt+
        geom_line(aes(y=predicted_median_temporal,color='temporal'),linewidth=0.1) +
        geom_ribbon(aes(ymin=predicted_lower_temporal,ymax=predicted_upper_temporal, fill='temporal'),alpha=0.3)
    }
    if (add_rolling){
      plt<-plt+
        geom_line(aes(y=rolling_avg_target,color="rolling"),linewidth=0.1)
    }
    if (add_rescaled){
      plt<-plt+
        geom_line(aes(y=rescaled_aggregate_trend,color="aggregate"),linewidth=0.1)
    }
    plt<-plt+scale_color_manual(values = c("spatio-temporal" = "red", "temporal" = "blue","rolling"="green","aggregate"="black"))
    plt<-plt+scale_fill_manual(values = c("spatio-temporal" = "red", "temporal" = "blue"))
    # folder=paste0("figures/figs_",gsub("%20","_",target),"_",gsub("-","",sd),"_to_",gsub("-","",ed),"_",family)
    # if (!dir.exists(folder)) {
    #   dir.create(folder, recursive = TRUE)
    # }
    #ggsave(paste0(folder,"/plot_", group_name, ".png"), plt, width = 8, height = 3)
    plots[[group_name]] <- plt
  }
  
  return(plots)
  
  
}

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
    start_date,end_date,time_resolution,geo_resolution,state_filter=NULL, med_group_sys, categ_info=NULL){
  
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
    if (!is.null(state_filter)){
      counties<-counties_by_state(state_filter)
      print(paste0("Filtering to ", paste0(counties,collapse=", ")))
      
      url <- paste0(url,paste0("&geography=", gsub(" ","%20",tolower(counties)), collapse = ""))
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
  #url<-paste0(url,"&userId=",userid)
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

get_data<-function(sd,ed,time_res,geo_res,state_filter=NULL, med_group_sys, categ_info, profile){
  url_all <- make_table_builder_url(
    start_date=sd,
    end_date=ed,
    time_resolution=time_res,
    geo_resolution=geo_res,
    state_filter=state_filter,
    med_group_sys = med_group_sys
  )
  url_single <- make_table_builder_url(
    start_date=sd,
    end_date=ed,
    time_resolution=time_res,
    geo_resolution=geo_res,
    state_filter=state_filter,
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
