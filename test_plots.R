# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958


load("test_data_md.RData")
library(dplyr)

#' Round a Number Up to a "Nice" Threshold with Slack
#'
#' Rounds a numeric value upward to a human-friendly threshold based on order-of-magnitude rounding.
#' The result is guaranteed to be greater than or equal to \code{x}, but avoids overly coarse bounds
#' by recursively tightening the rounding scale when the gap between the bound and \code{x} exceeds a defined slack.
#'
#' @param x A positive numeric value to round upward.
#' @param slack A numeric value (default \code{0.2}) that controls the looseness of the upper bound. If the proposed bound
#'   is more than \code{(1 + slack)} times larger than \code{x}, a smaller magnitude will be tried.
#' @param magnitude (Optional) A power-of-ten step size to use when rounding. If not provided, the function will automatically
#'   determine an appropriate initial magnitude based on the order of \code{x}.
#'
#' @return A numeric value representing the smallest rounded upper bound that is greater than or equal to \code{x},
#' while keeping the upper bound within a user-defined tolerance (slack) of the original value.
#'
#' @details
#' This function is useful when setting axis or color scale limits in visualizations. It balances between "round" breakpoints
#' (e.g., 10, 50, 100, 200) and tightness of fit, by avoiding unnecessarily large upper bounds. Internally, it reduces the
#' rounding magnitude recursively until the result is within \code{slack} of the original value.
#'
#' @examples
#' round_up_max(101)        # → 110
#' round_up_max(57)         # → 60
#'
#' @export
round_up_max <- function(x, slack=0.2, magnitude= NULL) {
  if (x == 0) return(1)
  if (is.null(magnitude)){
    magnitude <- 10^floor(log10(x))
    
  }
  
  upper_ratio = ceiling(x/magnitude)
  bound = upper_ratio*magnitude
  if ((bound/x)>1+slack){
    return (round_up_max(x,slack=slack,magnitude = magnitude/10))
  } else{
    return (bound)
  }
}
#' Extract and Format Data for Mapping from an Epistemic Model
#'
#' Prepares a spatially indexed data frame and associated metadata for mapping a specific summary statistic
#' from an epistemic model. This function is typically used as a backend utility for plotting functions such as \code{make_map()}.
#'
#' @param model A model output object from an \pkg{epistemic} modeling pipeline, containing predictions over time and space.
#' @param data_cls A data class object from the \pkg{epistemic} package, used to interpret model output and apply the correct spatial context.
#' @param params A named list specifying which metric to extract and how to compute it. Must include:
#'   \describe{
#'     \item{\code{metric}}{Character string specifying the summary statistic to extract. One of:
#'       \code{"mean"}, \code{"median"}, \code{"quantile"}, or \code{"exceedance"}.}
#'     \item{\code{use_count} (optional)}{Logical. Indicates whether to extract values on the count scale (e.g., case counts)
#'       or the proportion scale (e.g., incidence or prevalence). Only applies to \code{metric = "mean"}, \code{"median"}, or \code{"quantile"}.}
#'     \item{\code{quantile} (optional)}{Numeric value between 0 and 1, required when \code{metric = "quantile"}.}
#'     \item{\code{threshold} (optional)}{Numeric value specifying the exceedance threshold when \code{metric = "exceedance"}.}
#'   }
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{\code{data}}{A \code{data.frame} or \code{sf} object containing the mapped data, including geometry and the display column.}
#'     \item{\code{column}}{A character string giving the name of the column in \code{data} that contains the metric values to map.}
#'     \item{\code{name}}{A human-readable name (title) for the mapped variable, for use in legends or tooltips.}
#'     \item{\code{min}}{Numeric minimum value of the metric, used to define the color scale lower bound.}
#'     \item{\code{max}}{Numeric maximum value of the metric, used to define the color scale upper bound.}
#'   }
#'
#' @details
#' This function acts as the data preparation layer for map rendering. It filters and reshapes model output based on the
#' selected \code{metric}, target date, and aggregation options provided in \code{params}. The result can be passed
#' directly to mapping functions like \code{make_map()} for display.
#'
#' @seealso \code{\link{make_map}}, \code{\link[leaflet]{leaflet}}, \code{\link[tigris]{counties}}
#'
#' @examples
#' \dontrun{
#' map_info <- get_map_data(
#'   model = my_model,
#'   data_cls = my_data_cls,
#'   params = list(metric = "quantile", quantile = 0.9, use_count = FALSE)
#' )
#'
#' head(map_info$data)  # inspect result
#' map_info$column      # name of value column to map
#' map_info$max         # useful for setting color scale limits
#' }
#'
#' @export

get_map_data <-function(model,
                        data_cls,
                        params){
  if (!("metric" %in% names(params))){
    cli::cli_abort("Key metric must be provided.")
  }
  if (params$metric  == "mean"){
    if ("use_count" %in% names(params)){
      use_count = params$use_count
      if (use_count){
        display_col_name <- "Posterior mean (count)"
      } else {
        display_col_name <- "Posterior mean (proportion)"
      }
    } else {
      use_count = FALSE
      display_col_name <- "Posterior mean (proportion)"
    }
    dt <- epistemic::get_posterior_means(model,
                                         data_cls,
                                         use_suffix=FALSE,
                                         use_count_scale = use_count)
    display_col <- "predicted_mean"
    
    display_col_name <- "Posterior mean (proportion)"
    minv <- 0
    maxv <- round_up_max(max(dt[,predicted_mean]))
  } else if (params$metric  == "median"){
    if ("use_count" %in% names(params)){
      use_count = params$use_count
      if (use_count){
        display_col_name <- "Posterior median (count)"
      } else {
        display_col_name <- "Posterior median (proportion)"
      }
    } else {
      use_count = FALSE
      display_col_name <- "Posterior median (proportion)"
    }
    dt <- epistemic::get_posterior_medians(model,
                                         data_cls,
                                         use_suffix=FALSE,
                                         use_count_scale = use_count)
    display_col <- "0.5quant"
    minv <- 0
    maxv <- round_up_max(max(dt[,get(display_col)]))
  } else if (params$metric  == "quantile"){
    if ("quantile" %in% names(params)){
      q <- params$quantile 
      if (!is.numeric(q) || q < 0 || q > 1) {
        cli::cli_abort("Quantile must be a numeric value between 0 and 1.")
      }
    } else {
      cli::cli_abort("Required parameter 'quantile' for metric 'posterior_quantile' is missing from params.")
    }
    if (q<0.5){
      ci_width <-1-2*q
      display_col <- "lower"
    } else {
      ci_width <-2*q-1
      display_col <- "upper"
    }
    if ("use_count" %in% names(params)){
      use_count = params$use_count
      if (use_count){
        display_col_name <- paste0("Posterior quantile q=",q," (count)")
      } else {
        display_col_name <- paste0("Posterior quantile q=",q," (proportion)")
      }
    } else {
      use_count = FALSE
      display_col_name <- paste0("Posterior quantile q=",q," (proportion)")
    }
    dt <- epistemic::get_credible_intervals (model,
                                           data_cls,
                                           use_suffix=FALSE,
                                           ci_width = ci_width,
                                           use_count = use_count
                                           )
    minv <- 0
    maxv <- round_up_max(max(dt[,get(display_col)]))
  } else if (params$metric  == "exceedance"){
    if (!("threshold" %in% names(params))){
      cli::cli_abort("Threshold must be provided for metric exceedance.")
    } else {
      threshold <- params$threshold
    }
    dt <- epistemic::get_exceedance_probs(model,
                                        data_cls,
                                        threshold = threshold)
    display_col <- "exceedance_probs"
    display_col_name <- paste0("Exceedance Probability (threshold: ",threshold,")")
    minv <- 0
    maxv <- 1
  } else if (params$metric  == "change"){
    cli::cli_abort("Not Implemented Yet!")
    if (!("threshold" %in% names(params))){
      cli::cli_abort("Threshold must be provided for metric exceedance.")
    } else {
      threshold <- params$threshold
    }
    if ("use_count" %in% names(params)){
      use_count = params$use_count
    } else {
      use_count = FALSE
    }
    if ("use_absolute" %in% names(params)){
      use_absolute = params$use_absolute
    } else {
      use_absolute = FALSE
    }
    if (use_count){
      descriptor = "Counts"
    } else {
      descriptor = "Proportions"
    }
    if (use_absolute){
      display_col_name <- paste0("Absolute Change Probability for ",descriptor," (threshold: ",threshold,")")
    } else {
      display_col_name <- paste0("Relative Change Probability for ",descriptor," (threshold: ",threshold,")")
    }
  
    dt <- epistemic::get_probability_of_increase(model,
                                                 data_cls,
                                                 threshold = threshold)
    display_col <- "change_prob"
    display_col_name <- paste0("Change Probability (threshold: ",threshold,")")
    minv <- 0
    maxv <- 1
  } else {
    cli::cli_abort("Invalid metric.")
  }
  return (list(data = dt, 
              column = display_col, 
              name = display_col_name, 
              min = minv, 
              max = maxv))
  
}


#' Generate a Leaflet Map from Epistemic Model Output
#'
#' Creates an interactive choropleth map using outputs from an epistemic model, filtered by a specified target date.
#' The map is rendered using \pkg{leaflet} and U.S. county shapefiles from \pkg{tigris}, with support for flexible
#' metric types and configuration parameters.
#'
#' @param model A model output object, typically produced by an \pkg{epistemic} modeling pipeline.
#' @param data_cls A data class object from the \pkg{epistemic} package, containing metadata for interpreting model output.
#' @param params A named list of parameters defining what to display on the map. Must include:
#'   \describe{
#'     \item{\code{metric}}{Character string indicating the summary statistic to plot. One of:
#'       \code{"mean"}, \code{"median"}, \code{"quantile"}, or \code{"exceedance"}.}
#'     \item{\code{use_count} (optional)}{Logical. Indicates whether model outputs represent **counts** (e.g., number of cases)
#'       or **proportions** (e.g., prevalence). This is used when \code{metric} is \code{"mean"}, \code{"median"}, or \code{"quantile"}.
#'       For \code{"exceedance"}, outputs are assumed to already be on the proportion scale.}
#'     \item{\code{quantile} (optional)}{Numeric value between 0 and 1, required when \code{metric = "quantile"}, specifying
#'       the quantile level to map.}
#'     \item{\code{threshold} (optional)}{Numeric value used when \code{metric = "exceedance"}; defines the threshold above which
#'       the probability is computed.}
#'   }
#' @param map_year Integer. Year of the geographic shapefile used from \pkg{tigris}. Defaults to \code{2020}.
#' @param target_date Date. The model output will be filtered to this date for display. Defaults to \code{2025-02-02}.
#'
#' @return A \code{leaflet} HTML widget showing an interactive choropleth map of the specified model output metric across U.S. counties.
#'
#' @details
#' This function filters model output to a specific \code{target_date} and computes a selected summary metric using the
#' \code{params} list. The appropriate spatial geometry for U.S. counties is obtained from \pkg{tigris}, controlled by \code{map_year}.
#'
#' When plotting \code{"mean"}, \code{"median"}, or \code{"quantile"}, the \code{use_count} flag controls whether to extract
#' count-based summaries or proportion-based summaries from the model. For \code{"exceedance"}, values are assumed to already
#' be proportions, and \code{use_count} is ignored.
#'
#' @examples
#' \dontrun{
#' # Plot exceedance probabilities
#' make_map(
#'   model = my_model,
#'   data_cls = my_data_cls,
#'   params = list(metric = "exceedance", threshold = 0.9),
#'   target_date = as.Date("2025-02-02")
#' )
#'
#' # Plot median counts
#' make_map(
#'   model = my_model,
#'   data_cls = my_data_cls,
#'   params = list(metric = "median", use_count = TRUE),
#'   target_date = as.Date("2025-02-02")
#' )
#' }
#'
#' @import leaflet
#' @importFrom tigris counties
#' @export

make_map <- function(
    model,
    data_cls,
    params,
    map_year = 2020,
    target_date = as.Date("2025-02-02")){
  res <- get_map_data(model,data_cls,params)
  dt <- res$data 
  display_col <-res$column 
  display_col_name <- res$name 
  minv <- res$min 
  maxv <- res$max
  
  date_col <- data_cls$date_column
  region_col <- data_cls$region_column 
  
  
  # Subset the data using dynamic date column
  dt_sub <- dt[get(date_col) == target_date]
  
  # Ensure FIPS is a 5-digit string (pad with zeros)
  dt_sub[, (region_col) := sprintf("%05s", get(region_col))]
  dt_sub <- dt_sub[, .(countyfips = get(region_col), value = get(display_col))]

  # Load U.S. county geometries
  options(tigris_use_cache = TRUE)
  counties_sf <- tigris::counties(cb = TRUE, year = map_year, class = "sf") %>%
    filter(!STATEFP %in% c("60", "66", "69", "72", "78")) %>%
    mutate(countyfips = paste0(STATEFP, COUNTYFP))
  
  # Merge spatial and value data
  map_data <- right_join(counties_sf, dt_sub, by = "countyfips")
  map_data <- sf::st_transform(map_data, crs = 4326)
  
  map_data <- map_data %>% 
    mutate(hover_label = paste0("County: ",NAME,"<br>",
                                "FIPS Code: ",GEOID,"<br>",
                                display_col_name,": ",round(value, 4)))
  # get centroids for labels
  centroids <- sf::st_centroid(map_data)
  centroids <- sf::st_transform(centroids, 4326)
  coords <- sf::st_coordinates(centroids)

  # define color scale
  pal <- leaflet::colorNumeric("plasma", domain = c(minv,maxv), na.color = "transparent")
  pal_rev <- leaflet::colorNumeric("plasma", domain = c(maxv,minv), reverse = TRUE, na.color = "transparent")
    # Plot
  p <- leaflet::leaflet(map_data) %>%
    leaflet::addProviderTiles("CartoDB.Positron") %>%
    leaflet::addPolygons(
      fillColor = ~pal(value),
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = lapply(map_data$hover_label, htmltools::HTML)
    )  %>% 
    addLabelOnlyMarkers(
      lng = coords[, 1],
      lat = coords[, 2],
      label = centroids$NAME,
      labelOptions = labelOptions(
        noHide = TRUE,
        direction = 'center',
        textOnly = TRUE,
        style = list(
          "font-size" = "14px",
          "color" = "black",
          "text-shadow" = "1px 1px white"
        )
      )
    ) %>% 
    leaflet::addLegend(
      pal = pal_rev,
      values = c(minv,maxv),
      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
      title = display_col_name,
      position = "bottomright"
    )
  return (p)
}
iter = 0 
for (params in list(
            #list(metric = "change", dt = 1, threshold=1, use_absolute=TRUE, use_count=FALSE),
            #list(metric = "change", dt = 1, threshold=0.1, use_absolute=FALSE, use_count=FALSE)
            list(metric = "mean", use_count = TRUE),
            list(metric = "mean", use_count = FALSE),
            list(metric = "median", use_count = TRUE),
            list(metric = "median", use_count = FALSE),
            list(metric = "quantile", quantile = 0.75, use_count = TRUE),
            list(metric = "quantile", quantile = 0.75, use_count = FALSE),
            list(metric = "quantile", quantile = 0.25, use_count = TRUE),
            list(metric = "exceedance", threshold = 0.01)
            )) {
  print(params)
  p<-make_map(
    model,
    data_cls,
    params = params,
    target_date = as.Date("2025-02-02")
  )
  print(p)
  fn <- paste0("my_plot_",iter,".html")
  htmlwidgets::saveWidget(p, fn, selfcontained = TRUE)
  browseURL(fn)
  iter=iter+1
  
}
            