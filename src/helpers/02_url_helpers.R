get_categorical_values <- function(profile=NULL) {
  
  if(is.null(profile)) return(NULL)
  
  #########################################################
  comb_url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/datasources/va_hosp/fields/combinedCategory"
  url <- "https://essence.syndromicsurveillance.org/nssp_essence/servlet/SyndromeDefinitionsServlet_CCDD?action=getCCDDTerms"
  ##################################################   Note CCDD name change!
  
  combinedCategories <- comb_url |> get_api_data(profile = profile)
  ccddterms <- get_api_data(url, profile = profile)
  
  if(!is.null(combinedCategories) && !is.null(ccddterms)) {
    
    combinedCategories <- combinedCategories |>       
      pluck("values") |> 
      rename(combined_category = display) |> 
      left_join(
        ccddterms |> 
          pluck("categories") |> 
          select(combined_category = category, query_logic = definition) |> 
          mutate(combined_category = paste("CCDD", combined_category)), 
        by = "combined_category"
      ) |> 
      mutate(across(where(is.character), ~replace_na(., "")))
    
    
    setDT(combinedCategories)
    ccdd_cats <- combinedCategories[grepl("^CCDD", combined_category), combined_category]
    ccdd_cats <- gsub("^CCDD ", "", ccdd_cats, perl=T)
    syndromes <- combinedCategories[grepl("^SYNDROME", combined_category), combined_category]
    syndromes <- gsub("SYNDROME ", "", syndromes)
    subsyndromes <- combinedCategories[grepl("^SUBSYNDROME", combined_category), combined_category]
    subsyndromes <- gsub("SUBSYNDROME ", "", subsyndromes)
  } 
    
  return(list(
    ccdd_cats = ccdd_cats,
    syndromes = syndromes,
    subsyndromes = subsyndromes
  ))
  
}
