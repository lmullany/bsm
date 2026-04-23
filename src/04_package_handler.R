# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

# Package check and handle function. 

package_handler <- function(
    install_required=FALSE, 
    inla_min = "25.04.09", 
    epistemic_min = "1.6.0"
) {
  
  # check all the names of installed packages accessible in .libPaths()
  accessible_packages <- lapply(
    .libPaths(), \(lp) installed.packages(lp) |> row.names()
  ) |> 
    unlist() |> 
    unique()
  
  # Is INLA available?
  
  # set the current version with a NULL value
  INLA_version = "0.0.0"
  
  # set the minimum INLA version
  MIN_INLA_VERSION = inla_min
  
  # Update the current version to the one that is installed, if any
  if("INLA" %in% accessible_packages) INLA_version <-utils::packageVersion("INLA") 
  
  # if the current version is less than the minimum fail gracefully
  if(INLA_version < MIN_INLA_VERSION) {
    stop(paste0(
      "This app cannot be run until you install INLA version ",
      MIN_INLA_VERSION,
      " or greater.\n",
      "See https://r-inla.org/download/index.html for more information"
    ))
  }
  
  # Okay, if we get to this point, INLA is installed, and meets the minimum 
  # version. Can a test INLA model be fit on this machine?
  failure_msg <- "a test run of INLA failed; app cannot be run"
  tryCatch(
    {
      x <- rnorm(100); y <- 1 + 2*x + rnorm(100)
      fit <- INLA::inla(y ~ x, data = data.frame(x = x, y = y))
      if(!fit$ok) stop(msg)
    },
    error =  function(e) stop(msg)
  )
  
  # these packages cannot be installed automatically; they are too heavy
  # with numerous dependencies
  uninstallable_packages <- c(
    "ggplot2", "dplyr", "tidyr", "stringr", "plotly",
    "geojsonsf", "sf", "leaflet", "leaflet.extras", "leafpop", "Rnssp"
  )
  # update to those that aren't available:
  uninstallable_packages <- uninstallable_packages[
    !uninstallable_packages %in% accessible_packages
  ]
  
  # if this contains any, we fail gracefully:
  if(length(uninstallable_packages)>0) {
    msg = paste0(
      "Install the following missing packages and try again:\n",
      paste0(uninstallable_packages, collapse = ",")
    )
    
    if("Rnssp" %in% uninstallable_packages) {
      msg <- paste(
        msg,
        "\n\n",
        "Note: Rnssp package not available on CRAN: ", 
        "see https://cdcgov.github.io/Rnssp/"
      )   
    }
    stop(msg)
  }
  
  # Okay, this is the remainder of the 
  installable_required_packages <- c(
    "shiny", "shinyjs", "cli", "data.table", "bslib",
    "bsicons", "lubridate","MMWRweek", "shinycssloaders", "gridExtra",
    "rlang", "reactable", "viridisLite"
  )
  
  missing_required <- installable_required_packages[
    !installable_required_packages %in% accessible_packages
  ]
  if(length(missing_required)>0) {
    if(install_required) {
      cat(
        "The following required app dependencies are missing, and will be installed:",
        paste0(missing_required,collapse = ",")
      )
      lapply(missing_required, \(mr) install.packages(mr))
    } else {
      stop(
        "Install the following missing packages and try again:\n",
        paste0(missing_required, collapse = ",")
      )
    }
  }
  
  epistemic_version = "0.0.0"
  epistemic_installed <- "epistemic" %in% accessible_packages
  # update the version if epistemic is installed
  if(epistemic_installed) epistemic_version <- utils::packageVersion("epistemic")
  
  # Also need epistemic, and must be of a certain version
  MIN_EPISTEMIC_VERSION = epistemic_min
  if(epistemic_version<MIN_EPISTEMIC_VERSION) {
    # pak or devtools might not be availabe
    if(!"pak" %in% accessible_packages) {
      stop(paste(
        "epistemic is not installed and must be installed from github, but",
        "pak package is not available. Install pak, devtools or remotes, and",
        "manually install using pak::pak(\"mpanaggio/epistemic\") or similar."
      ))
    } else {
      cat("Installing 'epistemic' package from github repo")
      pak::pak("mpanaggio/epistemic")
    }
  }
  
  cat("all required packages and version(s) found.")
  
  return(invisible())
}
