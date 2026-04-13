<!--
# (c) 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958
-->

## Bayesian Spatiotemporal Modeling Documentation

This page is the main in-app guide for the Bayesian Spatiotemporal Modeling application. It covers both:

- a user guide for analysts running ESSENCE queries, fitting models, and reviewing results
- a technical guide for developers extending the model-building workflow

## Where Documentation Lives

The current app renders this file directly in the Documentation modal opened from the gear menu.

- In-app documentation source: `src/documentation/documentation.md`
- Documentation renderer: `src/modules/documentation.R`
- Documentation entry point in the navbar menu: `src/modules/extras_module.R`

If you want to expand the docs later, this file is the fastest place to add user-facing content without changing app code.

## User Guide

### Application Overview

The app supports a common workflow for Bayesian spatiotemporal surveillance modeling:

1. Load training data from ESSENCE or from a saved query file.
2. Fit a new INLA model or load a previously saved model.
3. Explore fitted values and forecasts in tables, maps, and time-series views.
4. Create additional posterior summary features for reporting and export.

The main work areas are:

- `Data Loader`: retrieve or reload source data
- `INLA Model`: define, fit, save, or reload a model
- `Visualization`: inspect model outputs and derived features
- `Documentation`: open this guide from the gear menu

### Before You Start

The app expects:

- valid ESSENCE credentials or an RStudio profile that resolves them
- the `epistemic` package at version `1.5.1` or higher
- county-level adjacency matrices used by the spatial model components

By default, the app checks credentials during startup in `src/00_setup.R` and `src/01_credentials.R`.

### Data Loader

Use the `Data Loader` tab to prepare the analysis dataset.

#### Query ESSENCE

The `Query ESSENCE` panel lets you define:

- geographic resolution, which is currently configured for county-level querying
- states and counties, where you can start with selected states and then refine the county set in the county selector modal
- date range, where weekly data should use Sunday start dates and Saturday end dates to avoid partial weeks
- time resolution, with weekly and daily currently available
- diagnostic grouping, including chief complaint and discharge diagnosis category, syndrome, or sub-syndrome

After you click `Query ESSENCE`, the app builds ESSENCE API URLs, downloads both:

- the target series
- the overall visit series used as the denominator/reference

Those are merged into a table with fields such as:

- `date`
- `region`
- `countyfips`
- `target`
- `overall`

#### County Selection Notes

The county selector and validation logic help prevent common setup problems.

- The app warns if too many counties are selected.
- The app warns if selected counties are not connected in the physical adjacency matrix.
- If only a small number of states are selected, counties are preselected automatically.

#### Load Saved Query

You can reload a previously prepared dataset from a `.bsm_query` file. A saved query contains:

- an `.rds` file with the retrieved data
- a `.json` file with the query settings needed to restore the UI

This is useful when you want to share or repeat a query without pulling from ESSENCE again.

#### Data Exports and Covariates

Once data are loaded, you can:

- download the merged dataset as CSV
- save the query as a `.bsm_query` archive
- add covariates to the working dataset for later use in the model

### INLA Model

Use the `INLA Model` tab to fit a new model or load a saved model.

#### Inputs

The model sidebar lets you set:

- forecast horizon
- distribution family: `poisson`, `nbinomial`, `binomial`, or `betabinomial`
- model specification mode

#### Model Specification Modes

There are three supported ways to define the model formula.

##### 1. Default Model

This is the fastest path and the recommended starting point for most users. The app assembles a formula from built-in defaults, including:

- an intercept
- a region random effect
- a spatial component
- a temporal component when required by the time resolution

##### 2. Customize Components

This mode keeps the guided UI while letting you turn model pieces on or off and tune selected hyperparameters.

Available components include:

- region random effect
- spatiotemporal component
- seasonal or temporal component

Advanced options include:

- adjacency basis selection using mobility or physical adjacency
- spatial model type: `besagproper`, `besag`, or `bym`
- temporal dependence model using autoregressive or random walk variants
- penalized complexity prior settings for precision terms

##### 3. Custom Model Formula

Advanced users can type a full INLA formula directly. The app shows the currently available numeric feature columns and validates the formula against the processed modeling dataset before enabling model fit.

Use this mode when you need to:

- reference imported covariates directly
- omit default random effects
- prototype alternative INLA structures quickly

If your custom formula references `graph`, the UI also asks which adjacency matrix should be bound into the model fit.

#### Running the Model

When you click `Run Model`, the app:

1. preprocesses the loaded data into an `epistemic` data-class object
2. adds future dates for forecasting
3. adds helper identifiers such as region and time indices
4. constructs the formula
5. calls `epistemic::fit_inla_model()`
6. postprocesses fitted output into stored posterior summary columns

The fitted model summary card then provides:

- a fit summary
- data export options
- model save and reload support
- the actual formula passed into INLA

#### Saved Models

Saved model files use the `.bsm_model` extension. They contain:

- an `.rds` file with the fitted model object and processed data class
- a `.json` file with the key UI settings needed to restore the session

Use saved models when:

- model fitting is slow and you want to reuse results
- you need to share a fitted model configuration with another analyst
- you want the visualization tabs to reopen with the same modeling context

### Visualization

The `Visualization` tab group reads from the fitted model output and the shared feature store.

#### Region-Wide Map

This tab displays one numeric stored feature at a time on a county choropleth.

Key behaviors:

- the sparkline above the map selects the displayed date
- the default map view starts at the last observed date before the forecast horizon
- clicking a region opens a popup time series for that same feature
- advanced settings allow color palette, legend position, and shared-range control

This tab is best for scanning geographic patterns at a single date.

#### Prediction Time Series Plots

This tab focuses on forecast trajectories for selected regions.

- choose count or proportion scale
- choose a stored credible interval feature
- select one or more regions
- optionally fix the y-axis across panels

The plot uses stored posterior median and credible interval columns rather than recomputing summaries inside the tab.

#### Other Time Series Plots

This tab is intended for broader exploratory plotting of stored features over time.

- select one or more features
- select one or more regions
- choose whether features are overlaid or split into separate panels
- optionally fix the y-axis

This view is helpful when comparing custom derived features, covariates, or posterior summaries.

#### Posterior Data

This tab exposes the fitted data table used across the visualization workflow.

You can:

- choose which stored features appear in the table
- filter numeric, date, and categorical columns
- download the displayed table as CSV

#### Add Feature

This tab lets users create additional stored posterior summary columns without refitting the model.

Supported feature types:

- posterior mean
- posterior quantile
- credible interval
- exceedance probability

These user-added features are registered in the shared feature store and become available to the other visualization tabs immediately after calculation.

### Recommended End-to-End Workflow

For a typical analysis session:

1. Load data from ESSENCE for the desired syndrome, geography, and dates.
2. Add any external covariates needed for the model.
3. Fit the default model first.
4. Review prediction plots and posterior tables.
5. Add custom posterior summaries only after the baseline model looks reasonable.
6. Save the fitted model if you expect to revisit the analysis.

## Technical Guide for Model Development

### Code Organization

The app follows a modular Shiny layout.

- startup and dependency loading: `app.R`, `src/00_setup.R`
- global helpers: `src/helpers/`
- feature and visualization modules: `src/modules/viz/`
- data ingestion: `src/modules/data_loader.R`
- model fitting: `src/modules/inla_model.R`
- location selection: `src/modules/location_selector/`
- documentation modal: `src/modules/documentation.R`

All helper and module files are sourced recursively from `src/00_setup.R`.

### Main Reactive Objects

The server uses several shared reactive containers in `app.R`.

- `dc`
  - data-loading configuration such as geography, dates, syndrome, and adjacency matrices
- `cache_transitions`
  - values passed across modules when loading saved queries or models
- `im`
  - model objects, processed data class, posterior data, forecast horizon, and feature store
- `results`
  - cross-module data outputs, currently including the loaded source data

If you are adding model-development features, these shared reactives are the first place to check for existing state.

### Data Flow Into the Model

The modeling path is:

1. `Data Loader` retrieves ESSENCE data and merges target and overall counts.
2. Optional covariates are merged into the working dataset.
3. `pre_process_data()` converts the table into an `epistemic` data-class object.
4. Future dates and helper indices are added.
5. `get_formula()` builds or validates the INLA formula.
6. `epistemic::fit_inla_model()` runs the fit.
7. Built-in calculated posterior summaries are stored back into `im$data_cls$data`.

The app is designed so visualization modules consume stored columns instead of recomputing posterior summaries repeatedly.

### Formula Construction

Formula assembly is centered in `src/modules/inla_model.R`.

Key functions:

- `get_formula()`
- `build_region_random_effect()`
- `build_temporal_component()`
- `build_spatial_component()`
- `pretty_formula()`

When changing defaults, update both:

- the UI defaults exposed to users
- `MODEL_COMPONENT_DEFAULTS`, which controls programmatic formula generation

### Preprocessing Conventions

`pre_process_data()` adds structure that downstream modules rely on, including:

- canonical region and date columns
- explicit ID columns such as `r_id`, `d_id`, week and day-of-week identifiers
- inferred covariate columns
- core modeling columns such as target, denominator, and expected values

If you modify preprocessing, review any code that reads:

- `data_cls$id_columns`
- `data_cls$core_columns`
- `data_cls$covariate_columns`
- `data_cls$other_columns`

Those classifications drive feature registration and visualization behavior.

### Feature Store Design

The shared feature catalog is initialized by `init_feature_df()` in `src/modules/inla_model.R`.

It tracks:

- feature IDs
- user-facing labels and descriptions
- feature type and scale
- output column names stored in the posterior table
- grouping metadata for composite features such as credible intervals

This design is important because:

- built-in posterior summaries are registered once at fit time
- user-added summaries are persisted into the shared data table
- visualization modules filter on feature metadata instead of hard-coded column names

When adding a new feature type, update both:

- the feature registration logic
- the storage and plotting logic that materializes columns into `im$data_cls$data`

### Saved Artifacts

The app currently uses two archive formats:

- `.bsm_query`
  - raw retrieved data plus serialized query settings
- `.bsm_model`
  - fitted model object plus serialized model and session settings

Loading helpers live in `src/helpers/01_auxiliary_functions.R`.

If you change saved object structure, keep backward compatibility in mind by updating the normalization helpers rather than assuming all saved files match the newest schema.

### Extension Points for Developers

Common customization points include:

- adding new covariate ingestion behavior in the data-loading path
- changing default model components or priors
- extending custom formula validation
- adding new derived posterior feature types
- exposing additional stored features in tables, maps, or plots
- expanding the documentation modal with examples and screenshots

### Developer Notes and Cautions

- The current UI is county-focused even though some helper functions support broader geography concepts.
- Visualization modules expect stored feature columns to exist before plotting.
- Confidence intervals are treated as composite features with paired lower and upper output columns.
- Saved models may require schema normalization when internal data-class fields change.
- The app currently disables the Visualization tab until a model is available.

### Suggested Next Documentation Additions

Useful follow-on material would include:

- one worked example with sample settings and screenshots
- guidance on choosing distribution families
- a short section on interpreting posterior count versus proportion scales
- conventions for naming user-defined features
- developer examples of custom formulas that include covariates and graph-based terms
