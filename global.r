# =============================================================================
# DESCRIPTION
# =============================================================================
# Initialize the project environment and prepare the datasets, country mappings,
# available years, and variable lists used in downstream index and PCA analyses.
#
# Data sources:
#   - VDemCY: V-Dem country-year data.
#   - WGI: Worldwide Governance Indicators.
#   - FSI: Fragile States Index.
#
# Main objects:
#   - map:         World map object.
#   - YearList:    Sorted available years for each data source.
#   - Map_data:    Country mappings for each data source.
#   - Index_data:  Indicator datasets for each data source.
#   - PCA_Varlist: Variables selected for PCA for each data source.
# =============================================================================


# =============================================================================
# 1. LOAD DEPENDENCIES, FUNCTIONS, SETTINGS, AND PARAMETERS
# =============================================================================

# Load project dependencies and general helper functions.
source("./script/dependencies.R")
source("./script/helper.R")

# Load project settings, PCA functions, exclusion-related functions,
# and analysis parameters.
source("./script/setting.R")
source("./script/pca_fun.R")
source("./script/drop_exit_func.R")
source("./script/parameter.R")


# =============================================================================
# 2. LOAD COUNTRY MAPPINGS AND THE WORLD MAP
# =============================================================================

# Standardize the grouping column name from "Groupe" to "Group" and remove
# any existing grouping attributes from each country mapping.
VDemCY_map <- readRDS("./data/VDemCmap.rds") |>
  rename(Group = Groupe) |>
  ungroup()

WGI_map <- readRDS("./data/WGImap.rds") |>
  rename(Group = Groupe) |>
  ungroup()

FSI_map <- readRDS("./data/FSImap.rds") |>
  rename(Group = Groupe) |>
  ungroup()

# Load the world map for downstream geographic visualization.
map <- readRDS("./data/world_map.RDS")


# =============================================================================
# 3. LOAD INDICATOR DATASETS
# =============================================================================

# Load the V-Dem country-year dataset as stored.
VDemCY_data <- readRDS("./data/VDemCYdata.rds")

# Load WGI data, remove existing grouping, and retain only countries or
# territories included in the corresponding country mapping.
WGI_data <- readRDS("./data/WGIdata.rds") |>
  ungroup() |>
  filter(Country.Territory %in% WGI_map[["Country.Territory"]])

# Apply the same country-coverage restriction to the FSI dataset.
FSI_data <- readRDS("./data/FSIdata.rds") |>
  ungroup() |>
  filter(Country.Territory %in% FSI_map[["Country.Territory"]]) |>
  mutate( across(-c(Year, `Country.Territory`), \(x) 10 - x ))


# =============================================================================
# 4. IDENTIFY AVAILABLE YEARS
# =============================================================================

# Extract distinct years from each dataset and sort them in ascending order.
YearFSI <- sort(unlist(unique(FSI_data$Year)))
YearWGI <- sort(unlist(unique(WGI_data$Year)))
YearVDemCY <- sort(unlist(unique(VDemCY_data$Year)))

# Store the available years in a named list for source-specific access.
YearList <- list(
  FSI = YearFSI,
  WGI = YearWGI,
  VDemCY = YearVDemCY
)


# =============================================================================
# 5. ORGANIZE INPUTS FOR DOWNSTREAM ANALYSIS
# =============================================================================

# Collect country mappings using consistent data-source identifiers.
Map_data <- list(
  WGI = WGI_map,
  FSI = FSI_map,
  VDemCY = VDemCY_map
)

# Collect the indicator datasets using the same identifiers.
Index_data <- list(
  WGI = WGI_data,
  FSI = FSI_data,
  VDemCY = VDemCY_data
)

# Collect the source-specific PCA variable selections.
# These variable objects must be available after sourcing the project scripts.
PCA_Varlist <- list(
  WGI = WGIvariable,
  FSI = FSIvariable,
  VDemCY = VDemCYvariable
)


# =============================================================================
# 6. CLEAN UP STANDALONE INPUT OBJECTS
# =============================================================================

# Remove the individual datasets and country mappings from the workspace.
# Their contents remain accessible through Index_data and Map_data.
rm(
  WGI_data,
  FSI_data,
  VDemCY_data,
  WGI_map,
  FSI_map,
  VDemCY_map
)