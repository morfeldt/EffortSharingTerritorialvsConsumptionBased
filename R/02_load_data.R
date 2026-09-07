# =============================================================================
# 02_load_data.R  –  Download and read all raw data
# =============================================================================
# Data sources
#   A. Global Carbon Project    – national territorial + consumption-based emissions
#   B. World Bank (API)         – country classifications, canonical names, ISO codes
#   C. UN Population Division   – historical population (API, cached to data/un_population.csv)
#   D. SSP Scenario Database    – future GDP and population projections
#      (data/ssp_data.csv, produced by python/fetch_ssp_data.py)
# =============================================================================

# ---------------------------------------------------------------------------
# A. Global Carbon Project (GCP) – national emission data
# ---------------------------------------------------------------------------
# Units in the file: MtC yr⁻¹  →  converted to MtCO₂ yr⁻¹ (factor 44/12)

# Helper: read one emissions sheet and pivot to long format
read_gcb_national <- function(file, sheet, skip_rows) {
  read_excel(file, sheet = sheet, skip = skip_rows) %>%
    # First column is the year (unnamed in the file, imported as "...1")
    rename(Year = 1) %>%
    filter(!is.na(Year), !is.na(suppressWarnings(as.numeric(Year)))) %>%
    mutate(Year = as.integer(Year)) %>%
    pivot_longer(
      cols      = -Year,
      names_to  = "Country",
      values_to = "EmissionsMtCO2"
    ) %>%
    mutate(
      EmissionsMtCO2 = as.numeric(EmissionsMtCO2) * 44 / 12,   # MtC → MtCO₂
      Country        = str_replace_all(Country, fixed("."), " ") # dots → spaces
    )
}

gcb_file <- GCB_NATIONAL_FILE

DataGlobalCarbonBudget <- bind_rows(
  read_gcb_national(gcb_file, "Territorial Emissions",  skip_rows = 11) %>%
    mutate(Accounting = "Territorial Emissions"),
  read_gcb_national(gcb_file, "Consumption Emissions",  skip_rows = 8) %>%
    mutate(Accounting = "Consumption Emissions")
) %>%
  filter(Year >= YearStart, Year <= YearEnd)

# ---------------------------------------------------------------------------
# B2. GCP – global totals (fossil + land-use change)
# ---------------------------------------------------------------------------
# Units: GtC yr⁻¹  →  converted to MtCO₂ yr⁻¹ (× 1000 × 44/12)
#
# Expected columns (row 22 of the sheet is the header):
#   Col 1: year
#   Col 2: fossil emissions excluding carbonation (GtC)
#   Col 4: land-use change emissions (GtC)

DataGlobalCarbonBudgetGlobal <- read_excel(
  GCB_GLOBAL_FILE,
  sheet = "Global Carbon Budget",
  skip  = 21
) %>%
  rename_with(make.names) %>%
  rename(Year = 1) %>%
  filter(!is.na(Year), !is.na(suppressWarnings(as.numeric(Year)))) %>%
  mutate(
    Year                              = as.integer(Year),
    fossil.emissions.excl.carbonation = as.numeric(fossil.emissions.excluding.carbonation),
    land.use.change.emissions         = as.numeric(land.use.change.emissions)
  ) %>%
  filter(Year >= YearStart, Year <= YearEnd)

# ---------------------------------------------------------------------------
# C. World Bank – country classifications, canonical names, and ISO codes
# ---------------------------------------------------------------------------
# Downloaded from wb_countries() and cached to data/wb_classif.csv.
# Delete this file to force a fresh download.
#
# All country name overrides and manually added entities (EU aggregate, Taiwan)
# are applied in-code after loading so all canonical-name adjustments are
# in one place.
WB_CLASSIF_CACHE <- "data/wb_classif.csv"

if (file.exists(WB_CLASSIF_CACHE)) {
  message(sprintf("Reading World Bank classifications from %s ...", WB_CLASSIF_CACHE))
  DataWorldBankClassif <- read_csv(WB_CLASSIF_CACHE, show_col_types = FALSE)
} else {
  message("Downloading country classifications from World Bank ...")
  DataWorldBankClassif <- wb_countries() %>%
    select(Country = country, iso2c, iso3c, region, income_level)
  write_csv(DataWorldBankClassif, WB_CLASSIF_CACHE)
  message(sprintf("World Bank classifications cached to %s", WB_CLASSIF_CACHE))
}

# Normalise column name, add manually managed entities not present in the World Bank country list, 
# input income level for Ethiopia (low income) and Venezuela (upper middle 
# income) that are currently not classified by the World Bank, and adjust
# spelling of country names.
DataWorldBankClassif <- DataWorldBankClassif %>%
  rename_with(~ ifelse(. == "country", "Country", .)) %>%
  filter(!region == "Aggregates") %>%
  bind_rows(
    tibble(Country = "European Union", iso3c = "EUU", iso2c = "EU", income_level = "High income"),
    tibble(Country = "Taiwan",         iso3c = "TWN", iso2c = "TW", income_level = "High income")
  ) %>%
  mutate(income_level = case_when(
    iso3c == "ETH" ~ "Low income",
    iso3c == "VEN" ~ "Upper middle income",
    TRUE           ~ income_level
  )) %>%
  mutate(Country = case_when(
    iso3c == "TUR" ~ "Türkiye",
    iso3c == "CIV" ~ "Côte d'Ivoire",
    TRUE           ~ Country
  ))

# ---------------------------------------------------------------------------
# D. UN Population Division – total population, medium variant
# ---------------------------------------------------------------------------
# Data source: UN Population Division Data Portal API (indicator 49: total population by sex)
# Historical data only (up to YearEnd); future population projections come from
# the SSP database (see section E below).
# API access requires a Bearer token. Register at:
#   https://population.un.org/dataportal/
# When prompted, paste your token (it will not be stored anywhere).
# Filters: SexId == 3 (both sexes), VariantId == 4 (medium variant)
#
# Downloaded data is cached to data/un_population.csv. Delete this file to
# force a fresh download.
UN_POP_CACHE <- "data/un_population.csv"

fetch_un_population <- function(start_year = YearStart, end_year = YearEnd,
                                sex_id = 3, variant_id = 4) {
  # Prompt user for Bearer token at runtime — never hardcode credentials
  # Works interactively (console or source()); set env var UN_POP_TOKEN to
  # supply non-interactively (e.g. in a batch/CI environment).
  token <- Sys.getenv("UN_POP_TOKEN")
  if (nchar(trimws(token)) == 0) {
    token <- readline("Enter UN Population Data Portal Bearer token: ")
  }
  if (nchar(trimws(token)) == 0) stop("No token provided.")

  base_url <- "https://population.un.org/dataportalapi/api/v1"

  # Helper: authenticated request (60s timeout, retry up to 3x on 5xx/timeout)
  un_request <- function(path, ...) {
    httr2::request(base_url) %>%
      httr2::req_url_path_append(path) %>%
      httr2::req_url_query(...) %>%
      httr2::req_headers(Authorization = paste("Bearer", token)) %>%
      httr2::req_timeout(60) %>%
      httr2::req_retry(max_tries = 3, is_transient = \(r)
        httr2::resp_status(r) %in% c(500, 502, 503, 504)) %>%
      httr2::req_perform() %>%
      httr2::resp_body_json(simplifyVector = TRUE)
  }

  # Helper: fetch all pages of an endpoint
  fetch_all_pages <- function(path, ...) {
    pages <- list()
    page  <- 1
    repeat {
      body <- un_request(path, pageNumber = page, pageSize = 500,
                         pagingInHeader = "false", format = "json", ...)
      pages[[page]] <- body$data
      if (page >= body$pages) break
      page <- page + 1
      Sys.sleep(0.3)
    }
    dplyr::bind_rows(pages)
  }

  # Step 1: fetch location IDs for modelled countries + World (location 900).
  # World is included here so DataUNPopulation is self-contained.
  needed_iso3c <- DataWorldBankClassif %>%
    dplyr::filter(!iso3c %in% c("WLD", "EUU", "ROW")) %>%
    dplyr::pull(iso3c) %>%
    unique()

  message("Fetching UN location list...")
  locations <- fetch_all_pages("locations")
  country_ids <- c(
    locations %>%
      dplyr::filter(iso3 %in% needed_iso3c) %>%
      dplyr::pull(id),
    900L   # UN location code for World
  )

  message(sprintf("  %d countries to fetch (incl. World)", length(country_ids)))

  # Step 2: fetch population data in batches to avoid gateway timeouts
  # The v1 API ignores sexId/variantId query params, so we fetch all variants
  # and sexes and filter client-side to both-sexes (sexId=3) + median (variantId=4).
  message(sprintf("Fetching UN population data %d–%d in batches ...",
                  start_year, end_year))
  # API does not support comma-separated location IDs — fetch one at a time
  dplyr::bind_rows(lapply(seq_along(country_ids), function(i) {
    message(sprintf("  Country %d / %d", i, length(country_ids)))
    fetch_all_pages(
      sprintf("data/indicators/49/locations/%d/start/%d/end/%d",
              country_ids[[i]], start_year, end_year)
    )
  })) %>%
    dplyr::filter(sexId == sex_id, variantId == variant_id) %>%
    dplyr::transmute(
      Country    = location,
      # World (location 900) has no ISO codes in the UN system — map manually
      iso3c      = dplyr::if_else(nchar(trimws(iso3)) == 3, iso3, "WLD"),
      iso2c      = dplyr::if_else(nchar(trimws(iso2)) == 2, iso2, "WL"),
      Year       = as.integer(timeLabel),
      Population = value
    )
}

if (file.exists(UN_POP_CACHE)) {
  message(sprintf("Reading UN population data from %s ...", UN_POP_CACHE))
  DataUNPopulation <- read_csv(UN_POP_CACHE, show_col_types = FALSE)
} else {
  DataUNPopulation <- fetch_un_population(start_year = YearStart, end_year = YearEnd)
  write_csv(DataUNPopulation, UN_POP_CACHE)
  message(sprintf("UN population data cached to %s", UN_POP_CACHE))
}

# Aggregate EU-27 population by summing member-state rows
DataUNPopulation <- bind_rows(
  DataUNPopulation,
  DataUNPopulation %>%
    filter(iso2c %in% EUStatesISO2) %>%
    group_by(Year) %>%
    summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop") %>%
    mutate(Country = "European Union", iso3c = "EUU", iso2c = "EU")
)

# ---------------------------------------------------------------------------
# E. SSP scenario data – future GDP (PPP) and population (2010–2100)
# ---------------------------------------------------------------------------
# Preferred path: data/ssp_data.csv produced by python/fetch_ssp_data.py
# Fallback path:  iamc_db_GDP.xlsx and iamc_db_POP.xlsx (IAMC wide format)
#
# Both are harmonised into a long-format tibble with columns:
#   Model, Scenario, Region, Variable, Unit, Year (integer), Value (numeric)
# Values are linearly interpolated to annual resolution (2010–2100).

interpolate_ssp_annual <- function(df) {
  # Interpolate a grouped SSP dataset from its native time step to annual.
  # df must already be grouped by Model, Scenario, Region, Variable, Unit.
  df %>%
    group_modify(~ {
      tibble(
        Year  = 2010:2100,
        Value = approx(.x$Year, .x$Value, xout = 2010:2100, rule = 2)$y
      )
    }) %>%
    ungroup()
}

message("Reading SSP data from data/ssp_data.csv ...")

DataSSPRaw <- read_csv("data/ssp_data.csv", show_col_types = FALSE) %>%
  rename_with(str_to_title)   # model → Model, region → Region, etc.

# Validate SSP model selections against available models in the data
.check_ssp_model <- function(model_param, param_name, variable) {
  available <- DataSSPRaw %>% filter(Variable == variable) %>% pull(Model) %>% unique() %>% sort()
  if (is.null(model_param)) {
    stop(sprintf(
      "%s is not set in R/01_parameters.R. Available models for \"%s\":\n  %s",
      param_name, variable, paste(available, collapse = "\n  ")
    ))
  }
  if (!model_param %in% available) {
    stop(sprintf(
      "%s = \"%s\" not found in data. Available models for \"%s\":\n  %s",
      param_name, model_param, variable, paste(available, collapse = "\n  ")
    ))
  }
}
.check_ssp_model(SSPModelGDP,        "SSPModelGDP",        "GDP|PPP")
.check_ssp_model(SSPModelPopulation, "SSPModelPopulation", "Population")
rm(.check_ssp_model)

DataSSPFutureGDP <- DataSSPRaw %>%
  filter(
    Variable %in% c("GDP|PPP", "Population"),
    Year >= 2010, Year <= 2100,
    (Variable == "GDP|PPP"    & Model == SSPModelGDP        & Unit == SSPUnitGDP) |
    (Variable == "Population" & Model == SSPModelPopulation & Unit == "million")
  ) %>%
  mutate(Year = as.integer(Year)) %>%
  group_by(Model, Scenario, Region, Variable, Unit) %>%
  interpolate_ssp_annual()
# EU-27 SSP aggregation is done in 03_prepare_data.R after region names are
# converted to iso3c codes.
