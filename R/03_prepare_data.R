# =============================================================================
# 03_prepare_data.R  –  Clean, reshape and aggregate data
# =============================================================================
# Steps
#   1. Harmonise GCP country names and build CountryAssumptions from data
#   2. Add a "World" row to DataGlobalCarbonBudget using GCP global totals
#   3. Adjust the carbon budget for 2020–2021 observed emissions
#   4. Restrict all datasets to the analysed country set
#   5. Compute "Rest of world" residuals
#   6. Convert SSP region names to iso3c
#   7. Extend DataUNPopulation with SSP population for future years
#   8. Sort datasets
#   9. Set factor levels for plotting
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Build CountryAssumptions from data
# -----------------------------------------------------------------------------

# Remove GCP aggregate regions — this analysis computes its own aggregates
gcb_aggregates <- c(
  "KP Annex B", "Non KP Annex B", "OECD", "Non-OECD",
  "Africa", "Asia", "Central America", "North America", "Europe",
  "Middle East", "Oceania", "South America",
  "International Shipping", "International Aviation",
  "Statistical Difference", "World"
)

# Income level → development classification
income_to_dev <- c(
  "High income"         = "High",
  "Upper middle income" = "Upper-middle",
  "Lower middle income" = "Lower-middle",
  "Low income"          = "Low"
)

DataGlobalCarbonBudget <- DataGlobalCarbonBudget %>%
  filter(!Country %in% gcb_aggregates)

# Assign iso3c codes directly to GCP countries.
# Most GCP names already match the canonical names in DataWorldBankClassif.
# Overrides below handle known mismatches (GCP-specific names → iso3c codes).
gcb_iso3c_overrides <- tribble(
  ~Country,          ~iso3c,
  "USA",             "USA",   # WB: "United States"
  "EU27",            "EUU",   # WB: not present (aggregate)
  "Slovakia",        "SVK",   # WB: "Slovak Republic"
  "Hong Kong",       "HKG",   # WB: "Hong Kong SAR, China"
  "Kyrgyzstan",      "KGZ",   # WB: "Kyrgyz Republic"
  "Venezuela",       "VEN",   # WB: "Venezuela, RB"
  "Laos",            "LAO",   # WB: "Lao PDR"
  "Egypt",           "EGY",   # WB: "Egypt, Arab Rep."
  "Gambia",          "GMB",   # WB: "Gambia, The"
  "Iran",            "IRN",   # WB: "Iran, Islamic Rep."
  "Russia",          "RUS",   # WB: "Russian Federation"
  "South Korea",     "KOR"    # WB: "Korea, Rep."
)

gcb_to_iso3c <- bind_rows(
  gcb_iso3c_overrides,
  DataWorldBankClassif %>% distinct(Country, iso3c)
) %>%
  distinct(Country, .keep_all = TRUE)   # overrides take priority

DataGlobalCarbonBudget <- DataGlobalCarbonBudget %>%
  left_join(gcb_to_iso3c, by = "Country")

# Warn about GCP countries with consumption emissions but no iso3c resolved
unmatched <- DataGlobalCarbonBudget %>%
  filter(Accounting == "Consumption Emissions", !is.na(EmissionsMtCO2), is.na(iso3c)) %>%
  distinct(Country)
if (nrow(unmatched) > 0)
  warning(sprintf(
    "No iso3c found for: %s — add to gcb_iso3c_overrides in 03_prepare_data.R.",
    paste(unmatched$Country, collapse = ", ")
  ))

# Analysed country set: iso3c codes with actual consumption emissions AND a
# recognised World Bank income classification. The income-level filter excludes
# GCB regional aggregates (e.g. "North America") whose iso3c codes exist in the
# World Bank API but carry no income level.
countries_iso3c <- DataGlobalCarbonBudget %>%
  filter(Year == YearStart, !is.na(EmissionsMtCO2), !is.na(iso3c),
         Accounting %in% c("Consumption Emissions", "Territorial Emissions")) %>%
  group_by(iso3c) %>%
  filter(n_distinct(Accounting) == 2) %>%
  ungroup() %>%
  semi_join(
    DataWorldBankClassif %>% filter(income_level %in% names(income_to_dev)),
    by = "iso3c"
  ) %>%
  distinct(iso3c) %>%
  pull(iso3c)

# Build CountryAssumptions using canonical names and ISO codes from DataWorldBankClassif
CountryAssumptions <- tibble(iso3c = countries_iso3c) %>%
  left_join(DataWorldBankClassif %>% distinct(iso3c, Country, iso2c), by = "iso3c") %>%
  left_join(
    DataWorldBankClassif %>%
      transmute(iso3c, Development = recode(income_level, !!!income_to_dev)),
    by = "iso3c"
  ) %>%
  mutate(EUMemberState = iso2c %in% EUStatesISO2) %>%
  bind_rows(
    tibble(Country = "Rest of world", iso3c = "ROW", iso2c = "RW",
           Development = "Rest of world", EUMemberState = FALSE),
    tibble(Country = "World",         iso3c = "WLD", iso2c = "WL",
           Development = "World",         EUMemberState = FALSE)
  )

# Filter GCP to analysed countries and update Country to canonical name
# (e.g. "USA" → "United States", "EU27" → "European Union")
DataGlobalCarbonBudget <- DataGlobalCarbonBudget %>%
  inner_join(
    CountryAssumptions %>% select(iso3c, CanonicalCountry = Country),
    by = "iso3c"
  ) %>%
  mutate(Country = CanonicalCountry) %>%
  select(-CanonicalCountry)

# Store the full country-level dataset before filtering years (used for RoW calc)
DataGlobalCarbonBudgetALL <- DataGlobalCarbonBudget

# -----------------------------------------------------------------------------
# 3. Add global totals row (fossil + land-use change, from GCP global budget)
# -----------------------------------------------------------------------------
DataGlobalCarbonBudgetWorld <- DataGlobalCarbonBudgetGlobal %>%
  transmute(
    Year,
    Country    = "World",
    Accounting = "World",
    EmissionsMtCO2 = (fossil.emissions.excl.carbonation + land.use.change.emissions) *
                      1000 * 44 / 12,   # GtC → MtCO₂
    iso3c      = "WLD"
  )

DataGlobalCarbonBudget <- bind_rows(
  DataGlobalCarbonBudget %>% filter(Country != "World"),
  DataGlobalCarbonBudgetWorld
)

# -----------------------------------------------------------------------------
# 4. Adjust carbon budgets for 2020–2021 actual emissions
# -----------------------------------------------------------------------------
# Subtract observed global emissions in 2020 and 2021 from the AR6 budget
# (the AR6 budgets are stated from 1 Jan 2020; we start from 1 Jan 2022).
emissions_2020_2021 <- DataGlobalCarbonBudget %>%
  filter(Country == "World", Accounting == "World", Year %in% 2020:2021) %>%
  pull(EmissionsMtCO2) %>%
  sum()

CarbonBudget <- CarbonBudget %>%
  mutate(BudgetGtCO2 = BudgetGtCO2 - emissions_2020_2021 / 1000)

# -----------------------------------------------------------------------------
# 5. Restrict to analysed countries and historical period
# -----------------------------------------------------------------------------
DataGlobalCarbonBudget <- DataGlobalCarbonBudget %>%
  filter(
    (Country %in% CountryAssumptions$Country | Country == "World") &
    Year >= YearStart & Year <= YearEnd
  )

DataUNPopulation <- DataUNPopulation %>%
  inner_join(CountryAssumptions %>% select(Country, iso3c), by = "iso3c") %>%
  select(-Country.x) %>%
  rename(Country = Country.y)

# Convert SSP region names to iso3c codes (SSP 3.2 uses full country names)
DataSSPFutureGDP <- DataSSPFutureGDP %>%
  left_join(
    DataWorldBankClassif %>% distinct(Country, iso3c) %>% filter(!is.na(iso3c)),
    by = c("Region" = "Country")
  ) %>%
  mutate(Region = case_when(
    !is.na(iso3c) ~ iso3c,
    Region == "World" ~ "WLD",
    TRUE ~ Region
  )) %>%
  select(-iso3c) %>%
  filter(Region %in% c(CountryAssumptions$iso3c, "WLD"))

# -----------------------------------------------------------------------------
# 6. Compute "Rest of world" residuals
# -----------------------------------------------------------------------------
# "Rest of world" = World total minus the sum of all explicitly modelled countries.
# Non-EU countries only, so that EU member-state totals are not double-counted.
non_eu_iso3c <- CountryAssumptions %>%
  filter(!EUMemberState, Country != "World") %>%
  pull(iso3c)

# --- Population ---
row_pop <- DataUNPopulation %>%
  filter(iso3c %in% non_eu_iso3c) %>%
  group_by(Year) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")

world_pop <- DataUNPopulation %>% filter(Country == "World") %>% select(Year, Population)

DataUNPopulation <- bind_rows(
  DataUNPopulation,
  world_pop %>%
    left_join(row_pop, by = "Year", suffix = c("_world", "_modelled")) %>%
    transmute(
      Country    = "Rest of world",
      iso3c      = "ROW",
      iso2c      = "RW",
      Year,
      Population = Population_world - Population_modelled
    )
)

# --- Emissions ---
row_terr <- DataGlobalCarbonBudget %>%
  filter(iso3c %in% non_eu_iso3c, Accounting == "Territorial Emissions") %>%
  group_by(Year, Accounting) %>%
  summarise(EmissionsMtCO2 = sum(EmissionsMtCO2, na.rm = TRUE), .groups = "drop")

row_cons <- DataGlobalCarbonBudget %>%
  filter(iso3c %in% non_eu_iso3c, Accounting == "Consumption Emissions") %>%
  group_by(Year, Accounting) %>%
  summarise(EmissionsMtCO2 = sum(EmissionsMtCO2, na.rm = TRUE), .groups = "drop")

world_emiss <- DataGlobalCarbonBudget %>%
  filter(Country == "World") %>%
  select(Year, WorldEmissions = EmissionsMtCO2)

DataGlobalCarbonBudget <- bind_rows(
  DataGlobalCarbonBudget,
  bind_rows(row_terr, row_cons) %>%
    left_join(world_emiss, by = "Year") %>%
    mutate(
      Country        = "Rest of world",
      iso3c          = "ROW",
      EmissionsMtCO2 = WorldEmissions - EmissionsMtCO2
    ) %>%
    select(Year, Country, Accounting, EmissionsMtCO2, iso3c)
)

# --- SSP (GDP and population) ---
row_ssp <- DataSSPFutureGDP %>%
  filter(Region %in% non_eu_iso3c) %>%
  group_by(Scenario, Year, Variable, Unit) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Model = "Aggregate", Region = "ROW")

world_ssp <- DataSSPFutureGDP %>%
  filter(Region == "WLD") %>%
  select(Scenario, Year, Variable, Unit, ValueWorld = Value)

DataSSPFutureGDP <- bind_rows(
  DataSSPFutureGDP,
  row_ssp %>%
    left_join(world_ssp, by = c("Scenario", "Year", "Variable", "Unit")) %>%
    mutate(Value = ValueWorld - Value) %>%
    select(-ValueWorld)
)

# -----------------------------------------------------------------------------
# 7. Build DataPopulation: historical UN data + SSP projections, same units
# -----------------------------------------------------------------------------
# DataUNPopulation (historical, YearStart:YearEnd) is kept unchanged.
# DataPopulation extends it through YearHorizon using SSP projections for the
# chosen scenario, anchored per-entity at YearEnd so that:
#   (a) units are aligned (SSP reports millions; UN reports persons), and
#   (b) the future trajectory is continuous with the last observed UN value.
#
# Structure of DataPopulation:
#   - Individual countries + EU + World: historical from DataUNPopulation,
#     future from SSP × per-entity scale factor
#   - Rest of world: residual (World − sum of non-EU individual countries)

ssp_pop_scenario <- DataSSPFutureGDP %>%
  filter(Scenario == SSPScenario, Variable == "Population")

# Per-entity anchoring factor: UN_value_at_YearEnd / SSP_value_at_YearEnd
entity_scale <- DataUNPopulation %>%
  filter(Year == YearEnd) %>%
  inner_join(
    ssp_pop_scenario %>%
      filter(Year == YearEnd) %>%
      select(Region, ssp_val = Value),
    by = c("iso3c" = "Region")
  ) %>%
  transmute(iso3c, Country, iso2c, scale = Population / ssp_val)

# Future rows (YearBudget:YearHorizon) from SSP, scaled to UN units
pop_future_raw <- ssp_pop_scenario %>%
  filter(Year >= YearBudget, Year <= YearHorizon) %>%
  inner_join(entity_scale, by = c("Region" = "iso3c")) %>%
  transmute(Country, iso3c = Region, iso2c, Year, Population = Value * scale)

# EU future: aggregate from member-state future rows
pop_future_eu <- pop_future_raw %>%
  filter(iso2c %in% EUStatesISO2) %>%
  group_by(Year) %>%
  summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop") %>%
  mutate(Country = "European Union", iso3c = "EUU", iso2c = "EU")

# ROW future: World − sum of non-EU individual countries
non_eu_individual_iso3c <- CountryAssumptions %>%
  filter(!EUMemberState, !iso3c %in% c("ROW", "WLD", "EUU")) %>%
  pull(iso3c)

pop_future_row <- pop_future_raw %>%
  filter(iso3c == "WLD") %>%
  select(Year, World_pop = Population) %>%
  left_join(
    pop_future_raw %>%
      filter(iso3c %in% non_eu_individual_iso3c) %>%
      group_by(Year) %>%
      summarise(modelled = sum(Population, na.rm = TRUE), .groups = "drop"),
    by = "Year"
  ) %>%
  transmute(Country = "Rest of world", iso3c = "ROW", iso2c = "RW",
            Year, Population = World_pop - modelled)

DataPopulation <- bind_rows(
  DataUNPopulation,                                              # historical
  pop_future_raw %>%
    filter(!iso3c %in% c("WLD", "EUU", "ROW")),                 # future individual countries (incl. EU members)
  pop_future_eu,                                                 # future EU aggregate
  pop_future_raw %>% filter(iso3c == "WLD"),                    # future World
  pop_future_row                                                 # future ROW
) %>%
  arrange(Country, Year)

# -----------------------------------------------------------------------------
# 8. Sort datasets
# -----------------------------------------------------------------------------
DataUNPopulation       <- DataUNPopulation       %>% arrange(Country, Year)
DataPopulation         <- DataPopulation         %>% arrange(Country, Year)
DataGlobalCarbonBudget <- DataGlobalCarbonBudget %>% arrange(Country, Year)
DataSSPFutureGDP       <- DataSSPFutureGDP       %>% arrange(Region, Year)

# -----------------------------------------------------------------------------
# 9. Set country factor levels for plotting
# -----------------------------------------------------------------------------
# Countries in alphabetical order, with Rest of world and World at the end.
CountryLevels <- c(
  sort(CountryAssumptions$Country[!CountryAssumptions$Country %in% c("World", "Rest of world")]),
  "Rest of world",
  "World"
)
