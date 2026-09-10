# =============================================================================
# 09_export.R  –  Write all supplementary output to a single Excel workbook
# =============================================================================
# Produces:
#   output/SupplementaryData.xlsx
#     Tab 1:  Contents                – table of contents and publication info
#     Tab 2:  Full Results            – complete budget dataset
#     Tab 3:  Figure 1               – national carbon budgets, sample countries
#     Tab 4:  Figure 2               – implied net-zero emission years, sample countries
#     Tab 5:  Figure 3               – per-capita emissions trends, sample countries
#     Tab 6:  Figure 4               – scatter (net emissions embodied in trade), non-EU countries
#     Tab 7:  Figure 5               – scatter (economic development), non-EU countries
#     Tab 8:  Figure 6               – Sankey / budget-category transitions
#     Tab 9:  Extended Data Figure 1 – annual vs cumulative net emissions embodied in trade
#     Tabs 10–18: Extended Data Figures 2–10
#     Tab 19: Budget Shift (HR 1990)
#     Tab 20: Budget (Other Principles)
#     Tab 21: Net-Zero by Principle
#     Tab 22: Net-Zero Summary
#     Tab 23: Net-Zero Shifts
#     Tab 24: Budget Categories
#     Tab 25: Group Transitions
# =============================================================================

dir.create("output", recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Helper: write a data frame to a sheet with bold headers, auto column widths,
# and Excel autoFilter.
# -----------------------------------------------------------------------------
write_sheet <- function(wb, sheet, df) {
  df <- df %>% mutate(across(where(is.factor), as.character))
  writeData(wb, sheet, df)
  n_cols <- ncol(df)
  addStyle(wb, sheet, createStyle(textDecoration = "bold"),
           rows = 1, cols = seq_len(n_cols), gridExpand = TRUE)
  setColWidths(wb, sheet, cols = seq_len(n_cols), widths = "auto")
  addFilter(wb, sheet, row = 1, cols = seq_len(n_cols))
}

# -----------------------------------------------------------------------------
# Combined workbook. "Contents" placeholder is added first so the TOC tab is
# always the leftmost sheet. Content is written at the end of this script.
# -----------------------------------------------------------------------------
wb <- createWorkbook()
addWorksheet(wb, "Contents")

# =============================================================================
# Part A – Figure data (from NationalCarbonBudgets and DataSankeyWide)
# =============================================================================

# Helper: return the allocation-principle label (principle names are already display names)
make_principle_label <- function(AllocationPrinciple, HistoricResponsibility) {
  as.character(AllocationPrinciple)
}

# Column title and value labels for the responsibility weight variable
AF_COL    <- "Responsibility Weight (α)"
LABEL_AF0  <- LabelWeightResponsibility[["0"]]
LABEL_AF05 <- LabelWeightResponsibility[["0.5"]]
LABEL_AF1  <- LabelWeightResponsibility[["1"]]

# Country set used in sample-country figures (defined in 06_plot_sample_countries.R)
SamplePrinciples <- c("Historic Responsibility from 1990", "Equality", "Capability")

# ── Figure 1: National carbon budgets for sample countries ────────────────────

DataFig1 <- NationalCarbonBudgets %>%
  filter(
    Country             %in% SampleCountries,
    TempTarget          %in% c(1.5, 2),
    AllocationPrinciple %in% SamplePrinciples,
    !is.na(NationalCarbonBudget)
  ) %>%
  mutate(`Allocation Principle` = make_principle_label(AllocationPrinciple, HistoricResponsibility)) %>%
  select(
    Country,
    `Temperature Target (°C)`       = TempTarget,
    `Allocation Principle`,
    !!AF_COL                        := WeightResponsibility,
    `National Carbon Budget (MtCO2)` = NationalCarbonBudget
  ) %>%
  arrange(`Temperature Target (°C)`, Country, `Allocation Principle`, !!AF_COL)

# ── Figure 2: Implied net-zero years for sample countries ─────────────────────

DataFig2 <- NationalCarbonBudgets %>%
  filter(
    Country             %in% SampleCountries,
    TempTarget          %in% c(1.5, 2),
    AllocationPrinciple %in% SamplePrinciples
  ) %>%
  mutate(`Allocation Principle` = make_principle_label(AllocationPrinciple, HistoricResponsibility)) %>%
  select(
    Country,
    `Temperature Target (°C)` = TempTarget,
    `Allocation Principle`,
    !!AF_COL                  := WeightResponsibility,
    `Implied Net-Zero Year`   = ImplicitNetZero
  ) %>%
  arrange(`Temperature Target (°C)`, Country, `Allocation Principle`, !!AF_COL)

# ── Figure 3: Per-capita emissions trends ─────────────────────────────────────

DataFig3 <- bind_rows(
  DataGlobalCarbonBudget %>%
    filter(
      Country    %in% SampleCountries,
      Accounting %in% c("Territorial Emissions", "Consumption Emissions")
    ) %>%
    left_join(
      DataUNPopulation %>%
        filter(Country %in% SampleCountries) %>%
        select(Country, Year, Population),
      by = c("Country", "Year")
    ) %>%
    mutate(
      EmissionsPerCapita = EmissionsMtCO2 / Population * 1e6,
      EmissionsType      = Accounting
    ),
  DataGlobalCarbonBudget %>%
    filter(iso3c == "WLD", Accounting == "World") %>%
    left_join(
      DataUNPopulation %>% filter(iso3c == "WLD") %>% select(Year, Population),
      by = "Year"
    ) %>%
    mutate(
      EmissionsPerCapita = EmissionsMtCO2 / Population * 1e6,
      EmissionsType      = "World average",
      Country            = NA_character_
    ) %>%
    cross_join(tibble(Country = SampleCountries)) %>%
    rename(Country = Country.y)
) %>%
  select(
    Country,
    Year,
    `Emissions Type`              = EmissionsType,
    `Emissions per Capita (tCO2)` = EmissionsPerCapita
  ) %>%
  arrange(Country, `Emissions Type`, Year)

# ── Figure 4 & Extended Data Figure 2: Scatter coloured by embodied-emissions balance ──

.ew <- DataGlobalCarbonBudget %>%
  filter(
    Year       %in% 1990:YearEnd,
    Accounting %in% c("Territorial Emissions", "Consumption Emissions")
  ) %>%
  select(iso3c, Year, Accounting, EmissionsMtCO2) %>%
  pivot_wider(names_from = Accounting, values_from = EmissionsMtCO2)

EmissionsBalance_supp <- left_join(
  .ew %>%
    filter(Year == YearEnd) %>%
    mutate(
      EmDiff_Pct = (`Consumption Emissions` - `Territorial Emissions`) /
                   `Territorial Emissions` * 100
    ) %>%
    select(iso3c, EmDiff_Pct),
  .ew %>%
    group_by(iso3c) %>%
    summarise(
      CumEmDiff_Pct = (sum(`Consumption Emissions`, na.rm = TRUE) -
                       sum(`Territorial Emissions`, na.rm = TRUE)) /
                       sum(`Territorial Emissions`, na.rm = TRUE) * 100,
      .groups = "drop"
    ),
  by = "iso3c"
)
rm(.ew)

make_scatter_balance_data <- function(eu_panel) {
  panel_countries <- CountryAssumptions %>%
    filter(iso3c != "ROW", EUMemberState == eu_panel) %>%
    pull(Country)

  base <- NationalCarbonBudgets %>%
    filter(
      Country    %in% panel_countries,
      TempTarget %in% c(1.5, 2)
    ) %>%
    mutate(
      PrincipleLabel = make_principle_label(AllocationPrinciple, HistoricResponsibility)
    )

  base %>%
    filter(WeightResponsibility == 1) %>%
    left_join(
      base %>%
        filter(WeightResponsibility == 0) %>%
        select(Country, AllocationPrinciple, HistoricResponsibility, TempTarget,
               TerritorialNetZero = ImplicitNetZero,
               TerritorialBudget  = NationalCarbonBudget),
      by = c("Country", "AllocationPrinciple", "HistoricResponsibility", "TempTarget")
    ) %>%
    rename(
      ConsumptionNetZero = ImplicitNetZero,
      ConsumptionBudget  = NationalCarbonBudget
    ) %>%
    mutate(
      DifferenceNetZeros = ConsumptionNetZero - TerritorialNetZero
    ) %>%
    left_join(CountryAssumptions %>% select(Country, iso3c), by = "Country") %>%
    left_join(EmissionsBalance_supp, by = "iso3c") %>%
    mutate(
      EmbodiedBalance_Pct = if_else(
        PrincipleLabel == "Historic Responsibility from 1990",
        CumEmDiff_Pct, EmDiff_Pct
      )
    ) %>%
    select(
      Country,
      `ISO Country Code`                                    = iso3c,
      `Economic Development`                                = EconDevelopment,
      `Temperature Target (°C)`                             = TempTarget,
      `Allocation Principle`                                = PrincipleLabel,
      !!paste0("Net-Zero Year (", LABEL_AF0, ")")            := TerritorialNetZero,
      !!paste0("Net-Zero Year (", LABEL_AF1, ")")            := ConsumptionNetZero,
      `Change in Net-Zero Year (years)`                     = DifferenceNetZeros,
      `Embodied Emissions Balance (%)`                      = EmbodiedBalance_Pct,
      !!paste0("National Carbon Budget - ", LABEL_AF0, " (MtCO2)") := TerritorialBudget,
      !!paste0("National Carbon Budget - ", LABEL_AF1, " (MtCO2)") := ConsumptionBudget
    ) %>%
    arrange(`Temperature Target (°C)`, `Allocation Principle`, Country)
}

DataFig4    <- make_scatter_balance_data(eu_panel = FALSE)
DataExtFig2 <- make_scatter_balance_data(eu_panel = TRUE)

# ── Figure 5: Scatter coloured by economic development ────────────────────────

.non_eu <- CountryAssumptions %>%
  filter(iso3c != "ROW", !EUMemberState) %>%
  pull(Country)

.base5 <- NationalCarbonBudgets %>%
  filter(
    Country    %in% .non_eu,
    TempTarget %in% c(1.5, 2)
  ) %>%
  mutate(PrincipleLabel = make_principle_label(AllocationPrinciple, HistoricResponsibility))

DataFig5 <- .base5 %>%
  filter(WeightResponsibility == 1) %>%
  left_join(
    .base5 %>%
      filter(WeightResponsibility == 0) %>%
      select(Country, AllocationPrinciple, HistoricResponsibility, TempTarget,
             TerritorialNetZero = ImplicitNetZero,
             TerritorialBudget  = NationalCarbonBudget),
    by = c("Country", "AllocationPrinciple", "HistoricResponsibility", "TempTarget")
  ) %>%
  rename(
    ConsumptionNetZero = ImplicitNetZero,
    ConsumptionBudget  = NationalCarbonBudget
  ) %>%
  mutate(DifferenceNetZeros = ConsumptionNetZero - TerritorialNetZero) %>%
  left_join(CountryAssumptions %>% select(Country, iso3c), by = "Country") %>%
  select(
    Country,
    `ISO Country Code`                                    = iso3c,
    `Economic Development`                                = EconDevelopment,
    `Temperature Target (°C)`                             = TempTarget,
    `Allocation Principle`                                = PrincipleLabel,
    !!paste0("Net-Zero Year (", LABEL_AF0, ")")            := TerritorialNetZero,
    !!paste0("Net-Zero Year (", LABEL_AF1, ")")            := ConsumptionNetZero,
    `Change in Net-Zero Year (years)`                     = DifferenceNetZeros,
    !!paste0("National Carbon Budget - ", LABEL_AF0, " (MtCO2)") := TerritorialBudget,
    !!paste0("National Carbon Budget - ", LABEL_AF1, " (MtCO2)") := ConsumptionBudget
  ) %>%
  arrange(`Temperature Target (°C)`, `Allocation Principle`, Country)

rm(.base5, .non_eu)

# ── Figure 6: Sankey – budget-category transitions ────────────────────────────

DataFig6 <- DataSankeyWide %>%
  mutate(across(c(category_AF0, category_AF1), as.character)) %>%
  select(
    Country,
    `ISO Country Code`                                    = iso3c,
    `Economic Development`                                = EconDevelopment,
    `Temperature Target (°C)`                             = TempTarget,
    `Allocation Principle`                                = AllocationPrinciple,
    !!paste0("Budget Category (", LABEL_AF0, ")")                  := category_AF0,
    !!paste0("Budget Category (", LABEL_AF1, ")")                  := category_AF1,
    !!paste0("National Carbon Budget - ", LABEL_AF0, " (MtCO2)")   := NationalCarbonBudget_AF0,
    !!paste0("Implied Net-Zero Year (", LABEL_AF0, ")")             := ImplicitNetZero_AF0,
    !!paste0("National Carbon Budget - ", LABEL_AF1, " (MtCO2)")   := NationalCarbonBudget_AF1,
    !!paste0("Implied Net-Zero Year (", LABEL_AF1, ")")             := ImplicitNetZero_AF1
  ) %>%
  arrange(`Temperature Target (°C)`, `Allocation Principle`, Country)

# ── Extended Data Figure 1: Annual vs cumulative embodied-emissions balance ───

DataExtFig1 <- DataEmDiff %>%
  select(
    `ISO Country Code`          = iso3c,
    `Economic Development`      = EconDevelopment,
    !!paste0("Annual Embodied Emissions Balance at ", YearEnd, " (%)") := EmissionsDiff_Pct,
    !!paste0("Cumulative Embodied Emissions Balance 1990–", YearEnd, " (%)") := CumEmissionsDiff_Pct
  ) %>%
  arrange(`Economic Development`, `ISO Country Code`)

# ── Extended Data Figures 3–6: Implied net-zero year for all countries ────────

make_netzero_data <- function(temp_target, eu_panel) {
  panel_countries <- if (eu_panel) {
    CountryAssumptions$Country[CountryAssumptions$EUMemberState]
  } else {
    CountryAssumptions$Country[!CountryAssumptions$EUMemberState]
  }

  NationalCarbonBudgets %>%
    filter(
      TempTarget == temp_target,
      Country    %in% panel_countries
    ) %>%
    mutate(`Allocation Principle` = make_principle_label(AllocationPrinciple, HistoricResponsibility)) %>%
    left_join(CountryAssumptions %>% select(Country, iso3c), by = "Country") %>%
    select(
      Country,
      `ISO Country Code`               = iso3c,
      `Economic Development`           = EconDevelopment,
      `Temperature Target (°C)`        = TempTarget,
      `Allocation Principle`,
      !!AF_COL                         := WeightResponsibility,
      `Implied Net-Zero Year`          = ImplicitNetZero,
      `National Carbon Budget (MtCO2)` = NationalCarbonBudget
    ) %>%
    arrange(Country, `Allocation Principle`, !!AF_COL)
}

# ── Extended Data Figures 7–10: National carbon budgets for all countries ─────

make_budget_data <- function(temp_target, eu_panel) {
  panel_countries <- if (eu_panel) {
    CountryAssumptions$Country[CountryAssumptions$EUMemberState]
  } else {
    CountryAssumptions$Country[
      !CountryAssumptions$EUMemberState & CountryAssumptions$iso3c != "ROW"
    ]
  }

  NationalCarbonBudgets %>%
    filter(
      TempTarget           == temp_target,
      Country              %in% panel_countries,
      WeightResponsibility %in% c(0, 1)
    ) %>%
    mutate(`Allocation Principle` = make_principle_label(AllocationPrinciple, HistoricResponsibility)) %>%
    left_join(CountryAssumptions %>% select(Country, iso3c), by = "Country") %>%
    select(
      Country,
      `ISO Country Code`               = iso3c,
      `Economic Development`           = EconDevelopment,
      `Temperature Target (°C)`        = TempTarget,
      `Allocation Principle`,
      !!AF_COL                         := WeightResponsibility,
      `National Carbon Budget (MtCO2)` = NationalCarbonBudget
    ) %>%
    arrange(Country, `Allocation Principle`, !!AF_COL)
}

DataExtFig3  <- make_netzero_data(1.5, FALSE)
DataExtFig4  <- make_netzero_data(2,   FALSE)
DataExtFig5  <- make_netzero_data(1.5, TRUE)
DataExtFig6  <- make_netzero_data(2,   TRUE)
DataExtFig7  <- make_budget_data(1.5,  FALSE)
DataExtFig8  <- make_budget_data(2,    FALSE)
DataExtFig9  <- make_budget_data(1.5,  TRUE)
DataExtFig10 <- make_budget_data(2,    TRUE)

# ── Write Full Results and figure sheets ──────────────────────────────────────

addWorksheet(wb, "Full Results")
write_sheet(wb, "Full Results",
            NationalCarbonBudgets %>%
              mutate(across(where(is.factor), as.character)) %>%
              left_join(CountryAssumptions %>% select(Country, iso3c), by = "Country") %>%
              select(
                Country,
                `ISO Country Code`                                = iso3c,
                `Economic Development`                            = EconDevelopment,
                `Temperature Target (°C)`                         = TempTarget,
                `Allocation Principle`                            = AllocationPrinciple,
                `Historic Base Year (0 = not applicable)`         = HistoricResponsibility,
                !!AF_COL                                          := WeightResponsibility,
                `National Carbon Budget (MtCO2)`                  = NationalCarbonBudget,
                `Implied Net-Zero Year`                           = ImplicitNetZero,
                `Positive Budget for All α Values`           = CompleteResultsAccounting,
                `Any Net-Zero After 2100`                         = AnyNetZeroAbove2100,
                `Same Net-Zero Year (α=0 and α=1)`      = SameImplicitNetZero
              ))

fig_tabs <- list(
  "Figure 1"               = DataFig1,
  "Figure 2"               = DataFig2,
  "Figure 3"               = DataFig3,
  "Figure 4"               = DataFig4,
  "Figure 5"               = DataFig5,
  "Figure 6"               = DataFig6,
  "Extended Data Figure 1" = DataExtFig1,
  "Extended Data Figure 2" = DataExtFig2,
  "Extended Data Figure 3" = DataExtFig3,
  "Extended Data Figure 4" = DataExtFig4,
  "Extended Data Figure 5" = DataExtFig5,
  "Extended Data Figure 6" = DataExtFig6,
  "Extended Data Figure 7" = DataExtFig7,
  "Extended Data Figure 8" = DataExtFig8,
  "Extended Data Figure 9" = DataExtFig9,
  "Extended Data Figure 10" = DataExtFig10
)

for (tab_name in names(fig_tabs)) {
  addWorksheet(wb, tab_name)
  write_sheet(wb, tab_name, fig_tabs[[tab_name]])
}

# =============================================================================
# Part B – In-text data (sample countries and all countries)
# =============================================================================

MainAllocationPrinciples <- c(
  "Historic Responsibility from 1990",
  "Equality",
  "Capability"
)

label_principle <- function(AllocationPrinciple, HistoricResponsibility) {
  as.character(AllocationPrinciple)
}

# -----------------------------------------------------------------------------
# Base data: same filter as prepare_sample_data(), AF = 0 and AF = 1 only
# -----------------------------------------------------------------------------
DataSample <- NationalCarbonBudgets %>%
  filter(
    Country             %in% SampleCountries,
    TempTarget          %in% c(1.5, 2),
    AllocationPrinciple %in% MainAllocationPrinciples,
    WeightResponsibility %in% c(0, 1)
  ) %>%
  mutate(
    PrincipleLabel = label_principle(AllocationPrinciple, HistoricResponsibility)
  )

# -----------------------------------------------------------------------------
# (a-i) Budget shift for "Historic Responsibility from 1990"
# -----------------------------------------------------------------------------
BudgetShift <- DataSample %>%
  filter(
    PrincipleLabel == "Historic Responsibility from 1990",
    !is.na(NationalCarbonBudget)
  ) %>%
  select(Country, TempTarget, PrincipleLabel, WeightResponsibility,
         NationalCarbonBudget) %>%
  pivot_wider(
    names_from  = WeightResponsibility,
    values_from = NationalCarbonBudget,
    names_prefix = "AF_"
  ) %>%
  mutate(BudgetChange_MtCO2 = AF_1 - AF_0) %>%
  rename(
    Budget_Territorial = AF_0,
    Budget_Consumption = AF_1
  ) %>%
  arrange(TempTarget, Country)

# -----------------------------------------------------------------------------
# (a-ii) Constant budgets for the remaining principles (AF=0 value = AF=1 value)
# -----------------------------------------------------------------------------
BudgetConstant <- DataSample %>%
  filter(
    PrincipleLabel != "Historic Responsibility from 1990",
    WeightResponsibility == 0,
    !is.na(NationalCarbonBudget)
  ) %>%
  select(Country, TempTarget, PrincipleLabel, NationalCarbonBudget) %>%
  rename(Budget_MtCO2 = NationalCarbonBudget) %>%
  arrange(TempTarget, Country, PrincipleLabel)

# -----------------------------------------------------------------------------
# (b) Change in implied net-zero year  (AF=1 minus AF=0, negative = earlier)
# -----------------------------------------------------------------------------
NetZeroShift <- DataSample %>%
  filter(!is.na(ImplicitNetZero), CompleteResultsAccounting == TRUE) %>%
  select(Country, TempTarget, PrincipleLabel, WeightResponsibility,
         ImplicitNetZero) %>%
  pivot_wider(
    names_from  = WeightResponsibility,
    values_from = ImplicitNetZero,
    names_prefix = "AF_"
  ) %>%
  mutate(NetZeroChange_Years = AF_1 - AF_0) %>%
  rename(
    NetZero_Territorial = AF_0,
    NetZero_Consumption = AF_1
  ) %>%
  arrange(TempTarget, Country, PrincipleLabel)

NetZeroShift_Summary <- NetZeroShift %>%
  group_by(Country, TempTarget) %>%
  summarise(
    Min_Change_Years = min(NetZeroChange_Years, na.rm = TRUE),
    Max_Change_Years = max(NetZeroChange_Years, na.rm = TRUE),
    N_Principles     = n(),
    .groups = "drop"
  ) %>%
  arrange(TempTarget, Country)

# -----------------------------------------------------------------------------
# Console output – sample countries
# -----------------------------------------------------------------------------
message("\n========== BUDGET: Historic Responsibility from 1990 (AF=0 → AF=1) ==========")
message("Positive = larger budget under consumption accounting\n")
for (tt in c(1.5, 2)) {
  message(sprintf("--- %.1f°C ---", tt))
  BudgetShift %>%
    filter(TempTarget == tt) %>%
    pwalk(function(Country, TempTarget, PrincipleLabel,
                   Budget_Territorial, Budget_Consumption, BudgetChange_MtCO2, ...) {
      message(sprintf(
        "  %-20s  territorial: %7.0f MtCO2  consumption: %7.0f MtCO2  change: %+7.0f MtCO2",
        Country, Budget_Territorial, Budget_Consumption, BudgetChange_MtCO2
      ))
    })
}

message("\n========== BUDGET: Constant principles (same for all WeightResponsibility values) ==========\n")
for (tt in c(1.5, 2)) {
  message(sprintf("--- %.1f°C ---", tt))
  BudgetConstant %>%
    filter(TempTarget == tt) %>%
    pwalk(function(Country, TempTarget, PrincipleLabel, Budget_MtCO2, ...) {
      message(sprintf(
        "  %-20s  %-40s  %7.0f MtCO2",
        Country, PrincipleLabel, Budget_MtCO2
      ))
    })
}

message("\n========== NET-ZERO SHIFT (years): AF=0 → AF=1 ==========")
message("Negative = net-zero earlier under consumption accounting\n")
for (tt in c(1.5, 2)) {
  message(sprintf("--- %.1f°C ---", tt))
  NetZeroShift_Summary %>%
    filter(TempTarget == tt) %>%
    pwalk(function(Country, TempTarget, Min_Change_Years, Max_Change_Years,
                   N_Principles, ...) {
      message(sprintf(
        "  %-20s  %+.1f to %+.1f years  (across %d principles)",
        Country, Min_Change_Years, Max_Change_Years, N_Principles
      ))
    })
}

message("\n--- Net-zero detail by principle ---")
print(as.data.frame(
  NetZeroShift %>%
    select(Country, TempTarget, PrincipleLabel,
           NetZero_Territorial, NetZero_Consumption, NetZeroChange_Years)
))

# =============================================================================
# Part C – In-text data for all countries
# =============================================================================

econ_levels <- c("High", "Upper-middle", "Lower-middle", "Low")

IsoLookup <- CountryAssumptions %>%
  select(Country, iso3c) %>%
  distinct()

.EmissionsWide <- DataGlobalCarbonBudget %>%
  filter(
    Year       %in% 1990:YearEnd,
    Accounting %in% c("Territorial Emissions", "Consumption Emissions")
  ) %>%
  select(iso3c, Year, Accounting, EmissionsMtCO2) %>%
  pivot_wider(names_from = Accounting, values_from = EmissionsMtCO2)

.EmissionsAnnual <- .EmissionsWide %>%
  filter(Year == YearEnd) %>%
  mutate(
    EmissionsDiff_MtCO2 = round(`Consumption Emissions` - `Territorial Emissions`, 1),
    EmissionsDiff_Pct   = round(
      (`Consumption Emissions` - `Territorial Emissions`) / `Territorial Emissions` * 100,
      1
    )
  ) %>%
  select(iso3c, EmissionsDiff_MtCO2, EmissionsDiff_Pct)

.EmissionsCumul <- .EmissionsWide %>%
  group_by(iso3c) %>%
  summarise(
    .CumTerr = sum(`Territorial Emissions`,  na.rm = TRUE),
    .CumCons = sum(`Consumption Emissions`,  na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(
    CumEmissionsDiff_MtCO2 = round(.CumCons - .CumTerr, 0),
    CumEmissionsDiff_Pct   = round((.CumCons - .CumTerr) / .CumTerr * 100, 1)
  ) %>%
  select(iso3c, CumEmissionsDiff_MtCO2, CumEmissionsDiff_Pct)

EmissionsDiff <- .EmissionsAnnual %>%
  left_join(.EmissionsCumul, by = "iso3c")

rm(.EmissionsWide, .EmissionsAnnual, .EmissionsCumul)

AllCountries <- CountryAssumptions %>%
  filter(iso3c != "ROW", EUMemberState == FALSE) %>%
  pull(Country)

DataAll <- NationalCarbonBudgets %>%
  filter(
    Country    %in% AllCountries,
    TempTarget %in% c(1.5, 2)
  ) %>%
  mutate(
    AllocationPrinciple = factor(AllocationPrinciple,
                                 levels = c("Historic Responsibility from 1990",
                                            "Equality",
                                            "Capability")),
    EconDevelopment = factor(EconDevelopment, levels = econ_levels)
  ) %>%
  left_join(IsoLookup, by = "Country")

# -----------------------------------------------------------------------------
# (a) Net-zero year shift: AF=0 → AF=1, binned in 5-year intervals
# -----------------------------------------------------------------------------
NetZeroWide <- DataAll %>%
  filter(WeightResponsibility %in% c(0, 1)) %>%
  select(Country, iso3c, EconDevelopment, TempTarget, AllocationPrinciple,
         WeightResponsibility, NationalCarbonBudget, ImplicitNetZero) %>%
  pivot_wider(
    names_from  = WeightResponsibility,
    values_from = c(NationalCarbonBudget, ImplicitNetZero),
    names_sep   = "_AF"
  ) %>%
  left_join(EmissionsDiff, by = "iso3c") %>%
  mutate(
    NegBudget_AF0   = NationalCarbonBudget_AF0 <= 0,
    NegBudget_AF1   = NationalCarbonBudget_AF1 <= 0,
    LateNetZero_AF0 = !NegBudget_AF0 & !is.na(ImplicitNetZero_AF0) & ImplicitNetZero_AF0 > 2100,
    LateNetZero_AF1 = !NegBudget_AF1 & !is.na(ImplicitNetZero_AF1) & ImplicitNetZero_AF1 > 2100,
    NetZeroChange_Years = ImplicitNetZero_AF1 - ImplicitNetZero_AF0,
    BinStart = if_else(
      !is.na(NetZeroChange_Years) & !LateNetZero_AF0 & !LateNetZero_AF1,
      floor(NetZeroChange_Years / 5) * 5,
      NA_real_
    ),
    NetZeroChangeBin = case_when(
      NegBudget_AF0   & NegBudget_AF1   ~ "Neg. budget under both α=0 and α=1",
      NegBudget_AF0                      ~ "Neg. budget under α=0",
      NegBudget_AF1                      ~ "Neg. budget under α=1",
      LateNetZero_AF0 & LateNetZero_AF1 ~ "Net-zero after 2100 under both α=0 and α=1",
      LateNetZero_AF0                    ~ "Net-zero after 2100 under α=0",
      LateNetZero_AF1                    ~ "Net-zero after 2100 under α=1",
      is.na(NetZeroChange_Years)         ~ "N/A",
      TRUE                               ~ sprintf("[%+d, %+d)", BinStart, BinStart + 5L)
    )
  ) %>%
  arrange(TempTarget, AllocationPrinciple, BinStart, iso3c)

# -----------------------------------------------------------------------------
# (b) Group transitions
# -----------------------------------------------------------------------------
Transitions <- NetZeroWide %>%
  mutate(
    NegBudget_Transition = case_when(
      !NegBudget_AF0 &  NegBudget_AF1 ~ "Enters neg. budget",
       NegBudget_AF0 & !NegBudget_AF1 ~ "Leaves neg. budget",
      TRUE                             ~ NA_character_
    ),
    LateNetZero_Transition = case_when(
      !LateNetZero_AF0 &  LateNetZero_AF1 ~ "Enters late net-zero",
       LateNetZero_AF0 & !LateNetZero_AF1 ~ "Leaves late net-zero",
      TRUE                                ~ NA_character_
    )
  ) %>%
  filter(!is.na(NegBudget_Transition) | !is.na(LateNetZero_Transition)) %>%
  select(TempTarget, AllocationPrinciple, iso3c, Country, EconDevelopment,
         NegBudget_Transition, LateNetZero_Transition,
         Budget_AF0 = NationalCarbonBudget_AF0,
         Budget_AF1 = NationalCarbonBudget_AF1,
         NetZero_AF0 = ImplicitNetZero_AF0,
         NetZero_AF1 = ImplicitNetZero_AF1,
         EmissionsDiff_MtCO2) %>%
  arrange(TempTarget, AllocationPrinciple, NegBudget_Transition, LateNetZero_Transition, iso3c)

# -----------------------------------------------------------------------------
# Console output – all countries
# -----------------------------------------------------------------------------

message("\n\n========== COUNTRY DETAIL (sorted by net-zero change) ==========")
message(sprintf(
  "Columns: iso3c | EconDev | ΔNetZero (yrs) | EmDiff at %d (MtCO2, %%) | CumEmDiff 1990-%d (MtCO2, %%)\n",
  YearEnd, YearEnd
))

for (tt in c(1.5, 2)) {
  for (pr in levels(DataAll$AllocationPrinciple)) {
    message(sprintf("\n--- %.1f°C | %s ---", tt, pr))
    NetZeroWide %>%
      filter(TempTarget == tt, AllocationPrinciple == pr) %>%
      arrange(NetZeroChange_Years) %>%
      pwalk(function(iso3c, EconDevelopment, NetZeroChange_Years,
                     NetZeroChangeBin, EmissionsDiff_MtCO2, EmissionsDiff_Pct,
                     CumEmissionsDiff_MtCO2, CumEmissionsDiff_Pct, ...) {
        message(sprintf(
          "  %-6s  %-14s  %+6.1f yrs  [%s]  EmDiff: %+8.1f MtCO2 (%+6.1f%%)  CumEmDiff: %+9.0f MtCO2 (%+6.1f%%)",
          iso3c, as.character(EconDevelopment),
          ifelse(is.na(NetZeroChange_Years),    NA, NetZeroChange_Years),
          NetZeroChangeBin,
          ifelse(is.na(EmissionsDiff_MtCO2),   NA, EmissionsDiff_MtCO2),
          ifelse(is.na(EmissionsDiff_Pct),      NA, EmissionsDiff_Pct),
          ifelse(is.na(CumEmissionsDiff_MtCO2), NA, CumEmissionsDiff_MtCO2),
          ifelse(is.na(CumEmissionsDiff_Pct),   NA, CumEmissionsDiff_Pct)
        ))
      })
  }
}

message("\n\n========== GROUP TRANSITIONS (AF=0 → AF=1) ==========")
message("Countries that enter or leave the negative-budget or late-net-zero group\n")

for (tt in c(1.5, 2)) {
  message(sprintf("===== %.1f°C =====", tt))
  for (pr in levels(DataAll$AllocationPrinciple)) {
    sub <- Transitions %>% filter(TempTarget == tt, AllocationPrinciple == pr)
    if (nrow(sub) == 0) {
      message(sprintf("\n  -- %s: no transitions --", pr))
      next
    }
    message(sprintf("\n  -- %s --", pr))
    for (trans_type in c("Enters neg. budget", "Leaves neg. budget",
                         "Enters late net-zero", "Leaves late net-zero")) {
      rows <- sub %>% filter(
        NegBudget_Transition   == trans_type |
        LateNetZero_Transition == trans_type
      )
      if (nrow(rows) == 0) next
      message(sprintf("  %s (n=%d):", trans_type, nrow(rows)))
      rows %>% pwalk(function(iso3c, EconDevelopment,
                              Budget_AF0, Budget_AF1,
                              NetZero_AF0, NetZero_AF1,
                              EmissionsDiff_MtCO2, ...) {
        message(sprintf(
          "    %-6s %-14s  Budget: %+8.0f → %+8.0f MtCO2   NetZero: %s → %s   EmDiff: %+7.1f MtCO2",
          iso3c, as.character(EconDevelopment),
          Budget_AF0, Budget_AF1,
          ifelse(is.na(NetZero_AF0), "  N/A ", sprintf("%.1f", NetZero_AF0)),
          ifelse(is.na(NetZero_AF1), "  N/A ", sprintf("%.1f", NetZero_AF1)),
          ifelse(is.na(EmissionsDiff_MtCO2), NA, EmissionsDiff_MtCO2)
        ))
      })
    }
  }
  message("")
}

# ── Budget category counts per α, allocation principle, temp target, economic development ──
GroupSummary <- DataAll %>%
  filter(WeightResponsibility %in% c(0, 1)) %>%
  mutate(
    BudgetCategory = case_when(
      NationalCarbonBudget <= 0                             ~ "Negative budget",
      !is.na(ImplicitNetZero) & ImplicitNetZero > 2100      ~ "Net-zero after 2100",
      TRUE                                                  ~ "Net-zero before 2100"
    )
  ) %>%
  count(TempTarget, AllocationPrinciple, WeightResponsibility, EconDevelopment, BudgetCategory) %>%
  arrange(TempTarget, AllocationPrinciple, WeightResponsibility, EconDevelopment, BudgetCategory) %>%
  rename(
    `Temperature Target (°C)` = TempTarget,
    `Allocation Principle`    = AllocationPrinciple,
    !!AF_COL                  := WeightResponsibility,
    `Economic Development`    = EconDevelopment,
    `Budget Category`         = BudgetCategory,
    `Number of Countries`     = n
  ) %>%
  mutate(across(where(is.factor), as.character))

# =============================================================================
# Write Part B + C sheets to workbook
# =============================================================================

addWorksheet(wb, "Budget Shift (HR 1990)")
write_sheet(wb, "Budget Shift (HR 1990)",
            BudgetShift %>%
              rename(
                `Temperature Target (°C)`                                        = TempTarget,
                `Allocation Principle`                                           = PrincipleLabel,
                !!paste0("National Carbon Budget - ", LABEL_AF0, " (MtCO2)")    := Budget_Territorial,
                !!paste0("National Carbon Budget - ", LABEL_AF1, " (MtCO2)")    := Budget_Consumption,
                `Change in Carbon Budget (MtCO2)`                               = BudgetChange_MtCO2
              ))

addWorksheet(wb, "Budget (Other Principles)")
write_sheet(wb, "Budget (Other Principles)",
            BudgetConstant %>%
              rename(
                `Temperature Target (°C)`        = TempTarget,
                `Allocation Principle`           = PrincipleLabel,
                `National Carbon Budget (MtCO2)` = Budget_MtCO2
              ))

addWorksheet(wb, "Net-Zero by Principle")
write_sheet(wb, "Net-Zero by Principle",
            NetZeroShift %>%
              select(Country, TempTarget, PrincipleLabel,
                     NetZero_Territorial, NetZero_Consumption, NetZeroChange_Years) %>%
              rename(
                `Temperature Target (°C)`                                  = TempTarget,
                `Allocation Principle`                                     = PrincipleLabel,
                !!paste0("Implied Net-Zero Year (", LABEL_AF0, ")")        := NetZero_Territorial,
                !!paste0("Implied Net-Zero Year (", LABEL_AF1, ")")        := NetZero_Consumption,
                `Change in Net-Zero Year (years)`                          = NetZeroChange_Years
              ))

addWorksheet(wb, "Net-Zero Summary")
write_sheet(wb, "Net-Zero Summary",
            NetZeroShift_Summary %>%
              rename(
                `Temperature Target (°C)`             = TempTarget,
                `Min Change in Net-Zero Year (years)` = Min_Change_Years,
                `Max Change in Net-Zero Year (years)` = Max_Change_Years,
                `Number of Allocation Principles`     = N_Principles
              ))

addWorksheet(wb, "Net-Zero Shifts")
write_sheet(wb, "Net-Zero Shifts",
            NetZeroWide %>%
              select(
                `Temperature Target (°C)`                              = TempTarget,
                `Allocation Principle`                                 = AllocationPrinciple,
                `Net-Zero Change Bin (5-year intervals)`               = NetZeroChangeBin,
                `Bin Start (years)`                                    = BinStart,
                `ISO Country Code`                                     = iso3c,
                Country,
                `Economic Development`                                 = EconDevelopment,
                `Change in Net-Zero Year (years)`                      = NetZeroChange_Years,
                !!paste0("Implied Net-Zero Year (", LABEL_AF0, ")")             := ImplicitNetZero_AF0,
                !!paste0("Implied Net-Zero Year (", LABEL_AF1, ")")             := ImplicitNetZero_AF1,
                !!paste0("National Carbon Budget - ", LABEL_AF0, " (MtCO2)")   := NationalCarbonBudget_AF0,
                !!paste0("National Carbon Budget - ", LABEL_AF1, " (MtCO2)")   := NationalCarbonBudget_AF1,
                !!paste0("Annual Embodied Emissions Balance at ", YearEnd, " (MtCO2)") := EmissionsDiff_MtCO2,
                !!paste0("Annual Embodied Emissions Balance at ", YearEnd, " (%)") := EmissionsDiff_Pct,
                !!paste0("Cumulative Embodied Emissions Balance 1990–", YearEnd, " (MtCO2)") := CumEmissionsDiff_MtCO2,
                !!paste0("Cumulative Embodied Emissions Balance 1990–", YearEnd, " (%)") := CumEmissionsDiff_Pct
              ) %>%
              mutate(across(where(is.factor), as.character)) %>%
              arrange(`Temperature Target (°C)`, `Allocation Principle`, `Bin Start (years)`,
                      `ISO Country Code`))


addWorksheet(wb, "Budget Categories")
write_sheet(wb, "Budget Categories", GroupSummary)

addWorksheet(wb, "Group Transitions")
write_sheet(wb, "Group Transitions",
            Transitions %>%
              select(
                `Temperature Target (°C)`                              = TempTarget,
                `Allocation Principle`                                 = AllocationPrinciple,
                `ISO Country Code`                                     = iso3c,
                Country,
                `Economic Development`                                 = EconDevelopment,
                `Negative Budget Transition`                           = NegBudget_Transition,
                `Late Net-Zero Transition`                             = LateNetZero_Transition,
                !!paste0("National Carbon Budget - ", LABEL_AF0, " (MtCO2)")   := Budget_AF0,
                !!paste0("National Carbon Budget - ", LABEL_AF1, " (MtCO2)")   := Budget_AF1,
                !!paste0("Implied Net-Zero Year (", LABEL_AF0, ")")             := NetZero_AF0,
                !!paste0("Implied Net-Zero Year (", LABEL_AF1, ")")             := NetZero_AF1,
                !!paste0("Annual Embodied Emissions Balance at ", YearEnd, " (MtCO2)") := EmissionsDiff_MtCO2
              ) %>%
              mutate(across(where(is.factor), as.character)))


# =============================================================================
# Contents tab – publication info and table of contents
# =============================================================================

toc_tabs <- tibble(
  `Tab` = c(
    "Full Results",
    "Figure 1", "Figure 2", "Figure 3",
    "Figure 4", "Figure 5", "Figure 6",
    "Extended Data Figure 1", "Extended Data Figure 2",
    "Extended Data Figure 3", "Extended Data Figure 4",
    "Extended Data Figure 5", "Extended Data Figure 6",
    "Extended Data Figure 7", "Extended Data Figure 8",
    "Extended Data Figure 9", "Extended Data Figure 10",
    "Budget Shift (HR 1990)", "Budget (Other Principles)",
    "Net-Zero by Principle", "Net-Zero Summary",
    "Net-Zero Shifts", "Budget Categories", "Group Transitions"
  ),
  `Description` = c(
    paste0("Complete national carbon budget dataset for all countries, allocation principles, temperature targets, and Responsibility Weight (α) values (α = 0–1 in steps of 0.05). α = 0: ", LABEL_AF0, ". α = 1: ", LABEL_AF1, "."),
    paste0("National carbon budgets (GtCO₂) for sample countries across the full range of Responsibility Weight (α = 0–1). α = 0: ", LABEL_AF0, "; α = 1: ", LABEL_AF1, "."),
    paste0("Implied net-zero years for sample countries across the full range of Responsibility Weight (α = 0–1)."),
    "Per-capita CO₂ emissions trends for sample countries (historical data).",
    paste0("Net-zero year scatter coloured by embodied emissions balance; non-EU countries. Compares α = 0 (", LABEL_AF0, ") vs α = 1 (", LABEL_AF1, ")."),
    paste0("Net-zero year scatter coloured by economic development; non-EU countries. Compares α = 0 (", LABEL_AF0, ") vs α = 1 (", LABEL_AF1, ")."),
    paste0("Budget-category transitions when shifting from α = 0 (", LABEL_AF0, ") to α = 1 (", LABEL_AF1, "); non-EU countries."),
    "Annual vs cumulative embodied emissions balance for all non-EU countries.",
    paste0("Net-zero year scatter coloured by embodied emissions balance; EU member states. Compares α = 0 (", LABEL_AF0, ") vs α = 1 (", LABEL_AF1, ")."),
    "Implied net-zero years for all non-EU countries at 1.5°C.",
    "Implied net-zero years for all non-EU countries at 2°C.",
    "Implied net-zero years for EU member states at 1.5°C.",
    "Implied net-zero years for EU member states at 2°C.",
    "National carbon budgets for all non-EU countries at 1.5°C.",
    "National carbon budgets for all non-EU countries at 2°C.",
    "National carbon budgets for EU member states at 1.5°C.",
    "National carbon budgets for EU member states at 2°C.",
    paste0("Change in national carbon budget when shifting from α = 0 (", LABEL_AF0, ") to α = 1 (", LABEL_AF1, "); Historic Responsibility from 1990 principle only."),
    "National carbon budgets under Equality and Capability principles (identical across all Responsibility Weight values).",
    paste0("Implied net-zero year for α = 0 (", LABEL_AF0, ") and α = 1 (", LABEL_AF1, "), and the shift between them, by country, temperature target, and allocation principle."),
    paste0("Summary of net-zero year shifts (min/max range across allocation principles) when moving from α = 0 to α = 1, by country and temperature target."),
    paste0("Net-zero year change data for all non-EU countries when shifting from α = 0 (", LABEL_AF0, ") to α = 1 (", LABEL_AF1, "), categorised in 5-year bins, with embodied emissions balance."),
    "Number of non-EU countries in each budget category (negative budget; net-zero before or after 2100) by allocation principle, temperature target, α value (0 or 1), and economic development group.",
    paste0("Countries entering or leaving the negative-budget or late-net-zero categories when shifting from α = 0 (", LABEL_AF0, ") to α = 1 (", LABEL_AF1, ").")
  ),
  `Source Figure` = c(
    "—",
    "Figure 1", "Figure 2", "Figure 3",
    "Figure 4", "Figure 5", "Figure 6",
    "Extended Data Figure 1", "Extended Data Figure 2",
    "Extended Data Figure 3", "Extended Data Figure 4",
    "Extended Data Figure 5", "Extended Data Figure 6",
    "Extended Data Figure 7", "Extended Data Figure 8",
    "Extended Data Figure 9", "Extended Data Figure 10",
    "In-text", "In-text", "In-text", "In-text",
    "In-text", "In-text", "In-text"
  )
)

# Publication info block (rows 1–4)
pub_info <- data.frame(
  Field = c("Publication", "Authors", "Journal", "DOI"),
  Value = c(
    "Net-Zero Targets under Dual Emissions Accounting and Global Effort Sharing",
    "[Authors]",
    "[Journal]",
    "[DOI]"
  )
)
writeData(wb, "Contents", pub_info, colNames = FALSE, startRow = 1)
addStyle(wb, "Contents", createStyle(textDecoration = "bold"),
         rows = 1:4, cols = 1, gridExpand = TRUE)
addStyle(wb, "Contents", createStyle(textDecoration = "bold", fontSize = 12),
         rows = 1, cols = 2)

# Section header (row 6)
writeData(wb, "Contents", "Data tabs in this workbook",
          startRow = 6, startCol = 1, colNames = FALSE)
addStyle(wb, "Contents", createStyle(textDecoration = "bold", fontSize = 11),
         rows = 6, cols = 1)

# TOC table starting at row 7 — write all columns first, then overwrite the
# Tab column with HYPERLINK formulas so clicking navigates to the sheet.
writeData(wb, "Contents", toc_tabs, startRow = 7)
addStyle(wb, "Contents", createStyle(textDecoration = "bold"),
         rows = 7, cols = 1:3, gridExpand = TRUE)

tab_links <- sprintf('HYPERLINK("#\'%s\'!A1","%s")', toc_tabs$Tab, toc_tabs$Tab)
writeFormula(wb, "Contents", tab_links, startRow = 8, startCol = 1)
addStyle(wb, "Contents",
         createStyle(fontColour = "#0563C1", textDecoration = "underline"),
         rows = 8:(7 + nrow(toc_tabs)), cols = 1, gridExpand = TRUE)
setColWidths(wb, "Contents", cols = 1, widths = 32)
setColWidths(wb, "Contents", cols = 2, widths = 90)
setColWidths(wb, "Contents", cols = 3, widths = 28)
setColWidths(wb, "Contents", cols = 4, widths = 14)  # Field column from pub_info
addStyle(wb, "Contents",
         createStyle(wrapText = TRUE),
         rows = 8:(7 + nrow(toc_tabs)), cols = 2,
         gridExpand = TRUE, stack = TRUE)
setRowHeights(wb, "Contents",
              rows = 8:(7 + nrow(toc_tabs)),
              heights = 45)

# =============================================================================
# Save
# =============================================================================
out_path <- "output/SupplementaryData.xlsx"
saveWorkbook(wb, out_path, overwrite = TRUE)
message("Written: ", out_path)
