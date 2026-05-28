# =============================================================================
# 09_text_data.R  –  In-text data for sample countries and all countries
# =============================================================================
# Part A: sample countries (China, EU, South Africa, Sweden, United States)
#   Output: output/TextData_SampleCountries.xlsx   (four sheets)
#
# Part B: all non-EU countries
#   Output: output/TextData_AllCountries.xlsx
# =============================================================================

dir.create("output", recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# Part A  –  In-text data for sample countries
# =============================================================================
# Quantifies how shifting from full territorial (AccountingFramework = 0) to
# full consumption-based (AccountingFramework = 1) accounting changes:
#   (a) the national carbon budget (MtCO2)
#   (b) the implied net-zero year
#
# For carbon budgets:
#   - "Historic Responsibility from 1990" varies with AccountingFramework
#     (historic emissions differ under territorial vs consumption accounting).
#     → budget shift is reported.
#   - All other principles (Annual Equality, Capability, Equal Cumulative per
#     Capita without historic responsibility) are constant across AF.
#     → single budget value is reported (same for AF=0 and AF=1).
#
# For net-zero years all principles vary and are reported as a range.
# =============================================================================

MainAllocationPrinciples <- c(
  "Equal Cumulative per Capita",
  "Annual Equal per Capita",
  "Capability"
)

label_principle <- function(AllocationPrinciple, HistoricResponsibility) {
  case_when(
    HistoricResponsibility == 0 & AllocationPrinciple == "Annual Equal per Capita"
                                     ~ "Annual Equality",
    HistoricResponsibility == 0      ~ as.character(AllocationPrinciple),
    TRUE                             ~ paste0("Historic Responsibility from ",
                                              HistoricResponsibility)
  )
}

# -----------------------------------------------------------------------------
# Base data: same filter as prepare_sample_data(), AF = 0 and AF = 1 only
# -----------------------------------------------------------------------------
DataSample <- NationalCarbonBudgets %>%
  filter(
    Country                %in% SampleCountries,
    TempTarget             %in% c(1.5, 2),
    HistoricResponsibility %in% c(0, 1990),
    AllocationPrinciple    %in% MainAllocationPrinciples,
    AccountingFramework    %in% c(0, 1)
  ) %>%
  mutate(
    PrincipleLabel = label_principle(AllocationPrinciple, HistoricResponsibility)
  )

# -----------------------------------------------------------------------------
# (a-i) Budget shift for "Historic Responsibility from 1990"
#        (the only principle where the budget varies with AccountingFramework)
# -----------------------------------------------------------------------------
BudgetShift <- DataSample %>%
  filter(
    PrincipleLabel == "Historic Responsibility from 1990",
    !is.na(NationalCarbonBudget)
  ) %>%
  select(Country, TempTarget, PrincipleLabel, AccountingFramework,
         NationalCarbonBudget) %>%
  pivot_wider(
    names_from  = AccountingFramework,
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
    AccountingFramework == 0,          # same as AF=1, so pick one
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
  select(Country, TempTarget, PrincipleLabel, AccountingFramework,
         ImplicitNetZero) %>%
  pivot_wider(
    names_from  = AccountingFramework,
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
# Console output
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

message("\n========== BUDGET: Constant principles (same for all AccountingFramework values) ==========\n")
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

# -----------------------------------------------------------------------------
# Write to Excel (four sheets)
# -----------------------------------------------------------------------------
wb_sample <- createWorkbook()

addWorksheet(wb_sample, "Budget_HR1990_Shift")
writeData(wb_sample, "Budget_HR1990_Shift", BudgetShift)

addWorksheet(wb_sample, "Budget_Constant")
writeData(wb_sample, "Budget_Constant", BudgetConstant)

addWorksheet(wb_sample, "NetZero_Detail")
writeData(wb_sample, "NetZero_Detail",
          NetZeroShift %>%
            select(Country, TempTarget, PrincipleLabel,
                   NetZero_Territorial, NetZero_Consumption, NetZeroChange_Years))

addWorksheet(wb_sample, "NetZero_Summary")
writeData(wb_sample, "NetZero_Summary", NetZeroShift_Summary)

saveWorkbook(wb_sample, "output/TextData_SampleCountries.xlsx", overwrite = TRUE)
message("\nWritten: output/TextData_SampleCountries.xlsx")

# =============================================================================
# Part B  –  In-text data for all countries
# =============================================================================
# For each combination of temperature target and allocation principle:
#   (a) Countries binned by change in implied net-zero year (AF=0 → AF=1),
#       in 5-year intervals. iso3c used for brevity.
#   (b) Each country entry includes the consumption-minus-territorial emission
#       difference for the most recent observation year (YearEnd).
#   (c) Counts of countries with negative budgets or net-zero > 2100, by
#       economic development level, for AF ∈ {0, 0.5, 1}.
#
# Uses the same country filter as the supplementary scatter plots:
#   non-EU-member-state countries only, same three allocation principles.
# =============================================================================

econ_levels <- c("High", "Upper-middle", "Lower-middle", "Low")

# CombHistOther label (mirrors 07_plot_all_countries.R)
add_comb_label_all <- function(df) {
  df %>% mutate(
    CombHistOther = factor(case_when(
      HistoricResponsibility == 0 & AllocationPrinciple == "Annual Equal per Capita"
                                       ~ "Annual Equality",
      HistoricResponsibility == 0      ~ as.character(AllocationPrinciple),
      TRUE                             ~ paste0("Historic Responsibility from ",
                                                HistoricResponsibility)
    ), levels = c("Historic Responsibility from 1990",
                  "Annual Equality",
                  "Capability"))
  )
}

# -----------------------------------------------------------------------------
# Supporting lookups
# -----------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------
# Base data
# -----------------------------------------------------------------------------
AllCountries <- CountryAssumptions %>%
  filter(iso3c != "ROW", EUMemberState == FALSE) %>%
  pull(Country)

DataAll <- NationalCarbonBudgets %>%
  filter(
    Country %in% AllCountries,
    TempTarget %in% c(1.5, 2),
    HistoricResponsibility %in% c(0, 1990),
    !AllocationPrinciple %in% c("Grandfathering", "Contraction and Convergence"),
    !(AllocationPrinciple == "Equal Cumulative per Capita" & HistoricResponsibility == 0)
  ) %>%
  add_comb_label_all() %>%
  mutate(EconDevelopment = factor(EconDevelopment, levels = econ_levels)) %>%
  left_join(IsoLookup, by = "Country")

# -----------------------------------------------------------------------------
# (a) Net-zero year shift: AF=0 → AF=1, binned in 5-year intervals
# -----------------------------------------------------------------------------
NetZeroWide <- DataAll %>%
  filter(AccountingFramework %in% c(0, 1)) %>%
  select(Country, iso3c, EconDevelopment, TempTarget, CombHistOther,
         AccountingFramework, NationalCarbonBudget, ImplicitNetZero) %>%
  pivot_wider(
    names_from  = AccountingFramework,
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
      NegBudget_AF0   & NegBudget_AF1   ~ "Neg. budget (both AF)",
      NegBudget_AF0                      ~ "Neg. budget at AF=0",
      NegBudget_AF1                      ~ "Neg. budget at AF=1",
      LateNetZero_AF0 & LateNetZero_AF1 ~ "Net-zero after 2100 (both AF)",
      LateNetZero_AF0                    ~ "Net-zero after 2100 at AF=0",
      LateNetZero_AF1                    ~ "Net-zero after 2100 at AF=1",
      is.na(NetZeroChange_Years)         ~ "N/A",
      TRUE                               ~ sprintf("[%+d, %+d)", BinStart, BinStart + 5L)
    )
  ) %>%
  arrange(TempTarget, CombHistOther, BinStart, iso3c)

BinSummary <- NetZeroWide %>%
  group_by(TempTarget, CombHistOther, NetZeroChangeBin, BinStart, EconDevelopment) %>%
  summarise(
    N         = n(),
    Countries = paste(sort(iso3c), collapse = ", "),
    .groups   = "drop"
  ) %>%
  arrange(TempTarget, CombHistOther, BinStart, EconDevelopment)

BinCompact <- NetZeroWide %>%
  group_by(TempTarget, CombHistOther, NetZeroChangeBin, BinStart) %>%
  summarise(
    N              = n(),
    Countries_iso3c = paste(sort(iso3c), collapse = ", "),
    .groups        = "drop"
  ) %>%
  arrange(TempTarget, CombHistOther, BinStart)

# -----------------------------------------------------------------------------
# (b) Transitions: which countries enter / leave the negative-budget and
#     late-net-zero groups when shifting from AF=0 to AF=1?
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
  select(TempTarget, CombHistOther, iso3c, Country, EconDevelopment,
         NegBudget_Transition, LateNetZero_Transition,
         Budget_AF0 = NationalCarbonBudget_AF0,
         Budget_AF1 = NationalCarbonBudget_AF1,
         NetZero_AF0 = ImplicitNetZero_AF0,
         NetZero_AF1 = ImplicitNetZero_AF1,
         EmissionsDiff_MtCO2) %>%
  arrange(TempTarget, CombHistOther, NegBudget_Transition, LateNetZero_Transition, iso3c)

# -----------------------------------------------------------------------------
# (c) Counts of negative budgets / net-zero > 2100 by EconDevelopment
# -----------------------------------------------------------------------------
NegAndLate <- DataAll %>%
  filter(AccountingFramework %in% c(0, 0.5, 1)) %>%
  group_by(TempTarget, CombHistOther, EconDevelopment, AccountingFramework) %>%
  summarise(
    N_Countries        = n_distinct(Country),
    N_NegBudget        = sum(NationalCarbonBudget <= 0, na.rm = TRUE),
    N_NetZeroAfter2100 = sum(ImplicitNetZero > 2100,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(TempTarget, CombHistOther, EconDevelopment, AccountingFramework)

# -----------------------------------------------------------------------------
# Console output
# -----------------------------------------------------------------------------
message("\n========== NET-ZERO YEAR CHANGE (AF=0 → AF=1), 5-YEAR BINS ==========")
message("Negative = earlier net-zero under consumption-based accounting")
message(sprintf("Emissions difference = consumption minus territorial at %d (MtCO2)\n", YearEnd))

for (tt in c(1.5, 2)) {
  for (pr in levels(DataAll$CombHistOther)) {
    message(sprintf("\n--- %.1f°C | %s ---", tt, pr))

    BinCompact %>%
      filter(TempTarget == tt, CombHistOther == pr) %>%
      pwalk(function(NetZeroChangeBin, N, Countries_iso3c, ...) {
        message(sprintf("  %18s  (n=%2d)  %s", NetZeroChangeBin, N, Countries_iso3c))
      })
  }
}

message("\n\n========== COUNTRY DETAIL (sorted by net-zero change) ==========")
message(sprintf(
  "Columns: iso3c | EconDev | ΔNetZero (yrs) | EmDiff at %d (MtCO2, %%) | CumEmDiff 1990-%d (MtCO2, %%)\n",
  YearEnd, YearEnd
))

for (tt in c(1.5, 2)) {
  for (pr in levels(DataAll$CombHistOther)) {
    message(sprintf("\n--- %.1f°C | %s ---", tt, pr))
    NetZeroWide %>%
      filter(TempTarget == tt, CombHistOther == pr) %>%
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
  for (pr in levels(DataAll$CombHistOther)) {
    sub <- Transitions %>% filter(TempTarget == tt, CombHistOther == pr)
    if (nrow(sub) == 0) {
      message(sprintf("\n  -- %s: no transitions --", pr))
      next
    }
    message(sprintf("\n  -- %s --", pr))
    for (trans_type in c("Enters neg. budget", "Leaves neg. budget",
                         "Enters late net-zero", "Leaves late net-zero")) {
      rows <- sub %>% filter(
        NegBudget_Transition    == trans_type |
        LateNetZero_Transition  == trans_type
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

message("\n\n========== NEGATIVE BUDGETS & NET-ZERO > 2100 ==========")
message("N_Neg  = negative carbon budget (no valid net-zero year)")
message("N_Late = net-zero implied after 2100 (mutually exclusive with N_Neg)\n")

for (tt in c(1.5, 2)) {
  message(sprintf("===== %.1f°C =====", tt))
  for (pr in levels(DataAll$CombHistOther)) {
    message(sprintf("\n  -- %s --", pr))
    message(sprintf("  %-16s  %4s   AF=0: neg / late   AF=0.5: neg / late   AF=1: neg / late",
                    "EconDevelopment", "N"))
    NegAndLate %>%
      filter(TempTarget == tt, CombHistOther == pr) %>%
      pivot_wider(
        id_cols     = c(TempTarget, CombHistOther, EconDevelopment, N_Countries),
        names_from  = AccountingFramework,
        values_from = c(N_NegBudget, N_NetZeroAfter2100),
        names_sep   = "_AF"
      ) %>%
      pwalk(function(EconDevelopment, N_Countries,
                     N_NegBudget_AF0,   N_NetZeroAfter2100_AF0,
                     N_NegBudget_AF0.5, N_NetZeroAfter2100_AF0.5,
                     N_NegBudget_AF1,   N_NetZeroAfter2100_AF1, ...) {
        message(sprintf(
          "  %-16s  %4d   %2d / %2d              %2d / %2d               %2d / %2d",
          EconDevelopment, N_Countries,
          N_NegBudget_AF0,   N_NetZeroAfter2100_AF0,
          N_NegBudget_AF0.5, N_NetZeroAfter2100_AF0.5,
          N_NegBudget_AF1,   N_NetZeroAfter2100_AF1
        ))
      })
  }
  message("")
}

# -----------------------------------------------------------------------------
# Write to Excel
# -----------------------------------------------------------------------------
wb_all <- createWorkbook()

addWorksheet(wb_all, "CountryDetail")
writeData(wb_all, "CountryDetail",
          NetZeroWide %>%
            select(TempTarget, CombHistOther, NetZeroChangeBin, BinStart,
                   iso3c, Country, EconDevelopment,
                   NetZeroChange_Years,
                   NetZero_AF0       = ImplicitNetZero_AF0,
                   NetZero_AF1       = ImplicitNetZero_AF1,
                   Budget_AF0        = NationalCarbonBudget_AF0,
                   Budget_AF1        = NationalCarbonBudget_AF1,
                   EmissionsDiff_MtCO2, EmissionsDiff_Pct,
                   CumEmissionsDiff_MtCO2, CumEmissionsDiff_Pct) %>%
            arrange(TempTarget, CombHistOther, BinStart, iso3c))

addWorksheet(wb_all, "BinSummary_ByEcon")
writeData(wb_all, "BinSummary_ByEcon", BinSummary)

addWorksheet(wb_all, "BinSummary_Compact")
writeData(wb_all, "BinSummary_Compact", BinCompact)

addWorksheet(wb_all, "GroupTransitions")
writeData(wb_all, "GroupTransitions", Transitions)

addWorksheet(wb_all, "NegBudget_Late_Counts")
writeData(wb_all, "NegBudget_Late_Counts", NegAndLate)

saveWorkbook(wb_all, "output/TextData_AllCountries.xlsx", overwrite = TRUE)
message("\nWritten: output/TextData_AllCountries.xlsx")
