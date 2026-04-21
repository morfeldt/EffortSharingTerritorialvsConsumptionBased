# =============================================================================
# 05_calculate_budgets.R  –  National carbon budget calculations
# =============================================================================
# Computes national carbon budgets for all combinations of:
#   AllocationPrinciple  × TempTarget × AccountingFramework × HistoricResponsibility × Country
#
# The inner loop is parallelised with doParallel / foreach.
#
# Output: NationalCarbonBudgets – one row per combination (valid combos only)
# =============================================================================

# Allocation principles and loop dimensions -----------------------------------
AllocationPrinciples <- c(
  "Equal Cumulative per Capita",
  "Annual Equal per Capita",
  "Grandfathering",
  "Contraction and Convergence",
  "Capability"
)

# AccountingFramework values: 0 = full territorial, 1 = full consumption-based
AccountingFrameworkValues <- seq(0, 1, by = 0.05)  # 21 values

# Historic responsibility base years (0 = no historic responsibility)
HistoricYears <- c(0, seq(1990, 2021, by = 5))

# iso3c codes to compute budgets for (excluding World); names used only for output
BudgetCountries <- CountryAssumptions$iso3c[CountryAssumptions$Country != "World"]

# -----------------------------------------------------------------------------
# Parallelised budget calculation
# -----------------------------------------------------------------------------
message(sprintf("Starting budget calculation on %d cores ...", NoCores))
Cluster <- makeCluster(NoCores)
registerDoParallel(Cluster)

NationalCarbonBudgets <-
  foreach(
    alloc = AllocationPrinciples,
    .combine = "rbind"
  ) %:%
  foreach(
    temp = CarbonBudget$TempTarget,
    .combine = "rbind"
  ) %:%
  foreach(
    acct = AccountingFrameworkValues,
    .combine = "rbind"
  ) %:%
  foreach(
    base_year = HistoricYears,
    .combine = "rbind"
  ) %:%
  foreach(
    country = BudgetCountries,   # holds iso3c; name resolved only in output
    .combine  = "rbind",
    .packages = c("dplyr", "purrr"),
    .export   = c("DataSSPFutureGDP", "SSPScenario",
                  "AnnualCapability", "AnnualPerCapitaConvergence")
  ) %dopar% {

    # ----- Skip invalid combinations ----------------------------------------
    # "Equal Cumulative per Capita" requires a historic base year (base_year != 0).
    # All other principles use no historic base year (base_year == 0).
    valid_combo <- (alloc == "Equal Cumulative per Capita" & base_year != 0) |
                   (alloc != "Equal Cumulative per Capita" & base_year == 0)
    if (!valid_combo) return(NULL)

    # ----- Skip ECPC if consumption emissions are incomplete for base_year:YearEnd --
    # Countries with NA consumption data in any year of the historical
    # responsibility window cannot produce a valid ECPC budget.
    if (alloc == "Equal Cumulative per Capita") {
      cons_vals <- DataGlobalCarbonBudget$EmissionsMtCO2[
        DataGlobalCarbonBudget$iso3c      == country &
        DataGlobalCarbonBudget$Accounting == "Consumption Emissions" &
        DataGlobalCarbonBudget$Year       %in% base_year:YearEnd
      ]
      if (any(is.na(cons_vals))) return(NULL)
    }

    # ----- Year at which global net-zero is reached --------------------------
    budget_MtCO2 <- CarbonBudget$BudgetGtCO2[CarbonBudget$TempTarget == temp] * 1e3
    E2021 <- DataGlobalCarbonBudget$EmissionsMtCO2[
      DataGlobalCarbonBudget$iso3c       == "WLD" &
      DataGlobalCarbonBudget$Accounting  == "World" &
      DataGlobalCarbonBudget$Year        == YearEnd
    ]
    GlobalNetZero <- YearBudget + 2 * budget_MtCO2 / E2021

    # ----- Compute national budget ------------------------------------------
    NationalBudget <- switch(alloc,

      # Equal Cumulative per Capita (Eq. 1 in paper)
      # Budget = (global budget + cumulative global emissions from base_year to 2021)
      #          × (cumulative population share from base_year to net-zero)
      #          – (cumulative national emissions from base_year to 2021, blended)
      "Equal Cumulative per Capita" = {
        global_hist <- DataGlobalCarbonBudget %>%
          filter(iso3c == "WLD", Accounting == "World",
                 Year %in% base_year:YearEnd) %>%
          pull(EmissionsMtCO2) %>% sum()
        pop_c <- DataPopulation %>%
          filter(iso3c == country,
                 Year %in% base_year:ceiling(GlobalNetZero)) %>%
          pull(Population) %>% sum()
        pop_w <- DataPopulation %>%
          filter(iso3c == "WLD",
                 Year %in% base_year:ceiling(GlobalNetZero)) %>%
          pull(Population) %>% sum()
        hist_c_terr <- DataGlobalCarbonBudget %>%
          filter(iso3c == country, Accounting == "Territorial Emissions",
                 Year %in% base_year:YearEnd) %>%
          pull(EmissionsMtCO2) %>% sum()
        hist_c_cons <- DataGlobalCarbonBudget %>%
          filter(iso3c == country, Accounting == "Consumption Emissions",
                 Year %in% base_year:YearEnd) %>%
          pull(EmissionsMtCO2) %>% sum()

        (budget_MtCO2 + global_hist) * (pop_c / pop_w) -
          ((1 - acct) * hist_c_terr + acct * hist_c_cons)
      },

      # Annual Equal per Capita (Eq. 2)
      # Each year's global budget is split proportionally to population
      "Annual Equal per Capita" = {
        global_curve <- GlobalEmissionCurves %>%
          filter(TempTarget == temp, Year %in% YearBudget:YearHorizon)
        pop_c <- DataPopulation %>%
          filter(iso3c == country, Year %in% YearBudget:YearHorizon) %>%
          arrange(Year) %>% pull(Population)
        pop_w <- DataPopulation %>%
          filter(iso3c == "WLD", Year %in% YearBudget:YearHorizon) %>%
          arrange(Year) %>% pull(Population)
        sum(global_curve$EmissionsMtCO2 * pop_c / pop_w)
      },

      # Grandfathering
      # Budget = global budget × country's share of 2021 emissions (blended)
      "Grandfathering" = {
        e_terr <- DataGlobalCarbonBudget$EmissionsMtCO2[
          DataGlobalCarbonBudget$iso3c      == country &
          DataGlobalCarbonBudget$Year       == YearEnd &
          DataGlobalCarbonBudget$Accounting == "Territorial Emissions"
        ]
        e_cons <- DataGlobalCarbonBudget$EmissionsMtCO2[
          DataGlobalCarbonBudget$iso3c      == country &
          DataGlobalCarbonBudget$Year       == YearEnd &
          DataGlobalCarbonBudget$Accounting == "Consumption Emissions"
        ]
        e_world <- DataGlobalCarbonBudget$EmissionsMtCO2[
          DataGlobalCarbonBudget$iso3c      == "WLD" &
          DataGlobalCarbonBudget$Year       == YearEnd &
          DataGlobalCarbonBudget$Accounting == "World"
        ]
        budget_MtCO2 * ((1 - acct) * e_terr + acct * e_cons) / e_world
      },

      # Contraction and Convergence
      # Convergence to equal per-capita by 2050 (see AnnualPerCapitaConvergence)
      "Contraction and Convergence" = {
        AnnualPerCapitaConvergence(country, temp, acct, conv_year = 2050)
      },

      # Capability
      # Shares weighted by population² / GDP (see AnnualCapability)
      "Capability" = {
        AnnualCapability(country, temp)
      }
    )

    # ----- Derive implicit net-zero year ------------------------------------
    e_current <- {
      e_terr <- DataGlobalCarbonBudget$EmissionsMtCO2[
        DataGlobalCarbonBudget$iso3c      == country &
        DataGlobalCarbonBudget$Accounting == "Territorial Emissions" &
        DataGlobalCarbonBudget$Year       == YearEnd
      ]
      e_cons <- DataGlobalCarbonBudget$EmissionsMtCO2[
        DataGlobalCarbonBudget$iso3c      == country &
        DataGlobalCarbonBudget$Accounting == "Consumption Emissions" &
        DataGlobalCarbonBudget$Year       == YearEnd
      ]
      (1 - acct) * e_terr + acct * e_cons
    }

    # Assuming linear reduction from current emissions to zero:
    # Budget = e_current × (NetZero - YearEnd) / 2
    ImplicitNetZero <- if (isTRUE(NationalBudget > 0) && isTRUE(e_current > 0)) {
      NationalBudget / e_current * 2 + YearEnd
    } else {
      NA_real_
    }

    data.frame(
      Country                = CountryAssumptions$Country[CountryAssumptions$iso3c == country],
      EconDevelopment        = CountryAssumptions$Development[CountryAssumptions$iso3c == country],
      AllocationPrinciple    = alloc,
      TempTarget             = temp,
      AccountingFramework    = acct,
      HistoricResponsibility = base_year,
      NationalCarbonBudget   = NationalBudget,
      ImplicitNetZero        = ImplicitNetZero,
      stringsAsFactors       = FALSE
    )
  }

stopCluster(Cluster)
rm(Cluster)
message("Budget calculation complete.")

# -----------------------------------------------------------------------------
# Post-processing
# -----------------------------------------------------------------------------

# Flag whether ALL accounting-framework values give a positive budget
# (CompleteResultsAccounting = TRUE), none do (FALSE), or some do (NA).
# Also flag whether ALL yield a net-zero year after 2100.
n_acct_values <- length(AccountingFrameworkValues)   # 21

NationalCarbonBudgets <- NationalCarbonBudgets %>%
  group_by(Country, AllocationPrinciple, TempTarget, HistoricResponsibility) %>%
  mutate(
    n_positive = sum(NationalCarbonBudget > 0, na.rm = TRUE),
    CompleteResultsAccounting = case_when(
      n_positive == n_acct_values ~ TRUE,
      n_positive == 0             ~ FALSE,
      TRUE                        ~ NA
    ),
    AnyNetZeroAbove2100 = n_positive == n_acct_values &
      sum(ImplicitNetZero > 2100, na.rm = TRUE) == n_acct_values
  ) %>%
  select(-n_positive) %>%
  ungroup()

# For each country/principle/target/year group, flag whether the implied
# net-zero year is the same under full territorial (f=0) and full
# consumption-based (f=1) accounting.
NationalCarbonBudgets <- NationalCarbonBudgets %>%
  left_join(
    NationalCarbonBudgets %>%
      filter(AccountingFramework %in% c(0, 1)) %>%
      select(Country, AllocationPrinciple, TempTarget, HistoricResponsibility,
             AccountingFramework, ImplicitNetZero) %>%
      pivot_wider(
        names_from   = AccountingFramework,
        values_from  = ImplicitNetZero,
        names_prefix = "NZ_"
      ) %>%
      mutate(SameImplicitNetZero = ceiling(NZ_0) == ceiling(NZ_1)) %>%
      select(-NZ_0, -NZ_1),
    by = c("Country", "AllocationPrinciple", "TempTarget", "HistoricResponsibility")
  )

# Set factor levels for plotting
NationalCarbonBudgets <- NationalCarbonBudgets %>%
  mutate(
    AllocationPrinciple = factor(AllocationPrinciple, levels = AllocationPrinciples),
    EconDevelopment     = factor(EconDevelopment,
                                 levels = c("High", "Upper-middle",
                                            "Lower-middle", "Low",
                                            "Rest of world", "World")),
    Country             = factor(Country, levels = CountryLevels)
  )
