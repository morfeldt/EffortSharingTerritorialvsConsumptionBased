# =============================================================================
# 04_allocation_functions.R  –  Allocation-principle helper functions
# =============================================================================
# This module defines:
#   GlobalEmissionCurves          – global linear emission pathway to net-zero
#   AnnualCapability()            – Capability allocation (Supplementary Eq. 4)
#   AnnualPerCapitaConvergence()  – Contraction & Convergence (Supplementary Eq. 3)
#
# Both allocation functions reference GlobalEmissionCurves, which is therefore
# computed first.
# =============================================================================

# -----------------------------------------------------------------------------
# Global emission pathways
# -----------------------------------------------------------------------------
# For each temperature target, construct a linear decline from 2022 emissions
# to zero at the year implied by the remaining carbon budget.
# Each row gives the average annual global emissions (MtCO₂) for that year.
#
# GlobalNetZero = 2022 + 2 × Budget / E₂₀₂₁
# (factor 2 arises from integrating a linear ramp from E₂₀₂₁ to 0)

GlobalEmissionCurves <-
  map_dfr(CarbonBudget$TempTarget, function(t) {
    budget_MtCO2 <- CarbonBudget$BudgetGtCO2[CarbonBudget$TempTarget == t] * 1e3
    E2021 <- DataGlobalCarbonBudget %>%
      filter(Country == "World", Accounting == "World", Year == YearEnd) %>%
      pull(EmissionsMtCO2)
    GlobalNetZero <- YearBudget + 2 * budget_MtCO2 / E2021

    # Linear decline: emissions(x) = max(0, E2021 × (1 - (x - 2022)/(NZ - 2022)))
    linear_e <- function(x) {
      pmax(0, E2021 * (1 - (x - YearBudget) / (GlobalNetZero - YearBudget)))
    }

    tibble(TempTarget = t, Year = YearBudget:YearHorizon) %>%
      mutate(EmissionsMtCO2 = map_dbl(Year, function(y) {
        # Average emissions over the interval [y, min(y+1, GlobalNetZero)]
        t_end <- min(y + 1, GlobalNetZero)
        if (t_end <= y) 0 else (linear_e(y) + linear_e(t_end)) / 2 * (t_end - y)
      }))
  })

# -----------------------------------------------------------------------------
# Capability allocation
# -----------------------------------------------------------------------------
# Each country's share is proportional to (Population² / GDP), normalised
# across all countries in each year.  See Supplementary Equation 4.
#
# Arguments:
#   country     – character, country name matching CountryAssumptions$Country
#   temp_target – numeric temperature target (1.5 or 2)
#
# Returns: scalar budget in MtCO₂

AnnualCapability <- function(country, temp_target) {

  # All non-World, non-EU-aggregate country iso3c codes
  all_iso3c <- CountryAssumptions %>%
    filter(!Country %in% c("World", "European Union")) %>%
    pull(iso3c)

  country_iso3c <- CountryAssumptions$iso3c[CountryAssumptions$Country == country]

  ssp_year <- DataSSPFutureGDP %>%
    filter(Scenario == SSPScenario, Region %in% c(all_iso3c, country_iso3c))

  sum(map_dbl(YearBudget:YearHorizon, function(yr) {
    global_e <- GlobalEmissionCurves %>%
      filter(TempTarget == temp_target, Year == yr) %>%
      pull(EmissionsMtCO2)

    pop_all <- ssp_year %>%
      filter(Year == yr, Region %in% all_iso3c, Variable == "Population") %>%
      pull(Value)
    gdp_all <- ssp_year %>%
      filter(Year == yr, Region %in% all_iso3c, Variable == "GDP|PPP") %>%
      pull(Value)

    cap_all <- sum(pop_all^2 / gdp_all, na.rm = TRUE)

    pop_c <- ssp_year %>%
      filter(Year == yr, Region == country_iso3c, Variable == "Population") %>%
      pull(Value)
    gdp_c <- ssp_year %>%
      filter(Year == yr, Region == country_iso3c, Variable == "GDP|PPP") %>%
      pull(Value)

    if (length(global_e) != 1 || length(pop_c) != 1 || length(gdp_c) != 1) return(NA_real_)
    global_e * (pop_c^2 / gdp_c) / cap_all
  }), na.rm = TRUE)
}

# -----------------------------------------------------------------------------
# Contraction and Convergence allocation
# -----------------------------------------------------------------------------
# Shares converge linearly from current emission shares to equal per-capita
# shares by ConvYear (default 2050).  See Supplementary Equation 3.
#
# Arguments:
#   country            – character, country name
#   temp_target        – numeric temperature target (1.5 or 2)
#   accounting_weight  – numeric in [0, 1]; weight on consumption-based share
#                        (0 = territorial only, 1 = consumption-based only)
#   conv_year          – integer convergence year (default 2050)
#
# Returns: scalar budget in MtCO₂

AnnualPerCapitaConvergence <- function(country, temp_target,
                                       accounting_weight, conv_year = 2050) {

  country_iso3c <- CountryAssumptions$iso3c[CountryAssumptions$Country == country]

  # Current (YearEnd) emission share of this country
  e_terr <- DataGlobalCarbonBudget %>%
    filter(Country == country, Accounting == "Territorial Emissions",
           Year == YearEnd) %>%
    pull(EmissionsMtCO2)
  e_cons <- DataGlobalCarbonBudget %>%
    filter(Country == country, Accounting == "Consumption Emissions",
           Year == YearEnd) %>%
    pull(EmissionsMtCO2)
  e_world <- DataGlobalCarbonBudget %>%
    filter(Country == "World", Accounting == "World", Year == YearEnd) %>%
    pull(EmissionsMtCO2)

  current_share <- ((1 - accounting_weight) * e_terr +
                      accounting_weight  * e_cons) / e_world

  sum(map_dbl(YearBudget:YearHorizon, function(yr) {
    global_e <- GlobalEmissionCurves %>%
      filter(TempTarget == temp_target, Year == yr) %>%
      pull(EmissionsMtCO2)

    pop_c     <- DataPopulation %>%
      filter(iso3c == country_iso3c, Year == yr) %>%
      pull(Population)
    pop_world <- DataPopulation %>%
      filter(Country == "World", Year == yr) %>%
      pull(Population)

    if (length(global_e) != 1 || length(pop_c) != 1 || length(pop_world) != 1) return(NA_real_)

    # Linear interpolation between current share and equal per-capita share
    conv_weight <- min((yr - YearEnd) / (conv_year - YearEnd), 1)
    share <- conv_weight * (pop_c / pop_world) +
             (1 - conv_weight) * current_share

    global_e * share
  }), na.rm = TRUE)
}
