# =============================================================================
# 04_allocation_functions.R  –  Allocation-principle helper functions
# =============================================================================
# This module defines:
#   GlobalEmissionCurves  – global linear emission pathway to net-zero
#   AnnualCapability()    – Capability allocation
#
# AnnualCapability references GlobalEmissionCurves, which is therefore
# computed first.
# =============================================================================

# -----------------------------------------------------------------------------
# Global emission pathways
# -----------------------------------------------------------------------------
# For each temperature target, construct a linear decline from YearEnd emissions
# to zero at the year implied by the remaining carbon budget.
# Each row gives the average annual global emissions (MtCO₂) for that year.
#
# GlobalNetZero = YearBudget + 2 × Budget / E_YearEnd
# (factor 2 arises from integrating a linear ramp from E_YearEnd to 0)

GlobalEmissionCurves <-
  map_dfr(CarbonBudget$TempTarget, function(t) {
    budget_MtCO2 <- CarbonBudget$BudgetGtCO2[CarbonBudget$TempTarget == t] * 1e3
    E_YearEnd <- DataGlobalCarbonBudget %>%
      filter(Country == "World", Accounting == "World", Year == YearEnd) %>%
      pull(EmissionsMtCO2)
    GlobalNetZero <- YearBudget + 2 * budget_MtCO2 / E_YearEnd

    # Linear decline: emissions(x) = max(0, E_YearEnd × (1 - (x - YearBudget)/(NZ - YearBudget)))
    linear_e <- function(x) {
      pmax(0, E_YearEnd * (1 - (x - YearBudget) / (GlobalNetZero - YearBudget)))
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

AnnualCapability <- function(country_iso3c, temp_target) {

  # All non-World, non-EU-aggregate country iso3c codes
  all_iso3c <- CountryAssumptions %>%
    filter(!iso3c %in% c("WLD", "EUU")) %>%
    pull(iso3c)

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

