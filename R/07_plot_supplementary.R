# =============================================================================
# 07_plot_supplementary.R  –  Supplementary figures
# =============================================================================
# Produces (one file per temperature target unless noted):
#
#   output/Graphs/ResultsCompareTargets{1.5|2}{All|EU}.png
#     Scatter: territorial vs consumption-based net-zero year, by allocation
#
#   output/Graphs/ResultsCarbonBudgets{1.5|2}.png
#     Bar chart: national carbon budgets for major emitters
#
#   output/Graphs/ResultsCarbonBudgetsEU{1.5|2}.png
#     Bar chart: national carbon budgets for EU member states
#
#   output/Graphs/ResultsAllCountries{1.5|2}.png
#     Point plot: implied net-zero year, all non-EU countries
#
#   output/Graphs/ResultsEUMemberStates{1.5|2}.png
#     Point plot: implied net-zero year, EU member states
#
# The DataForSupplementary data table (one Excel sheet per plot) is written
# by 08_export.R.
# =============================================================================

dir.create("output/Graphs", recursive = TRUE, showWarnings = FALSE)

# Shared theme elements -------------------------------------------------------
theme_supp <- theme_bw(base_size = 7.4) +
  theme(
    legend.position   = "bottom",
    strip.background  = element_rect(fill = "black", color = "transparent"),
    strip.text        = element_text(color = "white"),
    axis.text.x       = element_text(angle = 45, vjust = 1, hjust = 1)
  )

# Helper: build "CombHistOther" label used in scatter plots -------------------
add_comb_label <- function(df) {
  df %>% mutate(
    CombHistOther = case_when(
      HistoricResponsibility == 0 & AllocationPrinciple == "Annual Equal per Capita"
                                       ~ "Annual Equality",
      HistoricResponsibility == 0      ~ as.character(AllocationPrinciple),
      TRUE                             ~ paste0("Historic Responsibility from ",
                                                HistoricResponsibility)
    )
  )
}

# Panel letter labels for scatter plots
scatter_labels <- tibble(
  CombHistOther = c("Annual Equality", "Capability",
                    "Contraction and Convergence",
                    "Historic Responsibility from 1990",
                    "Historic Responsibility from 2000",
                    "Historic Responsibility from 2010"),
  Label = paste0(letters[1:6], ")")
)

# -----------------------------------------------------------------------------
# Loop over temperature targets and country sets
# -----------------------------------------------------------------------------
# DataForSupplementary is assembled here and passed to 08_export.R
DataForSupplementary <- list()

for (t in c(1.5, 2)) {
  for (country_set in c("All", "EU")) {

    is_eu <- country_set == "EU"

    # Countries in this panel
    panel_iso <- CountryAssumptions %>%
      filter(
        iso3c != "ROW",
        EUMemberState == is_eu
      ) %>%
      pull(Country)

    # Base data: consumption-based (f=1) and territorial (f=0) net-zero years
    base_data <- NationalCarbonBudgets %>%
      filter(
        Country %in% panel_iso,
        TempTarget == t,
        HistoricResponsibility %in% c(0, 1990, 2000, 2010),
        AllocationPrinciple != "Grandfathering"
      ) %>%
      add_comb_label()

    PlotData <- base_data %>%
      filter(AccountingFramework == 1) %>%
      left_join(
        base_data %>%
          filter(AccountingFramework == 0) %>%
          select(Country, AllocationPrinciple, HistoricResponsibility,
                 TerritorialNetZero  = ImplicitNetZero,
                 TerritorialBudget   = NationalCarbonBudget),
        by = c("Country", "AllocationPrinciple", "HistoricResponsibility")
      ) %>%
      rename(
        ConsumptionBasedNetZero   = ImplicitNetZero,
        ConsumptionBasedBudget    = NationalCarbonBudget
      ) %>%
      mutate(DifferenceNetZeros = ConsumptionBasedNetZero - TerritorialNetZero)

    # Store for export
    DataForSupplementary[[paste0(t, country_set)]] <- PlotData %>%
      select(AllocationPrinciple, HistoricResponsibility, EconDevelopment, Country,
             ConsumptionBasedNetZero, ConsumptionBasedBudget,
             TerritorialNetZero, TerritorialBudget, DifferenceNetZeros) %>%
      arrange(AllocationPrinciple, HistoricResponsibility, EconDevelopment, Country) %>%
      mutate(
        HistoricResponsibility = na_if(as.character(HistoricResponsibility), "0"),
        Country = as.character(Country)
      )

    # ── Scatter plot: territorial vs consumption-based net-zero ──────────────
    n_dev <- if (is_eu) 2L else 4L   # number of development categories shown
    dev_cols <- scico(4, palette = "roma")[1:n_dev]

    ResultsCompareTargets <- ggplot(PlotData) +
      # Reference line (both frameworks agree)
      geom_abline(intercept = 0, slope = 1, color = "gray") +
      # Grey boxes for "out of range" annotations
      annotate("rect", xmin = 2088, xmax = 2102, ymin = 2025, ymax = 2050,
               fill = "lightgray") +
      annotate("rect", xmin = 2018, xmax = 2032, ymin = 2080, ymax = 2100,
               fill = "lightgray") +
      # Main scatter
      geom_point(
        aes(
          x = if_else(ConsumptionBasedBudget < 0, 2025,
              if_else(ConsumptionBasedNetZero > 2100 | TerritorialNetZero > 2100,
                      2095, ConsumptionBasedNetZero)),
          y = if_else(ConsumptionBasedBudget < 0,
              case_when(EconDevelopment == "High"         ~ 2090,
                        EconDevelopment == "Upper-middle" ~ 2085,
                        EconDevelopment == "Lower-middle" ~ 2080,
                        TRUE                              ~ 2075),
              if_else(ConsumptionBasedNetZero > 2100 | TerritorialNetZero > 2100,
              case_when(EconDevelopment == "High"         ~ 2025,
                        EconDevelopment == "Upper-middle" ~ 2030,
                        EconDevelopment == "Lower-middle" ~ 2035,
                        TRUE                              ~ 2040),
              TerritorialNetZero)),
          color = EconDevelopment
        )
      ) +
      # Count labels for ">2100" box
      geom_text(
        data = PlotData %>%
          filter(ConsumptionBasedNetZero > 2100 | TerritorialNetZero > 2100) %>%
          count(EconDevelopment, CombHistOther) %>%
          filter(n > 0),
        aes(label = n, x = 2097,
            y = case_when(EconDevelopment == "High"         ~ 2025,
                          EconDevelopment == "Upper-middle" ~ 2030,
                          EconDevelopment == "Lower-middle" ~ 2035,
                          TRUE                              ~ 2040)),
        hjust = 0, vjust = 0.5, size = 2
      ) +
      annotate("text", x = 2095, y = 2045, label = ">2100", size = 2) +
      # Count labels for "negative budget" box
      geom_text(
        data = PlotData %>%
          filter(ConsumptionBasedBudget < 0) %>%
          count(EconDevelopment, CombHistOther) %>%
          filter(n > 0),
        aes(label = n, x = 2027,
            y = case_when(EconDevelopment == "High"         ~ 2090,
                          EconDevelopment == "Upper-middle" ~ 2085,
                          EconDevelopment == "Lower-middle" ~ 2080,
                          TRUE                              ~ 2075)),
        hjust = 0, vjust = 0.5, size = 2
      ) +
      annotate("text", x = 2025, y = 2095,
               label = expression(paste("<0 CO"[2])), size = 2) +
      # Country labels for outliers
      geom_text_repel(
        data = PlotData %>%
          filter(abs(TerritorialNetZero - ConsumptionBasedNetZero) > 4,
                 TerritorialNetZero < 2100, ConsumptionBasedNetZero < 2100),
        aes(y = TerritorialNetZero, x = ConsumptionBasedNetZero, label = Country),
        size = 2, nudge_x = 0.5, nudge_y = 0.5,
        min.segment.length = 0, max.overlaps = 30,
        xlim = c(2020, 2100), ylim = c(2020, 2100)
      ) +
      # Panel letters
      geom_text(
        data = scatter_labels,
        aes(x = 2018, y = 2022, label = Label),
        size = 2.5, color = "black"
      ) +
      facet_wrap(~ CombHistOther) +
      scale_color_manual(values = dev_cols) +
      coord_cartesian(xlim = c(2018, 2100), ylim = c(2018, 2100)) +
      labs(
        y     = "Producing countries bear responsibility",
        x     = "Consuming countries bear responsibility",
        color = "Economic development"
      ) +
      theme_bw(base_size = 9) +
      theme(
        legend.position   = "bottom",
        legend.margin     = margin(0, 0, 0, 0),
        strip.background  = element_rect(fill = "black", color = "transparent"),
        strip.text        = element_text(color = "white")
      )

    ggsave(
      sprintf("output/Graphs/ResultsCompareTargets%s%s.png", t, country_set),
      ResultsCompareTargets,
      width = 6, height = 4, units = "in", dpi = 300
    )
  }

  # ── Bar charts: national carbon budgets ──────────────────────────────────

  # Separate large and small emitters to allow independent x-scales
  large_emitters     <- c("United States", "China", "India", "European Union")
  large_emitters_eu  <- c("Germany", "Italy", "Spain", "France", "Poland", "Romania")

  budget_data <- NationalCarbonBudgets %>%
    filter(
      TempTarget == t,
      AccountingFramework %in% c(0, 1),
      Country != "Rest of world",
      AllocationPrinciple != "Grandfathering",
      HistoricResponsibility %in% c(0, 1990, 2000, 2010)
    ) %>%
    add_comb_label()

  for (eu_panel in c(FALSE, TRUE)) {
    panel_countries <- if (eu_panel) {
      CountryAssumptions$Country[CountryAssumptions$EUMemberState]
    } else {
      CountryAssumptions$Country[!CountryAssumptions$EUMemberState & CountryAssumptions$iso3c != "ROW"]
    }
    large <- if (eu_panel) large_emitters_eu else large_emitters

    plot_data_bar <- budget_data %>%
      filter(Country %in% panel_countries) %>%
      mutate(LargeBudgets = Country %in% large)

    PlotBudgets <- ggplot(plot_data_bar) +
      geom_col(
        aes(y = Country, x = NationalCarbonBudget / 1e3,
            fill = as.character(AccountingFramework)),
        position = position_dodge2()
      ) +
      facet_grid2(
        LargeBudgets ~ AllocationPrinciple + HistoricResponsibility,
        scales = "free", space = "free_y", axes = "x", independent = "x",
        labeller = labeller(HistoricResponsibility = LabelHistoricResponsibility)
      ) +
      scale_fill_scico_d(labels = LabelAccountingFramework, palette = "roma") +
      labs(
        x    = expression(paste("National Carbon Budget (GtCO"[2], ")")),
        fill = "", y = NULL
      ) +
      theme_bw(base_size = 7.4) +
      theme(
        legend.position      = "bottom",
        strip.background.x   = element_rect(fill = "black", color = "transparent"),
        strip.text.x         = element_text(color = "white"),
        strip.background.y   = element_blank(),
        strip.text.y         = element_blank()
      )

    suffix <- if (eu_panel) "EU" else ""
    ggsave(
      sprintf("output/Graphs/ResultsCarbonBudgets%s%s.png", t, suffix),
      PlotBudgets,
      width = 8.38, height = 5.11, units = "in", dpi = 300
    )
  }

  # ── Point plots: implied net-zero year, all countries ─────────────────────

  for (eu_panel in c(FALSE, TRUE)) {
    panel_countries <- if (eu_panel) {
      CountryAssumptions$Country[CountryAssumptions$EUMemberState]
    } else {
      CountryAssumptions$Country[!CountryAssumptions$EUMemberState]
    }

    plot_data_nz <- NationalCarbonBudgets %>%
      filter(
        TempTarget == t,
        HistoricResponsibility %in% c(0, 1990, 2000, 2010),
        AllocationPrinciple != "Grandfathering",
        Country %in% panel_countries
      )

    PlotNetZero <- ggplot(plot_data_nz) +
      geom_point(
        aes(x = ImplicitNetZero, y = Country,
            color = AccountingFramework, fill = AccountingFramework),
        shape = 25
      ) +
      geom_text(
        size = 2, color = "palegreen4",
        aes(x = 2060, y = Country,
            label = if_else(AnyNetZeroAbove2100 & AccountingFramework == 0.5,
                            "Net-zero after 2100", ""))
      ) +
      geom_text(
        size = 2, color = "maroon3",
        aes(x = 2060, y = Country,
            label = if_else(isFALSE(CompleteResultsAccounting) & AccountingFramework == 0.5,
                            "Negative carbon budget", ""))
      ) +
      facet_grid(
        ~ AllocationPrinciple + HistoricResponsibility,
        labeller = labeller(HistoricResponsibility = LabelHistoricResponsibility)
      ) +
      scale_color_scico(
        palette = "roma", breaks = c(1, 0.5, 0),
        labels  = c("Consumer responsibility", "Symmetrical", "Producer responsibility")
      ) +
      scale_fill_scico(
        palette = "roma", breaks = c(1, 0.5, 0),
        labels  = c("Consumer responsibility", "Symmetrical", "Producer responsibility")
      ) +
      coord_cartesian(xlim = c(2020, 2100)) +
      guides(color = guide_colorbar(barwidth = 20)) +
      labs(x = "Net-zero year", y = NULL, color = "", fill = "") +
      theme_supp

    suffix <- if (eu_panel) "EUMemberStates" else "AllCountries"
    ggsave(
      sprintf("output/Graphs/%s%s.png", suffix, t),
      PlotNetZero,
      width = 8.38, height = 5.11, units = "in", dpi = 300
    )
  }
}
