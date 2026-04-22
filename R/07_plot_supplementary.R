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

# -----------------------------------------------------------------------------
# Use legendry for nested y-axis (country + economic development group).
#
# ggh4x's guide_axis_nested() was deprecated in ggh4x 0.3.0 in favour of
# legendry::guide_axis_nested() and is now gutted in 0.3.1 (it just returns a
# plain guide_axis(), which is why the old nested styling stopped working).
# Install with: install.packages("legendry")
# -----------------------------------------------------------------------------
if (!requireNamespace("legendry", quietly = TRUE)) {
  stop("Package 'legendry' is required. Install with install.packages(\"legendry\").")
}

# Output filename map: old stem → new stem
figure_stem_map <- c(
  "ResultsCompareTargets1.5All" = "Figure4",
  "ResultsCompareTargets1.5EU"  = "Figure5",
  "AllCountries1.5"             = "SupplementaryFigure1",
  "EUMemberStates1.5"           = "SupplementaryFigure2",
  "ResultsCarbonBudgets1.5"     = "SupplementaryFigure3",
  "ResultsCarbonBudgets1.5EU"   = "SupplementaryFigure4",
  "AllCountries2"               = "SupplementaryFigure5",
  "EUMemberStates2"             = "SupplementaryFigure6",
  "ResultsCarbonBudgets2"       = "SupplementaryFigure7",
  "ResultsCarbonBudgets2EU"     = "SupplementaryFigure8"
)
figure_path <- function(stem) {
  mapped <- figure_stem_map[stem]
  sprintf("output/Graphs/%s.png", if (!is.na(mapped)) mapped else stem)
}

# Shared theme elements -------------------------------------------------------
theme_supp <- theme_bw(base_size = 7.4) +
  theme(
    legend.position    = "bottom",
    strip.background   = element_rect(fill = "black", color = "transparent"),
    strip.text         = element_text(color = "white"),
    axis.text.x        = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.grid.major.y = element_blank()
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

# Panel letter labels for scatter plots (3 panels: no C&C, no ECPC with HR=0, HR=1990 only)
scatter_labels <- tibble(
  CombHistOther = c("Annual Equality", "Capability",
                    "Historic Responsibility from 1990"),
  Label = paste0(letters[1:3], ")")
)

temp_labeller_scatter <- labeller(TempTarget = c("1.5" = "1.5°C", "2" = "2°C"))

# -----------------------------------------------------------------------------
# Figure 4 / Figure 5 – scatter plots combining both temperature targets as rows
# -----------------------------------------------------------------------------
# DataForSupplementary is assembled here and passed to 08_export.R
DataForSupplementary <- list()

for (country_set in c("All", "EU")) {

  is_eu <- country_set == "EU"

  panel_iso <- CountryAssumptions %>%
    filter(iso3c != "ROW", EUMemberState == is_eu) %>%
    pull(Country)

  base_data <- NationalCarbonBudgets %>%
    filter(
      Country %in% panel_iso,
      TempTarget %in% c(1.5, 2),
      HistoricResponsibility %in% c(0, 1990),
      !AllocationPrinciple %in% c("Grandfathering", "Contraction and Convergence"),
      !(AllocationPrinciple == "Equal Cumulative per Capita" & HistoricResponsibility == 0)
    ) %>%
    add_comb_label()

  PlotData <- base_data %>%
    filter(AccountingFramework == 1) %>%
    left_join(
      base_data %>%
        filter(AccountingFramework == 0) %>%
        select(Country, AllocationPrinciple, HistoricResponsibility, TempTarget,
               TerritorialNetZero  = ImplicitNetZero,
               TerritorialBudget   = NationalCarbonBudget),
      by = c("Country", "AllocationPrinciple", "HistoricResponsibility", "TempTarget")
    ) %>%
    rename(
      ConsumptionBasedNetZero = ImplicitNetZero,
      ConsumptionBasedBudget  = NationalCarbonBudget
    ) %>%
    mutate(DifferenceNetZeros = ConsumptionBasedNetZero - TerritorialNetZero)

  DataForSupplementary[[paste0("scatter_", country_set)]] <- PlotData %>%
    select(TempTarget, AllocationPrinciple, HistoricResponsibility, EconDevelopment, Country,
           ConsumptionBasedNetZero, ConsumptionBasedBudget,
           TerritorialNetZero, TerritorialBudget, DifferenceNetZeros) %>%
    arrange(TempTarget, AllocationPrinciple, HistoricResponsibility, EconDevelopment, Country) %>%
    mutate(
      HistoricResponsibility = na_if(as.character(HistoricResponsibility), "0"),
      Country = as.character(Country)
    )

  n_dev    <- if (is_eu) 2L else 4L
  dev_cols <- scico(4, palette = "roma")[1:n_dev]

  ResultsCompareTargets <- ggplot(PlotData) +
    geom_abline(intercept = 0, slope = 1, color = "gray") +
    annotate("rect", xmin = 2088, xmax = 2102, ymin = 2025, ymax = 2050,
             fill = "lightgray") +
    annotate("rect", xmin = 2018, xmax = 2032, ymin = 2080, ymax = 2100,
             fill = "lightgray") +
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
    geom_text(
      data = PlotData %>%
        filter(ConsumptionBasedNetZero > 2100 | TerritorialNetZero > 2100) %>%
        count(EconDevelopment, CombHistOther, TempTarget) %>%
        filter(n > 0),
      aes(label = n, x = 2097,
          y = case_when(EconDevelopment == "High"         ~ 2025,
                        EconDevelopment == "Upper-middle" ~ 2030,
                        EconDevelopment == "Lower-middle" ~ 2035,
                        TRUE                              ~ 2040)),
      hjust = 0, vjust = 0.5, size = 2
    ) +
    annotate("text", x = 2095, y = 2045, label = ">2100", size = 2) +
    geom_text(
      data = PlotData %>%
        filter(ConsumptionBasedBudget < 0) %>%
        count(EconDevelopment, CombHistOther, TempTarget) %>%
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
    geom_text_repel(
      data = PlotData %>%
        filter(abs(TerritorialNetZero - ConsumptionBasedNetZero) > 4,
               TerritorialNetZero < 2100, ConsumptionBasedNetZero < 2100),
      aes(y = TerritorialNetZero, x = ConsumptionBasedNetZero, label = Country),
      size = 2, nudge_x = 0.5, nudge_y = 0.5,
      min.segment.length = 0, max.overlaps = 30,
      xlim = c(2020, 2100), ylim = c(2020, 2100)
    ) +
    geom_text(
      data = scatter_labels,
      aes(x = 2018, y = 2022, label = Label),
      size = 2.5, color = "black"
    ) +
    facet_grid(TempTarget ~ CombHistOther, labeller = temp_labeller_scatter) +
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
    figure_path(sprintf("ResultsCompareTargets1.5%s", country_set)),
    ResultsCompareTargets,
    width = 6, height = 4, units = "in", dpi = 300
  )
}

# -----------------------------------------------------------------------------
# Supplementary figures – loop over temperature targets
# -----------------------------------------------------------------------------

for (t in c(1.5, 2)) {

  # ── Bar charts: national carbon budgets ──────────────────────────────────

  # Separate large and small emitters to allow independent x-scales
  large_emitters     <- c("United States", "China", "India", "European Union")
  large_emitters_eu  <- c("Germany", "Italy", "Spain", "France", "Poland", "Romania")

  budget_data <- NationalCarbonBudgets %>%
    filter(
      TempTarget == t,
      AccountingFramework %in% c(0, 1),
      Country != "Rest of world",
      !AllocationPrinciple %in% c("Grandfathering", "Contraction and Convergence"),
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

    # Build y_grouped factor: plain country name for large emitters and
    # "Rest of world" (no economic development bracket); "Country§EconDevelopment"
    # for all other countries.
    bar_country_eco <- plot_data_bar %>%
      distinct(Country, EconDevelopment, LargeBudgets) %>%
      arrange(Country)
    bar_skip_bracket <- bar_country_eco$LargeBudgets |
      as.character(bar_country_eco$Country) == "Rest of world"
    bar_y_levels <- if_else(
      bar_skip_bracket,
      as.character(bar_country_eco$Country),
      paste(as.character(bar_country_eco$Country),
            as.character(bar_country_eco$EconDevelopment), sep = "§")
    )
    plot_data_bar <- plot_data_bar %>%
      mutate(y_grouped = factor(
        if_else(LargeBudgets | as.character(Country) == "Rest of world",
                as.character(Country),
                paste(as.character(Country), as.character(EconDevelopment), sep = "§")),
        levels = bar_y_levels
      ))

    hlines_bar <- plot_data_bar %>%
      distinct(Country, LargeBudgets) %>%
      count(LargeBudgets, name = "n_countries") %>%
      rowwise() %>%
      mutate(yintercept = list(seq(0.5, n_countries - 0.5, 1))) %>%
      unnest(yintercept) %>%
      select(LargeBudgets, yintercept)

    PlotBudgets <- ggplot(plot_data_bar) +
      geom_hline(data = hlines_bar, aes(yintercept = yintercept),
                 color = "gray90", linewidth = 0.3) +
      geom_col(
        aes(y = y_grouped, x = NationalCarbonBudget / 1e3,
            fill = as.character(AccountingFramework)),
        position = position_dodge2()
      ) +
      facet_grid2(
        LargeBudgets ~ AllocationPrinciple + HistoricResponsibility,
        scales = "free", space = "free_y", axes = "x", independent = "x",
        labeller = labeller(HistoricResponsibility = LabelHistoricResponsibility)
      ) +
      scale_fill_scico_d(labels = LabelAccountingFramework, palette = "roma") +
      scale_y_discrete(
        limits = rev,
        # legendry's nested guide: `key = "§"` is passed as the `sep` argument to
        # key_range_auto(), so the part of each label before "§" becomes the
        # regular axis label (country) and the part after becomes a bracket
        # label (economic development group). `levels_text` rotates the bracket
        # labels 90° so "Upper-middle" etc. read vertically outside the line.
        guide  = legendry::guide_axis_nested(
          key = "§",
          levels_text = list(
            NULL,
            element_text(angle = 90, hjust = 0.5, vjust = 0.5)
          )
        )
      ) +
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
        strip.text.y         = element_blank(),
        panel.grid.major.y   = element_blank(),
        # Styling for the legendry nested-axis bracket line + label spacing.
        legendry.bracket      = element_line(linewidth = 0.5, colour = "gray30"),
        legendry.bracket.size = grid::unit(1.5, "mm")
      )

    suffix <- if (eu_panel) "EU" else ""
    ggsave(
      figure_path(sprintf("ResultsCarbonBudgets%s%s", t, suffix)),
      PlotBudgets,
      width = 8.38, height = if_else(eu_panel,5.11,11.5), units = "in", dpi = 300
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
        !AllocationPrinciple %in% c("Grandfathering", "Contraction and Convergence"),
        Country %in% panel_countries
      )

    # Build y_grouped factor: "Country§EconDevelopment" for regular countries,
    # plain country name for "Rest of world" (no economic development bracket).
    nz_country_eco <- plot_data_nz %>%
      distinct(Country, EconDevelopment) %>%
      arrange(Country)
    nz_y_levels <- if_else(
      as.character(nz_country_eco$Country) == "Rest of world",
      as.character(nz_country_eco$Country),
      paste(as.character(nz_country_eco$Country),
            as.character(nz_country_eco$EconDevelopment), sep = "§")
    )
    plot_data_nz <- plot_data_nz %>%
      mutate(y_grouped = factor(
        if_else(as.character(Country) == "Rest of world",
                as.character(Country),
                paste(as.character(Country), as.character(EconDevelopment), sep = "§")),
        levels = nz_y_levels
      ))

    n_nz <- length(panel_countries)
    PlotNetZero <- ggplot(plot_data_nz) +
      geom_hline(yintercept = seq(0.5, n_nz - 0.5, 1), color = "gray90", linewidth = 0.3) +
      geom_point(
        aes(x = ImplicitNetZero, y = y_grouped,
            color = AccountingFramework, fill = AccountingFramework),
        shape = 25
      ) +
      geom_text(
        size = 2, color = "palegreen4",
        aes(x = 2060, y = y_grouped,
            label = if_else(AnyNetZeroAbove2100 & AccountingFramework == 0.5,
                            "Net-zero after 2100", ""))
      ) +
      geom_text(
        size = 2, color = "maroon3",
        aes(x = 2060, y = y_grouped,
            label = if_else(CompleteResultsAccounting == FALSE & AccountingFramework == 0.5,
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
      scale_y_discrete(
        limits = rev,
        guide  = legendry::guide_axis_nested(
          key = "§",
          levels_text = list(
            NULL,
            element_text(angle = 90, hjust = 0.5, vjust = 0.5)
          )
        )
      ) +
      coord_cartesian(xlim = c(2020, 2100)) +
      guides(color = guide_colorbar(barwidth = 20)) +
      labs(x = "Net-zero year", y = NULL, color = "", fill = "") +
      theme_supp +
      theme(
        # Styling for the legendry nested-axis bracket line + label spacing.
        legendry.bracket      = element_line(linewidth = 0.5, colour = "gray30"),
        legendry.bracket.size = grid::unit(1.5, "mm")
      )

    suffix <- if (eu_panel) "EUMemberStates" else "AllCountries"
    ggsave(
      figure_path(sprintf("%s%s", suffix, t)),
      PlotNetZero,
      width = 8.38, height = if_else(eu_panel,5.11,11.5), units = "in", dpi = 300
    )
  }
}