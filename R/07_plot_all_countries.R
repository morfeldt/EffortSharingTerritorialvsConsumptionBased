# =============================================================================
# 07_plot_all_countries.R  –  All-countries figures
# =============================================================================
# Produces:
#
#   output/Graphs/Figure4.png
#     Scatter: territorial vs consumption-based net-zero emission year,
#     coloured by embodied emissions balance (all non-EU countries)
#
#   output/Graphs/Figure5.png
#     Scatter: territorial vs consumption-based net-zero emission year,
#     coloured by economic development (all non-EU countries)
#
#   output/Graphs/ExtendedDataFigure1.png
#     Annual vs cumulative embodied emissions balance, all non-EU countries
#
#   output/Graphs/ExtendedDataFigure2.png
#     Same as Figure 4 but for EU member states
#
#   output/Graphs/ExtendedDataFigure3–10.png (one per temperature target × country set)
#     Bar charts (national carbon budgets) and point plots (implied net-zero year)
#
# The DataForSupplementary data table is written by 08_export.R.
# =============================================================================

dir.create("output/Graphs", recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Use legendry for nested y-axis (country + economic development group).
#
# NOTE: legendry::guide_axis_nested() is incompatible with ggplot2 >= 4.0.0.
# If you hit "object 'decor' not found", downgrade ggplot2:
#   remotes::install_version("ggplot2", version = "3.5.1")
# -----------------------------------------------------------------------------
if (!requireNamespace("legendry", quietly = TRUE)) {
  stop("Package 'legendry' is required. Install with install.packages(\"legendry\").")
}

# Output filename map: old stem → new stem
figure_stem_map <- c(
  "ResultsCompareTargets1.5All" = "Figure5",
  "AllCountries1.5"             = "ExtendedDataFigure3",
  "EUMemberStates1.5"           = "ExtendedDataFigure5",
  "ResultsCarbonBudgets1.5"     = "ExtendedDataFigure7",
  "ResultsCarbonBudgets1.5EU"   = "ExtendedDataFigure9",
  "AllCountries2"               = "ExtendedDataFigure4",
  "EUMemberStates2"             = "ExtendedDataFigure6",
  "ResultsCarbonBudgets2"       = "ExtendedDataFigure8",
  "ResultsCarbonBudgets2EU"     = "ExtendedDataFigure10"
)
figure_path <- function(stem) {
  mapped <- figure_stem_map[stem]
  sprintf("output/Graphs/%s.png", if (!is.na(mapped)) mapped else stem)
}

# Shared theme elements -------------------------------------------------------
theme_supp <- theme_bw(base_size = 9) +
  theme(
    legend.position    = "bottom",
    strip.background   = element_rect(fill = "black", color = "transparent"),
    strip.text         = element_text(color = "white"),
    panel.grid.major.y = element_blank()
  )

principle_levels <- c("Historic Responsibility from 1990",
                      "Capability",
                      "Annual Equality")

# Panel letter labels for scatter plots: a-c for 1.5°C, d-f for 2°C
scatter_labels <- expand_grid(
  TempTarget          = c(1.5, 2),
  AllocationPrinciple = factor(principle_levels, levels = principle_levels)
) %>%
  mutate(Label = paste0(letters[row_number()], ")"))

temp_labeller_scatter <- labeller(TempTarget = LabelTempTarget)

# -----------------------------------------------------------------------------
# Figure 5 – scatter combining both temperature targets as rows
# -----------------------------------------------------------------------------
# DataForSupplementary is assembled here and passed to 08_export.R
DataForSupplementary <- list()

panel_iso_scatter <- CountryAssumptions %>%
  filter(iso3c != "ROW", !EUMemberState) %>%
  pull(Country)

base_data <- NationalCarbonBudgets %>%
  filter(
    Country %in% panel_iso_scatter,
    TempTarget %in% c(1.5, 2)
  ) %>%
  mutate(AllocationPrinciple = factor(AllocationPrinciple, levels = principle_levels))

PlotData <- base_data %>%
  filter(WeightResponsibility == 1) %>%
  left_join(
    base_data %>%
      filter(WeightResponsibility == 0) %>%
      select(Country, AllocationPrinciple, HistoricResponsibility, TempTarget,
             TerritorialNetZero  = ImplicitNetZero,
             TerritorialBudget   = NationalCarbonBudget),
    by = c("Country", "AllocationPrinciple", "HistoricResponsibility", "TempTarget")
  ) %>%
  rename(
    ConsumptionBasedNetZero = ImplicitNetZero,
    ConsumptionBasedBudget  = NationalCarbonBudget
  ) %>%
  mutate(
    DifferenceNetZeros = ConsumptionBasedNetZero - TerritorialNetZero
  )

DataForSupplementary[["scatter_All"]] <- PlotData %>%
  select(TempTarget, AllocationPrinciple, HistoricResponsibility, EconDevelopment, Country,
         ConsumptionBasedNetZero, ConsumptionBasedBudget,
         TerritorialNetZero, TerritorialBudget, DifferenceNetZeros) %>%
  arrange(TempTarget, AllocationPrinciple, HistoricResponsibility, EconDevelopment, Country) %>%
  mutate(
    HistoricResponsibility = na_if(as.character(HistoricResponsibility), "0"),
    Country = as.character(Country)
  )

Figure5 <- ggplot(PlotData) +
  geom_abline(intercept = 0, slope = 1, color = "gray") +
  annotate("rect", xmin = 2020, xmax = 2100, ymin = 2000, ymax = 2020,
           , fill = "lightgray") +
  annotate("segment", x = 2060, xend = 2060, y = 2000, yend = 2020, color = "white") +
  annotate("text", x = 2025, y = 2015,
           label = expression(paste("<0 CO"[2]," budget")), size = 2, hjust = 0) +
  annotate("text", x = 2065, y = 2015, label = "Net-zero >2100", size = 2, hjust = 0) +
  geom_point(
    aes(
      x = if_else((ConsumptionBasedBudget < 0 & TerritorialBudget < 0) | (ConsumptionBasedBudget < 0 & TerritorialBudget > 0) | (ConsumptionBasedBudget > 0 & TerritorialBudget < 0),
        case_when(EconDevelopment == "High"         ~ 2030,
                    EconDevelopment == "Upper-middle" ~ 2030,
                    EconDevelopment == "Lower-middle" ~ 2050,
                    TRUE                              ~ 2050),
          if_else(ConsumptionBasedNetZero > 2100 | TerritorialNetZero > 2100,
                  case_when(EconDevelopment == "High"         ~ 2090,
                    EconDevelopment == "Upper-middle" ~ 2090,
                    EconDevelopment == "Lower-middle" ~ 2070,
                    TRUE                              ~ 2070), ConsumptionBasedNetZero)),
      y = if_else((ConsumptionBasedBudget < 0 & TerritorialBudget < 0) | (ConsumptionBasedBudget < 0 & TerritorialBudget > 0) | (ConsumptionBasedBudget > 0 & TerritorialBudget < 0),
          case_when(EconDevelopment == "High"         ~ 2010,
                    EconDevelopment == "Upper-middle" ~ 2005,
                    EconDevelopment == "Lower-middle" ~ 2010,
                    TRUE                              ~ 2005),
          if_else(ConsumptionBasedNetZero > 2100 | TerritorialNetZero > 2100,
          case_when(EconDevelopment == "High"         ~ 2005,
                    EconDevelopment == "Upper-middle" ~ 2010,
                    EconDevelopment == "Lower-middle" ~ 2005,
                    TRUE                              ~ 2010),
          TerritorialNetZero)),
      color = EconDevelopment
    )
  ) +
  geom_text(
    data = PlotData %>%
      filter(ConsumptionBasedNetZero > 2100 | TerritorialNetZero > 2100) %>%
      count(EconDevelopment, AllocationPrinciple, TempTarget) %>%
      filter(n > 0),
    aes(label = n, x = case_when(EconDevelopment == "High"         ~ 2092,
                    EconDevelopment == "Upper-middle" ~ 2092,
                    EconDevelopment == "Lower-middle" ~ 2072,
                    TRUE                              ~ 2072),
        y = case_when(EconDevelopment == "High"         ~ 2005,
                      EconDevelopment == "Upper-middle" ~ 2010,
                      EconDevelopment == "Lower-middle" ~ 2005,
                      TRUE                              ~ 2010)),
    hjust = 0, vjust = 0.5, size = 2
  ) +
  geom_text(
    data = PlotData %>%
      filter((ConsumptionBasedBudget < 0 & TerritorialBudget < 0) | (ConsumptionBasedBudget < 0 & TerritorialBudget > 0) | (ConsumptionBasedBudget > 0 & TerritorialBudget < 0)) %>%
      count(EconDevelopment, AllocationPrinciple, TempTarget) %>%
      filter(n > 0),
    aes(label = n, x = case_when(EconDevelopment == "High"         ~ 2032,
                    EconDevelopment == "Upper-middle" ~ 2032,
                    EconDevelopment == "Lower-middle" ~ 2052,
                    TRUE                              ~ 2052),
        y = case_when(EconDevelopment == "High"         ~ 2010,
                      EconDevelopment == "Upper-middle" ~ 2005,
                      EconDevelopment == "Lower-middle" ~ 2010,
                      TRUE                              ~ 2005)),
    hjust = 0, vjust = 0.5, size = 2
  ) +
  geom_text_repel(
    data = PlotData %>%
      filter(abs(TerritorialNetZero - ConsumptionBasedNetZero) > 5,
             TerritorialNetZero < 2100, ConsumptionBasedNetZero < 2100) %>%
      left_join(CountryAssumptions %>% select(Country, iso3c), by = "Country"),
    aes(y = TerritorialNetZero, x = ConsumptionBasedNetZero, label = iso3c),
    size = 2, nudge_x = 0.5, nudge_y = 0.5,
    min.segment.length = 0, max.overlaps = 30, force = 5,
    xlim = c(2020, 2100), ylim = c(2020, 2100)
  ) +
  geom_text(
    data = scatter_labels,
    aes(x = 2018, y = 1998, label = Label),
    size = 2.5, color = "black"
  ) +
  facet_grid(TempTarget ~ AllocationPrinciple, labeller = temp_labeller_scatter) +
  scale_color_manual(values = scico(4, palette = "roma")) +
  scale_y_continuous(breaks = seq(2020, 2100, 20)) +
  coord_cartesian(xlim = c(2018, 2100), ylim = c(1998, 2100)) +
  labs(
    y     = label_wrap_gen(width = 50)(LabelWeightResponsibility[["0"]]),
    x     = label_wrap_gen(width = 50)(LabelWeightResponsibility[["1"]]),
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
  figure_path("ResultsCompareTargets1.5All"),
  Figure5,
  width = 6, height = 5, units = "in", dpi = 300
)

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
      WeightResponsibility %in% c(0, 1),
      Country != "Rest of world"
    ) %>%
    mutate(AllocationPrinciple = factor(AllocationPrinciple, levels = principle_levels))

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
            fill = as.character(WeightResponsibility)),
        position = position_dodge2()
      ) +
      facet_grid2(
        LargeBudgets ~ AllocationPrinciple,
        scales = "free", space = "free_y", axes = "x", independent = "x"
      ) +
      scale_fill_scico_d(labels = LabelWeightResponsibility, palette = "roma") +
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
      labs(
        x    = expression(paste("National carbon budget (GtCO"[2], ")")),
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
        legendry.bracket      = element_line(linewidth = 0.5, colour = "gray30"),
        legendry.bracket.size = grid::unit(1.5, "mm")
      )

    suffix <- if (eu_panel) "EU" else ""
    ggsave(
      figure_path(sprintf("ResultsCarbonBudgets%s%s", t, suffix)),
      PlotBudgets,
      width = 8.38, height = if_else(eu_panel,4.9,11.4), units = "in", dpi = 300
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
        Country %in% panel_countries
      ) %>%
      mutate(AllocationPrinciple = factor(AllocationPrinciple, levels = principle_levels))

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
            color = WeightResponsibility, fill = WeightResponsibility),
        shape = 25
      ) +
      geom_text(
        size = 2.5, color = "palegreen4",
        aes(x = 2060, y = y_grouped,
            label = if_else(AnyNetZeroAbove2100 & WeightResponsibility == 0.5,
                            "Net-zero after 2100", ""))
      ) +
      geom_text(
        size = 2.5, color = "maroon3",
        aes(x = 2060, y = y_grouped,
            label = if_else(CompleteResultsAccounting == FALSE & WeightResponsibility == 0.5,
                            "Negative carbon budget", ""))
      ) +
      facet_grid(~ AllocationPrinciple) +
      scale_color_scico(
        palette = "roma", breaks = c(1, 0.5, 0),
        labels  = label_wrap_gen(width = 20)(c(LabelWeightResponsibility[["1"]], LabelWeightResponsibility[["0.5"]], LabelWeightResponsibility[["0"]]))
      ) +
      scale_fill_scico(
        palette = "roma", breaks = c(1, 0.5, 0),
        labels  = label_wrap_gen(width = 20)(c(LabelWeightResponsibility[["1"]], LabelWeightResponsibility[["0.5"]], LabelWeightResponsibility[["0"]]))
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
      guides(color = guide_colorbar(barheight = 10, barwidth = 0.5),
             fill  = guide_colorbar(barheight = 10, barwidth = 0.5)) +
      labs(x = "Net-zero year", y = NULL, color = "", fill = "") +
      theme_supp +
      theme(
        legend.position       = "right",
        legendry.bracket      = element_line(linewidth = 0.5, colour = "gray30"),
        legendry.bracket.size = grid::unit(1.5, "mm")
      )

    suffix <- if (eu_panel) "EUMemberStates" else "AllCountries"
    ggsave(
      figure_path(sprintf("%s%s", suffix, t)),
      PlotNetZero,
      width = 8.38, height = if_else(eu_panel,4.9,11.4), units = "in", dpi = 300
    )
  }
}

# =============================================================================
# Figure 4 / ExtendedDataFigure2 – scatter coloured by embodied emissions balance
# =============================================================================
# Colour variable:
#   "Historic Responsibility from 1990" panel → cumulative diff 1990–YearEnd (%)
#   "Capability" / "Annual Equality" panels   → annual diff at YearEnd (%)
# Positive = consumption > territorial (net importer of embedded carbon)
#
# EmissionsBalance is kept alive after this loop — reused by ExtendedDataFigure1.

.ew_balance <- DataGlobalCarbonBudget %>%
  filter(Year %in% 1990:YearEnd,
         Accounting %in% c("Territorial Emissions", "Consumption Emissions")) %>%
  select(iso3c, Year, Accounting, EmissionsMtCO2) %>%
  pivot_wider(names_from = Accounting, values_from = EmissionsMtCO2)

EmissionsBalance <- left_join(
  .ew_balance %>% filter(Year == YearEnd) %>%
    mutate(EmDiff_Pct = (`Consumption Emissions` - `Territorial Emissions`) /
                         `Territorial Emissions` * 100) %>%
    select(iso3c, EmDiff_Pct),
  .ew_balance %>%
    group_by(iso3c) %>%
    summarise(
      CumEmDiff_Pct = (sum(`Consumption Emissions`, na.rm = TRUE) -
                       sum(`Territorial Emissions`, na.rm = TRUE)) /
                       sum(`Territorial Emissions`, na.rm = TRUE) * 100,
      .groups = "drop"
    ),
  by = "iso3c"
)
rm(.ew_balance)

for (country_set in c("All", "EU")) {
  is_eu <- country_set == "EU"

  panel_iso <- CountryAssumptions %>%
    filter(iso3c != "ROW", EUMemberState == is_eu) %>%
    pull(Country)

  base_scatter <- NationalCarbonBudgets %>%
    filter(
      Country %in% panel_iso,
      TempTarget %in% c(1.5, 2)
    ) %>%
    mutate(AllocationPrinciple = factor(AllocationPrinciple, levels = principle_levels))

  PlotDataBalance <- base_scatter %>%
    filter(WeightResponsibility == 1) %>%
    left_join(
      base_scatter %>%
        filter(WeightResponsibility == 0) %>%
        select(Country, AllocationPrinciple, HistoricResponsibility, TempTarget,
               TerritorialNetZero = ImplicitNetZero,
               TerritorialBudget  = NationalCarbonBudget),
      by = c("Country", "AllocationPrinciple", "HistoricResponsibility", "TempTarget")
    ) %>%
    rename(ConsumptionBasedNetZero = ImplicitNetZero,
           ConsumptionBasedBudget  = NationalCarbonBudget) %>%
    mutate(
      DifferenceNetZeros = ConsumptionBasedNetZero - TerritorialNetZero
    ) %>%
    left_join(CountryAssumptions %>% select(Country, iso3c), by = "Country") %>%
    left_join(EmissionsBalance, by = "iso3c") %>%
    mutate(ColorVar = if_else(
      AllocationPrinciple == "Historic Responsibility from 1990",
      CumEmDiff_Pct,
      EmDiff_Pct
    ))

  clim_upper <- ceiling(max(PlotDataBalance$ColorVar, na.rm = TRUE) / 10) * 10
  out_name   <- if (is_eu) "ExtendedDataFigure2" else "Figure4"

  p <- ggplot(PlotDataBalance) +
    geom_abline(intercept = 0, slope = 1, color = "gray") +
    geom_point(aes(
      x = ConsumptionBasedNetZero,
      y = TerritorialNetZero,
      color = ColorVar
    )) +
    geom_text_repel(
      data = PlotDataBalance %>%
        filter(abs(TerritorialNetZero - ConsumptionBasedNetZero) > 5,
               TerritorialNetZero < 2100, ConsumptionBasedNetZero < 2100),
      aes(y = TerritorialNetZero, x = ConsumptionBasedNetZero, label = iso3c),
      size = 2, nudge_x = 0.5, nudge_y = 0.5,
      min.segment.length = 0, max.overlaps = 30, force = 5,
      xlim = c(2020, 2100), ylim = c(2020, 2100)
    ) +
    geom_text(
      data = scatter_labels,
      aes(x = 2018, y = 2018, label = Label),
      size = 2.5, color = "black"
    ) +
    facet_grid(TempTarget ~ AllocationPrinciple, labeller = temp_labeller_scatter) +
    scale_color_gradientn(
      colours  = scico(256, palette = "vik"),
      limits   = c(-100, clim_upper),
      breaks   = c(-100, seq(0, clim_upper, by = 100)),
      rescaler = function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
        ifelse(x < 0,
               scales::rescale(x, to = c(0, 0.5), from = c(from[1], 0)),
               scales::rescale(x, to = c(0.5, 1), from = c(0, from[2])))
      },
      name     = "Embodied emissions balance (%)",
      guide    = guide_colorbar(barwidth = 8, barheight = 0.5, title.position = "top",
                                title.hjust = 0.5)
    ) +
    scale_y_continuous(breaks = seq(2020, 2100, 20)) +
    coord_cartesian(xlim = c(2018, 2100), ylim = c(2018, 2100)) +
    labs(
      y = label_wrap_gen(width = 50)(LabelWeightResponsibility[["0"]]),
      x = label_wrap_gen(width = 50)(LabelWeightResponsibility[["1"]])
    ) +
    theme_bw(base_size = 9) +
    theme(
      legend.position  = "bottom",
      legend.margin    = margin(0, 0, 0, 0),
      strip.background = element_rect(fill = "black", color = "transparent"),
      strip.text       = element_text(color = "white")
    )

  ggsave(
    sprintf("output/Graphs/%s.png", out_name),
    p,
    width = 6, height = 4.5, units = "in", dpi = 300
  )
  message(sprintf("Written: output/Graphs/%s.png", out_name))
}

# =============================================================================
# ExtendedDataFigure1 – annual vs cumulative embodied emissions balance
# =============================================================================
# x: annual consumption−territorial difference at YearEnd (%)
# y: cumulative consumption−territorial difference 1990–YearEnd (%)
# Coloured by economic development group; iso3c labels for |diff| > 25% on
# either axis. Non-EU countries only.

.edc1 <- scico(4, palette = "roma")
econ_palette_ed1 <- c(
  "High"         = .edc1[1],
  "Low"          = .edc1[2],
  "Lower-middle" = .edc1[3],
  "Upper-middle" = .edc1[4]
)
rm(.edc1)

DataEmDiff <- EmissionsBalance %>%
  rename(EmissionsDiff_Pct = EmDiff_Pct, CumEmissionsDiff_Pct = CumEmDiff_Pct) %>%
  inner_join(
    CountryAssumptions %>%
      filter(iso3c != "ROW", EUMemberState == FALSE) %>%
      select(iso3c, EconDevelopment = Development),
    by = "iso3c"
  ) %>%
  mutate(EconDevelopment = factor(
    EconDevelopment, levels = c("High", "Upper-middle", "Lower-middle", "Low")
  ))

rm(EmissionsBalance)

ExtendedDataFigure1 <- ggplot(
  DataEmDiff,
  aes(x = EmissionsDiff_Pct, y = CumEmissionsDiff_Pct, color = EconDevelopment)
) +
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_point(size = 1.5, alpha = 0.8) +
  geom_text_repel(
    aes(label = ifelse(
      abs(EmissionsDiff_Pct) > 25 | abs(CumEmissionsDiff_Pct) > 25,
      iso3c, ""
    )),
    size               = 2,
    min.segment.length = 0,
    max.overlaps       = Inf,
    force              = 5,
    point.padding      = 0.5,
    box.padding        = 0.4,
    seed               = 42
  ) +
  scale_color_manual(
    values = econ_palette_ed1,
    labels = c("High", "Upper-middle", "Lower-middle", "Low"),
    guide  = guide_legend(nrow = 1)
  ) +
  labs(
    x     = paste0("Embodied emissions balance at ", YearEnd, " (%)"),
    y     = paste0("Cumulative embodied emissions balance 1990–", YearEnd, " (%)"),
    color = "Economic development"
  ) +
  theme_bw(base_size = 9) +
  theme(
    legend.position  = "bottom",
    legend.key.size  = unit(0.4, "cm"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "output/Graphs/ExtendedDataFigure1.png",
  ExtendedDataFigure1,
  width  = 120,
  height = 110,
  units  = "mm",
  dpi    = 500
)
message("Written: output/Graphs/ExtendedDataFigure1.png")