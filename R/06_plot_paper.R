# =============================================================================
# 06_plot_paper.R  –  Main-paper figures
# =============================================================================
# Produces:
#   output/Graphs/ResultsSampleCountries.png   – Fig. 1: implied net-zero years
#   output/Graphs/TrendsSampleCountries.png    – Fig. 2: per-capita emission trends
# =============================================================================

dir.create("output/Graphs", recursive = TRUE, showWarnings = FALSE)

SampleCountries <- c("China", "European Union", "South Africa", "Sweden", "United States")

# Colour scale for accounting framework (0 = territorial, 1 = consumption-based)
scico_roma_bar <- scale_color_scico(
  palette = "roma",
  breaks  = c(1, 0.5, 0),
  labels  = c("Consuming country's responsibility",
               "Symmetrical",
               "Producing country's responsibility")
)
scico_roma_fill <- scale_fill_scico(
  palette = "roma",
  breaks  = c(1, 0.5, 0),
  labels  = c("Consuming country's responsibility",
               "Symmetrical",
               "Producing country's responsibility")
)

# -----------------------------------------------------------------------------
# Fig. 1 – Implied net-zero years for a selection of countries
# -----------------------------------------------------------------------------
# Combines allocation principles without historic responsibility and
# the Equal Cumulative per Capita principle with historic responsibility years.

DataSampleCountries <- NationalCarbonBudgets %>%
  filter(
    Country %in% SampleCountries,
    TempTarget == 1.5,
    HistoricResponsibility %in% c(0, 1990, 2000, 2010),
    AllocationPrinciple != "Grandfathering"
  ) %>%
  mutate(
    # Combined x-axis label: allocation principle (no hist. resp.) or hist. year
    CombHistOther = case_when(
      HistoricResponsibility == 0 & AllocationPrinciple == "Annual Equal per Capita"
                                       ~ "Annual Equality",
      HistoricResponsibility == 0      ~ as.character(AllocationPrinciple),
      TRUE                             ~ paste0("Historic Responsibility from ",
                                                HistoricResponsibility)
    )
  )

# Panel labels (a–e) placed at the bottom-left of each facet
panel_labels <- tibble(
  Country = SampleCountries,
  Label   = paste0(letters[seq_along(SampleCountries)], ")")
)

ResultsSampleCountries <- ggplot(DataSampleCountries) +
  geom_point(
    aes(x = ImplicitNetZero, y = CombHistOther,
        color = AccountingFramework, fill = AccountingFramework),
    size = 2, shape = 25
  ) +
  # Annotation when net-zero is beyond the horizon
  geom_text(
    aes(x = 2040, y = CombHistOther,
        label = if_else(AnyNetZeroAbove2100 & AccountingFramework == 0.5,
                        "Net-zero after 2070", "")),
    size = 2.5, color = "dimgray"
  ) +
  # Annotation when budget is negative
  geom_text(
    aes(x = 2040, y = CombHistOther,
        label = if_else(isFALSE(CompleteResultsAccounting) & AccountingFramework == 0.5,
                        "Negative carbon budget", "")),
    size = 2.5, color = "dimgray"
  ) +
  geom_text(
    data = panel_labels,
    aes(x = 2022, y = 0.75, label = Label),
    size = 2.5, color = "black"
  ) +
  facet_grid(~ Country, scales = "free") +
  scico_roma_bar +
  scico_roma_fill +
  scale_y_discrete(labels = label_wrap_gen(width = 24)) +
  coord_cartesian(xlim = c(2020, 2060)) +
  guides(color = guide_colorbar(barwidth = 20)) +
  labs(x = "Net-zero year", y = NULL, color = "", fill = "") +
  theme_bw(base_size = 9) +
  theme(
    legend.position    = "bottom",
    strip.background   = element_rect(fill = "black", color = "transparent"),
    strip.text         = element_text(color = "white"),
    axis.ticks.length.y = unit(0, "cm"),
    legend.key.height  = unit(0.3, "cm"),
    axis.text.x        = element_text(angle = 90, vjust = 0.5, hjust = 0.5,
                                      color = "black"),
    axis.text.y        = element_text(color = "black")
  )

ggsave(
  "output/Graphs/ResultsSampleCountries.png",
  ResultsSampleCountries,
  width = 176, height = 85, units = "mm", dpi = 500
)

# -----------------------------------------------------------------------------
# Fig. 2 – Per-capita emission trends (historical)
# -----------------------------------------------------------------------------
# Shows territorial, consumption-based and world-average per-capita emissions
# for each sample country.

DataTrends <- bind_rows(
  # Country-level emissions per capita
  DataGlobalCarbonBudget %>%
    filter(Country %in% SampleCountries,
           Accounting %in% c("Territorial Emissions", "Consumption Emissions")) %>%
    left_join(
      DataUNPopulation %>% filter(Country %in% SampleCountries) %>%
        select(Country, Year, Population),
      by = c("Country", "Year")
    ) %>%
    mutate(EmissionsPerCapita = EmissionsMtCO2 / Population * 1e6), # MtCO₂ → tCO₂

  # World average emissions per capita
  DataGlobalCarbonBudget %>%
    filter(iso3c == "WLD", Accounting == "World") %>%
    left_join(
      DataUNPopulation %>% filter(iso3c == "WLD") %>%
        select(Year, Population),
      by = "Year"
    ) %>%
    mutate(
      EmissionsPerCapita = EmissionsMtCO2 / Population * 1e6,
      Country            = NA_character_   # will be broadcast across facets
    ) %>%
    # Replicate world average into each sample-country facet
    cross_join(tibble(Country = SampleCountries)) %>%
    rename(Country = Country.y) %>%
    mutate(Accounting = "World")
)

# Panel labels
panel_labels_trends <- tibble(
  Country = SampleCountries,
  Label   = paste0(letters[seq_along(SampleCountries)], ")")
)

TrendsSampleCountries <- ggplot(DataTrends) +
  geom_line(
    aes(x = Year, y = EmissionsPerCapita,
        linetype = Accounting, color = Accounting)
  ) +
  geom_text(
    data = panel_labels_trends,
    aes(x = 1990, y = 0, label = Label),
    vjust = -0.5, hjust = -0.3, size = 2.5, color = "black"
  ) +
  facet_grid2(cols = vars(Country), scales = "free", independent = TRUE) +
  scale_linetype_manual(
    values = c("dashed", "dotdash", "solid"),
    labels = c("Territorial emissions",
               "Consumption-based emissions",
               "World average")
  ) +
  scale_color_manual(
    values = c(scico(2, palette = "roma"), "black"),
    labels = c("Territorial emissions",
               "Consumption-based emissions",
               "World average")
  ) +
  scale_y_continuous(limits = c(0, NA), n.breaks = 6) +
  guides(linetype = guide_legend(nrow = 1), color = guide_legend(nrow = 1)) +
  labs(
    x = "Year",
    y = expression(paste("Emissions per capita (tCO"[2], ")")),
    linetype = "", color = ""
  ) +
  theme_bw(base_size = 9) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "black", color = "transparent"),
    strip.text       = element_text(color = "white"),
    axis.text        = element_text(color = "black")
  )

ggsave(
  "output/Graphs/TrendsSampleCountries.png",
  TrendsSampleCountries,
  width = 176, height = 60, units = "mm", dpi = 500
)
