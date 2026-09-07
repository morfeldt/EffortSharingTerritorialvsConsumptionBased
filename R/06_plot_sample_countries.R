# =============================================================================
# 06_plot_sample_countries.R  –  Sample-country figures
# =============================================================================
# Produces:
#   output/Graphs/Figure1.png  – Fig. 1: national carbon budgets across
#                                         the full accounting spectrum
#   output/Graphs/Figure2.png  – Fig. 2: implied net-zero emission years
#   output/Graphs/Figure3.png  – Fig. 3: per-capita emission trends
#
# Figures 1 and 2 show both temperature targets (1.5 °C and 2 °C) as two
# rows of panels, with countries as columns.
# =============================================================================

dir.create("output/Graphs", recursive = TRUE, showWarnings = FALSE)

SampleCountries <- c("China", "European Union", "South Africa", "Sweden", "United States")

# Colour scale for accounting framework (0 = territorial, 1 = consumption-based)
scico_roma_bar <- scale_color_scico(
  palette = "roma",
  breaks  = c(1, 0.5, 0),
  labels  = label_wrap_gen(width = 30)(c(LabelWeightResponsibility[["1"]], LabelWeightResponsibility[["0.5"]], LabelWeightResponsibility[["0"]]))
)
scico_roma_fill <- scale_fill_scico(
  palette = "roma",
  breaks  = c(1, 0.5, 0),
  labels  = label_wrap_gen(width = 30)(c(LabelWeightResponsibility[["1"]], LabelWeightResponsibility[["0.5"]], LabelWeightResponsibility[["0"]]))
)

# -----------------------------------------------------------------------------
# Shared data and theme for Fig. 1 and Fig. 2
# -----------------------------------------------------------------------------
# All three allocation principles; both temperature targets.

prepare_sample_data <- function(df) {
  df %>%
    filter(
      Country %in% SampleCountries,
      TempTarget %in% c(1.5, 2),
      AllocationPrinciple %in% c("Historic Responsibility from 1990",
                                 "Annual Equality",
                                 "Capability")
    ) %>%
    mutate(
      AllocationPrinciple = factor(AllocationPrinciple,
                                   levels = c("Capability", "Annual Equality",
                                              "Historic Responsibility from 1990"))
    )
}

# Panel labels a–j: rows are temperature targets (1.5, 2), columns are countries
panel_labels <- expand_grid(
  TempTarget = c(1.5, 2),
  Country    = SampleCountries
) %>%
  mutate(Label = paste0(letters[row_number()], ")"))

# Row strip labels for temperature targets
temp_labeller <- labeller(TempTarget = LabelTempTarget)

# Shared theme for Fig. 1 and Fig. 2
theme_sample <- theme_bw(base_size = 9) +
  theme(
    legend.position     = "bottom",
    strip.background    = element_rect(fill = "black", color = "transparent"),
    strip.text          = element_text(color = "white"),
    axis.ticks.length.y = unit(0, "cm"),
    legend.key.height   = unit(0.3, "cm"),
    axis.text.y         = element_text(color = "black")
  )

# -----------------------------------------------------------------------------
# Fig. 1 – National carbon budgets across the full accounting spectrum
# -----------------------------------------------------------------------------

DataBudgetSample <- prepare_sample_data(NationalCarbonBudgets) %>%
  filter(!is.na(NationalCarbonBudget))

Figure1 <- ggplot(DataBudgetSample) +
  geom_col(
    aes(x = NationalCarbonBudget / 1e3, y = AllocationPrinciple,
        color = WeightResponsibility, fill = WeightResponsibility),
    position = "dodge2"
  ) +
  geom_vline(xintercept = 0, color = "gray70", linewidth = 0.25) +
  geom_text(
    data = panel_labels,
    aes(x = -Inf, y = Inf, label = Label),
    hjust = -0.3, vjust = 1.5,
    size = 2.5, color = "black", inherit.aes = FALSE
  ) +
  facet_grid(TempTarget ~ Country, scales = "free_x",
             labeller = temp_labeller) +
  scico_roma_bar +
  scico_roma_fill +
  scale_y_discrete(labels = label_wrap_gen(width = 24)) +
  scale_x_continuous(expand = expansion(mult = c(0.13, 0.07))) +
  guides(color = guide_colorbar(barwidth = 20)) +
  labs(
    x = expression(paste("National Carbon Budget (GtCO"[2], ")")),
    y = NULL, color = "", fill = ""
  ) +
  theme_sample +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5,
                                   color = "black"))

ggsave(
  "output/Graphs/Figure1.png",
  Figure1,
  width = 176, height = 100, units = "mm", dpi = 500
)

# -----------------------------------------------------------------------------
# Fig. 2 – Implied net-zero years for sample countries
# -----------------------------------------------------------------------------

DataSampleCountries <- prepare_sample_data(NationalCarbonBudgets)

Figure2 <- ggplot(DataSampleCountries) +
  geom_point(
    aes(x = ImplicitNetZero, y = AllocationPrinciple,
        color = WeightResponsibility, fill = WeightResponsibility),
    size = 2, shape = 25
  ) +
  geom_text(
    aes(x = 2040, y = AllocationPrinciple,
        label = if_else(AnyNetZeroAbove2100 & WeightResponsibility == 0.5,
                        "Net-zero after 2060", "")),
    size = 2.5, color = "dimgray"
  ) +
  geom_text(
    aes(x = 2040, y = AllocationPrinciple,
        label = if_else(CompleteResultsAccounting == FALSE & WeightResponsibility == 0.5,
                        "Negative carbon budget", "")),
    size = 2.5, color = "dimgray"
  ) +
  geom_text(
    data = panel_labels,
    aes(x = 2022, y = 0.75, label = Label),
    size = 2.5, color = "black"
  ) +
  facet_grid(TempTarget ~ Country, scales = "free",
             labeller = temp_labeller) +
  scico_roma_bar +
  scico_roma_fill +
  scale_y_discrete(labels = label_wrap_gen(width = 24)) +
  coord_cartesian(xlim = c(2020, 2060)) +
  guides(color = guide_colorbar(barwidth = 20)) +
  labs(x = "Net-zero year", y = NULL, color = "", fill = "") +
  theme_sample +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5,
                                   color = "black"))

ggsave(
  "output/Graphs/Figure2.png",
  Figure2,
  width = 176, height = 100, units = "mm", dpi = 500
)

# -----------------------------------------------------------------------------
# Fig. 3 – Per-capita emission trends (historical)
# -----------------------------------------------------------------------------

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
    mutate(EmissionsPerCapita = EmissionsMtCO2 / Population * 1e6),

  # World average emissions per capita replicated into each sample-country facet
  DataGlobalCarbonBudget %>%
    filter(iso3c == "WLD", Accounting == "World") %>%
    left_join(
      DataUNPopulation %>% filter(iso3c == "WLD") %>%
        select(Year, Population),
      by = "Year"
    ) %>%
    mutate(
      EmissionsPerCapita = EmissionsMtCO2 / Population * 1e6,
      Country            = NA_character_
    ) %>%
    cross_join(tibble(Country = SampleCountries)) %>%
    rename(Country = Country.y) %>%
    mutate(Accounting = "World")
)

panel_labels_trends <- tibble(
  Country = SampleCountries,
  Label   = paste0(letters[seq_along(SampleCountries)], ")")
)

Figure3 <- ggplot(DataTrends) +
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
    values = c("Territorial Emissions" = "dashed",
               "Consumption Emissions" = "dotdash",
               "World"                = "solid"),
    labels = c("Consumption Emissions" = "Consumption-based emissions",
               "Territorial Emissions" = "Territorial emissions",
               "World"                = "World average")
  ) +
  scale_color_manual(
    values = c("Territorial Emissions" = scico(2, palette = "roma")[1],
               "Consumption Emissions" = scico(2, palette = "roma")[2],
               "World"                = "black"),
    labels = c("Consumption Emissions" = "Consumption-based emissions",
               "Territorial Emissions" = "Territorial emissions",
               "World"                = "World average")
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
  "output/Graphs/Figure3.png",
  Figure3,
  width = 176, height = 60, units = "mm", dpi = 500
)
