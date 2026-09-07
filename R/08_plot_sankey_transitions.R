# =============================================================================
# 08_plot_sankey_transitions.R  –  Parallel-sets diagram of budget-category transitions
# =============================================================================
# Shows how countries move between three carbon budget categories when
# the weight shifts from territorial (α=0) to consumption-based (α=1)
# emissions. Uses ggforce::geom_parallel_sets (ggalluvial is incompatible with
# ggplot2 3.5.2). Faceted by allocation principle (3 cols) × temperature
# target (2 rows); flows coloured by economic development group.
#
# Budget categories:
#   "Net-zero before 2100" : NationalCarbonBudget > 0  AND  ImplicitNetZero ≤ 2100
#   "Net-zero after 2100"  : NationalCarbonBudget > 0  AND  ImplicitNetZero > 2100
#   "Negative budget"      : NationalCarbonBudget ≤ 0
#
# Output: output/Graphs/Figure6.png
# =============================================================================

dir.create("output/Graphs", recursive = TRUE, showWarnings = FALSE)

# ─── Constants ────────────────────────────────────────────────────────────────

SankeyPrincipleLevels <- c(
  "Historic Responsibility from 1990",
  "Capability",
  "Annual Equality"
)

# Category order: top to bottom in axes
CategoryLevels <- c("Negative\nbudget", "Net-zero\nbefore 2100", "Net-zero\nafter 2100")

# Match Figure 4: scico(4, "roma") assigned alphabetically ("High", "Low",
# "Lower-middle", "Upper-middle") as ggplot2 does for character variables
.econ_cols <- scico(4, palette = "roma")
EconPalette <- c(
  "High"         = .econ_cols[1],
  "Low"          = .econ_cols[2],
  "Lower-middle" = .econ_cols[3],
  "Upper-middle" = .econ_cols[4]
)
EconLevels <- c("High", "Upper-middle", "Lower-middle", "Low")

# ─── Data preparation ─────────────────────────────────────────────────────────

AllCountriesSankey <- CountryAssumptions %>%
  filter(iso3c != "ROW", EUMemberState == FALSE) %>%
  pull(Country)

assign_category <- function(budget, net_zero) {
  factor(case_when(
    budget <= 0     ~ "Negative\nbudget",
    net_zero > 2100 ~ "Net-zero\nafter 2100",
    TRUE            ~ "Net-zero\nbefore 2100"
  ), levels = CategoryLevels)
}

DataSankeyWide <- NationalCarbonBudgets %>%
  filter(
    Country %in% AllCountriesSankey,
    TempTarget %in% c(1.5, 2),
    WeightResponsibility %in% c(0, 1)
  ) %>%
  mutate(AllocationPrinciple = factor(AllocationPrinciple, levels = SankeyPrincipleLevels)) %>%
  left_join(CountryAssumptions %>% select(Country, iso3c), by = "Country") %>%
  select(Country, iso3c, EconDevelopment, TempTarget, AllocationPrinciple,
         WeightResponsibility, NationalCarbonBudget, ImplicitNetZero) %>%
  pivot_wider(
    names_from  = WeightResponsibility,
    values_from = c(NationalCarbonBudget, ImplicitNetZero),
    names_sep   = "_AF"
  ) %>%
  mutate(
    category_AF0    = assign_category(NationalCarbonBudget_AF0, ImplicitNetZero_AF0),
    category_AF1    = assign_category(NationalCarbonBudget_AF1, ImplicitNetZero_AF1),
    EconDevelopment = factor(EconDevelopment, levels = EconLevels)
  )

# ─── Data inspection ──────────────────────────────────────────────────────────

message("\n========== SANKEY DATA INSPECTION ==========\n")

message("--- Countries per facet ---")
print(
  DataSankeyWide %>%
    count(TempTarget, AllocationPrinciple, name = "N") %>%
    pivot_wider(names_from = AllocationPrinciple, values_from = N)
)

for (tt in c(1.5, 2)) {
  message(sprintf("\n--- %.1f°C: category counts AF=0 (territorial) ---", tt))
  print(
    DataSankeyWide %>% filter(TempTarget == tt) %>%
      count(AllocationPrinciple, category_AF0) %>%
      pivot_wider(names_from = category_AF0, values_from = n, values_fill = 0L)
  )
  message(sprintf("--- %.1f°C: category counts AF=1 (consumption) ---", tt))
  print(
    DataSankeyWide %>% filter(TempTarget == tt) %>%
      count(AllocationPrinciple, category_AF1) %>%
      pivot_wider(names_from = category_AF1, values_from = n, values_fill = 0L)
  )
  message(sprintf("--- %.1f°C: transition matrix ---", tt))
  print(
    DataSankeyWide %>% filter(TempTarget == tt) %>%
      count(AllocationPrinciple, category_AF0, category_AF1) %>%
      filter(n > 0) %>%
      arrange(AllocationPrinciple, category_AF0, category_AF1)
  )
}

# Flag boundary cases
Ambiguous <- DataSankeyWide %>%
  filter(
    NationalCarbonBudget_AF0 == 0 | NationalCarbonBudget_AF1 == 0 |
    (!is.na(ImplicitNetZero_AF0) & abs(ImplicitNetZero_AF0 - 2100) < 1) |
    (!is.na(ImplicitNetZero_AF1) & abs(ImplicitNetZero_AF1 - 2100) < 1) |
    is.na(category_AF0) | is.na(category_AF1)
  )

if (nrow(Ambiguous) > 0) {
  message(sprintf(
    "\nWARNING: %d boundary cases (budget = 0 or net-zero within 1 yr of 2100):",
    nrow(Ambiguous)
  ))
  print(Ambiguous %>%
    select(iso3c, TempTarget, AllocationPrinciple, EconDevelopment,
           NationalCarbonBudget_AF0, NationalCarbonBudget_AF1,
           ImplicitNetZero_AF0, ImplicitNetZero_AF1,
           category_AF0, category_AF1))
} else {
  message("\nNo boundary cases found.")
}

# ─── Long format for geom_parallel_sets ───────────────────────────────────────
# geom_parallel_sets needs: x (axis), id (flow), split (stratum), value (weight)

AxisLabels <- c(
  category_AF0 = label_wrap_gen(width = 10)(LabelWeightResponsibility[["0"]]),
  category_AF1 = label_wrap_gen(width = 10)(LabelWeightResponsibility[["1"]])
)

DataPS <- DataSankeyWide %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols      = c(category_AF0, category_AF1),
    names_to  = "x_raw",
    values_to = "split"
  ) %>%
  mutate(
    x     = factor(AxisLabels[x_raw],
                   levels = unname(AxisLabels)),
    split = factor(split, levels = CategoryLevels)
  )


# ─── Figure ───────────────────────────────────────────────────────────────────

temp_labeller_sankey <- labeller(TempTarget = LabelTempTarget)

# Category colours for axis bars (neutral but distinct)
CategoryPalette <- c(
  "Net-zero\nbefore 2100" = "gray75",
  "Net-zero\nafter 2100"  = "gray60",
  "Negative\nbudget"      = "gray40"
)

Figure6 <- ggplot(DataPS,
  aes(x = x, id = id, split = split, value = 1)) +
  geom_parallel_sets(
    aes(fill = EconDevelopment),
    alpha = 0.72, axis.width = 0.35, sep = 0.02
  ) +
  geom_parallel_sets_axes(
    aes(fill = split),
    axis.width = 0.35, sep = 0.02,
    color = "gray25", linewidth = 0.25
  ) +
  geom_parallel_sets_labels(
    color      = "white",
    size       = 2.1,
    angle      = 0,
    vjust      = 0.5,
    fontface   = "bold",
    lineheight = 0.85,
    sep        = 0.02
  ) +
  facet_grid(TempTarget ~ AllocationPrinciple, labeller = temp_labeller_sankey) +
  scale_fill_manual(
    values   = c(EconPalette, CategoryPalette),
    breaks   = EconLevels,
    labels   = c("High income", "Upper-middle income",
                 "Lower-middle income", "Low income"),
    guide    = guide_legend(nrow = 1)
  ) +
  scale_x_discrete(expand = expansion(add = 0)) +
  scale_y_continuous(expand = expansion(mult = 0)) +
  labs(
    x    = NULL,
    y    = "Number of countries",
    fill = "Economic development"
  ) +
  theme_bw(base_size = 9) +
  theme(
    legend.position   = "bottom",
    legend.key.size   = unit(0.4, "cm"),
    legend.key.height = unit(0.3, "cm"),
    strip.background  = element_rect(fill = "black", color = "transparent"),
    strip.text        = element_text(color = "white", size = 8),
    axis.text.y       = element_blank(),
    axis.ticks.y      = element_blank(),
    axis.text.x       = element_text(size = 7.5, color = "black"),
    panel.grid        = element_blank(),
    panel.spacing.x   = unit(0.6, "cm"),
    panel.spacing.y   = unit(0.4, "cm")
  )

ggsave(
  "output/Graphs/Figure6.png",
  Figure6,
  width  = 180,
  height = 130,
  units  = "mm",
  dpi    = 500
)
message("Written: output/Graphs/Figure6.png")
