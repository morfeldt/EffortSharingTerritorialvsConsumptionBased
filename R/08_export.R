# =============================================================================
# 08_export.R  –  Write output tables to Excel
# =============================================================================
# Produces:
#   output/NationalCarbonBudgets.xlsx   – full results (one sheet)
#   output/DataForSupplementary.xlsx    – supplementary data (one sheet per panel)
# =============================================================================

dir.create("output", recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Full results table
# -----------------------------------------------------------------------------
write.xlsx(
  NationalCarbonBudgets %>% mutate(across(where(is.factor), as.character)),
  "output/NationalCarbonBudgets.xlsx",
  sheetName  = "NationalCarbonBudgets",
  row.names  = FALSE,
  overwrite  = TRUE
)
message("Written: output/NationalCarbonBudgets.xlsx")

# -----------------------------------------------------------------------------
# Supplementary data  (assembled in 07_plot_supplementary.R)
# -----------------------------------------------------------------------------
# DataForSupplementary is a named list: names are "{TempTarget}{CountrySet}"
# e.g. "1.5All", "1.5EU", "2All", "2EU"

supp_path <- "output/DataForSupplementary.xlsx"
if (file.exists(supp_path)) file.remove(supp_path)

wb <- createWorkbook()

for (sheet_key in names(DataForSupplementary)) {
  sheet_name <- paste0("TempTarget", sheet_key)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, DataForSupplementary[[sheet_key]])
}

saveWorkbook(wb, supp_path, overwrite = TRUE)
message("Written: output/DataForSupplementary.xlsx")
