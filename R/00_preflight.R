# =============================================================================
# 00_preflight.R  –  Pre-flight checks
# =============================================================================
# Verifies that all required input files and credentials are present before
# any data loading or computation begins. Stops with a clear message if
# anything is missing.
# =============================================================================

.problems <- character(0)

# 1. Global Carbon Project data files
#    Download from: https://globalcarbonbudget.org/datahub/
#    Two files are needed — update the filenames in 02_load_data.R if a new
#    version has been published (e.g. "...2025v1.0.xlsx").
if (!file.exists("data/National_Fossil_Carbon_Emissions_2025_v0.3.xlsx"))
  .problems <- c(.problems, paste(
    "MISSING: National_Fossil_Carbon_Emissions_2025_v0.3.xlsx",
    "  Download from https://globalcarbonbudget.org/datahub/ and place in",
    "  the project root. If a newer version is available, also update the",
    "  filename on line 48 of R/02_load_data.R.", sep = "\n"))

if (!file.exists("data/Global_Carbon_Budget_2025_v0.6.xlsx"))
  .problems <- c(.problems, paste(
    "MISSING: Global_Carbon_Budget_2025_v0.6.xlsx",
    "  Download from https://globalcarbonbudget.org/datahub/ and place in",
    "  the project root. If a newer version is available, also update the",
    "  filename on line 69 of R/02_load_data.R.", sep = "\n"))

# 2. UN Population data (only needed on first run; cached afterwards)
if (!file.exists("data/un_population.csv") &&
    nchar(trimws(Sys.getenv("UN_POP_TOKEN"))) == 0)
  .problems <- c(.problems, paste(
    "MISSING: UN Population API token (needed for first run only).",
    "  Register and generate a token at:",
    "  https://population.un.org/dataportalapi/index.html",
    "  Then run:  Sys.setenv(UN_POP_TOKEN = \"your_token_here\")",
    "  Or add UN_POP_TOKEN=your_token_here to your .Renviron file.",
    "  After the first successful run the data is cached to",
    "  data/un_population.csv and the token is no longer needed.", sep = "\n"))

# 3. SSP scenario data
if (!file.exists("data/ssp_data.csv"))
  .problems <- c(.problems, paste(
    "MISSING: data/ssp_data.csv",
    "  Run the following to download SSP scenario data:",
    "  python python/fetch_ssp_data.py",
    "  (requires a free IIASA account)", sep = "\n"))

if (length(.problems) > 0) {
  message("\n", paste(rep("=", 70), collapse = ""))
  message("ACTION REQUIRED before this script can run:\n")
  message(paste(.problems, collapse = "\n\n"))
  message(paste(rep("=", 70), collapse = ""), "\n")
  stop("Resolve the issues above, then re-run main.R.", call. = FALSE)
}
rm(.problems)
