"""
fetch_ssp_data.py  –  Fetch GDP|PPP and Population projections from the
                       IIASA SSP Scenario Database 3.2 via pyam.

The output is a long-format CSV used by R/02_load_data.R.

Requirements
------------
    pip install pyam-iamc pandas

Authentication
--------------
First-time use prompts for IIASA credentials.
Register at https://data.ece.iiasa.ac.at to create a free account.
Credentials are cached locally after the first login.

Usage
-----
    python python/fetch_ssp_data.py

Output
------
    data/ssp_data.csv  –  IAMC long format with columns:
        model, scenario, region, variable, unit, year, value

    After downloading, open data/ssp_data.csv and note the model names.
    Update SSPModelGDP and SSPModelPopulation in R/01_parameters.R to pin
    the model selection for reproducibility.
"""

from __future__ import annotations

import os
import sys

# ── Dependency check ─────────────────────────────────────────────────────────
try:
    import pyam
except ImportError:
    sys.exit(
        "ERROR: pyam not found.\n"
        "Install with:  pip install pyam-iamc pandas\n"
        "See https://pyam-iamc.readthedocs.io/"
    )

import pandas as pd

# ── Configuration ─────────────────────────────────────────────────────────────
DATABASE   = "ssp"                               # IIASA SSP Scenario Explorer 3.2
VARIABLES  = ["GDP|PPP", "Population"]
SCENARIOS  = ["SSP1", "SSP2", "SSP3", "SSP4", "SSP5"]

OUT_DIR    = "data"
OUT_FILE   = os.path.join(OUT_DIR, "ssp_data.csv")

# ── Fetch data ────────────────────────────────────────────────────────────────
os.makedirs(OUT_DIR, exist_ok=True)

print(f"Connecting to IIASA {DATABASE.upper()} Scenario Explorer ...")
try:
    df = pyam.read_iiasa(DATABASE, variable=VARIABLES, scenario=SCENARIOS)
except Exception as exc:
    sys.exit(
        f"ERROR: Could not retrieve data from the SSP Scenario Explorer.\n"
        f"  {exc}\n\n"
        "  Possible causes:\n"
        "    • No internet connection\n"
        "    • Invalid or expired IIASA credentials (re-run to re-authenticate)\n"
        "    • pyam version incompatible with the database API\n\n"
        "  See https://pyam-iamc.readthedocs.io/en/stable/tutorials/iiasa.html"
    )

# ── Summary ───────────────────────────────────────────────────────────────────
print("\nDownloaded data summary:")
print(f"  Models:     {list(df.model)}")
print(f"  Scenarios:  {list(df.scenario)}")
print(f"  Variables:  {list(df.variable)}")
print(f"  Regions:    {len(df.region)} regions")
print(f"  Years:      {sorted(df.year)}")
print(f"  Total rows: {len(df.data):,}")

# ── Save ──────────────────────────────────────────────────────────────────────
df.data.to_csv(OUT_FILE, index=False)
print(f"\nSaved to {OUT_FILE}")

# ── Guidance for R/01_parameters.R ────────────────────────────────────────────
print(
    "\n" + "=" * 70 + "\n"
    "Next step: pin the model names in R/01_parameters.R\n"
    "=" * 70 + "\n"
    "\nAvailable Population models:"
)
for m in df.filter(variable="Population").model:
    print(f"  {m}")

print("\nAvailable GDP|PPP models:")
for m in df.filter(variable="GDP|PPP").model:
    print(f"  {m}")

print(
    "\nExample (add to R/01_parameters.R):\n"
    '  SSPModelPopulation <- "<model name from above>"\n'
    '  SSPModelGDP        <- "<model name from above>"\n'
)
