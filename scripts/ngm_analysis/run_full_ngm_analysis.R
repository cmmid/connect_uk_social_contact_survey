# This script runs the full analysis pipeline, from data cleaning to rendering the final report.

# Set a default CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com/"))


message("--- STARTING FULL ANALYSIS PIPELINE ---")

# --- 1. Clean Census Data ---
message("\n--- Step 1: Cleaning census data ---")
source("scripts/analyses/clean/clean_census_new.R")

# --- 2. Run Stratified NGM Analyses ---
message("\n--- Step 2: Running stratified NGM analyses ---")
source("scripts/ngm_analysis/stratified_analyses.R")

# --- 3. Generate All Plots ---
message("\n--- Step 3: Generating all plots (unified script) ---")
# Set configuration for all plots
source("scripts/ngm_analysis/generate_all_plots.R")

# --- 4. Create Appendix Tables ---
message("\n--- Step 4: Creating appendix tables ---")
source("scripts/ngm_analysis/create_appendix_tables.R")

# --- 5. Render Quarto Report ---
message("\n--- Step 5: Rendering Quarto report ---")
if (!requireNamespace("quarto", quietly = TRUE)) {
    install.packages("quarto")
}
quarto::quarto_render("scripts/ngm_analysis/ngm_analysis.qmd")

message("\n--- FULL ANALYSIS PIPELINE COMPLETE ---")
