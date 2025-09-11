# Main Orchestrator Script for Stratified NGM Analyses

# --- Load Libraries ---
message("Loading required libraries for the orchestrator script...")
if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
library(data.table)
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr") # Used by some core functions
library(dplyr)
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)
if (!requireNamespace("qs", quietly = TRUE)) install.packages("qs")
library(qs)
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2") # For plotting functions
library(ggplot2)
if (!requireNamespace("ggh4x", quietly = TRUE)) install.packages("ggh4x") # For plotting functions
library(ggh4x)
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr") # For separate_wider_delim
library(tidyr)


# --- Source Local Function Scripts ---
message("Sourcing local function scripts...")
core_functions_script <- here::here("scripts", "ngm_analysis", "core_analysis_functions.R")
plotting_functions_script <- here::here("scripts", "ngm_analysis", "plotting_functions.r")

if (!file.exists(core_functions_script)) {
    stop(paste("CRITICAL ERROR: Core analysis functions script not found at:", core_functions_script))
}
source(core_functions_script)
message(paste("Sourced core functions from:", core_functions_script))

if (!file.exists(plotting_functions_script)) {
    stop(paste("CRITICAL ERROR: Plotting functions script not found at:", plotting_functions_script))
}
source(plotting_functions_script)
message(paste("Sourced plotting functions from:", plotting_functions_script))


# --- Global Parameters & Paths ---
message("Defining global parameters and paths...")
# Data paths (relative to project root via here::here())
# Prefer Zenodo CSV directory if present; fall back to legacy RDS
ZENODO_DIR <- here::here("data", "zenodo")
RAW_SURVEY_DATA_RDS_PATH <- if (dir.exists(ZENODO_DIR)) ZENODO_DIR else here::here("data", "reconnect_survey.rds")

# Paths for new 2-way stratified census files
CENSUS_DATA_AGE_ETH_QS_PATH <- here::here("data", "derived", "census_age_eth_data.qs")
CENSUS_DATA_AGE_SES_QS_PATH <- here::here("data", "derived", "census_age_ses_data.qs")
CENSUS_DATA_AGE_GENDER_QS_PATH <- here::here("data", "derived", "census_age_sex_data.qs")
CENSUS_DATA_SES_ETH_QS_PATH <- here::here("data", "derived", "census_ses_eth_data.qs")

# Main output directories
MAIN_CACHE_DIR <- here::here("cache", "stratified_ngm_cache")
MAIN_OUTPUT_DIR <- here::here("results", "stratified_ngm_output")
MAIN_RESULTS_DIR <- here::here("results", "stratified_ngm_results_inline") # For Quarto inline data

# Create main directories if they don't exist
if (!dir.exists(MAIN_CACHE_DIR)) dir.create(MAIN_CACHE_DIR, recursive = TRUE)
if (!dir.exists(MAIN_OUTPUT_DIR)) dir.create(MAIN_OUTPUT_DIR, recursive = TRUE)
if (!dir.exists(MAIN_RESULTS_DIR)) dir.create(MAIN_RESULTS_DIR, recursive = TRUE)

# Analysis control parameters (can be overridden by specific analysis configs if needed)
# These should ideally be defined in core_analysis_functions.R and used from there.
# For run-specific control, they can be re-defined here or passed into functions.
# PERFORM_BOOTSTRAP_ANALYSIS is already in core_analysis_functions.R
# N_BOOTSTRAP_ITERATIONS_GLOBAL is already in core_analysis_functions.R
OVERWRITE_MAIN_CACHE <- TRUE # Overwrite consolidated results for an analysis type
OVERWRITE_ITERATION_CACHE <- TRUE # Overwrite individual bootstrap iteration caches


# Helper function to format reference group for display in plot subtitles
.format_ref_group_for_subtitle <- function(ref_group_list) {
    if (is.null(ref_group_list) || length(ref_group_list) == 0 || !is.list(ref_group_list)) {
        return("N/A")
    }
    # Sanitize and format
    ref_items <- lapply(names(ref_group_list), function(name) {
        val <- ref_group_list[[name]]
        paste0(tools::toTitleCase(name), ": '", val, "'")
    })
    paste(ref_items, collapse = ", ")
}

# --- Define Analysis Configurations ---
# A list of configurations, where each element is a list defining one analysis run.
# Key elements for each config:
#   - analysis_type_name (character): e.g., "Age_Ethnicity_Stratified"
#   - stratification_vars (character vector): e.g., c("Age", "Ethnicity")
#   - reference_group_strata (named list): For relative burden calculation.
#                                         Must match the stratification_vars.
#                                         e.g., list(Age = "60-64")
#   - (optional) n_bootstrap_iterations: Specific for this analysis type.
#   - (optional) overwrite_main_cache_this_run: Specific for this analysis type.
#   - (optional) overwrite_iteration_cache_this_run: Specific for this analysis type.
#   - (NEW) census_data_path: Path to the specific census .qs file for this analysis.
#   - (NEW) custom_age_banding: (optional) list with breaks and labels for age.

# Define 5-year age bands for the new analysis
age_breaks_5y <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 120)
age_labels_5y <- c(
    "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
    "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75+"
)

message("Defining analysis configurations...")
ANALYSIS_CONFIGURATIONS <- list(
    # --- Configurations using new 2-way census data and 5-year age bands ---
    list(
        analysis_type_name = "Age_Eth_Stratified_5y",
        stratification_vars = c("Age", "Ethnicity"),
        reference_group_strata = list(Ethnicity = "White"),
        reference_group_by_stratum = "Age",
        census_data_path = CENSUS_DATA_AGE_ETH_QS_PATH,
        custom_age_banding = list(breaks = age_breaks_5y, labels = age_labels_5y)
    ),
    list(
        analysis_type_name = "Age_SES_Stratified_5y",
        stratification_vars = c("Age", "SES"),
        reference_group_strata = list(SES = "1"),
        reference_group_by_stratum = "Age",
        census_data_path = CENSUS_DATA_AGE_SES_QS_PATH,
        custom_age_banding = list(breaks = age_breaks_5y, labels = age_labels_5y)
    ),
    list(
        analysis_type_name = "Age_Gender_Stratified_5y",
        stratification_vars = c("Age", "Gender"),
        reference_group_strata = list(Gender = "Female"),
        reference_group_by_stratum = "Age",
        census_data_path = CENSUS_DATA_AGE_GENDER_QS_PATH,
        custom_age_banding = list(breaks = age_breaks_5y, labels = age_labels_5y)
    ),
    list(
        analysis_type_name = "Age_Only_Stratified_5y",
        stratification_vars = c("Age"),
        reference_group_strata = list(Age = "60-64"),
        census_data_path = CENSUS_DATA_AGE_GENDER_QS_PATH, # Can use any of them for age only
        custom_age_banding = list(breaks = age_breaks_5y, labels = age_labels_5y)
    ),
    # --- Univariable analyses for Eth, SES, Gender ---
    list(
        analysis_type_name = "Eth_Only_Stratified",
        stratification_vars = c("Ethnicity"),
        reference_group_strata = list(Ethnicity = "White"),
        census_data_path = CENSUS_DATA_AGE_ETH_QS_PATH
    ),
    list(
        analysis_type_name = "SES_Only_Stratified",
        stratification_vars = c("SES"),
        reference_group_strata = list(SES = "1"),
        census_data_path = CENSUS_DATA_AGE_SES_QS_PATH
    ),
    list(
        analysis_type_name = "Gender_Only_Stratified",
        stratification_vars = c("Gender"),
        reference_group_strata = list(Gender = "Female"),
        census_data_path = CENSUS_DATA_AGE_GENDER_QS_PATH
    ),
    list(
        analysis_type_name = "Eth_SES_Stratified",
        stratification_vars = c("Ethnicity", "SES"),
        reference_group_strata = list(Ethnicity = "White"),
        reference_group_by_stratum = "SES",
        census_data_path = CENSUS_DATA_SES_ETH_QS_PATH
    )
)

# --- Load and Pre-process Data (once for survey data) ---
message("Loading and pre-processing base survey data (once)...")
# The custom age banding will be applied inside the loop.
# Here we just load the raw data. The original age groups don't matter as they'll be replaced.
survey_data_list <- load_and_preprocess_survey_data(
    survey_rds_path = RAW_SURVEY_DATA_RDS_PATH,
    age_breaks = AGE_BREAKS, # Placeholder, will be replaced by custom banding
    age_levels = FINAL_AGE_LEVELS, # Placeholder
    ses_levels = FINAL_SES_LEVELS, # Defined in core_analysis_functions.R
    eth_levels = FINAL_ETH_LEVELS, # Defined in core_analysis_functions.R
    gender_levels = FINAL_GENDER_LEVELS # ADDED: Pass gender_levels
)
participants_full_dt <- survey_data_list$participants
contacts_full_dt <- survey_data_list$contacts

# Census data will be loaded inside the loop for each configuration.
# census_full_dt <- load_and_prepare_census_data(...) # This line is removed from here.

# Check if survey data loading was successful
if (is.null(participants_full_dt) || nrow(participants_full_dt) == 0) {
    stop("CRITICAL ERROR: Participant data is NULL or empty after loading.")
}
if (is.null(contacts_full_dt) || nrow(contacts_full_dt) == 0) {
    stop("CRITICAL ERROR: Contact data is NULL or empty after loading.")
}

message("Base data loaded successfully.")
message(paste0("  Participants: ", nrow(participants_full_dt)))
message(paste0("  Contacts: ", nrow(contacts_full_dt)))


# --- Main Loop: Iterate Through Analysis Configurations ---
message("\nStarting main analysis loop through configurations...")

all_analysis_summaries_list <- list() # To store key summaries for final reporting

for (config_idx in seq_along(ANALYSIS_CONFIGURATIONS)) {
    current_config <- ANALYSIS_CONFIGURATIONS[[config_idx]]
    analysis_name <- current_config$analysis_type_name
    strat_vars_config <- current_config$stratification_vars
    ref_group_config <- current_config$reference_group_strata
    current_census_path <- current_config$census_data_path
    ref_group_by_stratum_config <- current_config$reference_group_by_stratum
    expected_census_vars_config <- current_config$expected_census_vars

    # Get run-specific control parameters or use global defaults
    perform_bootstrap_this_run <- ifelse(!is.null(current_config$perform_bootstrap), current_config$perform_bootstrap, PERFORM_BOOTSTRAP_ANALYSIS)
    n_iterations_this_run <- ifelse(!is.null(current_config$n_bootstrap_iterations), current_config$n_bootstrap_iterations, N_BOOTSTRAP_ITERATIONS_GLOBAL)
    overwrite_main_cache_this_run <- ifelse(!is.null(current_config$overwrite_main_cache_this_run), current_config$overwrite_main_cache_this_run, OVERWRITE_MAIN_CACHE)
    overwrite_iteration_cache_this_run <- ifelse(!is.null(current_config$overwrite_iteration_cache_this_run), current_config$overwrite_iteration_cache_this_run, OVERWRITE_ITERATION_CACHE)

    log_prefix_config <- paste0("Config ", config_idx, "/", length(ANALYSIS_CONFIGURATIONS), ": ''", analysis_name, "''")
    message(paste0("\n>>> Processing ", log_prefix_config, " <<<"))
    message(paste0("    Stratification by: ", paste(strat_vars_config, collapse = " & ")))
    message(paste0("    Bootstrap: ", perform_bootstrap_this_run, ", Iterations: ", if (perform_bootstrap_this_run) n_iterations_this_run else "N/A"))
    message(paste0("    Census data source: ", current_census_path))

    # --- Handle custom age banding for this configuration ---
    # Create copies of survey data to modify for this iteration if needed
    participants_iter_dt <- copy(participants_full_dt)
    contacts_iter_dt <- copy(contacts_full_dt)
    ALL_STRATA_LEVELS_LIST_iter <- copy(ALL_STRATA_LEVELS_LIST) # Use a mutable copy

    current_age_levels <- ALL_STRATA_LEVELS_LIST_iter$Age # Default to coarse

    if (!is.null(current_config$custom_age_banding)) {
        message("    Applying custom age banding for this analysis...")
        custom_breaks <- current_config$custom_age_banding$breaks
        custom_labels <- current_config$custom_age_banding$labels

        # Re-bin participant ages
        if ("part_age" %in% names(participants_iter_dt)) {
            participants_iter_dt[, part_age_group := cut(part_age, breaks = custom_breaks, labels = custom_labels, right = FALSE, include.lowest = TRUE)]
        }

        # Re-bin contact ages
        if ("cnt_age" %in% names(contacts_iter_dt)) {
            contacts_iter_dt[, cnt_age_group := cut(cnt_age, breaks = custom_breaks, labels = custom_labels, right = FALSE, include.lowest = TRUE)]
        }

        # Update the strata levels list for this run
        ALL_STRATA_LEVELS_LIST_iter$Age <- custom_labels
        current_age_levels <- custom_labels
    } else if (analysis_name %in% c("Eth_Only_Stratified", "SES_Only_Stratified", "Gender_Only_Stratified")) {
        # For univariable analyses, census data has 5y bands. We need to load it
        # with the correct age levels before aggregation, even though survey data
        # for these runs might not be binned by age at all.
        message("    Using 5-year age levels for loading census data for univariable analysis.")
        current_age_levels <- age_labels_5y
    }

    # --- Special filtering for Age_SES_Stratified_5y analysis ---
    # Note: We keep all SES categories for infection share calculation, but will filter
    # the relative burden results to working-age categories later
    if (analysis_name == "Age_SES_Stratified_5y") {
        message("    Age_SES_Stratified_5y: Keeping all SES categories for infection share, will filter relative burden to working-age only.")
        # No filtering here - let the NGM include all categories for proper infection share calculation
    }

    # --- Load census data for THIS configuration ---
    census_full_dt_iter <- load_and_prepare_census_data(
        census_qs_path = current_census_path,
        expected_vars = expected_census_vars_config,
        age_levels = current_age_levels, # Use the determined levels
        ses_levels = ALL_STRATA_LEVELS_LIST_iter$SES,
        eth_levels = ALL_STRATA_LEVELS_LIST_iter$Ethnicity,
        gender_levels = FINAL_GENDER_LEVELS
    )
    if (is.null(census_full_dt_iter) || nrow(census_full_dt_iter) == 0) {
        stop(paste0("CRITICAL ERROR (Config '", analysis_name, "'): Census data is NULL or empty after loading from ", current_census_path))
    }

    # Note: No census filtering for Age_SES_Stratified_5y - we keep all categories for infection share calculation

    message(paste0("    Census data for '", analysis_name, "' loaded successfully. Entries: ", nrow(census_full_dt_iter)))

    # --- Setup Directories for this Analysis Type ---
    analysis_cache_dir <- file.path(MAIN_CACHE_DIR, analysis_name)
    analysis_output_dir <- file.path(MAIN_OUTPUT_DIR, analysis_name)

    if (!dir.exists(analysis_cache_dir)) dir.create(analysis_cache_dir, recursive = TRUE)
    if (!dir.exists(analysis_output_dir)) dir.create(analysis_output_dir, recursive = TRUE)
    message(paste0("    Cache directory: ", analysis_cache_dir))
    message(paste0("    Output directory: ", analysis_output_dir))

    # Placeholder for consolidated results file path
    consolidated_results_qs_path <- file.path(analysis_cache_dir, paste0(analysis_name, "_consolidated_results.qs"))

    # --- Run Analysis: Bootstrap or Single Run ---
    all_iteration_results_list <- list()
    # This will hold the direct result of a single run, or a list of all iteration results for bootstrap.
    run_outputs <- list()

    # Check if consolidated results already exist and if we should skip re-computing everything
    if (!overwrite_main_cache_this_run && file.exists(consolidated_results_qs_path)) {
        message(paste0("    Loading consolidated results from cache: ", consolidated_results_qs_path))
        final_results_for_plotting_and_saving <- qs::qread(consolidated_results_qs_path)
        # We still might need to regenerate plots based on another global switch (not implemented here)
        # For now, if consolidated cache exists & no overwrite, we assume we can skip to plotting/next config.
    } else {
        if (perform_bootstrap_this_run) {
            message(paste0("    Starting bootstrap process for ", n_iterations_this_run, " iterations..."))
            for (iter in 1:n_iterations_this_run) {
                iteration_log_prefix <- paste0(log_prefix_config, ", Iter: ", iter)

                message(paste0("      Calculating for iteration ", iter, "..."))
                # Resample participants
                participants_boot_dt <- participants_iter_dt[sample(.N, size = .N, replace = TRUE)]
                participants_boot_dt[, original_part_id_iter := part_id]
                participants_boot_dt[, part_id := .I]

                # Filter and update contacts based on the resampled participants
                contacts_temp_dt <- contacts_iter_dt[part_id %in% participants_boot_dt$original_part_id_iter]
                contacts_boot_dt <- merge(
                    contacts_temp_dt,
                    participants_boot_dt[, .(original_part_id_iter, part_id_new_iter = part_id)],
                    by.x = "part_id", by.y = "original_part_id_iter",
                    all.x = TRUE,
                    allow.cartesian = TRUE
                )
                contacts_boot_dt[, part_id := part_id_new_iter]
                contacts_boot_dt[, part_id_new_iter := NULL]

                if (anyNA(contacts_boot_dt$part_id)) {
                    warning(paste0(iteration_log_prefix, ": NA part_ids found in contacts_boot_dt after merge. This is unexpected."))
                    contacts_boot_dt <- contacts_boot_dt[!is.na(part_id)]
                }

                # Prepare analysis_config for this iteration
                iter_analysis_config <- list(
                    analysis_type_name = analysis_name,
                    stratification_vars = strat_vars_config,
                    reference_group_strata = ref_group_config,
                    reference_group_by_stratum = ref_group_by_stratum_config,
                    iteration_number = iter
                    # census_data_path is not needed by perform_single_stratified_ngm_calculation_set directly
                    # as it receives the already loaded census_dt_iter
                )

                iteration_result <- perform_single_stratified_ngm_calculation_set(
                    analysis_name = iter_analysis_config$analysis_type_name,
                    iteration_id = iter_analysis_config$iteration_number,
                    participants_dt_iter = participants_boot_dt,
                    contacts_dt_iter = contacts_boot_dt,
                    census_dt_iter = census_full_dt_iter, # Use the census data loaded for this iteration/config
                    stratification_vars = iter_analysis_config$stratification_vars,
                    reference_group_strata = iter_analysis_config$reference_group_strata,
                    reference_group_by_stratum = iter_analysis_config$reference_group_by_stratum,
                    all_strata_levels = ALL_STRATA_LEVELS_LIST_iter
                )

                # --- Filter SES for relative burden (keep all categories for infection share) ---
                if (analysis_name == "Age_SES_Stratified_5y" && !is.null(iteration_result$relative_burden_dt)) {
                    # message("      Filtering relative burden to working-age SES categories (1-7) and age groups (20-64).")
                    working_age_labels <- c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64")
                    working_ses_levels <- as.character(1:7)
                    iteration_result$relative_burden_dt <- iteration_result$relative_burden_dt[
                        Age %in% working_age_labels & SES %in% working_ses_levels
                    ]
                }

                message(paste0(
                    "      Iteration ", iter, " result: error_occurred = ", isTRUE(iteration_result$error_occurred),
                    ", R0 = ", ifelse(is.null(iteration_result$r0), "NULL/NA", round(iteration_result$r0, 3))
                ))

                all_iteration_results_list[[iter]] <- iteration_result
            } # End bootstrap iteration loop

            run_outputs <- all_iteration_results_list
        } else { # Single run (no bootstrap)
            message("    Performing a single run (no bootstrap)...")
            # Prepare analysis_config for single run (iteration_number can be NA or 1)
            single_run_analysis_config <- list(
                analysis_type_name = analysis_name,
                stratification_vars = strat_vars_config,
                reference_group_strata = ref_group_config,
                reference_group_by_stratum = ref_group_by_stratum_config,
                iteration_number = NA_integer_
            )
            # Census data (census_full_dt_iter) is already loaded at the start of the config loop.

            single_run_result <- perform_single_stratified_ngm_calculation_set(
                analysis_name = single_run_analysis_config$analysis_type_name,
                iteration_id = single_run_analysis_config$iteration_number,
                participants_dt_iter = participants_full_dt,
                contacts_dt_iter = contacts_full_dt,
                census_dt_iter = census_full_dt_iter, # Use the census data loaded for this config
                stratification_vars = single_run_analysis_config$stratification_vars,
                reference_group_strata = single_run_analysis_config$reference_group_strata,
                reference_group_by_stratum = single_run_analysis_config$reference_group_by_stratum,
                all_strata_levels = ALL_STRATA_LEVELS_LIST_iter
            )

            # --- Filter SES for relative burden (keep all categories for infection share) ---
            if (analysis_name == "Age_SES_Stratified_5y" && !is.null(single_run_result$relative_burden_dt)) {
                message("    Filtering relative burden to working-age SES categories (1-7) and age groups (20-64).")
                working_age_labels <- c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64")
                working_ses_levels <- as.character(1:7)
                single_run_result$relative_burden_dt <- single_run_result$relative_burden_dt[
                    Age %in% working_age_labels & SES %in% working_ses_levels
                ]
            }

            run_outputs <- list(single_run_result) # Store as a list of one for consistency with bootstrap path
        }

        # --- Consolidate Results ---
        message("    Consolidating results...")
        # run_outputs is now a list of results (1 element for single run, N elements for bootstrap)
        # Each element is a list from perform_single_stratified_ngm_calculation_set containing:
        # r0, eigenvector_dt, normalized_incidence_dt, projected_case_share_dt, relative_burden_dt, etc.

        if (length(run_outputs) == 0 || all(sapply(run_outputs, function(x) x$error_occurred))) {
            warning(paste0(log_prefix_config, ": No valid run outputs to consolidate. All runs may have failed."))
            # Create an empty shell for final_results_for_plotting_and_saving with error status
            final_results_for_plotting_and_saving <- list(
                analysis_type_name = analysis_name,
                stratification_vars = strat_vars_config,
                reference_group_strata = ref_group_config,
                reference_group_by_stratum = ref_group_by_stratum_config,
                perform_bootstrap = perform_bootstrap_this_run,
                n_iterations = ifelse(perform_bootstrap_this_run, n_iterations_this_run, 1),
                r0_summary = data.table(mean = NA_real_, median = NA_real_, lowerCI = NA_real_, upperCI = NA_real_),
                projected_case_share_summary_dt = data.table(),
                relative_contribution_summary_dt = data.table(),
                relative_burden_summary_dt = data.table(),
                mean_contact_matrix_final_dt = data.table(), # This would be from iter 1 or averaged if meaningful
                aggregated_census_final_dt = if (length(run_outputs) > 0 && !is.null(run_outputs[[1]]$aggregated_census_dt)) run_outputs[[1]]$aggregated_census_dt else data.table(),
                all_runs_had_errors = TRUE
            )
        } else {
            # Filter out runs that had errors, if any, before summarization
            valid_run_outputs <- Filter(function(x) !x$error_occurred, run_outputs)
            if (length(valid_run_outputs) < length(run_outputs)) {
                message(paste0("    Note: ", length(run_outputs) - length(valid_run_outputs), " run(s) had errors and were excluded from summarization."))
            }
            if (length(valid_run_outputs) == 0) {
                warning(paste0(log_prefix_config, ": No valid run outputs after filtering errors. Cannot consolidate."))
                # Similar empty shell as above
                final_results_for_plotting_and_saving <- list(
                    analysis_type_name = analysis_name,
                    stratification_vars = strat_vars_config,
                    reference_group_strata = ref_group_config,
                    reference_group_by_stratum = ref_group_by_stratum_config,
                    perform_bootstrap = perform_bootstrap_this_run,
                    n_iterations = ifelse(perform_bootstrap_this_run, n_iterations_this_run, 1),
                    r0_summary = data.table(mean = NA_real_, median = NA_real_, lowerCI = NA_real_, upperCI = NA_real_),
                    projected_case_share_summary_dt = data.table(),
                    relative_contribution_summary_dt = data.table(),
                    relative_burden_summary_dt = data.table(),
                    mean_contact_matrix_final_dt = data.table(),
                    aggregated_census_final_dt = if (length(run_outputs) > 0 && !is.null(run_outputs[[1]]$aggregated_census_dt)) run_outputs[[1]]$aggregated_census_dt else data.table(),
                    all_runs_had_errors = TRUE # Still true as no valid runs
                )
            } else {
                # R0 values: extract from each valid run
                r0_values <- vapply(valid_run_outputs, function(res) res$r0, numeric(1))
                r0_summary_stats <- data.table(
                    mean = mean(r0_values, na.rm = TRUE),
                    median = median(r0_values, na.rm = TRUE),
                    lowerCI = quantile(r0_values, probs = 0.025, na.rm = TRUE),
                    upperCI = quantile(r0_values, probs = 0.975, na.rm = TRUE)
                )
                if (!perform_bootstrap_this_run) { # For single run, CI is just the mean value
                    r0_summary_stats$lowerCI <- r0_summary_stats$mean
                    r0_summary_stats$upperCI <- r0_summary_stats$mean
                }

                # For data.table metrics (eigenvector, norm_incidence, etc.)
                # Use summarize_bootstrap_results if bootstrap, otherwise adapt single run output
                metrics_to_summarize <- list(
                    list(name = "projected_case_share", dt_name = "projected_case_share_dt", value_col = "ProjectedCaseShare"),
                    list(name = "relative_contribution", dt_name = "left_eigenvector_dt", value_col = "RelativeContribution"),
                    list(name = "relative_burden", dt_name = "relative_burden_dt", value_col = "RelativeBurden")
                )

                summarized_metrics_dts <- list()

                for (metric_info in metrics_to_summarize) {
                    list_of_dts_for_metric <- lapply(valid_run_outputs, function(res) res[[metric_info$dt_name]])
                    # Ensure all tables have the required value column and strat_vars_config columns before summarizing
                    # And that strat_vars_config are factors with correct levels (should be from calc function)

                    if (perform_bootstrap_this_run && length(list_of_dts_for_metric) > 1) {
                        summarized_dt <- summarize_bootstrap_results(
                            dt_list = list_of_dts_for_metric,
                            value_col_name = metric_info$value_col,
                            group_cols = strat_vars_config,
                            all_levels_list = ALL_STRATA_LEVELS_LIST_iter
                        )
                    } else { # Single run or only one valid bootstrap iteration
                        single_dt <- list_of_dts_for_metric[[1]]
                        if (!is.null(single_dt) && nrow(single_dt) > 0 && metric_info$value_col %in% names(single_dt)) {
                            summarized_dt <- copy(single_dt)
                            # For consistency with summarize_bootstrap_results, rename value col to mean, add median/CI
                            setnames(summarized_dt, metric_info$value_col, paste0(metric_info$value_col, "_mean"))
                            summarized_dt[, paste0(metric_info$value_col, "_median") := get(paste0(metric_info$value_col, "_mean"))]
                            summarized_dt[, paste0(metric_info$value_col, "_lowerCI") := get(paste0(metric_info$value_col, "_mean"))]
                            summarized_dt[, paste0(metric_info$value_col, "_upperCI") := get(paste0(metric_info$value_col, "_mean"))]
                        } else {
                            # Create empty structure if single_dt is NULL or empty
                            empty_summary_cols <- c(strat_vars_config, paste0(metric_info$value_col, c("_mean", "_median", "_lowerCI", "_upperCI")))
                            summarized_dt <- data.table(matrix(ncol = length(empty_summary_cols), nrow = 0))
                            setnames(summarized_dt, empty_summary_cols)
                            for (col in strat_vars_config) {
                                summarized_dt[, (col) := factor(levels = ALL_STRATA_LEVELS_LIST_iter[[col]])]
                            }
                            for (stat_col in grep(paste0("^", metric_info$value_col, "_"), names(summarized_dt), value = TRUE)) {
                                summarized_dt[, (stat_col) := numeric()]
                            }
                        }
                    }
                    summarized_metrics_dts[[paste0(metric_info$name, "_summary_dt")]] <- summarized_dt
                }

                final_results_for_plotting_and_saving <- list(
                    analysis_type_name = analysis_name,
                    stratification_vars = strat_vars_config,
                    reference_group_strata = ref_group_config,
                    reference_group_by_stratum = ref_group_by_stratum_config,
                    perform_bootstrap = perform_bootstrap_this_run,
                    n_iterations_completed = length(valid_run_outputs),
                    n_iterations_target = ifelse(perform_bootstrap_this_run, n_iterations_this_run, 1),
                    r0_summary = r0_summary_stats
                )
                final_results_for_plotting_and_saving <- c(final_results_for_plotting_and_saving, summarized_metrics_dts)

                # Add mean contact matrix (from first valid run for simplicity, or could be averaged if appropriate)
                # And aggregated census (also from first valid run)
                if (length(valid_run_outputs) > 0) {
                    final_results_for_plotting_and_saving$mean_contact_matrix_final_dt <- valid_run_outputs[[1]]$mean_contact_matrix_flat_dt
                    final_results_for_plotting_and_saving$aggregated_census_final_dt <- valid_run_outputs[[1]]$aggregated_census_dt
                }
                final_results_for_plotting_and_saving$all_runs_had_errors <- FALSE # Since we have valid_run_outputs
            }
        }

        # Save the consolidated results
        qs::qsave(final_results_for_plotting_and_saving, consolidated_results_qs_path)
        message(paste0("    Saved consolidated results to: ", consolidated_results_qs_path))
    }

    # --- Generate Plots (if results are valid) ---
    if (!is.null(final_results_for_plotting_and_saving) &&
        (exists("all_runs_had_errors", where = final_results_for_plotting_and_saving) && !final_results_for_plotting_and_saving$all_runs_had_errors)) {
        message("    Generating plots...")

        plot_ngm_approach_label <- ifelse(perform_bootstrap_this_run,
            paste0("(Bootstrap N=", final_results_for_plotting_and_saving$n_iterations_completed, ")"),
            "(Single Run)"
        )

        # Plot Transmission Contribution (from left eigenvector)
        if ("relative_contribution_summary_dt" %in% names(final_results_for_plotting_and_saving) &&
            nrow(final_results_for_plotting_and_saving$relative_contribution_summary_dt) > 0) {
            generate_stratified_ngm_plots(
                data_dt = final_results_for_plotting_and_saving$relative_contribution_summary_dt,
                value_col = "RelativeContribution_mean",
                lower_ci_col = if (perform_bootstrap_this_run) "RelativeContribution_lowerCI" else NULL,
                upper_ci_col = if (perform_bootstrap_this_run) "RelativeContribution_upperCI" else NULL,
                stratification_vars = strat_vars_config,
                all_strata_list = ALL_STRATA_LEVELS_LIST_iter,
                top_n_count = 20,
                plot_title_stem = "Transmission Contribution by",
                plot_subtitle_stem = "",
                x_axis_label = "Contribution to R0",
                output_dir = analysis_output_dir,
                output_filename_base_suffix = paste0(analysis_name, "_transmission_contribution"),
                ngm_approach_label = plot_ngm_approach_label,
                y_scale_labels_format = scales::percent_format(accuracy = 0.1),
                log_axis_labels_decimal = FALSE
            )
        } else {
            message("    Skipping Transmission Contribution plots as data is missing or empty.")
        }

        # Plot Relative Risk of Infection (Relative Burden)
        if ("relative_burden_summary_dt" %in% names(final_results_for_plotting_and_saving) &&
            nrow(final_results_for_plotting_and_saving$relative_burden_summary_dt) > 0) {
            generate_stratified_ngm_plots(
                data_dt = final_results_for_plotting_and_saving$relative_burden_summary_dt,
                value_col = "RelativeBurden_mean",
                lower_ci_col = if (perform_bootstrap_this_run) "RelativeBurden_lowerCI" else NULL,
                upper_ci_col = if (perform_bootstrap_this_run) "RelativeBurden_upperCI" else NULL,
                stratification_vars = strat_vars_config,
                all_strata_list = ALL_STRATA_LEVELS_LIST_iter,
                top_n_count = 20,
                plot_title_stem = "Relative Risk of Infection by",
                plot_subtitle_stem = paste("Reference:", .format_ref_group_for_subtitle(ref_group_config)),
                x_axis_label = "Relative Risk of Infection (vs Reference)",
                output_dir = analysis_output_dir,
                output_filename_base_suffix = paste0(analysis_name, "_risk_ratio"),
                ngm_approach_label = plot_ngm_approach_label,
                y_scale_labels_format = scales::number_format(accuracy = 0.01),
                use_log_value_axis = TRUE, # Relative risk of infection often benefits from log scale
                filter_positive_value_for_log = TRUE,
                log_axis_labels_decimal = TRUE
            )
        } else {
            message("    Skipping Relative Risk of Infection plots as data is missing or empty.")
        }
    } else {
        message(paste0("    Skipping plot generation for ", analysis_name, " as consolidated results are missing or indicated errors."))
    }

    # --- Extract key metrics for final summary / Quarto ---
    if (!is.null(final_results_for_plotting_and_saving) &&
        (exists("all_runs_had_errors", where = final_results_for_plotting_and_saving) && !final_results_for_plotting_and_saving$all_runs_had_errors)) {
        # Store the entire consolidated results object. This object contains all summary
        # data tables (R0, incidence, etc.) needed for inline reporting in Quarto.
        all_analysis_summaries_list[[analysis_name]] <- final_results_for_plotting_and_saving
    } else {
        # Store basic info even if errors occurred, to indicate it was attempted.
        # Add reference group strata to avoid errors in Quarto if it tries to access it.
        all_analysis_summaries_list[[analysis_name]] <- list(
            analysis_name = analysis_name,
            stratification_vars = strat_vars_config,
            status = "Error in calculation or consolidation",
            reference_group_strata = ref_group_config,
            reference_group_by_stratum = ref_group_by_stratum_config
        )
    }

    message(paste0(">>> Completed Processing for ", log_prefix_config, " <<<"))
} # End of loop through ANALYSIS_CONFIGURATIONS

message("\nAll analysis configurations processed.")

# Save all_analysis_summaries_list for Quarto if populated
if (exists("all_analysis_summaries_list") && length(all_analysis_summaries_list) > 0) {
    quarto_summary_file_path <- file.path(MAIN_RESULTS_DIR, "all_stratified_analyses_summary_for_quarto.qs")
    tryCatch(
        {
            qs::qsave(all_analysis_summaries_list, quarto_summary_file_path)
            message(paste("Successfully saved Quarto summary data to:", quarto_summary_file_path))
        },
        error = function(e) {
            warning(paste("Failed to save Quarto summary data:", e$message))
        }
    )
} else {
    message("No summary data for Quarto to save (all_analysis_summaries_list is empty or does not exist).")
}

# --- End of Script ---
message("Stratified NGM analysis run script finished.")
