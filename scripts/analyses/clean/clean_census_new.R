library(readxl)
library(dplyr)
library(qs)
library(tibble)

# Define final demographic levels
final_ses_levels <- c("1", "2", "3", "4", "5", "6", "7", "Retired", "Student", "Under 17", "Unemployed", "Unknown")
final_eth_levels <- c("White", "Asian", "Black", "Mixed/Other")
final_sex_levels <- c("Male", "Female", "Unknown")
# Define new 5-year age bands
final_age_levels_5y <- c(
    "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
    "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75+"
)

# --- File Paths ---
# Input files for 2-way stratifications
CENSUS_FILE_AGE_ETH <- "data/census_for_ngm/age_eth_custom-filtered-2025-06-17T08_42_20Z.xlsx"
CENSUS_FILE_AGE_SES <- "data/census_for_ngm/age_ses_custom-filtered-2025-06-17T08_43_57Z.xlsx"
CENSUS_FILE_AGE_SEX <- "data/census_for_ngm/age_sex_custom-filtered-2025-06-17T08_44_45Z.xlsx"
CENSUS_FILE_SES_ETH <- "data/census_for_ngm/ses_eth_custom-filtered-2025-06-19T11_21_51Z.xlsx"

# Output files for 2-way stratifications
OUTPUT_QS_AGE_ETH <- "data/derived/census_age_eth_data.qs"
OUTPUT_QS_AGE_SES <- "data/derived/census_age_ses_data.qs"
OUTPUT_QS_AGE_SEX <- "data/derived/census_age_sex_data.qs"
OUTPUT_QS_SES_ETH <- "data/derived/census_ses_eth_data.qs"

if(!file.exists('data/derived')){dir.create('data/derived')}

# --- Helper function for 5-year age mapping ---
map_age_categories_5y <- function(df, age_category_raw_col_name) {
    df %>%
        mutate(
            age_category = as.character(.data[[age_category_raw_col_name]]),
            # Group all 75+ categories together
            age_category = if_else(
                grepl("75 to 79|80 to 84|85 years and over", age_category),
                "75+",
                age_category
            ),
            # Remap to shorter labels
            age_category = case_when(
                age_category == "Aged 4 years and under" ~ "0-4",
                age_category == "Aged 5 to 9 years" ~ "5-9",
                age_category == "Aged 10 to 14 years" ~ "10-14",
                age_category == "Aged 15 to 19 years" ~ "15-19",
                age_category == "Aged 20 to 24 years" ~ "20-24",
                age_category == "Aged 25 to 29 years" ~ "25-29",
                age_category == "Aged 30 to 34 years" ~ "30-34",
                age_category == "Aged 35 to 39 years" ~ "35-39",
                age_category == "Aged 40 to 44 years" ~ "40-44",
                age_category == "Aged 45 to 49 years" ~ "45-49",
                age_category == "Aged 50 to 54 years" ~ "50-54",
                age_category == "Aged 55 to 59 years" ~ "55-59",
                age_category == "Aged 60 to 64 years" ~ "60-64",
                age_category == "Aged 65 to 69 years" ~ "65-69",
                age_category == "Aged 70 to 74 years" ~ "70-74",
                age_category == "75+" ~ "75+",
                TRUE ~ "Unknown" # Should not be used if all raw names are handled
            )
        ) %>%
        filter(age_category %in% final_age_levels_5y) %>%
        mutate(age_category = factor(age_category, levels = final_age_levels_5y)) %>%
        mutate(age_lower_bound_approx = case_when(
            age_category == "0-4" ~ 0,
            age_category == "5-9" ~ 5,
            age_category == "10-14" ~ 10,
            age_category == "15-19" ~ 15,
            age_category == "20-24" ~ 20,
            age_category == "25-29" ~ 25,
            age_category == "30-34" ~ 30,
            age_category == "35-39" ~ 35,
            age_category == "40-44" ~ 40,
            age_category == "45-49" ~ 45,
            age_category == "50-54" ~ 50,
            age_category == "55-59" ~ 55,
            age_category == "60-64" ~ 60,
            age_category == "65-69" ~ 65,
            age_category == "70-74" ~ 70,
            age_category == "75+" ~ 75,
            TRUE ~ NA_real_
        ))
}

# --- Helper function for SES derivation (reusable) ---
derive_ses <- function(df, ns_sec_raw_col, econ_activity_raw_col) {
    df %>%
        mutate(SES = case_when(
            age_lower_bound_approx < 16 ~ "Under 17",
            age_lower_bound_approx >= 65 & .data[[econ_activity_raw_col]] == "Economically inactive" ~ "Retired",
            age_lower_bound_approx >= 65 & .data[[econ_activity_raw_col]] == "Economically active: In employment (including full-time students)" & grepl("L15", .data[[ns_sec_raw_col]]) ~ "Student",
            age_lower_bound_approx >= 65 & .data[[econ_activity_raw_col]] == "Economically active: In employment (including full-time students)" & grepl("L1,|L2,|L3", .data[[ns_sec_raw_col]]) ~ "1",
            age_lower_bound_approx >= 65 & .data[[econ_activity_raw_col]] == "Economically active: In employment (including full-time students)" & grepl("L4,|L5,|L6", .data[[ns_sec_raw_col]]) ~ "2",
            age_lower_bound_approx >= 65 & .data[[econ_activity_raw_col]] == "Economically active: In employment (including full-time students)" & grepl("L7", .data[[ns_sec_raw_col]]) ~ "3",
            age_lower_bound_approx >= 65 & .data[[econ_activity_raw_col]] == "Economically active: In employment (including full-time students)" & grepl("L8,|L9", .data[[ns_sec_raw_col]]) ~ "4",
            age_lower_bound_approx >= 65 & .data[[econ_activity_raw_col]] == "Economically active: In employment (including full-time students)" & grepl("L10,|L11", .data[[ns_sec_raw_col]]) ~ "5",
            age_lower_bound_approx >= 65 & .data[[econ_activity_raw_col]] == "Economically active: In employment (including full-time students)" & grepl("L12", .data[[ns_sec_raw_col]]) ~ "6",
            age_lower_bound_approx >= 65 & .data[[econ_activity_raw_col]] == "Economically active: In employment (including full-time students)" & grepl("L13", .data[[ns_sec_raw_col]]) ~ "7",
            age_lower_bound_approx >= 65 & .data[[econ_activity_raw_col]] == "Economically active: In employment (including full-time students)" ~ "Unknown", # Default for this complex case
            age_lower_bound_approx >= 65 & .data[[econ_activity_raw_col]] == "Economically active: Unemployed (including full-time students)" & grepl("L15", .data[[ns_sec_raw_col]]) ~ "Student",
            age_lower_bound_approx >= 65 & .data[[econ_activity_raw_col]] == "Economically active: Unemployed (including full-time students)" ~ "Unemployed",
            age_lower_bound_approx >= 65 & .data[[econ_activity_raw_col]] == "Does not apply" & .data[[ns_sec_raw_col]] == "Does not apply" ~ "Retired", # Assuming "Does not apply" for econ activity in 65+ is retired
            age_lower_bound_approx >= 65 & .data[[econ_activity_raw_col]] == "Does not apply" & grepl("L15", .data[[ns_sec_raw_col]]) ~ "Student",
            age_lower_bound_approx >= 65 & .data[[econ_activity_raw_col]] == "Does not apply" & grepl("L1,|L2,|L3", .data[[ns_sec_raw_col]]) ~ "1",
            age_lower_bound_approx >= 65 & .data[[econ_activity_raw_col]] == "Does not apply" & grepl("L4,|L5,|L6", .data[[ns_sec_raw_col]]) ~ "2",
            age_lower_bound_approx >= 65 & .data[[econ_activity_raw_col]] == "Does not apply" & grepl("L7", .data[[ns_sec_raw_col]]) ~ "3",
            age_lower_bound_approx >= 65 & .data[[econ_activity_raw_col]] == "Does not apply" & grepl("L8,|L9", .data[[ns_sec_raw_col]]) ~ "4",
            age_lower_bound_approx >= 65 & .data[[econ_activity_raw_col]] == "Does not apply" & grepl("L10,|L11", .data[[ns_sec_raw_col]]) ~ "5",
            age_lower_bound_approx >= 65 & .data[[econ_activity_raw_col]] == "Does not apply" & grepl("L12", .data[[ns_sec_raw_col]]) ~ "6",
            age_lower_bound_approx >= 65 & .data[[econ_activity_raw_col]] == "Does not apply" & grepl("L13", .data[[ns_sec_raw_col]]) ~ "7",
            age_lower_bound_approx >= 65 & .data[[econ_activity_raw_col]] == "Does not apply" & grepl("L14", .data[[ns_sec_raw_col]]) ~ "Unemployed", # L14 for "Does not apply" econ activity
            age_lower_bound_approx >= 65 ~ "Retired", # Broad default for 65+
            grepl("L1,|L2,|L3", .data[[ns_sec_raw_col]]) & age_lower_bound_approx >= 17 & age_lower_bound_approx < 65 ~ "1",
            grepl("L4,|L5,|L6", .data[[ns_sec_raw_col]]) & age_lower_bound_approx >= 17 & age_lower_bound_approx < 65 ~ "2",
            grepl("L7", .data[[ns_sec_raw_col]]) & age_lower_bound_approx >= 17 & age_lower_bound_approx < 65 ~ "3",
            grepl("L8,|L9", .data[[ns_sec_raw_col]]) & age_lower_bound_approx >= 17 & age_lower_bound_approx < 65 ~ "4",
            grepl("L10,|L11", .data[[ns_sec_raw_col]]) & age_lower_bound_approx >= 17 & age_lower_bound_approx < 65 ~ "5",
            grepl("L12", .data[[ns_sec_raw_col]]) & age_lower_bound_approx >= 17 & age_lower_bound_approx < 65 ~ "6",
            grepl("L13", .data[[ns_sec_raw_col]]) & age_lower_bound_approx >= 17 & age_lower_bound_approx < 65 ~ "7",
            grepl("L14", .data[[ns_sec_raw_col]]) & age_lower_bound_approx >= 17 & age_lower_bound_approx < 65 ~ "Unemployed",
            grepl("L15", .data[[ns_sec_raw_col]]) & age_lower_bound_approx >= 17 & age_lower_bound_approx < 65 ~ "Student",
            .data[[ns_sec_raw_col]] == "Does not apply" & age_lower_bound_approx >= 17 & age_lower_bound_approx < 65 ~ "Unknown",
            TRUE ~ "Unknown"
        ), SES = factor(SES, levels = final_ses_levels))
}

# --- Helper function for SES derivation (NO AGE) ---
derive_ses_no_age <- function(df, ns_sec_raw_col) {
    # This simplified version assumes the data is for the working-age population
    # and doesn't handle "Under 17" or "Retired" which are age-dependent.
    df %>%
        mutate(SES = case_when(
            grepl("L1,|L2,|L3", .data[[ns_sec_raw_col]]) ~ "1",
            grepl("L4,|L5,|L6", .data[[ns_sec_raw_col]]) ~ "2",
            grepl("L7", .data[[ns_sec_raw_col]]) ~ "3",
            grepl("L8,|L9", .data[[ns_sec_raw_col]]) ~ "4",
            grepl("L10,|L11", .data[[ns_sec_raw_col]]) ~ "5",
            grepl("L12", .data[[ns_sec_raw_col]]) ~ "6",
            grepl("L13", .data[[ns_sec_raw_col]]) ~ "7",
            grepl("L14", .data[[ns_sec_raw_col]]) ~ "Unemployed",
            grepl("L15", .data[[ns_sec_raw_col]]) ~ "Student",
            .data[[ns_sec_raw_col]] == "Does not apply" ~ "Unknown",
            TRUE ~ "Unknown"
        ), SES = factor(SES, levels = final_ses_levels))
}

# --- Helper function for Ethnicity mapping (reusable) ---
map_ethnicity <- function(df, ethnic_group_raw_col) {
    df %>%
        mutate(ethnic_group = case_when(
            .data[[ethnic_group_raw_col]] == "White" ~ "White",
            .data[[ethnic_group_raw_col]] == "Asian, Asian British or Asian Welsh" ~ "Asian",
            .data[[ethnic_group_raw_col]] == "Black, Black British, Black Welsh, Caribbean or African" ~ "Black",
            .data[[ethnic_group_raw_col]] == "Mixed or Multiple ethnic groups" ~ "Mixed",
            .data[[ethnic_group_raw_col]] == "Other ethnic group" ~ "Other",
            TRUE ~ "Unknown"
        )) %>%
        # Combine Mixed and Other into Mixed/Other to match survey data processing
        mutate(ethnic_group = case_when(
            ethnic_group %in% c("Mixed", "Other") ~ "Mixed/Other",
            TRUE ~ ethnic_group
        )) %>%
        filter(ethnic_group != "Unknown") %>%
        mutate(ethnic_group = factor(ethnic_group, levels = final_eth_levels))
}

# --- Helper function for Sex mapping (reusable) ---
map_sex <- function(df, sex_raw_col) {
    df %>%
        mutate(sex = case_when(
            .data[[sex_raw_col]] == "Male" ~ "Male", # Assuming raw data uses these exact strings
            .data[[sex_raw_col]] == "Female" ~ "Female",
            TRUE ~ "Unknown"
        ), sex = factor(sex, levels = final_sex_levels))
}

# --- Function to check for missing columns ---
check_columns <- function(df, expected_cols, filepath) {
    missing_cols <- setdiff(expected_cols, colnames(df))
    if (length(missing_cols) > 0) {
        stop(paste0(
            "CRITICAL ERROR: The following expected columns are MISSING from the loaded census file '",
            filepath, "': ", paste(missing_cols, collapse = ", "),
            ". Please check the 'guessed_...' column name variables in the script for this file and update them."
        ))
    }
    message(paste("All expected columns for processing seem to be present in the loaded census file:", filepath))
}

# --- 1. Process Age x Ethnicity File ---
message(paste("\n--- Processing Age x Ethnicity File (from", CENSUS_FILE_AGE_ETH, ") ---"))
raw_data_ae <- read_excel(CENSUS_FILE_AGE_ETH, sheet = "Dataset")
guessed_age_col_ae <- "Age (B) (18 categories)"
guessed_eth_col_ae <- "Ethnic group (6 categories)"
guessed_pop_col_ae <- "Observation"
check_columns(raw_data_ae, c(guessed_age_col_ae, guessed_eth_col_ae, guessed_pop_col_ae), CENSUS_FILE_AGE_ETH)

processed_data_ae <- raw_data_ae %>%
    select(
        age_category_raw = all_of(guessed_age_col_ae),
        ethnic_group_raw = all_of(guessed_eth_col_ae),
        population_count = all_of(guessed_pop_col_ae)
    ) %>%
    mutate(region = "England and Wales") %>%
    map_age_categories_5y(age_category_raw_col_name = "age_category_raw") %>%
    map_ethnicity(ethnic_group_raw_col = "ethnic_group_raw") %>%
    group_by(age_category, region, ethnic_group) %>%
    summarise(population_count = sum(as.numeric(population_count), na.rm = TRUE), .groups = "drop")

qsave(processed_data_ae, OUTPUT_QS_AGE_ETH)
message(paste("Saved processed Age x Ethnicity data to:", OUTPUT_QS_AGE_ETH))

# --- 2. Process Age x SES File ---
message(paste("\n--- Processing Age x SES File (from", CENSUS_FILE_AGE_SES, ") ---"))
raw_data_as <- read_excel(CENSUS_FILE_AGE_SES, sheet = "Dataset")
guessed_age_col_as <- "Age (B) (18 categories)"
guessed_ns_sec_col_as <- "National Statistics Socio-economic Classification (NS-SeC) (10 categories)"
guessed_econ_activity_col_as <- "Economic activity status (4 categories)"
guessed_pop_col_as <- "Observation"
check_columns(raw_data_as, c(guessed_age_col_as, guessed_ns_sec_col_as, guessed_econ_activity_col_as, guessed_pop_col_as), CENSUS_FILE_AGE_SES)

processed_data_as <- raw_data_as %>%
    select(
        age_category_raw = all_of(guessed_age_col_as),
        ns_sec_raw = all_of(guessed_ns_sec_col_as),
        economic_activity_raw = all_of(guessed_econ_activity_col_as),
        population_count = all_of(guessed_pop_col_as)
    ) %>%
    mutate(region = "England and Wales") %>%
    map_age_categories_5y(age_category_raw_col_name = "age_category_raw") %>%
    derive_ses(ns_sec_raw_col = "ns_sec_raw", econ_activity_raw_col = "economic_activity_raw") %>%
    group_by(age_category, region, SES) %>%
    summarise(population_count = sum(as.numeric(population_count), na.rm = TRUE), .groups = "drop")

qsave(processed_data_as, OUTPUT_QS_AGE_SES)
message(paste("Saved processed Age x SES data to:", OUTPUT_QS_AGE_SES))

# --- 3. Process Age x Sex File ---
message(paste("\n--- Processing Age x Sex File (from", CENSUS_FILE_AGE_SEX, ") ---"))
raw_data_asx <- read_excel(CENSUS_FILE_AGE_SEX, sheet = "Dataset")
guessed_age_col_asx <- "Age (B) (18 categories)"
guessed_sex_col_asx <- "Sex (2 categories)"
guessed_pop_col_asx <- "Observation"
check_columns(raw_data_asx, c(guessed_age_col_asx, guessed_sex_col_asx, guessed_pop_col_asx), CENSUS_FILE_AGE_SEX)

processed_data_asx <- raw_data_asx %>%
    select(
        age_category_raw = all_of(guessed_age_col_asx),
        sex_raw = all_of(guessed_sex_col_asx),
        population_count = all_of(guessed_pop_col_asx)
    ) %>%
    mutate(region = "England and Wales") %>%
    map_age_categories_5y(age_category_raw_col_name = "age_category_raw") %>%
    map_sex(sex_raw_col = "sex_raw") %>%
    group_by(age_category, region, sex) %>%
    summarise(population_count = sum(as.numeric(population_count), na.rm = TRUE), .groups = "drop")

qsave(processed_data_asx, OUTPUT_QS_AGE_SEX)
message(paste("Saved processed Age x Sex data to:", OUTPUT_QS_AGE_SEX))

# --- 4. Process SES x Ethnicity File ---
message(paste("\n--- Processing SES x Ethnicity File (from", CENSUS_FILE_SES_ETH, ") ---"))
raw_data_se <- read_excel(CENSUS_FILE_SES_ETH, sheet = "Dataset")
guessed_eth_col_se <- "Ethnic group (6 categories)"
guessed_ns_sec_col_se <- "National Statistics Socio-economic Classification (NS-SeC) (10 categories)"
guessed_pop_col_se <- "Observation"
check_columns(raw_data_se, c(guessed_eth_col_se, guessed_ns_sec_col_se, guessed_pop_col_se), CENSUS_FILE_SES_ETH)

processed_data_se <- raw_data_se %>%
    select(
        ethnic_group_raw = all_of(guessed_eth_col_se),
        ns_sec_raw = all_of(guessed_ns_sec_col_se),
        population_count = all_of(guessed_pop_col_se)
    ) %>%
    mutate(region = "England and Wales") %>%
    map_ethnicity(ethnic_group_raw_col = "ethnic_group_raw") %>%
    derive_ses_no_age(ns_sec_raw_col = "ns_sec_raw") %>%
    group_by(region, ethnic_group, SES) %>%
    summarise(population_count = sum(as.numeric(population_count), na.rm = TRUE), .groups = "drop")

qsave(processed_data_se, OUTPUT_QS_SES_ETH)
message(paste("Saved processed SES x Ethnicity data to:", OUTPUT_QS_SES_ETH))

message("\n--- All census data processing complete. ---")
