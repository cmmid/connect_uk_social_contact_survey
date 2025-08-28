#### run contact matrices analyses ####

# censor any categories of participant with less than x:
censor_low_val <- 5
# right-censor total contact counts at:
max_n_contacts <- 10000 # deliberately redundant
 
# choose age groups for analysis
age_breaks <- c(-Inf, 5*1:(75/5), Inf)
age_vals <- age_breaks[is.finite(age_breaks)]
age_labels <- c(paste0(c(0, age_vals[1:length(age_vals)-1]), '-', c(age_vals-1)), paste0(age_vals[length(age_vals)], '+'))

library(here)

## install appropriate packages ##
source(here::here('scripts','analyses','install_packages.R'))
source(here::here('scripts','analyses','colors.R'))

## source analysis functions ##
source(here::here('scripts','analyses','functions.R'))
## negative binomial functions ##
source(here::here('scripts','analyses','negative_binom','negative_binomial_fcns.R'))

## load UK gender-specific age structure etc. ##
source(here::here('scripts','analyses','age_structure.R'))

## load data ##
reconnect_survey <- readRDS(here::here('data','reconnect_survey.rds'))

# load age weights for large_n
polymod_wts <- polymod_weights()

# negative binomial contact matrices ##
source(here::here('scripts','analyses','negative_binom','contact_matrix_nb.R'))

## negative binomial contact matrices (gender-specific) ##
source(here::here('scripts','analyses','negative_binom','contact_matrix_nb_gender.R'))
