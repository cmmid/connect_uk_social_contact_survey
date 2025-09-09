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
reconnect_participant_common <- read_csv(here::here('data','zenodo','reconnect_participant_common.csv'), show_col_types = F)
reconnect_participant_extra <- read_csv(here::here('data','zenodo','reconnect_participant_extra.csv'), show_col_types = F)
reconnect_participant_sday <- read_csv(here::here('data','zenodo','reconnect_sday.csv'), show_col_types = F)
reconnect_participant <- left_join(reconnect_participant_common, 
                                   reconnect_participant_extra, 
                                   by = 'part_id') %>% left_join(reconnect_participant_sday, by = 'part_id')

reconnect_contact_common <- read_csv(here::here('data','zenodo','reconnect_contact_common.csv'), show_col_types = F)
reconnect_contact_extra <- read_csv(here::here('data','zenodo','reconnect_contact_extra.csv'), show_col_types = F)
reconnect_contact <- left_join(reconnect_contact_common, 
                               reconnect_contact_extra, 
                                   by = c('cnt_id','part_id'))

# load age weights for large_n
polymod_wts <- polymod_weights()

# negative binomial contact matrices ##
source(here::here('scripts','analyses','negative_binom','contact_matrix_nb.R'))

## NGM analyses ##
source(here::here('scripts','ngm_analysis','run_full_ngm_analysis.R'))

