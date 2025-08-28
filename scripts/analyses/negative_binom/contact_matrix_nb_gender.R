
## reformatting survey data ##

part <- reconnect_survey$participants %>% 
  rename(p_id = part_id,
         p_age_group = part_age_group,
         p_adult_child = part_adult_child,
         p_gender = part_gender,
         p_ethnicity = part_ethnicity,
         p_sec_input = part_ses)

contacts <- reconnect_survey$contacts %>% 
  rename(c_id = cnt_id,
         p_id = part_id,
         c_location = cnt_location,
         c_age_group = cnt_age_group,
         c_sex = cnt_gender,
         c_ethnicity = cnt_ethnicity,
         c_sec_input = cnt_ses)

# if results folder doesn't exist, create
if(!file.exists('results')){dir.create('results')}
# if gender folder doesn't exist, create
if(!file.exists('results/gender_contacts')){dir.create('results/gender_contacts')}

## Gender-gender plots (home location only)

plot_gender_matrix <- function(i){
  
  if(i %notin% 1:4){stop('i needs to be in 1:4')}
  
  p_gender_i <- if(i %in% 1:2){'Female'}else{'Male'}
  c_sex_i <- if(i %in% c(1,3)){'Female'}else{'Male'}
  
  cat('\n', p_gender_i, ' to ', c_sex_i, ' contacts\n', sep = '')
  
  gender_contacts <- nb_matrix_fit(
    participant_data = part %>% filter(p_gender == p_gender_i),
    contact_data = contacts %>% filter(c_sex == c_sex_i),
    participant_var = "p_age_group",
    contact_var = "c_age_group",
    n_bootstrap = 1000,
    trunc = max_n_contacts,
    polymod_weighting = polymod_wts,
    large_n_input = F,
    weighting_vec = c('p_ethnicity','day_week'),
    locations = c("Home"),
    save = F
  )
  
  saveRDS(gender_contacts, here::here('results',paste0('nb_',tolower(p_gender_i),'_to_',tolower(c_sex_i),'.rds'))) 
  # gender_contacts <- readRDS(here::here('results',paste0('nb_',tolower(p_gender_i),'_to_',tolower(c_sex_i),'.rds'))) 
  
  gender_matrices <- map(.x = gender_contacts, 
                         .f = ~plot_mu_matrix(.x,
                                              'p_age_group', 'c_age_group',
                                              variable = 'mu', 
                                              uncertainty = F))
  
  gender_matrices <- gender_matrices[[1]] + ggtitle(paste0(p_gender_i, '-', c_sex_i, ', ', 'Home'))
  
  gender_matrices
  
}

matrix_plots_tot <- mclapply(1:4, plot_gender_matrix, mc.cores = 4)

patch <- patchwork::wrap_plots(matrix_plots_tot, ncol = 2)
  
ggsave(
    filename = file.path('results', 'gender_contacts', paste0('Home', "_combined.png")),
    plot = patch,
    width = 18,
    height = 18,
    dpi = 600,
    bg = "white"
  )
  

