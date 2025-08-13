
## COMBINE MEAN CONTACT SUPPLEMENT TABLES INTO ONE ##

# ### Define analyses to be performed
analysis_specs_main_analyses <- list(
  list(Variable = "Total contacts", variable_name = NULL, is_participant_attribute = TRUE, weights = c('p_age_group','p_gender','p_ethnicity','day_week')),
  list(Variable = "Participant adult/child", variable_name = "p_adult_child", is_participant_attribute = TRUE, weights = c('p_age_group','p_gender','p_ethnicity','day_week')),
  list(Variable = "Participant age group", variable_name = "p_age_group", is_participant_attribute = TRUE, weights = c('p_gender','p_ethnicity','day_week')),
  list(Variable = "Participant's gender", variable_name = "p_gender", is_participant_attribute = TRUE, weights = c('p_age_group','p_ethnicity','day_week')),
  list(Variable = "Participant's ethnicity", variable_name = "p_ethnicity", is_participant_attribute = TRUE, weights = c('p_age_group','p_gender','day_week')),
  list(Variable = "Participant's employment status (aged 18+)", variable_name = "p_emp_1", is_participant_attribute = TRUE, weights = c('p_gender','p_ethnicity','day_week')),
  list(Variable = "Participant's highest qualification (aged 18+)", variable_name = "p_hiqual", is_participant_attribute = TRUE, weights = c('p_age_group','p_gender','p_ethnicity','day_week')),
  list(Variable = "Participant's country", variable_name = "p_country", is_participant_attribute = TRUE, weights = c('p_age_group','p_gender','day_week')),
  list(Variable = "Participant's housing status (aged 18+)", variable_name = "p_tenure", is_participant_attribute = TRUE, weights = c('p_age_group','p_gender','p_ethnicity','day_week')),
  list(Variable = "Participant's NS-SEC class", variable_name = "p_sec_input", is_participant_attribute = TRUE, weights = c('day_week')),
  list(Variable = "Urban/Rural", variable_name = "p_urban_rural", is_participant_attribute = TRUE, weights = c('p_age_group','p_gender','p_ethnicity','day_week')),
  list(Variable = "Day of week", variable_name = "c_day_of_week", is_participant_attribute = TRUE, weights = c('p_age_group','p_gender','p_ethnicity')),
  list(Variable = "Participant's household size", variable_name = "household_members_max8", is_participant_attribute = TRUE, weights = c('p_age_group','p_gender','p_ethnicity','day_week')),
  list(Variable = "Participant's household income (aged 18+)", variable_name = "p_income", is_participant_attribute = TRUE, weights = c('p_age_group','p_gender','p_ethnicity','day_week'))
  # list(Variable = "Contact location", variable_name = "c_location", is_participant_attribute = FALSE, weights = c('p_age_group','p_gender','p_ethnicity','day_week'))
)

# read data

table_nb <- data.table(read_xlsx(here::here("results",survey,'neg_bin',"mean_contacts_table_nb.xlsx")))

table_nb_100 <- data.table(read_xlsx(here::here("results",survey,'neg_bin',"mean_contacts_table_nb_100.xlsx")))

# change column names
# colnames(table_bs) <- c('Variable','Category','Mean weighted bootstrap estimate (95% UI)')
colnames(table_nb) <- c('Variable','Category','Mean weighted negative binomial regression estimate (95% UI)')
colnames(table_nb_100) <- c('Variable','Category','Mean weighted negative binomial regression estimate with right-truncation at 100 (95% UI)')

# merge data

# table_nb <- table_nb[table_bs, on = c('Variable','Category')]
table_nb <- table_nb[table_nb_100, on = c('Variable','Category')]

analysis_levels <- unique(suppressWarnings(rbindlist(analysis_specs_main_analyses))$Variable)

table_nb <- table_nb %>% filter(! Category == "Child (Not Applic.)")

levels_categories <- c('Total','Adult','Child',age_labels,'Female','Male','White','Asian','Black','Mixed','Prefer not to say',
                       'Employed full-time (35+ hours per week)','Employed part-time','Self-employed full time','Self-employed part time','Long-term sick or disabled','Looking after home or family',
                       'Retired','Student','Unemployed (currently looking for work)',
                       'Unemployed (not currently looking for work)','Apprenticeship','Level 1 (1-4 GCSEs, O-levels (any), NVQ level 1, etc.)',
                       'Level 2 (5+ GCSEs, O-levels (passes), NVQ level 2, etc.)','Level 3 (A-level, BTEC, NVQ level 3, etc.)','Level 4+ (University degree and above)',
                       'No qualifications','England','Northern Ireland','Scotland','Wales','Living rent free','Other private rented','Owned outright',
                       'Owned with a mortgage or loan','Rented from Council (Local Authority)','Rented from a relative or friend of household member',
                       'Rented from housing association, housing co-operative, charitable trust, or registered social landlord','Rented from private landlord or letting agency',
                       'Shared ownership - with a mortgage and paying rent','Tied accommodation (accommodation provided by employer of a household member)',
                       '1 (Higher managerial, administrative and professional occupations)','2 (Lower managerial, administrative and professional occupations)',
                       '3 (Intermediate occupations)','4 (Small employers and own account workers)','5 (Lower supervisory and technical occupations)',
                       '6 (Semi-routine occupations)','7 (Routine occupations)','Rural','Urban',
                       'Monday','Tuesday','Thursday','Wednesday','Friday','Saturday','Sunday',
                       '1','2','3','4','5','6','7','8+',
                       'Less than £20,000','£20,000 - £39,999','£40,000 - £59,999','£60,000 - £100,000','Over £100,000','Other')

table_nb$Category <- factor(table_nb$Category,
                                levels = levels_categories)

table_nb$Variable <- factor(table_nb$Variable, levels = analysis_levels)

table_nb <- table_nb %>% arrange(Variable, Category)

# save data

write_xlsx(table_nb, here::here('results',survey,'merged_mean_contact_tables.xlsx'))



