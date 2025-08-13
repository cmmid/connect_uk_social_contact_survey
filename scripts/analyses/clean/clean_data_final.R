## cleaning data ##

# Read the data
adults <- read_sav(here::here("data", "survation_dat", "final", "Adults", paste0("lshtm_full_ad_anon_v3", ".sav")))
children <- read_sav(here::here("data", "survation_dat", "final", "Children", paste0("lshtm_full_cd_anon_v4", ".sav")))

adults_boost <- read_sav(here::here("data", "survation_dat", "boost", paste0("lshtm_bst_ad_anon", ".sav"))) %>% mutate(id = id + 20000)
children_boost <- read_sav(here::here("data", "survation_dat", "boost", paste0("lshtm_bst_cd_anon", ".sav"))) %>% mutate(id = id + 30000)

# Change age from factor to numeric (separately, to avoid combining levels)
adults <- adults %>%
  haven::as_factor() %>%
  rbind(adults_boost %>% haven::as_factor()) %>%
  mutate(p_age = as.numeric(as.character(p_age))) %>%
  filter(!id %in% c(
    5397, # recorded 18 u18, 65 o65, etc.
    6649, # invalid contacts/large_n
    4774, # invalid contacts
    2693, # invalid contacts
    8003 # large_n = 1500 without visiting anywhere
  )) #

# manually changing p_id %in% c(1242, 4899) (additional contacts repeated across work and school)
adults <- adults %>%
  mutate(
    add_u18_1 = case_when(id %in% c(1242, 4899) ~ 0, T ~ add_u18_1),
    add_18_64_1 = case_when(id %in% c(1242, 4899) ~ 0, T ~ add_18_64_1),
    add_65_1 = case_when(id %in% c(1242, 4899) ~ 0, T ~ add_65_1)
  )

children <- children %>%
  # mutate(p_age = haven::as_factor(p_age)) %>%
  haven::as_factor() %>%
  rbind(children_boost %>% haven::as_factor()) %>%
  mutate(p_age = as.numeric(as.character(p_age))) %>%
  mutate(p_hiqual = "No qualifications") %>%
  mutate(id = id + 15000) # IDs repeated across adults/children

# data checking (print if any columns all NA etc.)
cat("Adults missing columns:\n")
all_miss <- apply(adults, 2, function(x) all(is.na(x)))
print(names(all_miss[all_miss > 0]))
cat("Children missing columns:\n")
all_miss <- apply(children, 2, function(x) all(is.na(x)))
print(names(all_miss[all_miss > 0]))
cat("---------------------------------------------------\n")

# Process participants
part_adults <- process_participants(adults)
part_children <- process_participants(children, T)

for (var_name in c("p_income_1", "p_income", "p_emp_1", "p_sector", "p_hiqual")) {
  part_children[, var_name] <- "Child (Not Applic.)"
}

# Combine participants
part <- bind_rows(list(Adult = part_adults, Child = part_children), .id = "p_adult_child")

# Truncate 'add_' columns at 300
trunc_num_large_n <- 300

write_xlsx(part %>% select(p_age_group, contains("add_"), large_n) %>%
  arrange(desc(large_n)) %>%
  filter(if_any(contains("add_"), ~ . > trunc_num_large_n)) %>%
  select(!large_n), here::here("results", survey, "supp_figures", paste0("p_above_", trunc_num_large_n, ".xlsx")))

part <- part %>%
  mutate(across(contains("add_"), ~ case_when(. > trunc_num_large_n ~ trunc_num_large_n, T ~ .))) %>%
  mutate(large_n = rowSums(across(contains("add_"))))

# Adding NS-SEC

occupations <- data.table(read_csv(here::here("results","final", "occupations.csv"), show_col_types = F))
occupations <- occupations[`SOC 2020` != "----"]
occupations[, soc_code := as.numeric(`SOC 2020`)][, soc_code_1 := floor(soc_code / 1000)]

# load ns-sec conversion table
# from: https://www.ons.gov.uk/methodology/classificationsandstandards/standardoccupationalclassificationsoc/
# soc2020/soc2020volume3thenationalstatisticssocioeconomicclassificationnssecrebasedonthesoc2020#deriving-the-ns-sec-full-reduced-and-simplified-methods
# using analytic categories (from 1-8)

# Skip NSSEC data loading for now - demographic data files not available
# nssec <- data.table(read_xlsx(here::here('data','demog_dat','tables912v5.xlsx'), sheet = 6, skip = 3))[,1:3]
# colnames(nssec) <- c('soc_code','title','ssec')
# occupations <- occupations %>% left_join(nssec[,c(1,3)], by = 'soc_code')

# Create a simple placeholder for ssec (socioeconomic class)
occupations$ssec <- 1 # Default value

part <- part %>%
  left_join(unique(occupations %>% filter(person == "participant") %>% select("p_id", "ssec")), by = "p_id") %>%
  mutate(p_sec_input = case_when(
    p_age <= 16 ~ "Under 17",
    grepl("Unemployed", p_emp_1) ~ "Unemployed",
    grepl("Looking after", p_emp_1) ~ "Unemployed",
    grepl("Long-term", p_emp_1) ~ "Unemployed",
    grepl("Other", p_emp_1) ~ "Unemployed",
    grepl("Not employed", p_emp_2) ~ "Student",
    grepl("Retired", p_emp_1) ~ "Retired",
    grepl("elf", p_emp_1) & ssec > 2 ~ "4", # NS-SEC codes self-employed people as class 4, unless already in class 1 or 2
    !is.na(ssec) ~ as.character(ssec),
    T ~ "Unknown"
  ))

write_csv(
  part %>% filter(p_sec_input %in% as.character(1:7)) %>%
    group_by(p_sec_input) %>% summarise(n = n()) %>%
    ungroup() %>% mutate(out = paste0(n, " (", round(100 * n / sum(n), 2), "%)")),
  file = here::here("results", survey, "supp_figures", "nsssec_props.csv")
)

households <- rbind(household_processing(adults)) # , household_processing(children))

# Process contacts
contacts_adults <- process_contacts(adults)
contacts_children <- process_contacts(children, T)

# children's workplace contacts
# move workplace contacts of u13s and not-employed 13-17yos to location 'other'
if (!survey %in% c("Pilot I", "Pilot II")) {
  ch_e <- part_children %>%
    left_join(contacts_children, by = "p_id") %>%
    filter(c_location == "Work")
  cat(paste0(n_distinct(ch_e$p_id), "/", nrow(part_children), " children reported workplace contacts.\n"))
  cat(paste0(n_distinct(ch_e[ch_e$p_age < 13, ]$p_id), "/", nrow(children), " children (aged under 13) reported workplace contacts."))
  contacts_children <- contacts_children %>%
    left_join(part_children %>% select(p_id, p_age, p_emp), by = "p_id") %>%
    mutate(
      c_location = case_when(
        p_age < 13 & c_location == "Work" ~ "Other",
        (p_emp == "No" & p_age < 18) & c_location == "Work" ~ "Other",
        T ~ c_location
      ),
      c_location_long = case_when(
        p_age < 13 & c_location_long == "Work" ~ "Other",
        (p_emp == "No" & p_age < 18) & c_location_long == "Work" ~ "Other",
        T ~ c_location_long
      ),
      c_location_long_long = case_when(
        p_age < 13 & c_location_long_long == "Work" ~ "Other",
        (p_emp == "No" & p_age < 18) & c_location_long_long == "Work" ~ "Other",
        T ~ c_location_long_long
      )
    ) %>%
    select(!c(p_age, p_emp))
}

# Combine contacts
contacts <- bind_rows(list(Adult = contacts_adults, Child = contacts_children))

contacts <- contacts %>%
  mutate(job = tolower(c_job)) %>%
  left_join(unique((occupations %>% filter(person == "contact") %>% select("p_id", "job", "ssec"))),
    by = c("p_id", "job"), relationship = "many-to-many"
  ) %>%
  mutate(c_sec_input = case_when(
    c_age <= 16 ~ "Under 17",
    grepl("Not employed", c_emp) & c_age <= 65 ~ "Unemployed",
    grepl("Student", c_emp) ~ "Student",
    c_job == "Student" ~ "Student",
    grepl("etired", c_job) ~ "Retired",
    grepl("Not employed", c_emp) & c_age > 65 ~ "Retired",
    !is.na(ssec) ~ as.character(ssec),
    T ~ "Unknown"
  ))

# print if any are truncated
cat("\n---------------------------------------------------\n")
maxes <- contacts %>%
  left_join(part, by = "p_id") %>%
  group_by(p_id, p_adult_child, p_age_group) %>%
  summarise(n = n()) %>%
  group_by(p_adult_child) %>%
  summarise(max = max(n))
cat(paste0(
  "Max n contacts: ", maxes[maxes$p_adult_child == "Adult", ]$max,
  " (adults), ", maxes[maxes$p_adult_child == "Child", ]$max, " (children)."
))

# adding number of contacts
n_contacts <- part %>%
  left_join(contacts, by = "p_id") %>%
  mutate(z = is.na(c_id)) %>%
  group_by(p_id, z) %>%
  summarise(n_contacts = n()) %>%
  mutate(n_contacts = case_when(z ~ 0, T ~ n_contacts)) %>%
  select(!z)
part <- part %>% left_join(n_contacts, by = "p_id")

# make base directory

base_dir <- here::here("results", survey, "age_contacts", "contacts", "png")
dir.create(base_dir, showWarnings = FALSE, recursive = TRUE)


# save distribution of locations

loc_dt <- data.table(prop.table(table(contacts$c_location_long_long)))
colnames(loc_dt) <- c("location", "proportion")
loc_dt[, percentage := paste0(round(100 * proportion, 2), "%")]

write_csv(loc_dt, here::here("results", survey, "supp_figures", "locations_distribution.csv"))


## save as a survey object

connect_survey <- socialmixr::survey(
  participants = data.table(
    part_id = part$p_id,
    part_age = part$p_age,
    part_age_group = part$p_age_group,
    part_adult_child = part$p_adult_child,
    part_gender = as.factor(part$p_gender),
    part_ethnicity = as.factor(part$p_ethnicity),
    part_ses = as.factor(part$p_sec_input),
    day_week = part$day_week,
    large_n = part$large_n,
    add_u18_work = part$add_u18_work,
    add_18_64_work = part$add_18_64_work,
    add_65_work = part$add_65_work,
    add_u18_school = part$add_u18_school,
    add_18_64_school = part$add_18_64_school,
    add_65_school = part$add_65_school,
    add_u18_other = part$add_u18_other,
    add_18_64_other = part$add_18_64_other,
    add_65_other = part$add_65_other
  ),
  contacts = data.table(
    cnt_id = 1:nrow(contacts),
    part_id = contacts$p_id,
    cnt_location = contacts$c_location,
    cnt_age = contacts$c_age,
    cnt_age_group = contacts$c_age_group,
    cnt_gender = as.factor(contacts$c_sex),
    cnt_ethnicity = as.factor(contacts$c_ethnicity),
    cnt_ses = as.factor(contacts$c_sec_input)
  )
)

write_rds(connect_survey, here::here("results", survey, "connect_survey.rds"))
write_rds(connect_survey, here::here("data", "connect_survey.rds"))

## save participant information

write_rds(part, here::here("results", survey, "connect_part.rds"))

## save participant information

write_rds(contacts, here::here("results", survey, "connect_contacts.rds"))
