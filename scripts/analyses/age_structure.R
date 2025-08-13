
# Read in ONS age structure data
ons_data <- readxl::read_xlsx(here::here("data","age_structure_dat","ons_2022_age_structure.xlsx"), 
skip = 5) %>% 
    filter(`Area name` == "UNITED KINGDOM") %>% 
    #Select columns age and population contains 2022
    select("age" = Age, contains("2022")) %>% 
    # Sum across Female and Male columns
    mutate(pop = `Mid-2022 population (Female)` + `Mid-2022 population (Male)`) %>% 
    select(age, pop)

# Define age groups for analysis
# Modify these as needed for your specific analysis

# Calculate sum and proportion for each age group
age_structure_coarse <- ons_data %>%
  mutate(p_age_group = cut(age, 
                           right = F,
                           breaks = c(-Inf, 5, 12, 18, 30, 40, 50, 60, 70, Inf),
                           labels = c("0-4", "5-11", "12-17", "18-29", "30-39", "40-49", "50-59", "60-69", "70+"))) %>% 
  group_by(p_age_group) %>%
  summarise(
    n = sum(pop),
    proportion = n / sum(ons_data$pop))

age_structure_fine <- ons_data %>%
  mutate(p_age_group = cut(age,
                           right = F,
                           breaks = age_breaks,
                           labels = age_labels)) %>% 
  group_by(p_age_group) %>%
  summarise(
    n = sum(pop),
    proportion = n / sum(ons_data$pop))

age_structure_adult_child <- 
  readxl::read_xlsx(here::here("data","age_structure_dat","ons_2022_age_structure.xlsx"), 
                    skip = 5) %>% 
  filter(`Area name` == "UNITED KINGDOM") %>% 
  select("age" = Age, contains("2022")) %>% 
  rename(female = `Mid-2022 population (Female)`, male = `Mid-2022 population (Male)`) %>% 
  pivot_longer(cols = c("female", "male"), names_to = "p_gender", values_to = "pop") %>% 
  mutate(p_age_group = cut(age,
                           right = F,
                           # from 0 to 75+ by 5 year age groups
                           breaks = age_breaks,
                           labels = age_labels)) %>% 
  mutate(p_adult_child = case_when(age < 18 ~ 'Child',
                                   T ~ 'Adult')) %>% 
  group_by(p_age_group, p_gender, p_adult_child) %>%
  summarise(n = sum(pop)) %>% 
  group_by(p_adult_child) %>% mutate(n_ac = sum(n)) %>% 
  ungroup() %>% mutate(proportion = n/n_ac) %>% 
  select(!n_ac)

ggplot(age_structure_coarse, aes(x = p_age_group, y = n/1000000)) +
  geom_bar(stat = "identity", fill = 'orchid3') +
  labs(title = "Age structure - Coarse age groups",
       x = "Age group",
       y = "Population (millions)") +
  scale_y_continuous(breaks=seq(0,10,2)) +
  theme_minimal() + theme(text=element_text(size=14))

age_sex_strata <- # Read in ONS age structure data
  readxl::read_xlsx(here::here("data","age_structure_dat","ons_2022_age_structure.xlsx"), 
                                skip = 5) %>% 
  filter(`Area name` == "UNITED KINGDOM") %>% 
  #Select columns age and population contains 2022
  select("age" = Age, contains("2022")) %>% 
  rename(female = `Mid-2022 population (Female)`, male = `Mid-2022 population (Male)`) %>% 
  pivot_longer(cols = c("female", "male"), names_to = "p_gender", values_to = "pop") %>% 
  mutate(p_age_group = cut(age,
                           right = F,
                           # from 0 to 75+ by 5 year age groups
                           breaks = age_breaks,
                           labels = age_labels)) %>% 
  group_by(p_age_group, p_gender) %>% 
  summarise(
    n = sum(pop),
    proportion = n / sum(ons_data$pop)) %>% ungroup() 

age_sex_strata %>% 
  mutate(n = ifelse(p_gender=="male", -n, n)) %>% 
  ggplot(aes(x = p_age_group, y = n/1000000, fill=p_gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Age structure",
       x = "Age group",
       y = "Population (millions)") +
  scale_y_continuous(breaks = (-3):3, labels=c(3,2,1,0,1,2,3)) + 
  coord_flip() + scale_fill_brewer(palette = "Set1") +
  labs(fill="Sex") + 
  theme_minimal() + theme(text=element_text(size=14))

# from census 2021 (https://www.ons.gov.uk/peoplepopulationandcommunity/
# culturalidentity/ethnicity/bulletins/ethnicgroupenglandandwales/census2021)
ons_ethnicity <- data.table(
  p_ethnicity = c('Asian','Black','Mixed','White','Other'),
  proportion = c(9.3,
                     4.0,
                     2.9,
                     81.7,
                     2.1)/100)


# from census 2021 (https://www.ons.gov.uk/peoplepopulationandcommunity/culturalidentity
# /ethnicity/datasets/ethnicgroupbyageandsexinenglandandwales)
eth_age_sex <- suppressWarnings(data.table(read_xlsx(here::here('data','age_structure_dat','ethnicgroupagesex11.xlsx'), sheet = 6, skip = 3)))
eth_age_sex <- eth_age_sex[get(colnames(eth_age_sex)[1])=='K04000001',]

eth_age_sex[grepl('100 or over', Age), Age := 100]

c_to_0 <- function(v){if(length(v[v=='c']) > 0){v[v=='c'] <- 0; v}else{v}}
eth_age_sex <- eth_age_sex[, lapply(.SD, c_to_0)]

eth_age_sex[, 3:ncol(eth_age_sex)] <- lapply(eth_age_sex[, 3:ncol(eth_age_sex)], as.numeric)

for(ethn in c('Asian','Black','Mixed','White','Other')){
  for(sex in c('Female','Male')){
    vec <- (substr(colnames(eth_age_sex),1,5) == ethn) & grepl(sex,colnames(eth_age_sex))
    eth_age_sex$next_col <- rowSums(eth_age_sex[, ..vec])
    colnames(eth_age_sex)[length(colnames(eth_age_sex))] <- paste0(ethn,'_',sex)
   }
}

eth_age_sex <- eth_age_sex %>% select('Age',contains('_')) %>% mutate('p_age_group' = cut(Age,
                                                                                          breaks = age_breaks,
                                                                                          labels = age_labels,
                                                                                          right = F)) %>% 
  mutate('p_adult_child' = cut(Age, breaks = c(-Inf, 18, Inf), labels = c('Child','Adult'), right = F)) %>% 
  select(!Age) %>% pivot_longer(!c(p_age_group, p_adult_child)) %>% 
  separate_wider_delim(name, delim = "_", names = c("p_ethnicity", "p_gender")) %>% 
  group_by(p_age_group, p_adult_child, p_ethnicity, p_gender) %>% summarise(value = sum(value)) %>% ungroup() %>% 
  mutate(proportion = value/sum(value)) %>% complete(p_adult_child, p_age_group, p_ethnicity, p_gender,
                                                     fill = list(value = 0, proportion = 0))

# household size
# from https://www.ons.gov.uk/datasets/TS017/editions/2021/versions/3

household_ons <- data.frame(read_csv(here::here('data','age_structure_dat','household_size.csv'),show_col_types=F)[,c(3:5)])
colnames(household_ons) <- c('size','name','occs')
household_ons <- household_ons %>% mutate(pop = size*occs) %>%
  mutate(proportion = pop/sum(pop))


# population in each country
# from:
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/
#   populationestimates/bulletins/annualmidyearpopulationestimates/mid2023

country_pops <- data.table(
  p_country = c('England','Wales','Scotland','Northern Ireland'),
  pop = c(57690300, 3164400, 5490100, 1920400))
country_pops[, proportion := pop/sum(pop)]


# highest level of qualification
# from: https://www.ons.gov.uk/filters/0ab4f1f5-7194-477a-90c4-3f6168e0f7a8/dimensions

high_qual_ons <- data.frame(read_csv(here::here('data','age_structure_dat','high_qual_census.csv'),show_col_types=F)[,c(3:5)])
colnames(high_qual_ons) <- c('qual','name','occs')
high_qual_ons <- high_qual_ons %>% mutate(name = c('NA',
                                                   'No qualifications',
                                                   'Level 1 (1-4 GCSEs, O-levels (any), NVQ level 1, etc.)',
                                                   'Level 2 (5+ GCSEs, O-levels (passes), NVQ level 2, etc.)',
                                                   'Apprenticeship','Level 3 (A-level, BTEC, NVQ level 3, etc.)',
                                                   'Level 4+ (University degree and above)','Other'))

high_qual_ons <- high_qual_ons %>% 
  mutate(proportion = occs/sum(occs))  


# employment status
# data from: 
# https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/datasets/employmentinenglandandwales

emp_pops <- data.table(
  p_emp_1 = c("Self-employed part time", "Looking after home or family", "Retired",                                    
              "Employed full-time (35+ hours per week)", "Unemployed (currently looking for work)", "Employed part-time",                         
              "Student", "Self-employed full time", "Long-term sick or disabled",                  
              "Other", "Unemployed (not currently looking for work)", "Child (Not Applic.)"),
  pop = c(0, 2317340, 10513715, 23107385,  1675185, 0,  2740635, 4666280,  2025620, 0, 
          1520215, 0))

emp_pops <- emp_pops %>% 
  mutate(proportion = pop/59600000) %>% filter(pop!=0) 


#### URBAN/RURAL ASSIGNMENTS ####

urban_rural_assignments <- data.table(read_csv(here::here('data','age_structure_dat','urban_rural_assignments.csv'), show_col_types=F))


### NS-SEC CLASSES ###

nssec_census <- data.table(read_csv(here::here('data','age_structure_dat','nssec_census.csv'), show_col_types=F))

nssec_census <- nssec_census[, c('National Statistics Socio-economic Classification (NS-SeC) (10 categories) Code',
                                 'employed_prop')]

colnames(nssec_census) <- c('nssec_class', 'proportion')






