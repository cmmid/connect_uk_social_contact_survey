library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(lubridate)
library(forcats)
library(readr)
library(purrr)
library(flextable)
library(here)
library(data.table)
library(readxl) 
library(writexl)
library(ggplot2)
library(stringr)
library(viridis)
library(tidyverse)  
library(sf)
library(boot)      
library(survey)     
library(patchwork)
library(survey)
library(MASS)
library(fitdistrplus)
library(socialmixr)
library(parallel)
library(haven)
library(ggarchery)
library(reshape2)

select <- dplyr::select
wday <- lubridate::wday
map <- purrr::map

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

