# PSborrowing - Demo

```r
# install.packages("remotes") 
# install.packages("tidyverse") 
if (!requireNamespace("PSborrowing", quietly = TRUE)) {
    remotes::install_github("MinLinSTAT/PSborrowing")
}

# load packages
library(tidyverse)
library(PSborrowing)

set.seed(2025)

glimpse(dat1$trial_data)
glimpse(dat1$external_data)

combined <- combine_trials(
    current_trial = dat1$trial_data,
    external_trial = dat1$external_data,
    response = Y,
    covariates = x1:x3,
    group = group,
    group_levels = c("Treatment", "Control", "External") 
)

combined

combined_with_ps <- combined %>% 
    estimate_ps(code ~ x1 + x2 + x3)

combined_with_ps

combined_stratified <- combined_with_ps %>% 
    ps_stratify()

combined_stratified

res <- combined_stratified %>% 
    pssBPP()

res

# use shiny
run_shiny_app()
