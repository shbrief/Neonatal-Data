Harmonize Neonatal-Related Attributes in cMD Metadata
================
Britney Pheng, Sehyun Oh <br>
February 20, 2024

#### Load Packages

``` r
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

suppressPackageStartupMessages({
  library(curatedMetagenomicData)
  library(dplyr)
})
```

#### Create a list of selected sampleMetadata columns to work with

``` r
selected_cols <- c("age_category",
                   "infant_age",
                   "born_method", 
                   "gestational_age",
                   "premature",
                   "birth_weight",
                   "lactating",
                   "c_section_type",
                   "feeding_practice",
                   "formula_first_day",
                   "breastfeeding_duration")
```

#### Create a new dataframe *nn.ds* with a new column for unique *study:sample_id* rows and the selected sampleMetadata columns

``` r
nn.ds <- sampleMetadata %>%
    mutate(curation_id = paste(study_name, sample_id, sep = ":")) %>%
    dplyr::select(curation_id, selected_cols)
```

### Curated Neonatal Columns

#### Clean up *born_method* column into curated *neonatal_delivery_procedure* column

Use **c_section_type** data element to expand **born_method** column
answers. C-section can be either an Emergency CS, Elective CS, or kept
broadly as “C-section” if there is no expanded answer in the
corresponding **c_section_type** column.

``` r
nn.ds$neonatal_delivery_procedure <- NA

for (i in 1:nrow(nn.ds)) {
  if (is.na(nn.ds$born_method[i])) {
    nn.ds$neonatal_delivery_procedure[i] <- NA
  } else {
    if (nn.ds$born_method[i] == 'c_section' & !is.na(nn.ds$c_section_type[i])) {
    nn.ds$neonatal_delivery_procedure[i] <- nn.ds$c_section_type[i]
  } else {
      nn.ds$neonatal_delivery_procedure[i] <- nn.ds$born_method[i]
    }
  }
}
```

#### Clean up *premature* column into curated *neonatal_preterm_birth* column

Use **gestational_age** data element to clean up premature status.

Pre-maturity (preterm birth) is defined as any birth less than 37 weeks
and 0 days gestational age.

``` r
nn.ds$neonatal_preterm_birth <- NA

for (i in 1:nrow(nn.ds)) {
  if (is.na(nn.ds$gestational_age[i])) {
    nn.ds$neonatal_preterm_birth[i] <- nn.ds$premature[i]
  } else {
    if (nn.ds$gestational_age[i] < 37) {
      nn.ds$neonatal_preterm_birth[i] <- 'yes'
    } else if (nn.ds$gestational_age[i] >= 37) {
      nn.ds$neonatal_preterm_birth[i] <- 'no'
    } else {
      nn.ds$neonatal_preterm_birth[i] <- nn.ds$premature[x]
    }
  }
}
```

#### Clean up *feeding_practice* column into curated *neonatal_feeding_method* column

Replace “any_breastfeeding” **feeding_practice** data element answer
with “exclusively_breastfeeding; mixed_feeding” answer.

Clean up “exclusively_breastfeeding” answer in the **feeding_practice**
column to include any infant who was breastfed *and* did not receive
formula between the time of birth and data collection. If **infant_age**
data element is less than the day marked for **formula_first_day**
element and there is a valid (non-NA) **breastfeeding_duration** answer,
change feeding method answer to “exclusively_breastfeeding”. This is
with the notion that **infant_age** column provides the age of the
infant at the time that samples were collected.

``` r
nn.ds$neonatal_feeding_method <- NA

for (i in 1:nrow(nn.ds)) {
  if (is.na(nn.ds$feeding_practice[i])) {
    nn.ds$neonatal_feeding_method[i] <- nn.ds$feeding_practice[i]
  } else if (nn.ds$feeding_practice[i] == 'any_breastfeeding') {
    nn.ds$neonatal_feeding_method[i] <- 'exclusively_breastfeeding; mixed_feeding'
  } else if (is.na(nn.ds$formula_first_day[i]) == TRUE) {
    nn.ds$neonatal_feeding_method[i] <- nn.ds$feeding_practice[i]
  } else if (!(nn.ds$formula_first_day[i] <= nn.ds$infant_age[i]) 
             & !(is.na(nn.ds$breastfeeding_duration[i]))) {
    nn.ds$neonatal_feeding_method[i] <- 'exclusively_breastfeeding'
  } else {
    nn.ds$neonatal_feeding_method[i] <- nn.ds$feeding_practice[i]
  }
}
```

#### Rename unchanged neonatal data columns

``` r
nn.ds %>% 
  rename(
    neonatal_birth_weight = birth_weight,
    neonatal_gestational_age = gestational_age)
```

#### Generate a .csv file with the curated neonatal data columns

``` r
write.csv(nn.ds, file = 'curated_neonatal_cols.csv')
```
