Harmonize Neonatal-Related Attributes in cMD Metadata
================
Britney Pheng, Sehyun Oh <br>
February 13, 2024

``` r
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install('curatedMetagenomicData')
```

    ## Bioconductor version 3.18 (BiocManager 1.30.22), R 4.3.2 (2023-10-31)

    ## Warning: package(s) not installed when version(s) same as or greater than current; use
    ##   `force = TRUE` to re-install: 'curatedMetagenomicData'

    ## Old packages: 'rvest'

``` r
suppressPackageStartupMessages({
  library(curatedMetagenomicData)
  library(dplyr)
})
```

#### Create a list of neonatal data column names

``` r
neonatal_cols <- c("age_category",
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

#### Create a new dataframe *curated_neo* with a column for unique study:sample_id rows and to select for neonatal columns

``` r
curated_neo <- sampleMetadata %>%
    mutate(curation_id = paste(study_name, sample_id, sep = ":")) %>%
    dplyr::select(curation_id, neonatal_cols)
```

#### Clean up *born_method* column into curated *neonatal_delivery_procedure* column

Use **c_section_type** data element to expand **born_method** column
answers. C-section could either be an Emergency CS, Elective CS, or kept
broadly as “C-section” if there is no expanded answer in the
corresponding **c_section_type** column.

``` r
curated_neo$neonatal_delivery_procedure <- NA

for (x in 1:nrow(curated_neo)) {
  if (is.na(curated_neo$born_method[x]) == TRUE) {
    curated_neo$neonatal_delivery_procedure[x] <- NA
  } else {
    if (curated_neo$born_method[x] == 'c_section' & !is.na(curated_neo$c_section_type[x])) {
    curated_neo$neonatal_delivery_procedure[x] <- curated_neo$c_section_type[x]
  } else {
      curated_neo$neonatal_delivery_procedure[x] <- curated_neo$born_method[x]
    }
  }
}
```

#### Clean up *premature* column into curated *neonatal_preterm_birth* column

Use **gestational_age** data element to clean up premature status.

Prematurity (preterm birth) is defined as any birth less than 37 weeks
and 0 days gestational age.

``` r
curated_neo$neonatal_preterm_birth <- NA

for (x in 1:nrow(curated_neo)) {
  if (is.na(curated_neo$gestational_age[x]) == TRUE) {
    curated_neo$neonatal_preterm_birth[x] <- curated_neo$premature[x]
  } else {
    if (curated_neo$gestational_age[x] < 37) {
      curated_neo$neonatal_preterm_birth[x] <- 'yes'
    } else if (curated_neo$gestational_age[x] >= 37) {
      curated_neo$neonatal_preterm_birth[x] <- 'no'
    } else {
      curated_neo$neonatal_preterm_birth[x] <- curated_neo$premature[x]
    }
  }
}
```

#### Clean up *feeding_practice* column into curated *neonatal_feeding_method* column

Replace “any_breastfeeding” **feeding_practice** data element answer
with “exclusively_breastfeeding; mixed_feeding” answer.

Clean up “exclusively_breastfeeding” answer in the **feeding_practice**
column to count as any infant who was breastfed *and* did not receive
formula between the time of birth and data collection. If **infant_age**
data element is less than the day marked for **formula_first_day**
element and there is a valid (non-NA) **breastfeeding_duration** answer,
change feeding method answer to “exclusively_breastfeeding”. This is
with the notion that **infant_age** column provides the age of the
infant at the time that answers were collected.

``` r
curated_neo$neonatal_feeding_method <- NA

for (x in 1:nrow(curated_neo)) {
  if (is.na(curated_neo$feeding_practice[x]) == TRUE) {
    curated_neo$neonatal_feeding_method[x] <- curated_neo$feeding_practice[x]
  } else if (curated_neo$feeding_practice[x] == 'any_breastfeeding') {
    curated_neo$neonatal_feeding_method[x] <- 'exclusively_breastfeeding; mixed_feeding'
  } else if (is.na(curated_neo$formula_first_day[x]) == TRUE) {
    curated_neo$neonatal_feeding_method[x] <- curated_neo$feeding_practice[x]
  } else if (!(curated_neo$formula_first_day[x] <= curated_neo$infant_age[x]) 
             & !(is.na(curated_neo$breastfeeding_duration[x]))) {
    curated_neo$neonatal_feeding_method[x] <- 'exclusively_breastfeeding'
  } else {
    curated_neo$neonatal_feeding_method[x] <- curated_neo$feeding_practice[x]
  }
}
```

#### Rename unchanged neonatal data columns

``` r
curated_neo %>% 
  rename(
    neonatal_birth_weight = birth_weight,
    neonatal_gestational_age = gestational_age)
```

#### Generate a .csv file with the curated neonatal data columns

``` r
write.csv(curated_neo, file = 'curated_neonatal_cols.csv')
```
