Harmonize Neonatal-Related Attributes in cMD Metadata
================
Britney Pheng, Sehyun Oh <br>
February 28, 2024

# Setup

## Load packages

``` r
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install('curatedMetagenomicData')
```

``` r
suppressPackageStartupMessages({
  library(curatedMetagenomicData)
  library(tidyverse)
})
```

## Load data

``` r
## A list of neonatal data column names
neonatal_cols <- c("infant_age",
                   "born_method", 
                   "gestational_age",
                   "premature",
                   "birth_weight",
                   "c_section_type",
                   "feeding_practice",
                   "formula_first_day",
                   "breastfeeding_duration")
```

``` r
tb <- sampleMetadata %>%
    mutate(curation_id = paste(study_name, sample_id, sep = ":")) %>%
    dplyr::select(curation_id, neonatal_cols)
```

# neonatal_delivery_procedure

The two original columns `born_method` and `c_section_type` are
harmonized to the curated column, `neonatal_delivery_procedure`.

## EDA

``` r
## 782 samples have information from both `born_method` and `c_section_type`
na_summary <- rowSums(!is.na(tb[c("born_method", "c_section_type")]))
table(na_summary)
```

    ## na_summary
    ##     0     1     2 
    ## 19660  2146   782

``` r
## All the used terms
unique(tb$born_method)
```

    ## [1] NA          "vaginal"   "c_section"

``` r
unique(tb$c_section_type)
```

    ## [1] NA             "Elective_CS"  "Emergency_CS"

``` r
both_ind <- which(na_summary == 2)
unique(tb$born_method[both_ind])
```

    ## [1] "c_section"

``` r
## No sample has only `c_section_type` info
sum(is.na(tb$born_method) & !is.na(tb$c_section_type)) 
```

    ## [1] 0

## Consolidate redundant info

``` r
tb$source <- NA
tb$value <- NA

for (i in seq_len(nrow(tb))) {
    if (is.na(tb$born_method[i])) {
        tb$source[i] <- NA
        tb$value[i] <- NA
    } else if (!is.na(tb$born_method[i]) & is.na(tb$c_section_type[i])) {
        tb$source[i] <- "born_method"
        tb$value[i] <- tb$born_method[i]
    } else {
        tb$source[i] <- "c_section_type"
        tb$value[i] <- tb$c_section_type[i]
    }
}
```

## Curate ontology terms

### Create curation map

The ‘allowedvalues’ and their ‘ontology ids’ are available in the data
dictionary Google Sheet, `cMD_data_dictionary`, or in the GitHub repo.

You can look up the format of the curation maps in the manuscript’s
supplementary method section or other curation maps.

``` r
delivery_map <- data.frame(
    original_value = c("Elective_CS", 
                       "Emergency_CS",
                       "c_section",
                       "vaginal"),
    curated_ontology_term = c("Elective Cesarean Delivery", 
                              "Emergency Cesarean Delivery",
                              "Cesarean Section",
                              "Vaginal Delivery"),
    curated_ontology_term_id = c("NCIT:C114141", 
                                 "NCIT:C92772", 
                                 "NCIT:C46088", 
                                 "NCIT:C81303"),
    curated_ontology_term_db = c("NCIT", "NCIT", "NCIT", "NCIT")
)
```

### Update the metadata with the curated terms

``` r
curated_delivery_dat <- tb %>%
    transmute(curation_id = curation_id,
              original_source = source,
              original_value = value,
              curated_ontology_term = plyr::mapvalues(
                  x = value, 
                  from = delivery_map$original_value,
                  to = delivery_map$curated_ontology_term,
                  warn_missing = FALSE
              )) %>%
    mutate(curated_ontology_term_id = plyr::mapvalues(
        x = curated_ontology_term,
        from = delivery_map$curated_ontology_term,
        to = delivery_map$curated_ontology_term_id,
        warn_missing = FALSE
    )) %>%
    mutate(curated_ontology_term_db = plyr::mapvalues(
        x = curated_ontology_term,
        from = delivery_map$curated_ontology_term,
        to = delivery_map$curated_ontology_term_db,
        warn_missing = FALSE
    ))
```

Check the harmonized/curated metadata

``` r
non_na_ind <- which(!is.na(curated_delivery_dat$original_source))
head(curated_delivery_dat[c(1:3, non_na_ind),])
```

    ##                      curation_id original_source original_value
    ## 1    AsnicarF_2017:MV_FEI1_t1Q14            <NA>           <NA>
    ## 2    AsnicarF_2017:MV_FEI2_t1Q14            <NA>           <NA>
    ## 3    AsnicarF_2017:MV_FEI3_t1Q14            <NA>           <NA>
    ## 1123       BackhedF_2015:SID10_B     born_method        vaginal
    ## 1125      BackhedF_2015:SID10_4M     born_method        vaginal
    ## 1126     BackhedF_2015:SID10_12M     born_method        vaginal
    ##      curated_ontology_term curated_ontology_term_id curated_ontology_term_db
    ## 1                     <NA>                     <NA>                     <NA>
    ## 2                     <NA>                     <NA>                     <NA>
    ## 3                     <NA>                     <NA>                     <NA>
    ## 1123      Vaginal Delivery              NCIT:C81303                     NCIT
    ## 1125      Vaginal Delivery              NCIT:C81303                     NCIT
    ## 1126      Vaginal Delivery              NCIT:C81303                     NCIT

## Save the results

``` r
readr::write_csv(delivery_map, "maps/cMD_delivery_procedure_map.csv")
readr::write_csv(curated_delivery_dat, "data/curated_delivery_procedure.csv")
```

# neonatal_preterm_birth

The two original columns `gestational_age` and `premature` are
harmonized to the curated column `neonatal_preterm_birth`.

## EDA

``` r
## 1891 samples have information for both `gestational_age` and `premature`
na_summary <- rowSums(!is.na(tb[c("gestational_age", "premature")]))
table(na_summary)
```

    ## na_summary
    ##     0     1     2 
    ## 20045   652  1891

``` r
## All the used values
unique(tb$premature)
```

    ## [1] NA    "yes" "no"

``` r
sort(unique(tb$gestational_age), na.last=FALSE)
```

    ##  [1]   NA 25.0 26.0 27.0 28.0 29.0 30.0 31.0 32.0 32.6 36.4 36.6 37.0 37.1 37.2
    ## [16] 37.3 37.4 38.0 38.4 38.7 38.9 39.0 39.1 39.2 39.3 39.4 39.5 39.6 39.9 40.0
    ## [31] 40.1 40.3 40.6 40.7 40.9 41.4 41.7 42.1 42.4

``` r
## 329 samples have no `premature` info, but a provided `gestational_age` observation
sum(is.na(tb$premature) & !is.na(tb$gestational_age))
```

    ## [1] 329

``` r
## 4 samples can have `neonatal_preterm_birth` == 'yes' 
sum(is.na(tb$premature) & tb$gestational_age < 37, na.rm=TRUE)
```

    ## [1] 4

``` r
## 325 samples can have `neonatal_preterm_birth` == 'no'
sum(is.na(tb$premature) & tb$gestational_age >= 37, na.rm=TRUE)
```

    ## [1] 325

## Consolidate redundant info

``` r
tb$source <- NA
tb$value <- NA

for (i in seq_len(nrow(tb))) {
  if (is.na(tb$gestational_age[i])) {
    tb$source[i] <- NA
    tb$value[i] <- NA
  } else if (is.na(tb$premature[i]) & tb$gestational_age[i] < 37) {
    tb$source[i] <- "gestational_age"
    tb$value[i] <- "yes"
  } else if (is.na(tb$premature[i]) & tb$gestational_age[i] >= 37) {
    tb$source[i] <- "gestational_age"
    tb$value[i] <- "no"
  } else {
    tb$source[i] <- "premature"
    tb$value[i] <- tb$premature[i]
  }
}
```

## Curate ontology terms

### Create curation map

``` r
premature_map <- data.frame(
  original_value = c("yes",
                     "no"),
  curated_ontology_term = c("Yes",
                            "No"),
  curated_ontology_term_id = c("NCIT:C92861",
                              "NCIT:C114093"),
  curated_ontology_term_db = c("NCIT", "NCIT")
)
```

### Update the metadata with the curated terms

``` r
curated_premature_dat <- tb %>%
  transmute(curation_id = curation_id,
            original_source = source,
            original_value = value,
            curated_ontology_term = plyr::mapvalues(
              x = value,
              from = premature_map$original_value,
              to = premature_map$curated_ontology_term,
              warn_missing = FALSE
            )) %>%
    mutate(curated_ontology_term_id = plyr::mapvalues(
      x = curated_ontology_term,
      from = premature_map$curated_ontology_term,
      to = premature_map$curated_ontology_term_db,
      warn_missing = FALSE
    ))
```

Check the harmonized/curated metadata

``` r
non_na_ind <- which(!is.na(curated_premature_dat$original_source))
head(curated_premature_dat[c(1:3, non_na_ind),])
```

    ##                      curation_id original_source original_value
    ## 1    AsnicarF_2017:MV_FEI1_t1Q14            <NA>           <NA>
    ## 2    AsnicarF_2017:MV_FEI2_t1Q14            <NA>           <NA>
    ## 3    AsnicarF_2017:MV_FEI3_t1Q14            <NA>           <NA>
    ## 1964   BrooksB_2017:N2_031_008G1       premature            yes
    ## 1965   BrooksB_2017:N2_031_010G1       premature            yes
    ## 1966   BrooksB_2017:N2_031_012G1       premature            yes
    ##      curated_ontology_term curated_ontology_term_id
    ## 1                     <NA>                     <NA>
    ## 2                     <NA>                     <NA>
    ## 3                     <NA>                     <NA>
    ## 1964                   Yes                     NCIT
    ## 1965                   Yes                     NCIT
    ## 1966                   Yes                     NCIT

### Save the results

``` r
readr::write_csv(premature_map, "maps/cMD_preterm_birth_map.csv")
readr::write_csv(curated_premature_dat, "data/curated_preterm_birth.csv")
```

# neonatal_feeding_method

The three original columns `feeding_practice`, `formula_first_day`, and
`breastfeeding_duration` are harmonized to the curated column
`neonatal_feeding_method`.

## EDA

``` r
## 136 samples have information for all three columns
na_summary <- rowSums(!is.na(tb[c("feeding_practice", "formula_first_day", "breastfeeding_duration")]))
table(na_summary)
```

    ## na_summary
    ##     0     1     2     3 
    ## 21850   587    15   136

``` r
## All the used values
unique(tb$feeding_practice)
```

    ## [1] NA                            "mixed_feeding"              
    ## [3] "exclusively_breastfeeding"   "no_breastfeeding"           
    ## [5] "any_breastfeeding"           "exclusively_formula_feeding"

``` r
sort(unique(tb$formula_first_day), na.last=FALSE)
```

    ##  [1]  NA   1   2   3   4   6  16  47  69 101 139 162 172 175 182 183 184 186 194
    ## [20] 195 199 227 252

``` r
sort(unique(tb$breastfeeding_duration), na.last=FALSE)
```

    ##  [1]  NA 108 110 147 182 217 246 265 268 279 296 305 326 331 344 345 365 370 376
    ## [20] 385 408 409 411 419 456 486 487 488 699 735

``` r
## No samples with a blank feeding practice and values for breastfeeding and formula feeding
sum(is.na(tb$feeding_practice) & !is.na(tb$formula_first_day) & !is.na(tb$breastfeeding_duration))
```

    ## [1] 0

``` r
## 21 samples with "any_breastfeeding" value for `feeding_practice` variable
sum((tb$feeding_practice == "any_breastfeeding"), na.rm = TRUE)
```

    ## [1] 21

``` r
## 85 samples where `feeding_practice` should be "exclusively_breastfeeding"
sum(!is.na(tb$breastfeeding_duration) & (tb$infant_age < tb$formula_first_day), na.rm = TRUE)
```

    ## [1] 85

``` r
## 70 samples out of the 85 have "exclusively_breastfeeding" for `feeding_practice` column
sum((tb$feeding_practice == "exclusively_breastfeeding") & !is.na(tb$breastfeeding_duration) & (tb$infant_age < tb$formula_first_day), na.rm = TRUE)
```

    ## [1] 70

``` r
## 15 samples should have "exclusively_breastfeeding" as their feeding_practice value given that the first day of formula feeding was after the date the sample was collected
sum((tb$feeding_practice == 'mixed_feeding') & !is.na(tb$breastfeeding_duration) & (tb$infant_age < tb$formula_first_day))
```

    ## [1] 15

## Consolidate redundant info

``` r
tb$source <- NA
tb$value <- NA

for (i in seq_len(nrow(tb))) {
  if (is.na(tb$feeding_practice[i])) {
    tb$source[i] <- NA
    tb$value[i] <- NA
  } else if (tb$feeding_practice[i] == "any_breastfeeding") {
    tb$source[i] <- "feeding_practice"
    tb$value[i] <- "exclusively_breastfeeding; mixed_feeding"
  } else if ((tb$formula_first_day[i] > tb$infant_age[i]) &
             !is.na(tb$breastfeeding_duration[i]) &
             !(tb$feeding_practice[i] == "exclusively_breastfeeding")) {
    tb$source[i] <- "formula_first_day"
    tb$value[i] <- "exclusively_breastfeeding"
  } else {
    tb$source[i] <- "feeding_practice"
    tb$value[i] <- tb$feeding_practice[i]
  }
}
```

## Curate ontology terms

### Create curation map

“mixed_feeding” “exclusively_breastfeeding”  
\[4\] “no_breastfeeding” “any_breastfeeding”
“exclusively_formula_feeding”

``` r
feeding_map <- data.frame(
  original_value = c("exclusively_breastfeeding; mixed_feeding",
                     "exclusively_breastfeeding",
                     "exclusively_formula_feeding",
                     "mixed_feeding",
                     "no_breastfeeding"
                     ),
  curated_ontology_term = c("Exclusively Breastfeeding; Mixed Feeding",
                            "Exclusively Breastfeeding",
                            "Exclusively Formula Feeding",
                            "Mixed Feeding",
                            "No Breastfeeding"),
  curated_ontology_term_id = c("ONS:1000029; NA",
                              "ONS:1000029",
                              "MAXO:0000775",
                              NA,
                              NA),
  curated_ontology_term_db = c("ONS; NA", "ONS", "MAXO", NA, NA)
)
```

### Update the metadata with the curated terms

``` r
curated_feeding_dat <- tb %>%
  transmute(curation_id = curation_id,
            original_source = source,
            original_value = value,
            curated_ontology_term = plyr::mapvalues(
              x = value,
              from = feeding_map$original_value,
              to = feeding_map$curated_ontology_term,
              warn_missing = FALSE
            )) %>%
    mutate(curated_ontology_term_id = plyr::mapvalues(
      x = curated_ontology_term,
      from = feeding_map$curated_ontology_term,
      to = feeding_map$curated_ontology_term_db,
      warn_missing = FALSE
    ))
```

Check the harmonized/curated metadata

``` r
non_na_ind <- which(!is.na(curated_feeding_dat$original_source))
head(curated_feeding_dat[c(1:3, non_na_ind),])
```

    ##                      curation_id  original_source            original_value
    ## 1    AsnicarF_2017:MV_FEI1_t1Q14             <NA>                      <NA>
    ## 2    AsnicarF_2017:MV_FEI2_t1Q14             <NA>                      <NA>
    ## 3    AsnicarF_2017:MV_FEI3_t1Q14             <NA>                      <NA>
    ## 1123       BackhedF_2015:SID10_B feeding_practice             mixed_feeding
    ## 1125      BackhedF_2015:SID10_4M feeding_practice exclusively_breastfeeding
    ## 1126     BackhedF_2015:SID10_12M feeding_practice          no_breastfeeding
    ##          curated_ontology_term curated_ontology_term_id
    ## 1                         <NA>                     <NA>
    ## 2                         <NA>                     <NA>
    ## 3                         <NA>                     <NA>
    ## 1123             Mixed Feeding                     <NA>
    ## 1125 Exclusively Breastfeeding                      ONS
    ## 1126          No Breastfeeding                     <NA>

### Save the results

``` r
readr::write_csv(feeding_map, "maps/cMD_feeding_map.csv")
readr::write_csv(curated_feeding_dat, "data/curated_feeding_method.csv")
```

#### Rename unchanged neonatal data columns

# `{r rename data columns, results='hide'} # nn.ds %>% #   rename( #     neonatal_birth_weight = birth_weight, #     neonatal_gestational_age = gestational_age) #`
