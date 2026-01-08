This repository contains R scripts and data to visualise the distribution of GP registered patients by Primary Care Network (PCN) for **Staffordshire and Stoke on Trent (SSOT) ICB**.


## Data Sources
GP registered patients are taken from the [Quarterly data](https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/october-2025) published at LSOA level by NHS England.

LSOA boundaries are taken from the [ONS Open Geography Portal](https://geoportal.statistics.gov.uk/search?q=BDY_LSOA%20DEC_2021&sort=Title%7Ctitle%7Casc)


## Patient Extent
There were approximately **1.2 million patients** registered with a GP practice in Staffordshire or Stoke-on-Trent at the end of October 2025.  

Although most patients live close to their GP practice, some have moved out of the area but retain their practice registration.  As this application aims to show the extent of the PCN boundaries, those patients living well outside the core SSOT ICB area have been excluded.  To do this LSOAs were ranked in descending order of patients and only those LSOAs which account cumulatively for up to **98.0%** of patients have been included (**1,183,293 patients**).  This has reduced the number of LSOAs included in the boundary visualisation from **3,340** to **769**.  The total number of ICB patients excluded is approximately **25,600**.


## The Application
For each Primary Care Network, users are able to show where between **50.0%** and **98.0%** of their registered patients live.  If users select 50% (the minimum proportion), the map will display those LSOAs that cumulatively account for half of the PCN patients.  This will provide the tightest catchment, with the least overlap to neighbouring PCN's.  The higher the proportion of patients displayed, the wider the PCN boundary and the bigger the overlap.  

Users can select multiple PCN's to see how the overlap changes depending on the proportion of patients selected.

The proportion selected applies to all PCN's i.e. selecting 50% will show where half the patients in every PCN displayed on the map live, 60% where 60% of patients in every PCN displayed on the map live etc.


## R Script to limit LSOAs to 98% of SSOT patients

The LSOA boundary file for England and Wales is approximately 0.5GB and too large to load directly into Github.  A SHAPE file was created for the LSOA boundaries that would be used in the application.  The code block below shows the steps taken to create this file

```

------------------------------------------------------
Load Libraries (install.packages first if necessary)
------------------------------------------------------

library(sf)
library(dplyr)

------------------------------
  # 1) Load SSOT patient data 
------------------------------

SSOT_data <- read.csv(here("Outputs","SSOT_data.csv"))

# -------------------------------------------------------------------
# 2) Load & prepare spatial data (change the path to match your file)
# -------------------------------------------------------------------

lsoa_shapes <- st_read("your path/LSOA_(Dec_2021)_Boundaries_Full_Clipped_EW_(BFC).shp")

# check Co-ordinate Reference System (likely to be British National Grid - 27700)
sf::st_crs(lsoa_shapes)

# Then transform to WGS84
lsoa_shapes <- st_transform(lsoa_shapes, 4326)

# -----------------------------------------------------------------------------------------------------------------------------------------
# 3) Join SSOT patient data to the LSOA boundaries and filter LSOAs up to 98% of patients 
# st_make_valid() function from sf package that repairs broken spatial geometries e.g. slivers, polygons with holes touching a boundary etc
# -----------------------------------------------------------------------------------------------------------------------------------------

map_data <- lsoa_shapes  |> 
  left_join(SSOT_data, by = c("LSOA21CD" = "LSOA_CODE"))  |>    
  filter(!is.na(PCN_NAME))  |>                                 
  filter(substr(LSOA21CD, 1, 1) == "E") |>
  filter(CumulativePercent < 98) |> 
  st_make_valid()


# ------------------------------------------------------------------------------------------------------
# 4) Inspect names in SHAPE file as writing to shape files truncates column names to only 10 characters
--------------------------------------------------------------------------------------------------------

names(map_data)  
  
name_map <- c(
  "PCN_Patients" = "PCN_Pat",
  "Proportion_PCN" ="PCN_Prop",
  "LSOA_Population" = "LSOA_Pop",
  "Proportion_LSOA" = "LSOA_Prop",
  "CumulativePatients" = "Cum_Pat",
  "CumulativePercent" = "Cum_Perc",
  "SUB_ICB_LOCATION_NAME" = "Sub_ICB"
)


# Apply the mapping only to columns that exist
to_rename <- intersect(names(map_data), names(name_map))
names(map_data)[match(to_rename, names(map_data))] <- name_map[to_rename]

# Double-check no name exceeds 10 characters
stopifnot(all(nchar(names(map_data)) <= 10))

# -----------------------
# 5) Write to Shapefile
-------------------------

write_sf(map_data,"your file path/map_data.shp", delete_layer = TRUE)

```

