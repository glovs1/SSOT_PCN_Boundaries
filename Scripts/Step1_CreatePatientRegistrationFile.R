library(tidyverse)
library(here)


# -------------------------------
# 1. Load GP Registered Data and Practice Lookup
# -------------------------------
gp_patients <- read.csv(here("Data","gp-reg-pat-prac-lsoa-all.csv"))
practice_lookup <- read.csv(here("Data","gp-reg-pat-prac-map.csv"))


# -------------------------------
# 2. Merge, clean and filter for Stoke ICB (ICB_CODE = "QNC")
# -------------------------------
SSOT_data <- gp_patients |> 
  left_join(practice_lookup, by = "PRACTICE_CODE") |> 
  select(LSOA_CODE, PRACTICE_CODE, PRACTICE_NAME.x, NUMBER_OF_PATIENTS,
         PCN_CODE, PCN_NAME, SUB_ICB_LOCATION_NAME, ICB_CODE) |> 
  filter(ICB_CODE == "QNC", LSOA_CODE != "CLOSED", LSOA_CODE != "EMPTY")


# -------------------------------
# 3. Create a PCN to Sub_ICB lookup choosing the SubICB with most practices for ABC (05W) and Rugeley (04Y)
# -------------------------------


PCN_SUBICB_LOOKUP <- practice_lookup |>
  filter(ICB_CODE == "QNC") |>
  group_by(PCN_NAME, SUB_ICB_LOCATION_NAME) |>
  summarise(n_practices = n(), .groups = "drop") |>
  group_by(PCN_NAME) |>
  slice_max(n_practices, n = 1, with_ties = FALSE) |>
  ungroup() |> 
  select(PCN_NAME,SUB_ICB_LOCATION_NAME)



# ----------------------------------
# this section of code allocates the Sub_ICB depending on which one has the most patients, rather than the number of practice
# for ABC it was split fairly equally between 05W and 05G with 26k patients in 05W and 23k in 05G and Rugeley had more in Cannock than Stafford
# ----------------------------------

#summarise(patients = sum(PCN_Patients, na.rm = TRUE), .groups = "drop") %>%
#  group_by(PCN_NAME) %>%
#  slice_max(patients, n = 1, with_ties = FALSE) %>%
#  ungroup() %>%
#  rename(SUB_ICB_LOCATION_NAME = Sub_ICB)



# -------------------------------
# 4. Aggregate patients by PCN and LSOA - this version does not group by Sub_ICB as ABC and Rugeley PCN's split across 2 sub-ICB's
# -------------------------------

SSOT_data <- SSOT_data |> 
  group_by(LSOA_CODE, PCN_NAME) |> 
  summarise(PCN_Patients = sum(NUMBER_OF_PATIENTS), .groups = "drop")

# -------------------------------
# 4. Calculate PCN totals and proportion within PCN
# -------------------------------
pcn_totals <- SSOT_data |> 
  group_by(PCN_NAME) |> 
  summarise(PCN_Total = sum(PCN_Patients), .groups = "drop")

SSOT_data <- SSOT_data |> 
  left_join(pcn_totals, by = "PCN_NAME") |> 
  mutate(Proportion_PCN = PCN_Patients / PCN_Total)


# -------------------------------
# 5. Get LSOA total population (from GP data)
# -------------------------------
Total_Pop <- gp_patients |> 
  filter(LSOA_CODE != "CLOSED", LSOA_CODE != "EMPTY") |> 
  group_by(LSOA_CODE) |> 
  summarise(LSOA_Total = sum(NUMBER_OF_PATIENTS), .groups = "drop")

# -------------------------------
# 6. Append LSOA population using match() (not join)
# -------------------------------
SSOT_data <- SSOT_data |> 
  mutate(LSOA_Population = Total_Pop$LSOA_Total[match(LSOA_CODE, Total_Pop$LSOA_CODE)],
         Proportion_LSOA = PCN_Patients / LSOA_Population)

# -------------------------------
# 7. Order by PCN and descending patient count
# -------------------------------
SSOT_data <- SSOT_data |> 
  arrange(PCN_NAME, desc(PCN_Patients)) |> 
  group_by(PCN_NAME) |> 
  mutate(CumulativePatients = cumsum(PCN_Patients),
         CumulativePercent = (CumulativePatients / PCN_Total) * 100) |> 
  ungroup()

# -------------------------------
# 8. Join Sub_ICB onto data file - need to make a note on any output about ABC and Rugeley allocated to 1 sub-icb
# -------------------------------

SSOT_data <- SSOT_data |> 
  left_join(PCN_SUBICB_LOOKUP, by = "PCN_NAME")

# -----------------------------------------------------------
# 8A) Recode SUB_ICB_LOCATION_NAME (uses dplyr)
# -----------------------------------------------------------

subicb_rename <- c(
  "NHS Staffordshire and Stoke-on-Trent ICB - 05W" = "Stoke-on-Trent",
  "NHS Staffordshire and Stoke-on-Trent ICB - 05G" = "North Staffordshire",
  "NHS Staffordshire and Stoke-on-Trent ICB - 04Y" = "Cannock Chase",
  "NHS Staffordshire and Stoke-on-Trent ICB - 05D" = "East Staffordshire",
  "NHS Staffordshire and Stoke-on-Trent ICB - 05Q" = "South East Staffs and Seisdon Peninsular",
  "NHS Staffordshire and Stoke-on-Trent ICB - 05V" = "Stafford and Surrounds"
)

# 8B) Replace the original Sub_ICB name with the vector from (8A). !!! expands or splices values from the named vector sub_icb_rename so you only have to change them once  

SSOT_data <- SSOT_data  |> 
  mutate(
    SUB_ICB_LOCATION_NAME =
      recode(SUB_ICB_LOCATION_NAME, !!!subicb_rename, .default = SUB_ICB_LOCATION_NAME)
  )


# 8C) Title Case manipulation

title_case_preserve <- function(x,
                                acronyms = c("NHS", "ICB", "PCN", "GP", "GPs", "CQC", "GPP", "PVC","ABC","HIPC"),
                                keep_small_words_lower = c("and","or","of","the","in","on","at","for","to","by","with")) {
  # 1) Normalize spacing
  x <- str_squish(x)
  
  # 2) Lowercase everything first, then title case words
  #    Use a regex-friendly approach to handle hyphens/apostrophes
  tc <- str_to_title(str_to_lower(x))
  
  # 3) Keep common small words in lowercase (except at start)
  #    Split and recombine to control first-word capitalization
  tc <- str_replace_all(tc, "\\s+", " ")
  tc <- vapply(str_split(tc, " "), function(words) {
    if (length(words) == 0) return("")
    # First word stays as-is (title cased)
    words[1] <- words[1]
    if (length(words) > 1) {
      words[-1] <- ifelse(
        str_to_lower(words[-1]) %in% keep_small_words_lower,
        str_to_lower(words[-1]),
        words[-1]
      )
    }
    paste(words, collapse = " ")
  }, character(1))
  
  # 4) Re-capitalize specified acronyms anywhere they occur
  #    Match whole words, including around punctuation (start/end/boundaries)
  for (ac in acronyms) {
    # Build a boundary-safe pattern (handles parentheses, commas, hyphens)
    pattern <- paste0("(?i)(?<=^|[^A-Za-z])(", ac, ")(?=[^A-Za-z]|$)")
    tc <- str_replace_all(tc, pattern, toupper(ac))
  }
  
  # 5) Ensure trailing "PCN" stays uppercase when it appears at the end,
  #    optionally with punctuation like ")"
  #    Examples: "… pcn", "… pcn)", "… pcn -"
  tc <- str_replace(tc, "(?i)\\bpcn\\b\\)?\\s*$", function(m) toupper(m))
  
  # 6) Fix title case around hyphens/apostrophes (e.g., "O'Connor", "Well-being")
  #    Apply a light touch—this preserves previously enforced acronyms
  tc <- str_replace_all(tc, "(?<=\\b\\w)'’", ~ str_to_upper(.x))
  tc <- str_replace_all(tc, "(?<=\\b\\w)-(\\w)", ~ str_to_title(.x))
  
  return(tc)
}



# 8D) Change PCN_NAME from CAPS to Title Case

SSOT_data <- SSOT_data |> 
  mutate(PCN_NAME = title_case_preserve(PCN_NAME))





# -------------------------------
# 9. Export final dataset
# -------------------------------
write_csv(SSOT_data, file = here("Outputs","SSOT_data.csv"))
          





