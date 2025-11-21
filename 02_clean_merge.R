# Step 9: Clean and merge POD and Irrigation datasets (fixed join types)
library(dplyr)
library(readr)

# --- Convert join columns to character in both datasets ---
pod_clean <- pod %>%
  mutate(
    APPLICATION_NUMBER = as.character(APPLICATION_NUMBER),
    PERMIT_ID = as.character(PERMIT_ID),
    LICENSE_ID = as.character(LICENSE_ID),
    CERTIFICATE_ID = as.character(CERTIFICATE_ID)
  ) %>%
  select(
    WR_WATER_RIGHT_ID,
    APPLICATION_NUMBER,
    PERMIT_ID,
    LICENSE_ID,
    CERTIFICATE_ID,
    WATER_RIGHT_TYPE,
    WATER_RIGHT_STATUS,
    COUNTY,
    LATITUDE,
    LONGITUDE,
    FACE_VALUE_AMOUNT,
    FACE_VALUE_UNITS
  )

irrigation_clean <- irrigation %>%
  mutate(
    APPLICATION_NUMBER = as.character(APPLICATION_NUMBER),
    PERMIT_ID = as.character(PERMIT_ID),
    LICENSE_ID = as.character(LICENSE_ID),
    CERTIFICATE_ID = as.character(CERTIFICATE_ID)
  ) %>%
  select(
    REPORT_YEAR,
    APPLICATION_NUMBER,
    PERMIT_ID,
    LICENSE_ID,
    CERTIFICATE_ID,
    WATER_RIGHT_TYPE,
    WATER_RIGHT_STATUS,
    AREA_IRRIGATED,
    TOTAL_ACRES_IRRIGATED,
    FACE_VALUE,
    FACE_VALUE_UNIT,
    COUNTY,
    CROP_TYPE
  )

# --- Merge datasets ---
merged <- irrigation_clean %>%
  left_join(
    pod_clean,
    by = c("APPLICATION_NUMBER", "PERMIT_ID", "LICENSE_ID", "CERTIFICATE_ID")
  )

# --- Check merge results ---
cat("Merged dataset dimensions:\n")
print(dim(merged))

cat("\nMerged dataset columns:\n")
print(names(merged))

# --- Save merged dataset ---
write_csv(merged, "data/merged_water_rights.csv")
cat("\nMerged dataset saved successfully in data/merged_water_rights.csv\n")
