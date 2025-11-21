# Step 8: Load and inspect datasets
# --------------------------------
# Purpose: Read both datasets and review column names for merging
# Author: Lasya Ramachandruni
# Date: November 2025

# --- Load libraries ---
library(readr)
library(dplyr)

# --- Load Data (use double backslashes in Windows file paths) ---
pod <- read_csv("C:\\Users\\gsaip\\OneDrive\\Attachments\\Desktop\\Proposal 2\\California Water Rights Points of Diversion (POD) List\\ewrims_flat_file_pod-flat-file.csv")

irrigation <- read_csv("C:\\Users\\gsaip\\OneDrive\\Attachments\\Desktop\\Proposal 2\\California Water Rights Irrigation Use Annual Reports\\water-rights-irrigation-water-use-annual-report.csv")

# --- Inspect column names ---
cat("POD dataset columns:\n")
print(names(pod))

cat("\nIrrigation dataset columns:\n")
print(names(irrigation))

# --- Quick overview of data types and first few rows ---
cat("\nPOD Dataset Overview:\n")
glimpse(pod)

cat("\nIrrigation Dataset Overview:\n")
glimpse(irrigation)
