# Step 10: Exploratory Data Analysis (EDA)
library(dplyr)
library(ggplot2)
library(readr)

# --- Load merged data ---
merged <- read_csv("data/merged_water_rights.csv")

# --- Quick summary ---
cat("Merged data dimensions:\n")
print(dim(merged))

cat("\nColumn overview:\n")
print(names(merged))

summary(merged)

# --- Check missing values ---
cat("\nMissing values per column:\n")
colSums(is.na(merged))

# --- Basic numeric summaries ---
merged %>%
  summarise(
    mean_irrigated = mean(TOTAL_ACRES_IRRIGATED, na.rm = TRUE),
    median_irrigated = median(TOTAL_ACRES_IRRIGATED, na.rm = TRUE),
    mean_face_value = mean(FACE_VALUE, na.rm = TRUE),
    mean_face_value_amount = mean(FACE_VALUE_AMOUNT, na.rm = TRUE)
  )

# --- Histogram: total acres irrigated ---
ggplot(merged, aes(x = TOTAL_ACRES_IRRIGATED)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Total Acres Irrigated",
    x = "Total Acres Irrigated",
    y = "Frequency"
  ) +
  theme_minimal()

# --- Histogram: FACE_VALUE_AMOUNT ---
ggplot(merged, aes(x = FACE_VALUE_AMOUNT)) +
  geom_histogram(binwidth = 50, fill = "darkgreen", color = "white") +
  labs(
    title = "Distribution of Face Value Amount (AF/Year)",
    x = "Face Value Amount (Acre-Feet/Year)",
    y = "Frequency"
  ) +
  theme_minimal()

# --- Scatterplot: Irrigated Area vs Face Value ---
ggplot(merged, aes(x = TOTAL_ACRES_IRRIGATED, y = FACE_VALUE_AMOUNT)) +
  geom_point(alpha = 0.3, color = "purple") +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  labs(
    title = "Irrigated Area vs Face Value Amount (log scale)",
    x = "Total Acres Irrigated (log)",
    y = "Face Value Amount (log)"
  ) +
  theme_minimal()
