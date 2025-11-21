# Step 11: Regression Modeling
library(dplyr)
library(ggplot2)
library(broom)
library(readr)

# --- Load the merged dataset ---
merged <- read_csv("data/merged_water_rights.csv")

# --- Prepare data ---
# Create irrigation intensity variable (AF per acre)
merged <- merged %>%
  mutate(
    irrigation_intensity = FACE_VALUE_AMOUNT / TOTAL_ACRES_IRRIGATED,
    log_FACE_VALUE_AMOUNT = log1p(FACE_VALUE_AMOUNT),  # log(1+x) avoids log(0)
    log_irrigation_intensity = log1p(irrigation_intensity),
    log_TOTAL_ACRES_IRRIGATED = log1p(TOTAL_ACRES_IRRIGATED)
  ) %>%
  filter(
    !is.na(FACE_VALUE_AMOUNT),
    !is.na(TOTAL_ACRES_IRRIGATED),
    TOTAL_ACRES_IRRIGATED > 0
  )

# --- Model 1: Predict total diverted volume (FACE_VALUE_AMOUNT) ---
model1 <- lm(FACE_VALUE_AMOUNT ~ TOTAL_ACRES_IRRIGATED + LATITUDE + LONGITUDE + WATER_RIGHT_TYPE.x + COUNTY.x, data = merged)
summary(model1)

# --- Model 2: Log-transformed version ---
model2 <- lm(log_FACE_VALUE_AMOUNT ~ log_TOTAL_ACRES_IRRIGATED + LATITUDE + LONGITUDE + WATER_RIGHT_TYPE.x + COUNTY.x, data = merged)
summary(model2)

# --- Model 3: Predict irrigation intensity (AF/acre) ---
model3 <- lm(irrigation_intensity ~ LATITUDE + LONGITUDE + WATER_RIGHT_TYPE.x + COUNTY.x, data = merged)
summary(model3)

# --- Model 4: Log-transformed intensity ---
model4 <- lm(log_irrigation_intensity ~ LATITUDE + LONGITUDE + WATER_RIGHT_TYPE.x + COUNTY.x, data = merged)
summary(model4)

# --- Compare models ---
results <- data.frame(
  Model = c("Raw Volume", "Log Volume", "Raw Intensity", "Log Intensity"),
  R2 = c(summary(model1)$r.squared, summary(model2)$r.squared,
         summary(model3)$r.squared, summary(model4)$r.squared),
  Adj_R2 = c(summary(model1)$adj.r.squared, summary(model2)$adj.r.squared,
             summary(model3)$adj.r.squared, summary(model4)$adj.r.squared)
)
print(results)

# --- Residual diagnostics plot (log models) ---
par(mfrow = c(2, 2))
plot(model2, main = "Diagnostics: Log Volume Model")
plot(model4, main = "Diagnostics: Log Intensity Model")

# --- Save coefficients table ---
coef_tbl <- broom::tidy(model2) %>%
  mutate(Model = "Log Volume") %>%
  bind_rows(broom::tidy(model4) %>% mutate(Model = "Log Intensity"))

write_csv(coef_tbl, "paper/tables/model_coefficients.csv")
cat("\nModel results saved to paper/tables/model_coefficients.csv\n")
