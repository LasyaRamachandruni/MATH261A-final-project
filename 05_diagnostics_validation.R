install.packages("car")
library(car)
install.packages("caret")
library(caret)
install.packages(c(
  "dplyr", "readr", "ggplot2", "broom",
  "lmtest", "sandwich", "car", "caret", "tibble", "data.table"
))
library(dplyr)
library(readr)
library(ggplot2)
library(broom)
library(lmtest)
library(sandwich)
library(car)
library(caret)
library(tibble)
library(data.table)
# Step 12: Diagnostics & Validation
# - Robust SEs (HC)
# - Heteroskedasticity tests
# - Multicollinearity (VIF)
# - k-fold CV (RMSE/MAE) for the two log models
# - Save tables & plots

library(dplyr)
library(readr)
library(ggplot2)
library(broom)
library(lmtest)
library(sandwich)
library(car)
library(caret)

# --- Load data and rebuild features consistently ---
merged <- read_csv("data/merged_water_rights.csv") %>%
  mutate(
    irrigation_intensity = FACE_VALUE_AMOUNT / TOTAL_ACRES_IRRIGATED,
    log_FACE_VALUE_AMOUNT = log1p(FACE_VALUE_AMOUNT),
    log_irrigation_intensity = log1p(irrigation_intensity),
    log_TOTAL_ACRES_IRRIGATED = log1p(TOTAL_ACRES_IRRIGATED)
  ) %>%
  filter(
    !is.na(FACE_VALUE_AMOUNT),
    !is.na(TOTAL_ACRES_IRRIGATED),
    TOTAL_ACRES_IRRIGATED > 0
  )

# Ensure categorical vars are factors (drop super-rare levels safely)
merged <- merged %>%
  mutate(
    WATER_RIGHT_TYPE.x = factor(WATER_RIGHT_TYPE.x),
    COUNTY.x = factor(COUNTY.x)
  ) %>%
  group_by(COUNTY.x) %>%
  mutate(.n_cty = n()) %>%
  ungroup() %>%
  mutate(COUNTY.x = ifelse(.n_cty < 50, "Other", as.character(COUNTY.x))) %>%
  mutate(COUNTY.x = factor(COUNTY.x)) %>%
  select(-.n_cty)

# --- Refit the two main log models ---
m_log_vol <- lm(log_FACE_VALUE_AMOUNT ~ log_TOTAL_ACRES_IRRIGATED + LATITUDE + LONGITUDE + WATER_RIGHT_TYPE.x + COUNTY.x, data = merged)
m_log_int <- lm(log_irrigation_intensity ~ LATITUDE + LONGITUDE + WATER_RIGHT_TYPE.x + COUNTY.x, data = merged)

# -----------------------------
# Robust SEs (HC) + coeftest
# -----------------------------
robust_tbl <- bind_rows(
  broom::tidy(coeftest(m_log_vol, vcov = vcovHC(m_log_vol, type = "HC1"))) %>% mutate(Model = "Log Volume"),
  broom::tidy(coeftest(m_log_int, vcov = vcovHC(m_log_int, type = "HC1"))) %>% mutate(Model = "Log Intensity")
)
write_csv(robust_tbl, "paper/tables/model_coefficients_robustHC.csv")

# -----------------------------
# Heteroskedasticity tests
# -----------------------------
bp_vol <- bptest(m_log_vol)
bp_int <- bptest(m_log_int)

cat("\nBreusch-Pagan (Log Volume):\n"); print(bp_vol)
cat("\nBreusch-Pagan (Log Intensity):\n"); print(bp_int)

# -----------------------------
# Multicollinearity (VIF)
# -----------------------------
vif_vol <- car::vif(m_log_vol)
vif_int <- car::vif(m_log_int)

write.csv(as.data.frame(vif_vol), "paper/tables/vif_log_volume.csv", row.names = TRUE)
write.csv(as.data.frame(vif_int), "paper/tables/vif_log_intensity.csv", row.names = TRUE)

# -----------------------------
# Residual diagnostics plots
# -----------------------------
dir.create("paper/figures", showWarnings = FALSE, recursive = TRUE)

png("paper/figures/diag_log_volume.png", width = 1400, height = 1000, res = 160)
par(mfrow = c(2,2)); plot(m_log_vol)
dev.off()

png("paper/figures/diag_log_intensity.png", width = 1400, height = 1000, res = 160)
par(mfrow = c(2,2)); plot(m_log_int)
dev.off()

# -----------------------------
# k-fold Cross-Validation
# -----------------------------
set.seed(261)
ctrl <- trainControl(method = "cv", number = 5)

# Prepare recipe-like data frames for caret (drop NAs in used vars)
df_vol <- merged %>%
  select(log_FACE_VALUE_AMOUNT, log_TOTAL_ACRES_IRRIGATED, LATITUDE, LONGITUDE, WATER_RIGHT_TYPE.x, COUNTY.x) %>%
  na.omit()

df_int <- merged %>%
  select(log_irrigation_intensity, LATITUDE, LONGITUDE, WATER_RIGHT_TYPE.x, COUNTY.x) %>%
  na.omit()

fit_cv_vol <- train(
  log_FACE_VALUE_AMOUNT ~ log_TOTAL_ACRES_IRRIGATED + LATITUDE + LONGITUDE + WATER_RIGHT_TYPE.x + COUNTY.x,
  data = df_vol,
  method = "lm",
  trControl = ctrl,
  metric = "RMSE"
)

fit_cv_int <- train(
  log_irrigation_intensity ~ LATITUDE + LONGITUDE + WATER_RIGHT_TYPE.x + COUNTY.x,
  data = df_int,
  method = "lm",
  trControl = ctrl,
  metric = "RMSE"
)

cv_summary <- tibble(
  Model = c("Log Volume", "Log Intensity"),
  RMSE_log = c(fit_cv_vol$results$RMSE[1], fit_cv_int$results$RMSE[1]),
  MAE_log  = c(fit_cv_vol$results$MAE[1],  fit_cv_int$results$MAE[1])
)

# Back-transform (approximate) to original scale using expm1 of residual metrics
# Note: RMSE on log scale doesn’t map linearly; this is just a rough sense.
cv_summary <- cv_summary %>%
  mutate(
    RMSE_backtransform_approx = expm1(RMSE_log),
    MAE_backtransform_approx  = expm1(MAE_log)
  )

write_csv(cv_summary, "paper/tables/cv_results.csv")
print(cv_summary)

cat("\nSaved:\n- paper/tables/model_coefficients_robustHC.csv\n- paper/tables/vif_*.csv\n- paper/figures/diag_*.png\n- paper/tables/cv_results.csv\n")
