################################################################################
#Linear Regression Model Building
################################################################################


# Base model (without digital indicators)
base_lm <- lm(emp_growth_pct ~ ., data = base_train)

# Digital model (with digital indicators)
dig_lm <- lm(emp_growth_pct ~ ., data = dig_train)

# Save the models (optional, keeping your workflow)
saveRDS(base_lm, file = "outputs/models/base_lm.rds")
saveRDS(dig_lm, file = "outputs/models/dig_lm.rds")


################################################################################
#Evaluating Linear Performance
################################################################################


# Predictions
base_predict_lm <- predict(base_lm, newdata = base_test)
dig_predict_lm <- predict(dig_lm, newdata = dig_test)

# MSE
mse_base_lm <- mean((base_true - base_predict_lm)^2)
mse_dig_lm <- mean((dig_true - dig_predict_lm)^2)

# MAE
mae_base_lm <- mean(abs(base_true - base_predict_lm))
mae_dig_lm <- mean(abs(dig_true - dig_predict_lm))

# Test Set R-squared (using your previous Pseudo R2 formula for consistency)
pseudo_r2_base_lm <- 1 - sum((base_true - base_predict_lm)^2) / sum((base_true - mean(base_true))^2)
pseudo_r2_dig_lm <- 1 - sum((dig_true - dig_predict_lm)^2) / sum((dig_true - mean(dig_true))^2)

# Printing performance metrics
cat("Base Linear Model:\n", "MSE:", round(mse_base_lm, 3), "MAE:", round(mae_base_lm, 2),
    "Test R2:", round(pseudo_r2_base_lm, 3), "\n")
cat("Digital Linear Model:\n", "MSE:", round(mse_dig_lm, 3), "MAE:", round(mae_dig_lm, 2),
    "Test R2:", round(pseudo_r2_dig_lm, 3), "\n")

#Internal training summaries
summary(base_lm)
summary(dig_lm)


################################################################################
#Saving model outputs
################################################################################


#Base lm model
base_results_lm <- data.frame(Actual = base_true, Predicted = base_predict_lm, 
                           MSE = mse_base_lm, MAE = mae_base_lm, Pseudo_r2 = pseudo_r2_base_lm)
write_csv(base_results_lm, file = "outputs/models/base_lm_results.csv")

capture.output(summary(base_lm), file = "outputs/models/base_lm_summary.txt")

#Digital lm model
dig_results_lm <- data.frame(Actual = dig_true, Predicted = dig_predict_lm,
                          MSE = mse_dig_lm, MAE = mae_dig_lm, Pseudo_r2 = pseudo_r2_dig_lm)
write_csv(dig_results_lm, file = "outputs/models/digital_lm_results.csv")

capture.output(summary(dig_lm), file = "outputs/models/digital_lm_summary.txt")
