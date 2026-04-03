################################################################################
#Random forest model building
################################################################################


#Base model (without digital indicators)
base_train <- train_rf%>%
  select(-c(sfbb_availability_pct, unable_to_recieve_10mbits_pct))

base_test <- test_rf%>%
  select(-c(sfbb_availability_pct, unable_to_recieve_10mbits_pct))


base_rf <- randomForest(formula = emp_growth_pct ~.,
                         data = base_train,
                         importance = TRUE)

#Digital model (with digital indicators)
dig_train <- train_rf
dig_test <- test_rf

dig_rf <- randomForest(formula = emp_growth_pct ~.,
                        data = dig_train,
                        importance = TRUE)


saveRDS(base_rf, file = "outputs/models/base_rf.rds")
saveRDS(dig_rf, file = "outputs/models/dig_rf.rds")


################################################################################
#Evaluating performance
################################################################################


base_true <- base_test$emp_growth_pct
dig_true <- dig_test$emp_growth_pct
base_predict <- predict(base_rf, newdata = base_test)
dig_predict <- predict(dig_rf, newdata = dig_test)

#MSE
mse_base <- mean((base_test$emp_growth_pct - base_predict)^2)
mse_dig <- mean((dig_test$emp_growth_pct - dig_predict)^2)

#MAE
mae_base <- mean(abs(base_true - base_predict))
mae_dig <- mean(abs(dig_true - dig_predict))

#pseudo-r2
pseudo_r2_base <- 1 - sum((base_true - base_predict)^2) / sum((base_true - mean(base_true))^2)
pseudo_r2_dig <- 1 - sum((dig_true - dig_predict)^2) / sum((dig_true - mean(dig_true))^2)


#Printing performance metrics
cat("Base Model:\n", "MSE:",round(mse_base, 3), "MAE:", round(mae_base, 2),
    "Pseudo R2:", round(pseudo_r2_base, 3))
cat("Digital model:\n", "MSE:", round(mse_dig, 3), "MAE:", round(mae_dig, 2),
    "Pseudo R2:", round(pseudo_r2_dig, 3))

#Internal training summaries
summary(base_rf)

summary(dig_rf)


################################################################################
#Saving model outputs
################################################################################


#Base rf model
base_results_rf <- data.frame(Actual = base_true, Predicted = base_predict, 
                           MSE = mse_base, MAE = mae_base, Pseudo_r2 = pseudo_r2_base)
write_csv(base_results_rf, file = "outputs/models/base_rf_results.csv")

capture.output(summary(base_rf), file = "outputs/models/base_rf_summary.txt")

#Digital rf model
dig_results_rf <- data.frame(Actual = dig_true, Predicted = dig_predict, 
                           MSE = mse_dig, MAE = mae_dig, Pseudo_r2 = pseudo_r2_dig)
write_csv(dig_results_rf, file = "outputs/models/digital_rf_results.csv")

capture.output(summary(dig_rf), file = "outputs/models/digital_rf_summary.txt")
