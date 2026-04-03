################################################################################
#Removing redundant variables for modelling
################################################################################


data_model <- data_eda%>%
  filter(!year == 2015)%>%
  select(-c(agriculture_forestry_fishing:arts_entertainment_recreation_other))


################################################################################
# 70/30 Train/test splitting (Randomised spatial split) 
################################################################################


set.seed(123456)

#Extracting unique list of LAs
unique_las <- unique(data_model$local_authority)

train_las <- sample(unique_las, size = 0.7 * length(unique_las))

train_data <- data_model%>%
  filter(local_authority %in% train_las)

test_data <- data_model%>%
  filter(!(local_authority %in% train_las))


################################################################################
#Principal Component Analysis (Fitted on TRAIN only)
################################################################################


#Defining share columns
industry_cols <- c("accomodation_food_share","agriculture_forestry_fishing_share", "arts_entertainment_recreation_other_share",
                   "business_admin_support_services_share","construction_share", "education_share", "finance_insurance_share", "health_share",
                   "information_communication_share", "manufacturing_share", "mining_quarrying_utilities_share", "motor_trades_share", "professional_scientific_technical_share",
                   "property_share", "public_admin_defence_share", "retail_share", "transport_storage_postal_share", "wholesale_share", "net_business"
)

#isolating industry shares
train_shares <- train_data%>%
  select(all_of(industry_cols))

test_shares <- test_data%>%
  select(all_of(industry_cols))

#Fitting PCA on training data
pca_shares <- prcomp(train_shares, center = TRUE, scale. = TRUE)

#Assessing component importance
summary(pca_shares)

#Extracting top 7 components (based off summary interpretation)
train_pcs <- as.data.frame(pca_shares$x[, 1:7])

#Transforming test data using training PCA model
test_pcs <- as.data.frame(predict(pca_shares, newdata = test_shares)[, 1:7])


################################################################################
#Re-merging datasets
################################################################################


#Removing non-predictive correlative, and redundant variables
train_rf <- train_data%>%
  select(-year, -local_authority, -births, -deaths, -total_emp, -growth_group,
         -all_of(industry_cols))%>%
  bind_cols(train_pcs)

test_rf <- test_data%>%
  select(-year, -local_authority, -births, -deaths, -total_emp, -growth_group,
         -all_of(industry_cols))%>%
  bind_cols(test_pcs)


################################################################################
#Building bootstrapped random forest model on training data
################################################################################


B <- 500 #Number of bootstraps


#Setting variable names
var_names <-setdiff(names(train_rf), "emp_growth_pct")
#Creating feature importance matrix
feat_import_matrix <- matrix(NA, nrow = B, ncol=length(var_names), dimnames = list(NULL, var_names))

for(b in 1:B){
  
  #Bootstrap sample
  idx <- sample(seq_len(nrow(train_rf)), replace = TRUE)
  boot_rf <- train_rf[idx, ]
  
  rf <- randomForest(emp_growth_pct ~ .,
                     data = boot_rf,
                     importance = TRUE)
  
  feat_import_matrix[b, ] <- importance(rf)[, "%IncMSE"]

}


################################################################################
#Summarising initial feature importance test
################################################################################


feat_importance_summary <- data.frame(
  variable = var_names,
  mean_importance = colMeans(feat_import_matrix, na.rm= TRUE),
  pct_positive = colMeans(feat_import_matrix > 0, na.rm = TRUE))%>% #Values that had positive importance only
  arrange(desc(mean_importance))

summary(feat_importance_summary)
print(feat_importance_summary)

#Figure 11: Plotting Importance Rankings of Structural Indicators on training data
feat_importance_summary <- feat_importance_summary%>%
  mutate(overall_mean = mean(mean_importance))

feat_importance_plot <- ggplot(feat_importance_summary, aes(x = reorder(variable, mean_importance), y = mean_importance))+
  geom_point()+
  geom_hline(aes(yintercept = overall_mean, linetype = "Mean Importance"))+
  labs(title = "Importance Rankings of Structural Indicators", 
       x = "Indicator", y = "Importance Ranking",
       linetype = "")+
  scale_linetype_manual(values = c("Mean Importance" = "dashed"))+
  theme_minimal()+
  theme(title = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, size = 8), 
        legend.position = "top")

print(feat_importance_plot)
ggsave("figure11_rq2.png", plot = feat_importance_plot, path = "outputs/figures",
       height = 541, width = 865, units = "px", dpi = 96)
