################################################################################
#Employment growth (outcome) EDA
################################################################################


#Table3: Employment growth summarised by local authority
table3_eda <- growth_groups%>%
  select(growth_group, local_authority, mean_growth, median_growth, sd_growth)

print(table3_eda)
write_csv(table3_eda, file = "outputs/tables/table3_eda.csv")

#Average growth per group per year
growth_group_year <- data_eda%>%
  group_by(growth_group, year)%>%
  summarise(mean_growth_group = mean(emp_growth_pct, na.rm = TRUE),
            sd_growth_group = sd(emp_growth_pct, na.rm = TRUE),
            n_obs = n(),
            .groups = "drop")%>%
  mutate(se = sd_growth_group / sqrt(n_obs))

#Figure 1: Yorkshire and The Humber LA's Average Employment Growth Trajectories by Employment Growth Group
eda_growth_plot <- growth_group_year%>% 
  filter(!year %in% 2015)%>%
  ggplot(aes(x = year, y = mean_growth_group, colour = growth_group, fill = growth_group))+
  geom_line(linewidth = 1.1)+
  geom_ribbon(aes(ymin = mean_growth_group - se,
                  ymax = mean_growth_group + se),
              alpha = 0.2, colour = NA)+
  scale_colour_manual(values = growth_group_cols)+
  labs(title = "Yorkshire and The Humber LA’s Average Employment Growth Trajectories by Employment Growth Group", 
       subtitle = "Shaded Bands = ±1 Standard Error",
       x = "Year", y = "Employment Growth (%)", fill = NULL, colour = "Employment Growth Group",
       caption = "Business Register and Employment Survey (2015-2023).")+
  guides(fill = "none")+
  theme_minimal()+
  theme(title = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "bottom",
        legend.direction = "horizontal")

print(eda_growth_plot)
ggsave("figure1_eda.png", plot = eda_growth_plot, path = "outputs/figures",
       height = 541, width = 865, units = "px", dpi = 96)


################################################################################
#Digital connectivity EDA
################################################################################


# LA Digital indicator summary statistics
la_dig_summary <- data_eda %>%
  group_by(local_authority) %>%
  summarise(mean_sfbb = mean(sfbb_availability_pct, na.rm = TRUE),
            median_sfbb = median(sfbb_availability_pct, na.rm = TRUE),
            mean_10mbit = mean(unable_to_recieve_10mbits_pct, na.rm = TRUE),
            median_10mbit = median(unable_to_recieve_10mbits_pct, na.rm = TRUE),
            sd_sfbb = sd(sfbb_availability_pct, na.rm = TRUE),
            sd_10mbit = sd(unable_to_recieve_10mbits_pct, na.rm = TRUE))

print(la_dig_summary)

# Figure 2: Independent Distributions (Faceted Histograms)
eda_dig_dist_plot <- data_eda %>%
  pivot_longer(cols = c(sfbb_availability_pct, unable_to_recieve_10mbits_pct),
               names_to = "indicator",
               values_to = "percentage") %>%
  # Cleaning up the names for the facet labels
  mutate(indicator = factor(indicator, 
                            levels = c("sfbb_availability_pct", "unable_to_recieve_10mbits_pct"),
                            labels = c("SFBB Availability (%)", "Unable to Receive 10 Mbit/s (%)"))) %>%
  ggplot(aes(x = percentage, fill = indicator)) +
  geom_histogram(bins = 30, color = "white", alpha = 0.8) +
  facet_wrap(~ indicator, scales = "free") + 
  scale_fill_manual(values = c("SFBB Availability (%)" = "#00BFC4", 
                               "Unable to Receive 10 Mbit/s (%)" = "#F8766D")) +
  labs(title = "Distributions of Local Digital Connectivity", 
       subtitle = "Evaluating the spread of high-speed availability and poor connectivity",
       x = "Percentage of Area", y = "Count (Local Authorities)",
       caption = "Ofcom Connected Nations (2015-2023)") +
  theme_minimal() +
  theme(title = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "none",
        strip.text = element_text(face = "bold", size = 10))

print(eda_dig_dist_plot)
ggsave("figure2_eda.png", plot = eda_dig_dist_plot, path = "outputs/figures",
       height = 541, width = 865, units = "px", dpi = 96)


# Figure 3: The Internal Digital Divide (Scatter Plot)
eda_dig_cor_plot <- data_eda %>%
  ggplot(aes(x = sfbb_availability_pct, y = unable_to_recieve_10mbits_pct)) +
  geom_point(color = "#00BFC4", alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", color = "#F8766D", se = FALSE, linetype = "dashed", linewidth = 1.2) +
  labs(title = "SFBB Availability vs. Poor Connectivity", 
       subtitle = "Does high availability guarantee fewer 'not-spots'?",
       x = "SFBB Availability (%)", 
       y = "Unable to Receive 10 Mbit/s (%)",
       caption = "Ofcom Connected Nations (2015-2023)") +
  theme_minimal() +
  theme(title = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

print(eda_dig_cor_plot)
ggsave("figure3_eda.png", plot = eda_dig_cor_plot, path = "outputs/figures",
       height = 541, width = 865, units = "px", dpi = 96)


################################################################################
#Industry composition EDA
################################################################################


#Mean industry share per LA across years
mean_industry_share_la <- industry_shares%>%
  group_by(local_authority, industry)%>%
  summarise(mean_share_la = mean(ind_share))%>%
  left_join(select(growth_groups, growth_group, local_authority), by = "local_authority")

#Computing average industry share by growth group
mean_industry_share_group <- mean_industry_share_la%>%
  group_by(growth_group, industry)%>%
  summarise(mean_share_group = mean(mean_share_la))

#Figure 4: Mean Industry Share (High Vs Low Growth LAs)
eda_group_industry_share_plot <- mean_industry_share_group%>%
  filter(growth_group %in% c("High Growth", "Low Growth"))%>%
  ggplot(aes(x = reorder(industry, mean_share_group), y = mean_share_group, fill = growth_group))+
  geom_col(position = "dodge", width = 0.7)+
  scale_fill_manual(values = growth_group_cols)+
  coord_flip()+
  labs(title = "Mean Industry Share (High Vs Low Growth LAs)", 
       x = "Industry", y = "Mean Industry Share (Proportion)",
       fill = "Growth group",
       caption = "Business Register and Employment Survey (2015-2023)")+
  theme_minimal()+
  theme(title = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "top",
        legend.direction = "horizontal")

print(eda_group_industry_share_plot)
ggsave("figure4_eda.png", plot = eda_group_industry_share_plot, path = "outputs/figures",
       height = 541, width = 865, units = "px", dpi = 96)

#HHI summary statistics by local authority
la_hhi_summary <- data_eda%>%
  group_by(local_authority)%>%
  summarise(mean_hhi = mean(hhi),
            median_hhi = median(hhi),
            sd_hhi = sd(hhi))

print(la_hhi_summary)


################################################################################
#Educational data EDA
################################################################################


#LA-level (mean) trends of level 2 & level 3 attainment level percentages over time
la_attainment_summary <- data_eda%>%
  group_by(local_authority)%>%
  summarise(mean_level_2 = mean(level_2_attainment_pct),
            mean_level_3 = mean(level_3_attainment_pct),
            median_level_2 = median(level_2_attainment_pct),
            median_level_3 = median(level_3_attainment_pct),
            sd_level_2 = sd(level_2_attainment_pct),
            sd_level_3 = sd(level_3_attainment_pct))

print(la_attainment_summary)

#Yearly Regional-level (mean) trends of level 2 & level 3 attainment level percentages over time
year_attainment_summary <- data_eda%>%
  group_by(year)%>%
  summarise(mean_level_2 = mean(level_2_attainment_pct),
            mean_level_3 = mean(level_3_attainment_pct),
            median_level_2 = median(level_2_attainment_pct),
            median_level_3 = median(level_3_attainment_pct),
            sd_level_2 = sd(level_2_attainment_pct),
            sd_level_3 = sd(level_3_attainment_pct))


#Figure 5 Mean Level 2 & Level 3 Attainment Over Time (Regional-Level)
eda_attainment_summary_plot <- year_attainment_summary%>%
  pivot_longer(cols = c(mean_level_2, mean_level_3), # need to rename these
               names_to = "level",
               values_to = "attainment_pct")%>%
  ggplot(aes(x = year, y = attainment_pct, colour = level))+
  geom_line(linewidth = 1.2)+
  geom_point()+
  scale_colour_manual(values = c("#F8766D", "#00BFC4"),
                      labels = c("mean_level_2" = "Level 2 Attainment",
                                 "mean_level_3" = "Level 3 Attainment"))+
  labs(title = "Mean Level 2 & Level 3 Educational Attainment Over Time", 
       x = "Year", y = "Attainment (%)",
       colour = "",
       caption = "Department for Education (2015-2023)")+ # need to make sure correct source
  theme_minimal()+
  theme(title = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "bottom",
        legend.direction = "horizontal")

print(eda_attainment_summary_plot)
ggsave("figure5_eda.png", plot = eda_attainment_summary_plot, path = "outputs/figures",
       height = 541, width = 865, units = "px", dpi = 96)


################################################################################
#Business demography EDA
################################################################################


#Business demography summary statistics by local authority
la_demography_summary <- data_eda%>%
  group_by(local_authority)%>%
  summarise(sum_births = sum(births),
            sum_deaths = sum(deaths),
            sum_net_business = sum(net_business))

#Yearly regional net business change over time
year_demography_summary <- data_eda%>%
  group_by(year)%>%
  summarise(sum_births = sum(births),
            sum_deaths = sum(deaths),
            sum_net_business = sum(net_business))

#Assigning colours to demography groups
demography_group_cols <- c("sum_births" = "#59A14F",
                           "sum_deaths" = "#E15759",
                           "sum_net_business" = "#4E78A7")

#Figure 6: Regional Business Births, Deaths, and Net Change Over Time
eda_demography_summary_plot <- year_demography_summary%>%
  ggplot(aes(x = year))+
  geom_line(aes(y = sum_births, colour = "Births"), linewidth = 1.2)+
  geom_line(aes(y = sum_deaths, colour = "Deaths"), linewidth = 1.2)+
  geom_line(aes(y = sum_net_business, colour = "Net Business"), linewidth = 1.2)+
  scale_colour_manual(values = c("#59A14F", "#E15759", "#4E78A7"))+
  labs(title = "Regional Business Births, Deaths, and Net Change Over Time", 
       x = "Year", y = "Count",
       colour = "",
       caption = "ONS Business Demography (2015-2023)")+
  theme_minimal()+
  theme(title = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "bottom",
        legend.direction = "horizontal")

print(eda_demography_summary_plot)
ggsave("figure6_eda.png", plot = eda_demography_summary_plot, path = "outputs/figures",
       height = 541, width = 865, units = "px", dpi = 96)


################################################################################
#Printing LA summaries
################################################################################


la_summaries <- growth_groups%>%
  left_join(la_dig_summary, by = "local_authority")%>%
  left_join(la_hhi_summary, by = "local_authority")%>%
  left_join(la_attainment_summary, by = "local_authority")%>%
  left_join(la_demography_summary, by = "local_authority")

print(la_summaries)
