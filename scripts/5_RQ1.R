################################################################################
#RQ1 Part 1: Digital Connectivity
################################################################################

#Figure 7: Yorkshire and The Humber Digital Connectivity by Employment Growth Groups
rq1_digital_plot <- data_eda %>%
  group_by(growth_group, year) %>%
  summarise(mean_sfbb = mean(sfbb_availability_pct, na.rm = TRUE),
            mean_10mbit = mean(unable_to_recieve_10mbits_pct, na.rm = TRUE),
            .groups = 'drop') %>%
  rename("SFBB Availability (%)" = mean_sfbb,
         "Unable to Receive 10 Mbit/s (%)" = mean_10mbit) %>%
  pivot_longer(cols = c("SFBB Availability (%)", "Unable to Receive 10 Mbit/s (%)"),
               names_to = "digital_metric",
               values_to = "value") %>%
  ggplot(aes(x = year, y = value, colour = factor(growth_group, levels = c("Low Growth",
                                                                           "Medium Growth",
                                                                           "High Growth")))) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = growth_group_cols) + # Uses your existing palette
  facet_wrap(~digital_metric, scales = "free_y") +
  labs(title = "Yorkshire and The Humber Digital Connectivity by Employment Growth Groups (2015-2023)", 
       x = "Year", y = "Percentage of Area (%)",
       colour = "Employment Growth Group",
       caption = "Business Register and Employment Survey (2015-2023); Ofcom Connected Nations (2015-2023).") +
  theme_minimal() +
  theme(title = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "bottom")

print(rq1_digital_plot)
ggsave("figure7_rq1.png", plot = rq1_digital_plot, path = "outputs/figures",
       height = 541, width = 865, units = "px", dpi = 96)


################################################################################
#RQ1 Part 2: Industrial Composition
################################################################################

#Figure 8: Yorkshire and The Humber LA's Industrial Composition by Employment Growth Groups
rq1_hhi_plot <- data_eda%>%
  group_by(local_authority)%>%
  mutate(mean_hhi = mean(hhi, na.rm = TRUE))%>%
  ggplot(aes(x = growth_group, y = mean_hhi, fill = factor(growth_group, levels = c("Low Growth",
                                                                                    "Medium Growth",
                                                                                    "High Growth"))))+
  geom_boxplot(position = "dodge", width = 0.7)+
  scale_fill_manual(values = growth_group_cols)+
  labs(title = "Yorkshire and The Humber LA’s Industrial Composition by Employment Growth Groups", 
       x = "Employment Growth Group",y = "Mean HHI (2015-2023)", fill = NULL,
       caption = "Business Register and Employment Survey (2015-2023).")+
  guides(fill = "none")+
  theme_minimal()+
  theme(title = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

print(rq1_hhi_plot)
ggsave("figure8_rq1.png", plot = rq1_hhi_plot, path = "outputs/figures",
       height = 541, width = 865, units = "px", dpi = 96)


################################################################################
#rq1 Part 3: Education
################################################################################


#Figure 9: Yorkshire and The Humber's Educational Attainment by Employment Growth Groups (2015-2023)
rq1_education_plot <- data_eda%>%
  group_by(growth_group, year)%>%
  summarise(level_2_attainment_pct = mean(level_2_attainment_pct, na.rm = TRUE),
            level_3_attainment_pct = mean(level_3_attainment_pct, na.rm = TRUE))%>%
  rename("Level 2" = level_2_attainment_pct,
         "Level 3" = level_3_attainment_pct)%>%
  pivot_longer(cols = c("Level 2", "Level 3"),
               names_to = "attainment_level",
               values_to = "value")%>%
  ggplot(aes(x = year, y = value, colour = factor(growth_group, levels = c("Low Growth",
                                                                           "Medium Growth",
                                                                           "High Growth") )))+
  geom_line(linewidth = 1.2)+
  scale_colour_manual(values = growth_group_cols)+
  facet_wrap(~attainment_level, scales = "free_y")+
  labs(title = "Yorkshire and The Humber’s Educational Attainment by Employment Growth Groups (2015-2023)", 
       x = "Year", y = "Attainment (%)", colour = "Growth Group",
       caption = "Department for Education (2015-2023); Business Register and Employment Survey (2015-2023).")+
  theme_minimal()+
  theme(title = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "bottom")
  

print(rq1_education_plot)
ggsave("figure9_rq1.png", plot = rq1_education_plot, path = "outputs/figures",
       height = 541, width = 865, units = "px", dpi = 96)

################################################################################
#EDA Part 4: Business Demography
################################################################################

#Figure 10: Yorkshire and The Humber Net Business by Derived Growth Groups (2015-2023)
rq1_net_business_plot <- demography_net%>%#--------------this one is better
  group_by(year, growth_group)%>%
  ggplot(aes(x = year, y = net_business, colour = factor(growth_group, levels = c("Low Growth",
                                                                         "Medium Growth",
                                                                         "High Growth"))))+
  geom_point()+
  geom_smooth(se = FALSE, linewidth = 1.2)+
  scale_colour_manual(values = growth_group_cols)+
  labs(title = "Yorkshire and The Humber’s Net Business by Employment Growth Groups (2015-2023)", 
       x = "Year",
       y = "Net Business Change",
       colour = "Growth Group",
       caption = "Business Register and Employment Survey (2015-2023); ONS Business Demograpy (2015-2023)")+
  theme_minimal()+
  theme(title = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "bottom",
        legend.direction = "horizontal")

print(rq1_net_business_plot)
ggsave("figure10_rq1.png", plot = rq1_net_business_plot, path = "outputs/figures",
       height = 541, width = 865, units = "px", dpi = 96)
