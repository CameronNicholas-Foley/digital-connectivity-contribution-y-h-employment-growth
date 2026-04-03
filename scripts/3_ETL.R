################################################################################
#Data Cleaning
################################################################################


#-------------------------------------------------------------------------------
#Business Regional Employment Survey
#-------------------------------------------------------------------------------

#Loading data set 
bres_raw <- read_excel("raw_data/bres/nomis_2026_01_07_102048.xlsx", col_names = FALSE)

#Slicing each table
bres_2015 <- bres_raw %>% slice(8:27)
bres_2016 <- bres_raw %>% slice(39:58)
bres_2017 <- bres_raw %>% slice(70:89)
bres_2018 <- bres_raw %>% slice(101:120)
bres_2019 <- bres_raw %>% slice(132:151)
bres_2020 <- bres_raw %>% slice(163:182)
bres_2021 <- bres_raw %>% slice(194:213)
bres_2022 <- bres_raw %>% slice(225:244)
bres_2023 <- bres_raw %>% slice(256:275)

#Combining all the sliced tables as dataframes in a list
bres_list <- list(
  "2015" = bres_2015, 
  "2016" = bres_2016, 
  "2017" = bres_2017, 
  "2018" = bres_2018, 
  "2019" = bres_2019,
  "2020" = bres_2020, 
  "2021" = bres_2021, 
  "2022" = bres_2022, 
  "2023" = bres_2023)

#Mapping bres_reformatter to each tibble in bres_list
bres_clean <- suppressWarnings(map2(bres_list, names(bres_list), bres_reformatter))

#Combing tibbles in bres_clean with year as a column
bres_comb <- bind_rows(bres_clean, .id = "year")%>%
  mutate(year = as.integer(year))

bres_comb <- bres_comb%>% #Pivoting industry column wider to prevent duplicate values when merging
  pivot_wider(
    names_from = industry,
    values_from = employee_count
  )

#Calculating how many NAs for each variables
bres_comb%>%
  summarise(across(everything(), ~sum(is.na(.)))) #No NAs

#-------------------------------------------------------------------------------
#OFCOM Connected Nations
#-------------------------------------------------------------------------------

#Loading Connected Nations data sets
ofcom_2015 <- read_csv("raw_data/ofcom/Fixed_La_updated_2016.csv")
ofcom_2016 <- read_csv("raw_data/ofcom/2016_fixed_laua_r01.csv")
ofcom_2017 <- read_csv("raw_data/ofcom/fixed_la_2017.csv")
ofcom_2018 <- read_csv("raw_data/ofcom/201809_fixed_laua_coverage_r01.csv")
ofcom_2019 <- read_csv("raw_data/ofcom/201909_fixed_laua_coverage_r01.csv")
ofcom_2020 <- read_csv("raw_data/ofcom/202009_fixed_laua_coverage_r01.csv")
ofcom_2021 <- read_csv("raw_data/ofcom/202109_fixed_laua_coverage_r01.csv")
ofcom_2022 <- read_csv("raw_data/ofcom/202209_fixed_laua_coverage_r02.csv")
ofcom_2023 <- read_csv("raw_data/ofcom/202309_fixed_laua_coverage_r01.csv")

#combining all dataframes into list
ofcom_list <- list(
  "2015" = ofcom_2015,
  "2016" = ofcom_2016,
  "2017" = ofcom_2017,
  "2018" = ofcom_2018,
  "2019" = ofcom_2019,
  "2020" = ofcom_2020,
  "2021" = ofcom_2021,
  "2022" = ofcom_2022,
  "2023" = ofcom_2023
)

#Mutating column names from Ofcom 2015 data frame to match others
ofcom_list$`2015` <- ofcom_list$`2015`%>%
  rename("laua_name" = "Local Authority Name",
         "SFBB Availability (% premises)" = "SFBB Availability (% premises) by Local Authority",
  )

#Mutating relevant LA names in Ofcom 2015 data frame to match others 
ofcom_list$`2015` <- ofcom_list$`2015`%>%
  mutate(laua_name = la_normaliser(laua_name))
    
ofcom_clean <- map(ofcom_list, ofcom_reformatter) #Mapping ofcom function to ofcom list of data frames

#Binding ofcom_list into one dataframe with year as the ID
ofcom_comb <- bind_rows(ofcom_clean, .id = "year")%>%
  mutate(year = as.integer(year))

#Calculating how many NAs for each variables
ofcom_comb%>%
  summarise(across(everything(), ~sum(is.na(.)))) #No NAs

#-------------------------------------------------------------------------------
#Department of Education Level 2 & Level 3 Attainment Levels Ages 16-19
#-------------------------------------------------------------------------------

#Loading data set (Original file had to be trimmed by year and region)
dfe_raw <- read_excel("raw_data/dfe/level_2_3_la_figures_trimmed.xlsx")

#Converting la_name to lowercase before using yorkshire_las as filter
dfe_raw <- dfe_raw%>%
  mutate(la_name = la_normaliser(la_name))

#Filtering raw data to required time_period, geographic_level, and columns
dfe_filtered <- dfe_raw%>%
  filter(geographic_level == "Local authority",
    la_name %in% yorkshire_las)%>%
  select(time_period, la_name, qualification_level, percentage)

#Calculating how many NAs for each variables
dfe_filtered%>%
  summarise(across(everything(), ~sum(is.na(.)))) #No NAs

#Converting percentage variable to correct data type
dfe_clean <- dfe_filtered%>%
  mutate(percentage = as.numeric(percentage))

#Aggregating percentages of same time_period, la_name, and qualification_level to singular value
dfe_clean <- dfe_clean%>%
  group_by(time_period, la_name, qualification_level)%>%
  summarise(percentage = mean(percentage), .groups = "drop")

#Pivoting data frame wider to convert qualification_level categories to variables
dfe_clean <- dfe_clean%>%
  pivot_wider(
    names_from = qualification_level,
    values_from = percentage
  )

#Renaming columns for consistency with other datasets
dfe_clean <- dfe_clean%>%
  rename("academic_year" = "time_period",
         "local_authority" = "la_name",
         "level_2_attainment_pct" = "Level 2",
         "level_3_attainment_pct" = "Level 3"
  )

#Converting academic year to calendar to match other data sets (extracting latter year as exam results are included)
dfe_clean <- dfe_clean%>%
  mutate(year = as.integer(substr(academic_year, 5, 6)) + 2000)

dfe_clean <- dfe_clean%>%
  select(year, local_authority, level_2_attainment_pct, level_3_attainment_pct)
  
#-------------------------------------------------------------------------------
#Business Demography
#-------------------------------------------------------------------------------

#String of required tables for extraction
bd_2020_sheets = c("Table 1.1a", "Table 2.1a")
bd_2023_sheets = c("Table 1.1b", "Table 1.1c", "Table 1.1d", "Table 2.1b", "Table 2.1c", "Table 2.1d")

#Extracting required tables and loading data
bd_2020_raw <- bd_2020_sheets%>%
  set_names()%>%
  map(~ read_excel("raw_data/business_demog/business_demog2020.xlsx", sheet = .x))

bd_2023_raw <- bd_2023_sheets%>%
  set_names()%>%
  map(~ read_excel("raw_data/business_demog/business_demog2023.xlsx", sheet = .x))

#Renaming tables
names(bd_2020_raw) <- c("births_2015-2018","deaths_2015-2018")
names(bd_2023_raw) <- c("births_2019", "births_2020", "births_2021-2023", "deaths_2019", "deaths_2020", "deaths_2021-2023")

#Mapping bd_2020_reformatter to tables in bd_2020_raw
bd_2020_clean <- map2(bd_2020_raw, names(bd_2020_raw), bd_2020_reformatter)

#Mapping bd_2023_reformatter function to bd_2023_raw
bd_2023_clean <- map2(bd_2023_raw, names(bd_2023_raw), bd_2023_reformatter)

#Combining bd_2020_clean into one data set
bd_2020_comb <- reduce(bd_2020_clean, full_join, by = c("local_authority", "year"))

#Binding rows by births and deaths separately 
bd_2023_births <- bind_rows(
  bd_2023_clean$births_2019,
  bd_2023_clean$births_2020,
  bd_2023_clean$`births_2021-2023`
)

bd_2023_deaths <- bind_rows(
  bd_2023_clean$deaths_2019,
  bd_2023_clean$deaths_2020,
  bd_2023_clean$`deaths_2021-2023`
)

#combining all tables from 2023
bd_2023_comb <- left_join(bd_2023_births, bd_2023_deaths, by = c("local_authority","year"))

#Binding bd data
bd_comb <- bind_rows(
  bd_2020_comb,
  bd_2023_comb)


################################################################################
#Data Integration
################################################################################


data_list <- list(
  bres = bres_comb,
  ofcom = ofcom_comb,
  dfe = dfe_clean,
  bd = bd_comb
)

data_processed <- reduce(data_list, full_join, by = c("local_authority", "year"))


#Checking NAs
sum(is.na(data_processed))

View(data_processed)
write.csv(data_processed, file = "outputs/tables/data_processed.csv")


################################################################################
#Data Transformations
################################################################################


#Computing annual employment growth (%)
data_growth <- data_processed%>%
  arrange(local_authority, year)%>%
  group_by(local_authority)%>%
  mutate(total_emp = rowSums(across(agriculture_forestry_fishing:arts_entertainment_recreation_other)),
         emp_growth_pct = (total_emp - lag(total_emp)) / lag(total_emp) * 100)%>%
  ungroup()

#Summarises overall employment growth per LA (to compute growth groups)
la_growth_summary <- data_growth%>%
  group_by(local_authority)%>%
  summarise(mean_growth = mean(emp_growth_pct, na.rm = TRUE),
            median_growth = median(emp_growth_pct, na.rm = TRUE),
            sd_growth = sd(emp_growth_pct, na.rm = TRUE))%>%
  arrange(desc(mean_growth))

print(la_growth_summary)

growth_groups <- la_growth_summary%>%
  mutate(growth_group = ntile(mean_growth, 3))%>%
  mutate(growth_group = case_when(growth_group == 1 ~ "Low Growth",
                                  growth_group == 2 ~ "Medium Growth",
                                  growth_group == 3 ~ "High Growth"))

#Assigning colours to growth groups for future visualisations
growth_group_cols <- c("Low Growth" = "#E15759", 
                       "Medium Growth" = "#4E78A7",
                       "High Growth" = "#59A14F")

#Joining groups to main df data set for RQ1
data_eda <- data_growth%>%
  left_join(select(growth_groups, local_authority, growth_group), by = "local_authority")

#Pivoting industry data longer and computing industry shares per LA
industry_shares <- data_eda%>%
  pivot_longer(cols = c(agriculture_forestry_fishing:arts_entertainment_recreation_other),
               names_to = "industry",
               values_to = "employee_count")%>%
  group_by(local_authority, year, industry)%>%
  summarise(ind_share = employee_count / total_emp)%>%
  ungroup()

#Checking shares add to 1
industry_shares%>%
  group_by(local_authority, year)%>%
  summarise(ind_share = sum(ind_share, na.rm = TRUE))

#Computing mean HHI across years, comparing with mean growth and growth group
hhi_la <- industry_shares%>%
  group_by(local_authority, year)%>%
  summarise(hhi = sum((ind_share)^2))%>%
  arrange(year)

#Joining HHI to main df
data_eda <- data_eda%>%
  left_join(select(hhi_la, hhi, local_authority, year), by = c("local_authority", "year"))

#Pivoting industry shares back to wide and joining to main df as seperate cols
industry_shares_wide <- industry_shares%>%
  pivot_wider(names_from = industry,
              values_from = ind_share)%>%
  rename_with(~paste0(., "_share"), .cols = c(accomodation_food:wholesale))

data_eda <- data_eda%>%
  left_join(select(industry_shares_wide, accomodation_food_share:wholesale_share, local_authority, year), by = c("local_authority", "year"))

#Computing net business
demography_net <- data_eda%>%
  group_by(local_authority, year)%>%
  mutate(net_business = (births - deaths))

#Joining net_business to data growth for RQ2 & RQ3
data_eda <- data_eda%>%
  left_join(select(demography_net, net_business, local_authority, year), by = c("local_authority", "year"))%>%
  arrange(year)

View(data_eda)
write_csv(data_eda, file = "outputs/tables/data_eda.csv")
