################################################################################
#Functions and global objects
################################################################################


#Vector of Yorkshire and the Humber LAs to filter data sets 
#(North Yorkshire not included as not available in Ofcom data)
yorkshire_las <- c("barnsley", "bradford","calderdale","doncaster",
                   "east riding of yorkshire",
                   "kingston upon hull, city of", "kirklees","leeds",
                   "north east lincolnshire",
                   "north lincolnshire","rotherham","sheffield",
                   "wakefield","york")

#Function normalises local authority names across data sets
la_normaliser <- function(x) {
  x <- as.character(x)
  x <- str_to_lower(x)
  x <- str_remove(x, "\\*")
  x <- str_replace_all(x, "_", " ")
  
  case_when(
    str_detect(x, "barnsley") ~ "barnsley",
    str_detect(x, "bradford") ~ "bradford",
    str_detect(x, "calderdale") ~ "calderdale",
    str_detect(x, "doncaster") ~ "doncaster",
    str_detect(x, "east riding") ~ "east riding of yorkshire",
    str_detect(x, "kingston upon hull") ~ "kingston upon hull, city of",
    str_detect(x, "kirklees") ~ "kirklees",
    str_detect(x, "leeds") ~ "leeds",
    str_detect(x, "north east lincolnshire") ~ "north east lincolnshire",
    str_detect(x, "north lincolnshire") ~ "north lincolnshire",
    str_detect(x, "rotherham") ~ "rotherham",
    str_detect(x, "sheffield") ~ "sheffield",
    str_detect(x, "wakefield") ~ "wakefield",
    str_equal(x, "york") ~ "york",
    TRUE ~ NA_character_)}

#Function re-formats sliced BRES data frames
bres_reformatter <- function(df, year) {
  
  df%>%
    
    row_to_names(row_number = 1, remove_row = TRUE)%>% #Assigning the first row as the header
    
    clean_names()%>% #Using janitor library to clean column names
    
    mutate(industry = case_when( #Cleaning industry sector names
      industry == "1 : Agriculture, forestry & fishing (A)" ~ "agriculture_forestry_fishing",                 
      industry == "2 : Mining, quarrying & utilities (B,D and E)" ~ "mining_quarrying_utilities",
      industry == "3 : Manufacturing (C)" ~ "manufacturing",
      industry == "4 : Construction (F)" ~ "construction",
      industry == "5 : Motor trades (Part G)" ~ "motor_trades",
      industry == "6 : Wholesale (Part G)" ~ "wholesale",
      industry == "7 : Retail (Part G)" ~ "retail",
      industry == "8 : Transport & storage (inc postal) (H)" ~ "transport_storage_postal",
      industry == "9 : Accommodation & food services (I)" ~ "accomodation_food",
      industry == "10 : Information & communication (J)" ~ "information_communication",
      industry == "11 : Financial & insurance (K)" ~ "finance_insurance",
      industry == "12 : Property (L)" ~ "property",
      industry == "13 : Professional, scientific & technical (M)" ~ "professional_scientific_technical",
      industry == "14 : Business administration & support services (N)" ~ "business_admin_support_services",
      industry == "15 : Public administration & defence (O)" ~ "public_admin_defence",
      industry == "16 : Education (P)" ~ "education",
      industry == "17 : Health (Q)" ~ "health",
      industry == "18 : Arts, entertainment, recreation & other services (R,S,T and U)" ~ "arts_entertainment_recreation_other",
      TRUE ~ industry
    ))%>%
    
    select(-contains("flags", ignore.case = TRUE))%>% #Removing "Flags" columns
    
    filter(!is.na(industry) & industry != "")%>% #Removing blank row between header and first industry
    
    pivot_longer(#Pivoting each table to long format
      cols = -industry,
      names_to = "local_authority",
      values_to = "employee_count",
    )%>%
    
    mutate(local_authority = la_normaliser(local_authority),
           employee_count = as.numeric(employee_count))%>%
    
    filter(local_authority %in% yorkshire_las)
  
}

#Function re-formats Ofcom dataframes
ofcom_reformatter <- function(df) {
  df%>%
    
    rename(local_authority = laua_name)%>% #Matching LA variable name with other data sets
    
    clean_names()%>% #Using janitor library to lowercase and underscore all variable names
    
    select(local_authority,
           sfbb_availability_percent_premises, percent_of_premises_unable_to_receive_10mbit_s
    )%>%
    
    rename(sfbb_availability_pct = sfbb_availability_percent_premises,
           unable_to_recieve_10mbits_pct = percent_of_premises_unable_to_receive_10mbit_s)%>%
    
    mutate(local_authority = la_normaliser(local_authority), #Converting LAs to lowercase for consistency
           unable_to_recieve_10mbits_pct = as.double(unable_to_recieve_10mbits_pct))%>% #Correcting dtype coerced by missing value
    
    mutate(unable_to_recieve_10mbits_pct = replace_na(unable_to_recieve_10mbits_pct, median(unable_to_recieve_10mbits_pct, na.rm = TRUE)))%>% #Replacing NA with median
    
    filter(local_authority %in% yorkshire_las) #Using yorkshire_las vector to filter out LAs outside Yorkshire and the Humber
  
}

#Function re-formats bd_2020_raw
bd_2020_reformatter <- function(df, table_name) {
  
  value_col <- ifelse(str_detect(table_name, "birth"), "births", "deaths") #If else condition to determine if value column is births or deaths

  df%>%
    select(-1) %>%  # remove LA code column
    
    rename(
      local_authority = 1,
      "2015" = 2,
      "2016" = 3,
      "2017" = 4,
      "2018" = 5
    )%>%
    
    mutate(local_authority = la_normaliser(local_authority))%>%
    
    # keeping only desired LAs 
    filter(local_authority %in% yorkshire_las)%>%
    
    pivot_longer(
      cols = c("2015", "2016", "2017", "2018"),
      names_to = "year",
      values_to = value_col
    )%>%
    
    mutate(year = as.integer(year))
}

#Function re-formats bd_2023_raw
bd_2023_reformatter <- function(df, table_name) {
  
  value_col <- ifelse(str_detect(table_name, "birth"), "births", "deaths") #If else condition to determine if value column is births or deaths
  
  if (str_detect(table_name, "2019")) { #If-else condition to discriminate variable 'years'
    years <- "2019"
  } else if (str_detect(table_name, "2020")) {
    years <- "2020"
  } else if (str_detect(table_name, "2021-2023")) {
    years <- c("2021", "2022", "2023")
  } else {
    stop("Unexpected table_name: ", table_name)
  }
  
  df <- df%>%
    select(-1)%>% #Removing LA code column
    
    {
      new_names <- c("local_authority", years) #Assigning column names using years condition
      names(.) <- new_names
      .
    }%>%
    
    mutate(local_authority = la_normaliser(local_authority))%>%
    
    filter(local_authority %in% yorkshire_las)%>%
    
    mutate(across(all_of(years), ~ as.numeric(.)))%>%
    
    pivot_longer(
      cols = all_of(years),
      names_to = "year",
      values_to = value_col
    )%>%
    
    mutate(year = as.integer(year))
  
}
