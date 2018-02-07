library(dplyr)
library(datapkg)
library(reshape2)
library(stringr)

##################################################################
#
# Processing Script for Children in Placement by Race and Ethnicity
# Created by Jenna Daly
# On 01/05/2018
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))
cip_file <- dir(path_to_raw, recursive=T, pattern = ".csv")

#Read in data
cip_df <- read.csv(paste0(path_to_raw, "/", cip_file), stringsAsFactors=F, header=T, check.names=F)
cip_df <- cip_df[-c(1,3)]

#Reshape data
cip_df_long <- melt(
    cip_df,
    id.vars = c("DCF_Region", "Demographic", "Out_of_State", "Month_in_Care"),
    variable.name = "Type of Placement",
    variable.factor = F,
    value.name = "Value",
    value.factor = F
)

# recode all the things
# Region Names
cip_df_long$DCF_Region[cip_df_long$DCF_Region == "Region 1"] <- "Region 1: Southwest"
cip_df_long$DCF_Region[cip_df_long$DCF_Region == "Region 2"] <- "Region 2: South Central"
cip_df_long$DCF_Region[cip_df_long$DCF_Region == "Region 3"] <- "Region 3: Eastern"
cip_df_long$DCF_Region[cip_df_long$DCF_Region == "Region 4"] <- "Region 4: North Central"
cip_df_long$DCF_Region[cip_df_long$DCF_Region == "Region 5"] <- "Region 5: Western"
cip_df_long$DCF_Region[cip_df_long$DCF_Region == "Region 6"] <- "Region 6: Central"

# Race/Ethnicity
cip_df_long$Demographic <- str_to_title(cip_df_long$Demographic)
cip_df_long$Demographic <- gsub("Non-Hispanic ", "", cip_df_long$Demographic)

# In/out of state
cip_df_long$Out_of_State[cip_df_long$Out_of_State == "Y"] <- "Out of State"
cip_df_long$Out_of_State[cip_df_long$Out_of_State == "N"] <- "In State"

# Year
cip_df_long$Year <- str_sub(cip_df_long$Month_in_Care,-4,-1)
cip_df_long$Year <- paste("SFY", paste(cip_df_long$Year, as.numeric(cip_df_long$Year)+1, sep="-"))

#Type of Placement
cip_df_long$`Type of Placement` <- as.character(cip_df_long$`Type of Placement`)
cip_df_long$`Type of Placement`[cip_df_long$`Type of Placement` == "TOTAL_CIP"] <- "Total"
cip_df_long$`Type of Placement`[cip_df_long$`Type of Placement` == "FOSTER_CARE"] <- "Foster Care"                      
cip_df_long$`Type of Placement`[cip_df_long$`Type of Placement` == "RELATIVE_CARE"] <- "Relative Foster Care"
cip_df_long$`Type of Placement`[cip_df_long$`Type of Placement` == "SPECIAL_STUDY"] <- "Special Study"
cip_df_long$`Type of Placement`[cip_df_long$`Type of Placement` == "THERAPEUTIC_FOSTER_CARE"] <- "Therapeutic Foster Care"
cip_df_long$`Type of Placement`[cip_df_long$`Type of Placement` == "PDC_SAFE_HOME"] <- "PDC or Safe Home"
cip_df_long$`Type of Placement`[cip_df_long$`Type of Placement` == "SHELTER"] <- "Shelter"
cip_df_long$`Type of Placement`[cip_df_long$`Type of Placement` == "GROUP_HOME"] <- "Group Home"
cip_df_long$`Type of Placement`[cip_df_long$`Type of Placement` == "RESIDENTIAL"] <- "Residential Treatment Center"
cip_df_long$`Type of Placement`[cip_df_long$`Type of Placement` == "DCF_HIGHMEADOWS"] <- "High Meadows"
cip_df_long$`Type of Placement`[cip_df_long$`Type of Placement` == "DCF_SOLNIT"] <- "Solnit Center"
cip_df_long$`Type of Placement`[cip_df_long$`Type of Placement` == "DCF_CJTS"] <- "Connecticut Juvenile Training School"
cip_df_long$`Type of Placement`[cip_df_long$`Type of Placement` == "HOSPITAL"] <- "Medical or Psychiatric Hospital Placement"
cip_df_long$`Type of Placement`[cip_df_long$`Type of Placement` == "INDEPENDENT_LIVING"] <- "Independent Living"     

#Aggregate totals by Year
cip_df_long_clean <- unique(cip_df_long %>% 
  group_by(DCF_Region, Demographic, Out_of_State, `Type of Placement`, Year) %>% 
  mutate(tot_Value = sum(Value)) %>% 
  select(DCF_Region, Demographic, Out_of_State, `Type of Placement`, Year, tot_Value))

#Create Totals for all races
tot_Race <- cip_df_long_clean %>% 
  group_by(DCF_Region, Out_of_State, `Type of Placement`, Year) %>% 
  mutate(tot_Value = sum(tot_Value))

tot_Race$Demographic <- "Total"

tot_Race <- unique(tot_Race)

cip_df_long_clean <- rbind(cip_df_long_clean, tot_Race)

#Backfill groups
regions <- c("Other", "Region 0", "Region 1: Southwest", 
                         "Region 2: South Central", "Region 3: Eastern", 
                         "Region 4: North Central", "Region 5: Western", 
                         "Region 6: Central")

backfill <- expand.grid(
  DCF_Region = regions,
  `Year` = unique(cip_df_long_clean$Year),
  `Demographic` = unique(cip_df_long_clean$Demographic),
  `Out_of_State` = unique(cip_df_long_clean$Out_of_State),
  `Type of Placement` = unique(cip_df_long_clean$`Type of Placement`)
)

cip_df_long_clean <- as.data.frame(cip_df_long_clean, stringsAsFactors=FALSE)
backfill <- as.data.frame(backfill, stringsAsFactors=FALSE)

cip_df_long_clean <- merge(cip_df_long_clean, backfill, 
                           by = c("DCF_Region", "Year", "Demographic", "Out_of_State", "Type of Placement"), 
                           all.y=T)

# Add Measure type and variable
cip_df_long_clean$`Measure Type` <- "Number"
cip_df_long_clean$Variable <- "Children in Placement"

#Create copy with Region 0 for CONNECT
cip_df_long_clean_CONNECT <- cip_df_long_clean

write.table(
    cip_df_long_clean_CONNECT,
    file.path(getwd(), "data", "children-in-placement-by-race-ethnicity_CONNECT.csv"),
    sep = ",",
    row.names = F,
    na = "-6666" #Missing data that was backfilled
)

cip_df_long_clean <- cip_df_long_clean[cip_df_long_clean$DCF_Region != "Region 0",]

# Use factors to order data
# Region
cip_df_long_clean$DCF_Region <- factor(cip_df_long_clean$DCF_Region, 
                                   levels = c("Region 1: Southwest",
                                              "Region 2: South Central",
                                              "Region 3: Eastern",
                                              "Region 4: North Central",
                                              "Region 5: Western",
                                              "Region 6: Central",
                                              "Other"))

# Race
cip_df_long_clean$Demographic <- factor(cip_df_long_clean$Demographic, 
                                        levels = c("White",
                                                   "Black",
                                                   "Hispanic",
                                                   "Other",
                                                   "Total"))

# Type of Placement
cip_df_long_clean$`Type of Placement` <- factor(cip_df_long_clean$`Type of Placement`, 
                                                levels = c("Foster Care",
                                                           "Relative Foster Care",
                                                           "Special Study",
                                                           "Therapeutic Foster Care",
                                                           "PDC or Safe Home",
                                                           "Shelter",
                                                           "Group Home",
                                                           "Residential Treatment Center",
                                                           "High Meadows",
                                                           "Solnit Center",
                                                           "Connecticut Juvenile Training School",
                                                           "Medical or Psychiatric Hospital Placement",
                                                           "Independent Living",
                                                           "Total"))

cip_df_long_clean <- as.data.frame(cip_df_long_clean)

#Select, sort, and rename columns
complete_df <- cip_df_long_clean %>% 
  select(DCF_Region, Year, Demographic, Out_of_State, `Type of Placement`, `Measure Type`, Variable, tot_Value) %>% 
  arrange(DCF_Region, Year, Demographic, Out_of_State, `Type of Placement`) %>% 
  rename(Region = DCF_Region, `Race/Ethnicity`  = Demographic, `Location of Placement` = Out_of_State, Value = tot_Value)

# Write to File
write.table(
    complete_df,
    file.path(getwd(), "data", "children-in-placement-by-race-ethnicity.csv"),
    sep = ",",
    row.names = F,
    na = "-6666" #Missing data that was backfilled
)
