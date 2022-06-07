##set working directory
rm(list=ls()) #removing previous objects

#Loading packages------------
require (RColorBrewer)
require (plyr)
require (ggplot2)
require (gridExtra)
require (pvclust)
require (reshape2)
require (dplyr)
require(readxl)
require(xlsx)

#Changing the variables from WP1 data--------------
load("Data/SBS_v3.Rda")
df<-data%>%transmute(Sector=Sector,
                     Pressure=Pressure,
                     Ecological.Characteristic=Ecological.Characteristic,
                     Overlap=Overlap,
                     Frequency=Frequency,
                     DoI=DoI)

write.xlsx(df,'Data/sbs_v4.xlsx')

#Function for merging tables WP1/WP7-----------
Add_Matrices <- function(input, excel_file) {
  
  # input is a dataframe with columns "Sector" "Pressure" 
  # "Ecological.Characteristic" "Overlap" Frequency "DoI"
  # excel_file is the full path as a character string to a two sheet xlsx file 
  # with a sheet "Persistence" containg one matrix, and another in sheet "Resilience"
  
  # DO NOT UNCOMMENT, FOR FUNCTION DEVELOPMENT  
  # input <- readxl::read_xlsx("./InputLetters.xlsx")
  # excel_file <- "./Example matrix.xlsx"
  
  ## Process resilience matrix
  Resilience <- readxl::read_xlsx(excel_file, sheet = "Resilience")    # Import matrix
  Resilience <- dplyr::rename(Resilience, Pressure = `...1`)           # Overwrite the column name generated for empty cell A1
  Resilience <- tidyr::pivot_longer(Resilience, -Pressure,             # Convert to long-format dataframe
                                    names_to = "Ecological.Characteristic", values_to = "Resilience")
  Resilience <- dplyr::left_join(input, Resilience)                    # Bind to input object 
  
  ## Process Persistence matrix
  Persistence <- readxl::read_xlsx(excel_file, sheet = "Persistence")  # Import matrix
  
  Persistence_sector <- dplyr::filter(Persistence, !is.na(Sector))     # Limit to scores with a specific sector
  Persistence_sector <- tidyr::separate_rows(Persistence_sector, Sector, sep = ", ") # Create a unique row per sector, duplicating scores
  Persistence_sector <- tidyr::pivot_longer(Persistence_sector, -c(Sector, Pressure), # Convert to long-format dataframe
                                            names_to = "Ecological.Characteristic", values_to = "Persistence")
  
  Persistence_all <- dplyr::filter(Persistence, is.na(Sector))         # Import matrix
  Persistence_all <- dplyr::select(Persistence_all, -Sector)           # Limit to scores without specific sector
  Persistence_all <- tidyr::pivot_longer(Persistence_all, -Pressure,   # Convert to long-format dataframe
                                         names_to = "Ecological.Characteristic", values_to = "Persistence")
  
  ## Combine 
  
  add <- dplyr::semi_join(Resilience, Persistence_sector)              # Limit the resilience data to matches with sector-specific persistence  
  add <- dplyr::left_join(add, Persistence_sector)                     # Combine resilience and persistence
  
  add2 <- dplyr::anti_join(Resilience, Persistence_sector)             # Limit the resilience data to matches without sector-specific persistence
  add2 <- dplyr::left_join(add2, Persistence_all)                      # Combine resilience and persistence
  
  COMBINED <- dplyr::bind_rows(add, add2)                              # Add together sector and non-sector specific persistence
  
  return(COMBINED)
}

## Example - change to your input files
InputLetters <- readxl::read_xlsx("Data/sbs_v4.xlsx")
data <- Add_Matrices(InputLetters, "Data/MA_Vulnerability_Linkage_Framework_SBS_V3_May_2022.xlsx")

unique(data$Ecological.Characteristic)

# data$Ecological.Characteristic <- gsub("Demersal Elasmo", "Demersal Elasmobranch", data$Ecological.Characteristic)
# data$Ecological.Characteristic <- gsub("Deep Sea Elasmo", "Deep Sea Elasmobranch", data$Ecological.Characteristic)
# data$Ecological.Characteristic <- gsub("Pelagic Elasmo", "Pelagic Elasmobranch", data$Ecological.Characteristic)



# If using InputLetters score each rating 
data$Overlap.Score = ifelse(data$Overlap == "WE", 1,
                          ifelse(data$Overlap == "WP", 0.67,   
                            ifelse(data$Overlap == "L", 0.37, 
                                   ifelse(data$Overlap == "S", 0.03, 
                                     ifelse(data$Overlap == "E", 0.01, NA)))))
data$Frequency.Score = ifelse(data$Frequency == "P", 1,
                              ifelse(data$Frequency == "C", 0.67, 
                                     ifelse(data$Frequency == "O", 0.33,
                                            ifelse(data$Frequency == "R", 0.08, NA ))))
data$DoI.Score = ifelse(data$DoI == "A" , 1,
                        ifelse(data$DoI == "C", 0.13,
                               ifelse(data$DoI == "L", 0.01, NA)))
data$Resilience.Score = ifelse(data$Resilience == "L", 0.55,
                               ifelse(data$Resilience == "M", 0.06, 
                                      ifelse(data$Resilience == "H", 0.01, NA)))
data$Persistence.Score = ifelse(data$Persistence == "C", 1,
                                ifelse(data$Persistence == "H", 0.55,
                                       ifelse(data$Persistence == "M", 0.06, 
                                              ifelse(data$Persistence == "L", 0.01, NA))))


### Calculate Impact Risk, Recovery Lag and log IR
data$ImpactRisk = data$Overlap.Score*data$Frequency.Score*data$DoI.Score
data$RecoveryLag = data$Resilience.Score*data$Persistence.Score
data$LN.IR = log(data$ImpactRisk)

# calculate recovery years from resilience and persistence
data$Ryr =  (data$Resilience.Score + data$Persistence.Score) *100

data$TotalRisk = data$ImpactRisk * data$RecoveryLag

names(data)[4]<-'EcoChar'

# Removing rows with NAs
data<-data%>%filter(complete.cases(data))

write.xlsx(data,'Data/MA_SBS_Pressures_and_Vulnerabilities.xlsx')
