
library(curl)
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(readr)
library(readxl)

body <- list(useConsole = TRUE, consoleDatabase = "NMML_AEP_SSL", consoleQuery = "SELECT   
             [AnimalID] = [viewBrandResight].[AnimalID]
             ,[SiteName] = [viewBrandResight].[SiteName]
             ,[Beach] = [viewBrandResight].[Beach]
             ,[Region] = [viewBrandResight].[Region]
             ,[Year] = [viewBrandResight].[Year]
             ,[Month] = [viewBrandResight].[Month]
             ,[Day] = [viewBrandResight].[Day]
             ,[Time] = [viewBrandResight].[Time]
             ,[Sex] = [viewBrandResight].[Sex]
             ,[Age] = [viewBrandResight].[Age]
             ,[Behavior] = [viewBrandResight].[Behavior] 
             ,[Platform] = [viewBrandResight].[Platform] 
             FROM (SELECT * FROM (
             SELECT * 
             FROM [NMML_AEP_SSL].[sighting].[viewBrandResight]) AS A  
             ) AS viewBrandResight ")
r <- POST("https://afsc-apps-internal.nmfs.local/NMML/internals/AEP/rest/gettable.php", body = body, encode="json")
json <- content(r, "text")
result <- fromJSON(json)

### Initial cleaning, etc...
res_data <- result %>% mutate(
  date = paste(Year, Month, Day, sep="-")
  ) %>% filter(!is.na(AnimalID) & !is.na(SiteName) & !is.na(Month))

res_data <- res_data %>% 
  mutate(
    datetime = ifelse(!is.na(Time) & !is.na(date), paste(date, Time), NA) %>% lubridate::ymd_hms(),
    date = as.Date(date) %>% lubridate::date()
  ) %>% select(-Month, -Day, -Year) %>% as_tibble()

####
# Get first capture data
#####

body <- list(useConsole = TRUE, consoleDatabase = "NMML_AEP_SSL", consoleQuery = "SELECT   
	[AnimalID] = [viewFirstCapture].[AnimalID]
	           ,[Brand] = [viewFirstCapture].[Brand]
             ,[SiteName] = [viewFirstCapture].[SiteName]
             ,[Beach] = [viewFirstCapture].[Beach]
             ,[Year] = [viewFirstCapture].[Year]
             ,[Month] = [viewFirstCapture].[Month]
             ,[Day] = [viewFirstCapture].[Day]
             ,[Sex] = [viewFirstCapture].[Sex]
             ,[AdjustedSex] = [viewFirstCapture].[AdjustedSex]
             ,[Age] = [viewFirstCapture].[Age]
             ,[Mass_kg] = [viewFirstCapture].[Mass_kg]
             ,[StandardLength_cm] = [viewFirstCapture].[StandardLength_cm]
             ,[AxillaryGirth_cm] = [viewFirstCapture].[AxillaryGirth_cm]
             ,[FlipperLength_cm] = [viewFirstCapture].[FlipperLength_cm] 
             FROM (SELECT * FROM (
             SELECT * 
             FROM [NMML_AEP_SSL].[capture].[viewFirstCapture]) AS A  
             ) AS viewFirstCapture ")
r <- POST("https://afsc-apps-internal.nmfs.local/NMML/internals/AEP/rest/gettable.php", body = body, encode="json")
json <- content(r, "text")
fc_data <- fromJSON(json) %>% as_tibble()

fc_data <- fc_data %>% 
  filter(!is.na(AnimalID) & !is.na(Brand) & Age=="P" & Year>=2000) %>% 
  arrange(AnimalID)

######
# Data QC
#####
fc_data <- fc_data %>% mutate(
  Brand_lead = substr(Brand, start = 1, stop = 1)
) %>% filter(Brand_lead %in% c('T', 'X', 'A', 'J', 'E', '~', '>'))

fc_data <- fc_data %>% mutate(
  date = paste(Year, Month, Day, sep="-") %>% as.Date() %>% lubridate::as_date()
)

site_reg <- res_data %>% select(SiteName, Region) %>% distinct()

fc_data <- fc_data %>% left_join(site_reg)

res_data <- res_data %>% filter(AnimalID%in%unique(fc_data$AnimalID)) 

# Add initial capture to resights
res_data <- res_data %>% 
  full_join(select(fc_data, AnimalID, SiteName, Beach, Sex, date, Region)) %>% 
  mutate(AnimalID = as.integer(AnimalID)) %>% 
  arrange(AnimalID, date) 

# Remove everything but summer

res_data <- res_data %>% filter(month(date)>=5 & month(date)<=8)

###
# Load pup evidence
###
pup_evidence_data <- read_excel("behave_codes_evidence.xlsx", range = cell_cols("B:F")) %>% 
  select(Behavior, Luxa, Chumbley, Kelly) %>% 
  gather(key=rater, value=pup_evidence, -Behavior) %>% group_by(Behavior) %>% 
  summarise(pup_evidence = mean(pup_evidence, na.rm=T)) %>% arrange(desc(pup_evidence))

res_data <- res_data %>% 
  mutate(Behavior = toupper(Behavior)) %>% 
  left_join(pup_evidence_data) %>%  arrange(AnimalID) %>% 
  mutate(pup_evidence=ifelse(is.na(pup_evidence), 0, pup_evidence))

# write_csv(res_data, "data/resight_data.csv")
# write_csv(fc_data, "data/initial_capture_data.csv")

