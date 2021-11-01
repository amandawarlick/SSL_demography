#load and tidy separate oceanographic indices and create one csv file

library(tidyr)
library(data.table)
library(dplyr)
library(ggplot2)
library(scales)
library(knitr)
library(reshape2)
library(stringr)
library(magrittr) 
library(readr) #parse()
library(zoo)
library(ncdf4)
library(here)


##season level notes
#for phi of 2000 pups as measured in 2001, env would be:
#winter/spring 1 should contain jan-mar/apr-may of 2001
#summer/fall 1 should contain june-sept/oct-dec 2000
#winter of 2017 applies to 2016 pup survival, which is 17th year of data

##Don't want adj_year = 0 (jan-may 2000), unless want to look at it against #of pups born in 2000
##and won't have use for adj_year = 18 (nov-dec 2017) until 2018 resights
##spring 18 is NA because it would be 1-5 2019 data

#pull in monthly data from individual csvs, and then assign data to year-month combinations
NB <- c(9, 10, 11, 12, 1, 2, 3, 4, 5) #Sept-May (non-breeding)
winter <- c(1,2,3) #Jan-Mar (winter)
spring <- c(4:5) #Apr-May (spring)
fall <- c(10,11,12) #Oct-Dec
summer <- c(6,7,8,9) #June-Sept

#MEI (monthly): https://www.esrl.noaa.gov/psd/enso/mei.old/table.html
MEI_raw <- read.csv(here::here('data', 'Ocean/MEI_mo.csv'), header = T, stringsAsFactors = F) %>%
  melt(id.vars = 'year', value.name = 'MEI') %>%
  transform(month = as.numeric(gsub('X', '', variable))) %>%
  dplyr::select(-variable) %>%
  filter(year > 1999 & year < 2019) %>%
  transform(yr = as.numeric(factor(year))) %>%
  transform(type = ifelse(month %in% NB, 'NB', 'B')) %>%
  transform(season = ifelse(month %in% winter, 'winter', ifelse(month %in% spring, 'spring',
                            ifelse(month %in% summer, 'summer', 'fall')))) 

MEI_seas <- data.frame()
MEI_temp <- MEI_raw 
for (i in 1:nrow(MEI_temp)) {
  temp <- MEI_temp[i,]
  year_num <- MEI_temp[i, 'yr']
  if (temp$season %in% c('winter', 'spring')) {
  temp$season_yr <- paste(temp$season, year_num-1, sep = '')
  } else {
  temp$season_yr <- as.character(paste(as.character(temp$season), year_num, sep = ''))
  }
  MEI_seas <- rbind(MEI_seas, temp)
}

MEI_seas <- MEI_seas %>%
  group_by(season_yr) %>%
  dplyr::summarize(MEI = mean(MEI, na.rm = T)) %>%
  transform(adj_year = parse_number(season_yr), season = gsub("^\\d+|\\d+$", "", season_yr)) %>%
  dplyr::select(-season_yr) %>%
  dcast(adj_year ~ season, value.var = 'MEI')

#annual level - excludes summer breeding months
MEI_yr <- data.frame()
MEI_temp <- MEI_raw %>% filter(type == 'NB')
for (i in 1:nrow(MEI_temp)) {
  temp <- MEI_temp[i,]
  year_num <- MEI_temp[i, 'yr']
    if (temp$month %in% c(9:12)) {
      temp$period <- as.character(paste(as.character(temp$type), year_num, sep = ''))
    } else {
      temp$period <- paste(as.character(temp$type), year_num-1, sep = '')
  }
  MEI_yr <- rbind(MEI_yr, temp)
}

MEI_yr <- MEI_yr %>%
  group_by(period) %>%
  dplyr::summarize(MEI = mean(MEI, na.rm = T)) %>%
  transform(adj_year = parse_number(period), type = gsub("^\\d+|\\d+$", "", period)) %>%
  dplyr::select(-period) %>%
  dcast(adj_year ~ type, value.var = 'MEI')

MEI <- MEI_yr %>%
  merge(MEI_seas, by = 'adj_year')
colnames(MEI) <- c('adj_year', paste('MEI', c('NB', 'fall', 'spring', 'summer', 'winter'), sep = '_'))
  
#NPGO (monthly)
NPGO_raw <- read.csv(here::here('data', 'Ocean/NPGO.csv'), header = T, stringsAsFactors = F) %>%
  filter(year > 1999 & year < 2019) %>%
  transform(yr = as.numeric(factor(year))) %>%
  transform(type = ifelse(month %in% NB, 'NB', 'B')) %>%
  transform(season = ifelse(month %in% winter, 'winter', ifelse(month %in% spring, 'spring',
                                                                ifelse(month %in% summer, 'summer', 'fall')))) 
##season level
NPGO_seas <- data.frame()
NPGO_temp <- NPGO_raw #%>% filter(season %in% c('winter', 'spring'))
for (i in 1:nrow(NPGO_temp)) {
  temp <- NPGO_temp[i,]
  year_num <- NPGO_temp[i, 'yr']
  if (temp$season %in% c('winter', 'spring')) {
    temp$season_yr <- paste(temp$season, year_num-1, sep = '')
  } else {
    temp$season_yr <- as.character(paste(as.character(temp$season), year_num, sep = ''))
  }
  NPGO_seas <- rbind(NPGO_seas, temp)
}

NPGO_seas <- NPGO_seas %>%
  group_by(season_yr) %>%
  dplyr::summarize(NPGO = mean(NPGO, na.rm = T)) %>%
  transform(adj_year = parse_number(season_yr), season = gsub("^\\d+|\\d+$", "", season_yr)) %>%
  dplyr::select(-season_yr) %>%
  dcast(adj_year ~ season, value.var = 'NPGO')

##year-level 
NPGO_yr <- data.frame()
NPGO_temp <- NPGO_raw %>% filter(type == 'NB')
for (i in 1:nrow(NPGO_temp)) {
  temp <- NPGO_temp[i,]
  year_num <- NPGO_temp[i, 'yr']
  if (temp$month %in% c(9:12)) {
    temp$period <- as.character(paste(as.character(temp$type), year_num, sep = ''))
  } else {
    temp$period <- paste(as.character(temp$type), year_num-1, sep = '')
  }
  NPGO_yr <- rbind(NPGO_yr, temp)
}

NPGO_yr <- NPGO_yr %>%
  group_by(period) %>%
  dplyr::summarize(NPGO = mean(NPGO, na.rm = T)) %>%
  transform(adj_year = parse_number(period), type = gsub("^\\d+|\\d+$", "", period)) %>%
  dplyr::select(-period) %>%
  dcast(adj_year ~ type, value.var = 'NPGO')

NPGO <- NPGO_yr %>%
  merge(NPGO_seas, by = 'adj_year')
colnames(NPGO) <- c('adj_year', paste('NPGO', c('NB', 'fall', 'spring', 'summer', 'winter'), sep = '_'))


# aleutian low (monthly)
ALBSA_raw <- read.csv(here::here('data', 'Ocean/ALBSA_mo.csv'), header = T, stringsAsFactors = F) %>%
  dplyr::select(Date, ALBSA) %>%
  transform(Date = mdy(Date)) %>%
  transform(year = year(Date), month = month(Date)) %>%
  filter(year > 1999 & year < 2019) %>% dplyr::select(-Date) %>%
  transform(yr = as.numeric(factor(year))) %>%
  transform(type = ifelse(month %in% NB, 'NB', 'B')) %>%
  transform(season = ifelse(month %in% winter, 'winter', ifelse(month %in% spring, 'spring',
                                                                ifelse(month %in% summer, 'summer', 'fall')))) 

##season level
ALBSA_seas <- data.frame()
ALBSA_temp <- ALBSA_raw 
for (i in 1:nrow(ALBSA_temp)) {
  temp <- ALBSA_temp[i,]
  year_num <- ALBSA_temp[i, 'yr']
  if (temp$season %in% c('winter', 'spring')) {
    temp$season_yr <- paste(temp$season, year_num-1, sep = '')
  } else {
    temp$season_yr <- as.character(paste(as.character(temp$season), year_num, sep = ''))
  }
  ALBSA_seas <- rbind(ALBSA_seas, temp)
}


ALBSA_seas <- ALBSA_seas %>%
  group_by(season_yr) %>%
  dplyr::summarize(ALBSA = mean(ALBSA, na.rm = T)) %>%
  transform(adj_year = parse_number(season_yr), season = gsub("^\\d+|\\d+$", "", season_yr)) %>%
  dplyr::select(-season_yr) %>%
  dcast(adj_year ~ season, value.var = 'ALBSA')

##year-level 
ALBSA_yr <- data.frame()
ALBSA_temp <- ALBSA_raw %>% filter(type == 'NB')
for (i in 1:nrow(ALBSA_temp)) {
  temp <- ALBSA_temp[i,]
  year_num <- ALBSA_temp[i, 'yr']
  if (temp$month %in% c(9:12)) {
    temp$period <- as.character(paste(as.character(temp$type), year_num, sep = ''))
  } else {
    temp$period <- paste(as.character(temp$type), year_num-1, sep = '')
  }
  ALBSA_yr <- rbind(ALBSA_yr, temp)
}

ALBSA_yr <- ALBSA_yr %>%
  group_by(period) %>%
  dplyr::summarize(ALBSA = mean(ALBSA, na.rm = T)) %>%
  transform(adj_year = parse_number(period), type = gsub("^\\d+|\\d+$", "", period)) %>%
  dplyr::select(-period) %>%
  dcast(adj_year ~ type, value.var = 'ALBSA')

ALBSA <- ALBSA_yr %>%
  merge(ALBSA_seas, by = 'adj_year')
colnames(ALBSA) <- c('adj_year', paste('ALBSA', c('NB', 'fall', 'spring', 'summer', 'winter'), sep = '_'))

# NOI (monthly)
NOI_raw <- read.csv(here::here('data', 'Ocean/NOI_mo.csv'), header = T, stringsAsFactors = F) %>%
  transform(Date = as.Date(Date)) %>%
  transform(year = year(Date), month = month(Date))%>%
  filter(year > 1999 & year < 2019) %>% dplyr::select(-Date) %>%
  transform(yr = as.numeric(factor(year))) %>%
  transform(type = ifelse(month %in% NB, 'NB', 'B')) %>%
  transform(season = ifelse(month %in% winter, 'winter', ifelse(month %in% spring, 'spring',
                                                                ifelse(month %in% summer, 'summer', 'fall')))) 

##season level
NOI_seas <- data.frame()
NOI_temp <- NOI_raw 
for (i in 1:nrow(NOI_temp)) {
  temp <- NOI_temp[i,]
  year_num <- NOI_temp[i, 'yr']
  if (temp$season %in% c('winter', 'spring')) {
    temp$season_yr <- paste(temp$season, year_num-1, sep = '')
  } else {
    temp$season_yr <- as.character(paste(as.character(temp$season), year_num, sep = ''))
  }
  NOI_seas <- rbind(NOI_seas, temp)
}


NOI_seas <- NOI_seas %>%
  group_by(season_yr) %>%
  dplyr::summarize(NOI = mean(NOI, na.rm = T)) %>%
  transform(adj_year = parse_number(season_yr), season = gsub("^\\d+|\\d+$", "", season_yr)) %>%
  dplyr::select(-season_yr) %>%
  dcast(adj_year ~ season, value.var = 'NOI')

##year-level
NOI_yr <- data.frame()
NOI_temp <- NOI_raw %>% filter(type == 'NB')
for (i in 1:nrow(NOI_temp)) {
  temp <- NOI_temp[i,]
  year_num <- NOI_temp[i, 'yr']
  if (temp$month %in% c(9:12)) {
    temp$period <- as.character(paste(as.character(temp$type), year_num, sep = ''))
  } else {
    temp$period <- paste(as.character(temp$type), year_num-1, sep = '')
  }
  NOI_yr <- rbind(NOI_yr, temp)
}

NOI_yr <- NOI_yr %>%
  group_by(period) %>%
  dplyr::summarize(NOI = mean(NOI, na.rm = T)) %>%
  transform(adj_year = parse_number(period), type = gsub("^\\d+|\\d+$", "", period)) %>%
  dplyr::select(-period) %>%
  dcast(adj_year ~ type, value.var = 'NOI')

NOI <- NOI_yr %>%
  merge(NOI_seas, by = 'adj_year')
colnames(NOI) <- c('adj_year', paste('NOI', c('NB', 'fall', 'spring', 'summer', 'winter'), sep = '_'))


# NPI (monthly)
NPI_raw <- read.csv(here::here('data', 'Ocean/NPI_mo.csv'), header = T, stringsAsFactors = F) %>%
  transform(year = as.numeric(substr(Date, 1,4)), month = as.numeric(substr(Date, 5, 6))) %>%
  filter(year > 1999 & year < 2019) %>% dplyr::select(-Date) %>%
  transform(yr = as.numeric(factor(year))) %>%
  transform(type = ifelse(month %in% NB, 'NB', 'B')) %>%
  transform(season = ifelse(month %in% winter, 'winter', ifelse(month %in% spring, 'spring',
                                                                ifelse(month %in% summer, 'summer', 'fall')))) 

##season level
NPI_seas <- data.frame()
NPI_temp <- NPI_raw 
for (i in 1:nrow(NPI_temp)) {
  temp <- NPI_temp[i,]
  year_num <- NPI_temp[i, 'yr']
  if (temp$season %in% c('winter', 'spring')) {
    temp$season_yr <- paste(temp$season, year_num-1, sep = '')
  } else {
    temp$season_yr <- as.character(paste(as.character(temp$season), year_num, sep = ''))
  }
  NPI_seas <- rbind(NPI_seas, temp)
}

NPI_seas <- NPI_seas %>%
  group_by(season_yr) %>%
  dplyr::summarize(NPI = mean(NPI, na.rm = T)) %>%
  transform(adj_year = parse_number(season_yr), season = gsub("^\\d+|\\d+$", "", season_yr)) %>%
  dplyr::select(-season_yr) %>%
  dcast(adj_year ~ season, value.var = 'NPI')

##year-level
NPI_yr <- data.frame()
NPI_temp <- NPI_raw %>% filter(type == 'NB')
for (i in 1:nrow(NPI_temp)) {
  temp <- NPI_temp[i,]
  year_num <- NPI_temp[i, 'yr']
  if (temp$month %in% c(9:12)) {
    temp$period <- as.character(paste(as.character(temp$type), year_num, sep = ''))
  } else {
    temp$period <- paste(as.character(temp$type), year_num-1, sep = '')
  }
  NPI_yr <- rbind(NPI_yr, temp)
}

NPI_yr <- NPI_yr %>%
  group_by(period) %>%
  dplyr::summarize(NPI = mean(NPI, na.rm = T)) %>%
  transform(adj_year = parse_number(period), type = gsub("^\\d+|\\d+$", "", period)) %>%
  dplyr::select(-period) %>%
  dcast(adj_year ~ type, value.var = 'NPI')

NPI <- NPI_yr %>%
  merge(NPI_seas, by = 'adj_year')
colnames(NPI) <- c('adj_year', paste('NPI', c('NB', 'fall', 'spring', 'summer', 'winter'), sep = '_'))


## upwelling
upwell_raw <- read.csv(here::here('data', 'Ocean',
                                  "upwell_anom_SSL.csv"), stringsAsFactors = F) %>%
  filter(year > 1999 & year < 2019 & lat == '60N' & long == '149W') %>%
  dplyr::select(-c(lat, long)) %>%
  melt(id.vars = 'year', value.name = 'upwell') %>%
  transform(month = as.numeric(gsub('X', '', variable))) %>% dplyr::select(-variable) %>%
  transform(upwell = scale(upwell)) %>%
  transform(yr = as.numeric(factor(year))) %>%
  transform(type = ifelse(month %in% NB, 'NB', 'B')) %>%
  transform(season = ifelse(month %in% winter, 'winter', ifelse(month %in% spring, 'spring',
                                                                ifelse(month %in% summer, 'summer', 'fall')))) 

##season level
upwell_seas <- data.frame()
upwell_temp <- upwell_raw 
for (i in 1:nrow(upwell_temp)) {
  temp <- upwell_temp[i,]
  year_num <- upwell_temp[i, 'yr']
  if (temp$season %in% c('winter', 'spring')) {
    temp$season_yr <- paste(temp$season, year_num-1, sep = '')
  } else {
    temp$season_yr <- as.character(paste(as.character(temp$season), year_num, sep = ''))
  }
  upwell_seas <- rbind(upwell_seas, temp)
}

upwell_seas <- upwell_seas %>%
  group_by(season_yr) %>%
  dplyr::summarize(upwell = mean(upwell, na.rm = T)) %>%
  transform(adj_year = parse_number(season_yr), season = gsub("^\\d+|\\d+$", "", season_yr)) %>%
  dplyr::select(-season_yr) %>%
  dcast(adj_year ~ season, value.var = 'upwell')

##year-level 
upwell_yr <- data.frame()
upwell_temp <- upwell_raw %>% filter(type == 'NB')
for (i in 1:nrow(upwell_temp)) {
  temp <- upwell_temp[i,]
  year_num <- upwell_temp[i, 'yr']
  if (temp$month %in% c(9:12)) {
    temp$period <- as.character(paste(as.character(temp$type), year_num, sep = ''))
  } else {
    temp$period <- paste(as.character(temp$type), year_num-1, sep = '')
  }
  upwell_yr <- rbind(upwell_yr, temp)
}

upwell_yr <- upwell_yr %>%
  group_by(period) %>%
  dplyr::summarize(upwell = mean(upwell, na.rm = T)) %>%
  transform(adj_year = parse_number(period), type = gsub("^\\d+|\\d+$", "", period)) %>%
  dplyr::select(-period) %>%
  dcast(adj_year ~ type, value.var = 'upwell')

upwell <- upwell_yr %>%
  merge(upwell_seas, by = 'adj_year')
colnames(upwell) <- c('adj_year', paste('upwell', c('NB', 'fall', 'spring', 'summer', 'winter'), sep = '_'))


#AO: https://www.ncdc.noaa.gov/teleconnections/ao/ monthly 1990-2018
AOI_raw <- read.csv(here::here('SSL_CJS', "Data/Ocean/New/AOI_mo.csv"), header = T, stringsAsFactors = F) %>%
  filter(year > 1999 & year < 2019) %>%
  transform(yr = as.numeric(factor(year))) %>%
  transform(type = ifelse(month %in% NB, 'NB', 'B')) %>%
  transform(season = ifelse(month %in% winter, 'winter', ifelse(month %in% spring, 'spring',
                                                                ifelse(month %in% summer, 'summer', 'fall')))) 

##season level
AOI_seas <- data.frame()
AOI_temp <- AOI_raw 
for (i in 1:nrow(AOI_temp)) {
  temp <- AOI_temp[i,]
  year_num <- AOI_temp[i, 'yr']
  if (temp$season %in% c('winter', 'spring')) {
    temp$season_yr <- paste(temp$season, year_num-1, sep = '')
  } else {
    temp$season_yr <- as.character(paste(as.character(temp$season), year_num, sep = ''))
  }
  AOI_seas <- rbind(AOI_seas, temp)
}

AOI_seas <- AOI_seas %>%
  group_by(season_yr) %>%
  dplyr::summarize(AOI = mean(AO, na.rm = T)) %>%
  transform(adj_year = parse_number(season_yr), season = gsub("^\\d+|\\d+$", "", season_yr)) %>%
  dplyr::select(-season_yr) %>%
  dcast(adj_year ~ season, value.var = 'AOI')

##year-level (non-breeding only)
AOI_yr <- data.frame()
AOI_temp <- AOI_raw %>% filter(type == 'NB')
for (i in 1:nrow(AOI_temp)) {
  temp <- AOI_temp[i,]
  year_num <- AOI_temp[i, 'yr']
  if (temp$month %in% c(9:12)) {
    temp$period <- as.character(paste(as.character(temp$type), year_num, sep = ''))
  } else {
    temp$period <- paste(as.character(temp$type), year_num-1, sep = '')
  }
  AOI_yr <- rbind(AOI_yr, temp)
}

AOI_yr <- AOI_yr %>%
  group_by(period) %>%
  dplyr::summarize(AOI = mean(AO, na.rm = T)) %>%
  transform(adj_year = parse_number(period), type = gsub("^\\d+|\\d+$", "", period)) %>%
  dplyr::select(-period) %>%
  dcast(adj_year ~ type, value.var = 'AOI')

AOI <- AOI_yr %>%
  merge(AOI_seas, by = 'adj_year')
colnames(AOI) <- c('adj_year', paste('AOI', c('NB', 'fall', 'spring', 'summer', 'winter'), sep = '_'))


###PDO
PDO_raw <- read.csv(here::here('SSL_CJS', "Data/Ocean/New/PDO_mo.csv"), header = T, stringsAsFactors = F) %>%
  transform(year = as.numeric(substr(Date, 1,4)), month = as.numeric(substr(Date, 5, 6))) %>%
  filter(year > 1999 & year < 2019) %>% dplyr::select(-Date) %>%
  transform(yr = as.numeric(factor(year))) %>%
  transform(type = ifelse(month %in% NB, 'NB', 'B')) %>%
  transform(season = ifelse(month %in% winter, 'winter', ifelse(month %in% spring, 'spring',
                                                                ifelse(month %in% summer, 'summer', 'fall')))) 

##season level
PDO_seas <- data.frame()
PDO_temp <- PDO_raw 
for (i in 1:nrow(PDO_temp)) {
  temp <- PDO_temp[i,]
  year_num <- PDO_temp[i, 'yr']
  if (temp$season %in% c('winter', 'spring')) {
    temp$season_yr <- paste(temp$season, year_num-1, sep = '')
  } else {
    temp$season_yr <- as.character(paste(as.character(temp$season), year_num, sep = ''))
  }
  PDO_seas <- rbind(PDO_seas, temp)
}

PDO_seas <- PDO_seas %>%
  group_by(season_yr) %>%
  dplyr::summarize(PDO = mean(PDO, na.rm = T)) %>%
  transform(adj_year = parse_number(season_yr), season = gsub("^\\d+|\\d+$", "", season_yr)) %>%
  dplyr::select(-season_yr) %>%
  dcast(adj_year ~ season, value.var = 'PDO')

##year-level (non-breeding only)
PDO_yr <- data.frame()
PDO_temp <- PDO_raw %>% filter(type == 'NB')
for (i in 1:nrow(PDO_temp)) {
  temp <- PDO_temp[i,]
  year_num <- PDO_temp[i, 'yr']
  if (temp$month %in% c(9:12)) {
    temp$period <- as.character(paste(as.character(temp$type), year_num, sep = ''))
  } else {
    temp$period <- paste(as.character(temp$type), year_num-1, sep = '')
  }
  PDO_yr <- rbind(PDO_yr, temp)
}

PDO_yr <- PDO_yr %>%
  group_by(period) %>%
  dplyr::summarize(PDO = mean(PDO, na.rm = T)) %>%
  transform(adj_year = parse_number(period), type = gsub("^\\d+|\\d+$", "", period)) %>%
  dplyr::select(-period) %>%
  dcast(adj_year ~ type, value.var = 'PDO')

PDO <- PDO_yr %>%
  merge(PDO_seas, by = 'adj_year')
colnames(PDO) <- c('adj_year', paste('PDO', c('NB', 'fall', 'spring', 'summer', 'winter'), sep = '_'))


############load and clean remote sensed data using xtracto
#pick east/west
# reg <- 'east'
# reg <- 'west'

####SST
# reg <- 'east'
# sst_raw <- read.csv(here::here('data', 'Ocean', paste0('sst_data_', reg, '.csv')), 
#                     header = T, stringsAsFactors = F) %>%
#   transform(year = year(date), month = month(date))%>%
#   filter(year > 1999 & year < 2019) %>% dplyr::select(-c(date)) %>%
#   transform(yr = as.numeric(factor(year))) %>%
#   transform(type = ifelse(month %in% NB, 'NB', 'B')) %>%
#   transform(season = ifelse(month %in% winter, 'winter', ifelse(month %in% spring, 'spring',
#                                                                 ifelse(month %in% summer, 'summer', 'fall')))) 
# 
# ##season level
# sst_seas <- data.frame()
# sst_temp <- sst_raw #%>% filter(season %in% c('winter', 'spring'))
# for (i in 1:nrow(sst_temp)) {
#   temp <- sst_temp[i,]
#   year_num <- sst_temp[i, 'yr']
#   if (temp$season %in% c('winter', 'spring')) {
#     temp$season_yr <- paste(temp$season, year_num-1, sep = '')
#   } else {
#     temp$season_yr <- as.character(paste(as.character(temp$season), year_num, sep = ''))
#   }
#   sst_seas <- rbind(sst_seas, temp)
# }
# 
# sst_seas <- sst_seas %>%
#   group_by(season_yr) %>%
#   dplyr::summarize(sst = mean(sst, na.rm = T)) %>%
#   transform(adj_year = parse_number(season_yr), season = gsub("^\\d+|\\d+$", "", season_yr)) %>%
#   dplyr::select(-season_yr) %>%
#   dcast(adj_year ~ season, value.var = 'sst')
# 
# ##year-level (non-breeding only)
# sst_yr <- data.frame()
# sst_temp <- sst_raw %>% filter(type == 'NB')
# for (i in 1:nrow(sst_temp)) {
#   temp <- sst_temp[i,]
#   year_num <- sst_temp[i, 'yr']
#   if (temp$month %in% c(9:12)) {
#     temp$period <- as.character(paste(as.character(temp$type), year_num, sep = ''))
#   } else {
#     temp$period <- paste(as.character(temp$type), year_num-1, sep = '')
#   }
#   sst_yr <- rbind(sst_yr, temp)
# }
# 
# sst_yr <- sst_yr %>%
#   group_by(period) %>%
#   dplyr::summarize(sst = mean(sst, na.rm = T)) %>%
#   transform(adj_year = parse_number(period), type = gsub("^\\d+|\\d+$", "", period)) %>%
#   dplyr::select(-period) %>%
#   dcast(adj_year ~ type, value.var = 'sst')
# 
# sst <- sst_yr %>%
#   merge(sst_seas, by = 'adj_year')
# colnames(sst) <- c('adj_year', paste('sst', c('NB', 'fall', 'spring', 'summer', 'winter'), sep = '_'))

####SST: needs to be adjusted bc data starts in 2006
# reg <- 'west'
# sst_raw <- read.csv(here::here('data', 'Ocean', paste0('sst_data_', reg, '.csv')), 
#                      header = T, stringsAsFactors = F) %>%
#   transform(year = year(date), month = month(date))%>%
#   filter(year > 1999 & year < 2019) %>% dplyr::select(-c(date)) %>%
#   transform(yr = as.numeric(factor(year))+6) %>% #add 7 to bump up from 2006 start
#   transform(type = ifelse(month %in% NB, 'NB', 'B')) %>%
#   transform(season = ifelse(month %in% winter, 'winter', ifelse(month %in% spring, 'spring',
#                                                                 ifelse(month %in% summer, 'summer', 'fall')))) 
# 
# ##season level
# sst_seas <- data.frame()
# sst_temp <- sst_raw #%>% filter(season %in% c('winter', 'spring'))
# for (i in 1:nrow(sst_temp)) {
#   temp <- sst_temp[i,]
#   year_num <- sst_temp[i, 'yr']
#   if (temp$season %in% c('winter', 'spring')) {
#     temp$season_yr <- paste(temp$season, year_num-1, sep = '')
#   } else {
#     temp$season_yr <- as.character(paste(as.character(temp$season), year_num, sep = ''))
#   }
#   sst_seas <- rbind(sst_seas, temp)
# }
# 
# sst_seas <- sst_seas %>%
#   group_by(season_yr) %>%
#   dplyr::summarize(sst = mean(sst, na.rm = T)) %>%
#   transform(adj_year = parse_number(season_yr), season = gsub("^\\d+|\\d+$", "", season_yr)) %>%
#   dplyr::select(-season_yr) %>%
#   dcast(adj_year ~ season, value.var = 'sst')
# 
# ##year-level (non-breeding only)
# sst_yr <- data.frame()
# sst_temp <- sst_raw %>% filter(type == 'NB')
# for (i in 1:nrow(sst_temp)) {
#   temp <- sst_temp[i,]
#   year_num <- sst_temp[i, 'yr']
#   if (temp$month %in% c(9:12)) {
#     temp$period <- as.character(paste(as.character(temp$type), year_num, sep = ''))
#   } else {
#     temp$period <- paste(as.character(temp$type), year_num-1, sep = '')
#   }
#   sst_yr <- rbind(sst_yr, temp)
# }
# 
# sst_yr <- sst_yr %>%
#   group_by(period) %>%
#   dplyr::summarize(sst = mean(sst, na.rm = T)) %>%
#   transform(adj_year = parse_number(period), type = gsub("^\\d+|\\d+$", "", period)) %>%
#   dplyr::select(-period) %>%
#   dcast(adj_year ~ type, value.var = 'sst')
# 
# sst <- sst_yr %>%
#   merge(sst_seas, by = 'adj_year')
# colnames(sst) <- c('adj_year', paste('sst', c('NB', 'fall', 'spring', 'summer', 'winter'), sep = '_'))


#####scalar wind
scalar_raw <- read.csv(here::here('data', 'Ocean', paste0('scalarWind_data_', reg, '.csv')), 
                     header = T, stringsAsFactors = F) %>%
  transform(year = year(date), month = month(date))%>%
  filter(year > 1999 & year < 2019) %>% dplyr::select(-c(date)) %>%
  transform(yr = as.numeric(factor(year))) %>%
  transform(type = ifelse(month %in% NB, 'NB', 'B')) %>%
  transform(season = ifelse(month %in% winter, 'winter', ifelse(month %in% spring, 'spring',
                                                                ifelse(month %in% summer, 'summer', 'fall')))) 

##season level
scalar_seas <- data.frame()
scalar_temp <- scalar_raw #%>% filter(season %in% c('winter', 'spring'))
for (i in 1:nrow(scalar_temp)) {
  temp <- scalar_temp[i,]
  year_num <- scalar_temp[i, 'yr']
  if (temp$season %in% c('winter', 'spring')) {
    temp$season_yr <- paste(temp$season, year_num-1, sep = '')
  } else {
    temp$season_yr <- as.character(paste(as.character(temp$season), year_num, sep = ''))
  }
  scalar_seas <- rbind(scalar_seas, temp)
}

scalar_seas <- scalar_seas %>%
  group_by(season_yr) %>%
  dplyr::summarize(scalar = mean(scale, na.rm = T)) %>%
  transform(adj_year = parse_number(season_yr), season = gsub("^\\d+|\\d+$", "", season_yr)) %>%
  dplyr::select(-season_yr) %>%
  dcast(adj_year ~ season, value.var = 'scalar')

##year-level (non-breeding only)
scalar_yr <- data.frame()
scalar_temp <- scalar_raw %>% filter(type == 'NB')
for (i in 1:nrow(scalar_temp)) {
  temp <- scalar_temp[i,]
  year_num <- scalar_temp[i, 'yr']
  if (temp$month %in% c(9:12)) {
    temp$period <- as.character(paste(as.character(temp$type), year_num, sep = ''))
  } else {
    temp$period <- paste(as.character(temp$type), year_num-1, sep = '')
  }
  scalar_yr <- rbind(scalar_yr, temp)
}

scalar_yr <- scalar_yr %>%
  group_by(period) %>%
  dplyr::summarize(scalar = mean(scale, na.rm = T)) %>%
  transform(adj_year = parse_number(period), type = gsub("^\\d+|\\d+$", "", period)) %>%
  dplyr::select(-period) %>%
  dcast(adj_year ~ type, value.var = 'scalar')

scalar <- scalar_yr %>%
  merge(scalar_seas, by = 'adj_year')
colnames(scalar) <- c('adj_year', paste('scalar', c('NB', 'fall', 'spring', 'summer', 'winter'), sep = '_'))


#####vwnd
vwnd_raw <- read.csv(here::here('data', 'Ocean', paste0('vwnd_data_', reg, '.csv')), 
                     header = T, stringsAsFactors = F) %>%
  transform(year = year(date), month = month(date))%>%
  filter(year > 1999 & year < 2019) %>% dplyr::select(-c(date)) %>%
  transform(yr = as.numeric(factor(year))) %>%
  transform(type = ifelse(month %in% NB, 'NB', 'B')) %>%
  transform(season = ifelse(month %in% winter, 'winter', ifelse(month %in% spring, 'spring',
                                                                ifelse(month %in% summer, 'summer', 'fall')))) 

##season level
vwnd_seas <- data.frame()
vwnd_temp <- vwnd_raw 
for (i in 1:nrow(vwnd_temp)) {
  temp <- vwnd_temp[i,]
  year_num <- vwnd_temp[i, 'yr']
  if (temp$season %in% c('winter', 'spring')) {
    temp$season_yr <- paste(temp$season, year_num-1, sep = '')
  } else {
    temp$season_yr <- as.character(paste(as.character(temp$season), year_num, sep = ''))
  }
  vwnd_seas <- rbind(vwnd_seas, temp)
}

vwnd_seas <- vwnd_seas %>%
  group_by(season_yr) %>%
  dplyr::summarize(vwnd = mean(vwnd, na.rm = T)) %>%
  transform(adj_year = parse_number(season_yr), season = gsub("^\\d+|\\d+$", "", season_yr)) %>%
  dplyr::select(-season_yr) %>%
  dcast(adj_year ~ season, value.var = 'vwnd')

##year-level (non-breeding only)
vwnd_yr <- data.frame()
vwnd_temp <- vwnd_raw %>% filter(type == 'NB')
for (i in 1:nrow(vwnd_temp)) {
  temp <- vwnd_temp[i,]
  year_num <- vwnd_temp[i, 'yr']
  if (temp$month %in% c(9:12)) {
    temp$period <- as.character(paste(as.character(temp$type), year_num, sep = ''))
  } else {
    temp$period <- paste(as.character(temp$type), year_num-1, sep = '')
  }
  vwnd_yr <- rbind(vwnd_yr, temp)
}

vwnd_yr <- vwnd_yr %>%
  group_by(period) %>%
  dplyr::summarize(vwnd = mean(vwnd, na.rm = T)) %>%
  transform(adj_year = parse_number(period), type = gsub("^\\d+|\\d+$", "", period)) %>%
  dplyr::select(-period) %>%
  dcast(adj_year ~ type, value.var = 'vwnd')

vwnd <- vwnd_yr %>%
  merge(vwnd_seas, by = 'adj_year')
colnames(vwnd) <- c('adj_year', paste('vwnd', c('NB', 'fall', 'spring', 'summer', 'winter'), sep = '_'))


#####uwnd
uwnd_raw <- read.csv(here::here('data', 'Ocean', paste0('uwnd_data_', reg, '.csv')), 
                     header = T, stringsAsFactors = F) %>%
  transform(year = year(date), month = month(date))%>%
  filter(year > 1999 & year < 2019) %>% dplyr::select(-c(date)) %>%
  transform(yr = as.numeric(factor(year))) %>%
  transform(type = ifelse(month %in% NB, 'NB', 'B')) %>%
  transform(season = ifelse(month %in% winter, 'winter', ifelse(month %in% spring, 'spring',
                                                                ifelse(month %in% summer, 'summer', 'fall')))) 

##season level
uwnd_seas <- data.frame()
uwnd_temp <- uwnd_raw 
for (i in 1:nrow(uwnd_temp)) {
  temp <- uwnd_temp[i,]
  year_num <- uwnd_temp[i, 'yr']
  if (temp$season %in% c('winter', 'spring')) {
    temp$season_yr <- paste(temp$season, year_num-1, sep = '')
  } else {
    temp$season_yr <- as.character(paste(as.character(temp$season), year_num, sep = ''))
  }
  uwnd_seas <- rbind(uwnd_seas, temp)
}

uwnd_seas <- uwnd_seas %>%
  group_by(season_yr) %>%
  dplyr::summarize(uwnd = mean(uwnd, na.rm = T)) %>%
  transform(adj_year = parse_number(season_yr), season = gsub("^\\d+|\\d+$", "", season_yr)) %>%
  dplyr::select(-season_yr) %>%
  dcast(adj_year ~ season, value.var = 'uwnd')

##year-level (non-breeding only)
uwnd_yr <- data.frame()
uwnd_temp <- uwnd_raw %>% filter(type == 'NB')
for (i in 1:nrow(uwnd_temp)) {
  temp <- uwnd_temp[i,]
  year_num <- uwnd_temp[i, 'yr']
  if (temp$month %in% c(9:12)) {
    temp$period <- as.character(paste(as.character(temp$type), year_num, sep = ''))
  } else {
    temp$period <- paste(as.character(temp$type), year_num-1, sep = '')
  }
  uwnd_yr <- rbind(uwnd_yr, temp)
}

uwnd_yr <- uwnd_yr %>%
  group_by(period) %>%
  dplyr::summarize(uwnd = mean(uwnd, na.rm = T)) %>%
  transform(adj_year = parse_number(period), type = gsub("^\\d+|\\d+$", "", period)) %>%
  dplyr::select(-period) %>%
  dcast(adj_year ~ type, value.var = 'uwnd')

uwnd <- uwnd_yr %>%
  merge(uwnd_seas, by = 'adj_year')
colnames(uwnd) <- c('adj_year', paste('uwnd', c('NB', 'fall', 'spring', 'summer', 'winter'), sep = '_'))


#####curl
curl_raw <- read.csv(here::here('data', 'Ocean', paste0('curl_data_', reg, '.csv')), 
                     header = T, stringsAsFactors = F) %>%
  transform(year = year(date), month = month(date))%>%
  filter(year > 1999 & year < 2019) %>% dplyr::select(-c(date)) %>%
  transform(yr = as.numeric(factor(year))) %>%
  transform(type = ifelse(month %in% NB, 'NB', 'B')) %>%
  transform(season = ifelse(month %in% winter, 'winter', ifelse(month %in% spring, 'spring',
                                                                ifelse(month %in% summer, 'summer', 'fall')))) 

##season level
curl_seas <- data.frame()
curl_temp <- curl_raw 
for (i in 1:nrow(curl_temp)) {
  temp <- curl_temp[i,]
  year_num <- curl_temp[i, 'yr']
  if (temp$season %in% c('winter', 'spring')) {
    temp$season_yr <- paste(temp$season, year_num-1, sep = '')
  } else {
    temp$season_yr <- as.character(paste(as.character(temp$season), year_num, sep = ''))
  }
  curl_seas <- rbind(curl_seas, temp)
}

curl_seas <- curl_seas %>%
  group_by(season_yr) %>%
  dplyr::summarize(curl = mean(curl, na.rm = T)) %>%
  transform(adj_year = parse_number(season_yr), season = gsub("^\\d+|\\d+$", "", season_yr)) %>%
  dplyr::select(-season_yr) %>%
  dcast(adj_year ~ season, value.var = 'curl')

##year-level (non-breeding only)
curl_yr <- data.frame()
curl_temp <- curl_raw %>% filter(type == 'NB')
for (i in 1:nrow(curl_temp)) {
  temp <- curl_temp[i,]
  year_num <- curl_temp[i, 'yr']
  if (temp$month %in% c(9:12)) {
    temp$period <- as.character(paste(as.character(temp$type), year_num, sep = ''))
  } else {
    temp$period <- paste(as.character(temp$type), year_num-1, sep = '')
  }
  curl_yr <- rbind(curl_yr, temp)
}

curl_yr <- curl_yr %>%
  group_by(period) %>%
  dplyr::summarize(curl = mean(curl, na.rm = T)) %>%
  transform(adj_year = parse_number(period), type = gsub("^\\d+|\\d+$", "", period)) %>%
  dplyr::select(-period) %>%
  dcast(adj_year ~ type, value.var = 'curl')

curl <- curl_yr %>%
  merge(curl_seas, by = 'adj_year')
colnames(curl) <- c('adj_year', paste('curl', c('NB', 'fall', 'spring', 'summer', 'winter'), sep = '_'))


#productivity
prod_raw <- read.csv(here::here('data', 'Ocean', paste0('prod_data_', reg, '.csv')), 
                     header = T, stringsAsFactors = F) %>%
  transform(year = year(date), month = month(date))%>%
  filter(year > 1999 & year < 2019) %>% dplyr::select(-c(date)) %>%
  transform(yr = as.numeric(factor(year))) %>%
  transform(type = ifelse(month %in% NB, 'NB', 'B')) %>%
  transform(season = ifelse(month %in% winter, 'winter', ifelse(month %in% spring, 'spring',
                                                                ifelse(month %in% summer, 'summer', 'fall')))) 

##season level
prod_seas <- data.frame()
prod_temp <- prod_raw 
for (i in 1:nrow(prod_temp)) {
  temp <- prod_temp[i,]
  year_num <- prod_temp[i, 'yr']
  if (temp$season %in% c('winter', 'spring')) {
    temp$season_yr <- paste(temp$season, year_num-1, sep = '')
  } else {
    temp$season_yr <- as.character(paste(as.character(temp$season), year_num, sep = ''))
  }
  prod_seas <- rbind(prod_seas, temp)
}

prod_seas <- prod_seas %>%
  group_by(season_yr) %>%
  dplyr::summarize(prod = mean(prod, na.rm = T)) %>%
  transform(adj_year = parse_number(season_yr), season = gsub("^\\d+|\\d+$", "", season_yr)) %>%
  dplyr::select(-season_yr) %>%
  dcast(adj_year ~ season, value.var = 'prod')

##year-level (non-breeding only)
prod_yr <- data.frame()
prod_temp <- prod_raw %>% filter(type == 'NB')
for (i in 1:nrow(prod_temp)) {
  temp <- prod_temp[i,]
  year_num <- prod_temp[i, 'yr']
  if (temp$month %in% c(9:12)) {
    temp$period <- as.character(paste(as.character(temp$type), year_num, sep = ''))
  } else {
    temp$period <- paste(as.character(temp$type), year_num-1, sep = '')
  }
  prod_yr <- rbind(prod_yr, temp)
}

prod_yr <- prod_yr %>%
  group_by(period) %>%
  dplyr::summarize(prod = mean(prod, na.rm = T)) %>%
  transform(adj_year = parse_number(period), type = gsub("^\\d+|\\d+$", "", period)) %>%
  dplyr::select(-period) %>%
  dcast(adj_year ~ type, value.var = 'prod')

prod <- prod_yr %>%
  merge(prod_seas, by = 'adj_year')
colnames(prod) <- c('adj_year', paste('prod', c('NB', 'fall', 'spring', 'summer', 'winter'), sep = '_'))

####chlorophyll
chla_raw <- read.csv(here::here('data', 'Ocean', paste0('chla_data_', reg, '.csv')), 
                     header = T, stringsAsFactors = F) %>%
  transform(year = year(date), month = month(date))%>%
  filter(year > 1999 & year < 2019) %>% dplyr::select(-c(date)) %>%
  transform(yr = as.numeric(factor(year))) %>%
  transform(type = ifelse(month %in% NB, 'NB', 'B')) %>%
  transform(season = ifelse(month %in% winter, 'winter', ifelse(month %in% spring, 'spring',
                                                                ifelse(month %in% summer, 'summer', 'fall')))) 
##season level
chla_seas <- data.frame()
chla_temp <- chla_raw 
for (i in 1:nrow(chla_temp)) {
  temp <- chla_temp[i,]
  year_num <- chla_temp[i, 'yr']
  if (temp$season %in% c('winter', 'spring')) {
    temp$season_yr <- paste(temp$season, year_num-1, sep = '')
  } else {
    temp$season_yr <- as.character(paste(as.character(temp$season), year_num, sep = ''))
  }
  chla_seas <- rbind(chla_seas, temp)
}

chla_seas <- chla_seas %>%
  group_by(season_yr) %>%
  dplyr::summarize(chla = mean(chla, na.rm = T)) %>%
  transform(adj_year = parse_number(season_yr), season = gsub("^\\d+|\\d+$", "", season_yr)) %>%
  dplyr::select(-season_yr) %>%
  dcast(adj_year ~ season, value.var = 'chla')

##year-level (non-breeding only)
chla_yr <- data.frame()
chla_temp <- chla_raw %>% filter(type == 'NB')
for (i in 1:nrow(chla_temp)) {
  temp <- chla_temp[i,]
  year_num <- chla_temp[i, 'yr']
  if (temp$month %in% c(9:12)) {
    temp$period <- as.character(paste(as.character(temp$type), year_num, sep = ''))
  } else {
    temp$period <- paste(as.character(temp$type), year_num-1, sep = '')
  }
  chla_yr <- rbind(chla_yr, temp)
}

chla_yr <- chla_yr %>%
  group_by(period) %>%
  dplyr::summarize(chla = mean(chla, na.rm = T)) %>%
  transform(adj_year = parse_number(period), type = gsub("^\\d+|\\d+$", "", period)) %>%
  dplyr::select(-period) %>%
  dcast(adj_year ~ type, value.var = 'chla')

chla <- chla_yr %>%
  merge(chla_seas, by = 'adj_year')
colnames(chla) <- c('adj_year', paste('chla', c('NB', 'fall', 'spring', 'summer', 'winter'), sep = '_'))

###all combined; adjusted year is the phi period (adj year == 1 is pups born in 2000 surviving to 2001)
#fall and summer adj_year 0 == NA (would be Sept-Dec 1999)
#adj_year 0 is from jan-may of 2000, which would affect 1999 pups
#spring and winter adj_year 18 == NA (would be Jan-May 2017)
#if want to study lag time (pups born in June 2000 strongly affected by mom's forage availability in 
# winter 1999-2000 rather than first solo foraging winter 2000-2001), get June-Dec 1999 data 

ocean_all_adj <- PDO %>%
  merge(ALBSA, by = 'adj_year', all = T) %>%
  merge(upwell, by = 'adj_year', all = T) %>%
  merge(AOI, by = 'adj_year', all = T) %>%
  merge(MEI, by = 'adj_year', all = T) %>%
  merge(NPGO, by = 'adj_year', all = T) %>%
  merge(NOI, by = 'adj_year', all = T) %>%
  merge(NPI, by = 'adj_year', all = T) %>%
  merge(sst, by = 'adj_year', all = T) %>%
  merge(chla, by = 'adj_year', all = T) %>%
  merge(vwnd, by = 'adj_year', all = T) %>%
  merge(uwnd, by = 'adj_year', all = T) %>%
  merge(scalar, by = 'adj_year', all = T) %>%
  merge(curl, by = 'adj_year', all = T) %>%
  merge(prod, by = 'adj_year', all = T) %>%
  filter(adj_year > 0 & adj_year < 19)

pup_ref_years <- c('00-01', '01-02', '02-03', '03-04', '04-05', '05-06', '06-07', '07-08',
               '08-09', '09-10', '10-11', '11-12', '12-13', '13-14', '14-15', '15-16', '16-17', '17-18')
ocean_all_adj$ref_year <- pup_ref_years

#checks
# ggplot(ocean_all_adj, aes(adj_year, curl_winter)) +
#   geom_line()

# write.csv(ocean_all_adj, 
#           file = here::here('data', 'ProcData', paste0('ocean_all_adj_', reg, '.csv')), row.names = F)

# plot_dat <- ocean_all_adj %>%
#   transform(temp = scale(temp_NB), sst = scale(sst_NB)) %>%
#   dplyr::select(adj_year, temp, sst) %>%
#   melt(id.vars = 'adj_year')
# ggplot(plot_dat, aes(factor(adj_year), value, color = variable, group = variable)) + geom_point()


