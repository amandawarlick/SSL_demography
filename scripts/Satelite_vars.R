
require(lubridate)
require(maps)
require(mapdata)
require(maptools)
require(mapproj)
require(ncdf4)
require(sp)
require(dplyr)
require(devtools)
library(rerddap)
library(plotdap)
library(rerddapXtracto)
library(here)
# rerddap::cache_delete_all(force = TRUE) # if encounter an error reading the nc file clear the rerrdap cache:

#####update 4/2021
### boxes informed by discussions with Michelle

##EASTERN aleutians
#for drawing the map
# xcoord_east_high <- c(-170.5,-159.25)
# ycoord_east_high <- c(55.5, 58)
# xcoord_east_low <- c(-169, -145.5)
# ycoord_east_low <- c(50, 60)
# xcoord_east_mid2 <- -156
# ycoord_east_mid2 <- 53.5
# xcoord_east_mid <- -150
# ycoord_east_mid <- 56.5

#for pulling the variables
xcoord <- c(-170.5, -169, -156, -150, -145.5, -159.25)
ycoord <- c(55.5, 50, 53.5, 56.5, 59.975, 58)
zcoord <- c(0,0)

#WESTERN; agattu and ulak
#from the map
# xcoord_west <- c(-190.6, -177)
# ycoord_west <- c(50.00, 55)

#data pull
#upper left, lower left, lower right, upper right
# xcoord <- c(190.6, 190.6, -178, -178)
# xcoord <- c(170, 170, -178, -178)
xcoord <- c(170, 170, 183, 183) #this would be -175 rather than -178
ycoord <- c(55, 50, 50, 55)
zcoord <- c(0,0)

reg <- 'east'
reg <- 'west'

#####productivity
dataInfo <- rerddap::info('erdPPbfp1mday')  #1997-2010
parameter <- dataInfo$variable$variable_name[1] #just want prod

global <- dataInfo$alldata$NC_GLOBAL #start and end times
tt <- global[ global$attribute_name %in% c('time_coverage_end','time_coverage_start'), "value", ]
tcoord <- c(tt[2],"last")

prod1 <- rxtractogon(dataInfo,parameter=parameter,
                    tcoord=tcoord, zcoord = zcoord,
                    xcoord=xcoord, ycoord=ycoord) 

# plotBBox(prod1, maxpixels = 1000000)

# spatially average all the data within the box for each dataset  
prod1$avg <- apply(prod1$productivity, c(3),function(x) mean(x,na.rm=TRUE))

plot(as.Date(prod1$time), scale(prod1$avg),
     type='b', bg="blue", pch=21, xlab="", cex=.7,
     xlim=as.Date(c("1997-01-01","2019-01-01")),
     #   ylim=c(10,25),
     ylab="Chlorophyll", main=ttext)

prod1_data <- data.frame(date = prod1$time, prod1 = prod1$avg)

#productivity 2 for second half of study period
dataInfo <- rerddap::info('erdMH1ppmday')  #2003-present

parameter <- dataInfo$variable$variable_name[1] #just want prod

global <- dataInfo$alldata$NC_GLOBAL #start and end times
tt <- global[ global$attribute_name %in% c('time_coverage_end','time_coverage_start'), "value", ]
tcoord <- c(tt[2],"last")

prod2 <- rxtractogon(dataInfo,parameter=parameter,
                    tcoord=tcoord, zcoord = zcoord,
                    xcoord=xcoord, ycoord=ycoord) 

plotBBox(prod2, maxpixels = 1000000)

# spatially average all the data within the box for each dataset  
prod2$avg <- apply(prod2$productivity, c(3),function(x) mean(x,na.rm=TRUE))

plot(as.Date(prod2$time), scale(prod2$avg),
     type='b', bg="blue", pch=21, xlab="", cex=.7,
     xlim=as.Date(c("1997-01-01","2019-01-01")),
     #   ylim=c(10,25),
     ylab="Prod", main=ttext)

prod2_data <- data.frame(date = prod2$time, prod2 = prod2$avg)

prod_data <- prod2_data %>%
  merge(prod1_data, by = 'date', all = T) %>%
  transform(prod = ifelse(is.na(prod2), prod1, ifelse(is.na(prod1), prod2, (prod1+prod2)/2))) %>%
  dplyr::select(date, prod)

write.csv(prod_data, file = here::here('SSL_CJS', 'Data', 'Ocean', 'New', 
                                       paste0('prod_data_', reg, '.csv')), row.names = F)


#####chla
dataInfo <- rerddap::info('pmlEsaCCI42OceanColorMonthly')  #1997-2020; chlor_a

parameter <- dataInfo$variable$variable_name[1] #just want chla

global <- dataInfo$alldata$NC_GLOBAL #start and end times
tt <- global[ global$attribute_name %in% c('time_coverage_end','time_coverage_start'), "value", ]
tcoord <- c(tt[2],"last")

chla <- rxtractogon(dataInfo,parameter=parameter,
                     tcoord=tcoord, #zcoord = zcoord,
                     xcoord=xcoord, ycoord=ycoord) 

plotBBox(chla, maxpixels = 1000000)

# spatially average all the data within the box for each dataset  
chla$avg <- apply(chla$chlor_a, c(3),function(x) mean(x,na.rm=TRUE))

plot(as.Date(chla$time), scale(chla$avg),
     type='b', bg="blue", pch=21, xlab="", cex=.7,
     xlim=as.Date(c("1997-01-01","2019-01-01")),
     #   ylim=c(10,25),
     ylab="Chlorophyll", main=ttext)

chla_data <- data.frame(date = chla$time, chla = chla$avg)

write.csv(chla_data, file = here::here('SSL_CJS', 'Data', 'Ocean', 'New', 
                                       paste0('chla_data_', reg, '.csv')), row.names = F)

####wind: http://colaweb.gmu.edu/dev/clim301/lectures/wind/wind-uv

#scalar; measure of wind velocity
dataInfo <- rerddap::info('esrlIcoads2ge')
parameter <- dataInfo$variable$variable_name[21] # a little coarse
global <- dataInfo$alldata$NC_GLOBAL
tt <- global[ global$attribute_name %in% c('time_coverage_end','time_coverage_start'), "value", ]
tcoord <- c(tt[2],"last")

scale <- rxtractogon(dataInfo,parameter=parameter,
                    tcoord=tcoord, #zcoord = zcoord,
                    xcoord=xcoord,ycoord=ycoord)
plotBBox(scale, maxpixels = 100000)

# Now spatially average all the data within the box for each dataset
scale$avg <- apply(scale$wspd, c(3),function(x) mean(x,na.rm=TRUE))
scale_data <- data.frame(date = scale$time, scale = scale$avg) %>%
  filter(date > 1990)

plot(as.Date(scale$time), scale(scale$avg),
     type='b', bg="blue", pch=21, xlab="", cex=.7,
     xlim=as.Date(c("1997-01-01","2019-01-01")),
     #   ylim=c(10,25),
     ylab="")

write.csv(scale_data, file = here::here('SSL_CJS', 'Data', 'Ocean', 'New', 
                                       paste0('scalarWind_data_', reg, '.csv')), row.names = F)

#curl; measure of wind stress
dataInfo <- rerddap::info('erdlasFnWind20') #curl, uwnd, vwnd
parameter <- dataInfo$variable$variable_name[1] 
global <- dataInfo$alldata$NC_GLOBAL
tt <- global[ global$attribute_name %in% c('time_coverage_end','time_coverage_start'), "value", ]
tcoord <- c(tt[2],"last")

curl <- rxtractogon(dataInfo,parameter=parameter,
                    tcoord=tcoord, #zcoord = zcoord,
                    xcoord=xcoord,ycoord=ycoord)
plotBBox(curl, maxpixels = 100000)

# Now spatially average all the data within the box for each dataset
curl$avg <- apply(curl$curl, c(3),function(x) mean(x,na.rm=TRUE))
curl_data <- data.frame(date = curl$time, curl = curl$avg)

plot(as.Date(curl$time), scale(curl$avg),
     type='b', bg="blue", pch=21, xlab="", cex=.7,
     xlim=as.Date(c("1997-01-01","2019-01-01")),
     #   ylim=c(10,25),
     ylab="")

write.csv(curl_data, file = here::here('SSL_CJS', 'Data', 'Ocean', 'New', 
                                       paste0('curl_data_', reg, '.csv')), row.names = F)

#uwnd; eastward wind
parameter <- dataInfo$variable$variable_name[4] #uwnd
global <- dataInfo$alldata$NC_GLOBAL
tt <- global[ global$attribute_name %in% c('time_coverage_end','time_coverage_start'), "value", ]
tcoord <- c(tt[2],"last")

uwnd <- rxtractogon(dataInfo,parameter=parameter,
                    tcoord=tcoord, #zcoord = zcoord,
                    xcoord=xcoord,ycoord=ycoord)
plotBBox(uwnd, maxpixels = 100000)

# Now spatially average all the data within the box for each dataset
uwnd$avg <- apply(uwnd$u_mean, c(3),function(x) mean(x,na.rm=TRUE))
uwnd_data <- data.frame(date = uwnd$time, uwnd = uwnd$avg)

plot(as.Date(uwnd$time), scale(uwnd$avg),
     type='b', bg="blue", pch=21, xlab="", cex=.7,
     xlim=as.Date(c("1997-01-01","2019-01-01")),
     #   ylim=c(10,25),
     ylab="")

write.csv(uwnd_data, file = here::here('SSL_CJS', 'Data', 'Ocean', 'New', 
                                       paste0('uwnd_data_', reg, '.csv')), row.names = F)

#vwnd; northward wind 
parameter <- dataInfo$variable$variable_name[6] #vwnd
global <- dataInfo$alldata$NC_GLOBAL
tt <- global[ global$attribute_name %in% c('time_coverage_end','time_coverage_start'), "value", ]
tcoord <- c(tt[2],"last")

vwnd <- rxtractogon(dataInfo,parameter=parameter,
                   tcoord=tcoord, #zcoord = zcoord,
                   xcoord=xcoord,ycoord=ycoord)
plotBBox(vwnd, maxpixels = 100000)

# Now spatially average all the data within the box for each dataset
vwnd$avg <- apply(vwnd$v_mean, c(3),function(x) mean(x,na.rm=TRUE))
vwnd_data <- data.frame(date = vwnd$time, vwnd = vwnd$avg)

plot(as.Date(vwnd$time), scale(vwnd$avg),
     type='b', bg="blue", pch=21, xlab="", cex=.7,
     xlim=as.Date(c("1997-01-01","2019-01-01")),
     #   ylim=c(10,25),
     ylab="")

write.csv(vwnd_data, file = here::here('SSL_CJS', 'Data', 'Ocean', 'New', 
                                       paste0('vwnd_data_', reg, '.csv')), row.names = F)


### SST

#commented out so remember to pick one for east/west
#for east
# dataInfo <- rerddap::info('erdGAsstamday') #original.... land temp? works for eastern, patchy for west
# parameter <- dataInfo$variable$variable_name #sst
# 
#for west
# dataInfo <- rerddap::info('erdMBsstdmday') #2006-present
# parameter <- dataInfo$variable$variable_name
# 
# global <- dataInfo$alldata$NC_GLOBAL
# tt <- global[ global$attribute_name %in% c('time_coverage_end','time_coverage_start'), "value", ]
# tcoord <- c(tt[2],"last")

sst <- rxtractogon(dataInfo,parameter=parameter,
                             tcoord=tcoord, zcoord = zcoord,
                             xcoord=xcoord,ycoord=ycoord)

plotBBox(sst, maxpixels = 1000000)

# Now spatially average all the data within the box for each dataset
sst$avg <- apply(sst$sst, c(3),function(x) mean(x,na.rm=TRUE))

plot(as.Date(sst$time), scale(sst$avg),
     type='b', bg="blue", pch=21, xlab="", cex=.7,
     xlim=as.Date(c("1997-01-01","2019-01-01")),
     #   ylim=c(10,25),
     ylab="sst")

sst_data <- data.frame(date = sst$time, sst = sst$avg) %>%
  filter(date > 1990-01-16)

write.csv(sst_data, file = here::here('SSL_CJS', 'Data', 'Ocean', 'New', 
                                       paste0('sst_data_', reg, '.csv')), row.names = F)


#Mixed layer depth
path <- 'SSL_CJS/Data/Ocean/New/Copernicus/'
flist <- list.files(path = path, pattern = '^.*\\.(nc|NC|Nc|Nc)$')
cop <- flist[1] #look at all the files and pick the right one
nc <- nc_open(paste0(path, cop))

# attributes(nc)$names
# attributes(nc$var)$names
# nc$ndims
# nc$natts #netcdf attributes

#data goes from 1/1999 to 12/2019 2019-1999 = 21 years, 12 months each 
#mlotst (mixed layer depth, meters)
nyears <- 2019-1999+1

mld <- ncvar_get(nc, attributes(nc$var)$names[1]) #from -1 to 11 'height' in db

#each matrix slice is a month-year combination, so mean over c(1,2)?
#no clue what dims 1 and 2 are
mld_mean <- apply(mld, c(3), function(x) mean(x, na.rm = TRUE))

mld_data <- data.frame(mld = mld_mean, year = rep(1999:2019, each = 12), 
                       month = rep(1:12, times = nyears)) 

#zo (absolute/geopotential height, meters)
ssh <- ncvar_get(nc, attributes(nc$var)$names[2])
ssh_mean <- apply(ssh, c(3), function(x) mean(x, na.rm = TRUE))
ssh_data <- data.frame(ssh = ssh_mean, year = rep(1999:2019, each = 12), 
                       month = rep(1:12, times = nyears))

#to (temp, deg C)
temp <- ncvar_get(nc, attributes(nc$var)$names[3])
temp_mean <- apply(temp, c(3), function(x) mean(x, na.rm = TRUE))
temp_data <- data.frame(temp = temp_mean, year = rep(1999:2019, each = 12), 
                       month = rep(1:12, times = nyears))

#so (salinity -- )
so <- ncvar_get(nc, attributes(nc$var)$names[4])
so_mean <- apply(so, c(3), function(x) mean(x, na.rm = TRUE))
so_data <- data.frame(so = so_mean, year = rep(1999:2019, each = 12), 
                        month = rep(1:12, times = nyears))

#ugo (geostrophic wind -- zonal/east, m/s)
uwnd <- ncvar_get(nc, attributes(nc$var)$names[5])
uwnd_mean <- apply(uwnd, c(3), function(x) mean(x, na.rm = TRUE))
uwnd_data <- data.frame(uwnd = uwnd_mean, year = rep(1999:2019, each = 12), 
                        month = rep(1:12, times = nyears))

#vgo (geostrophic wind -- meridional/north, m/s)
vwnd <- ncvar_get(nc, attributes(nc$var)$names[6])
vwnd_mean <- apply(vwnd, c(3), function(x) mean(x, na.rm = TRUE))
vwnd_data <- data.frame(vwnd = vwnd_mean, year = rep(1999:2019, each = 12), 
                        month = rep(1:12, times = nyears))

copernicus_dat <- vwnd_data %>%
  merge(uwnd_data, by = c('year', 'month')) %>%
  merge(temp_data, by = c('year', 'month')) %>%
  merge(mld_data, by = c('year', 'month')) %>%
  merge(ssh_data, by = c('year', 'month')) %>%
  merge(so_data, by = c('year', 'month'))

# write.csv(copernicus_dat,
# file = here::here('SSL_CJS', 'Data', 'Ocean', 'New', 'copernicus_dat_east.csv'), row.names = F)
# 
# write.csv(copernicus_dat, 
#           file = here::here('SSL_CJS', 'Data', 'Ocean', 'New', 'copernicus_dat_west.csv'), row.names = F)

