
library(dplyr)
library(lubridate)
library(reshape2)
library(tibble)
library(rgdal)
library(broom)
library(maptools)
library(rmapshaper)
library(data.table)
library(ggsn)

map_theme <- function(...) {
  theme(axis.line = element_blank(),
        strip.text = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_blank(), legend.position = 'bottom',
        legend.key = element_blank(), panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 1))
}


all_data <- read.csv(here::here('SSL_CJS', "Data", "ProcData", "class_dat.csv"), stringsAsFactors = F) %>%
  dplyr::select(-c(NP_sum, diff)) %>%
  transform(Rookery = ifelse(Rookery == 1, "Rookery", "Haul-out")) %>%
  transform(Rookery = ifelse(is.na(Rookery), "Haul-out", Rookery)) %>%
  filter(Obs == 1) %>%
  transform(LON = ifelse(LON > 0, -180-(180-LON), LON)) %>%
  filter(!is.na(Bull) & Site != '') %>%
  filter(!is.na(SAM)) %>%
  dplyr::rename(SiteType = Rookery) %>%
  transform(Region = factor(Region, 
                            levels = c('W ALEU', 'C ALEU', 'E ALEU', 'W GULF', 'C GULF', 'E GULF', 'SE AK'),
                            labels = c('W. Aleu', 'C. Aleu', 'E. Aleu', 'W. Gulf', 'C. Gulf',
                                       'E. Gulf', 'SE AK'))) %>%
  transform(prop_pup = (Pup/NP_tot))

site_ref <- all_data[, c("Site", "Year", "Region", "Reg_no", "SiteType", "Obs", "Complex", "RCA", "LAT", "LON", "CH")]
row.names(site_ref) <- paste(site_ref[,1], site_ref[,2], sep = '-')
site_ref <- site_ref[, c("Year", "Region", "Reg_no", "SiteType", "Obs", "Complex", "RCA", "LAT", "LON", "CH")]

cov_ref <- all_data[, c("Site", "Year", "Region", "Reg_no", "SiteType", "Obs", "Complex", "RCA", "LAT", "LON", "CH")] 

cov_ref <- cov_ref %>%
  transform(SiteType = ifelse(Site %in% c('MARMOT', 'SUGARLOAF', 'UGAMAK/NORTH', 'SEAL ROCKS', 
                                          'CHISWELL ISLANDS', 'WOODED (FISH)'), 'Field Camp', SiteType)) %>%
  transform(SiteType = factor(SiteType, levels = c('Rookery', 'Haul-out', 'Field Camp')))

#state outline
canada <- map_data("world", region = "Canada") %>%
  dplyr::select(-c(region, subregion)) %>%
  transform(group = factor(group))

wd <- getwd()
dir_spatial <- paste0(wd, "/SSL_CJS/Data/")
dir_shp <- paste0(dir_spatial, "gz_2010_us_040_00_500k")

us_shp <- readOGR(dsn = dir_shp, layer = "gz_2010_us_040_00_500k")
ak_shp <- us_shp[us_shp$STATE == '02',]
ak <- tidy(ak_shp, region = "GEO_ID") %>%
  data.table() %>%
  transform(long_test = ifelse(long > 0, -180-(180-long), long)) %>%
  dplyr::select(-long) %>%
  rename(long = long_test) %>%
  dplyr::select(c(long, lat, group, order)) %>%
  bind_rows(canada)

us_tidy <- tidy(us_shp[us_shp$STATE != '02',], region = "GEO_ID") %>%
  data.table() %>%
  dplyr::select(c(long, lat, group, order)) %>%
  filter(long > -150)

north_am <- ak %>%
  bind_rows(us_tidy)

reg_loc <- cov_ref %>%
  group_by(Region) %>%
  dplyr::summarize(mean_long = mean(LON), mean_lat = mean(LAT)) %>%
  transform(Region = factor(Region, levels = c('W. Aleu', 'C. Aleu', 'E. Aleu',
                                               'W. Gulf', 'C. Gulf', 'E. Gulf', 'SE AK'),
                            labels = c('W. Aleutians', 'C. Aleutians', 'E. Aleutians',
                                       'W. Gulf', 'C. Gulf', 'E. Gulf', 'SE Alaska')))


#map with environmental variable boxes
#agattu and ulak
xcoord_west <- c(-190, -177)
ycoord_west <- c(50.00, 55)

# #eastern aleutians
xcoord_east_low <- c(-169, -145.5)
ycoord_east_low <- c(50.6, 60)
xcoord_east_high <- c(-170.5,-159.25)
ycoord_east_high <- c(55.5, 58)
xcoord_east_mid2 <- -156
ycoord_east_mid2 <- 53.5
xcoord_east_mid <- -150
ycoord_east_mid <- 56.5

#ak map with env boxes
ak_map <- ggplot() +
  geom_polygon(data = ak, aes(x = long, y = lat, group = group), fill = "grey93", color = "grey20", size = 0.3) +
  xlab("") + ylab("") +
  #western
  geom_segment(data = reg_loc,
               aes(x = xcoord_west[1], xend = xcoord_west[1], 
                   y = ycoord_west[2], yend = ycoord_west[1])) +
  geom_segment(data = reg_loc,
               aes(x = xcoord_west[2], xend = xcoord_west[2], 
                   y = ycoord_west[2], yend = ycoord_west[1])) +
  geom_segment(data = reg_loc,
               aes(x = xcoord_west[1], xend = xcoord_west[2], 
                   y = ycoord_west[1], yend = ycoord_west[1])) +
  geom_segment(data = reg_loc,
               aes(x = xcoord_west[1], xend = xcoord_west[2], 
                   y = ycoord_west[2], yend = ycoord_west[2])) +
  #eastern
  geom_segment(data = reg_loc,
               aes(x = xcoord_east_high[2], xend = xcoord_east_low[2], 
                   y = ycoord_east_high[2], yend = ycoord_east_low[2])) +
  geom_segment(data = reg_loc,
               aes(x = xcoord_east_high[1], xend = xcoord_east_high[2], 
                   y = ycoord_east_high[1], yend = ycoord_east_high[2])) +
  geom_segment(data = reg_loc,
               aes(x = xcoord_east_high[1], xend = xcoord_east_low[1], 
                   y = ycoord_east_high[1], yend = ycoord_east_low[1])) +
  geom_segment(data = reg_loc,
               aes(x = xcoord_east_low[2], xend = xcoord_east_mid,
                   y = ycoord_east_low[2], yend = ycoord_east_mid)) +
  geom_segment(data = reg_loc,
               aes(x = xcoord_east_low[1], xend = xcoord_east_mid2,
                   y = ycoord_east_low[1], yend = ycoord_east_mid2)) +
  geom_segment(data = reg_loc,
               aes(x = xcoord_east_mid2, xend = xcoord_east_mid,
                   y = ycoord_east_mid2, yend = ycoord_east_mid)) +
  coord_fixed(1.5, ylim = c(48, 67), xlim = c(-190, -131)) +
  scale_color_manual(values = c('black', 'salmon2', 'deepskyblue3')) + #or dodgerblue3
  scale_shape_manual(values = c(17, 16, 16)) +
  scale_size_manual(guide = 'none', values = c(3, 1, 1.5)) +
  #scale_y_continuous(limits = c(49, 63)) +
  map_theme() +
  scalebar(data = ak, dist = 25, dist_unit = "km",
           transform = FALSE, st.size = 4, location = "bottomleft")

#bigger us map
us_map <- ggplot() + 
  geom_polygon(data = north_am, aes(x = long, y = lat, group = group), fill = "grey80",
               color = NA, size = 0) +
  xlab("") + ylab("") +
  coord_fixed(1.5) +
  geom_rect(aes(ymin = 50, ymax = 63, xmin = -180, xmax = -145), fill = NA, colour = "black",
            size = 0.5) +
  scale_color_manual(values = c('black', 'salmon2', 'deepskyblue3')) + #or dodgerblue3
  scale_shape_manual(values = c(17, 16, 16)) +
  scale_size_manual(guide = 'none', values = c(3, 1, 1.5)) +
  # scale_alpha_manual(guide='none', values = c(1, 0.3, 0.3)) +
  #scale_y_continuous(limits = c(49, 63)) +
  theme_void() +
  map_theme()

#paper map -- playing with how C Aleu is represented
# cov_ref_new <- cov_ref %>%
#   transform(SiteType = ifelse(Region %in% c('W. Aleu'), 
#                               'Camera trap', as.character(SiteType))) %>%
#   transform(PopTrend = ifelse(Region %in% c('W. Aleu', 'C. Aleu'), 
#                               'Decreasing', 'Stable/Increasing'))
# 
# ggplot() +
#   geom_polygon(data = ak, aes(x = long, y = lat, group = group), fill = "grey93", color = "grey20", size = 0.3) +
#   geom_point(data = cov_ref %>% 
#                filter(SiteType == 'Rookery'),
#              aes(x = LON, y = LAT, shape = SiteType, size = SiteType,
#                  fill = SiteType, group = rev(SiteType))) +
#   xlab("") + ylab("") +
#   geom_point(data = cov_ref_new %>% filter(SiteType == 'Field Camp'),
#              aes(x = LON, y = LAT, shape = SiteType, size = SiteType,
#                  fill = SiteType, group = rev(SiteType))) +
#   geom_point(data = cov_ref_new %>% 
#                filter(Site %in% c('AGATTU/CAPE SABAK')),
#              aes(x = LON, y = LAT, shape = SiteType, size = SiteType,
#                  fill = SiteType, group = rev(SiteType))) +
#   geom_point(data = cov_ref_new %>% 
#                filter(Site %in% c('ULAK/HASGOX POINT')),
#              aes(x = LON, y = LAT, shape = SiteType, size = SiteType,
#                  fill = SiteType, group = rev(SiteType))) +
#   geom_segment(data = reg_loc,
#                aes(x = -144, xend = -144, y = 40, yend = 60), linetype = 'dotted') +
#   annotate("text", x = reg_loc$mean_long - 1*(c(0, 2, 2, 0.5, 1.5, 1, 6.5)),
#            y = c(50.3, 50, 50.5, 51.9, 53.5, 55, 56),
#            color = c('palegreen4', 'palegreen4', 'deepskyblue4', 'deepskyblue4',
#                      'deepskyblue4', 'deepskyblue4', 'black'),
#            label = reg_loc$Region, size = 3, hjust = 0, fontface = 'bold.italic') +
#   annotate("text", x = c(-170, -136), y = 48,
#            label = c('Western DPS', 'Eastern DPS'), size = 3, fontface = 'bold') +
#   coord_fixed(1.5, ylim = c(48, 67), xlim = c(-188, -131)) +
#   scale_shape_manual(values = c(22, 24, 21)) +
#   scale_size_manual(guide = 'none', values = c(3, 3, 1.5)) +
#   scale_fill_manual(values = c('palegreen3', 'deepskyblue2', 'salmon2')) +
#   # scale_alpha_manual(guide='none', values = c(1, 0.3, 0.3)) +
#   #scale_y_continuous(limits = c(49, 63)) +
#   map_theme(legend.position = 'top')

cov_ref_new <- cov_ref %>%
  transform(SiteType = ifelse(Region %in% c('W. Aleu', 'C. Aleu'), 
                              'Camera trap', as.character(SiteType))) %>%
  transform(PopTrend = ifelse(Region %in% c('W. Aleu', 'C. Aleu'), 
                              'Decreasing', 'Stable/Increasing'))

pdf(here("SSL_CJS", "figures", "map.pdf"), width = 7, height = 5)
ggplot() +
  geom_polygon(data = ak, aes(x = long, y = lat, group = group), fill = "grey93", color = "grey20", size = 0.3) +
  geom_point(data = cov_ref %>%
               filter(SiteType == 'Rookery'),
             aes(x = LON, y = LAT, shape = SiteType, size = SiteType,
                 fill = SiteType, group = rev(SiteType))) +
  xlab("") + ylab("") +
  geom_point(data = cov_ref_new %>% filter(SiteType == 'Field Camp'),
             aes(x = LON, y = LAT, shape = SiteType, size = SiteType,
                 fill = SiteType, group = rev(SiteType))) +
  geom_point(data = cov_ref_new %>%
               filter(Site %in% c('ULAK/HASGOX POINT', 'AGATTU/CAPE SABAK')),
             aes(x = LON, y = LAT, shape = SiteType, size = SiteType,
                 fill = SiteType, group = rev(SiteType))) +
  geom_segment(data = reg_loc,
               aes(x = -144, xend = -144, y = 40, yend = 60), linetype = 'dotted') +
  #western
  geom_segment(data = reg_loc,
               aes(x = xcoord_west[1], xend = xcoord_west[1], 
                   y = ycoord_west[2], yend = ycoord_west[1])) +
  geom_segment(data = reg_loc,
               aes(x = xcoord_west[2], xend = xcoord_west[2], 
                   y = ycoord_west[2], yend = ycoord_west[1])) +
  geom_segment(data = reg_loc,
               aes(x = xcoord_west[1], xend = xcoord_west[2], 
                   y = ycoord_west[1], yend = ycoord_west[1])) +
  geom_segment(data = reg_loc,
               aes(x = xcoord_west[1], xend = xcoord_west[2], 
                   y = ycoord_west[2], yend = ycoord_west[2])) +
  #eastern
  geom_segment(data = reg_loc,
               aes(x = xcoord_east_high[2], xend = xcoord_east_low[2], 
                   y = ycoord_east_high[2], yend = ycoord_east_low[2])) +
  geom_segment(data = reg_loc,
               aes(x = xcoord_east_high[1], xend = xcoord_east_high[2], 
                   y = ycoord_east_high[1], yend = ycoord_east_high[2])) +
  geom_segment(data = reg_loc,
               aes(x = xcoord_east_high[1], xend = xcoord_east_low[1], 
                   y = ycoord_east_high[1], yend = ycoord_east_low[1])) +
  geom_segment(data = reg_loc,
               aes(x = xcoord_east_low[2], xend = xcoord_east_mid,
                   y = ycoord_east_low[2], yend = ycoord_east_mid)) +
  geom_segment(data = reg_loc,
               aes(x = xcoord_east_low[1], xend = xcoord_east_mid2,
                   y = ycoord_east_low[1], yend = ycoord_east_mid2)) +
  geom_segment(data = reg_loc,
               aes(x = xcoord_east_mid2, xend = xcoord_east_mid,
                   y = ycoord_east_mid2, yend = ycoord_east_mid)) +
  annotate("text", x = reg_loc$mean_long - 1*(c(1, 2, 2, 0.5, 1.5, 1, 6.5)),
           y = c(49.2, 49, 50.1, 51.5, 53.5, 55, 56),
           color = c('palegreen4', 'palegreen4', 'deepskyblue4', 'deepskyblue4',
                     'deepskyblue4', 'deepskyblue4', 'black'),
           label = reg_loc$Region, size = 3, hjust = 0, fontface = 'bold.italic') +
  annotate("text", x = c(-170, -136), y = 47,
           label = c('Western DPS', 'Eastern DPS'), size = 3, fontface = 'bold') +
  coord_fixed(1.5, ylim = c(47, 67), xlim = c(-188, -131)) +
  scale_shape_manual(values = c(22, 24, 21)) +
  scale_size_manual(guide = 'none', values = c(3, 3, 1.5)) +
  scale_fill_manual(values = c('palegreen3', 'deepskyblue2', 'salmon2')) +
  # scale_alpha_manual(guide='none', values = c(1, 0.3, 0.3)) +
  #scale_y_continuous(limits = c(49, 63)) +
  map_theme(legend.position = 'top')
dev.off()

######survival and natality from other studies
phi_dat <- read.csv(file = here::here('SSL_CJS', 'Data', 'SSL_DATA_R.csv'), header = T) %>%
  transform(age = factor(age,levels = c("P","Y","J","A"),labels = c("Pups","Yearlings","Juveniles","Adults")))%>%
  transform(years = ifelse(region=="CGOA","(2002-2011)",
                           ifelse(region=="EGOA","(2002-2011)",
                                  ifelse(region=="EAI","(2002-2011)",
                                         ifelse(region=="MI","(1994-2003)",
                                                ifelse(region=="FI", "(1994-2003)",
                                                       "(2002-2013)"))))))%>%
  transform(region_date = paste(region,years,sep = " "))

ggplot(phi_dat, aes(age, mean), col = region_date, group = region_date) +
  geom_linerange(aes(ymin = lower, ymax = upper, col = region_date, group = region_date),
                 position = position_dodge(0.5)) +
  geom_point(aes(col = region_date, group = region_date), position = position_dodge(0.5)) +
  ylab(expression(paste('Survival Probability', ' ', (phi)))) + xlab('') +
  theme(legend.position = 'top', legend.title = element_blank()) +
  facet_wrap(~sex) +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_aaas(palette = "default")

nat_dat <- read.csv(file = here::here('SSL_CJS', 'Data', 'Natality_ALL.csv'), header = T) %>%
  transform(years = ifelse(region=="EGOA","(2003-2012)",
                           ifelse(region=="CGOA1","(1983-1988)",
                                  ifelse(region=="CGOA2","(1988-1992)",
                                         ifelse(region=="CGOA3","(1992-1997)", 
                                                ifelse(region=="CGOA4","(1997-2004)", NA)))))) %>%
  transform(Region = ifelse(region == 'EGOA', 'EGOA', 'CGOA')) %>%
  transform(Study = paste(Region, years))

ggplot(nat_dat, aes(Region, mean), col = Study, group = Study) +
  geom_linerange(aes(ymin = lower, ymax = upper, col = Study, group = Study), 
                 position = position_dodge(0.5)) +
  geom_point(aes(col = Study, group = Study), position = position_dodge(0.5)) +
  ylab(expression(paste('Natality'))) + xlab('') +
  theme(legend.position = 'top', legend.title = element_blank()) +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_aaas(palette = "default")
