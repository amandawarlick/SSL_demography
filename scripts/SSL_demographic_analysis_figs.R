## code to create dataframes and plots for SSL_demographic_analysis.Rmd
 
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)
library(ggplot2)
library(cowplot)
library(knitr)
library(stringr)
library(here)
library(corrplot)
library(Hmisc)
library(lubridate)
library(coda)
library(readr)

plot_theme <- function(...) {
  theme(
    #text = element_text(size = 11),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = "black", size = 8), 
    axis.text = element_text(vjust = 0.5, color = "black", size = 8), 
    axis.title = element_text(size = 8.5),
    # axis.line.y = element_line(colour = "black"), 
    # axis.line.x = element_line(colour = "black"), 
    plot.background = element_rect(), 
    panel.background = element_rect(fill = 'white'), 
    #panel.border = element_rect(fill = NA), #for square around plot
    panel.grid = element_blank(), 
    legend.key = element_blank(),
    strip.background = element_blank(), 
    strip.text = element_text(size = 7),
    legend.text = element_text(size = 7),
    ...)
}
rainbow2 <- c("violetred4", "dodgerblue3", 'deepskyblue1', "#4aaaa5", "#a3d39c", "#f6b61c", "chocolate2", "red3")

##########################################
######## Figure processing code ##########
##########################################

#### load in brands table created along with capture histories in data processing script ####
brands_table <- read.csv(here::here('data', 'ProcData', 'brands.csv'))
colnames(brands_table) <- gsub('X', '', colnames(brands_table))

#### posteriors and posterior summaries: null model ####

#west
# west_out <- readRDS(here::here('results', paste0('out_null_west.rds')))
# west_out <- readRDS(here::here('results', paste0('out_null_west2018dat.rds')))
west_out <- readRDS(here::here('results', paste0('out_null_west_car.rds')))
all_pars <- colnames(west_out$samples$chain1)
noZ <- all_pars[which(!grepl('z', all_pars))]

outmat_west <- as.matrix(west_out$samples)
posts_west <- outmat_west[,noZ]

post_sum_w <- data.frame(
  med = apply(posts_west, 2, function(x) quantile(x, probs = 0.5, na.rm = T, names = F)),
  lower = apply(posts_west, 2, function(x) quantile(x, probs = 0.025, na.rm = T, names = F)),
  upper = apply(posts_west, 2, function(x) quantile(x, probs = 0.975, na.rm = T, names = F)))
post_sum_w$variable <- row.names(post_sum_w)

#east -- do once and read back in for better markdown knitting speed
# east_out <- readRDS(here::here('results', paste0('out_null_east.rds')))
# 
# all_pars <- colnames(east_out$samples$chain1)
# noZ <- all_pars[which(!grepl('z', all_pars))]
# 
# outmat_east <- as.matrix(east_out$samples)
# posts_east <- outmat_east[,noZ]
# 
# write.csv(posts_east, file = here::here('results', 'posts_east.csv'),
#           row.names = F)

posts_east <- read.csv(file = here::here('results', 'posts_east.csv'),
                       header = T, stringsAsFactors = F)

post_sum_e <- data.frame(
  med = apply(posts_east, 2, function(x) quantile(x, probs = 0.50, na.rm = T, names = F)),
  lower = apply(posts_east, 2, function(x) quantile(x, probs = 0.025, na.rm = T, names = F)),
  upper = apply(posts_east, 2, function(x) quantile(x, probs = 0.975, na.rm = T, names = F)))
post_sum_e$variable <- row.names(post_sum_e)

##### base demographic rates ####
##survival -- east
vars <- which(grepl('int.', row.names(post_sum_e)))
mean.dem <- post_sum_e[vars,]

phi.vals.e <- mean.dem %>%
  filter(grepl('phi', variable)) %>%
  transform(age = gsub('int.phi', '', variable)) %>%
  transform(Sex = ifelse(grepl('M', age), 'Male', 'Female')) %>%
  transform(age = ifelse(Sex == 'Female', as.character(age), 
                         ifelse(age == 'BM', 'A', sub('M', '', age)))) %>%
  transform(Region = 'Eastern')

##breeding -- east
psi.vals.e <- mean.dem %>%
  filter(grepl('int.psi', variable)) %>%
  transform(age = gsub('int.psi', '', variable)) %>%
  transform(Region = 'Eastern')

##detection -- east
p.vals.e <- mean.dem %>%
  filter(!grepl('psi', variable)&!grepl('phi', variable)) %>%
  transform(age = gsub('int.p', '', variable)) %>%
  transform(Sex = ifelse(grepl('M', age), 'Male', 'Female')) %>%
  transform(age = ifelse(Sex == 'Female', as.character(age), 
                         ifelse(age == 'BM', 'A', sub('M', '', age)))) %>%
  transform(Region = 'Eastern')

##natality -- east
nat <- post_sum_e[grepl('nat', row.names(post_sum_e)),]

nat.vals.e <- nat %>%
  filter(!is.na(med)&med>0) %>% #filter out years when the value wasn't calculable in the model
  #this is silly and ugly but oh well -- replacing the periods with brackets 
  transform(variable = gsub('nat4.', 'nat4[', variable)) %>%
  transform(variable = gsub('nat5.', 'nat5[', variable)) %>%
  transform(variable = gsub('nat6plus.', 'nat6plus[', variable)) %>%
  transform(variable = gsub('nat_all.', 'nat_all[', variable)) %>%
  transform(variable = substr(variable, 1,nchar(variable)-1)) %>%
  transform(variable = paste0(variable, ']')) %>%
  transform(year = str_extract(variable, "\\[(\\d{1,2})\\]")) %>%
  transform(year = parse_number(year)) %>%
  transform(variable = sub("\\[.*", "", variable)) %>%
  transform(Region = 'Eastern')

##survival -- west: grab both the young smoothed and adults
vars <- c('mu.phi[1]', 'mu.phi[2]', 'mu.phi[3]', 'mu.phi[4]', 'mu.phi[5]', 'mu.phi[6]',
          'mu.phiM[1]', 'mu.phiM[2]', 'mu.phiM[3]', 'mu.phiM[4]', 'mu.phiM[5]', 'mu.phiM[6]',
          'mu.A', 'mu.AM')
# vars <- c('mu.phiP', 'mu.phi1', 'mu.phi2', 'mu.phi3', 'mu.phi4', 'mu.phi5',
#           'mu.phiPM', 'mu.phi1M', 'mu.phi2M', 'mu.phi3M', 'mu.phi4M', 'mu.phi5M',
#           'mu.A', 'mu.AM')
mean.dem <- post_sum_w[vars,]

library(stringr)
library(readr)
phi.vals.w <- mean.dem %>%
  transform(med = plogis(med), lower = plogis(lower), upper = plogis(upper)) %>%
  transform(age = str_extract(variable, "\\[(\\d{1,2})\\]")) %>%
  transform(age = parse_number(age)-1) %>%
  transform(Sex = ifelse(grepl('M', variable), 'Male', 'Female')) %>%
  transform(age = ifelse(is.na(age), 'A', ifelse(age == 0, 'P', age))) %>%
  transform(Region = 'Western')

#psi and p -- west
vars <- which(grepl('int.', row.names(post_sum_w)))
mean.dem <- post_sum_w[vars,]

psi.vals.w <- mean.dem %>%
  filter(grepl('psi', variable)) %>%
  transform(age = gsub('int.psi', '', variable)) %>%
  transform(Region = 'Western')

p.vals.w <- mean.dem %>%
  filter(!grepl('psi', variable)&!grepl('phi', variable)) %>%
  transform(age = gsub('int.p', '', variable)) %>%
  transform(Sex = ifelse(grepl('M', age), 'Male', 'Female')) %>%
  transform(age = ifelse(Sex == 'Female', as.character(age), 
                         ifelse(age == 'BM', 'A', sub('M', '', age)))) %>%
  transform(Region = 'Western')

##combine and write csv for IPM
# dem.rates <- bind_rows(psi.vals.w, phi.vals.w, psi.vals.e, phi.vals.e)
# write.csv(dem.rates, file = paste('/Users', 'awarlick', 'Documents', 'SAFS', 
#                                    'SSL_IPM', 'data', 'ProcData', 'dem.rates.csv', sep = '/'),
#           row.names = F)

## natality -- west
nat <- post_sum_w[grepl('nat', row.names(post_sum_w)),]

nat.vals.w <- nat %>%
  filter(!is.na(med)&med>0) %>% #filter out years when the value wasn't calculable in the model
  transform(year = str_extract(variable, "\\[(\\d{1,2})\\]")) %>%
  transform(year = parse_number(year)+11) %>% #add 11 to sync numbers with eastern "years"
  transform(variable = sub("\\[.*", "", variable)) %>%
  transform(Region = 'Western')

##combine regions -- for plots and vital rates table
phi.vals <- bind_rows(phi.vals.w, phi.vals.e) %>%
  transform(age = factor(age, levels = c('P', '1', '2', '3', '4', '5', 'B', 'NB', 'A'),
                         labels = c('P', '1', '2', '3', '4', '5', 'Breeder', 'Non-breeder', 'Adult'))) 

psi.vals <- bind_rows(psi.vals.w, psi.vals.e) %>%
  transform(age = factor(age, levels = c('3', '4', '5', 'B', 'NB'),
                         labels = c('4', '5', '6', 'Breeder', 'Non-breeder'))) 

p.vals <- bind_rows(p.vals.w, p.vals.e) %>%
  transform(age = factor(age, levels = c('1', '2', '3', '4', '5', 'B', 'NB', 'A'),
                         labels = c('1', '2', '3', '4', '5', 'Breeder', 'Non-breeder', 'Adult'))) 

nat.vals <- bind_rows(nat.vals.w, nat.vals.e) %>%
  transform(age = ifelse(variable == 'nat4', '4', 
                         ifelse(variable == 'nat5', '5',
                                ifelse(variable == 'nat6plus', '6+', 'all'))))

#null model vital rates table
phi_tab <- phi.vals %>%
  # transform(age = ifelse(age %in% c('Breeder', 'Non-breeder'), 'Adult', age)) %>%
  filter(grepl('int.phi', variable)) %>%
  transform(med_e = round(med, 2), lower = round(lower,2), upper = round(upper,2)) %>%
  transform(CI_e = paste0(lower, '-', upper)) %>% 
  transform(Parameter = factor(variable, 
                      levels = c('int.phiP', 'int.phiPM', 'int.phi1', 'int.phi1M',
                                 'int.phi2', 'int.phi2M', 'int.phi3', 'int.phi3M',
                                 'int.phi4', 'int.phi4M', 'int.phi5', 'int.phi5M',
                                 'int.phiB', 'int.phiNB', 'int.phiBM'),
                      labels = c('$\\phi_{P_F}$', '$\\phi_{P_M}$', 
                                 '$\\phi_{1_F}$', '$\\phi_{1_M}$', 
                                 '$\\phi_{2_F}$', '$\\phi_{2_M}$', 
                                 '$\\phi_{3_F}$', '$\\phi_{3_M}$', 
                                 '$\\phi_{4_F}$', '$\\phi_{4_M}$', 
                                 '$\\phi_{5_F}$', '$\\phi_{5_M}$', 
                                 '$\\phi_{B_F}$', '$\\phi_{NB_F}$', 
                                 '$\\phi_{A_M}$'))) %>%
  dplyr::select(age, Parameter, med_e, CI_e) %>% 
  merge(phi.vals %>%
          # transform(age = ifelse(age %in% c('Breeder', 'Non-breeder'), 'Adult', age)) %>%
          filter(grepl('mu.', variable)) %>%
          transform(med_w = round(med, 2), lower = round(lower,2), upper = round(upper,2)) %>%
          transform(CI_w = paste0(lower, '-', upper)) %>% 
          transform(Parameter = factor(variable, 
                                       levels = c('mu.phi[1]', 'mu.phiM[1]', 'mu.phi[2]', 'mu.phiM[2]',
                                                  'mu.phi[3]', 'mu.phiM[3]', 'mu.phi[4]', 'mu.phiM[4]',
                                                  'mu.phi[5]', 'mu.phiM[5]', 'mu.phi[6]', 'mu.phiM[6]',
                                                  'mu.A', 'mu.AM'),
                                       labels = c('$\\phi_{P_F}$', '$\\phi_{P_M}$', 
                                                  '$\\phi_{1_F}$', '$\\phi_{1_M}$', 
                                                  '$\\phi_{2_F}$', '$\\phi_{2_M}$', 
                                                  '$\\phi_{3_F}$', '$\\phi_{3_M}$', 
                                                  '$\\phi_{4_F}$', '$\\phi_{4_M}$', 
                                                  '$\\phi_{5_F}$', '$\\phi_{5_M}$', 
                                                  '$\\phi_{A_F}$', '$\\phi_{A_M}$'))) %>%
          dplyr::select(age, Parameter, med_w, CI_w), 
        by = c('age', 'Parameter'), all = T) %>%
  dplyr::select(Parameter, age, med_e, CI_e, med_w, CI_w) 

nat_tab <- nat.vals %>% filter(!grepl('plus', variable) & Region == "Eastern" & year > 10) %>%
                   group_by(age) %>%
                   dplyr::summarize(med_e = round(mean(med),2), lower = round(mean(lower),2), 
                                    upper = round(mean(upper),2), .groups = 'keep') %>%
                   transform(CI_e = paste0(lower, '-', upper)) %>%
                   transform(Parameter = factor(age, levels = c('4', '5', 'all'),
                                                labels = c('$f_4$', '$f_5$', '$f$'))) %>%
                   dplyr::select(age, Parameter, med_e, CI_e) %>%
  merge(nat.vals %>% filter(!grepl('plus', variable) & Region == "Western" & year > 16) %>%
                    group_by(age) %>%
                    dplyr::summarize(med_w = round(mean(med),2), lower = round(mean(lower),2), 
                                     upper = round(mean(upper),2), .groups = 'keep') %>%
                    transform(CI_w = paste0(lower, '-', upper)) %>%
                    transform(Parameter = factor(age, levels = c('4', '5', 'all'),
                                                 labels = c('$f_4$', '$f_5$', '$f$'))) %>%
                    dplyr::select(age, Parameter, med_w, CI_w), by = c('age', 'Parameter'))
        
pars_tab <- bind_rows(phi_tab, nat_tab)     
row.names(pars_tab) <- NULL
colnames(pars_tab) <- c('Parameter', 'Age class', 'Eastern', '95% CI', 'Western', '95% CI')

#null model plots
phi_plot <- ggplot(phi.vals, aes(age, med), col = Region, group = Region) +
  geom_linerange(aes(ymin = lower, ymax = upper, col = Region, group = Region), 
                 position = position_dodge(0.5), size = 0.5) +
  geom_point(aes(col = Region, group = Region), position = position_dodge(0.5), size = 0.8) +
  ylab(expression(paste('Survival Probability', ' ', (phi)))) + xlab('Age class') +
  plot_theme(legend.position = 'top', legend.title = element_blank(),
             panel.border = element_rect(fill = NA)) +
  ggtitle('') +
  facet_wrap(~Sex, scales = 'free') +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(values = c('black', 'grey65'))

#checking smoothing
ggplot(phi.vals.w, aes(variable, med), col = Region, group = Region) +
  geom_linerange(aes(ymin = lower, ymax = upper, col = Region, group = Region),
                 position = position_dodge(0.5), size = 0.5) +
  geom_point(aes(col = Region, group = Region), position = position_dodge(0.5), size = 0.8) +
  ylab(expression(paste('Survival Probability', ' ', (phi)))) + xlab('Age class') +
  plot_theme(legend.position = 'top', legend.title = element_blank(),
             panel.border = element_rect(fill = NA)) +
  ggtitle('') +
  facet_wrap(~Sex, scales = 'free') +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(values = c('black', 'grey65'))
# 
# ggplot(phi.vals.w, aes(age, med), col = Region, group = Region) +
#   geom_linerange(aes(ymin = lower, ymax = upper, col = Region, group = Region), 
#                  position = position_dodge(0.5), size = 0.5) +
#   geom_point(aes(col = Region, group = Region), position = position_dodge(0.5), size = 0.8) +
#   ylab(expression(paste('Survival Probability', ' ', (phi)))) + xlab('Age class') +
#   plot_theme(legend.position = 'top', legend.title = element_blank(),
#              panel.border = element_rect(fill = NA)) +
#   ggtitle('') +
#   facet_wrap(~Sex, scales = 'free') +
#   scale_y_continuous(limits = c(0,1)) +
#   scale_color_manual(values = c('black', 'grey65'))

psi_plot <- ggplot(psi.vals, aes(age, med), col = Region, group = Region) +
  geom_linerange(aes(ymin = lower, ymax = upper, col = Region, group = Region), 
                 position = position_dodge(0.5), size = 0.5) +
  geom_point(aes(col = Region, group = Region), position = position_dodge(0.5), size = 0.8) +
  ylab(expression(paste('Pupping Probability', ' ', (psi)))) + xlab('Age class') +
  plot_theme(legend.position = 'top', panel.border = element_rect(fill = NA),
             legend.title = element_blank()) +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(values = c('black', 'grey65'))

p_plot <- ggplot(p.vals, aes(age, med), col = Region, group = Region) +
  geom_linerange(aes(ymin = lower, ymax = upper, col = Region, group = Region), 
                 position = position_dodge(0.5), size = 0.5) +
  geom_point(aes(col = Region, group = Region), position = position_dodge(0.5), size = 0.8) +
  ylab('Detection Probability') + xlab('') +
  plot_theme(legend.position = 'top', panel.border = element_rect(fill = NA),
             legend.title = element_blank()) +
  facet_wrap(~Sex, scales = 'free') +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(values = c('black', 'grey65'))

nat_plot <- ggplot(nat.vals %>% filter(age != 'all'), 
                   aes(year, med, group = Region, color = Region)) +
  geom_point(position = position_dodge(width = 0.8), size = 0.8) +
  geom_linerange(aes(ymin = lower, ymax = upper, x = year),
                 position = position_dodge(width = 0.8), size = 0.5) +
  facet_wrap(~age) +
  xlab('') + ylab('Natality') +
  plot_theme(legend.position = 'top', legend.title = element_blank(),
             panel.border = element_rect(fill = NA)) +
  scale_color_manual(values = c('black', 'grey65')) +
  scale_x_continuous(breaks = c(seq(5, 19, by = 2)), labels = c(seq(2004, 2018, by = 2)))

#### time-varying predictions: 
# use full model, chose entire non-breeding season covariates #####
# east_out <- readRDS(here::here('results', paste0('out_full_east_NB.rds')))
# 
# all_pars <- colnames(east_out$samples$chain1)
# noZ <- all_pars[which(!grepl('z', all_pars))]
# 
# outmat_east <- as.matrix(east_out$samples)
# posts_east_t <- outmat_east[,noZ]
# # 
# write.csv(posts_east_t, file = here::here('results', 'posts_east_t.csv'),
#           row.names = F)

posts_east_t <- read.csv(file = here::here('results', 'posts_east_t.csv'), header = T, stringsAsFactors = F)

post_sum_t <- data.frame(
  med = apply(posts_east_t, 2, function(x) quantile(x, probs = 0.50, na.rm = T, names = F)),
  lower = apply(posts_east_t, 2, function(x) quantile(x, probs = 0.025, na.rm = T, names = F)),
  upper = apply(posts_east_t, 2, function(x) quantile(x, probs = 0.975, na.rm = T, names = F)))
post_sum_t$variable <- row.names(post_sum_t)

tvary.vars <- which(grepl('.prob', row.names(post_sum_t)))
tvary <- post_sum_t[tvary.vars,]

phi_t_vals <- tvary %>%
  transform(group = ifelse(grepl('psi', variable), 'Breeding', 'Survival')) %>%
  filter(!is.na(med) & med>0 & group == 'Survival') %>%
  #this is not graceful, but I had already done the regex for when there were brackets rather
  #than the periods from writing and then reading back in the csv files
  transform(variable = gsub('prob.', 'prob[', variable)) %>%
  transform(variable = substr(variable, 1,nchar(variable)-1)) %>%
  transform(variable = paste0(variable, ']')) %>%
  transform(year = str_extract(variable, "\\[(\\d{1,2})\\]")) %>%
  transform(year = parse_number(year)) %>%
  transform(variable = sub("\\[.*", "", variable)) %>%
  transform(variable = sub('.prob', '', variable)) %>%
  transform(variable = gsub('phi', '', variable)) %>%
  transform(sex = ifelse(grepl('M', variable), 'Male', 'Female')) %>%
  transform(variable = sub('M', '', variable)) %>%
  transform(variable = ifelse(variable == 'B' & sex == 'Male', 'NB', variable))

#take mean of ages 3-5 to simplify figure
phi_t_vals <- phi_t_vals %>% filter(sex == 'Female') %>%
  transform(variable = ifelse(variable %in% c('3', '4', '5'), 'PB', variable)) %>%
  group_by(variable, year) %>%
  dplyr::summarize(med = mean(med), lower = mean(lower), upper = mean(upper), .groups = 'keep') %>%
  transform(variable = factor(variable, levels = c('P', '1', '2', 'PB', 'B', 'NB'),
                              labels = c('Pup', '1', '2', 'Juvenile', 'Breeding adult', 
                                         'Non-breeding adult'))) 

#dots and CIs
phi_t_plot <- ggplot(phi_t_vals, aes(year, med, group = variable)) +
  geom_errorbar(aes(x = year, ymin=lower, ymax=upper), show.legend = F, width = 0.5) +
  geom_point(size = 0.8) + geom_line(size = 0.7) +
  xlab('') + ylab(expression(paste('Predicted survival probability ',  '(', phi, ')'))) +
  ggtitle('') +
  # facet_grid(sex~variable) +
  facet_grid(~variable) +
  plot_theme(legend.position = 'none', panel.border = element_rect(fill = NA),
             plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend("", nrow = 1, byrow = T)) +
  scale_color_manual(values = rainbow2) +
  scale_x_continuous(breaks = c(seq(1, 19, by = 2)), labels = c(seq(2000, 2018, by = 2)))

#ribbons and lines
# phi_t_plot <- ggplot(phi_t_vals, aes(year, med, color = variable, group = variable)) +
#   geom_ribbon(aes(ymin=lower, ymax=upper, fill = variable), show.legend = F,
#               alpha=0.1, col = NA) +
#   geom_line(size = 0.8) +
#   xlab('') + ylab(expression(paste('Predicted survival probability ',  '(', phi, ')'))) +
#   ggtitle('') +
#   facet_wrap(~sex) +
#   plot_theme(legend.position = 'top', panel.border = element_rect(fill = NA),
#              plot.title = element_text(hjust = 0.5)) +
#   guides(color = guide_legend("", nrow = 1, byrow = T)) +
#   scale_color_manual(values = rainbow2) +
#   scale_fill_manual(values = rainbow2) +
#   scale_x_continuous(breaks = c(seq(1, 19, by = 2)), labels = c(seq(2000, 2018, by = 2)))

#breeding probabilities
psi_t_vals <- tvary %>%
  transform(group = ifelse(grepl('psi', variable), 'Breeding', 'Survival')) %>%
  filter(!is.na(med) & med>0 & group != 'Survival') %>%
  transform(variable = gsub('prob.', 'prob[', variable)) %>%
  transform(variable = substr(variable, 1,nchar(variable)-1)) %>%
  transform(variable = paste0(variable, ']')) %>%
  transform(year = str_extract(variable, "\\[(\\d{1,2})\\]")) %>%
  transform(year = parse_number(year)) %>%
  transform(variable = sub("\\[.*", "", variable)) %>%
  transform(variable = sub('.prob', '', variable)) %>%
  transform(variable = gsub('psi', '', variable)) %>%
  transform(variable = factor(variable, levels = c('3', '4', '5', 'B', 'NB'),
                              labels = c('4', '5', '6', 'B | B', 'B | NB')))

psi_t_plot <- ggplot(psi_t_vals, aes(year, med, group = variable)) +
  geom_errorbar(aes(x = year, ymin=lower, ymax=upper), width = 0.5, show.legend = F) +
  geom_point(size = 0.8) + geom_line(size = 0.7) +
  xlab('') + ylab(expression(paste('Predicted pupping probability ',  '(', psi, ')'))) +
  ggtitle('') +
  facet_grid(~variable) +
  plot_theme(legend.position = 'none', panel.border = element_rect(fill = NA),
             plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend("", nrow = 1, byrow = T)) +
  scale_color_manual(values = rainbow2[c(4,5,6,7,8)]) +
  scale_x_continuous(breaks = c(seq(1, 19, by = 2)), labels = c(seq(2000, 2018, by = 2)))

pups <- phi_t_vals %>% filter(variable == 'Pup')
breeds <- phi_t_vals %>% filter(variable == 'Breeder')

sigma.vars <- which(grepl('sigma', row.names(post_sum_t)))
sigma_vals <- post_sum_t[sigma.vars,]

##detection
#take temporal variance eps.p and 'offset' b.eff[2]' (b.eff[1] is set to 0 in model)
ptvary <- post_sum_t[c(paste0('eps.p.', c(1:18), '.')),]
b.eff_vals <- post_sum_t['b.eff.2.',][1,c(1:3)]
eff <- c(rep(0, 18))[1:18] 
eff[c(6,17,18)] <- 1 #years with lower detection; b.eff[2] == estimated offset for these years

p_t_vals <- ptvary %>%
  transform(variable = gsub('eps.p.', 'eps.p[', variable)) %>%
  transform(variable = substr(variable, 1,nchar(variable)-1)) %>%
  transform(variable = paste0(variable, ']')) %>%
  transform(year = str_extract(variable, "\\[(\\d{1,2})\\]")) %>%
  transform(year = parse_number(year)) %>%
  transform(eff_med = b.eff_vals$med, eff_low = b.eff_vals$lower, eff_up = b.eff_vals$upper) %>%
  transform(eff = eff) %>%
  transform(med = ifelse(eff == 1, med+eff_med, med),
            lower = ifelse(eff == 1, lower+eff_low, lower),
            upper = ifelse(eff == 1, upper+eff_up, upper)) 

p_t_plot <- ggplot(p_t_vals, 
              aes(year, med), color = 'black') +
  geom_ribbon(aes(ymin=lower, ymax=upper), 
              alpha=0.5, fill = 'lightgrey', col = NA) +
  geom_line(size = 0.8) +
  ggtitle('') +
  xlab('') + ylab(expression(paste('Temporal variance in detection (', epsilon, ')'))) +
  guides(color = guide_legend("", nrow = 1, byrow = T)) +
  plot_theme(legend.position = 'top', panel.border = element_rect(fill = NA),
             plot.title = element_text(size = 55)) +
  scale_x_continuous(breaks = c(seq(0, 18, by = 2)), labels = c(seq(2000, 2018, by = 2)))

#old ribbons and detection
# time_vary_plot <- plot_grid(phi_t_plot, plot_grid(psi_t_plot, p_t_plot, labels = c('(b)', '(c)'), 
#                                              label_size = 10, rel_heights = c(1,1)), 
#                             labels = c('(a)', ''), label_size = 10, nrow = 2)

#new error bars and points; no detection
time_vary_plot <- plot_grid(phi_t_plot, psi_t_plot, labels = c('(a)', '(b)'),
                            rel_heights = c(1,1), rel_widths = c(1.4,0.5),
                            label_size = 10, nrow = 2)

####### covariate data -- run once ##########

# region <- c('east', 'west')
# season <- c('spr', 'sum', 'fall', 'win')
# 
# post_sum_all <- post_sum_all <- beta_probs_all <- waic_all <- data.frame()
# for (r in region) {
#   if (r == 'east') {
#     age <- c('P', '1', '.psi', '.psiB')
#     env_vars <- c('chla', 'albsa', 'up', 'AOI', 'vwnd', 'npgo')
#     bmi_vars <- c("b.mass1", "b.mass1M", "b.massP","b.massPM",  "b.mass.psi", "b.mass.psiB")
#     RE_waic <- readRDS(file = here::here('results', paste0('out_RE_WAIC.rds')))$WAIC}
#   else {
#   age <- c('P', '1')
#   env_vars <- c('up', 'AOI', 'vwnd', 'npgo')
#   bmi_vars <- c("b.mass1", "b.mass1M", "b.massP","b.massPM",  "b.mass.psi")
#   RE_waic <- NA}
#   
#   null_waic <- readRDS(here::here('results', paste0('out_null_', r, '.rds')))$WAIC
# 
# cov_vars <- numeric()
# for (e in env_vars) {
#   for (a in age) {
#     temp <- paste0('b.', e, a)
#     cov_vars <- c(cov_vars, temp)
#   }
# }
# 
# post_sum_seas <- beta_probs_seas <- waic_seas <- waic <- data.frame()
# for (s in season) {
#   
#   #just choose one season run from which to grab bmi effects
#   if (s == 'win') {cov_vars <- c(cov_vars, bmi_vars)} else {cov_vars <- cov_vars}
#   
# out <- readRDS(here::here('results', paste0('out_full_', r, '_', s, '.rds')))
# all_pars <- colnames(out$samples$chain1)
# noZ <- all_pars[which(!grepl('z', all_pars))]
# 
# outmat <- as.matrix(out$samples)
# posts <- outmat[,cov_vars]
# 
# #get summary quantiles (for plot) and make column for whether median is positive or negative
# post_sum <- data.frame(
#             med = apply(posts, 2, function(x) quantile(x, probs = 0.50, na.rm = T, names = F)),
#             lower = apply(posts, 2, function(x) quantile(x, probs = 0.025, na.rm = T, names = F)),
#             upper = apply(posts, 2, function(x) quantile(x, probs = 0.975, na.rm = T, names = F))) %>%
#   transform(Region = r) %>%
#   transform(Season = s) %>%
#   transform(direction = ifelse(med>0, 'positive', 'negative'))
# post_sum$variable <- row.names(post_sum)
# 
# #melt long and merge with summary stat 'direction'
# posts_long <- posts %>%
#   reshape2::melt() %>%
#   dplyr::rename(variable = Var2) %>%
#   merge(post_sum %>% select(variable, direction), by = c('variable'))
# 
# beta_probs <- data.frame()
# temp_probs <- data.frame(var = NA, prob = NA)
# for (v in cov_vars) {
#   temp <- posts_long %>% filter(variable == v)
#   if (sum(temp$direction == 'negative')>0) {
#     prob = -sum(temp$value < 0)/dim(temp)[1]
#   } else { prob = sum(temp$value > 0)/dim(temp)[1] }
#   temp_probs$var = v
#   temp_probs$prob = prob
#   beta_probs <- bind_rows(beta_probs, temp_probs)
# }
# 
# beta_probs <- beta_probs %>%
#   transform(Region = r, Season = s)
# 
# waic <- data.frame(Season = s, Region = r, null = null_waic, RE = RE_waic, full = out$WAIC)
# 
# beta_probs_seas <- bind_rows(beta_probs_seas, beta_probs)
# post_sum_seas <- bind_rows(post_sum_seas, post_sum)
# waic_seas <- bind_rows(waic, waic_seas)
# } #season
# 
# beta_probs_all <- bind_rows(beta_probs_seas, beta_probs_all)
# post_sum_all <- bind_rows(post_sum_seas, post_sum_all)
# waic_all <- bind_rows(waic_seas, waic_all)
# } #region
# 
# write.csv(waic_all, file = here::here('SSL_CJS', 'Output', 'Current', 'Final', 'waic_all.csv'),
#           row.names = F)
# write.csv(post_sum_all, file = here::here('SSL_CJS', 'Output', 'Current', 'Final', 'post_sum_all.csv'),
#           row.names = F)
# write.csv(beta_probs_all, file = here::here('SSL_CJS', 'Output', 'Current', 'Final', 'beta_probs_all.csv'),
#           row.names = F)

#### covariate plots ####
#load back in
cov_dat <- read.csv(file = here::here('results','post_sum_all.csv'), header = T, stringsAsFactors = F)

#effect of pup mass
bmi_dat <- cov_dat %>% filter(grepl('mass', variable)) %>%
  transform(age = ifelse(variable %in% c('b.mass1','b.mass1M'), '1-2',
                         ifelse(variable %in% c('b.massP', 'b.massPM'), 'P',
                                ifelse(grepl('psiB', variable), 'Repeat', 'First time')))) %>%
  transform(sex = ifelse(variable %in% c('b.massPM', 'b.mass1M'), 'Male', 'Female')) %>%
  transform(age = factor(age, levels = c('P', '1-2','First time','Repeat'),
                         labels = c('Pup survival', 'Young survival', 
                                    'First time pupping', 'Repeat pupping'))) %>%
  transform(Region = factor(Region, levels = c('east', 'west'), 
                            labels = c('Eastern', 'Western')))
bmi_plot <- ggplot(bmi_dat %>% filter(age != 'Repeat pupping'), 
                   aes(age, med, col = sex, group = sex)) +
  geom_linerange(aes(ymin = lower, ymax = upper), size = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(aes(), position = position_dodge(width = 0.5), size = 0.8) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  facet_grid(~Region) +
  ylab(expression(paste('Logit-scale effect of pup mass'))) + xlab('') +
  plot_theme(legend.position = 'top', panel.border = element_rect(fill = NA),
             legend.title = element_blank()) +
  # scale_color_manual(values = rainbow2[c(2,5)])
  scale_color_manual(values = c('black', 'grey65'))

#effect of env conditions -- east
env_dat_e <- cov_dat %>% filter(Region == 'east' & !grepl('mass', variable)) %>%
  transform(age = ifelse(grepl('P', variable), 'P', ifelse(grepl('1', variable), '1-2',  
                                                           ifelse(grepl('psiB', variable), 'Repeat', 'First time')))) %>%
  transform(Season = factor(Season, levels = c('sum', 'fall', 'win', 'spr'),
                            labels = c('Summer', 'Fall', 'Winter', 'Spring'))) %>%
  transform(age = factor(age, levels = c('P', '1-2', 'First time', 'Repeat'),
                         labels = c('Pup survival', 'Young survival',
                                    'First pupping', 'Repeat pupping'))) %>%
  transform(var_name = ifelse(grepl('chla', variable), 'Chlorophyll', 
                              ifelse(grepl('AOI', variable), 'Arctic Oscillation Index',
                                     ifelse(grepl('albsa', variable), 'Aleutian low', 
                                            ifelse(grepl('npgo', variable), 'North Pacific Gyre Oscillation',
                                                   ifelse(grepl('up', variable), 'Upwelling', 'Northward wind')))))) %>%
  transform(var_name = factor(var_name, levels = c('Aleutian low', 'Arctic Oscillation Index', 'North Pacific Gyre Oscillation',
                                                   'Chlorophyll', 'Northward wind', 'Upwelling'),
                              labels = c('Aleutian low', 'Arctic Oscillation Index', 'North Pacific Gyre Oscillation',
                                         'Chlorophyll', 'Northward wind', 'Upwelling')))

env_east_plot <- ggplot(env_dat_e, aes(x = age, y = med), col = Season, group = Season) +
  geom_linerange(aes(ymin = lower, ymax = upper, col = Season, group = Season),
                 position = position_dodge(0.5), size = 0.5) +
  geom_point(aes(x = age, y = med, col = Season, group = Season), size = 0.8, 
             position = position_dodge(0.5)) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  ylab(expression(paste('Logit-scale effect of environmental variable'))) + xlab('') +
  plot_theme(legend.position = 'top', panel.border = element_rect(fill = NA),
             legend.title = element_blank()) +
  facet_wrap(~var_name) +
  scale_color_manual(values = rainbow2[-c(1,4)])

#effect of env conditions -- west
env_dat_w <- cov_dat %>% filter(Region == 'west' & !grepl('mass', variable)) %>%
  transform(age = ifelse(grepl('P', variable), 'P', ifelse(grepl('1', variable), '1-2',  
                                                           ifelse(grepl('psiB', variable), 'Repeat', 'First time')))) %>%
  transform(Season = factor(Season, levels = c('sum', 'fall', 'win', 'spr'),
                            labels = c('Summer', 'Fall', 'Winter', 'Spring'))) %>%
  transform(age = factor(age, levels = c('P', '1-2', 'First time', 'Repeat'),
                         labels = c('Pup survival', 'Young survival',
                                    'First pupping', 'Repeat pupping'))) %>%
  transform(var_name = ifelse(grepl('AOI', variable), 'AOI', 
                              ifelse(grepl('npgo', variable), 'NPGO',
                                      ifelse(grepl('up', variable), 'Upwelling', 'Wind')))) %>%
  transform(var_name = factor(var_name, levels = c('AOI', 'NPGO', 'Wind', 'Upwelling'),
                              labels = c('Arctic Oscillation Index', 'North Pacific Gyre Oscillation', 
                                         'Northward wind', 'Upwelling')))

env_west_plot <- ggplot(env_dat_w, aes(age, med), col = Season, group = Season) +
  geom_linerange(aes(ymin = lower, ymax = upper, col = Season, group = Season), 
                 position = position_dodge(0.5), size = 0.5) +
  geom_point(aes(col = Season, group = Season), position = position_dodge(0.5), size = 0.8) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  ylab(expression(paste('Logit-scale effect of environmental variable'))) + xlab('') +
  plot_theme(legend.position = 'top', panel.border = element_rect(fill = NA),
             legend.title = element_blank()) +
  facet_wrap(~var_name) +
  scale_color_manual(values = rainbow2[-c(1,4)])

#probability of greater/less than zero beta values
prob_vals_env <- read.csv(file = here::here('results','beta_probs_all.csv'), header = T, stringsAsFactors = F) %>%
  transform(prob = abs(round(prob,2)))


#### waic values ####
waic_vals <- read.csv(file = here::here('results', 'waic_all.csv'), header = T, stringsAsFactors = F)

waic_tab <- waic_vals %>% filter(Region == 'east') %>%
  reshape2::melt(id.vars = c('Region', 'Season'), value.name = 'WAIC', variable.name = 'Model') %>%
  transform(delWAIC = round(WAIC-min(WAIC),1)) %>%
  bind_rows(waic_vals %>% filter(Region == 'west') %>% 
              reshape2::melt(id.vars = c('Region', 'Season'), 
                             value.name = 'WAIC', variable.name = 'Model') %>%
              filter(Model != 'RE') %>%
              transform(delWAIC = round(WAIC-min(WAIC),1))) %>%
  transform(WAIC = round(WAIC, 1)) %>%
  transform(Season = ifelse(Model %in% c('RE', 'null'), 'NA', Season)) %>%
  distinct() %>%
  transform(Season = factor(Season, levels = c('NA', 'spr', 'sum', 'fall', 'win'),
                            labels = c('', 'Spring', 'Summer', 'Fall', 'Winter'))) %>%
  transform(Model = factor(Model, 
                           levels = c('null', 'RE', 'full'),
                           labels = c('Null', 'Random effects only', 'Full'))) %>%
  transform(Model = ifelse(Model %in% c('Random effects only', 'Null'), as.character(Model),
                           paste(Model, ' (', Season, ')', sep = ''))) %>% 
  dplyr::select(-Season) %>%
  transform(Region = ifelse(Region == 'east', 'Eastern', 'Western')) %>%
  arrange(Region, delWAIC)


##### supplemental info: singles variables #######
season <- c('NB', 'spr', 'sum', 'fall', 'win')
vars <- c('npgo', 'up', 'albsa', 'chla', 'AOI', 'vwnd', 'scal', 'MEI', 'PDO', 'prod', 'curl',
          'sst', 'NOI', 'NPI', 'uwnd')
# 
# waic_vals_east <- data.frame()
# for (r in c('east')) {
#   null <- readRDS(file = here::here('results',paste0('out_null_east.rds')))$WAIC$WAIC
#   RE <- readRDS(file = here::here('SSL_CJS', 'Output', 'Current', 'Final',
#                                   paste0('out_RE_WAIC.rds')))$WAIC$WAIC
#                                   
#   for (v in vars) {
#     for (s in season) {
# 
#       env <- readRDS(file = here::here('results', 'SI', paste0('out_full_east_', s, '_',  v, '.rds')))$WAIC$WAIC
# 
#       vals <- data.frame(Region = r, Season = s, Var = v, WAIC = env, RE = RE, null = null)
# 
#       waic_vals_east <- bind_rows(vals, waic_vals_east)
#     }
#   }
# }
# 
# waic_vals_west <- data.frame()
# for (r in c('west')) {
#   null <- readRDS(file = here::here('results', paste0('out_null_west2018dat.rds')))$WAIC$WAIC
#   RE <- NA
# 
#   for (v in vars) {
#     for (s in season) {
# 
#       env <- readRDS(file = here::here('results', 'SI', paste0('out_env_west_', s, '_',  v, '.rds')))$WAIC$WAIC
# 
#       vals <- data.frame(Region = r, Season = s, Var = v, WAIC = env, RE = RE, null = null)
# 
#       waic_vals_west <- bind_rows(vals, waic_vals_west)
#     }
#   }
# }
# 
# waic_vals <- bind_rows(waic_vals_east, waic_vals_west)

# write.csv(waic_vals, here::here('results', 'SI',
#                                 'waic_vals_singles.csv'), row.names = F)


