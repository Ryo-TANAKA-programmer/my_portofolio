###
setwd("~ryotanaka/Working Directory")
library(tidyverse)
library(ggplot2)
DPRK <- read_csv('DPRK_Project.csv')
###

### US-Japan relation indicator and DPRK missile launch graph

### Mean and standard deviation of US-Japan relations
summarise(DPRK,mean = mean(us_jp_ab,na.rm = TRUE),
          sd = sd(us_jp_ab,na.rm = TRUE))

### DPRK missile launch Mean, SD, 

ggplot(data = DPRK) +
geom_bar(mapping = aes(x = year,
                       weight = total))

summarise(DPRK,min = min(total,na.rm = T),
          max = max(total,na.rm = T),
          total_q = quantile(total,c(0.25,0.5,0.75),na.rm = T),
          mean = mean(total,na.rm = T),
          sd = sd(total,na.rm = T))

### two univariate analyses: US-japan relations, the number of missile launches 

us_jp_graph <- ggplot(data = DPRK) +
  geom_point(size = 1.6,
             mapping = aes(x = year,
                           y = us_jp_ab)) +
  geom_line(color = 'blue',
            lwd = 0.72,
            mapping = aes(x = year,
                          y = us_jp_ab)) +
  geom_bar(mapping = aes(x = year,
                         weight = total),
           color = 'black',
           fill = 'indianred') +
  coord_cartesian(y = c(0,100)) +
  labs(x = 'year',
       y = 'US-Japan relations indicator (%)',
       title = 'Favorability of U.S.-Japan Relations - 
       Missile Launches by North Korea',
       caption = 'Data: Nuclear Threat Initiative') +
  scale_y_continuous(name = 'US-Japan relations indicator (%)',
                     sec.axis = sec_axis(trans = ~., name ='Missile Launches by North Korea')) +
  theme_minimal()

us_jp_graph
ggsave('us_jp_graph.png')


### DPRK launch = OV, US-Japan relations = EV

DP_lm <- lm(total ~ us_jp_ab,DPRK)
summary(DP_lm)
nobs(DP_lm)

###  Visualization for the above bivariate analysis

dprk_usjp_graph <- ggplot(data = DPRK,
       mapping = aes(us_jp_ab,total)) +
  geom_point(color = 'indianred') +
  geom_smooth(method = lm,
              se = FALSE) +
  labs(x = 'US-Japan Relations Indicator',
       y = 'the total number of missiles launched by DPRK',
       title = 'US-Japan relations and DPRK missile launch',
       caption = 'Data: nuclear Threat Initiative') +
  theme_light()
dprk_usjp_graph

ggsave('dprk_usjp_graph.png')
