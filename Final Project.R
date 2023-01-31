#Boot Up Procedure
setwd("/Users/ryotanaka/Working Directory")
library(tidyverse)

#Final Project

DPRK<-read_csv('DPRK.csv')

#Original data
org<-lm(total ~ us_jp_ab + us_rok_un + jp_rok_un, DPRK)
summary(org)
nobs(org)

orig.3<-lm(total~change_usjp+us_jp_un+us_rok_un+jp_rok_un+rok_dprk_un+jp_mil+rok_mil+us_mil,data=DPRK)
summary(orig.3)
nobs(orig.3)

#Spoof data

spoof<-lm(total~change_usjp+pprk_china_un+us_jp_un+us_rok_un+jp_rok_un+rok_dprk_un+jp_mil+rok_mil+us_mil,data=g12b)
summary(spoof)
nobs(spoof)

spoof.3<-lm(total~change_usjp+us_jp_un+us_rok_un+jp_rok_un+rok_dprk_un+jp_mil+rok_mil+us_mil,data=g12b)
summary(spoof.3)
nobs(spoof.3)

new_model1_lm <- lm(total ~ change_usjp,g12b)
summary(new_model1_lm)
nobs(new_model1_lm)

summarize(DPRK, 
          mean = mean(pprk_china_un,na.rm=T),
          sd = sd(pprk_china_un,na.rm=T),
          min = min(pprk_china_un,na.rm=T),
          max = max(pprk_china_un,na.rm=T))

summarize(DPRK,quantile(pprk_china_un,0.75,na.rm=T), 
          quantile(pprk_china_un, 0.25,na.rm=T))

#original
#25th percentile predicted launches
98.81384  -0.03436*(-1.5) +14.30741*(0.9) -24.13364*(0.45) + 7.15184*(0.455) -58.76529*(0.605) -20.89304*(0.89) -1.06614  *(0.93) -50.92563*(3.45) + 7.44614*(2.47)
#75th percentile predicted launches
98.81384  -0.03436*(2.5) +14.30741*(0.96) -24.13364*(0.58) + 7.15184*(0.585) -58.76529*(0.71) -20.89304*(0.94) -1.06614  *(0.96) -50.92563*(4.89) + 7.44614*(3.42)

-183.2975- -108.3048

#spoof
#25th percentile
21.92292 + 1.39409*(-1.5) -3.68284*(0.9) -1.01706*(0.85) + 1.27998*(0.455) -7.31143*(0.569) -11.31513*(0.89) -3.47722*(0.93) + 1.35463*(3.45) + 1.42670*(2.47)
#75 percentile
21.92292 + 1.39409*(2.5) -3.68284*(0.96) -1.01706*(0.94) + 1.27998*(0.585) -7.31143*(0.772) -11.31513*(0.94) -3.47722*(0.96) + 1.35463*(4.89) + 1.42670*(3.42)

### DPRK - OG ####
#total#
summarize(dprk,
          mean(total, na.rm=T),
          min(total, na.rm=T),
          max(total, na.rm=T),
          sd(total, na.rm=T))

summarize(dprk,
          IQR(total,na.rm=T),
          quantile(total,c(0.25,0.50,0.75),na.rm=T))


#Change_USJP#
summarize(dprk,
          mean(change_usjp, na.rm=T),
          min(change_usjp, na.rm=T),
          max(change_usjp, na.rm=T),
          sd(change_usjp, na.rm=T))



summarize(dprk,
          IQR(change_usjp,na.rm=T),
          quantile(change_usjp,c(0.25,0.50,0.75),na.rm=T))

#pprk china un#
summarize(dprk, 
          mean(pprk_china_un, na.rm=T),
          min(pprk_china_un, na.rm=T),
          max(pprk_china_un, na.rm=T),
          sd(pprk_china_un, na.rm=T))

summarize(dprk,
          IQR(pprk_china_un,na.rm=T),
          quantile(pprk_china_un,c(0.25,0.50,0.75),na.rm=T))

# us-jp-un #

summarize(dprk,
          mean(us_jp_un, na.rm=T),
          min(us_jp_un, na.rm=T),
          max(us_jp_un, na.rm=T),
          sd(us_jp_un, na.rm=T))

summarize(dprk,
          IQR(us_jp_un,na.rm=T),
          quantile(us_jp_un,c(0.25,0.50,0.75),na.rm=T))


# us-rok-un #


summarize(dprk,
          mean(us_rok_un, na.rm=T),
          min(us_rok_un, na.rm=T),
          max(us_rok_un, na.rm=T),
          sd(us_rok_un, na.rm=T))

summarize(dprk,
          IQR(us_rok_un,na.rm=T),
          quantile(us_rok_un,c(0.25,0.50,0.75),na.rm=T))


# rok-dprk-un #


summarize(dprk,
          mean(rok_dprk_un, na.rm=T),
          min(rok_dprk_un, na.rm=T),
          max(rok_dprk_un, na.rm=T),
          sd(rok_dprk_un, na.rm=T))

summarize(dprk,
          IQR(rok_dprk_un,na.rm=T),
          quantile(rok_dprk_un,c(0.25,0.50,0.75),na.rm=T))


# jp-rok-un #

summarize(dprk,
          mean(jp_rok_un, na.rm=T),
          min(jp_rok_un, na.rm=T),
          max(jp_rok_un, na.rm=T),
          sd(jp_rok_un, na.rm=T))

summarize(dprk,
          IQR(jp_rok_un,na.rm=T),
          quantile(jp_rok_un,c(0.25,0.50,0.75),na.rm=T))


# jp-mil #

summarize(DPRK,
          mean(jp_mil, na.rm=T),
          min(jp_mil, na.rm=T),
          max(jp_mil, na.rm=T),
          sd(jp_mil, na.rm=T))



summarize(dprk,
          IQR(jp_mil,na.rm=T),
          quantile(jp_mil,c(0.25,0.50,0.75),na.rm=T))


# us-mil #

summarize(dprk,
          min(us_mil, na.rm=T),
          max(us_mil, na.rm=T),
          sd(us_mil, na.rm=T))

summarize(dprk,
          IQR(us_mil,na.rm=T),
          quantile(us_mil,c(0.25,0.50,0.75),na.rm=T))


# rok-mil #

summarize(dprk,
          min(rok_mil, na.rm=T),
          max(rok_mil, na.rm=T),
          sd(rok_mil, na.rm=T))

summarize(dprk,
          IQR(rok_mil,na.rm=T),
          quantile(rok_mil,c(0.25,0.50,0.75),na.rm=T))


### G12b ###

# Total #

summarize(g12b,
          mean(total, na.rm=T),
          min(total, na.rm=T),
          max(total, na.rm=T),
          sd(total, na.rm=T))

summarize(g12b,
          IQR(total,na.rm=T),
          quantile(total,c(0.25,0.50,0.75),na.rm=T))


# Change_USJP #

summarize(g12b,
          mean(change_usjp, na.rm=T),
          min(change_usjp, na.rm=T),
          max(change_usjp, na.rm=T),
          sd(change_usjp, na.rm=T))

summarize(g12b,
          IQR(change_usjp,na.rm=T),
          quantile(change_usjp,c(0.25,0.50,0.75),na.rm=T))


# Pprk china un #

summarize(g12b,
          mean(pprk_china_un, na.rm=T),
          min(pprk_china_un, na.rm=T),
          max(pprk_china_un, na.rm=T),
          sd(pprk_china_un, na.rm=T))

summarize(g12b,
          IQR(pprk_china_un,na.rm=T),
          quantile(pprk_china_un,c(0.25,0.50,0.75),na.rm=T))


# us-jp-un #

summarize(g12b,
          mean(us_jp_un, na.rm=T),
          min(us_jp_un, na.rm=T),
          max(us_jp_un, na.rm=T),
          sd(us_jp_un, na.rm=T))


summarize(g12b,
          IQR(us_jp_un,na.rm=T),
          quantile(us_jp_un,c(0.25,0.50,0.75),na.rm=T))

# us-rok-un #

summarize(g12b,
          mean(us_rok_un, na.rm=T),
          min(us_rok_un, na.rm=T),
          max(us_rok_un, na.rm=T),
          sd(us_rok_un, na.rm=T))

summarize(g12b,
          IQR(us_rok_un,na.rm=T),
          quantile(us_rok_un,c(0.25,0.50,0.75),na.rm=T))

# rok-dprk-un #


summarize(g12b,
          mean(rok_dprk_un, na.rm=T),
          min(rok_dprk_un, na.rm=T),
          max(rok_dprk_un, na.rm=T),
          sd(rok_dprk_un, na.rm=T))



summarize(g12b,
          IQR(rok_dprk_un,na.rm=T),
          quantile(rok_dprk_un,c(0.25,0.50,0.75),na.rm=T))


# jp-rok-un #

summarize(g12b,
          mean(jp_rok_un, na.rm=T),
          min(jp_rok_un, na.rm=T),
          max(jp_rok_un, na.rm=T),
          sd(jp_rok_un, na.rm=T))

summarize(g12b,
          IQR(jp_rok_un,na.rm=T),
          quantile(jp_rok_un,c(0.25,0.50,0.75),na.rm=T))


# jp-mil #

summarize(g12b,
          mean(jp_mil, na.rm=T),
          min(jp_mil, na.rm=T),
          max(jp_mil, na.rm=T),
          sd(jp_mil, na.rm=T))


summarize(g12b,
          IQR(jp_mil,na.rm=T),
          quantile(jp_mil,c(0.25,0.50,0.75),na.rm=T))


# us-mil #

summarize(g12b,
          min(us_mil, na.rm=T),
          max(us_mil, na.rm=T),
          sd(us_mil, na.rm=T))

summarize(g12b,
          IQR(us_mil,na.rm=T),
          quantile(us_mil,c(0.25,0.50,0.75),na.rm=T))


# rok-mil #

summarize(g12b,
          min(rok_mil, na.rm=T),
          max(rok_mil, na.rm=T),
          sd(rok_mil, na.rm=T))

summarize(g12b,
          IQR(rok_mil,na.rm=T),
          quantile(rok_mil,c(0.25,0.50,0.75),na.rm=T))


# Total #

summarize(dprk,
          mean(total, na.rm=T),
          min(total, na.rm=T),
          max(total, na.rm=T),
          sd(total, na.rm=T))

summarize(dprk,
          IQR(total,na.rm=T),
          quantile(total,c(0.25,0.50,0.75),na.rm=T))


# Change_USJP #

summarize(dprk,
          mean(change_usjp, na.rm=T),
          min(change_usjp, na.rm=T),
          max(change_usjp, na.rm=T),
          sd(change_usjp, na.rm=T))

summarize(dprk,
          IQR(change_usjp,na.rm=T),
          quantile(change_usjp,c(0.25,0.50,0.75),na.rm=T))


# Pprk china un #

summarize(dprk,
          mean(pprk_china_un, na.rm=T),
          min(pprk_china_un, na.rm=T),
          max(pprk_china_un, na.rm=T),
          sd(pprk_china_un, na.rm=T))

summarize(dprk,
          IQR(pprk_china_un,na.rm=T),
          quantile(pprk_china_un,c(0.25,0.50,0.75),na.rm=T))


# us-jp-un #

summarize(dprk,
          mean(us_jp_un, na.rm=T),
          min(us_jp_un, na.rm=T),
          max(us_jp_un, na.rm=T),
          sd(us_jp_un, na.rm=T))

summarize(dprk,
          IQR(us_jp_un,na.rm=T),
          quantile(us_jp_un,c(0.25,0.50,0.75),na.rm=T))

# us-rok-un #

summarize(dprk,
          mean(us_rok_un, na.rm=T),
          min(us_rok_un, na.rm=T),
          max(us_rok_un, na.rm=T),
          sd(us_rok_un, na.rm=T))

summarize(dprk,
          IQR(us_rok_un,na.rm=T),
          quantile(us_rok_un,c(0.25,0.50,0.75),na.rm=T))

# rok-dprk-un #

summarize(dprk,
          mean(rok_dprk_un, na.rm=T),
          min(rok_dprk_un, na.rm=T),
          max(rok_dprk_un, na.rm=T),
          sd(rok_dprk_un, na.rm=T))

summarize(dprk,
          IQR(rok_dprk_un,na.rm=T),
          quantile(rok_dprk_un,c(0.25,0.50,0.75),na.rm=T))


# jp-rok-un #

summarize(dprk,
          mean(jp_rok_un, na.rm=T),
          min(jp_rok_un, na.rm=T),
          max(jp_rok_un, na.rm=T),
          sd(jp_rok_un, na.rm=T))

summarize(dprk,
          IQR(jp_rok_un,na.rm=T),
          quantile(jp_rok_un,c(0.25,0.50,0.75),na.rm=T))

# jp-mil #

summarize(dprk,
          mean(jp_mil, na.rm=T),
          min(jp_mil, na.rm=T),
          max(jp_mil, na.rm=T),
          sd(jp_mil, na.rm=T))

summarize(dprk,
          IQR(jp_mil,na.rm=T),
          quantile(jp_mil,c(0.25,0.50,0.75),na.rm=T))

# us-mil #

summarize(dprk,
          min(us_mil, na.rm=T),
          max(us_mil, na.rm=T),
          sd(us_mil, na.rm=T))

summarize(dprk,
          IQR(us_mil,na.rm=T),
          quantile(us_mil,c(0.25,0.50,0.75),na.rm=T))

# rok-mil #

summarize(dprk,
          min(rok_mil, na.rm=T),
          max(rok_mil, na.rm=T),
          sd(rok_mil, na.rm=T))

summarize(dprk,
          IQR(rok_mil,na.rm=T),
          quantile(rok_mil,c(0.25,0.50,0.75),na.rm=T))


### G12b ###

# Total #

summarize(g12b,
          mean(total, na.rm=T),
          min(total, na.rm=T),
          max(total, na.rm=T),
          sd(total, na.rm=T))

summarize(g12b,
          IQR(total,na.rm=T),
          quantile(total,c(0.25,0.50,0.75),na.rm=T))


# Change_USJP #

summarize(g12b,
          mean(change_usjp, na.rm=T),
          min(change_usjp, na.rm=T),
          max(change_usjp, na.rm=T),
          sd(change_usjp, na.rm=T))

summarize(g12b,
          IQR(change_usjp,na.rm=T),
          quantile(change_usjp,c(0.25,0.50,0.75),na.rm=T))


# Pprk china un #

summarize(g12b,
          mean(pprk_china_un, na.rm=T),
          min(pprk_china_un, na.rm=T),
          max(pprk_china_un, na.rm=T),
          sd(pprk_china_un, na.rm=T))

summarize(g12b,
          IQR(pprk_china_un,na.rm=T),
          quantile(pprk_china_un,c(0.25,0.50,0.75),na.rm=T))


# us-jp-un #

summarize(g12b,
          mean(us_jp_un, na.rm=T),
          min(us_jp_un, na.rm=T),
          max(us_jp_un, na.rm=T),
          sd(us_jp_un, na.rm=T))

summarize(g12b,
          IQR(us_jp_un,na.rm=T),
          quantile(us_jp_un,c(0.25,0.50,0.75),na.rm=T))


# us-rok-un #

summarize(g12b,
          mean(us_rok_un, na.rm=T),
          min(us_rok_un, na.rm=T),
          max(us_rok_un, na.rm=T),
          sd(us_rok_un, na.rm=T))

summarize(g12b,
          IQR(us_rok_un,na.rm=T),
          quantile(us_rok_un,c(0.25,0.50,0.75),na.rm=T))

# rok-dprk-un #

summarize(g12b,
          mean(rok_dprk_un, na.rm=T),
          min(rok_dprk_un, na.rm=T),
          max(rok_dprk_un, na.rm=T),
          sd(rok_dprk_un, na.rm=T))

summarize(g12b,
          IQR(rok_dprk_un,na.rm=T),
          quantile(rok_dprk_un,c(0.25,0.50,0.75),na.rm=T))


# jp-rok-un #

summarize(g12b,
          mean(jp_rok_un, na.rm=T),
          min(jp_rok_un, na.rm=T),
          max(jp_rok_un, na.rm=T),
          sd(jp_rok_un, na.rm=T))

summarize(g12b,
          IQR(jp_rok_un,na.rm=T),
          quantile(jp_rok_un,c(0.25,0.50,0.75),na.rm=T))

# jp-mil #

summarize(g12b,
          mean(jp_mil, na.rm=T),
          min(jp_mil, na.rm=T),
          max(jp_mil, na.rm=T),
          sd(jp_mil, na.rm=T))

summarize(g12b,
          IQR(jp_mil,na.rm=T),
          quantile(jp_mil,c(0.25,0.50,0.75),na.rm=T))


# us-mil #

summarize(g12b,
          min(us_mil, na.rm=T),
          max(us_mil, na.rm=T),
          sd(us_mil, na.rm=T))

summarize(g12b,
          IQR(us_mil,na.rm=T),
          quantile(us_mil,c(0.25,0.50,0.75),na.rm=T))

# rok-mil #

summarize(g12b,
          min(rok_mil, na.rm=T),
          max(rok_mil, na.rm=T),
          sd(rok_mil, na.rm=T))

summarize(g12b,
          IQR(rok_mil,na.rm=T),
          quantile(rok_mil,c(0.25,0.50,0.75),na.rm=T))

### Visualization ###

ggplot(g12b, aes(x=change_usjp, y=total))+
  geom_point()+
  geom_smooth(method=lm, se=F)+
  labs(title='US-Japan Relations and North Korean Missile Launch',
       x = 'Change in US-Japan Relations Over the Previous Year',
       y='total number of missile launch from North Korea',
       caption= 'data: Group 12 Simulated Data Set')

ggsave('graph1.jpeg', width=10, height=10)


ggplot(g12b, aes(x=us_jp_un, y=total))+
  geom_point()+
  geom_smooth(method=lm, se=F)+
  labs(title='US-Japan Voting Affinity and North Korean Missile Launch',
       x = 'US-Japan UN Voting Affinity',
       y='total number of missile launch from North Korea',
       caption= 'data: Group 12 Simulated Data Set')

ggsave('graph2.jpeg', width=10, height=10)


ggplot(g12b, aes(x=rok_dprk_un, y=total))+
  geom_point()+
  geom_smooth(method=lm, se=F)+
  labs(title='SK-NK Voting Affinity and North Korean Missile Launch',
       x = 'SK-NK UN Voting Affinity',
       y='total number of missile launch from North Korea',
       caption= 'data: Group 12 Simulated Data Set')

ggsave('graph3.jpeg', width=10, height=10)


ggplot(g12b, aes(x=rok_mil, y=total))+
  geom_point()+
  geom_jitter()+
  geom_smooth(method=lm, se=F)+
  labs(title='South Korean Military Expenses and North Korean Missile Launch',
       x = 'Change of S Korea Expense for Military Over the Prior Year',
       y='total number of missile launch from North Korea',
       caption= 'data: Group 12 Simulated Data Set')

ggsave('graph4.jpeg', width=10, height=10)



g12b <- mutate(g12b,
               un = us_jp_un+us_rok_un+rok_dprk_un+jp_rok_un)


g12b <- mutate(g12b,
               mil = rok_mil+us_mil)


ggplot(g12b, aes(x=change_usjp, y=total))+
  geom_point(aes(fill=un, color=un))+
  labs(title='UN Voting Affinity and North Korean Missile Launch',
       x = 'Change in US-Japan Relations Over the Previous Year',
       y='total number of missile launch from North Korea',
       caption= 'data: Group 12 Simulated Data Set')
ggsave('graph5.jpeg', width=5, height=5)


## Combined Variables ##

g12b <- mutate(g12b,
               un = us_jp_un+us_rok_un+rok_dprk_un+jp_rok_un)


g12b <- mutate(g12b,
               mil = rok_mil+us_mil)


summarize(g12b,
          mean(un, na.rm=T),
          min(un, na.rm=T),
          max(un, na.rm=T),
          sd(un,na.rm=T))


summarize(g12b,
          quantile(un, c(0.25,0.75), na.rm=T))

summarize(g12b,
          mean(mil, na.rm=T),
          min(mil, na.rm=T),
          max(mil, na.rm=T),
          sd(mil,na.rm=T))


summarize(g12b, 
          quantile(mil, c(0.25,0.75), na.rm=T))


dprk <- mutate(dprk,
               un = us_jp_un+us_rok_un+rok_dprk_un+jp_rok_un)


dprk <- mutate(dprk,
               mil = rok_mil+us_mil)

summarize(dprk,
          mean(un, na.rm=T),
          min(un, na.rm=T),
          max(un, na.rm=T),
          sd(un,na.rm=T))

summarize(dprk,
          quantile(un, c(0.25,0.75), na.rm=T))

summarize(dprk,
          mean(mil, na.rm=T),
          min(mil, na.rm=T),
          max(mil, na.rm=T),
          sd(mil,na.rm=T))


summarize(dprk,
          quantile(mil, c(0.25,0.75), na.rm=T))



###original data bivariate analysis
t_usjp_dprklm <- lm(total ~ change_usjp,dprk)
summary(t_usjp_dprklm)
nobs(t_usjp_dprklm)

#VISUALIZATION: original data bivariate analysis
bivar_original <- ggplot(dprk) +
  geom_point(mapping = aes(x = change_usjp,
                           y = total),
             color = 'blue') +
  geom_smooth(aes(x = change_usjp,
                  y = total),
              method = lm,
              se = F) +
  theme_bw() +
  labs(title = 'US-Japan Relations and North Korean missile launch',
       x = 'change in US-Japan relations over the prior year',
       y = 'total number of missile launch from North Korea',
       caption = 'data: Group12 original data set') 
ggsave('bivar_original.png')

#REGRESSION TABLE for the bivariate analysis above

dprk %>% 
  summarise(mean = mean(change_usjp,na.rm = T),
            sd = sd(change_usjp,na.rm = T),
            quantile = quantile(change_usjp,c(0.25,0.5,0.75),na.rm = T),
            min = min(change_usjp,na.rm = T),
            max = max(change_usjp,na.rm = T),
            median = median(change_usjp,na.rm = T))

dprk %>% 
  summarise(mean = mean(pprk_china_un,na.rm = T),
            sd = sd(pprk_china_un,na.rm = T),
            quantile = quantile(pprk_china_un,c(0.25,0.5,0.75),na.rm = T),
            min = min(pprk_china_un,na.rm = T),
            max = max(pprk_china_un,na.rm = T),
            median = median(pprk_china_un,na.rm = T))

#Calculations for substantive impact

9.36944 + 1.37309*0.66
9.36944 + 1.37309*-1.5
9.36944 + 1.37309*2.5
9.36944 + 1.37309*4.29

#ORIGINAL DATA:multivariate analysis
# MODEL1
model1_lm <- lm(total ~ change_usjp + pprk_china_un + us_jp_un + us_rok_un + rok_dprk_un + jp_rok_un, dprk)
summary(model1_lm)
#MODEL2
model2_lm <- lm(total ~ change_usjp + us_jp_un + us_rok_un + rok_dprk_un + jp_rok_un + jp_mil + us_mil + rok_mil, dprk)
summary(model2_lm)
#MODEL3
model3_lm <- lm(total ~ change_usjp + pprk_china_un + us_jp_un + us_rok_un + rok_dprk_un + jp_rok_un + jp_mil + us_mil + rok_mil, dprk)
summary(model3_lm)


#VISUALIZATION: original data set bivariate analysis OV:NK missile launch
#EV: us_jp_un

usjpun <- ggplot(dprk) +
  geom_point(mapping = aes(x = us_jp_un,
                           y = total),
             color = 'blue') +
  geom_smooth(aes(x = us_jp_un,
                  y = total),
              method = lm,
              se = F) +
  theme_bw() +
  labs(title = 'US-Japan UN voting affinity and N.Korea missile launch',
       x = 'US-Japan UN voting affinity',
       y = 'the total number of missile launch from North Korea',
       caption = 'data: Group12 original data set') 
ggsave('usjpun.png')

#Simulated Data
# NEW_MODEL1
new_model1_lm <- lm(total ~ change_usjp,g12b)
summary(new_model1_lm)
nobs(new_model1_lm)
9.36944 +1.37309*(0.66)
#NEW_MODEL2
new_model2_lm <- lm(total ~ change_usjp + us_jp_un + us_rok_un + rok_dprk_un + jp_rok_un, g12b)
summary(new_model2_lm)
#mean value for the total launch
17.8922 +  1.3933*(0.66) - 1.4294*(0.836) + 2.3688*(0.525) - 7.1064*(0.679) - 10.5690*(0.913) 

#NEW_MODEL3
new_model3_lm <- lm(total ~ change_usjp + pprk_china_un + us_jp_un + us_rok_un + rok_dprk_un + jp_rok_un + jp_mil + us_mil + rok_mil, g12b)
summary(new_model3_lm)

#25th percentile predicted launches
21.92292 + 1.39409*(-1.5) -3.68284*(0.9) -1.01706*(0.85) + 1.27998*(0.455) -7.31143*(0.569) -11.31513*(0.89) -3.47722*(0.93) + 1.35463*(3.45) + 1.42670*(2.47)
#75th percentile predicted launches
21.92292 + 1.39409*(2.5) -3.68284*(0.96) -1.01706*(0.94) + 1.27998*(0.585) -7.31143*(0.772) -11.31513*(0.94) -3.47722*(0.96) + 1.35463*(4.89) + 1.42670*(3.42)

#Difference between 25th and 75th percentile above
13.55005 - 6.968057

#REGRESSIOPNA TABLE Simulated Data

g12b %>% 
  summarise(mean = mean(pprk_china_un,na.rm = TRUE),
            sd = sd(pprk_china_un,na.rm = TRUE),
            min = min(pprk_china_un,na.rm = TRUE),
            max = max(pprk_china_un,na.rm = TRUE),
            quantile = quantile(pprk_china_un,c(0.25,0.75),na.rm = TRUE))

g12b %>% 
  summarise(mean = mean(us_jp_un,na.rm = T),
            sd = sd(us_jp_un,na.rm = T),
            min = min(us_jp_un,na.rm = T),
            max = max(us_jp_un,na.rm = T),
            quantile = quantile(us_jp_un,c(0.25,0.75),na.rm = T))

g12b %>% 
  summarise(mean = mean(us_rok_un,na.rm = T),
            sd = sd(us_rok_un,na.rm = T),
            min = min(us_rok_un,na.rm = T),
            max = max(us_rok_un,na.rm = T),
            quantile = quantile(us_rok_un,c(0.25,0.75),na.rm = T))

g12b %>% 
  summarise(mean = mean(rok_dprk_un,na.rm = T),
            sd = sd(rok_dprk_un,na.rm = T),
            min = min(rok_dprk_un,na.rm = T),
            max = max(rok_dprk_un,na.rm = T),
            quantile = quantile(rok_dprk_un,c(0.25,0.75),na.rm = T))

g12b %>% 
  summarise(mean = mean(jp_rok_un,na.rm = T),
            sd = sd(jp_rok_un,na.rm = T),
            min = min(jp_rok_un,na.rm = T),
            max = max(jp_rok_un,na.rm = T),
            quantile = quantile(jp_rok_un,c(0.25,0.75),na.rm = T))
g12b %>% 
  summarise(mean = mean(jp_rok_un,na.rm = T),
            sd = sd(jp_rok_un,na.rm = T),
            min = min(jp_rok_un,na.rm = T),
            max = max(jp_rok_un,na.rm = T),
            quantile = quantile(jp_rok_un,c(0.25,0.75),na.rm = T))

g12b %>% 
  summarise(mean = mean(jp_mil,na.rm = T),
            sd = sd(jp_mil,na.rm = T),
            min = min(jp_mil,na.rm = T),
            max = max(jp_mil,na.rm = T),
            quantile = quantile(jp_mil,c(0.25,0.75),na.rm = T))

g12b %>% 
  summarise(mean = mean(us_mil,na.rm = T),
            sd = sd(us_mil,na.rm = T),
            min = min(us_mil,na.rm = T),
            max = max(us_mil,na.rm = T),
            quantile = quantile(us_mil,c(0.25,0.75),na.rm = T))
g12b %>% 
  summarise(mean = mean(rok_mil,na.rm = T),
            sd = sd(rok_mil,na.rm = T),
            min = min(rok_mil,na.rm = T),
            max = max(rok_mil,na.rm = T),
            quantile = quantile(rok_mil,c(0.25,0.75),na.rm = T))



