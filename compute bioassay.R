setwd('F:/ytwang/Dropbox/USLab/pesticides/')
library(ggplot2)

profile_1 <- read.csv("profile_1.csv", stringsAsFactors=FALSE, header = TRUE)

profile_t <- t(profile_1)

profile_df <- as.data.frame(profile_t)

db_id <- gsub("-", "_", profile$compounds)

db_ida <- paste("c",db_id, sep = "")

names(profile_df) <- db_ida

profile_df <- profile_df[-1,]





db1 <- grep('(c1_)',db_ida, perl = TRUE, value = TRUE)
db2 <- grep('c2',db_ida, perl = TRUE, value = TRUE)
db3 <- grep('c3',db_ida, perl = TRUE, value = TRUE)
db4 <- grep('c4',db_ida, perl = TRUE, value = TRUE)
db5 <- grep('c5',db_ida, perl = TRUE, value = TRUE)
db6 <- grep('c6',db_ida, perl = TRUE, value = TRUE)
db7 <- grep('c7',db_ida, perl = TRUE, value = TRUE)
db8 <- grep('c8',db_ida, perl = TRUE, value = TRUE)
db9 <- grep('c9',db_ida, perl = TRUE, value = TRUE)
db10 <- grep('c10',db_ida, perl = TRUE, value = TRUE)
db11 <- grep('c11',db_ida, perl = TRUE, value = TRUE)
db12 <- grep('c12',db_ida, perl = TRUE, value = TRUE)
db13 <- grep('c13',db_ida, perl = TRUE, value = TRUE)
db14 <- grep('c14',db_ida, perl = TRUE, value = TRUE)
db15 <- grep('c15',db_ida, perl = TRUE, value = TRUE)


China <-c(db15,db11,db9,db13,db6,db5,db2,db7)
USA <-  c(db2,db5,db1,db3,db11,db12,db10,db9)
Europe <-  c(db5,db1,db8,db14,db10,db9,db13,db6)
Japan <-  c(db7,db2,db5,db1,db3,db4,db8,db6)


sumcount <-function(df){
  scv <- c()
  for (i in c(1:nrow(df))){
    #print(df[i,])
    sc <- sum(df[i,] == 1)
    scv <- append(scv,sc)
  }
  return(scv)
}

m <- sumcount(profile)

scv <- c()
for (i in c(1:nrow(profile_b))){
  #print(df[i,])
  sc <- sum(profile_b[i,] == 1)
  scv <- append(scv,sc)
}



profile <- data.frame(compounds = db_ida, active = profile_b$activate, inactive = profile_b$deactivate)

#generate part database
db1_df <- profile[profile$compounds %in% db1,]
db_df <- db2_df <- profile[profile$compounds %in% db2,]
db_df <- db3_df <- profile[profile$compounds %in% db3,]
db_df <- db4_df <- profile[profile$compounds %in% db4,]
db_df <- db5_df <- profile[profile$compounds %in% db5,]
db_df <- db6_df <- profile[profile$compounds %in% db6,]
db_df <- db7_df <- profile[profile$compounds %in% db7,]
db_df <- db8_df <- profile[profile$compounds %in% db8,]
db_df <- db9_df <- profile[profile$compounds %in% db9,]
db_df <- db10_df <- profile[profile$compounds %in% db10,]
db_df <- db11_df <- profile[profile$compounds %in% db11,]
db_df <- db12_df <- profile[profile$compounds %in% db12,]
db_df <- db13_df <- profile[profile$compounds %in% db13,]
db_df <- db14_df <- profile[profile$compounds %in% db14,]
db_df <- db15_df <- profile[profile$compounds %in% db15,]

db_df <- China_df <- profile[profile$compounds %in% China,]
db_df <- USA_df <- profile[profile$compounds %in% USA,]
db_df <- Europe_df <- profile[profile$compounds %in% Europe,]
db_df <- Japan_df <- profile[profile$compounds %in% Japan,]

China_df$ratio <-China_df$active/(China_df$active + China_df$inactive)
USA_df$ratio <-USA_df$active/(USA_df$active + USA_df$inactive)
Europe_df$ratio <-Europe_df$active/(Europe_df$active + Europe_df$inactive)
Japan_df$ratio <-Japan_df$active/(Japan_df$active + Japan_df$inactive)

all_df <- data.frame(country = c('China', 'USA', 'Europe', 'Japan'),
                     active = c(sum(China_df$active, na.rm = TRUE),sum(USA_df$active,  na.rm = TRUE),sum(Europe_df$active, na.rm = TRUE ), sum(Japan_df$active, na.rm = TRUE)),
                     inactive = c(sum(China_df$inactive, na.rm = TRUE),sum(USA_df$inactive,  na.rm = TRUE),sum(Europe_df$inactive, na.rm = TRUE ), sum(Japan_df$inactive, na.rm = TRUE)),
                     active_sd = c(sd(China_df$active, na.rm = TRUE),sd(USA_df$active,  na.rm = TRUE),sd(Europe_df$active, na.rm = TRUE ), sd(Japan_df$active, na.rm = TRUE)),
                     inactive_sd = c(sd(China_df$inactive, na.rm = TRUE),sd(USA_df$inactive,  na.rm = TRUE),sd(Europe_df$inactive, na.rm = TRUE ), sd(Japan_df$inactive, na.rm = TRUE)),
                     active_mean = c(mean(China_df$active, na.rm = TRUE),mean(USA_df$active,  na.rm = TRUE),mean(Europe_df$active, na.rm = TRUE ), mean(Japan_df$active, na.rm = TRUE)),
                     inactive_mean = c(mean(China_df$inactive, na.rm = TRUE),mean(USA_df$inactive,  na.rm = TRUE),mean(Europe_df$inactive, na.rm = TRUE ), mean(Japan_df$inactive, na.rm = TRUE)),
                     active_ratio = c(mean(China_df$ratio, na.rm = TRUE),mean(USA_df$ratio,  na.rm = TRUE),mean(Europe_df$ratio, na.rm = TRUE ), mean(Japan_df$ratio, na.rm = TRUE)),
                     ratio_sd =c(sd(China_df$ratio, na.rm = TRUE),sd(USA_df$ratio,  na.rm = TRUE),sd(Europe_df$ratio, na.rm = TRUE ), sd(Japan_df$ratio, na.rm = TRUE)))

all_df <- all_df[order(-all_df$active),]

barcenter <- barplot(height = all_df$active_ratio, col = c('red'), ylim = c(0, 0.25), names.arg = all_df$country, angle = 45)

barcenter <- barplot(height = all_df$active_mean, col = c('red'), ylim = c(0, 15), names.arg = all_df$country, angle = 45)

barcenter <- barplot(height = all_df$inactive_mean, col = c('red'), ylim = c(0, 200), names.arg = all_df$country, angle = 45)

barcenter <- barplot(height = all_df$active, col = c('red'), ylim = c(0, 200), names.arg = all_df$country, angle = 45)

text(x = barcenter, y = 0.5, srt = 45,
     adj = 1, labels = all_df$country, xpd = TRUE)

arrows(barcenter, all_df$inactive_mean+ all_df$active_mean - all_df$ratio_sd*20, barcenter,
       all_df$inactive_mean+ all_df$active_mean + all_df$ratio_sd*20, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

arrows(barcenter, all_df$active_mean - all_df$ratio_sd*3, barcenter,
       all_df$active_mean + all_df$ratio_sd*3, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

db_df <- all_dfmean <- data.frame(country = c('China', 'USA', 'Europe', 'Japan'),
                         active_mean = c(mean(China_df$active, na.rm = TRUE),mean(USA_df$active,  na.rm = TRUE),
                                         mean(Europe_df$active, na.rm = TRUE ), mean(Japan_df$active, na.rm = TRUE)),
                         inactive_mean = c(mean(China_df$inactive, na.rm = TRUE),mean(USA_df$inactive,  na.rm = TRUE),
                                           mean(Europe_df$inactive, na.rm = TRUE ), mean(Japan_df$inactive, na.rm = TRUE))
                         )



# sorting by descendin
db_df <- db_df[order(-db_df$active_mean),] 
# transformation
db_dft <-as.data.frame(t(db_df[,-1]))  
names(db_dft) <- db_df$country

all_dfmean <- all_dfmean[order(-all_dfmean$active_mean),]

# generate stack bar


barcenter <- barplot(as.matrix(db_dft), col=c("red","darkgreen"),
        legend.text = c("Active", "Inactive"), angle = 90, border = NA, space = 0.2, ylim = c(0, 250))

active_meansd = c(mean(China_df$active, na.rm = TRUE), mean(USA_df$active, na.rm = TRUE),
                  mean(Europe_df$active, na.rm = TRUE ), mean(Japan_df$active, na.rm = TRUE))
inactive_meansd = c(mean(China_df$inactive, na.rm = TRUE),mean(USA_df$inactive,  na.rm = TRUE),
                    mean(Europe_df$inactive, na.rm = TRUE ), mean(Japan_df$inactive, na.rm = TRUE))


arrows(barcenter, all_dfmean$active_mean - all_df$active_sd/nrow(China_df)*80, barcenter,
       all_dfmean$active_mean + all_df$active_sd/nrow(China_df)*80, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)



arrows(barcenter, all_dfmean$active_mean + all_dfmean$inactive_mean- all_df$inactive_sd/nrow(China_df)*30, barcenter,
       all_dfmean$active_mean + all_dfmean$inactive_mean +all_df$inactive_sd/nrow(China_df)*30, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
###计算每个数据集内有bioassay的比例
###compute the ratio between compounds with bioassay and without bioassay
db_df <- ratio_ifbioassay <- data.frame(country = c('China', 'USA', 'Europe', 'Japan'), 
                               bio_ratio = c((1-nrow(China_df[(China_df$active == 0) & (China_df$inactive == 0),])/nrow(China_df)), 
                                             (1-nrow(USA_df[(USA_df$active == 0) & (USA_df$inactive == 0),])/nrow(USA_df)),
                                             (1-nrow(Europe_df[(Europe_df$active == 0) & (Europe_df$inactive == 0),])/nrow(Europe_df)),
                                             (1-nrow(Japan_df[(Japan_df$active == 0) & (Japan_df$inactive == 0),])/nrow(Japan_df))))



# sorting by descendin
db_df <- db_df[order(-db_df$bio_ratio),] 

barcenter <- barplot(height = db_df$bio_ratio, col = c('red'), ylim = c(0, 1.0), names.arg = all_df$country, angle = 45)

