
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(splines)
library(plyr)


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files/filtered_4_box")

###########################################################################
###########################################################################

# load hourly data filtered at a the same time of the MODIS-Terra-Aqua overpass ove the UAE

AQ_data <- read_csv("PM25_PM10_data_filtered_4_box.csv")
AQ_data$Value <- as.numeric(AQ_data$Value)

# display only the date~~~~~~~~~~~~~~~~~~~~~~~~~~~~
AQ_data <- AQ_data %>%
  mutate(DateTime = ymd_hms(DateTime, tz = "UTC"),
         Date = date(DateTime)) 

# EAD_data_2013 <- read_csv("database_EAD_ 2013 _daily_filtered.csv")[3:14]
# EAD_data_2014 <- read_csv("database_EAD_ 2014 _daily_filtered.csv")[3:14]
# EAD_data_2015 <- read_csv("database_EAD_ 2015 _daily_filtered.csv")[3:14]
# EAD_data_2016 <- read_csv("database_EAD_ 2016 _daily_filtered.csv")[3:14]
# 
# 
# DM_data_2013 <- read_csv("database_DM_ 2013 _daily_filtered.csv")[3:14]
# DM_data_2014 <- read_csv("database_DM_ 2014 _daily_filtered.csv")[3:14]
# DM_data_2015 <- read_csv("database_DM_ 2015 _daily_filtered.csv")[3:14]
# DM_data_2016 <- read_csv("database_DM_ 2016 _daily_filtered.csv")[3:14]
# 
# NCMS_data_2013 <- read_csv("database_NCMS_ 2013 _daily_filtered.csv")[3:14]
# NCMS_data_2014 <- read_csv("database_NCMS_ 2014 _daily_filtered.csv")[3:14]
# NCMS_data_2015 <- read_csv("database_NCMS_ 2015 _daily_filtered.csv")[3:14]
# NCMS_data_2016 <- read_csv("database_NCMS_ 2016 _daily_filtered.csv")[3:14]
# 
# AQ_data <- rbind(EAD_data_2013, EAD_data_2014, EAD_data_2015, EAD_data_2016, 
#                  DM_data_2013, DM_data_2014, DM_data_2015, DM_data_2016,
#                  NCMS_data_2013, NCMS_data_2014, NCMS_data_2015, NCMS_data_2016)

str(AQ_data)

# AQ_data <- AQ_data %>%
#   mutate(Date = ymd(Date, tz = "UTC"))
# AQ_data <- AQ_data %>%
#   mutate(Date = date(Date))

AQ_data <- AQ_data %>%
  select(Date,
         Longitude,
         Latitude,
         Site,
         Pollutant,
         Value)


## get the months of observations
AQ_data$month <- factor(format(AQ_data$Date, format = "%b"), levels = month.abb)

## Define seasons
AQ_data$season <- character(length = nrow(AQ_data))
AQ_data$season[AQ_data$month %in% month.abb[c(1:2)]] <- "winter"
AQ_data$season[AQ_data$month %in% month.abb[c(12)]] <- "winter"
AQ_data$season[AQ_data$month %in% month.abb[c(3:5)]] <- "spring"
AQ_data$season[AQ_data$month %in% month.abb[c(6:8)]] <- "summer"
AQ_data$season[AQ_data$month %in% month.abb[c(9:11)]] <- "fall"
AQ_data$season <- factor(AQ_data$season, levels = c("winter","spring","summer","fall"))


# filter only PM25  data

AQ_data_PM25 <- AQ_data %>%
  filter(Pollutant == "PM2.5")
# names(AQ_data_PM25)[names(AQ_data_PM25) == 'Value'] <- 'Val_PM25'

# filter ony PM10 data

AQ_data_PM10 <- AQ_data %>%
  filter(Pollutant == "PM10")
# names(AQ_data_PM10)[names(AQ_data_PM10) == 'Value'] <- 'Val_PM10'


# load satellite data 
wd <- "E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/MODIS_LAADS_NASA/"
wd <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/MODIS_LAADS_NASA/"

AOD_2013 <- read_csv(paste0(wd,"2013_MODIS_processed/csv/extracted_all.csv"))
AOD_2014 <- read_csv(paste0(wd,"2014_MODIS_processed/csv/extracted_all.csv"))
AOD_2015 <- read_csv(paste0(wd,"2015_MODIS_processed/csv/extracted_all.csv"))
AOD_2016 <- read_csv(paste0(wd,"2016_MODIS_processed/csv/extracted_all.csv"))

# AOD_2013 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/MODIS_LAADS_NASA/2013_MODIS_processed/csv/extracted_all.csv") 
# AOD_2014 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/MODIS_LAADS_NASA/2014_MODIS_processed/csv/extracted_all.csv") 
# AOD_2015 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/MODIS_LAADS_NASA/2015_MODIS_processed/csv/extracted_all.csv") 
# AOD_2016 <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/MODIS_LAADS_NASA/2016_MODIS_processed/csv/extracted_all.csv") 
 
# do something to adjust the date...
AOD <- rbind(AOD_2013, AOD_2014, AOD_2015, AOD_2016)
AOD$Date <- as.POSIXct(as.Date(AOD$Date, "%m-%d-%Y"))
names(AOD)[names(AOD) == 'Value'] <- 'Val_AOD'


AOD <- AOD %>%
  mutate(date = ymd_hms(Date, tz = "UTC"))
AOD <- AOD %>%
  mutate(Date = date(date))
AOD$date <- NULL



# change site names....
AOD$Site  <- ifelse(grepl("ALAinIslamicIns", AOD$Site, ignore.case = TRUE), 
                         "Al Ain Islamic Ins", AOD$Site)

AOD$Site  <- ifelse(grepl("ALAinStreet", AOD$Site, ignore.case = TRUE), 
                         "Al Ain Street", AOD$Site)


AOD$Site  <- ifelse(grepl("AlMafraq", AOD$Site, ignore.case = TRUE), 
                         "Al Mafraq", AOD$Site)

AOD$Site  <- ifelse(grepl("AlQua0x27a", AOD$Site, ignore.case = TRUE), 
                         "Al Qua'a", AOD$Site)

AOD$Site  <- ifelse(grepl("AlRuwais", AOD$Site, ignore.case = TRUE), 
                         "Al Ruwais", AOD$Site)

AOD$Site  <- ifelse(grepl("AlTawia", AOD$Site, ignore.case = TRUE), 
                         "Al Tawia", AOD$Site)

AOD$Site <- ifelse(grepl("Bain Aljesrain", AOD$Site, ignore.case = TRUE), 
                   "Bain Al Jesrain", AOD$Site)

AOD$Site <- ifelse(grepl("BainAljesrain", AOD$Site, ignore.case = TRUE), 
                         "Bain Al Jesrain", AOD$Site)

AOD$Site  <- ifelse(grepl("baniyasSchool", AOD$Site, ignore.case = TRUE), 
                         "Baniyas School", AOD$Site)



AOD$Site  <- ifelse(grepl("BidaZayed", AOD$Site, ignore.case = TRUE), 
                         "Bida Zayed", AOD$Site)


AOD$Site  <- ifelse(grepl("E11Road", AOD$Site, ignore.case = TRUE), 
                         "E11 Road", AOD$Site)

AOD$Site  <- ifelse(grepl("GayathiSchool", AOD$Site, ignore.case = TRUE), 
                         "Gayathi School", AOD$Site)



AOD$Site  <- ifelse(grepl("Habshan", AOD$Site, ignore.case = TRUE), 
                         "Habshan", AOD$Site)


AOD$Site  <- ifelse(grepl("HamdanStreet", AOD$Site, ignore.case = TRUE), 
                         "Hamdan Street",AOD$Site)


AOD$Site  <- ifelse(grepl("KhadejaPrimarySchool", AOD$Site, ignore.case = TRUE), 
                         "Khadeja Primary School", AOD$Site)


AOD$Site  <- ifelse(grepl("KhalifaCityA", AOD$Site, ignore.case = TRUE), 
                         "Khalifa City A", AOD$Site)

AOD$Site <- ifelse(grepl("KhalifaHighSchool", AOD$Site, ignore.case = TRUE), 
                         "Khalifa High School", AOD$Site)


AOD$Site <- ifelse(grepl("LiwaOasis", AOD$Site, ignore.case = TRUE), 
                         "Liwa Oasis", AOD$Site)


AOD$Site <- ifelse(grepl("Mussafah", AOD$Site, ignore.case = TRUE),
                         "Mussafah", AOD$Site)

# change Safa by hand~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AOD$Site  <- ifelse(grepl("safa", AOD$Site, ignore.case = TRUE),
#                     "Safa", AOD$Site)

AOD$Site  <- ifelse(grepl("Sweihan", AOD$Site, ignore.case = TRUE), 
                         "Sweihan", AOD$Site)


AOD$Site  <- ifelse(grepl("Zakher", AOD$Site, ignore.case = TRUE), 
                         "Zakher", AOD$Site)

AOD$Site  <- ifelse(grepl("JEBELALIVILLAGE", AOD$Site, ignore.case = TRUE), 
                    "JEBEL ALI VILLAGE", AOD$Site)


AOD$Site  <- ifelse(grepl("EMIRATESHILLS", AOD$Site, ignore.case = TRUE), 
                    "EMIRATES HILLS", AOD$Site)

AOD$Site  <- ifelse(grepl("DUBAIAIRPORT", AOD$Site, ignore.case = TRUE), 
                    "DUBAI AIR PORT", AOD$Site)

AOD$Site  <- ifelse(grepl("DUBAI AIRPORT", AOD$Site, ignore.case = TRUE), 
                    "DUBAI AIR PORT", AOD$Site)


AOD$Site  <- ifelse(grepl("JEBELALIPORT", AOD$Site, ignore.case = TRUE), 
                    "JEBEL ALI PORT", AOD$Site)

AOD$Site  <- ifelse(grepl("SHK0x2EZAYEDROAD", AOD$Site, ignore.case = TRUE), 
                    "SHK. ZAYED ROAD", AOD$Site)

AOD$Site  <- ifelse(grepl("SHK0x2EMOHD0x2EBINZAYEDROAD", AOD$Site, ignore.case = TRUE), 
                    "SHK. MOHD. BIN ZAYED ROAD", AOD$Site)

AOD$Site  <- ifelse(grepl("AlHamriyah", AOD$Site, ignore.case = TRUE), 
                    "Al Hamriyah", AOD$Site)

AOD$Site  <- ifelse(grepl("ELdErLyHouse", AOD$Site, ignore.case = TRUE), 
                    "Elderly House", AOD$Site)

AOD$Site  <- ifelse(grepl("AlJeer", AOD$Site, ignore.case = TRUE), 
                    "Al Jeer", AOD$Site)

AOD$Site  <- ifelse(grepl("AlQasimiyah", AOD$Site, ignore.case = TRUE), 
                    "Al Qasimiyah ", AOD$Site)


# join AQ data and AOD data based on Date field


str(AQ_data_PM25)
str(AOD)

# for AOD only select "Date" and "Value_AOD" and "Site"

AOD <- AOD %>%
  select(Date,
         Site,
         Latitude,
         Longitude,
         Val_AOD)


## get the months of observations
# AOD$month <- factor(format(AOD$Date, format = "%b"), levels = month.abb)


# ## Define seasons
# AOD$season <- character(length = nrow(AOD))
# AOD$season[AOD$month %in% month.abb[c(1:2)]] <- "winter"
# AOD$season[AOD$month %in% month.abb[c(12)]] <- "winter"
# AOD$season[AOD$month %in% month.abb[c(3:5)]] <- "spring"
# AOD$season[AOD$month %in% month.abb[c(6:8)]] <- "summer"
# AOD$season[AOD$month %in% month.abb[c(9:11)]] <- "fall"
# AOD$season <- factor(AOD$season, levels = c("winter","spring","summer","fall"))



# Join PM2.5 data and AOD data from MODIS (Terra & Aqua) for all stations and all days
PM25_AOD <- AQ_data_PM25 %>%
  left_join(AOD, by = c("Date", "Site"))
names(PM25_AOD)[names(PM25_AOD) == 'Value'] <- 'Val_PM25'


# Join PM10 data and AOD data from MODIS (Terra & Aqua) for all stations and all days
PM10_AOD <- AQ_data_PM10 %>%
  left_join(AOD, by = c("Date", "Site"))
names(PM10_AOD)[names(PM10_AOD) == 'Value'] <- 'Val_PM10'



####### CORRELATION between PM2.5 and AOD ###----------------------------------------

#### remove outliers functions---------------------------------------------------------

# remove_outliers <- function(x, na.rm = TRUE, ...) {
#   qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
#   H <- 1.5 * IQR(x, na.rm = na.rm)
#   y <- x
#   y[x < (qnt[1] - H)] <- NA
#   y[x > (qnt[2] + H)] <- NA
#   y
# }

####### PM2.5 vs AOD #################################################


# remove lines with NA vlaues
PM25_AOD <- na.omit(PM25_AOD)
PM25_AOD <- PM25_AOD %>%
select(Val_AOD,
       Val_PM25,
       season)


plot(PM25_AOD$Val_AOD, PM25_AOD$Val_PM25)


# check your data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot <- ggplot(PM25_AOD, aes(season, Val_PM25)) +
  theme_bw() +
  geom_boxplot() + 
#  facet_grid(year ~ .) +
  guides(fill=FALSE) +   # no legend
  ylim(0, 100) 
  # xlab("Site") +
  # theme(axis.title.x = element_text(face="bold", colour="black", size=12),
  #       axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=12, colour = "black", face="bold")) +
  # ylab(expression(paste(PM[10], " (µg/",m^3, ")"))) + 
  # theme(axis.title.y = element_text(face="bold", colour="black", size=15),
  #       axis.text.y  = element_text(angle=0, vjust=0.5, size=14)) +
  # geom_hline(yintercept=50, col="red") +
  # ggtitle(expression(paste("Distribution of 24h-averaged"," ", PM[10], " concentration"))) + 
  # theme(plot.title = element_text(lineheight=.8, face="bold", size = 14, hjust = 0.5))
plot

# check your data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot <- ggplot(PM25_AOD, aes(season, Val_AOD)) +
  theme_bw() +
  geom_boxplot() + 
  guides(fill=FALSE)  +
  ylim(0, 1) 
plot



PM25_AOD <- PM25_AOD %>%
  filter(Val_PM25 > 10) %>%   #check background value (this value has been estimated from the above boxplot)
  filter(Val_AOD < 2) %>%
  filter(Val_AOD > 0.2) %>%
  filter(Val_PM25 < 350) 

plot(PM25_AOD$Val_AOD, PM25_AOD$Val_PM25)
 
  

# remove negative Values
# PM25_AOD <- PM25_AOD[PM25_AOD$Val_PM25 != -9900.0000000, ]

#### fit function and label for PM2.5------------------------------------------------
#### this funtion FORCE regression to pass through the origin #######################

lm_eqn <- function(df){
  m <- lm(Val_PM25 ~ -1 + Val_AOD, df);
  eq <- substitute(italic(y) == b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(b = format(coef(m)[1], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}


## this function includes the intercept~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# lm_eqn <- function(df){
#   m <- lm(Val_PM25 ~  Val_AOD, df);
#   eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
#                    list(a = format(coef(m)[2], digits = 2),
#                         b = format(coef(m)[1], digits = 2),
#                         r2 = format(summary(m)$r.squared, digits = 3)))
#   as.character(as.expression(eq));
# }

#####################################################################################

# plot with regression line-----

# jpeg('E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/Sat_AOD_Correlation/PM25_vs_AOD.jpg',
jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/Sat_AOD_Correlation/PM25_vs_AOD.jpg',    
      quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


# define regression equation for each season
eq <- ddply(PM25_AOD, .(season),lm_eqn)


# ggplot(PM25_AOD, aes(x=Val_AOD, y=Val_PM25, color = season)) +
  ggplot(PM25_AOD, aes(x=Val_AOD, y=Val_PM25)) +
  theme_bw() +
 # geom_point(size = 2) +
  geom_jitter(colour=alpha("black",0.15) ) +
  facet_grid(season ~ .) +
  theme( strip.text = element_text(size = 18)) + 
  scale_color_manual(values = c("#ff0000", "#0000ff", "#000000", "#ffb732")) + 
 # geom_smooth(method="lm") +  # Add linear regression line
   geom_smooth(method = "lm", formula = y ~ -1 + x) +  # force fit through the origin
  ylab(expression(paste(PM[2.5], " (µg/",m^3, ")"))) +
  xlab(expression("AOD (MODIS)")) +
  ylim(c(0,200)) + 
  xlim(c(0.25,2)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=13)) +
  
  geom_text(data = eq, aes(x = 1.7, y = 20, label = V1),
            parse = TRUE, inherit.aes=FALSE, size = 4, color = "black" ) +
            facet_grid(season ~.)
  
  # geom_text(x = 1.5, y = 55, label = lm_eqn(PM25_AOD),
  #           size = 5,
  #           color = "red",
  #           parse = TRUE)


par(oldpar)
dev.off()



fit <- lm(Val_PM25 ~ -1 + Val_AOD, PM25_AOD)
summary(fit)



####### PM10 vs AOD #################################################################
#####################################################################################
#### fit function and label for PM10------------------------------------------------

lm_eqn <- function(df){
  m <- lm(Val_PM10 ~ -1 + Val_AOD, df);
  eq <- substitute(italic(y) == b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(b = format(coef(m)[1], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}



# remove lines with NA vlaues
PM10_AOD <- na.omit(PM10_AOD)
PM10_AOD <- PM10_AOD %>%
  select(Val_AOD,
         Val_PM10,
         season)

plot(PM10_AOD$Val_AOD, PM10_AOD$Val_PM10)


# check your data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot <- ggplot(PM10_AOD, aes(season, Val_PM10)) +
  theme_bw() +
  geom_boxplot() + 
  #  facet_grid(year ~ .) +
  guides(fill=FALSE) +
  ylim(0, 300) 
plot


# check your data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot <- ggplot(PM10_AOD, aes(season, Val_AOD)) +
  theme_bw() +
  geom_boxplot() + 
  guides(fill=FALSE)  +
  ylim(0, 1) 
plot



PM10_AOD <- PM10_AOD %>%
  filter(Val_PM10 > 50) %>%  
  filter(Val_PM10 < 1000) %>%
  filter(Val_AOD < 2.5) %>%
  filter(Val_AOD > 0.2)

plot(PM10_AOD$Val_AOD, PM10_AOD$Val_PM10)


# plot with regression line-----

# jpeg('E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/Sat_AOD_Correlation/PM10_vs_AOD.jpg',
jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/Sat_AOD_Correlation/PM10_vs_AOD.jpg',
     quality = 100, bg = "white", res = 200, width = 7, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


# define regression for each season
eq <- ddply(PM10_AOD, .(season),lm_eqn)

# ggplot(PM10_AOD, aes(x=Val_AOD, y=Val_PM10, color = season)) +
  ggplot(PM10_AOD, aes(x=Val_AOD, y=Val_PM10)) +
  theme_bw() +
 # geom_point(size = 2) +
  geom_jitter(colour=alpha("black",0.15) ) +
  facet_grid(season ~ .) +
  theme( strip.text = element_text(size = 18)) + 
  scale_color_manual(values = c("#ff0000", "#0000ff", "#000000", "#ffb732")) + 
  geom_smooth(method = "lm", formula = y ~ -1 + x) +  # force fit through the origin
  ylab(expression(paste(PM[10], " (µg/",m^3, ")"))) +
  xlab(expression("AOD (MODIS)")) +
  ylim(c(0,750)) + 
    theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=13)) +
  geom_text(data = eq, aes(x = 1.8, y = 33, label = V1),
            parse = TRUE, inherit.aes=FALSE, size = 4, color = "black" ) +
  facet_grid(season ~.)


par(oldpar)
dev.off()


#########################################################################
#########################################################################

# convert AOD into PM2.5------------------------------------------------
# use the result of the regression above--------------------------------

AOD$AOD_PM25 <- (AOD$Val_AOD)*94  # value for summer, best R2 = 0.666
AOD$AOD_PM10 <- (AOD$Val_AOD)*294  # value for summer, best R2 = 0.682

write_csv(AOD, "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/Sat_AOD_Correlation/PM25_from_AOD_MODIS.csv")
