library(tidyverse)
library(data.table)

alldata = read_csv("modeling/data/AllSitesWPandTemp3.csv")
alldata = read_csv("C:/repos/bromecast-data/modeling/data/AllSitesWPandTemp3.csv")

# for becca's computer -- MLV: Sorry, should be using AllSitesWPandTemp4.csv -- this is just a scaling issue
alldata = read_csv("/Users/Becca/Desktop/Adler Lab/bromecast-data/modeling/data/AllSitesWPandTemp4.csv")

data = subset(alldata, Year >2020) # First 1-2 years of data may not be as accurate. 
#The model does not know starting conditions very well, so it takes a season or maybe 
#two for the model to produce accurate estimates


#calculate Growing Degrees ------ wet growing degrees = soil temperature when soil is wet (>-1.25 MPa)
## could make WP2 a variable so that we can manipulate it in value better
data$GrowingDegrees2cm = ifelse(data$WP2cm>-1.5 & data$Temp2cm > 0, data$Temp2cm,0)
data$GrowingDegrees5cm = ifelse(data$WP5cm>-1.5 & data$Temp5cm > 0, data$Temp5cm,0)
data$GrowingDegrees10cm = ifelse(data$WP10cm>-1.5 & data$Temp10cm > 0, data$Temp10cm,0)

#Mark Wet Days ------ mark if a day has wet soil 
data$GrowingDegreeDay2cm = ifelse(data$WP2cm>-1.5 & data$Temp2cm > 0, 1,0)
data$GrowingDegreeDay5cm = ifelse(data$WP5cm>-1.5 & data$Temp5cm > 0, 1,0)
data$GrowingDegreeDay10cm = ifelse(data$WP10cm>-1.5 & data$Temp10cm > 0, 1,0)


MonthlySummary = data %>%
  group_by(Site, Year, Month) %>%
  summarize(Growing_degree_days2cm = sum(GrowingDegrees2cm),
            Growing_degree_days5cm = sum(GrowingDegrees5cm),
            Growing_degree_days10cm = sum(GrowingDegrees10cm),
            Wet_days2cm = sum(GrowingDegreeDay2cm),
            Wet_days5cm = sum(GrowingDegreeDay5cm),
            Wet_days10cm = sum(GrowingDegreeDay10cm))

# Make individual plots for each site
data %>% 
  filter(Year == 2021) %>% 
  ggplot(aes(x = Jday, y = GrowingDegrees2cm, color = Site)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")

data %>% 
  filter(Year == 2023) %>% 
  ggplot(aes(x = Jday, y = log(-1*WP2cm), color = Site)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")

data %>% 
  filter(Year == 2023) %>% 
  ggplot(aes(x = Jday, y = Temp2cm, color = Site)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")

data %>% 
  filter(Year == 2023) %>% 
  ggplot(aes(x = Jday, y = Temp5cm, color = Site)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")

case <- data %>% filter(Site == "CaseAoyamaS1")


###### Aggregation of wet degree days
MonthlySummary = data %>%
  group_by(Site, Year, Month) %>%
  summarize(Growing_degree_days2cm = sum(GrowingDegrees2cm),
            Growing_degree_days5cm = sum(GrowingDegrees5cm),
            Growing_degree_days10cm = sum(GrowingDegrees10cm),
            Wet_days2cm = sum(GrowingDegreeDay2cm),
            Wet_days5cm = sum(GrowingDegreeDay5cm),
            Wet_days10cm = sum(GrowingDegreeDay10cm))

MonthlySummary %>% 
  #filter(Year == 2021) %>% 
  ggplot(aes(x = Month, y = Growing_degree_days2cm, color = Site)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")

MonthlySummary %>% 
  filter(Year == 2021) %>% 
  ggplot(aes(x = Month, y = Growing_degree_days5cm, color = Site)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")

MonthlySummary %>% 
  filter(Year == 2021) %>% 
  ggplot(aes(x = Month, y = Growing_degree_days10cm, color = Site)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")

MonthlySummary %>% 
  filter(Year == 2021) %>% 
  ggplot(aes(x = Month, y = Wet_days2cm, color = Site)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")

MonthlySummary %>% 
  filter(Year == 2021) %>% 
  ggplot(aes(x = Month, y = Wet_days5cm, color = Site)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")

MonthlySummary %>% 
  filter(Year == 2021) %>% 
  ggplot(aes(x = Month, y = Wet_days10cm, color = Site)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")

## get location info for sites 
siteinfo21 = read_csv("/Users/Becca/Desktop/Adler Lab/bromecast-data/satellites/rawdata/SiteInfo_2021-2022.csv")

site_list <- siteinfo21 %>% select("Site code", "Latitude (decimal degrees)", "Longitude (decimal degrees)") %>% distinct()

site_list$Site <- site_list$`Site code`


MonthlySummary_location <- left_join(MonthlySummary, site_list, by = "Site")
MonthlySummary_location$latitude <- MonthlySummary_location$`Latitude (decimal degrees)`
MonthlySummary_location$longitude <- MonthlySummary_location$`Longitude (decimal degrees)`

### make a map
library(maps)

#sample code adapted from bromecast github
#png("/Users/Becca/Desktop/Adler Lab/bromecast-data/name.png",height=4,width=6,units="in",res=600)
#par(mar=c(2,2,2,2))
#maps::map("state",xlim=c(-128,-95),ylim=c(30,52))
#points(x=site_means$Lon[site_means$Year==2021],y=site_means$Lat[site_means$Year==2021],
      # pch=0, cex=1.5,col=mypurple)
#points(x=site_means$Lon[site_means$Year==2022],y=site_means$Lat[site_means$Year==2022],
       #pch=1, cex=1.5,col=myorange)
#points(x=site_means$Lon[site_means$Year==2023],y=site_means$Lat[site_means$Year==2023],
       #pch=2, cex=1.5,col=myblue)
#legend("bottomright",c("2021","2022","2023"),pch=c(0,1,2),col=c(mypurple,myorange,myblue))
#dev.off()

#map of sat sites
par(mar=c(4,4,4,4))
map("state",xlim=c(-128,-95),ylim=c(30,52))
points(x=MonthlySummary_location$longitude,y=MonthlySummary_location$latitude, pch=1, cex=1.5,col="purple")

#with color ramp
library("fields")

color_palette <- colorRampPalette(c("blue", "yellow", "red"))
colors <- color_palette(100)  
MonthlySummary_location$color <- colors[cut(MonthlySummary_location$Growing_degree_days2cm, breaks = 100)]  

par(mar=c(4,4,4,4))
map("state",xlim=c(-128,-95),ylim=c(30,52))
title("Growing Degree Days 2 cm")
points(MonthlySummary_location$longitude, MonthlySummary_location$latitude, col = MonthlySummary_location$color, pch = 19, cex = 1.5)
image.plot(legend.only = TRUE, zlim = range(MonthlySummary$Growing_degree_days2cm), col = colors, legend.lab = "Growing_degree_days2cm")

par(mar=c(4,4,4,4))
map("state",xlim=c(-128,-95),ylim=c(30,52))
title("Growing Degree Days 5 cm")
points(MonthlySummary_location$longitude, MonthlySummary_location$latitude, col = MonthlySummary_location$color, pch = 19, cex = 1.5)
image.plot(legend.only = TRUE, zlim = range(MonthlySummary$Growing_degree_days5cm), col = colors, legend.lab = "Growing_degree_days5cm")

par(mar=c(4,4,4,4))
map("state",xlim=c(-128,-95),ylim=c(30,52))
title("Growing Degree Days 10 cm")
points(MonthlySummary_location$longitude, MonthlySummary_location$latitude, col = MonthlySummary_location$color, pch = 19, cex = 1.5)
image.plot(legend.only = TRUE, zlim = range(MonthlySummary$Growing_degree_days10cm), col = colors, legend.lab = "Growing_degree_days10cm")


par(mar=c(4,4,4,4))
map("state",xlim=c(-128,-95),ylim=c(30,52))
title("Wet Days 2 cm")
points(MonthlySummary_location$longitude, MonthlySummary_location$latitude, col = MonthlySummary_location$color, pch = 19, cex = 1.5)
image.plot(legend.only = TRUE, zlim = range(MonthlySummary$Wet_days2cm), col = colors, legend.lab = "Wet_days2cm")


par(mar=c(4,4,4,4))
map("state",xlim=c(-128,-95),ylim=c(30,52))
title("Wet Days 5 cm")
points(MonthlySummary_location$longitude, MonthlySummary_location$latitude, col = MonthlySummary_location$color, pch = 19, cex = 1.5)
image.plot(legend.only = TRUE, zlim = range(MonthlySummary$Wet_days5cm), col = colors, legend.lab = "Wet_days5cm")

par(mar=c(4,4,4,4))
map("state",xlim=c(-128,-95),ylim=c(30,52))
title("Wet Days 10 cm")
points(MonthlySummary_location$longitude, MonthlySummary_location$latitude, col = MonthlySummary_location$color, pch = 19, cex = 1.5)
image.plot(legend.only = TRUE, zlim = range(MonthlySummary$Wet_days10cm), col = colors, legend.lab = "Wet_days10cm")



