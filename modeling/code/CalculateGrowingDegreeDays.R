library(tidyverse)
library(data.table)

alldata = read_csv("modeling/data/AllSitesWPandTemp3.csv")
data = subset(alldata, Year >2020) # First 1-2 years of data may not be as accurate. 
#The model does not know starting conditions very well, so it takes a season or maybe 
#two for the model to produce accurate estimates


#calculate Growing Degrees ------ wet growing degrees = soil temperature when soil is wet (>-1.25 MPa)
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
