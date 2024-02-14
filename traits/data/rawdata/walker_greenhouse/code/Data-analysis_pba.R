library(dplyr)
library(Hmisc)

#Set WD to main project folder
#setwd("C:/Users/A01972774/OneDrive/Desktop/2023")

rawdata <- read.csv("../data/Raw-Data-BRTE-Study.csv") ##reading in raw data file

#flat_ids <- read.csv("../data/BRTE_Flat_IDs.csv")


# replace dots with NAs
rawdata[rawdata=="."] <- NA

write.csv(rawdata,"Raw-Data-BRTE-Study-NAs.csv",row.names=F)

rawdata <- read.csv("Raw-Data-BRTE-Study-NAs.csv",header=T) ##reading in raw data file
#rawdata <- merge(rawdata,flat_ids,all.x=T)

# count NAs by column
colSums(is.na(rawdata))

genotypes <- sort(unique(rawdata$Genotype.ID))

rawdata$SLA <- (rawdata$Shoot.leaf.area..cm2.)/(rawdata$Shoot.dry.wt...g.)

rawdata$SRL <- rawdata$Root.Length..cm./rawdata$Root.dry.wt..g.

rawdata$LeafSize <- rawdata$Shoot.leaf.area..cm2./rawdata$Leaf.no..35d

rawdata$logRSratio <- log(rawdata$Root.dry.wt..g./rawdata$Shoot.dry.wt...g.)


# test for differences among genotypes (random effects approach)
library(lme4)

#create column where genotype ID is treated as a factor

rawdata <- transform(
  rawdata, gfactor= as.factor(rawdata$Genotype.ID)
)

#run random effects 
g_random <- lmer(SRL ~ 1 + + Time.to.emergence..d. + (1|gfactor), data=rawdata)
summary(g_random)
performance::icc(g_random)
##ICC = .034 very minimal variation by genotype


g_randomSLA <- lmer(SLA ~ 1 + Time.to.emergence..d. + (1|gfactor), data=rawdata)
summary(g_randomSLA)
performance::icc(g_randomSLA)
##ICC = .058 very minimal variation by genotype


g_random_rootlength <- lmer(Root.Length..cm. ~ 1 + Time.to.emergence..d.+ (1|gfactor), data=rawdata)
performance::icc(g_random_rootlength)
#ICC = .064

g_random_leafSize <- lmer(LeafSize ~ 1 + + Time.to.emergence..d. + (1|gfactor), data=rawdata)
performance::icc(g_random_leafSize)

g_random_Emerge<- lmer(Time.to.emergence..d. ~ 1 +  Seed.wt...g. + (1|gfactor), data=rawdata)
performance::icc(g_random_Emerge)
#ICC = .064

g_random_seedmass<- lmer(Seed.wt...g. ~ 1  + (1|gfactor), data=rawdata)
performance::icc(g_random_seedmass)
#ICC = .064

#Take means and std dev of each data point

g_means <- rawdata %>%
  group_by(Genotype.ID) %>%
  summarise(missing =sum(is.na(SLA)),
            SLA_sd = sd(SLA,na.rm=TRUE),
            SLA = mean(SLA,na.rm=TRUE), 
            SRL_sd = sd(SRL,na.rm=TRUE),
            SRL = mean(SRL,na.rm=TRUE),
            seed_weight_sd = sd(Seed.wt...g., na.rm = TRUE),
            seed_weight = mean(Seed.wt...g., na.rm = TRUE),
            seed_SA_sd = sd(Seed.ProjArea.cm2., na.rm= TRUE),
            seed_SA= mean(Seed.ProjArea.cm2., na.rm = TRUE),
            emergence_time_sd= sd(Time.to.emergence..d., na.rm= TRUE),
            emergence_time= mean(Time.to.emergence..d., na.rm = TRUE),
            plant_height_21d_SD = sd(Plant.ht..21d..cm., na.rm=TRUE),
            plant_height_21d = mean(Plant.ht..21d..cm., na.rm=TRUE),
            leaf_no_21d_SD = sd(Leaf.no..21d, na.rm= TRUE),
            leaf_no_21d = mean(Leaf.no..21d, na.rm = TRUE),
            plant_height_35d_SD = sd(Plant.ht..35d..cm., na.rm =TRUE),
            plant_height_35d = mean(Plant.ht..35d..cm., na.rm = TRUE),
            leaf_no_35d_SD = sd(Leaf.no..35d, na.rm=TRUE),
            leaf_no_35d = mean(Leaf.no..35d, na.rm = TRUE),
            shoot_dryweight_SD =sd(Shoot.dry.wt...g., na.rm=TRUE),
            shoot_dryweight = mean(Shoot.dry.wt...g., na.rm = TRUE),
            leaf_area_SD = sd(Shoot.leaf.area..cm2., na.rm = TRUE),
            leaf_area = mean(Shoot.leaf.area..cm2., na.rm= TRUE),
            root_weight_SD = sd(Root.dry.wt..g., na.rm = TRUE),
            root_weight = mean(Root.dry.wt..g., na.rm =TRUE),
            root_length_SD = sd(Root.Length..cm., na.rm= TRUE),
            root_length= mean(Root.Length..cm., na.rm= TRUE),
            root_to_shoot_SD = sd(logRSratio, na.rm=TRUE),
            root_to_shoot_mean = mean(logRSratio, na.rm = TRUE)
            )
      

# what is uo with the SRL outlier?
# which genotype is it?
tmp <- which(g_means$SRL>40000)
badguy <- g_means$Genotype.ID[tmp]
# look at rawdata gor this genotype
rawdata[which(rawdata$Genotype.ID==badguy),]
# not clear what exactly is wrong with this one data point,
# combination of very low root dry weight and high root length,
# but on their own, neither of those values looks bad
# Let's remove this one data point
rawdata <- subset(rawdata, SRL < 150000 | is.na(SRL))


g_means <- rawdata %>%
  group_by(Genotype.ID) %>%
  summarise(missing =sum(is.na(SLA)),
            SLA_sd = sd(SLA,na.rm=TRUE),
            SLA = mean(SLA,na.rm=TRUE), 
            SRL_sd = sd(SRL,na.rm=TRUE),
            SRL = mean(SRL,na.rm=TRUE),
            seed_weight_sd = sd(Seed.wt...g., na.rm = TRUE),
            seed_weight = mean(Seed.wt...g., na.rm = TRUE),
            seed_SA_sd = sd(Seed.ProjArea.cm2., na.rm= TRUE),
            seed_SA= mean(Seed.ProjArea.cm2., na.rm = TRUE),
            emergence_time_sd= sd(Time.to.emergence..d., na.rm= TRUE),
            emergence_time= mean(Time.to.emergence..d., na.rm = TRUE),
            plant_height_21d_SD = sd(Plant.ht..21d..cm., na.rm=TRUE),
            plant_height_21d = mean(Plant.ht..21d..cm., na.rm=TRUE),
            leaf_no_21d_SD = sd(Leaf.no..21d, na.rm= TRUE),
            leaf_no_21d = mean(Leaf.no..21d, na.rm = TRUE),
            plant_height_35d_SD = sd(Plant.ht..35d..cm., na.rm =TRUE),
            plant_height_35d = mean(Plant.ht..35d..cm., na.rm = TRUE),
            leaf_no_35d_SD = sd(Leaf.no..35d, na.rm=TRUE),
            leaf_no_35d = mean(Leaf.no..35d, na.rm = TRUE),
            shoot_dryweight_SD =sd(Shoot.dry.wt...g., na.rm=TRUE),
            shoot_dryweight = mean(Shoot.dry.wt...g., na.rm = TRUE),
            leaf_area_SD = sd(Shoot.leaf.area..cm2., na.rm = TRUE),
            leaf_area = mean(Shoot.leaf.area..cm2., na.rm= TRUE),
            root_weight_SD = sd(Root.dry.wt..g., na.rm = TRUE),
            root_weight = mean(Root.dry.wt..g., na.rm =TRUE),
            root_length_SD = sd(Root.Length..cm., na.rm= TRUE),
            root_length= mean(Root.Length..cm., na.rm= TRUE),
            root_to_shoot_SD = sd(logRSratio, na.rm=TRUE),
            root_to_shoot_mean = mean(logRSratio, na.rm = TRUE),
            EmergeTime_SD = sd(Time.to.emergence..d., na.rm=TRUE),
            EmergeTime_SD_mean = mean(Time.to.emergence..d., na.rm = TRUE),
            LeafSize_SD = sd(LeafSize, na.rm=TRUE),
            LeafSize_mean = mean(LeafSize, na.rm = TRUE)
  )

      
##Analyze relationship between SLA and SRL
plot(g_means$SLA,g_means$SRL, main = "Specific Leaf Area\nto Specific Root Length by Population",
     xlab= "SLA (cm2/g)", ylab ="SRL (cm/g)")

SLA.SRL <- lm(SRL~SLA, data= g_means)
summary(SLA.SRL)
abline(SLA.SRL, lty= 1, lwd =2) ## Linear regression

##Multiple R-squared:  0.1686,	Adjusted R-squared:  0.1564 
## F-statistic: 13.79 on 1 and 68 DF,  p-value: 0.0004147

plot(rawdata$SLA, rawdata$SRL, main ="Specific Leaf Area to Specific Root Length
     by Individual", xlab = "SLA (cm2/g)", ylab ="SRL (cm/g)")
indivSLA.SRL <- lm(SRL~SLA, data =rawdata)
summary(indivSLA.SRL)
abline(indivSLA.SRL, lty =1, lwd =2)



# plot lots of pairs of variables
pairs(g_means[,c(4,6,8,12,30)])

# test whether corr's are significant
results <- rcorr(as.matrix(g_means[,c(4,6,8,12,30)]))


##SLA-SRL
##Seed weight and emergence

pairs(g_means[,c(6,10,14,16)])

results2 <- rcorr(as.matrix(g_means[, c(6,10,14,16)]))

##Plant height and seed surface area 

pairs(g_means[,c(18,20,22,26,28)])

results3 <- rcorr(as.matrix(g_means[,c(18,20,22,26,28)]))

results3$r
results3$P

# test for correlations
##cor.test(g_means[,c(4,6,8)]) ##This isn't working

gemergence_to_SRL <- lm(SRL~emergence_time, data= g_means)
summary(emergence_to_SRL)

plant_height_21d_to_SRL <- lm(SRL~plant_height_21d, data= g_means)
summary(plant_height_21d_to_SRL)


plant_height_35d_to_SRL <- lm(SRL~plant_height_35d, data= g_means)
summary(plant_height_35d_to_SRL)


plant_height35d_to_root_length <- lm(root_length~plant_height_21d, data= g_means)

summary(plant_height35d_to_root_length)



##Calculating overall means from individual seeds
SLA.mean.allseeds <- mean(rawdata$SLA)

SRL.mean.allseeds <- mean(rawdata$SRL)


# Plotting lines to show means for SRL and SLA 

## **** This isn't working- WHY ####
# abline(h=SRL.mean.allseeds, col= "darkorange4")
# abline(v=SLA.mean.allseeds, col="darkgreen")



#Analyze relationship between SRL and emergence time####

SRL.vs.emergence <- lm(g_means$SRL ~ g_means$emergence_time) 
##SRL to emergence by mean


#linear regression
summary(SRL.vs.emergence)
#plot relationship
plot(g_means$emergence_time, g_means$SRL,
     main = "Emergence time and Specific Root Length by Genotype",
     xlab= "Days to Emergence", ylab= "SRL (cm2/g)")

abline(SRL.vs.emergence, lty=1, lwd= 2)

SRL.vs.emergence_allseeds  <- lm(rawdata$SRL ~ rawdata$Time.to.emergence..d.)
summary(SRL.vs.emergence_allseeds)


#analyze relationship between SRL and plant height####

##By all seeds
SRL.vs.21dayheight <- lm(rawdata$SRL~rawdata$Plant.ht..21d..cm.)
summary(SRL.vs.21dayheight)




#### Climate Data analysis ####
climate <- read.csv("../data/BioclimateOfOrigin_AllGenotypes.csv")

# subset to genotypes in this study
climate <- subset(climate, climate$genotype %in% g_means$Genotype.ID)

# do a PCA of climate data to reduce dimensionality

# First center and scale all bioclimatic variables
climate_scaled <- climate
climate_scaled[,6:ncol(climate_scaled)] <- apply(climate_scaled[,6:ncol(climate_scaled)], 2, scale)

# Run PCA
pca_out <- prcomp(climate_scaled[,6:ncol(climate_scaled)])

#graph PCA
biplot(pca_out)

# Get percent explained by each PC axis
round(pca_out$sdev^2 / sum(pca_out$sdev^2),3) -> perc_explained


# Bind PC axis data with original data
cbind(climate_scaled, pca_out$x) -> climate_scaled

# select a few columns and join to g_means
g_meansXclim <- merge(g_means,climate_scaled[,c("genotype","lat","lon","ann.mean.tmp","ann.prc",
                                                "PC1","PC2","PC3")],
                      all.x=T,by.x="Genotype.ID",by.y="genotype")


# visualize

plot(g_meansXclim$PC1,g_meansXclim$SRL)
cor.test(g_meansXclim$PC1,g_meansXclim$SRL)

plot(g_meansXclim$PC2,g_meansXclim$SLA) #p-value = 0.09938
cor.test(g_meansXclim$PC2,g_meansXclim$SLA)

plot(g_meansXclim$PC1,g_meansXclim$root_length)
cor.test(g_meansXclim$PC1,g_meansXclim$root_length) #p-value = 0.08687

plot(g_meansXclim$PC1,g_meansXclim$emergence_time) #p-value = 0.5518
cor.test(g_meansXclim$PC1,g_meansXclim$emergence_time)

plot(g_meansXclim$PC3,g_meansXclim$emergence_time) #p-value = 0.06127
cor.test(g_meansXclim$PC3,g_meansXclim$emergence_time)

#seed mass
plot(g_meansXclim$PC2,g_meansXclim$seed_weight)
cor.test(g_meansXclim$PC2,g_meansXclim$seed_weight)

# leaf size
plot(g_meansXclim$PC1,g_meansXclim$LeafSize_mean)
# nothing here

library("ggplot2")

ggplot(g_meansXclim, aes(x=SLA, y= SRL, color= PC1)) +
  geom_point(size=2) +
  geom_smooth( method="lm", se = FALSE, color = "black") +
  scale_color_gradient(low= "blue", high = "red")+
  labs(x = "Specific Leaf Area (cm2/g)", y = "Specific Root Length (cm/g)", color = "PC1")+
  ggtitle("Relationship of Average SLA to \nAverage SRL, Color Coded by PC1")+
  theme(text = element_text(color="black", size = 10),
      axis.title = element_text(color="black", size=18), #Changes axis title size and color
      axis.text.x = element_text(color="black", size = 10, margin = margin(l=0, r=0, t=2, b=1)), #Changes x axis text size
      axis.text.y = element_text(color="black", size = 10, margin = margin(l = 2, r=2)), #Changes y axis text siz
      legend.title = element_text(size=12),
      legend.spacing = unit(1,"line"),
      #legend.key.size = unit(1, "cm"),
      legend.spacing.y = unit(4, "pt"),
      legend.spacing.x = unit(4, "pt"),
      #legend.key = element_rect(fill="white"),
      legend.key.height = unit(20, "pt"), #change key
      legend.key.width = unit(20, "pt"), #change key
      legend.text = element_text(size=12),
      panel.background = element_rect(fill = "white"),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(size = .3, linetype = "solid", colour = "black"),
      plot.margin= grid::unit(c(.1, 1, 0, 0), "in"),
      plot.title = element_text(size = 20, margin = margin(t = 0, r = 0, b = 0, l = 0), face="bold", vjust = .5, hjust = .5)) #modifies location and size of ggtitle




#Graph with color coded points on gradient by PC1
#PC1 high end is mean annual temperature, low end is annual precip. So hot and dry
#versus cool and wet

## PC2 precipitation in coldest quarter - mean diurnal range
ggplot(g_meansXclim, aes(x=SLA, y= SRL, color= PC2)) +
  geom_point(size=2) +
  geom_smooth( method="lm", se = FALSE, color = "black") +
  scale_color_gradient(low= "red", high = "blue")+
  labs(x = "Specific Leaf Area (cm/g)", y = "Specific Root Length (cm2/g)", color = "PC2")+
  ggtitle("Relationship of Average SLA to\nAverage SRL, Color Coded by PC2")+
  theme(text = element_text(color="black", size = 10),
        axis.title = element_text(color="black", size=18), #Changes axis title size and color
        axis.text.x = element_text(color="black", size = 10, margin = margin(l=0, r=0, t=2, b=1)), #Changes x axis text size
        axis.text.y = element_text(color="black", size = 10, margin = margin(l = 2, r=2)), #Changes y axis text siz
        legend.title = element_text(size=12),
        legend.spacing = unit(1,"line"),
        #legend.key.size = unit(1, "cm"),
        legend.spacing.y = unit(4, "pt"),
        legend.spacing.x = unit(4, "pt"),
        #legend.key = element_rect(fill="white"),
        legend.key.height = unit(20, "pt"), #change key
        legend.key.width = unit(20, "pt"), #change key
        legend.text = element_text(size=12),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = .3, linetype = "solid", colour = "black"),
        plot.margin= grid::unit(c(.1, 1, 0, 0), "in"),
        plot.title = element_text(size = 20, margin = margin(t = 0, r = 0, b = 0, l = 0), face="bold", vjust = .5, hjust = .5)) #modifies location and size of ggtitle




#root mass proportion
#leaf mass proportion






