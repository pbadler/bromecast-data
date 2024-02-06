
# call from run_everything.R

###
### probability of producing seeds -- get site means
###

# Get rid of missing records
D <- subset(D,Reproduced!="missing")

# convert N/Y to 0/1
D$Reproduced <- as.numeric(as.factor(D$Reproduced))-1

prob_reprod <- D %>% group_by(SiteCode,Year,Treatment) %>%
                summarize(mean=mean(Reproduced)) %>% 
                pivot_wider(names_from=Treatment, names_prefix="pR_",values_from=c(mean))
prob_reprod$pR_overall <- (prob_reprod$pR_control + prob_reprod$pR_removal)/2
                               
###
###  calculate site means for fecundity
###

fecD <- subset(D,D$Fecundity>0 & D$fecundityflag==0)

mean_fecundity <- fecD %>% group_by(SiteCode,Year,Treatment) %>%
  summarize(mean=mean(log(Fecundity)),
            q05=quantile(log(Fecundity),0.05),
            q95=quantile(log(Fecundity),0.95) )  %>% 
  pivot_wider(names_from=Treatment, names_prefix="logF_",values_from=c(mean, q05, q95))

mean_fecundity$fec_logratio <- mean_fecundity$mean_logF_control - mean_fecundity$mean_logF_removal


# join site means
site_means <- merge(prob_reprod,mean_fecundity,all.x=T)

# calculate fitness on log scales
site_means$fit_control <- log(site_means$pR_control) + site_means$mean_logF_control
site_means$fit_removal <- log(site_means$pR_removal) + site_means$mean_logF_removal
site_means$fit_logratio <- site_means$fit_control - site_means$fit_removal

# set fitness NAs (caused by zero prob. reproduction) to -6 (lower than lowest log fitness)
site_means$fit_control[is.na(site_means$fit_control)] <- -6
site_means$fit_removal[is.na(site_means$fit_removal)] <- -6


# merge to site info and climate data
site_means <- merge(siteD,site_means)

###
### Figures
###

# make transparent colors
colvals <- col2rgb("black")
myblack <- rgb(colvals[1],colvals[2],colvals[3],alpha=120,maxColorValue = 255)
colvals <- col2rgb("blue3")
myblue <- rgb(colvals[1],colvals[2],colvals[3],alpha=120,maxColorValue = 255)
colvals <- col2rgb("red3")
myred <- rgb(colvals[1],colvals[2],colvals[3],alpha=120,maxColorValue = 255)
colvals <- col2rgb("green4")
mygreen <- rgb(colvals[1],colvals[2],colvals[3],alpha=120,maxColorValue = 255)
colvals <- col2rgb("darkorchid3")
mypurple <- rgb(colvals[1],colvals[2],colvals[3],alpha=120,maxColorValue = 255)
colvals <- col2rgb("darkorange1")
myorange <- rgb(colvals[1],colvals[2],colvals[3],alpha=120,maxColorValue = 255)

# map of locations
png("../figures/site_map.png",height=4,width=6,units="in",res=600)
par(mar=c(2,2,2,2))
map("state",xlim=c(-128,-95),ylim=c(30,52))
points(x=site_means$Lon[site_means$Year==2021],y=site_means$Lat[site_means$Year==2021],
       pch=0, cex=1.5,col=mypurple)
points(x=site_means$Lon[site_means$Year==2022],y=site_means$Lat[site_means$Year==2022],
       pch=1, cex=1.5,col=myorange)
points(x=site_means$Lon[site_means$Year==2023],y=site_means$Lat[site_means$Year==2023],
       pch=2, cex=1.5,col=myblue)
legend("bottomright",c("2021","2022","2023"),pch=c(0,1,2),col=c(mypurple,myorange,myblue))
dev.off()

# treatment effects on prob of reproduction
png("../figures/pR_treatments.png",height=3.5,width=5,units="in",res=600)
par(tcl=-0.2,mgp=c(2,0.5,0),mar=c(3,5,1,1))
plot(site_means$pR_removal,site_means$pR_control,col=myblack,pch=16,
     xlab="Removal",ylab="Control", xlim=c(0,1), ylim=c(0,1))
abline(0,1,lty="dashed")
dev.off()

# map mean prob of reproduction
par(mar=c(4,4,4,4))
map("state",xlim=c(-128,-95),ylim=c(30,52))
#title("Fitness")
points(x=site_means$Lon,y=site_means$Lat,pch=".",col="black")
symbols(x=site_means$Lon,y=site_means$Lat,circles=site_means$pR_overall,inches=0.4,add=T)

# mean prob of reproduction and climate
png("../figures/pR_prcp.png",height=3.5,width=5,units="in",res=600)
par(tcl=-0.2,mgp=c(2,0.5,0),mar=c(3,5,1,1))
mycol <- ifelse(site_means$Lon < -109, mypurple,myorange)
plot(site_means$prcp,site_means$pR_overall,pch=16, col=mycol,
     xlab="Precipitation (mm)",ylab="Mean prob. seed production")
legend("topleft",c("Western","Eastern"),pch=16,col=c(mypurple,myorange),bty="n")
dev.off()

# # paired t-test
# x <- prob_reprod$mean_control-prob_reprod$mean_removal
# summary(lm(x~1))

###
### fecundity figures
###

threshold <- 0.05
tmp <- which(site_means$pR_control >= threshold & site_means$pR_removal >= threshold)

# treatment 1:1
png("../figures/fecundity_treatments.png",height=3.5,width=5,units="in",res=600)
par(tcl=-0.2,mgp=c(2,0.5,0),mar=c(3,5,1,1))
plot(site_means$mean_logF_removal[tmp],site_means$mean_logF_control[tmp], col="black",
     xlab="Removal",ylab="Control", xlim=c(0,7), ylim=c(0,7),pch=16,main="log Fecundity")
abline(0,1,lty="dashed")
# removals confidence intervals
arrows(x0=site_means$q05_logF_removal[tmp],
       y0=site_means$mean_logF_control[tmp],
       x1=site_means$q95_logF_removal[tmp],
       y1=site_means$mean_logF_control[tmp],angle=90,length=0,code=3)
# controls confidence intervals
arrows(x0=site_means$mean_logF_removal[tmp],
       y0=site_means$q05_logF_control[tmp],
       x1=site_means$mean_logF_removal[tmp],
       y1=site_means$q95_logF_control[tmp],angle=90,length=0,code=3)
dev.off()

# map fecundity in removals
par(mar=c(4,4,4,4))
map("state",xlim=c(-128,-95),ylim=c(30,52))
#title("Fitness")
points(x=site_means$Lon[tmp],y=site_means$Lat[tmp],pch=".",col="black")
symbols(x=site_means$Lon[tmp],y=site_means$Lat[tmp],circles=site_means$mean_logF_removal[tmp],inches=0.4,add=T)

# mean removal fecundity and climate
png("../figures/fecundity_prcp.png",height=3.5,width=5,units="in",res=600)
par(tcl=-0.2,mgp=c(2,0.5,0),mar=c(3,5,1,1))
mycol <- ifelse(site_means$Lon[tmp] < -109, mypurple,myorange)
plot(site_means$prcp[tmp],site_means$mean_logF_removal[tmp],pch=16, col=mycol,
     xlab="Precipitation (mm)",ylab="log Fecundity")
legend("topright",c("Western","Eastern"),pch=16,col=c(mypurple,myorange))
dev.off()

# removal fecundity vs effect of competition
mycol <- ifelse(site_means$Lon[tmp] < -109, "blue","red")
plot(site_means$mean_logF_removal[tmp],site_means$fec_logratio[tmp], 
     xlab="log Fecundity in Removals",ylab="log(Control/Removal)",pch=16,col=mycol)
abline(h=0,lty="dashed")
legend("bottomleft",c("west","east"),pch=16,col=c("blue","red"))


# not much here
pdf("../figures/SWExLongitude.pdf",height=3, width=8.5)

par(mfrow=c(1,3),mar=c(3,5,1,1),mgp=c(2,0.5,0))

plot(site_means$Lon[tmp],site_means$swe_mean[tmp],xlab="Longitude",ylab="Mean daily SWE",pch=16,cex=0.5,
     main="Fecundty in removals")
symbols(x=site_means$Lon[tmp],y=site_means$swe_mean[tmp],circles=sqrt(exp(site_means$mean_logF_removal[tmp])),inches=0.4,add=T,fg="blue")

plot(site_means$Lon[tmp],site_means$swe_mean[tmp],xlab="Longitude",ylab="Mean daily SWE",pch=16,cex=0.5,
     main="Fecundity in controls")
symbols(x=site_means$Lon[tmp],y=site_means$swe_mean[tmp],circles=sqrt(exp(site_means$mean_logF_control[tmp])),inches=0.4,add=T,fg="blue")

plot(site_means$Lon[tmp],site_means$swe_mean[tmp],xlab="Longitude",ylab="Mean daily SWE",pch=16,cex=0.5,
     main="Effect of competition")
tmp2 <- which(site_means$fec_logratio[tmp]<0)
symbols(x=site_means$Lon[tmp[tmp2]],y=site_means$swe_mean[tmp[tmp2]],
        circles=abs(site_means$fec_logratio[tmp[tmp2]]),inches=0.4,add=T,fg="red")
tmp2 <- which(site_means$fec_logratio>0)
symbols(x=site_means$Lon[tmp[tmp2]],y=site_means$swe_mean[tmp[tmp2]],
        circles=abs(site_means$fec_logratio[tmp[tmp2]]),inches=0.4,add=T,fg="blue")

dev.off()


# fitness figures


# assign color categories based on fitness outcomes
mycol <- rep(myblack,nrow(site_means))
tmp <- which(site_means$fit_control<0 & site_means$fit_removal<0) 
mycol[tmp] <- myred
tmp <- which(site_means$fit_control>0 & site_means$fit_removal<0)
mycol[tmp] <- mygreen
tmp <- which(site_means$fit_control<0 & site_means$fit_removal>0)
mycol[tmp] <- myblue

# set threshold for prob of reproduction
threshold <- 0.05
mypch <- ifelse(site_means$pR_control >= threshold & site_means$pR_removal >= threshold,16,1)

# treatment 1:1
png("../figures/fitness_treatments.png",height=3.5,width=5,units="in",res=600)
par(tcl=-0.2,mgp=c(2,0.5,0),mar=c(3,5,1,1))
plot(site_means$fit_removal,site_means$fit_control, col=mycol,
     xlab="Removal",ylab="Control", xlim=c(-6.1,7), ylim=c(-6.1,7),pch=mypch,cex=1.5,main="log Fitness")
abline(h=0)
abline(v=0)
abline(0,1,lty="dashed")
dev.off()

# map outcome category
png("../figures/fitness_map.png",height=4,width=6,units="in",res=600)
par(mar=c(2,2,2,2))
map("state",xlim=c(-128,-95),ylim=c(30,52))
points(x=site_means$Lon,y=site_means$Lat,col=mycol,pch=mypch,cex=2)
dev.off()

# mean removal fitness and climate
#png("../figures/fitness_prcp.png",height=3.5,width=5,units="in",res=600)
#par(tcl=-0.2,mgp=c(2,0.5,0),mar=c(3,5,1,1))
plot(site_means$prcp,site_means$fit_removal,pch=mypch, col=mycol,lwd=2,
     xlab="Precipitation (mm)",ylab="log Fitness")
abline(h=0,lty="dashed")
#dev.off()

# removal fitness vs effect of competition
plot(site_means$fit_removal,site_means$fit_logratio, 
     xlab="log Fitness in Removals",ylab="log(Control/Removal)",pch=mypch,col=mycol,lwd=2)
abline(h=0,lty="dashed")


# longitude, prcp, and fitness outcome
png("../figures/fitness_lon_prcp.png",height=3.5,width=5,units="in",res=600)
par(tcl=-0.2,mgp=c(2,0.5,0),mar=c(3,5,1,1))
plot(site_means$Lon,site_means$prcp,pch=mypch,col=mycol,lwd=2,cex=1.5,
     xlab="Longitude",ylab="Precipitation (mm)")
legend("bottomleft",c("Climate limits","Competition limits"),pch=16,col=c(myred,myblue))
dev.off()

# longitude, swe, and fitness outcome
plot(site_means$Lon,site_means$swe_mean,pch=mypch,col=mycol,lwd=2,
     xlab="Longitude",ylab="Snow water equivalent (cm)")

###
### cartoon plots for removal and control fitness, to log or not to log
###

# confirm log approach works when competition has 
# multiplicative effect on performance 

# constant proportional effect
removal <- c(1,2,4,8,16,32)
control <- removal/2

plot(removal,control,type="l",col="black")
abline(0,1,lty="dashed")

plot(log(removal),log(control),type="l",col="red")
abline(0,1)

plot(log(removal),log(control/removal))

# effect of competition increases with fitness
control <- removal^(0.5)

plot(removal,control,type="l",col="black")
abline(0,1,lty="dashed")

plot(log(removal),log(control),type="l",col="black")
abline(0,1)

plot(log(removal),log(control/removal))

# effect of competition decreases with fitness
control <- removal*c(0.1,0.2,0.3,0.4,0.5,0.6)

plot(removal,control,type="l",col="black")
abline(0,1,lty="dashed")

plot(log(removal),log(control),type="l",col="black")
abline(0,1,lty="dashed")

plot(log(removal),log(control/removal))


rm(removal,control)

# scratch
plot(site_means$swe_mean,site_means$fit_control)


