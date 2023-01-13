###
### Flag suspicious records
###

# import formatted growth and phenology data
pgD <- read.csv(paste0("../deriveddata/",dosite,doyear,"_growthphenology_by_plantID.csv"),header=T)

# import and merge standardized notes
tmp <- read.csv(paste0("../deriveddata/",dosite,doyear,"_notes_actions.csv"),header=T)
tmp <- subset(tmp, action=="flag")  # drop "ignore" records
tmp <- tmp[,c("notes","standard_note")] # drop "action" column
pgD <- merge(pgD,tmp,all.x=T)
pgD <- pgD[,c(2:ncol(pgD),1)] # reorder columns, put raw notes last

# ensure chronological order
pgD <- pgD[order(pgD$plantID,pgD$jday),] 

# import plantID list and set up dataframe to hold flags
tmp <- read.csv(paste0("../deriveddata/",dosite,doyear,"_plantID.csv"),header=T)
flagD <- data.frame(plantID = tmp$plantID,
                    missing_plant=NA,
                    emergence_date=NA,
                    death_date=NA,
                    resurrection_date=NA,
                    pheno_regress=NA,
                    growth_regress_mm=NA,
                    herbivory_date=NA,
                    frostheave_date=NA,  # at SS, frost heaving not tracked til day 122 in 2022
                    bad_position=NA,
                    other=NA)
rm(tmp)

# get list of phenological stages, and assign numeric order
phenoD <- data.frame("v"=sort(unique(pgD$v)))
phenoD$v_numeric <- NA
phenoD$v_numeric[phenoD$v=="V0"] <- 1
phenoD$v_numeric[phenoD$v=="V1"] <- 2
phenoD$v_numeric[phenoD$v=="V2"] <- 3
phenoD$v_numeric[phenoD$v=="V3"] <- 4
phenoD$v_numeric[phenoD$v=="V3+"] <- 5
phenoD$v_numeric[phenoD$v=="BS"] <- 6  # boot stage
phenoD$v_numeric[phenoD$v=="FG"] <- 7 # flowering green
phenoD <- phenoD[order(phenoD$v_numeric),]
#check!
print(phenoD)

# loop through plantIDs and check for each flag
for(i in 1:nrow(flagD)){
  
  # subset data to focal plant
  doplant <- flagD$plantID[i]
  dodata <- pgD[pgD$plantID==doplant,]
  
  # make sure data for this plant exists
  if(nrow(dodata)==0){
    
    flagD$missing_plant[i]<-T
    
  }else{
    # continue to other checks
    
    check_chronology(dodata)
    
    flagD$emergence_date[i] <- get_emergence_date(dodata)
    
    flagD$death_date[i] <- get_death_date(dodata)
    
    flagD$resurrection_date[i] <- flag_resurrection(dodata)
    
    flagD$pheno_regress[i] <- flag_phenoregress(dodata,phenoD)
    
    flagD$growth_regress_mm[i] <- flag_growthregress(dodata)
    
    flagD$herbivory_date[i] <- flag_herbivory(dodata)
    
    flagD$frostheave_date[i] <- flag_frostheave(dodata)
    
    flagD$bad_position[i] <- flag_badposition(dodata)
    
    flagD$other[i] <- flag_other(dodata)
    
  } # end if else
  
} # next i

# write flags to file
write.csv(flagD,file=paste0("../deriveddata/",dosite,doyear,"_flags.csv"),row.names=F)

