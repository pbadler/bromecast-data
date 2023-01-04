###
### Flag suspicious records
###

# import formatted growth and phenology data
pgD <- read.csv(paste0("../deriveddata/",dosite,doyear,"_growthphenology_by_plantID.csv"),header=T)
pgD <- pgD[order(pgD$plantID,pgD$jday),] # ensure chronological order

# import plantID list and set up dataframe to hold flags
tmp <- read.csv(paste0("../deriveddata/",dosite,doyear,"_plantID.csv"),header=T)
flagD <- data.frame(plantID = tmp$plantID,
                    missing_plant=NA,
                    late_emergence=NA,
                    resurrection=NA,
                    herbivory=NA,
                    frostheave_pregerm=NA,
                    frostheave_postgerm=NA,
                    pheno_regress=NA,
                    bad_position=NA)
rm(tmp)

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
    
    flagD$late_emergence[i] <- flag_lateemergence(dodata)
    
    
  } # end if else
  
}

