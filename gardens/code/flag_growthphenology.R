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
                    growth_regress=NA,
                    bad_position=NA)
rm(tmp)

# get list of phenological stages, and assign numeric order
phenoD <- data.frame("v"=sort(unique(pgD$v)))
phenoD$v_numeric <- NA
phenoD$v_numeric[phenoD$v=="V0"] <- 1
phenoD$v_numeric[phenoD$v=="V1"] <- 2
phenoD$v_numeric[phenoD$v=="V2"] <- 3
phenoD$v_numeric[phenoD$v=="V3"] <- 4
phenoD$v_numeric[phenoD$v=="V3+"] <- 5
phenoD$v_numeric[phenoD$v=="BS"] <- 6
phenoD$v_numeric[phenoD$v=="FG"] <- 7
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
    
    flagD$late_emergence[i] <- flag_lateemergence(dodata)
    
    flagD$resurrection[i] <- flag_resurrection(dodata)
    
    flagD$pheno_regress[i] <- flag_phenoregress(dodata,phenoD)
    
    flagD$growth_regress[i] <- flag_growthregress(dodata)
    
  } # end if else
  
} # next i

