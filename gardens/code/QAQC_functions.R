# functions used in data cleaning and formatting

check_chronology <- function(plantD){
  if(sum(plantD$jday==sort(plantD$jday))!=nrow(plantD)) stop("data not chronological")
}

flag_lateemergence <- function(plant_data){
  # was plant absent first visit, later present?
  if(plant_data$live[1]=="Y"){
    return(NA)  # emergence on first visit
  }else if(sum(plant_data$live=="Y")==0){
    return(NA)  # never emerged
  }else{
    # return date of first presence
    tmp <- which(plant_data$live=="Y")
    if(length(tmp)>1) tmp <- tmp[1] 
    return(plant_data$jday[tmp])
  }
}

get_emergence_date <- function(plant_data){
  # date of first presence
  if(sum(plant_data$live=="Y")==0){
    return(NA)  # never emerged
  }else{
    # return date of first presence
    tmp <- which(plant_data$live=="Y")
    if(length(tmp)>1) tmp <- tmp[1] 
    return(plant_data$jday[tmp])
  }
}

get_death_date <- function(plant_data){
  # date of first absence after a presence
  out <- NA  # death not observed
  tmp <- ifelse(plant_data$live=="Y",1,0) 
  tmp <- diff(tmp)
  deaths <- which(tmp==-1)
  if(length(deaths)>0){
      out <- plant_data$jday[1+min(deaths)]
  }
  return(out)
}

flag_resurrection <- function(plant_data){
  # was plant observed alive after being observed dead?
  out <- NA
  tmp <- ifelse(plant_data$live=="Y",1,0) 
  tmp <- diff(tmp)
  births <- which(tmp==1)
  deaths <- which(tmp==-1)
  if(length(births)>0 & length(deaths)>0){
    if(max(births)>min(deaths)){
      out <- plant_data$jday[1+max(births)]
    }
  }
  return(out)
}

flag_phenoregress <- function(plant_data,pheno_table){
  # does phenology go backwards? returns how # of stages of regression
  out <- NA
  if(sum(plant_data$live=="Y")>1){
    plant_data <- merge(plant_data,pheno_table)
    plant_data <- plant_data[order(plant_data$jday),]
    tmp <- diff(plant_data$v_numeric)
    if(min(tmp)<0) out <- min(tmp)
  }
  return(out)
}

flag_growthregress <- function(plant_data){
  # does leaf length go backwards? return max mm of shrinkage
  out <- NA
  if(sum(!is.na(plant_data$length_mm)>1)){
    tmp <- diff(plant_data$length_mm)
    if(sum(!is.na(tmp)>0)){
       if(min(tmp,na.rm=T)<0) out <- min(tmp,na.rm=T)
    }
  }
  return(out)
}

flag_frostheave <- function(plant_data){
  out <- NA
  # date of first frost heave
  tmp <- which(plant_data$frost_heave=="Y")
  # return date 
  if(length(tmp)>1){
    tmp <- min(tmp)
    out <- plant_data$jday[tmp]
  }
  return(out)
}

flag_herbivory <- function(plant_data){
  out <- NA
  # date of first herbivory observation
  tmp <- which(plant_data$herbivory=="Y")
  # return date 
  if(length(tmp)>1){
    tmp <- min(tmp)
    out <- plant_data$jday[tmp]
  }
  return(out)
}
