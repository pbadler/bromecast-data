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

flag_resurrection <- function(plant_data){
  # was plant observed alive after being observed dead?
  resurrection_date <- NA
  tmp <- ifelse(plant_data$live=="Y",1,0)
  tmp <- diff(tmp)
  if(sum(tmp==-1)>0){ # was death observed?
    first_death <- which(tmp==-1)[1]
    resurrection_index <- which(tmp[(first_death+1):length(tmp)]==1)[1]
    if(!is.null(resurrection_index)){
      resurrection_date <- plant_data$jday[1+first_death+resurrection_index]
    }
  }
  return(resurrection_date)
}
