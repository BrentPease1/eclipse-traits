#########################################################################
################## Create Occupancy Data with Less Than #################
############## Daily Sample Period from Camera Observations #############
#########################################################################
make_det_hist <- function(df, deployIDs = birdnet$ESIDNumber, species_col, time_stamp, start_eclipse, end_eclipse, interval_length){ # 1 minute, 10 min just mult by 10 instead of 1,etc.
  # df = data frame containing spp, site, timestamps
  # deployID_col = name of column that contains sampling location info. should be passed in quotes "colname"
  # species_col = name of column that contains species name. should be passed in quotes "colname"
  # interval_length = Set the capture period (minute, hour, day, week, month)
    # Set in terms of seconds, so sec = 1, minute=60,hour=60*60 day=60*60*24


  ################## Make a list of all existing deployments ###############
  
  aru.list <- unique(as.character(deployIDs))
  
  ########### Select the species to make the capture history for ###########
  
  
  #Subset the data for just detections of that species
  df.sp <- df[df[[species_col]] == species,]
  
  ####### Calculate Sample Period for each observation ####
  
  #Create sampling period IDs

    df.sp[,SamplePeriod := cut(
      x = as.POSIXct(df.sp[[time_stamp]], format = "%H:%M:%S", tz = "UTC"),  
      breaks = as.POSIXct(seq(
        from = first(as.POSIXct(df.sp[[start_eclipse]], format = "%H:%M:%S", tz = "UTC"), na.rm = TRUE),
        by = interval_length, 
        to = first(as.POSIXct(df.sp[[end_eclipse]], format = "%H:%M:%S", tz = "UTC"), na.rm = TRUE) + interval_length
      )),
      labels = FALSE
    )]

  df.sp[, SamplePeriod := cut(
    x = as.POSIXct(get(time_stamp), format = "%H:%M:%S", tz = "UTC"),  
    breaks = as.POSIXct(seq(
      from = first(as.POSIXct(get(start_eclipse), format = "%H:%M:%S", tz = "UTC"), na.rm = TRUE),
      by = interval_length, 
      to = first(as.POSIXct(get(end_eclipse), format = "%H:%M:%S", tz = "UTC"), na.rm = TRUE) + interval_length
    )),
    labels = FALSE
  ), by = ESIDNumber]
  #Remove all columns but group and SamplePeriod
  df.sp.simple <- df.sp[, .(ESIDNumber, SamplePeriod)]
  
  ################## Add in missing deployment names for ##################
  ############## deployments that did not detect the species ##############
  ########################### of interest #################################
  
  # Find names of missing cams
  missing_aru <- setdiff(aru.list,df.sp.simple$ESIDNumber) 
  
  if(length(missing_aru) == 0){
    'cool'
  } else{
    #Mark them with 0s as SamplePeriod which can be easily removed later
    M_C<-as.data.frame(cbind(missing_aru, rep(0, length(missing_aru))))
    names(M_C)<-c("ESIDNumber", "SamplePeriod")
    
    #Add them into the dataframe with the detection data
    df.sp.simple<-rbind(df.sp.simple, M_C)
    
    #Sort by Deployment
    df.sp.simple<-df.sp.simple[order(ESIDNumber), ]
  }

  
  #################### Create the capture history matrix ###################
  ########################## Just like a Pivot Table #######################
  
  #Reshape the data using melt
  transform=reshape2::melt(df.sp.simple, id.vars="group")
  pivot=reshape::cast(transform, group ~ value, fun.aggregate = length)
  setDT(pivot)
  #Delete the first sample period (0) we made when adding the rest of the
  #Deployments
  this_col <- which(colnames(pivot) == "0")
  if(length(this_col) == 0){
    'cool'
  }else{
    pivot[, (this_col) := NULL] 
    pivot[is.na(pivot)]=0
  }

  
  #Turn all non-zero matrix elements for sampleperiod into 1
  pivot[, names(pivot)[-1] := lapply(.SD, function(x) as.integer(x!=0)), .SDcols = 2:ncol(pivot)]
  
  #Get everything ready to output the CH find length of history
  df.sp.simple[, SamplePeriod := as.numeric(SamplePeriod)]
  
  ######### Add any missing hours (or days, minutes etc.) of data ##########
  
  #First create the full sequence of hours from beginning to end of the 
  #study that should be accounted for
  occStr <- seq(1,max(df.sp.simple$SamplePeriod),1);
  occStr <- as.character(occStr); 
  occStr <- c("group",occStr); # pre-pend tag onto occStr
  
  #Then find the names of missing time columns
  missing_time <- setdiff(occStr,names(pivot)) 
  pivot <- as.data.frame(pivot)
  pivot[missing_time] <- 0
  pivot <- pivot[occStr]
 
   # make an NAs a 0
  setDT(pivot)
  pivot[, names(pivot)[-1] := lapply(.SD, function(x) ifelse(is.na(x),0,x)), .SDcols = 2:ncol(pivot)]
  
  
  ############### Add "NA"s where arus were not running ###############
  
  #First create a list of the maximum Sample Period for each camera in the
  #entire dataset, this will be larger than the last time we calcualted
  #this since that was only for a specific species
  z2 <- df
  # z2 <- as.data.frame(df)
  # z2$group <- as.character(z2$group)
  # 
  # z2$SamplePeriod <- NA
  # 
  # for (i in unique(z2$group)){
  #   #print(i)
  #   x <- z2 %>%
  #     dplyr::filter(group == i)
  #   
  #   z2[z2$group == i,]$SamplePeriod <- cut(x$detection_time_date, breaks = as.POSIXct(seq(from = min(x$start_deployment, na.rm = T), by = day.sec, to = max(x$end_deployment, na.rm = T) + day.sec)), labels = F)
  #   
  # }
  # 

    z2[, samplePeriod := cut(detection_time_date, breaks = as.POSIXct(seq(from = min(start_deployment, na.rm = T), by = day.sec, to = max(end_deployment, na.rm = T) + day.sec)), labels = F), by = group]

  df5 <- z2[, .(group, samplePeriod)]
  max_period <- setnames(df5[, max(samplePeriod), by = group], c('group', 'samplePeriod'))
  
  #Then for each title, insert "NA" for each Sample Period greater than
  #the maximum
  pivot2<-pivot
  for(j in 2:ncol(pivot2)){
    for(i in 1:nrow(pivot2)){
      if (j > max_period[i,2]){
        pivot2[i,j]<-"NA"}
    }}
  
  return(pivot2)
}
