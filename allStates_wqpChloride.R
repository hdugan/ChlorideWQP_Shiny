####### STATE MAP #######
library(maps)
library(mapStats)
library(raster)
library(viridis)
library(lubridate)
library(tidyverse)
library(sf)

usMap <- st_read(system.file("shapes/usMap.shp", package="mapStats")[1],layer='usMap',stringsAsFactors = F)

states = c('MN','WI','IA','MO','MI','OH','IN','IL','PA','NY','ME','MA','MD','VT','CT','DE','NJ','RI','NH')

stMap = usMap %>% filter(STATE_ABBR %in% states) %>% 
  st_transform(crs = 4326)
stMap.ll = stMap[,1]

# type = 'Stream'

# states = c('OH','IN','IL','PA','NY','ME','MA','MD','VT','CT','DE','NJ','RI','NH')

for (state in states){
  ####################################################
  sites = read_csv(paste0('WQPchloride/',state,'station.csv'))
  a = sites %>% dplyr::select(OrganizationIdentifier:MonitoringLocationTypeName,LatitudeMeasure,LongitudeMeasure,
                              StateCode)
  
  ############## WQP DATA ########################
  readwqp = read_csv(paste0('WQPchloride/',state,'result.csv'))
  wqp = readwqp %>% 
    rename_at(vars(contains('MeasureUnitCode')), funs(sub('/', '.', .))) %>% 
    mutate(Chloride = as.numeric(ResultMeasureValue), Units = ResultMeasure.MeasureUnitCode) #%>% 
  # dplyr::select(OrganizationIdentifier:ctivityDepthHeightMeasure/MeasureUnitCode,Chloride,Units)
  
  range(as.numeric(wqp$ResultMeasureValue),na.rm = T)
  
  # Check units
  replaceUnits <- function(unit,scalingfactor){
    indx = which(wqp$Units== unit)
    wqp$Chloride[indx] = wqp$Chloride[indx] * scalingfactor
    wqp$Units[indx] = 'mg/l'
    return(wqp)
  }
  wqp = replaceUnits('ug/l',scalingfactor = 1/1000)
  wqp = replaceUnits('ug/l      ',scalingfactor = 1/1000)
  wqp = replaceUnits('umol      ',scalingfactor = 35.45/1000)
  wqp = replaceUnits('umol/L',scalingfactor = 35.45/1000)
  wqp = replaceUnits('ueq/L',scalingfactor = 35.45/1000)
  wqp = replaceUnits('ueq/L     ',scalingfactor = 35.45/1000)
  wqp = replaceUnits('mg/L',scalingfactor = 1)
  wqp = replaceUnits('mg/l      ',scalingfactor = 1)
  wqp = replaceUnits('mg/l      ',scalingfactor = 1)
  wqp = replaceUnits('mg/kg',scalingfactor = 1)
  wqp = replaceUnits('mg/kg     ',scalingfactor = 1)
  wqp = replaceUnits('ppm',scalingfactor = 1)
  wqp = replaceUnits('ppm       ',scalingfactor = 1)
  wqp = replaceUnits('mg/g',scalingfactor = 1000)
  wqp = replaceUnits('mg/g      ',scalingfactor = 1000)
  
  # Delete rows with no units 
  c = wqp %>% dplyr::filter(Units == 'mg/l') %>%
    dplyr::filter(!is.na(Chloride)) %>%
    left_join(a,by = 'MonitoringLocationIdentifier') %>%
    mutate(MonitoringLocationTypeName = ifelse(MonitoringLocationTypeName =='River/Stream', 'Stream',MonitoringLocationTypeName)) %>%
    mutate(MonitoringLocationTypeName = ifelse(MonitoringLocationTypeName =='River/Stream Perennial', 'Stream',MonitoringLocationTypeName)) %>%
    mutate(MonitoringLocationTypeName = ifelse(MonitoringLocationTypeName =='River/Stream Intermittent', 'Stream',MonitoringLocationTypeName)) %>%
    # mutate(MonitoringLocationTypeName = ifelse(MonitoringLocationTypeName =='Well: Collector or Ranney type well', 'Well',MonitoringLocationTypeName)) %>%
    mutate(MonitoringLocationTypeName = ifelse(MonitoringLocationTypeName =='Lake, Reservoir, Impoundment', 'Lake',MonitoringLocationTypeName)) %>%
    dplyr::filter(ResultStatusIdentifier == 'Accepted' | ResultStatusIdentifier =='Final') 
  
  spatialc = c %>% st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), 
                            crs = 4326) 
  d = st_crop(spatialc, st_bbox(stMap.ll))
  
  table(d$MonitoringLocationTypeName)
  
  f = c %>% filter(MonitoringLocationTypeName %in% c('Stream','Great Lake','Lake')) %>%
    filter(MonitoringLocationIdentifier %in% d$MonitoringLocationIdentifier) %>% 
    dplyr::filter(year(ActivityStartDate) >= 1980) %>%
    mutate(year = year(ActivityStartDate)) %>% 
    group_by(MonitoringLocationIdentifier,MonitoringLocationName,MonitoringLocationTypeName) %>%
    add_tally() %>% 
    summarise(n = first(n), MaxCl = max(Chloride, na.rm = T), MinCl = min(Chloride, na.rm = T),
              MeanCl = mean(Chloride, na.rm = T), minYear = min(year, na.rm = T), maxYear = max(year, na.rm = T),
              Lat = first(LatitudeMeasure), Long = first(LongitudeMeasure)) %>% 
    mutate(state = state) 
  
  write_csv(f,paste0('allStates/WQPchloride_',state,'.csv'))
}

########## Combine all states #################

# Get a List of all files in directory named with a key word, say all `.csv` files
filenames <- list.files("../allStates/", pattern="*.csv", full.names=TRUE)

# Read all csv files in the folder and create a list of dataframes
ldf <- lapply(filenames , read_csv)

# Combine each dataframe in the list into a single dataframe
df.final <- do.call("rbind", ldf)

# Write csv of all state
write_csv(df.final,'ChlorideObservations/Data/allstatesWQP.csv')


