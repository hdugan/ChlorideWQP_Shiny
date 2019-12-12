install.packages("dataRetrieval", 
                 repos=c("http://owi.usgs.gov/R",
                         getOption("repos")))

library(dataRetrieval)


parameterCdFile <- parameterCdFile
names(parameterCdFile)
phosCds <- parameterCdFile[grep("chloride",
                                parameterCdFile$parameter_nm,
                                ignore.case=TRUE),]
phosCds
16887

setwd('~/Dropbox/currentprojects/WQPchloride/WQPchloride/')

c('MN','WI','IA','MO','MI','OH','IN','IL','PA','NY','ME','MA','MD','VT','CT','DE','NJ','RI','NH')

state = 'IN'
states = c('PA','NY','ME','MA','MD','VT','CT','DE','NJ','RI')

for (state in states){
  clSites <- whatWQPsites(statecode=state, characteristicName="Chloride")
  write.csv(clSites,paste0(state,'station.csv'))
  
  clData <- readWQPdata(statecode=state, characteristicName="Chloride")
  write.csv(clData,paste0(state,'result.csv'))
  
}



