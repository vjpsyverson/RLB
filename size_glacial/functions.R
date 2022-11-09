require(RPostgreSQL)
require(rmarkdown)
require(openxlsx)
require(paleoTS)
require(Bchron)
require(rcarbon)
library(geoChronR)
library(lipdR)
library(oxcAAR)
library(dplyr)
library(ggplot2)

#functions
strip<-function(x){gsub("[^[:alnum:]]", "",x)}

insertBatch <- function(con,tn,df,size=100L) {
  if (nrow(df)==0L) return(invisible());
  cnt <- (nrow(df)-1L)%/%size+1L;
  for (i in seq(0L,len=cnt)) {
    sql <- paste0("insert into \"",tn,"\" values (",do.call(paste,c(sep=',',collapse='),(',lapply(df[seq(i*size+1L,min(nrow(df),(i+1L)*size)),],shQuote))),");");
    dbSendQuery(con,sql); 
  };
  invisible();
};

calibrate.pit.ages<-function(data) {
  ages.cal<-calibrate(
    x=as.numeric(data$uncal14cage),
    errors=as.numeric(data$uncal14cerror),
    calCurves = "intcal20")
  return(ages.cal)
};
