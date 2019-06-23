require(paleoTS);require(xlsx)
taxon<-"Felis_concolor"
path<-"Cats/"
data<-read.xlsx(paste0(path,taxon,".xlsx"),sheetName="m1")[,c("Pit","Age","L","W")]
