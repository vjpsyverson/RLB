require(RPostgreSQL);require(openxlsx)
PostgreSQL(max.con = 16,fetch.default.rec = 500,force.reload = FALSE)
driver<-dbDriver("PostgreSQL")
con<-dbConnect(driver,user="vsyverson",password="gymnogyps",dbname="rlb")
test<-fetch(dbSendQuery(con,"select * from helloworld;"))
test;rm(test)
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
#script
#get C14 samples data from xlsx
fileName<-"all_C14_ages.xlsx"
sheetNames<-getSheetNames("all_C14_ages.xlsx")
for (i in 1:length(sheetnames)){
  gotsheet<-as.data.frame(read.xlsx(fileName,sheet=sheetNames[i]),stringsAsFactors=F,na.string=c("<NA>","N/A","n.a.","NA","-"))
  assign(strip(sheetNames[i]),gotsheet)
}
#rename and clean
names(Holden2016)<-c("SampNo","CatNo","Pit","Taxon","Element","Material","Uncal14CAge","Uncal14CError")
Holden2017<-cbind(Holden2017,NA)
names(Holden2017)<-c("Taxon","CatNo","SampNo","MassC","Pit","Uncal14CAge","Uncal14CError","d13C","Cal14CAge","Cal14C-2S","Cal14C+2S","ProbWithin2S","Element")
Holden2017$Pit<-gsub("Pit ","",Holden2017$Pit)
Fuller2015[,2]<-paste0(Fuller2015[,1],Fuller2015[,2])
Fuller2015<-Fuller2015[,-1]
names(Fuller2015)<-c("CatNo","Pit","Taxon","Element","SampNo","Uncal14CAge","Uncal14CError","Accuracy","Reference")
Fuller2020<-cbind(Fuller2020,23)
names(Fuller2020)<-c("SampNo","CatNo","Taxon","Development","Element","Position","CollagenYield","Uncal14CAge","Uncal14CError","Cal14CAge_2SRange","Cal14C-2S","Cal14C+2S","MedianProbability","Reference","Pit")
names.C14ages<-c("CatNo","Pit","Taxon","Element","SampNo","Uncal14CAge","Uncal14CError")
head(Holden2016[,names.C14ages])
#make single table
C14ages<-data.frame(rbind(
  Holden2016[,names.C14ages],
  Holden2017[,names.C14ages],
  Fuller2015[,names.C14ages],
  Fuller2020[,names.C14ages]),stringsAsFactors = F)
C14ages$Element[which(C14ages$Element=="-")]<-NA
C14ages$Uncal14CAge[grep("limit",C14ages$Uncal14CAge)]<-gsub("[^0-9]", "", C14ages$Uncal14CAge[grep("limit",C14ages$Uncal14CAge)])
C14ages$Uncal14CAge<-gsub(",", "", C14ages$Uncal14CAge)
C14ages$Uncal14CError[which(C14ages$Uncal14CError=="N/A")]<-NA
C14ages$Uncal14CError<-gsub(",", "", C14ages$Uncal14CError)
#send to db
dbSendQuery(con,"drop table C14Samples;")
dbSendQuery(con,"create table C14Samples (
    CatNo varchar(50),
    Pit varchar(10),
    Taxon varchar(100),
    Element varchar(100),
    SampNo varchar(50),
    Uncal14CAge varchar(50),
    Uncal14CError varchar(50)
            );")
insertBatch(con,"c14samples",C14ages)
test<-fetch(dbGetQuery(con,"select * from c14samples limit 10;"))
test;rm(test)
###remember to calibrate ages before using them!
require(Bchron)
ages<-fetch(dbSendQuery(con,"select * from c14samples;"))
ages.clean<-ages[-grep("[^0-9]",ages$uncal14cage),]
ages.cal<-BchronCalibrate(ages=as.numeric(ages.clean$uncal14cage),ageSds=as.numeric(ages.clean$uncal14cerr))
sink(file="CI.txt")
summary(ages.cal)
sink()
file.show("CI.txt")
#get all bone measurements data from xlsx
taxonList<-read.csv("taxa.csv")
measurements<-vector(mode="list",nrow(taxonList))
allColNames.ij<-vector()
for (i in 1:nrow(taxonList)){
  path<-paste(taxonList$Folder[i],taxonList$Genus[i],taxonList$Filename[i],sep="/")
  sheetNames<-getSheetNames(path)
  names(measurements)[i]<-paste(taxonList$Genus[i],taxonList$Species[i],sep="_")
  if(length(sheetNames)==1){
    measurements[[i]]<-data.frame(read.xlsx(path,sheet=sheetNames))
    names(measurements)[i]<-paste(names(measurements)[i],sheetNames,sep="_")
    allColNames.ij<-append(allColNames.ij,colnames(measurements[[i]]))
  } else {
    measurements[[i]]<-vector(mode="list",length(sheetNames))
    for (j in 1:length(sheetNames)){
      measurements[[i]][[j]]<-data.frame(read.xlsx(path,sheet=sheetNames[j]))
      allColNames.ij<-append(allColNames.ij,colnames(measurements[[i]][[j]]))
    }
    names(measurements[[i]])<-sheetNames
  }
}

#rename fucking everything
allColNames<-unique(unlist(allColNames.ij))
usableColNames<-c("Collection","CatNo","Pit","Taxon","Element","Length","Width","Depth","ProxWidth","ProxDepth","MidWidth","MidDepth","DistWidth","DistDepth",)
cleaned<-measurements
#P. atrox & Smilodon astragali (4.1, 5.3): measurement names?
i<-16;j<-8
c(names(cleaned[i]),names(cleaned[[i]][j]))
#fix error in list structure
#temp<-cleaned[[i]];elemName<-unlist(strsplit(names(cleaned[i]),"_"))[length(unlist(strsplit(names(cleaned[i]),"_")))]
#cleaned[[i]]<-list(temp);names(cleaned[[i]])<-elemName;names(cleaned)[i]<-paste(unlist(strsplit(names(cleaned[i]),"_"))[-length(unlist(strsplit(names(cleaned[i]),"_")))],collapse="_")
#rename columns
head(cleaned[[i]][[j]])
for (i in 1:length(cleaned)){
  for (j in 1:length(cleaned[[i]])){
    names(cleaned[[i]][[j]])<-unlist(lapply(names(cleaned[[i]][[j]]),function(x) {gsub("[.]","_",x)}))
  }
}
#cleaned[[i]][[j]]<-cleaned[[i]][[j]][,-c(2)]
#cleaned[[i]][[j]]$Pit<-"NA"
#cleaned[[i]][[j]]$Pit<-gsub("Modern","NA",cleaned[[i]][[j]]$Pit)
#colnames(cleaned[[i]][[j]])<-c("Pit","Length","Width","Depth","L4","L5","L6")
#colnames(cleaned[[i]][[j]])[11:14]<-c("OccLengthL","OccLengthR","OccWidthL","OccWidthR")
colnames(cleaned[[i]][[j]])[1]<-"CatNo"
head(cleaned[[1]][[1]])

#construct measurements database
#first the species lookup table
dbSendQuery(con,"drop table species")
dbSendQuery(con,"create table species (
            Name varchar(250),
            Elements text ARRAY)")
for (i in 1:length(cleaned)){
  speciesname<-names(cleaned)[i]
  elementnames<-lapply(lapply(names(cleaned[[i]]), function(x){gsub("$.*","'",x)}), 
    function(x){gsub(".*^","'",x)})
  elementnames.sql<-paste0("ARRAY[",paste0(elementnames,collapse=","),"]",collapse="")
  dbSendQuery(con,paste0("insert into species(Name, Elements) values ('",speciesname,"',",elementnames.sql,");",collapse = ""))
}
#how to select with value in array
test<-dbGetQuery(con,"select * from species where 'm1'=any(Elements)")
test

#now the individual measurements
allcolnames<-vector();for (i in 1:length(cleaned)) {allcolnames<-append(allcolnames,unlist(lapply(cleaned[[i]],colnames)))}
typeLookup<-data.frame(Name=unique(allcolnames),Type=NA)
typeLookup[c(1,2,3,12),2]<-"varchar(100)"
typeLookup[-c(1,2,3,12),2]<-"numeric(100)"
for (i in 15:length(cleaned)){
  for (j in 1:length(cleaned[[i]])){
    tablename<-paste0(names(cleaned[i]),"_",names(cleaned[[i]][j]))
    tablecols<-data.frame(colnames(cleaned[[i]][[j]]),colnames(cleaned[[i]][[j]]))
    for (k in 1:nrow(tablecols)){
      tablecols[k,2]<-typeLookup[which(typeLookup[,1]==tablecols[k,1]),2]
    }
    createquery<-paste(apply(tablecols,1,paste0,collapse=" "),collapse=", ")
    dbSendQuery(con,paste("drop table",tablename,collapse=" ;"))
    dbSendQuery(con,paste0("create table ",tablename," (",createquery,");",collapse =""))
    dbSendQuery(con,paste0("copy ",tablename," from stdin;"))
    postgresqlCopyInDataframe(con,cleaned[[i]][[j]])
  }
}

