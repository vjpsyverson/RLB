require(ggridges);require(ggplot2);require(vistime);require(dplyr)
require(paleoTS);require(openxlsx);require(googledrive);require(rlist)

require(oxcAAR)
quickSetupOxcal(path="/home/vsyverson/R/x86_64-pc-linux-gnu-library/4.0/oxcAAR/")

drive_download(as_id("https://docs.google.com/spreadsheets/d/1YY_pgEDK4SVs1pztad0hhjTHD0iAaChdt6hTUYm9uEE/edit?usp=sharing"),path="driveDLs/rlb_2023_data.xlsx",overwrite=T)
drive_download(as_id("https://docs.google.com/spreadsheets/d/1OX5X8gyqEpXX6km3VOGMIruMgB24YgbujHa4JHzTtYY/edit?usp=sharing"),path="driveDLs/c14.xlsx",overwrite=T)

source("functions.R")

require(RPostgreSQL)
PostgreSQL(max.con = 16,fetch.default.rec = 500,force.reload = FALSE)
driver<-dbDriver("PostgreSQL")
if(!exists("con")) con<-dbConnect(driver,user="vsyverson",password="gymnogyps",dbname="rlb")
#test<-fetch(dbSendQuery(con,"select * from helloworld;"))
#test;rm(test)
