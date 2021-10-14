#load dataset
library(readr)
MSPB_p1 <- read_csv("path1 (1)/MSPB_MPS2016_Path1_Rel.CSV")
View(MSPB_p1)

d <- MSPB_p1

#female
table(d$DEM_12)

d$female <- ifelse(d$DEM_12 == 2, 1, ifelse(d$DEM_12 == 1, 0, NA)) #!= not equal to
table(d$female)

#minority 
d$minority <- ifelse(d$DEM_11R == 1, 1, ifelse(d$DEM_11R == 0, 0, NA))

table(d$minority)

#DV observed workplace violence - WVOBS_01 WVOBS_04 experienced WVOEXP_01 WVOEXP_04

#[LVJOB_07] Likelihood of leaving agency in next 12 months?

