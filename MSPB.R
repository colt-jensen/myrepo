setwd("~/Downloads/r-git/myrepo")

#Ye old PA token 
library(credentials)

set_github_pat()
#got to have these
library(tidyverse)
library(ggplot2)
library(skimr)
library(modelsummary)
library(foreign)
library(MASS)
#install.packages("sandwich", repos="http://R-Forge.R-project.org")
library(sandwich)
#2016 Merit Service Protection Board - Path 1 survey (wp violence)
d1 <- MSPB_p1

#2016 Merit Service Protection Board - Path 2 survey (ethics reporting)
d2 <- 'path2/MSPB_MPS2016_Path2_Rel.CSV' %>%
  read_csv

view(d1)
view(d2)

#cleaning the data example
d2$ETHC_16a[d2$ETHC_16a>=998] <- NA

d2$newvar <- d2$ETHC_16a - 1

#ETHC_18g - I feel comfortable reporting ethics violations.
d2$ETHC_18g[d2$ETHC_18g==998] <- NA

d2$report <- d2$ETHC_18g - 1

#DEM_12 - Sex(M/F)
d2$DEM_12[d2$DEM_12>=995] <- NA 

d2$female <- d2$DEM_12 - 1 

#DEM_11R - Ethnicity/RNO identification (recoded for release)
d2$DEM_11R[d2$DEM_11R>=995] <- NA 

d2$minority <- d2$DEM_11R 
summary(d2$DEM_11R)

"[NDA_03b] Does your job involve access to information that is specifically required by 
law or executive order to be kept secret in the interest of national defense 
or the conduct of foreign affairs?If so, do you know how to make a lawful, 
protected disclosure of wrongdoing involving information normally required 
to be kept secret (e.g., national security secrets)?"

d2$NDA_03b[d2$NDA_03b>=850] <- NA 

summary(d2$NDA_03b)

#[DEM_10R] Salary level (recoded for release) 
d2$DEM_10R[d2$DEM_10R>=995] <- NA 

d2$salary <- d2$DEM_10R - 1 


#summary stats ------------
skim(d2$ETHC_18g)
summary(d2$ETHC_18g)

skim(d2$ETHC_16a)
summary(d2$ETHC_18g)

skim(d2$newvar)
summary(d2$newvar)

#tab command from stata!!!
xtabs(~female + minority, data = d2)

#all the robust std. errors, but R default > STATA default esp. N < 250
est = lm(report ~ female + salary*minority , d2)

modelsummary::msummary(est, vcov = list('iid', 'HC0', 'HC1', 'HC2', 'HC3', 'HC4'))

summary(est)

myprobit <- glm(NDA_03b ~ female + salary*minority, family = binomial(link = "probit"), 
                data = d2)
myprobit
modelsummary::msummary(myprobit, vcov = list('iid', 'HC0', 'HC1', 'HC2', 'HC3', 'HC4'))

summary(myprobit)

## fit ordered logit model and store results 'm'
m <- polr(report1 ~ female + salary*minority, data = d2, Hess=TRUE)
d2$report1 <- d2$report 
d2$report1<-as.factor(d2$report1)
is.factor(d2$report1)
summary(m)

## check


