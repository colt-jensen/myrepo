setwd("~/Downloads/r-git/myrepo")

library(gitcreds)

gitcreds::gitcreds_set()

library(credentials)

set_github_pat()

#Merit Service Protection Board - Path 1 survey
d <- MSPB_p1

head(d)
