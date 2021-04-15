library(data.table);library(magrittr);library(fst)

setwd("~/ShinyApps/doctorssi/ranitidine")

names.study <- list.files("res")

list.balance <- lapply(names.study, function(x){
  fread(file.path("res",x, "covariate_balance.csv"), integer64 = "numeric")
}) %>% rbindlist %>% write_fst("covariate_balance_all.fst")


#data.balance <- read_fst("covariate_balance_all.fst", as.data.table = T)

lapply(names.study, function(x){
  fread(file.path("res",x, "covariate.csv"), integer64 = "numeric")
}) %>% rbindlist %>% write_fst("covariate_all.fst")

#data.covariate <- read_fst("covariate_all.fst", as.data.table = T)