library(data.table);library(magrittr);library(parallel);library(fst)

#setwd("~/ShinyApps/doctorssi/ranitidine")

names.study <- list.files("res")

#list.balance <- lapply(names.study, function(x){
#  fread(file.path("res",x, "covariate_balance.csv"), integer64 = "numeric")
#}) %>% rbindlist %>% write_fst("covariate_balance_all.fst")


data.balance <- read_fst("covariate_balance_all.fst", as.data.table = T)

#list.covariate <- lapply(names.study, function(x){
#  fread(file.path("res",x, "covariate.csv"), integer64 = "numeric")
#})

data.covariate <- read_fst("covariate_all.fst", as.data.table = T)


list.result <- lapply(names.study, function(x){
  fread(file.path("res",x, "cohort_method_result.csv"), integer64 = "numeric")
})


names(list.result) <- names.study


## ID info
list.idinfo <- lapply(c("exposure_of_interest.csv", "outcome_of_interest.csv", "cohort_method_analysis.csv"), function(x){
  info.id <- fread(file.path("res", names.study[1], x))[, 1:2]
  vec.id <- info.id[[1]]
  names(vec.id) <- info.id[[2]]
  return(vec.id)
})

names(list.idinfo) <- c("exposure", "outcome", "analysis")

analysis.originalN <- c(1, 1, 1, 1, 15, 15, 15, 15, 37, 39, 37, 39, 37, 39, 37, 39, rep(39, 5))
names(analysis.originalN) <- sort(lapply(list.result, function(x){unique(x$analysis_id)}) %>% Reduce(intersect, .))

