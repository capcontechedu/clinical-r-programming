##################################################
#   Author : Capcontech
#   Purpose : Create a simulation study data 
#   output : srcdata/...
# created Date : 05Apr2026
##################################################
# loading package
pkgs <- c("safetyData", "stringr", "dplyr","crayon")

for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}
# creating data directory <study directory equivalent/>
if (!any(dir.exists("srcdata"))) {
  dir.create("srcdata")
}

setwd("./srcdata")

# creating study data
std <- c("STUDY1","STUDY2","STUDY3","STUDY4")
for (jj in 1:length(std)) {

  ii <- std[jj]
  
  if (!any(dir.exists(ii))) {
    dir.create(ii)
  }
  
  m <- data(package = "safetyData")$results
  dfls <- m[,"Item"]
  
  adf <- dfls[!str_detect(dfls,"sdtm")]
  
  for (ds in adf) {
    x <- data(list = ds, package = "safetyData")
    y <- get(x)
    y <- y %>% 
      mutate(
        STUDYID = str_c(STUDYID,ii,sep = "-")
      )
    write_rds(y,file = paste0(getwd(),"/",ii,"/",ds,".rds"))
  }
  
  apath <- paste0(getwd(),"/srcdata/",ii)
  assign(paste0("apath",jj),apath)

  message(paste("read file path created as", paste0("apath",jj)))
  message(paste("in :",apath))
  cat(bgWhite(red("For example :\n To read adsl data into xx you can use below code:\n xx <- read_rds(file = file.path(apath1,'adam_adsl.rds'))")))

}

setwd("..")
remove(list = c("m",adf,"adf","dfls","ds","x","y","ii","apath","jj","std","pkgs","p"))
detach("package:crayon", unload = TRUE)
