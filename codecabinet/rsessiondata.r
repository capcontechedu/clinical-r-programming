# required library
libvec <- c("safetyData","stringr","haven")

for (i in libvec) {
  if (!requireNamespace(i, quietly = TRUE)) {
    install.packages(i)
  }
  
  library(i, character.only = TRUE)
}

pthvec <- c("./source","./source/lib_a","./source/lib_s")
# source directory creation
if (!dir.exists(pthvec[1])) {
  dir.create(pthvec[1])
} else {
  message("`source` directory within project has been located successfully")
}

# lib_a creation
if (!dir.exists(pthvec[2])) {
  dir.create(pthvec[2])
} else {
  message("`source/lib_a` directory within project has been located successfully")
}

# lib_s creation
if (!dir.exists(pthvec[3])) {
  dir.create(pthvec[3])
} else {
  message("`source/lib_s` directory within project has been located successfully")
}


w<- data(package = "safetyData")$results[,"Item"]

for (a in w) {
  
  b <- get(a)
  
  if (str_detect(a,"adam_")){
    write_xpt(b,paste0(pthvec[2],"/",str_remove(a,"adam_"),".xpt"),version = 5)
  }
  
  if (str_detect(a,"sdtm_")){
    write_xpt(b,paste0(pthvec[3],"/",str_remove(a,"sdtm_"),".xpt"),version = 5)
  }
}

remove(a,b,pthvec,w,libvec,i)
