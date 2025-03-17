
#######################| 1. MERGE ALL METADATA |################################ 


# Call them one by 
for (i in 1: length(list.name)){
  print(list.name[i])
  
  wb <- readRDS(paste0("wb_", list.name[[i]], ".RDS", sep = ""))
  df. <- wb
  to.append.1 <- paste0("s_", list.name[i], ".RDS", sep = "")
  to.append.2 <- paste0("odi_", list.name[i], ".RDS", sep = "")
  to.append.3 <- paste0("ictd_", list.name[i], ".RDS", sep = "")
  to.append.4 <- paste0("col_", list.name[i], ".RDS", sep = "")
  to.append.5 <- paste0("ifs_", list.name[i], ".RDS", sep = "")
  to.append.6 <- paste0("satied_", list.name[i], ".RDS", sep = "")
  
  # Append both dataframe, Code can still run even if no file from is available
  
  library(dplyr)
  
  if(file.exists(to.append.1)){
    s <- readRDS(paste0("s_", list.name[i], ".RDS", sep = ""))
    df. <- bind_rows(df., s)
    print("s merged")
  }
  
  if(file.exists(to.append.2)){
    odi <- readRDS(paste0("odi_", list.name[i], ".RDS", sep = ""))
    df. <- bind_rows( df., odi)
    print("odi merged")
  }
  
  if(file.exists(to.append.3)){
    ictd <- readRDS(paste0("ictd_", list.name[i], ".RDS", sep = ""))
    df. <- bind_rows(df., ictd)
    print("ictd merged")
  }
  
  if(file.exists(to.append.4)){
    els <- readRDS(paste0("col_", list.name[i], ".RDS", sep = ""))
    df. <- bind_rows(df., els)
    print("col merged")
  }
  
  if(file.exists(to.append.5)){
    els <- readRDS(paste0("ifs_", list.name[i], ".RDS", sep = ""))
    df. <- bind_rows(df., els)
    print("ifs merged")
  }
  
  if(file.exists(to.append.6)){
    els <- readRDS(paste0("satied_", list.name[i], ".RDS", sep = "")) #%>% select(-X)
    #saveRDS(els, paste0("satied_", list.name[1], ".RDS", sep = "")) 
    df. <- bind_rows(df., els)
    print("SATIED merged")
  }
  
  # Save them 
  saveRDS(df., file = paste0(list.name[[i]], ".RDS", sep = "" ))
  print(list.name[[i]])
  assign(paste(list.name[i], sep = ""), df.)  ## Store the graphs under their original name
}

print("Metadata merged")



