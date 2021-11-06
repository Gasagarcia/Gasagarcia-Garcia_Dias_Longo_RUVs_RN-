#### Load data ####
dt_prt <- as.data.frame(readxl::read_excel("Data/RN_Completo.xlsx",   sheet = "RN_Completo"))

#### Handling data #####
#Transform time in the partial approach into secconds
#The goal here is to create data checkpoints from which we can quickly load data

# 3 planned checkpoints:
#nw - raw data in flat shape conteining new records
#records - video and minute data in a data.frame shape
#Accs - a list of video datas containing comulative records 
#of each video plot in a given time (in minutes)

#Prepare nw
to_sec <- function(x) {
  a <- stringr::str_remove_all(x, "[\"]")
  b <- stringr::str_remove(a, stringr::fixed("''"))
  y <- strsplit(b, "'")
  z<-sapply(seq_along(y), function(i) {
    if (length(y[[i]]) < 2) {
      if (stringr::str_detect(b[[i]], "'")) {
        return(as.numeric(stringr::str_remove_all(b[[i]], "'"))*60)
      } else {
        return(as.numeric(y[[i]]))
      }
    } else {
      return((as.numeric(y[[i]][1]) * 60) + as.numeric(y[[i]][2]))
    }
  })
  z
}

#Which records are larger than 10 min?
which(to_sec(dt_prt$`Tempo início`) > 600)

prt <- data.frame(video = paste(dt_prt$`CÓDIGO FILME`), code = dt_prt$Spp, 
                  time = to_sec(dt_prt$`Tempo início`),
                  n = rep(1, nrow(dt_prt)))

prt$code <- as.character(prt$code)

#Name corrections
{
  prt$code[which(prt$code == "OPH_MAC")] <- "OPH_TRI"
  
  prt$code[which(prt$code == "CRY_ROS")] <- "SPA_RAD"
  
  prt$code[which(prt$code == "HAL_BIV")] <- "HAL_SAZ"
  
  prt$code[which(prt$code %in% c("LUT_APO", "LUT_ANA", "LUT_SP", "LUT_SYN"))] <- "LUT_ALE"
  
  prt$code[which(prt$code == "ACA_SP")] <- "ACA_POL"
  
  prt$code[which(prt$code == "HAL_RAD")] <- "HAL_BRA"
  
  prt$code[which(prt$code == "SCO_REG")] <- "SCO_BRA"
  
  prt$code[which(prt$code == "OCY_CRY")] <- "OCY_CHR"
  
  prt <- prt[!prt$code %in% c("TARTARUGA", "NI"),]
  
  #Correct Barreirinha name
  prt$video <- gsub('Barrerinha', 'Barreirinha', prt$video)
}


#Standardize time by first record
prt <- plyr::rbind.fill(by(prt, prt$video, function(x) {
  x$time <- (x$time - min(x$time))
  x <- x[x$time < 600,] #Cut the times greater than 10 min off
  x
}))

#Add minute
prt$min <- (prt$time %/% 60) + 1



str(prt)

#Remove duplicated entries
#First split by video, then remove duplications and merge everything again
nw <- split(prt, prt$video)
nw <- lapply(nw, function(x) x[!duplicated(x$code),])
nw <- plyr::rbind.fill(nw)


#Filter for videos filmed in 2013
nw <- nw[grepl('0313', nw$video),]

saveRDS(nw, "R_Objects/nw.rds")

rm(list = ls())

#Create records
#A.K.A. data for cumulative richness approaches
nw <- readRDS("R_Objects/nw.rds")

#Procedures: Create a binary matrix and sum rows to get richess
#Procedures: Split by video, reshape into matrix (sec per code by n), replace 0 with 1 after first detection

records <- reshape2::dcast(nw, video + min ~ code, value.var = "n", fun.aggregate = sum)

#Split by video to name rows after minutes
records <- Map(`rownames<-`, split(records, records$video), split(records$min, records$video))

#Fill in the gaps with 0s to consider times with no new records
miss_min <- function(mat) {
  #Create a 0 matrix named after each minute
  y <- matrix(0, nrow = 12, ncol = ncol(mat))
  rownames(y) <- 1:12
  #Obs: the 12 rows is a trick to avoid data being converted to a vector.
  #The extra two rows will be removed
  
  #If the matrix is incomplete, add missing rows
  if (!all(rownames(y) %in% rownames(mat))) {
    y <- as.data.frame(y[!(rownames(y) %in% rownames(mat)),])
    colnames(y) <- colnames(mat)
    #Bind rows and add rownames
    x <- `rownames<-`(rbind(mat, y), c(rownames(mat), rownames(y)))
    x$video <- rep(x$video[1], nrow(x))
    x$min <- rownames(x)
    x <- x[order(as.numeric(rownames(x))),]
    x <- x[1:10, ]
    return(x)
  } else return(mat)
}

records <- plyr::rbind.fill(lapply(records, miss_min))

rownames(records) <- paste(records$video, records$min, sep = "_")

str(records)

saveRDS(records, 'R_Objects/records.rds')

rm(list = ls())
#Create Accs
#A.K.A. a list containing cumulative video detections on specific times 
#(in minutes)
records <- readRDS('R_Objects/records.rds')

# This function splits a matrix according to a increasing number This will be 
#used to compare accumulation curves as observed time grows

progressive_split <- function(rec, minutes) {
  # Check inputs
  stopifnot(is.character(minutes) || is.numeric(minutes), is.matrix(rec) || is.data.frame(rec))
  # Assure fctr is a factor
  minutes <- as.numeric(minutes)
  # Ensure numeric order to be kept by adding 0s to the left
  minutes <- vapply(minutes, function(x, max_digits) {
    if(nchar(x) < max_digits) {
      return(paste0(rep(0, max_digits - nchar(x)), x))
    } else return(as.character(x))
  }, max_digits = max(nchar(as.character(minutes))), character(1))
  # Transform into a factor to keep order
  mins <- as.numeric(as.factor(minutes))
  m <- unique(as.numeric(as.factor(minutes)))
  # For each factor level, keep the ones equal or or smaller
  mat <- lapply(m, function(x) {
    rec[mins <= x, ]
  })
  names(mat) <- levels(minutes)
  return(mat)
}

# Progressively split times
Accs <- progressive_split(records, records$min)
#sum records from different minutes
Accs <-lapply(Accs, function(x) {
  y <- aggregate(x[,-c(1,2)], list('Videos' = x$video), FUN = sum)
  rownames(y) <- y$Videos
  return(y[,-1])
})

# Test if video records increases with observed periods
plot(vapply(Accs, sum, numeric(1))) #TRUE

saveRDS(Accs, 'R_Objects/Accs.rds')
