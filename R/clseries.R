#' clseries
#'
#' To visualize average movement in the arena over time, clseries splits each observation into ten even segments of the same length and visualizes relative time in zones per segment.
#' @param pathtofile path to folder hosting individual csv's generated by cowlog
#' @param zonename list of zones (generated via cldata)
#' @param seglength desired length (in seconds) of each segment
#' @param factor must be listed as either true or false, if false one location series graph is averaged acros all data, if true location series graphs are generated per factor category
#' @param factorindex (used if factor = TRUE) index in cowlog individual csv's filename that the factor of interest is located (separted by "_")
#' @param factorname (used if factor = TRUE) name of desired factor to apprear on the location series graphs
#' @keywords cowlog
#' @keywords location
#' @keywords time series
#' @export
#' @examples
#' clseries(pathtofile = "C:/Users/example_files", zonename = zones, seglength = 20, factor = TRUE, factorindex = 3, factorname = "round")

clseries <-function(pathtofile, zonename, seglength, factor, factorindex, 
          factorname) 
{
  file_list <- list.files(path = pathtofile, pattern = "*.csv")
  zones <- zonename
  segment_length <- seglength
  seriesdata <- as.data.frame(matrix(0, ncol = (length(zones) * 
                                                  10) + 1, nrow = length(file_list)))
  i <- 1
  j <- 1
  listnames <- "filename"
  for (j in 1:length(zones)) {
    for (i in 1:10) {
      listnames <- append(listnames, paste("segment", 
                                           i, "-", zones[j], sep = ""))
    }
  }
  colnames(seriesdata) <- listnames
  i <- 1
  for (i in 1:length(file_list)) {
    file <- read.csv(paste(pathtofile, file_list[i], sep = "/"))
    filename <- file_list[i]
    seriesdata$filename[i] <- filename
    file <- as.data.frame(file)
    stamp1 <- file$time[1]
    stamp2 <- file$time[1] + segment_length
    stamp3 <- file$time[1] + (segment_length * 2)
    stamp4 <- file$time[1] + (segment_length * 3)
    stamp5 <- file$time[1] + (segment_length * 4)
    stamp6 <- file$time[1] + (segment_length * 5)
    stamp7 <- file$time[1] + (segment_length * 6)
    stamp8 <- file$time[1] + (segment_length * 7)
    stamp9 <- file$time[1] + (segment_length * 8)
    stamp10 <- file$time[1] + (segment_length * 9)
    stamp11 <- file$time[1] + (segment_length * 10)
    segment1 <- file[2:max(which(file$time < stamp2)), ]
    segment2 <- file[(which(file$time > stamp2) - 1):max(which(file$time < 
                                                                 stamp3)), ]
    segment3 <- file[(which(file$time > stamp3) - 1):max(which(file$time < 
                                                                 stamp4)), ]
    segment4 <- file[(which(file$time > stamp4) - 1):max(which(file$time < 
                                                                 stamp5)), ]
    segment5 <- file[(which(file$time > stamp5) - 1):max(which(file$time < 
                                                                 stamp6)), ]
    segment6 <- file[(which(file$time > stamp6) - 1):max(which(file$time < 
                                                                 stamp7)), ]
    segment7 <- file[(which(file$time > stamp7) - 1):max(which(file$time < 
                                                                 stamp8)), ]
    segment8 <- file[(which(file$time > stamp8) - 1):max(which(file$time < 
                                                                 stamp9)), ]
    segment9 <- file[(which(file$time > stamp9) - 1):max(which(file$time < 
                                                                 stamp10)), ]
    segment10 <- file[(which(file$time > stamp10) - 1):max(which(file$time < 
                                                                   stamp11)), ]
    segment1$time[1] <- stamp1
    segment2$time[1] <- stamp2
    segment3$time[1] <- stamp3
    segment4$time[1] <- stamp4
    segment5$time[1] <- stamp5
    segment6$time[1] <- stamp6
    segment7$time[1] <- stamp7
    segment8$time[1] <- stamp8
    segment9$time[1] <- stamp9
    segment10$time[1] <- stamp10
    segment1 <- mutate(segment1, time.difference = -(time - 
                                                       lead(time)))
    segment1$time.difference[nrow(segment1)] <- (segment_length + 
                                                   segment1$time[1]) - segment1$time[nrow(segment1)]
    segment2 <- mutate(segment2, time.difference = -(time - 
                                                       lead(time)))
    segment2$time.difference[nrow(segment2)] <- (segment_length + 
                                                   segment2$time[1]) - segment2$time[nrow(segment2)]
    segment3 <- mutate(segment3, time.difference = -(time - 
                                                       lead(time)))
    segment3$time.difference[nrow(segment3)] <- (segment_length + 
                                                   segment3$time[1]) - segment3$time[nrow(segment3)]
    segment4 <- mutate(segment4, time.difference = -(time - 
                                                       lead(time)))
    segment4$time.difference[nrow(segment4)] <- (segment_length + 
                                                   segment4$time[1]) - segment4$time[nrow(segment4)]
    segment5 <- mutate(segment5, time.difference = -(time - 
                                                       lead(time)))
    segment5$time.difference[nrow(segment5)] <- (segment_length + 
                                                   segment5$time[1]) - segment5$time[nrow(segment5)]
    segment6 <- mutate(segment6, time.difference = -(time - 
                                                       lead(time)))
    segment6$time.difference[nrow(segment6)] <- (segment_length + 
                                                   segment6$time[1]) - segment6$time[nrow(segment6)]
    segment7 <- mutate(segment7, time.difference = -(time - 
                                                       lead(time)))
    segment7$time.difference[nrow(segment7)] <- (segment_length + 
                                                   segment7$time[1]) - segment7$time[nrow(segment7)]
    segment8 <- mutate(segment8, time.difference = -(time - 
                                                       lead(time)))
    segment8$time.difference[nrow(segment8)] <- (segment_length + 
                                                   segment8$time[1]) - segment8$time[nrow(segment8)]
    segment9 <- mutate(segment9, time.difference = -(time - 
                                                       lead(time)))
    segment9$time.difference[nrow(segment9)] <- (segment_length + 
                                                   segment9$time[1]) - segment9$time[nrow(segment9)]
    segment10 <- mutate(segment10, time.difference = -(time - 
                                                         lead(time)))
    segment10$time.difference[nrow(segment10)] <- (segment_length + 
                                                     segment10$time[1]) - segment10$time[nrow(segment10)]
    j <- 1
    k <- 1
    for (j in 1:length(zones)) {
      for (k in 1:10) {
        seriesdata[i, paste("segment", k, "-", zones[j], 
                            sep = "")] <- 0
        temp <- eval(parse(text = paste("segment", k, 
                                        sep = ""))) %>% filter(code == zones[j]) %>% 
          select(time.difference)
        if (nrow(temp) > 0) {
          seriesdata[i, paste("segment", k, "-", zones[j], 
                              sep = "")] <- temp %>% sum()/seglength
        }
      }
    }
  }
  
  
  
  fullframe <- as.data.frame(matrix(0, ncol = length(zones) + 
                                      1, nrow = 10))
  colnames(fullframe) <- c("segment", zones)
  fullframe$segment <- c(1:10)
  if (factor == FALSE) {
    i <- 1
    for (i in 1:10) {
      tempcollist <- paste("segment", i, "-", sep = "")
      tempseries <- seriesdata[names(seriesdata[grep(tempcollist, 
                                                     names(seriesdata))])]
      list<-as.vector(tempseries %>% summarise_if(is.numeric, mean))
      list<-as.numeric(list[1:length(list)])
      fullframe[i,c(2:length(fullframe))]<-list
      
      }
    
    segs <- rep(c("1", "2", "3", "4", "5", "6", "7", "8", 
                  "9", "t10"), length(zones))
    i <- 1
    zoneschart <- NA
    for (i in 1:length(zones)) {
      temp <- c(rep(zones[i], 10))
      zoneschart <- append(zoneschart, temp)
    }
    zoneschart <- zoneschart[is.na(zoneschart) == FALSE]
    percent <- unlist(fullframe[names(fullframe) != "segment"])
    data <- data.frame(segs, zoneschart, percent)
    colors <- viridis(length(zones))
    p <- ggplot(data, aes(fill = zoneschart, y = percent, 
                          x = segs)) + geom_bar(position = "stack", stat = "identity") + 
      ggtitle(paste("location over time, all data\n                    seconds per segment =", 
                    seglength)) + theme_classic() + labs(fill = "zones", 
                                                         x = "time segment", y = "proportion of time in area") + 
      scale_fill_manual(values = colors) + theme(axis.text.x = element_text(angle = 90, 
                                                                            vjust = 0.5, hjust = 1))
    plot(p)}


  if (factor == TRUE) {
    seriesdata[factorname] <- NA
    i <- 1
    for (i in 1:nrow(seriesdata)) {
      tempfactor <- file_list[i]
      tempfactor <- str_remove(tempfactor, ".csv")
      tempfactor <- strsplit(tempfactor, "_")[[1]][factorindex]
      seriesdata[i, factorname] <- tempfactor
    }
    k <- 1
    for (k in 1:length(unique(seriesdata[, factorname]))) {
      tempfactorname <- unique(seriesdata[, factorname])[k]
      seriesdatafactor <- seriesdata %>% filter(seriesdata[factorname] == 
                                                  tempfactorname)
      j <- 1
      for (j in 1:10) {
        tempcollist <- paste("segment", j, "-", sep = "")
        tempseries <- seriesdatafactor[names(seriesdatafactor[grep(tempcollist, 
                                                       names(seriesdata))])]
        list<-as.vector(tempseries %>% summarise_if(is.numeric, mean))
        list<-as.numeric(list[1:length(list)])
        fullframe[j,c(2:length(fullframe))]<-list
      }
      segs <- rep(c("1", "2", "3", "4", "5", "6", "7", 
                    "8", "9", "t10"), length(zones))
      i <- 1
      zoneschart <- NA
      for (i in 1:length(zones)) {
        temp <- c(rep(zones[i], 10))
        zoneschart <- append(zoneschart, temp)
      }
      zoneschart <- zoneschart[is.na(zoneschart) == FALSE]
      percent <- unlist(fullframe[names(fullframe) != 
                                    "segment"])
      data <- data.frame(segs, zoneschart, percent)
      colors <- viridis(length(zones))
      p <- ggplot(data, aes(fill = zoneschart, y = percent, 
                            x = segs)) + geom_bar(position = "stack", stat = "identity") + 
        ggtitle(paste("location over time,", tempfactorname, 
                      "\n                      seconds per segment =", 
                      seglength)) + theme_classic() + labs(fill = "zones", 
                                                           x = "time segment", y = "proportion of time in area") + 
        scale_fill_manual(values = colors) + theme(axis.text.x = element_text(angle = 90, 
                                                                              vjust = 0.5, hjust = 1))
      plot(p)
    }
  }
}
