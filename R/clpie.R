#' clpie
#'
#' To visualize where individuals are spending time in the observations, clpie generates a pie graph of proportion time in each zone, and can generate pie graphs per category.
#' @param dataname summary dataframe of cowlog observations (generated via cldata)
#' @param zonename list of zones (generated via cldata)
#' @param factor must be listed as either TRUE or FALSE, if true pie graphs are seperately generated for each category
#' @param factorname (used if factor = TRUE) name of relevant categorical column in summary dataframe
#' @keywords cowlog
#' @keywords pie
#' @export
#' @examples
#' clpie(dataname = dataframe_round, zonename = list_of_zones, factor = TRUE, factorname = "round")

clpie <- function(dataname, zonename, factor, factorname){
  zones<-zonename
  data<-dataname
  pct<-vector(length= length(zones))

  if(factor == FALSE){
    i<-1
    for (i in 1:length(zones)){
      temp<-as.list(data[as.character(paste(zones[i], "prop", sep = "_"))])
      pct[i]<-mean(temp[[1]], na.rm = TRUE)}
    pct<-pct*100
    pct<-round(pct)
    i<-1
    labels<-vector(length=length(zones))
    for (i in 1:length(zones)){
      labels[i]<-(paste(zones[i], " ", pct[i], "%", sep = ""))}
    colors<-viridis(length(zones))
    pie(pct, labels = labels, col = colors, border = "gray80", main="all data")
  }


  if(factor == TRUE){
    numfactor<-nrow(as.vector(unique(data[factorname])))
    factorlist<-as.vector(unique(data[factorname]))
    j<-1
    for (j in 1:numfactor){
      keeprows<-which(data[factorname] == as.character(factorlist[j,]))
      tempdata<-data[keeprows,]
      i<-1
      for (i in 1:length(zones)){
        temp<-as.list(tempdata[as.character(paste(zones[i], "prop", sep = "_"))])
        pct[i]<-0
        pct[i]<-mean(temp[[1]], na.rm = TRUE)}
      pct<-pct*100
      pct<-round(pct)
      i<-1
      labels<-vector(length=length(zones))
      for (i in 1:length(zones)){
        labels[i]<-(paste(zones[i], " ", pct[i], "%", sep = ""))}
      colors<-viridis(length(zones))
      pie(pct, labels = labels, col = colors, border = "gray80", main=factorlist[j,])
    }}}
