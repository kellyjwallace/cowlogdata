#' clboxplot
#'
#' To visualize and compare the distribution of values, clboxplot generates bosplots of values and additionally can compares distributions across categories via t-tests and anovas.
#' @param dataname summary dataframe of cowlog observations (generated via cldata)
#' @param factor must be listed as either TRUE or FALSE, if false boxplots of summary distributions are generated, if true boxplots are generated and compared by category (t-tests and anovas)
#' @param factorname (used if factor = TRUE) name of relevant categorical column in summary dataframe
#' @keywords cowlog
#' @keywords boxplot
#' @export
#' @examples
#' clboxplot(dataname = dataframe_round, factor = TRUE, factorname = "round")

clboxplot <- function (dataname, factor, factorname) 
{
  data <- dataname
  collist <- colnames(data)
  collist <- collist[collist != "file_name"]
  collist <- collist[collist != "vid_length"]
  if (factor == FALSE) {
    i <- 1
    for (i in 1:(length(collist))) {
      temp <- as.list(data[collist[i]])
      avg <- round(mean(temp[[1]], na.rm = TRUE), digits = 3)
      temp <- as.data.frame(temp)
      colnames(temp) <- "value"
      p <- ggplot(temp, aes(x = "", y = value)) + geom_boxplot() + 
        geom_dotplot(binaxis = "y", stackdir = "center", 
                     dotsize = 0.5) + theme_classic() + labs(title = "all data", 
                                                           x = "", y = paste(collist[i], "--mean:", avg)) + 
        theme(aspect.ratio = 3)
      plot(p)
    }
  }
  if (factor == TRUE) {
    collist <- collist[collist != factorname]
    i <- 1
    for (i in 1:(length(collist))) {
      temp <- data %>% dplyr::select(c(collist[[i]], factorname))
      temp <- as.data.frame(temp)
      colnames(temp) <- c("value", "factor")
      temp$value<-as.character(temp$value)
      temp<-temp[temp$value != "Inf",]
      temp$value<-as.numeric(temp$value)
      p_value_t <- NA
      p_value_anova <- NA
      omitted <- na.omit(temp)
      omitted$factor<-as.factor(omitted$factor)
      omitted$value<-as.numeric(omitted$value)
      if (length(unique(omitted$factor)) > 1 & length(unique(omitted$factor)) < 
          3) {
        factor_list <- unique(temp$factor)
        factor_list<-factor_list[!is.na(factor_list)]
        templist <- NA
        k <- 1
        for (k in 1:length(factor_list)) {
          lengthvalue <- omitted %>% filter(factor == 
                                              factor_list[k]) %>% nrow()
          templist <- append(templist, lengthvalue)
        }
        if (min(templist, na.rm = TRUE) > 1) {
          t_test <- t.test(temp$value ~ temp$factor)
          p_value_t <- t_test$p.value
        }
        print(paste(factorname, collist[[i]], "p value (t-test) = ", 
                    round(p_value_t, 4)))
        if(is.na(p_value_t)== FALSE){
          if (p_value_t < 0.05) {
            temp$value<-as.numeric(temp$value)
            temp$factor<-as.factor(temp$factor)
            p <- ggplot(temp, aes(x = factor, y = value)) + 
              geom_boxplot() + geom_dotplot(binaxis = "y", 
                                            stackdir = "center", dotsize = 0.5) + theme_classic() + 
              labs(title = (paste(collist[i], "by", factorname)), 
                   x = "", y = collist[i]) + theme(aspect.ratio = 3) + 
              theme(axis.text.x = element_text(angle = 90)) + 
              annotate(geom = "text", label = paste("p value (t-test) =", 
                                                    round(p_value_t, 4)), x = -Inf, y = Inf, 
                       hjust = 0, vjust = 1)
            plot(p)
          }}
      }
      if (length(unique(omitted$factor)) > 2) {
        factor_list <- unique(temp$factor)
        templist <- NA
        k <- 1
        for (k in 1:length(factor_list)) {
          lengthvalue <- omitted %>% filter(factor == 
                                              factor_list[k]) %>% nrow()
          templist <- append(templist, lengthvalue)
        }
        if (min(templist, na.rm = TRUE) > 1) {
          res.aov <- aov(value ~ factor, data = temp)
          p_value_anova <- summary(res.aov)[[1]][["Pr(>F)"]]
        }
        print(paste(factorname, collist[[i]], "p value (anova) = ", 
                    round(p_value_anova[1], 4)))
        if(is.na(p_value_anova)== FALSE){
        if (p_value_anova < 0.05) {
          p <- ggplot(temp, aes(x = factor, y = value)) + 
            geom_boxplot() + geom_dotplot(binaxis = "y", 
                                          stackdir = "center", dotsize = 0.5) + theme_classic() + 
            labs(title = (paste(collist[i], "by", factorname)), 
                 x = "", y = collist[i]) + theme(aspect.ratio = 3) + 
            theme(axis.text.x = element_text(angle = 90)) + 
            annotate(geom = "text", label = paste("p value (anova) =\n     ", 
                                                  round(p_value_anova[1], 4)), x = -Inf, 
                     y = Inf, hjust = 0, vjust = 1)
          plot(p)
        }}
      }
    }
  }
}
