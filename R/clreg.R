#' clreg
#'
#' To determine which metrics correlate with each other and/or show interaction effects of factors, clreg runs linear regression models and generates summary scatterplots.
#' @param dataname summary dataframe of cowlog observations (generated via cldata)
#' @param zonename list of zones (generated via cldata)
#' @param factor must be listed as either TRUE or FALSE, if false scatterplots and regresison models of all data are generated, if true scatterplots are colored by category and p values of interaction effects are reported
#' @param factorname (used if factor = TRUE) name of relevant categorical column in summary dataframe
#' @keywords cowlog
#' @keywords linear regression
#' @export
#' @examples
#' clreg(data = dataframe_round, zonename = list_of_zones, factor = TRUE, factorname = "round")

clreg <- function (dataname, zonename, factor, factorname) 
{
  zones <- zonename
  data <- dataname
  collist <- colnames(data)
  collist <- collist[collist != "file_name"]
  collist <- collist[collist != "vid_length"]
  if (factor == FALSE) {
    i <- 1
    j <- 1
    for (i in 1:length(collist)) {
      for (j in 1:length(collist)) {
        measure1 <- collist[i]
        measure2 <- collist[j]
        temp <- data %>% select(collist[i], collist[j])
        if (ncol(temp) > 1) {
          if (nrow(na.omit(temp)) > 0) {
            colnames(temp) <- c("measure1", "measure2")
            
            temp$measure1 <- as.character(temp$measure1)
            temp <- temp[temp$measure1 != "Inf", ]
            temp$measure1 <- as.numeric(temp$measure1)
            
            temp$measure2 <- as.character(temp$measure2)
            temp <- temp[temp$measure2 != "Inf", ]
            temp$measure2 <- as.numeric(temp$measure2)
            
            temp$measure1 <- as.numeric(temp$measure1)
            temp$measure2 <- as.numeric(temp$measure2)
            mod <- lm(temp$measure2 ~ temp$measure1)
            p_value <- summary(mod)$coefficients[8]
            print(paste(measure1, measure2, "p value = ", 
                        round(p_value, 4)))
            if (is.na(p_value) == TRUE) {
              p_value <- 100
            }
            if (p_value > 0 & p_value < 0.05) {
              p <- ggplot(temp, aes(measure1, measure2)) + 
                geom_point(size = 4) + geom_smooth(method = "lm", 
                                                   se = FALSE, colour = "black") + theme(text = element_text(size = 10)) + 
                labs(x = measure2, y = measure1) + theme_classic() + 
                annotate(geom = "text", label = paste("p value = ", 
                                                      round(p_value, 4)), x = -Inf, y = Inf, 
                         hjust = 0, vjust = 1)
              plot(p)
            }
          }
        }
      }
    }
  }
  if (factor == TRUE) {
    i <- 1
    j <- 1
    for (i in 1:length(collist)) {
      for (j in 1:length(collist)) {
        measure1 <- collist[i]
        measure2 <- collist[j]
        temp <- data %>% select(collist[i], collist[j], 
                                factorname)
        if (ncol(temp) > 2) {
          if (nrow(na.omit(temp)) > 0) {
            colnames(temp) <- c("measure1", "measure2", 
                                "category")
            temp$measure1 <- as.character(temp$measure1)
            temp <- temp[temp$measure1 != "Inf", ]
            temp$measure1 <- as.numeric(temp$measure1)
            
            temp$measure2 <- as.character(temp$measure2)
            temp <- temp[temp$measure2 != "Inf", ]
            temp$measure2 <- as.numeric(temp$measure2)
            
            
            temp$measure1 <- as.numeric(temp$measure1)
            temp$measure2 <- as.numeric(temp$measure2)
            colors <- viridis(length(unique(temp$category)))
            p_value <- NA
            omitted <- na.omit(temp)
            if (length(unique(omitted$category)) > 1) {
              mod <- lm(temp$measure2 ~ temp$measure1 * 
                          temp$category)
              p_value <- tidy(mod)[[5]][4]
            }
            print(paste(measure1, measure2, "p value (interaction effect) = ", 
                        round(p_value, 4)))
            if (is.na(p_value) == TRUE) {
              p_value <- 100
            }
            if (p_value > 0 & p_value < 0.05) {
              p <- ggplot(temp, aes(measure1, measure2, 
                                    color = category)) + geom_point(size = 4) + 
                geom_smooth(method = "lm", se = FALSE) + 
                theme(text = element_text(size = 10)) + 
                labs(x = measure2, y = measure1) + theme_classic() + 
                scale_color_manual(values = colors) + 
                annotate(geom = "text", label = paste("p value (interaction effect) = ", 
                                                      round(p_value, 4)), x = -Inf, y = Inf, 
                         hjust = 0, vjust = 1)
              plot(p)
            }
          }
        }
      }
    }
  }
}
