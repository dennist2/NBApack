#' Data Update
#' 
#' Use this to refresh data
#' 
#' @param nothing noii 
#' @export

RefreshData <- function(){
  
  k <- system.file("extdata","Agg_Season.csv",package = "NBApack")
  p <- read.csv(k)
  p <- p[,-c(1,3)]
  p$D0F1 <- as.factor(p$D0F1)
  p$strWL <- as.factor(p$strWL)
  p$H1A0 <- as.factor(p$H1A0)
  return(p)
  }