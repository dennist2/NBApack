#' NBA sraper
#' 
#' Use this to scrape NBA
#' 
#' @param no mas
#' @export


NBAUpdater <- function(){

current_date <- as.character(Sys.Date())

dri <- RSelenium::rsDriver(browser = c("chrome"), chromever = "77.0.3865.40", verbose = F)

remote <- dri[["client"]]
remote$open()



GrabData <- function(date1,date2,unit,sport){
  
  over_lord <- list()
  
  DateMaker <- function(date1,date2,d_unit,format="%Y-%m-%d"){
    dates <- seq(as.Date(date1,format=format),as.Date(date2, format=format), by=d_unit)
    format(dates,format)
  }
  
  date_list <- DateMaker(date1,date2,unit)
  sport <<- sport
  
  start_url <- "https://www.scoresandodds.com"
  
  new_date <- paste(start_url,"/printable/gameDate/",date_list[1],sep = "")
  
  remote$navigate(new_date)
  Sys.sleep(1)
  remote$refresh()
  Sys.sleep(7)
  
  response <- xml2::read_html(unlist(remote$getPageSource()), encoding = "UTF-8")
  ptables <- response %>% rvest::html_nodes('table') 
  
  leagues2 <- response %>% rvest::html_nodes('.league') %>% rvest::html_text()
  leaguetest <- stringr::str_locate(leagues2,pattern = " ")
  leaguetest <- leaguetest[,1]
  print("leagues2")
  leagues <- stringr::str_sub(leagues2,end = leaguetest)
  leagues <- stringr::str_trim(leagues)
  

  nbat <- c(which(leagues==sport))
  
  tablet <- ptables[nbat] %>% rvest::html_table(fill = TRUE)
  
  if(length(tablet)==0){
    print("aint no games!")
  } else {
    tablet2 <- tablet[[1]]
    tablet2 <- data.frame(tablet2,stringsAsFactors = FALSE)
    tablet2 <- transform(tablet2, Date = date_list[1])
    all_tables <- tablet2
    
    print("Sleeping before removing")
    
    temp <- all_tables
    
    both_na <- c(which(
      is.na(temp$Open) == TRUE 
      & is.na(temp$Line.Movements) == TRUE))
    
    if(sport == "NFL"|sport == "NBA"){
      tv_col <- c(which(is.na(temp$Money.Line)==TRUE & temp$Open==""))
    } else if (sport == "MLB"){
      tv_col <- c(which(temp$Line.Movements == temp$Open))
    }
    
    
    fin <- c(which(
      temp$Open == temp$Line.Movements
      & temp$Open == temp$Money.Line))
    
    bad_row <- c(both_na,tv_col,fin)
    bad_row <- sort(bad_row)
    temp <- temp[-bad_row,]
    sp <- temp$Open
    
    sp <- stringr::str_split(sp,pattern = "[' 'uo]",simplify = TRUE)
    
    temp$Open <- sp[,1]
    
    cur <- temp$Current
    cur <- stringr::str_split(cur,pattern = "[' 'uo]",simplify = TRUE)
    temp$Current <- cur[,1]
    
    
    
    
    
    kpop <- temp
    kpop <- transform(kpop,location="",row="")
    
    rown <- kpop$Team
    rown <- stringr::str_split(rown,pattern = " ",simplify = TRUE)
    rown <- rown[,1]
    kpop$row <- rown
    
    
    aways <- seq(1,length(kpop$Team),2)
    homes <- seq(2,length(kpop$Team),2)
    
    homs <- kpop[homes,]
    homs$location <- "Home"
    
    
    awas <- kpop[aways,]
    awas$location <- "Away"
    
    tits <- rbind(awas,homs)
    
    poot <- tits[order(tits$row),]
    
    
    tempT <- poot
    
    teamRM <- stringr::str_locate(tempT$Team,pattern = " ")
    teamRM <- teamRM[,1]
    tempT$Team <- stringr::str_sub(tempT$Team,start = teamRM+1)
    
    tempT <- transform(tempT,Line1="",Line2="",Line3="")
    tempT$Line.Movements <- stringr::str_replace_all(tempT$Line.Movements,pattern = "[ou]",replacement = " ")
    
    n <- length(tempT)
    np <- n-2
    
    tempT[,n:np] <- stringr::str_split(tempT$Line.Movements,pattern = "/",simplify = TRUE)
    
    tempT$Line1 <- stringr::str_trim(tempT$Line1)
    tempT$Line2 <- stringr::str_trim(tempT$Line2)
    tempT$Line3 <- stringr::str_trim(tempT$Line3)
    tempT$Current <- stringr::str_replace_all(tempT$Current,pattern = "PK",replacement = "-.01")
    
    L1 <- stringr::str_split(tempT$Line1,pattern = " ",simplify = TRUE)
    tempT$Line1 <- L1[,1]
    
    L2 <- stringr::str_split(tempT$Line2,pattern = " ",simplify = TRUE)
    tempT$Line2 <- L2[,1]
    
    L3 <- stringr::str_split(tempT$Line3,pattern = " ",simplify = TRUE)
    tempT$Line3 <- L3[,1]
    
    hop <- c(which(colnames(tempT) == "Scores"))
    hp <- hop+1
    tempT[,hop:hp] <- stringr::str_split(tempT$Scores,pattern = "\n",simplify = TRUE)
    
    totals <- c(which(tempT$Current > 0))
    fav <- c(which(tempT$Current < 0))
    
    tempT$Current <- stringr::str_replace_all(tempT$Current,pattern = "-.01",replacement = "0")
    tempT$Open <- stringr::str_replace_all(tempT$Open,pattern = "PK",replacement = "0")
    
    tots <- tempT[totals,]
    favs <- tempT[fav,]
    
    tots <- transform(tots,Blank="",Blank2="",Blank3="")
    favs <- transform(favs,Blank="",Blank2="",Blank3="")
    
    coln <- length(tots)
    aL <- coln-2
    aL2 <- coln-5
    aL3 <- coln-3
    
    tots[,aL:coln] <- tots[,aL2:aL3]    
    
    coln <- length(tots)
    coln2 <- length(favs)
    
    aL <- coln-2
    aL2 <- coln-5
    aL3 <- coln-3
    
    hL <- coln2-2
    hL2 <- coln2-5
    hL3 <- coln2-3
    
    tots[,aL2:aL3] <- favs[,hL2:hL3] 
    favs[,hL:coln2] <- tots[,aL:coln]
    
    tots <- transform(tots,openC="",closeC="")
    favs <- transform(favs,openC="",closeC="")
    
    pot <- length(tots)
    pot2 <- pot-1
    tots[,pot2] <- favs$Open
    tots[,pot] <- favs$Current
    tots[,pot2] <- as.numeric(tots[,pot2])*-1
    tots[,pot] <- as.numeric(tots[,pot])*-1
    
    top <- length(favs)
    top2 <- top-1
    favs[,top2] <- tots$Open
    favs[,top] <- tots$Current
    
    tots$Line1 <- as.numeric(tots$Line1)*-1
    tots$Line2 <- as.numeric(tots$Line2)*-1
    tots$Line3 <- as.numeric(tots$Line3)*-1
    
    

    totrows <- cbind(tots,favs$Team,tots$Scores,favs$Scores)
    peel <- colnames(totrows)
    peel[c(14:19)] <- c("opp1","opp2","opp3","oppOpen","oppClose","Opp")
    colnames(totrows) <- peel
    
    favrows <- cbind(favs,tots$Team,favs$Scores,tots$Scores)
    colnames(favrows) <- peel
    rmLine <- c(which(colnames(totrows) == "Line.Movements"))
    rmLine2 <- c(which(colnames(favrows) == "Line.Movements"))
    totrows <- totrows[,-rmLine]
    favrows <- favrows[,-rmLine2]
    all <- rbind(totrows,favrows)
    koolest <- all
    
    over_lord[[1]] <- koolest
  } 
  
  
  
  everything <- do.call(rbind,over_lord)
  return(everything)         
}





LineReversalsFunc <- function(sport,startD,endD,d_unit){
  start_url2 <- "https://zcodesystem.com/linereversals.php"
  remote$navigate(start_url2)
  
  if (sport == "NFL"){
    sn <- 3
  } else if (sport == "NCAAF"){
    sn <- 6
  } else if (sport == "NHL"){
    sn <- 2
  } else if (sport == "MLB"){
    sn <- 1
  } else if (sport == "NBA"){
    sn <- 4
  }
  print(sn)
  
  
  sport_paste <- paste("/html/body/div[1]/div[2]/div[1]/div/div[6]/div/span[",sn,"]",sep = "")
  
  sport_button <- remote$findElement(using = 'xpath', value = sport_paste)
  sport_button$clickElement()
  Sys.sleep(2)
  
  DateSeq <- function(startD,endD,d_unit,format="%d %b %Y"){
    dates <- seq(as.Date(startD,format=format),as.Date(endD, format=format), by=d_unit)
    format(dates,format)
  }
  
  date_list <- DateSeq(startD,endD,d_unit)
  print(date_list)
  
  
  date_data <- list()
  
  print(date_data)
  print("Above is date being scraped")
  
  
  date_in <- remote$findElement(using = 'class', value = 'hasDatepicker')
  date_in$clearElement()
  
  date_in$sendKeysToElement(list(date_list[1]))
  date_in$sendKeysToElement(list("\uE007"))
  Sys.sleep(3)
  
  
  print(date_list[1])
  
  
  response_page <- xml2::read_html(unlist(remote$getPageSource()), encoding = "UTF-8")
  
  response_tables <- response_page %>% xml2::html_nodes('table') %>% xml2::html_table(fill = TRUE)
  
  table2 <- response_tables[[2]]
  print(length(table2[,1]))
  print("above is length table2")
  if(length(table2[,1]) <= 2){ print("This day doesn't have games")}
  else {
    table3 <- table2[-1:-2,]
    table4 <- data.frame(table3, stringsAsFactors = FALSE)
    Sys.sleep(1)
    
    test_s <- data.frame(table4$X2, stringsAsFactors = FALSE)
    test_split <- test_s[[1]] %>% strsplit(split = "[:%(),]")
    
    ## Row Remover
    
    bad_rows <- c()
    remove_badrows <- function(input_data,subset){
      
      for(t in 1:length(subset)){
        
        if(length(subset[[t]]) == 1){
          bad_rows[t] <- t
          
        }  
      }
      if(length(bad_rows>0)){
        bad_frame <- data.frame(bad_rows)
        row_index <- remove_missing(bad_frame,na.rm = TRUE)
        output <- input_data[-row_index,]} else {input_data}
    }
    
    
    clean_data <- remove_badrows(table4,test_split)
    
    ## Split Data columns
    
    Dates <- data.frame(clean_data$X1, stringsAsFactors = F)
    TM1 <- data.frame(clean_data$X2, stringsAsFactors = F)
    TM1ML <- data.frame(clean_data$X3, stringsAsFactors = F)
    TM1O <- data.frame(clean_data$X14, stringsAsFactors = F)
    TM1U <- data.frame(clean_data$X15, stringsAsFactors = F)
    TM1SP <- data.frame(clean_data$X6, stringsAsFactors = F)
    TM2 <- data.frame(clean_data$X8, stringsAsFactors = F)
    TM2ML <- data.frame(clean_data$X9, stringsAsFactors = F)
    TM2O <- data.frame(clean_data$X14, stringsAsFactors = F)
    TM2U <- data.frame(clean_data$X15, stringsAsFactors = F)
    TM2SP <- data.frame(clean_data$X12, stringsAsFactors = F)
    Score <- data.frame(clean_data$X18, stringsAsFactors = F)
    
    
    split_score <- as.data.frame.character(Score[[1]],stringsAsFactors = FALSE)
    split_scores <- split_score[[1]] %>% stringr::str_split(pattern = ":", simplify = TRUE)
    
    split_scores[,2] <- split_scores[,2] %>% stringr::str_remove_all(pattern = " ")
    split_scores[,1] <- split_scores[,1] %>% stringr::str_remove_all(pattern = " ")
    
    team_score <- data.frame(split_scores[,1],stringsAsFactors = FALSE)
    opp_score <- data.frame(split_scores[,2],stringsAsFactors = FALSE)
    
    
    
    
    
    
    ## Split TM1, TM2
    
    TM1split <- TM1[[1]] %>% strsplit(split = "[:%(),]")
    TM2split <- TM2[[1]] %>% strsplit(split = "[:%(),]")
    
    ## Split each row function
    
    Splitter <- function(dataIN){
      
      row_mat <- list()
      test <- list()
      
      dummy <- matrix(nrow = length(dataIN), ncol = length(dataIN[[1]]))
      
      for (h in 1:length(dataIN)){
        test <- dataIN[[h]]
        dummy[h,] <- matrix(test, nrow = 1, ncol = length(dataIN[[1]]), byrow = TRUE)
      }
      
      output_Splitter <- data.frame(dummy, stringsAsFactors = FALSE)
    }
    
    
    
    ## Apply splitter to Teams1 Teams2
    
    Team1_final <- Splitter(TM1split)
    
    Team2_final <- Splitter(TM2split)
    Sys.sleep(2)
    
    
    ## Cbind all columns to TM1 TM2
    
    TM1rows <- cbind(Dates,Team1_final,TM1ML,TM1SP,TM1O,TM1U,team_score,opp_score)
    TM2rows <- cbind(Dates,Team2_final,TM2ML,TM2SP,TM2O,TM2U,opp_score,team_score)
    
    
    
    
    
    ## Delete unecessary columns 
    delete_bad_columns <- function(INPUTdata){
      output <- INPUTdata[,c(-4,-6,-8,-9,-11,-14,-17,-19,-22,-26,-29)]
    }
    
    TM1_final <- delete_bad_columns(TM1rows)
    TM2_final <- delete_bad_columns(TM2rows)
    
    
    
    ## Rbind TM1 TM2 to stacked columns
    final_data1 <- list(TM1_final,TM2_final)
    final_data <- data.table::rbindlist(final_data1)
    
    
    
    date_data[[1]] <- final_data
  }     
  
  
  final_out <- data.table::rbindlist(date_data,fill = TRUE)
  colnames(final_out) <- c("Date","Team","ML","SP","OV","UN","MLT","MLTR","SPT","SPTR","OVT","UNT","OUTR","PBR","SPTD","SPTDR","MLTD","MLTDR","OUTD","OUTDR","MLodd","Spreadd","OverL","UnderL","TeamPts","OppPts")
  return(final_out)
  
}

h2 <-as.character(Sys.Date())
h5 <- stringr::str_split(h2,pattern = "-",simplify = TRUE)
mid <- (as.integer(h5[3])-1)
h5[3] <- mid

hd <- paste(h5[1],h5[2],h5[3],sep = "-")
hd <- as.Date(hd)
s2 <- c(format="%Y-%m-%d")
current_date2 <- format(hd,s2)

ScoresOdds <- GrabData(date1 = current_date2,date2 = current_date2,unit = "days",sport = "NBA")


h <- as.character(Sys.Date())
h52 <- stringr::str_split(h,pattern = "-",simplify = TRUE)
midd <- (as.integer(h52[3])-1)
h52[3] <- midd
hdd <- paste(h52[1],h52[2],h52[3],sep = "-")
hdd <- as.Date(hdd)

s <- c(format="%d %b %Y")
current_date <- format(hdd,s)

LineRev <- LineReversalsFunc(sport = "NBA",current_date,current_date,"days")

sc <- ScoresOdds
lr <- LineRev


CombinedLRnSC <- function(dataLR,dataSnO){

  
  final_trim <- function(data){
    
    SP <- data$Spreadd
    OV <- data$OverL
    UN <- data$UnderL
    
    SPodd <- stringr::str_sub(SP,1,5)
    SPline <- stringr::str_sub(SP,6)
    
    OV <- stringr::str_split(OV, pattern = "\\(", simplify = TRUE)
    OV[,1] <- stringr::str_remove_all(OV[,1], pattern = "Over ")
    OV[,2] <- stringr::str_remove_all(OV[,2], pattern = "\\)")
    colnames(OV) <- c("Total22","OverOdd")
    
    UN <- stringr::str_split(UN, pattern = "\\(", simplify = TRUE)
    UN[,1] <- stringr::str_remove_all(UN[,1], pattern = "Under ")
    UN[,2] <- stringr::str_remove_all(UN[,2], pattern = "\\)")
    colnames(UN) <- c("Total2","UnderOdd")
    
    new_data <- cbind(data,SPodd,SPline,OV,UN)
    
    new_data$Team <- stringr::str_remove_all(new_data$Team, pattern = "ML public")
    new_data$Team <- stringr::str_remove_all(new_data$Team, pattern = "ML tickets")
    new_data$PBR <- stringr::str_remove_all(new_data$PBR,pattern = "Spread tickets difference")
    
    return(new_data)
  }

  dataLR <- final_trim(dataLR)
  d <- dataLR$Date
  d2 <- stringr::str_split(d,pattern = "[ ,]",simplify = TRUE)
  
  colnames(d2) <- c("Date","Mon","Year","Blank","Time","Time2")
  
  d2 <- data.frame(d2)
  d3 <- stringr::str_remove_all(d2$Date,pattern = "[thsndr]")
  d2$Date <- d3
  
  
  for(i in 1:length(d2$Date)){
    if(d2$Mon[i]=="Nov"){
      d2$moon[i] <- 11} 
    else if(d2$Mon[i]=="Sep"){
      d2$moon[i] <- 9} else if(d2$Mon[i]=="Oct"){
        d2$moon[i] <- 10} else if(d2$Mon[i]=="Dec"){
          d2$moon[i] <- 12} else if(d2$Mon[i]=="Jan"){
            d2$moon[i] <- 1} else if(d2$Mon[i]=="Feb"){
              d2$moon[i] <- 2} else if(d2$Mon[i]=="Mar"){
                d2$moon[i] <- 3} else if(d2$Mon[i]=="Apr"){
                  d2$moon[i] <- 4} 
  }
  
  
  ddd <- paste(d2$moon,d2$Date,d2$Year,sep = "-")
  
  LineRevD <- as.Date(ddd,format = "%m-%d-%Y")
  
  ScoresnoddD <- as.Date(dataSnO$Date,format = "%Y-%m-%d")
  
  
  dataLR$Date <- LineRevD
  dataSnO$Date <- ScoresnoddD
  
  MergeLR <- stringr::str_c(dataLR$Date,dataLR$Team)
  MergeOddSc <- stringr::str_c(dataSnO$Date,dataSnO$Team)
  
  
  LineRNFL <- transform(dataLR,Merge=MergeLR)
  ScoreOddNFL <- transform(dataSnO,Merge=MergeOddSc)
  
  print(colnames(LineRNFL))
  print(colnames(ScoreOddNFL))
  
  CombinedBitch <- merge(x= LineRNFL, y = ScoreOddNFL, by = "Merge", all.x = TRUE)
  
  return(CombinedBitch)
}

remote$close()
dri$server$stop()

new_day <- CombinedLRnSC(lr,sc)

write.csv(new_day,"currentUpdate.csv")

RefreshData <- function(){
  k <- system.file("extdata","Agg_Season.csv",package = "NBApack")
  p <- read.csv(k)
  return(p)
}

DataBase <- RefreshData()
D2 <- read.csv("currentUpdate.csv")
D2 <- D2[,-1]
DataBase <- DataBase[,-1]



D2[] <- lapply(D2, function(x) if(is.integer(x)) as.numeric(x) else as.factor(x))
DataBase[] <- lapply(DataBase, function(x) if(is.integer(x)) as.numeric(x) else as.factor(x))


D2[] <- lapply(D2, function(x) if(is.integer(x)) as.numeric(x) else as.factor(x))
DataBase[] <- lapply(DataBase, function(x) if(is.integer(x)) as.numeric(x) else as.factor(x))

p <- colnames(DataBase)
colnames(D2) <- p

dp <- rbind(DataBase,D2)

bad <- c(which(dp$MLTR=="Spread public" | dp$OV==" Under"))
o_bad <- c(which(is.na(dp$MLT)==TRUE & is.na(dp$MLTR)==TRUE))
tot <- c(which(dp$MLT=="MLT"))
all_b <- c(bad,o_bad,tot)

if(length(all_b)>0){
ddp <- dp[-all_b,]
} else {
  ddp <- dp
}


return(ddp)
}
