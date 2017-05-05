#--------------------------------Loading package-------------------------------------------------------------------------------------------
if(!suppressWarnings(require(shiny))){install.packages("shiny");library(shiny)}
if(!suppressWarnings(require(sca))){install.packages("sca");library(sca)}
if(!suppressWarnings(require(dplyr))){install.packages("dplyr");library(dplyr)}
if(!suppressWarnings(require(Rcpp))){install.packages("Rcpp");library(Rcpp)}
if(!suppressWarnings(require(ggplot2))){install.packages("ggplot2");library(ggplot2)}
#-----------------------------Cancel scientific notation-----------------------------------------------------------------------------------
options(scipen = 20) 
#---------------------obtain Data----------------------------------------------------------------------------------------------------------
load(file = "data\DATA.rda")
#------------------------------------------------------------------------------------------------------------------------------------------
shinyServer(function(input, output) { 
#-----------------------------------Set All object-----------------------------------------------------------------------------------------
#====================================================Clear Data===============================================================================
  #---------------------------------Warning-------------------------------------------------------------------------------------------------
  waringmessage <- compiler::cmpfun(function(tit, texts, easyClose = TRUE){
    showModal(modalDialog(
      title = tit, texts, easyClose = easyClose))
  })
  #---------------------------------function------------------------------------------------------------------------------------------------
  ZHSY <- compiler::cmpfun(function(comb){ 
     NData <- subset(FDATA_1, FDATA_1[, 5] == comb)
     names(NData) <- c("date", "scode", "w", "r", "rank")
     MData <- cbind(NData[, -5], wr = round(NData$w * NData$r, 4))
     names(MData) <- c("date", "scode", "w", "r", "wr")
     ZHSY <- aggregate(MData$wr ~ MData$date, MData, sum)
     ZHSY[, 2] <- round(ZHSY[, 2], 4)
     ZHSY[which(ZHSY[, 2] > 10)] <- 10
     ZHSY[which(ZHSY[, 2] < -10)] <- -10
    return(ZHSY)
  })
  #------------------------------------------------------------------------------------------------------------------------------------------
  cppFunction("NumericVector CZHJZ(NumericVector ZHSY, int length){
                 NumericVector ZHJZ(length + 1);
                 ZHJZ[0] = 1; 
                 for (int i = 0; i < length; i++){
                     ZHJZ[i + 1] = ZHJZ[i] * (1 + ZHSY[i + 1]);
                 }
                return ZHJZ;
               }")
  ZHJZ <- compiler::cmpfun(function(comb){
    ZHSY <- ZHSY(comb = input$comb)[-1, 2] / 100
    CZHJZ <- CZHJZ(ZHSY, length(ZHSY))
    round(CZHJZ, 4)
  })
  #-------------------------------------------------------------------------------------------------------------------------------------------
  cppFunction("NumericVector CMHC(NumericVector ZHJZ, int length){
                 NumericVector MHC(length);
                 NumericVector vec(length);
                 for (int i = 0; i < length; i++){
                    vec[i] = ZHJZ[i];
                    MHC[i] = 1 - ZHJZ[i] / max(vec);
                 }
                return MHC;
               }")
  MHC <- compiler::cmpfun(function(comb){
    ZHJZ <- ZHJZ(comb = input$comb) 
    CMHC <- CMHC(ZHJZ, length(ZHJZ))
    round(CMHC, 4)
  }) 
  #-----------------------ֵ--------------------------------------------------------------------------------------------------------------------
  ZHTSY <- compiler::cmpfun(function(comb){
    ZHJZ <- ZHJZ(input$comb)
    ZHTSY <- round((ZHJZ - 1), 4) 
   return(ZHTSY)
  })
  #-------------------------------------------------------------------------------------------------------------------------------------------
  ZHTM <- compiler::cmpfun(function(cash, comb){
    ZHJZ <- ZHJZ(input$comb)
    ZHTM <- round(as.numeric(cash) * ZHJZ, 4)
    return(ZHTM)
  })
  #-------------------------------------------------------------------------------------------------------------------------------------------
  QSR <- compiler::cmpfun(function(comb){
    ZHSY <- ZHSY(comb = input$comb)
    ZHJZ <- ZHJZ(comb = input$comb)
    MHC <- MHC(comb = input$comb)
    ZHTSY <- ZHTSY(comb = input$comb)
    ZHTM <- ZHTM(cash = input$cash, comb = input$comb)
    data.frame(Date = as.character(ZHSY[, 1]), ZHSY = ZHSY[, 2], ZHJZ = ZHJZ, MHC = MHC, ZHTSY = ZHTSY, ZHTM = ZHTM)
  })
#=============================================================================================================================================
QSRInput <- compiler::cmpfun(eventReactive(input$Btn1, {
   if(!nchar(input$cash)){
     waringmessage("Warning", "Please enter cash!\n")
   }  else {
    QSR(input$comb)
   }
  }))
#============================================Net Figure Drawing===============================================================================
JZTinput <- compiler::cmpfun(eventReactive(input$Btn2, {
   if(!nchar(input$cash)){
     waringmessage("Warning", "Please enter cash!\n")
   } else {
    QSR <- QSR(input$comb)
    ggplot(data = QSR) + 
    geom_rect(mapping = aes(ymin = mean(ZHJZ), ymax = max(ZHJZ), xmin = 0, xmax = length(ZHJZ)), fill = "palegreen", alpha = 0.01) +
    geom_rect(mapping = aes(ymin = min(ZHJZ), ymax = mean(ZHJZ), xmin = 0, xmax = length(ZHJZ)), fill = "khaki", alpha = 0.01) +
    geom_line(mapping = aes(x = 1:length(ZHJZ), y = ZHJZ, col = "red")) + labs(x = "Date", y = "ZHJZ") +
    geom_hline(mapping = aes(yintercept = 1), lty = 2, col = "deepskyblue")
  }
}))
#==============================================================================================================================================
output$QSRtable <- compiler::cmpfun(DT::renderDataTable({
  QSRInput()
  }))
output$plot <- compiler::cmpfun(renderPlot({
  JZTinput()
  }))
#------------------------------------------------------------------------------------------------------------------------------------------
output$DD_QSR <- compiler::cmpfun(downloadHandler(
  filename = function(){
    paste('QSR.csv', sep='')
    },content = function(file){
      write.csv(QSRInput(), file)
      }))
#-------------------------------------------------------------------------------------------------------------------------------------------
})
