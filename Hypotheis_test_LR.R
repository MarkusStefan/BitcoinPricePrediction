# hypothesis test coefficients
library(tidyverse)
data <- read_csv('https://raw.githubusercontent.com/pratikpv/predicting_bitcoin_market/master/crypto_data_news_reddit_final.csv')
data %>% View()
# dropping the timestamp
data <- data %>% select(-timestamp)

# moving close_BTCUSDT to first place
data <- cbind(data$close_BTCUSDT, select(data, -close_BTCUSDT))
colnames(data)

regressors <- colnames(data)


i <- 1
while (i<24) {
  if (i>2) {
    equation <- cat(equation, '+', regressors[i], append = T)
    i <- i+1
    
  }
  else if (i==1) {
    equation <- regressors[i]
    i <- i+1
  }
   

  else {
    equation <- cat(equation, '~',regressors [i], append = T)
    i <- i+1
  }
}

# copy output string from console

lm1 <- lm(formula = open_BTCUSDT ~ high_BTCUSDT + low_BTCUSDT + close_BTCUSDT + volume_BTCUSDT + close_LTCUSD + volume_LTCUSD + close_ETHUSD + volume_ETHUSD + gnews_flair + gnews_tb_polarity + gnews_tb_subjectivity + gnews_sid_pos + gnews_sid_neg + gnews_sid_neu + gnews_sid_com + reddit_flair + reddit_tb_polarity + reddit_tb_subjectivity + reddit_sid_pos + reddit_sid_neg + reddit_sid_neu + reddit_sid_com, data=data)

lm1 %>% summary()

library(stargazer)
stargazer(lm, type = "text")

lm2 <- lm(formula = open_BTCUSDT ~ high_BTCUSDT + close_LTCUSD +  close_ETHUSD, data=data)

stargazer(lm1, lm2, title="Results", align=TRUE, omit.stat=c("LL","ser"), no.space = T, single.row=TRUE, 
          column.sep.width = "1pt")


mkTexTable <- function(..., file){
  
  tbl <- capture.output({
    stargazer(...)
  })    
  
  tbl <- gsub("\\begin{tabular}", "\\resizebox{\\textwidth}{!}{\\begin{tabular}", tbl, fixed = T)
  tbl <- gsub("\\end{tabular}", "\\end{tabular}}", tbl, fixed = T)
  
  fileConn <- file(file)
  writeLines(tbl, fileConn)
  close(fileConn)
}

mkTexTable(lm1, lm2, title="Results", align=TRUE, omit.stat=c("LL","ser"), no.space = T, single.row=TRUE, 
           column.sep.width = "1pt", file="texOutput.tex")
