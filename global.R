#load required libraries
library(shiny)
library(dygraphs)
library(DT)
library(ggplot2)
library(RCurl)
library(reshape2)
library(foreign)
library(vcd)
library(plyr)
library(dplyr)
library(scales)
library(shiny)
library(shinyapps)
library(shinythemes)
library(lattice)
library(ggvis)
library(ggthemes)
library(RColorBrewer)
library(plotly)
library(DescTools)
library(RColorBrewer)
library(extrafont)
library(devtools)
library(markdown)
library(extrafont)
library(datasets)
library(tables)
library(xtable)

####creation of full dataset on local directory for uploading####

getYRBSSData <- function() {
  
  # get data from github
  url <- "https://raw.githubusercontent.com/LivingSocially/HealthDataSciencePracticum/master/data/YRBSSData.csv"
  YRBSSData.data <- getURL(url)                
  YRBSSData <- read.csv(textConnection(YRBSSData.data))}

  #create frequency tables for full YRBSS dataset
  #Table of Variable How Do You Describe Your Weight by sex, grade level, and year of survey
  (YRBSSData1 <- structable(Year ~ Sex + Grade + HowDoYouDescribeYourWGT, data = YRBSSData))
  (YRBSSData2 <- structable(Year ~ Sex + Grade + WhatAreYouTrying2DoAbtWGT, data = YRBSSData))
  (YRBSSData3 <- structable(Year ~ Sex + Grade + DidYouFastToLoseWGTPast30, data = YRBSSData))
  (YRBSSData4 <- structable(Year ~ Sex + Grade + DidYouTakePillsToLoseWGTPast30, data = YRBSSData))
  (YRBSSData5 <- structable(Year ~ Sex + Grade + DidYouVomitToLoseWGTPast30, data = YRBSSData))
  
  (YRBSSData6 <- structable(Year ~ Sex + Grade + PCTWhoDescribeThemselvesOWGT, data = YRBSSData))
  (YRBSSData7 <- structable(Year ~ Sex + Grade + PCTWhoAreTrying2LoseWGT, data = YRBSSData))
  (YRBSSData8 <- structable(Year ~ Sex + Grade + PCTWhoFastedPast30Days, data = YRBSSData))
  (YRBSSData9 <- structable(Year ~ Sex + Grade + PCTWhoTookPillsPast30Days, data = YRBSSData))
  (YRBSSData10 <- structable(Year ~ Sex + Grade + PCTWhoVomitedPast30Days, data = YRBSSData))
  (YRBSSData11 <- structable(Year ~ Sex + Grade + PCTWhoAreOWGT, data = YRBSSData))
  (YRBSSData12 <- structable(Year ~ Sex + Grade + PCTWhoAreOBESE, data = YRBSSData))
  (YRBSSData13 <- structable(Year ~ Sex + Grade + PCTAtRisk4OWGT, data = YRBSSData))
  
  #Create dataframe from frequency tables
  HowDoYouDescribeYourWGTdfYRBSS <- as.data.frame(YRBSSData1)
  WhatAreYouTrying2DoAbtWGTdfYRBSS  <- as.data.frame(YRBSSData2)
  DidYouFastToLoseWGTPast30dfYRBSS  <- as.data.frame(YRBSSData3)
  DidYouTakePillsToLoseWGTPast30dfYRBSS  <- as.data.frame(YRBSSData4)
  DidYouVomitToLoseWGTPast30dfYRBSS  <- as.data.frame(YRBSSData5)
  
  PCTWhoDescribeThemselvesOWGTdfYRBSS  <- as.data.frame(YRBSSData6)
  PCTWhoAreTrying2LoseWGTdfYRBSS  <- as.data.frame(YRBSSData7)
  PCTWhoFastedPast30DaysdfYRBSS  <- as.data.frame(YRBSSData8)
  PCTWhoTookPillsPast30DaysdfYRBSS  <- as.data.frame(YRBSSData9)
  PCTWhoVomitedPast30DaysdfYRBSS  <- as.data.frame(YRBSSData10)
  PCTWhoAreOWGTdfYRBSS  <- as.data.frame(YRBSSData11)
  PCTWhoAreOBESEdfYRBSS  <- as.data.frame(YRBSSData12)
  PCTAtRisk4OWGTdfYRBSS <- as.data.frame(YRBSSData13)

  
  #How Do You Describe Your Weight? - Very Underweight 2001-2013
  Underweight <- subset(HowDoYouDescribeYourWGTdfYRBSS, 
                        HowDoYouDescribeYourWGT=='Slightly underweight'| 
                          HowDoYouDescribeYourWGT=='Very underweight',  
                        select=c(Sex, Grade, HowDoYouDescribeYourWGT, Year, Freq)) 
  
  Overweight <- subset(HowDoYouDescribeYourWGTdfYRBSS,
                       HowDoYouDescribeYourWGT=='Slightly overweight'| 
                         HowDoYouDescribeYourWGT=='Very overweight',  
                       select=c(Sex, Grade, HowDoYouDescribeYourWGT, Year, Freq)) 
  
  #Create data frame from table
  Underweightdf <- data.frame(Underweight)
  Overweightdf <- data.frame(Overweight)
  
  #melt for ggplot
  UnderWGT.melt <- melt(Underweight, c("Sex", "Grade", "HowDoYouDescribeYourWGT", "Year"), na.rm = TRUE)
  UnderWGTFinal <- dcast(UnderWGT.melt, Sex + Grade + Year ~ ., sum)
  UnderWGTFinal.melt<- melt(UnderWGTFinal, c("Sex", "Grade", "Year"), na.rm = TRUE)
  UnderWGTFinal.melt=UnderWGTFinal.melt[1:70, c(1,2,3,5)]
  
  
  
#YRBSS Data 2001   
# using subset function 
  YRBSSDatadf2001 <- 
    subset(YRBSSData, Year==2001, 
           select=c(Age, Sex, Grade, HGT, WGT, HowDoYouDescribeYourWGT,
                    WhatAreYouTrying2DoAbtWGT, DidYouFastToLoseWGTPast30,
                    DidYouTakePillsToLoseWGTPast30, DidYouVomitToLoseWGTPast30,
                    PCTWhoDescribeThemselvesOWGT, PCTWhoAreTrying2LoseWGT, 
                    PCTWhoFastedPast30Days, PCTWhoTookPillsPast30Days,
                    PCTWhoVomitedPast30Days, PCTWhoAreOWGT, RaceEth,
                    PCTWhoAreOBESE, PCTAtRisk4OWGT
           ))
  
  #create frequency tables for YRBS 2001 Survey Data
  #Table of Variable How Do You Describe Your Weight by sex, grade level, and year of survey
  (YRBSS2001Datadf1 <- structable(HowDoYouDescribeYourWGT ~ Sex + Grade , data = YRBSSDatadf2001))
  (YRBSS2001Datadf2 <- structable(WhatAreYouTrying2DoAbtWGT ~ Sex + Grade , data = YRBSSDatadf2001))
  (YRBSS2001Datadf3 <- structable(DidYouFastToLoseWGTPast30 ~ Sex + Grade , data = YRBSSDatadf2001))
  (YRBSS2001Datadf4 <- structable(DidYouTakePillsToLoseWGTPast30 ~ Sex + Grade , data = YRBSSDatadf2001))
  (YRBSS2001Datadf5 <- structable(DidYouVomitToLoseWGTPast30 ~ Sex + Grade , data = YRBSSDatadf2001))

  
#YRBSS Data 2003      
  YRBSSDatadf2003 <- 
    subset(YRBSSData, Year==2003, 
           select=c(Age, Sex, Grade, HGT, WGT, HowDoYouDescribeYourWGT,
                    WhatAreYouTrying2DoAbtWGT, DidYouFastToLoseWGTPast30,
                    DidYouTakePillsToLoseWGTPast30, DidYouVomitToLoseWGTPast30,
                    PCTWhoDescribeThemselvesOWGT, PCTWhoAreTrying2LoseWGT, 
                    PCTWhoFastedPast30Days, PCTWhoTookPillsPast30Days,
                    PCTWhoVomitedPast30Days, PCTWhoAreOWGT, RaceEth,
                    PCTWhoAreOBESE, PCTAtRisk4OWGT
           ))
  
  #create frequency tables for YRBS 2003 Survey Data
  #Table of Variable How Do You Describe Your Weight by sex, grade level, and year of survey
  (YRBSS2003Datadf1 <- structable(HowDoYouDescribeYourWGT ~ Sex + Grade , data = YRBSSDatadf2003))
  (YRBSS2003Datadf2 <- structable(WhatAreYouTrying2DoAbtWGT ~ Sex + Grade , data = YRBSSDatadf2003))
  (YRBSS2003Datadf3 <- structable(DidYouFastToLoseWGTPast30 ~ Sex + Grade , data = YRBSSDatadf2003))
  (YRBSS2003Datadf4 <- structable(DidYouTakePillsToLoseWGTPast30 ~ Sex + Grade , data = YRBSSDatadf2003))
  (YRBSS2003Datadf5 <- structable(DidYouVomitToLoseWGTPast30 ~ Sex + Grade , data = YRBSSDatadf2003))
  
  (YRBSS2003Datadf6 <- structable(PCTWhoDescribeThemselvesOWGT ~ Sex + Grade , data = YRBSSDatadf2003))
  (YRBSS2003Datadf7 <- structable(PCTWhoAreTrying2LoseWGT ~ Sex + Grade, data = YRBSSDatadf2003))
  (YRBSS2003Datadf8 <- structable(PCTWhoFastedPast30Days ~ Sex + Grade , data = YRBSSDatadf2003))
  (YRBSS2003Datadf9 <- structable(PCTWhoTookPillsPast30Days ~ Sex + Grade , data = YRBSSDatadf2003))
  (YRBSS2003Datadf10 <- structable(PCTWhoVomitedPast30Days ~ Sex + Grade , data = YRBSSDatadf2003))
  

#YRBSS Data 2005   
  YRBSSDatadf2005 <- 
    subset(YRBSSData, Year==2005, 
           select=c(Age, Sex, Grade, HGT, WGT, HowDoYouDescribeYourWGT,
                    WhatAreYouTrying2DoAbtWGT, DidYouFastToLoseWGTPast30,
                    DidYouTakePillsToLoseWGTPast30, DidYouVomitToLoseWGTPast30,
                    PCTWhoDescribeThemselvesOWGT, PCTWhoAreTrying2LoseWGT, 
                    PCTWhoFastedPast30Days, PCTWhoTookPillsPast30Days,
                    PCTWhoVomitedPast30Days, PCTWhoAreOWGT, RaceEth,
                    PCTWhoAreOBESE, PCTAtRisk4OWGT
           ))
  
  
  #create frequency tables for YRBS 2005 Survey Data
  #Table of Variable How Do You Describe Your Weight by sex, grade level, and year of survey
  (YRBSS2005Datadf1 <- structable(HowDoYouDescribeYourWGT ~ Sex + Grade , data = YRBSSDatadf2005))
  (YRBSS2005Datadf2 <- structable(WhatAreYouTrying2DoAbtWGT ~ Sex + Grade , data = YRBSSDatadf2005))
  (YRBSS2005Datadf3 <- structable(DidYouFastToLoseWGTPast30 ~ Sex + Grade , data = YRBSSDatadf2005))
  (YRBSS2005Datadf4 <- structable(DidYouTakePillsToLoseWGTPast30 ~ Sex + Grade , data = YRBSSDatadf2005))
  (YRBSS2005Datadf5 <- structable(DidYouVomitToLoseWGTPast30 ~ Sex + Grade , data = YRBSSDatadf2005))
  
  (YRBSS2005Datadf6 <- structable(PCTWhoDescribeThemselvesOWGT ~ Sex + Grade , data = YRBSSDatadf2005))
  (YRBSS2005Datadf7 <- structable(PCTWhoAreTrying2LoseWGT ~ Sex + Grade, data = YRBSSDatadf2005))
  (YRBSS2005Datadf8 <- structable(PCTWhoFastedPast30Days ~ Sex + Grade , data = YRBSSDatadf2005))
  (YRBSS2005Datadf9 <- structable(PCTWhoTookPillsPast30Days ~ Sex + Grade , data = YRBSSDatadf2005))
  (YRBSS2005Datadf10 <- structable(PCTWhoVomitedPast30Days ~ Sex + Grade , data = YRBSSDatadf2005))

  
#YRBSS Data 2007      
  YRBSSDatadf2007 <- 
    subset(YRBSSData, Year==2007, 
           select=c(Age, Sex, Grade, HGT, WGT, HowDoYouDescribeYourWGT,
                    WhatAreYouTrying2DoAbtWGT, DidYouFastToLoseWGTPast30,
                    DidYouTakePillsToLoseWGTPast30, DidYouVomitToLoseWGTPast30,
                    PCTWhoDescribeThemselvesOWGT, PCTWhoAreTrying2LoseWGT, 
                    PCTWhoFastedPast30Days, PCTWhoTookPillsPast30Days,
                    PCTWhoVomitedPast30Days, PCTWhoAreOWGT, RaceEth,
                    PCTWhoAreOBESE, PCTAtRisk4OWGT
           ))
  
  (YRBSS2007Datadf1 <- structable(HowDoYouDescribeYourWGT ~ Sex + Grade , data = YRBSSDatadf2007))
  (YRBSS2007Datadf2 <- structable(WhatAreYouTrying2DoAbtWGT ~ Sex + Grade , data = YRBSSDatadf2007))
  (YRBSS2007Datadf3 <- structable(DidYouFastToLoseWGTPast30 ~ Sex + Grade , data = YRBSSDatadf2007))
  (YRBSS2007Datadf4 <- structable(DidYouTakePillsToLoseWGTPast30 ~ Sex + Grade , data = YRBSSDatadf2007))
  (YRBSS2007Datadf5 <- structable(DidYouVomitToLoseWGTPast30 ~ Sex + Grade , data = YRBSSDatadf2007))
  
  (YRBSS2007Datadf6 <- structable(PCTWhoDescribeThemselvesOWGT ~ Sex + Grade , data = YRBSSDatadf2007))
  (YRBSS2007Datadf7 <- structable(PCTWhoAreTrying2LoseWGT ~ Sex + Grade, data = YRBSSDatadf2007))
  (YRBSS2007Datadf8 <- structable(PCTWhoFastedPast30Days ~ Sex + Grade , data = YRBSSDatadf2007))
  (YRBSS2007Datadf9 <- structable(PCTWhoTookPillsPast30Days ~ Sex + Grade , data = YRBSSDatadf2007))
  (YRBSS2007Datadf10 <- structable(PCTWhoVomitedPast30Days ~ Sex + Grade , data = YRBSSDatadf2007))

  
#YRBSS Data 2009      
  YRBSSDatadf2009 <- 
    subset(YRBSSData, Year==2009, 
           select=c(Age, Sex, Grade, HGT, WGT, HowDoYouDescribeYourWGT,
                    WhatAreYouTrying2DoAbtWGT, DidYouFastToLoseWGTPast30,
                    DidYouTakePillsToLoseWGTPast30, DidYouVomitToLoseWGTPast30,
                    PCTWhoDescribeThemselvesOWGT, PCTWhoAreTrying2LoseWGT, 
                    PCTWhoFastedPast30Days, PCTWhoTookPillsPast30Days,
                    PCTWhoVomitedPast30Days, PCTWhoAreOWGT, RaceEth,
                    PCTWhoAreOBESE, PCTAtRisk4OWGT
           ))
  
  (YRBSS2009Datadf1 <- structable(HowDoYouDescribeYourWGT ~ Sex + Grade , data = YRBSSDatadf2009))
  (YRBSS2009Datadf2 <- structable(WhatAreYouTrying2DoAbtWGT ~ Sex + Grade , data = YRBSSDatadf2009))
  (YRBSS2009Datadf3 <- structable(DidYouFastToLoseWGTPast30 ~ Sex + Grade , data = YRBSSDatadf2009))
  (YRBSS2009Datadf4 <- structable(DidYouTakePillsToLoseWGTPast30 ~ Sex + Grade , data = YRBSSDatadf2009))
  (YRBSS2009Datadf5 <- structable(DidYouVomitToLoseWGTPast30 ~ Sex + Grade , data = YRBSSDatadf2009))
  
  (YRBSS2009Datadf6 <- structable(PCTWhoDescribeThemselvesOWGT ~ Sex + Grade , data = YRBSSDatadf2009))
  (YRBSS2009Datadf7 <- structable(PCTWhoAreTrying2LoseWGT ~ Sex + Grade, data = YRBSSDatadf2009))
  (YRBSS2009Datadf8 <- structable(PCTWhoFastedPast30Days ~ Sex + Grade , data = YRBSSDatadf2009))
  (YRBSS2009Datadf9 <- structable(PCTWhoTookPillsPast30Days ~ Sex + Grade , data = YRBSSDatadf2009))
  (YRBSS2009Datadf10 <- structable(PCTWhoVomitedPast30Days ~ Sex + Grade , data = YRBSSDatadf2009))
  
#YRBSS Data 2011  
  YRBSSDatadf2011 <- 
    subset(YRBSSData, Year==2011, 
           select=c(Age, Sex, Grade, HGT, WGT, HowDoYouDescribeYourWGT,
                    WhatAreYouTrying2DoAbtWGT, DidYouFastToLoseWGTPast30,
                    DidYouTakePillsToLoseWGTPast30, DidYouVomitToLoseWGTPast30,
                    PCTWhoDescribeThemselvesOWGT, PCTWhoAreTrying2LoseWGT, 
                    PCTWhoFastedPast30Days, PCTWhoTookPillsPast30Days,
                    PCTWhoVomitedPast30Days, PCTWhoAreOWGT, RaceEth,
                    PCTWhoAreOBESE, PCTAtRisk4OWGT
           ))
  
  (YRBSS2011Datadf1 <- structable(HowDoYouDescribeYourWGT ~ Sex + Grade , data = YRBSSDatadf2011))
  (YRBSS2011Datadf2 <- structable(WhatAreYouTrying2DoAbtWGT ~ Sex + Grade , data = YRBSSDatadf2011))
  (YRBSS2011Datadf3 <- structable(DidYouFastToLoseWGTPast30 ~ Sex + Grade , data = YRBSSDatadf2011))
  (YRBSS2011Datadf4 <- structable(DidYouTakePillsToLoseWGTPast30 ~ Sex + Grade , data = YRBSSDatadf2011))
  (YRBSS2011Datadf5 <- structable(DidYouVomitToLoseWGTPast30 ~ Sex + Grade , data = YRBSSDatadf2011))
  
  (YRBSS2011Datadf6 <- structable(PCTWhoDescribeThemselvesOWGT ~ Sex + Grade , data = YRBSSDatadf2011))
  (YRBSS2011Datadf7 <- structable(PCTWhoAreTrying2LoseWGT ~ Sex + Grade, data = YRBSSDatadf2011))
  (YRBSS2011Datadf8 <- structable(PCTWhoFastedPast30Days ~ Sex + Grade , data = YRBSSDatadf2011))
  (YRBSS2011Datadf9 <- structable(PCTWhoTookPillsPast30Days ~ Sex + Grade , data = YRBSSDatadf2011))
  (YRBSS2011Datadf10 <- structable(PCTWhoVomitedPast30Days ~ Sex + Grade , data = YRBSSDatadf2011))
  
  
#YRBSS Data 2013  
  YRBSSDatadf2013 <- 
    subset(YRBSSData, Year==2013, 
           select=c(Age, Sex, Grade, HGT, WGT, HowDoYouDescribeYourWGT,
                    WhatAreYouTrying2DoAbtWGT, DidYouFastToLoseWGTPast30,
                    DidYouTakePillsToLoseWGTPast30, DidYouVomitToLoseWGTPast30,
                    PCTWhoDescribeThemselvesOWGT, PCTWhoAreTrying2LoseWGT, 
                    PCTWhoFastedPast30Days, PCTWhoTookPillsPast30Days,
                    PCTWhoVomitedPast30Days, PCTWhoAreOWGT, RaceEth,
                    PCTWhoAreOBESE, PCTAtRisk4OWGT
           ))
  
  (YRBSS2013Datadf1 <- structable(HowDoYouDescribeYourWGT ~ Sex + Grade , data = YRBSSDatadf2013))
  (YRBSS2013Datadf2 <- structable(WhatAreYouTrying2DoAbtWGT ~ Sex + Grade , data = YRBSSDatadf2013))
  (YRBSS2013Datadf3 <- structable(DidYouFastToLoseWGTPast30 ~ Sex + Grade , data = YRBSSDatadf2013))
  (YRBSS2013Datadf4 <- structable(DidYouTakePillsToLoseWGTPast30 ~ Sex + Grade , data = YRBSSDatadf2013))
  (YRBSS2013Datadf5 <- structable(DidYouVomitToLoseWGTPast30 ~ Sex + Grade , data = YRBSSDatadf2013))
 
  (YRBSS2013Datadf6 <- structable(PCTWhoDescribeThemselvesOWGT ~ Sex + Grade , data = YRBSSDatadf2013))
  (YRBSS2013Datadf7 <- structable(PCTWhoAreTrying2LoseWGT ~ Sex + Grade, data = YRBSSDatadf2013))
  (YRBSS2013Datadf8 <- structable(PCTWhoFastedPast30Days ~ Sex + Grade , data = YRBSSDatadf2013))
  (YRBSS2013Datadf9 <- structable(PCTWhoTookPillsPast30Days ~ Sex + Grade , data = YRBSSDatadf2013))
  (YRBSS2013Datadf10 <- structable(PCTWhoVomitedPast30Days ~ Sex + Grade , data = YRBSSDatadf2013))
  