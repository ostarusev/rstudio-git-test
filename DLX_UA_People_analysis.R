# 2019-1-10
# DataLex Project Data Analysis
#
library( tidyr )
library( dplyr )
library( ggplot2 )
library( scales )
library( ggpubr)

time_int <- function( dataset, date_col, start_date, end_date ) {
 return( filter( dataset %>% date_col >= start_date & date_col <= end_date))
}

setwd("~/R")
path <- "DLEX-ODC UA People - All Data.csv"

# Classes for importing instead of factors
ColClass <- c("character", "factor", "factor", "factor", "factor", "factor",
              "character", "factor", "factor", "factor", "character", "character", 
              "character",  "numeric", "factor", "factor", "factor")

# Import data from CSV file
DLX_UA_Ppl <- read.csv(path, colClasses = ColClass )

# from string to Date Conversion
DLX_UA_Ppl$StartDate <- as.Date(DLX_UA_Ppl$StartedIn, "%m/%d/%Y")
DLX_UA_Ppl$EndDate <- as.Date(DLX_UA_Ppl$EndDate, "%m/%d/%Y")
DLX_UA_Ppl$LastASMTdate <- as.Date(DLX_UA_Ppl$LastASMTdate, "%m/%d/%Y")
DLX_UA_Ppl$ExactEndDate <- as.Date(DLX_UA_Ppl$ExactEndDate, "%m/%d/%Y")

# Active vs. Total per City
DLX_UA_Ppl %>%
   group_by( City, Active ) %>%
    summarize( ppl_num = n()) %>%
     ggplot( aes( x = City, y = ppl_num, fill = Active )) +
      geom_col() +
       ylab("People number") +
       ggtitle("Active / Not Active per City v.1")

DLX_UA_Ppl %>%
  group_by( City, Active ) %>%
   summarize( ppl_num = n()) %>%
    ggplot( aes( x = City, y = ppl_num, fill = Active )) +
     geom_col(position = 'dodge', stat='identity') +
     ylab("People number") +
     geom_text(aes(label=ppl_num), position=position_dodge(width=0.9), vjust=-0.25) +
     ggtitle("Active / Not Active per City v.2")


# Years on account
DLX_UA_Ppl %>%
  group_by(Active) %>%
  ggplot( aes(x = YearsOnAccount, col = "red" )) +
  scale_y_continuous(breaks=c(0:15)) +
  scale_x_continuous(breaks=c(0:3)) +
# geom_histogram( binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)) ) +
  geom_histogram(binwidth = function(x) (max(x)-min(x))/nclass.Sturges(x), fill = "blue") +
#  geom_histogram(binwidth = function(x) (max(x)-min(x))/nclass.scott(x)) +
#  geom_histogram(binwidth = function(x) (max(x)-min(x))/nclass.FD(x)) +
  facet_grid(. ~ Active) +
  ylab("People Number") +
  ggtitle("Years on Account")
  
# Active Per Title
DLX_UA_Ppl %>% 
  group_by( CurrentTitle, City, Active ) %>%
  summarize( sumTitle = n()) %>%
  ggplot( aes(x = CurrentTitle, y = sumTitle, fill = City)) +
  scale_y_continuous(breaks=c(0:20)) +
  geom_col() +
  ylab("People Number") +
  ggtitle("Distribution of people per title") +
  theme( axis.text.x  = element_text(angle=90, vjust=0.5, size=10) ) +
  facet_grid(. ~ Active)

# Role Per City
DLX_UA_Ppl %>% 
  filter( Active == "Yes" ) %>%
   group_by( Role, City ) %>%
    summarize( sumTitle = n()) %>%
     ggplot( aes(x = Role, y = sumTitle, fill = City)) +
      scale_y_continuous(breaks=c(0:50)) +
       geom_col() +
       ylab("People Number") +
       ggtitle("Distribution of project roles per city") +
       theme( axis.text.x  = element_text(angle=90, vjust=0.5, size=10) ) +
       geom_text(aes(label=sumTitle), vjust=-0.25)
 
# Title Change 
DLX_UA_Ppl %>%
   group_by( TitleChangeReason, Active ) %>%
   summarize( ppl_num = n()) %>%
    ggplot( aes( x = TitleChangeReason, y = ppl_num, fill = Active )) +
    geom_col(position = 'dodge', stat='identity') +
    ylab("People number") +
    geom_text(aes(label=ppl_num), position=position_dodge(width=0.9), vjust=-0.25) +
    ggtitle("Title Changes reason")

# People per Teams
# v1 w/ ggplot / geom_bar / coor_polar
DLX_UA_Ppl %>%
  filter( Active == "Yes" ) %>%
   group_by( Team ) %>%
    summarize( ppl_cnt = n()) %>%
     ggplot(aes(x="", y=ppl_cnt, fill=Team)) +
      geom_bar(width=1,stat="identity") +
      coord_polar("y", start=0) +
      ggtitle("UA people distribution among teams") +
      ylab( "People Number" ) 
# v2 w/ ggpie from ggpubr
DLX_UA_Ppl %>%
  filter( Active == "Yes" ) %>%
   group_by( Team ) %>%
    summarize( ppl_cnt = n()) %>%
    mutate(prop = round(ppl_cnt*100/sum(ppl_cnt),1),
         labypos = cumsum(prop) - 0.5*prop) %>%
    ggpie( x = "prop", label = "ppl_cnt",
           lab.pos = "in",
           lab.font = list(color = "white"), fill = "Team", color = "white") +
    ggtitle("UA people distribution among teams")


#      scale_fill_grey()
#      scale_fill_brewer("Dark2")
#      scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

# Title Distribution
DLX_UA_Ppl %>%
  filter( Active == "Yes" ) %>%
   group_by( CurrentTitle ) %>%
    summarize( ppl_cnt = n()) %>%
     ggplot(aes(x="", y=ppl_cnt, fill=CurrentTitle)) +
      geom_bar(width=1,stat="identity") +
       coord_polar("y", start=0) +
        ggtitle("Title distribution in the account") +
        ylab( "People Number" ) 

# Leaving Reasons per team
DLX_UA_Ppl %>%
  filter( Active == "No") %>%
   group_by( Team, LeavingReason ) %>%
    summarize( ppl_cnt = n()) %>%
     ggplot( aes(x = Team, y = ppl_cnt, fill = LeavingReason)) +
      geom_col() +
      ylab("People Number") +
      ggtitle("Leaving reasons per team") +
      scale_y_continuous(breaks=c(0:15)) +
      geom_text(aes(label=ppl_cnt), position=position_stack(), vjust=-0.25) +
      theme( axis.text.x  = element_text(angle=45, vjust=0.5, size=10) )

# Leaving TimeLine
DLX_UA_Ppl %>%
  filter( !is.na(EndDate)) %>%
  group_by( EndDate ) %>%
   summarize( ppl_cnt =n()) %>%
    mutate( CumSum = cumsum(ppl_cnt)) %>%
     ggplot( aes(x = EndDate, y = CumSum, size = ppl_cnt)) +
      scale_x_date( date_minor_breaks = "1 month") +
      ylim( 0, 60 ) +
      ylab("People Number") +
      geom_line( col = "red") +
      geom_point( shape = 21, size=3 ) +
      ggtitle("Leaving timeline")

leave_ppl <- 
  DLX_UA_Ppl %>%
  group_by( EndDate ) %>%
  summarize( ppl_cnt =n())

# rpivoTable using
#library(rpivotTable)
#rpivotTable(trees, aggregatorName="Average",  vals="Volume", 
#            cols="Height", rendererName="Line Chart")

# -------------------------------------------------------
# Hiring / Dismissal per year
# -------------------------------------------------------
# People who were hired

years <- c("2016", "2017", "2018", "2019")
years <- as.factor(years)

# Hired people
h2016 <- DLX_UA_Ppl %>%
  filter( StartDate >= "2016-01-01" & StartDate <= "2016-12-31") 
hired2016 <- h2016 %>% summarize( hired16 = n())  
h2017 <- DLX_UA_Ppl %>%
  filter( StartDate >= "2017-01-01" & StartDate <= "2017-12-31")
hired2017 <- h2017 %>% summarize( hired17 = n())
h2018 <- DLX_UA_Ppl %>%
  filter( StartDate >= "2018-01-01" & StartDate <= "2018-12-31") 
hired2018 <- h2018 %>% summarize( hired18 = n())
h2019 <- DLX_UA_Ppl %>%
  filter( StartDate >= "2019-01-01" & StartDate <= "2019-12-31") 
hired2019 <- h2019 %>% summarize( hired19 = n())
hired <- c(hired2016$hired16, hired2017$hired17, hired2018$hired18, hired2019$hired19)

# Dismissed people
d2016 <- DLX_UA_Ppl %>%
  filter( EndDate >= "2016-01-01" & EndDate <= "2016-12-31") 
dismiss2016 <- d2016 %>% summarize( dm16 = n() )
d2017 <- DLX_UA_Ppl %>%
  filter( EndDate >= "2017-01-01" & EndDate <= "2017-12-31")
dismiss2017 <- d2017 %>% summarize( dm17 = n() )
d2018 <- DLX_UA_Ppl %>%
  filter( EndDate >= "2018-01-01" & EndDate <= "2018-12-31") 
dismiss2018 <- d2018 %>% summarize( dm18 = n() )
d2019 <- DLX_UA_Ppl %>%
  filter( EndDate >= "2019-01-01" & EndDate <= "2016-12-31") 
dismiss2019 <- d2019 %>% summarize( dm19 = n() )
dismiss <- c(dismiss2016$dm16, dismiss2017$dm17, dismiss2018$dm18, dismiss2019$dm19)

# People who are active at the end of the year
active16 <- hired2016$hired16 - dismiss2016$dm16 + 0
active17 <- hired2017$hired17 - dismiss2017$dm17 + active16
active18 <- hired2018$hired18 - dismiss2018$dm18 + active17
active19 <- hired2019$hired19 - dismiss2019$dm19 + active18
active_eoy <-c(active16, active17, active18, active19 )

# summary table
people_sum <- data.frame( years, hired, dismiss, active_eoy )

# All hired and dismissed people
#hired
hired1617 <- rbind(h2016, h2017)
hired1618 <- rbind(h2016, h2017, h2018)
hired1619 <- rbind( h2016, h2017, h2018, h2019 )

# Dismissed
dism1617 <- rbind(d2016, d2017)
dism1618 <- rbind(d2016, d2017, d2018)
dism1619 <- rbind( d2016, d2017, d2018, d2019)

# People who were hired and were not dismissed in the pointed year
work16 <- anti_join( h2016, d2016 )
work17 <- anti_join( hired1617, dism1617 )
work18 <- anti_join( hired1618, dism1618 )
work19 <- anti_join( hired1619, dism1619 )

# Breakdown of people who worked in pointed year
work18 %>%
  summarize( AQAcount = sum(Role == "Auto QA"),
             MQAcount = sum(Role == "Manual QA"),
             DevCount = sum(Role == "Developer"),
             KeyDevcount = sum(Role == "Key Dev"),
             TLcount  = sum(Role == "Team Lead"),
             TotalDev = DevCount + TLcount + KeyDevcount,
             TotalQA = AQAcount + MQAcount,
             OtherCount = n() - TotalDev - TotalQA )
work18 %>%
#  filter( Role %in% c("SA", "Key Dev", "Team Lead")) %>%
  filter( CurrentTitle %in% c("D3", "D4", "D5")) %>%
  summarize( DevSen = n())
work18 %>%
#  filter( Role %in% c("Manual QA", "Auto QA")) %>%
  filter( CurrentTitle %in% c("E3", "E4", "T3", "T4")) %>%
  summarize( QASen = n())
