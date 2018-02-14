library(dplyr)
library(ggplot2)
library(knitr)
library(tidyr)
library(readr)
library(lubridate)
library(stringr)
library(plotly)
library(reshape)
library(gridExtra)
library(directlabels)

# Set working directory & load data ---------------------------
setwd("~/CMU/Spring 2017/Systems/Data")

firms <- read.csv('firms.csv')
all_locations <- read.csv('all_locations.csv')

prod_sub <- all_locations %>%
  dplyr::select(minDate, maxDate, typesimp)

prod_sub$min_date = as.Date(prod_sub$minDate, format="%m/%d/%Y")
prod_sub$max_date = as.Date(prod_sub$maxDate, format="%m/%d/%Y")   


#### Producers
prod_min <- prod_sub %>%
  dplyr::select(typesimp, date = min_date) %>%
  dplyr::filter(typesimp == "Producer") %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(tot_prod = n()) %>%
  dplyr::mutate(type = "Producer")

prod_max <- prod_sub %>%
  dplyr::select(typesimp, date = max_date) %>%
  dplyr::filter(typesimp == "Producer") %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(tot_prod = n()) %>%
  dplyr::mutate(type = "Producer")

prod_count <- rbind(prod_min, prod_max)

prod_count <- prod_count %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(cum_sum = cumsum(tot_prod))

ggplot(prod_count, aes(x=date, y=cum_sum)) +
  geom_line() 


#### Processors
proc_min <- prod_sub %>%
  dplyr::select(typesimp, date = min_date) %>%
  dplyr::filter(typesimp == "Processor") %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(tot_prod = n()) %>%
  dplyr::mutate(type = "Processor")

proc_max <- prod_sub %>%
  dplyr::select(typesimp, date = max_date) %>%
  dplyr::filter(typesimp == "Processor") %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(tot_prod = n()) %>%
  dplyr::mutate(type = "Processor")

proc_count <- rbind(proc_min, proc_max)

proc_count <- proc_count %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(cum_sum = cumsum(tot_prod))

ggplot(proc_count, aes(x=date, y=cum_sum)) +
  geom_line() 


####Producer-Processors
prod_proc_min <- prod_sub %>%
  dplyr::select(typesimp, date = min_date) %>%
  dplyr::filter(typesimp == "Producer-Processor") %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(tot_prod = n()) %>%
  dplyr::mutate(type = "Producer-Processor")

prod_proc_max <- prod_sub %>%
  dplyr::select(typesimp, date = max_date) %>%
  dplyr::filter(typesimp == "Producer-Processor") %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(tot_prod = n()) %>%
  dplyr::mutate(type = "Producer-Processor")

prod_proc_count <- rbind(prod_proc_min, prod_proc_max)

prod_proc_count <- prod_proc_count %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(cum_sum = cumsum(tot_prod))

ggplot(prod_proc_count, aes(x=date, y=cum_sum)) +
  geom_line() 


########Retailers
ret_min <- prod_sub %>%
  dplyr::select(typesimp, date = min_date) %>%
  dplyr::filter(typesimp == "Retailer") %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(tot_prod = n()) %>%
  dplyr::mutate(type = "Retailer")

ret_max <- prod_sub %>%
  dplyr::select(typesimp, date = max_date) %>%
  dplyr::filter(typesimp == "Retailer") %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(tot_prod = n()) %>%
  dplyr::mutate(type = "Retailer")

ret_count <- rbind(ret_min, ret_max)

ret_count <- ret_count %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(cum_sum = cumsum(tot_prod))

ggplot(prod_count, aes(x=date, y=cum_sum)) +
  geom_line() 


####### All together



all_types <- rbind(prod_count, proc_count, prod_proc_count, ret_count)

all_types <- dplyr::filter(all_types, date <= "2016-10-12")
write_csv(all_types, "num_ppr.csv")


ggplot(all_types, aes(x=date, y=cum_sum, color=type)) +
  geom_line(size=1.5) +
  scale_x_date(date_breaks = "3 months", date_labels = "%m-%Y") +
  scale_y_continuous(breaks = seq.int(0, 2000, 150)) +
  theme(axis.text.x = element_text(colour="grey20",size=14,angle=90,hjust=0,vjust=0,face="plain"),
        axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=18,angle=90,hjust=.5,vjust=.5,face="plain")) +
  labs(x="",
       y="Number of Licenses for Producers, \n Processors & Retailers") +
  scale_color_brewer(palette="Dark2") +
  theme(legend.position="none") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  geom_dl(aes(label = type, size = 14), method = list(dl.combine("last.points"), cex = 1.5, hjust=1, vjust=0)) 
  
