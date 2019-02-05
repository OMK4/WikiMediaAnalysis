# Oscar Gutierrez
# Stat-196
# Professor Norris
# December 8th 2017
# Term Project - Analyzing a Large Dataset on Internet Searches

# Date and Time Functions
# as.POSIXct()
# as.POSIXlt()
# strptime()

# Import the dataset
eLog <- read.csv("~/Google Drive/Fall 2017/STAT196/Term Project/Discovery-Hiring-Analyst-2016-master/events_log.csv")

# Use RMarkdown to re-run your source code with the dataset in the same directory and obtain the same results and figures. 
# Task - Create a reproducible report* answering the following questions:

# 1 - What is our daily overall clickthrough rate? How does it vary between the groups?
# index and convert timestamp format into days in 1 - 8 
# round(timestamp/1000000) eliminates the HHMMSS from timestamp (YYYYMMDDHHMMSS)
# %% 10 eliminates the YYYYMM from timestamp (YYYYMM) leaving DD
total.days.index = round(eLog$timestamp/1000000, 0) %% 10 

overall.clickthrough.rate = eLog$action=="visitPage"
sum(overall.clickthrough.rate==T) 

#  Split into group a / group b 
# Index and convert timestamp format into days 1 - 8 for group a
group.a.clickthrough.index = eLog$group=="a" & eLog$action=="visitPage"
group.a                    = round(eLog[group.a.clickthrough.index,]$timestamp/1000000, 0) %% 10

# Index and convert timestamp format into days 1 - 8 for group a
group.b.clickthrough.index = eLog$group=="b" & eLog$action=="visitPage"
group.b       = round( eLog[group.b.clickthrough.index,  ]$timestamp/1000000, 0 ) %% 10

# Sum of each day for group a, b and print totals
total.click.rate   = NULL
group.a.click.rate = NULL
group.b.click.rate = NULL
for(i in 1:8){
  total.click.rate[i]   = sum(total.days.index == i & eLog$action=="visitPage")
  group.a.click.rate[i] = sum(group.a == i)
  group.b.click.rate[i] = sum(group.b == i)
  cat("\t\t\t\t\t","Group A   Group B   Total   Rate A     Rate B \n")
  cat("Daily click rate for March", i, "2016:\t", group.a.click.rate[i], 
      "    ", group.b.click.rate[i], "     ", total.click.rate[i],"  ",
      group.a.click.rate[i]/total.click.rate[i],"",group.b.click.rate[i]/total.click.rate[i], "\n\n")
}


# 2 - Which results do people tend to try first? How does it change day-to-day?
# Overall results that people tend to try first
# order the timestamp dataset from earliest to most recent
df1 = eLog[order(eLog$timestamp),]

summary(is.na(df1$session_id))

# subset the dataframe to remove session_id's after the earliest one during the session_id
df2 = subset(df1, !duplicated(df1$session_id))
# Action that people tend to try first
first.action.results = factor(df2$action)

# people tend to try searching the most ~99.85%
summary(first.action.results)

# Day-to-day results that people tend to try
each.days.index = round(df2$timestamp/1000000, 0) %% 10 
timestamp.day.index = NULL

for(i in 1:8){
  # Sum the days totaling each action && d
  action.checkins = sum((df2$action =="checkin") & (each.days.index==i) )
  action.visits   = sum((df2$action =="visitPage") & (each.days.index==i) )
  action.searches = sum((df2$action =="searchResultPage") & (each.days.index==i))
  
  cat("\t\t\t\t\t\t\t\t\t\t","Check-in   Search   Visit Page from Search\n")
  cat("Action results for March", i, "2016:\t", action.checkins, 
      "\t   ", action.searches, "   ", action.visits,"\n\n")
}


# 3 - What is our daily overall zero results rate? How does it vary between the groups?
# round(timestamp/1000000) eliminates the HHMMSS from timestamp (YYYYMMDDHHMMSS)
# %% 10 eliminates the YYYYMM from timestamp (YYYYMM) leaving DD
total.days.index = round(eLog$timestamp/1000000, 0) %% 10 

#  Split into group a / group b 
# Index and convert timestamp format into days 1 - 8 for group a
group.a.zero.index = eLog$group=="a" & (eLog$n_results==0 & !is.na(eLog$n_results))
group.a.zero       = round(eLog[group.a.zero.index,]$timestamp/1000000, 0) %% 10

# Index and convert timestamp format into days 1 - 8 for group a
group.b.zero.index = eLog$group=="b" & (eLog$n_results==0 & !is.na(eLog$n_results))
group.b.zero       = round(eLog[group.b.zero.index,]$timestamp/1000000, 0) %% 10

# Sum of each day for group a, b and totals
total.zero.rate   = NULL
group.a.zero.rate = NULL
group.b.zero.rate = NULL
for(i in 1:8){
  total.zero.rate[i]   = sum(total.days.index == i & eLog$action=="visitPage")
  group.a.zero.rate[i] = sum(group.a.zero == i)
  group.b.zero.rate[i] = sum(group.b.zero == i)
  cat("\t\t\t\t\t","Group A   Group B   Total   Rate A     Rate B \n")
  cat("Daily 0 result rate for March", i, "2016:", group.a.zero.rate[i], 
      "    ", group.b.zero.rate[i], " \t", total.zero.rate[i],"  ",
      group.a.zero.rate[i]/total.zero.rate[i],"  ",group.b.zero.rate[i]/total.zero.rate[i], "\n\n")
}


# 4 - Let session length be approximately the time between the first event and the last event in a session. 
#   Choose a variable from the dataset and describe its relationship to session length. Visualize the relationship.
# Get session lengths  
# order the dataset by timestamp from most recent to earliest 
eLog.time.ordered.dec = eLog[order(eLog$timestamp, decreasing = T),]
eLog.min.time          = subset(eLog.time.ordered.dec, !duplicated(eLog.time.ordered.dec$session_id))

# order the dataset by timestamp from earliest to most recent
eLog.time.ordered.inc = eLog[order(eLog$timestamp),]
eLog.max.time          = subset(eLog.time.ordered.inc, !duplicated(eLog.time.ordered.inc$session_id))

# order the both datasets by session_id to be able to subtract them
eLog.min.sess = eLog.max.time[order(eLog.max.time$session_id),]
eLog.max.sess = eLog.min.time[order(eLog.min.time$session_id),]

# store difference between max time stamps from min time stamps
time.diff = strptime(eLog.max.sess$timestamp,"%Y%m%d %H%M%S") - strptime(eLog.min.sess$timestamp,"%Y%m%d %H%M%S")
session.diff = abs(time.diff)
eLog.2  = cbind(eLog.max.sess,session.diff)
eLog.2$timestamp = as.POSIXct(strptime(eLog.2$timestamp,"%Y%m%d %H%M%S") )

# plot the relationship

library(ggplot2)
p1 = ggplot(data=eLog.2,aes(x=eLog.2$timestamp))+scale_x_datetime()
p2 = p1+ggtitle("Most Used Website Action")+ theme(plot.title = element_text(hjust = 0.5))
p3 = p2+geom_density(aes(group=eLog.2$action,fill=eLog.2$action),color='white', alpha=0.5)
p4 = p3+scale_fill_discrete(name="Action")
p5 = p4+labs(title="Actions per Day",x="March 1-8 2016",y="Density of User Sessions")
p5

# Non-Zero session lengths 
eLog.2.N0 = eLog.2[which(eLog.2$n_results>20),]
# Session lengths including zero - to have 
eLog.2.0 = eLog.2[which(eLog.2$n_results==0),]

# sample data set
# random sample of the data 
eLog.sample = eLog.2.N0[sample(1:500), ]

# 1b Multi-line scatterplot of mother weight vs birthweight using facet panes 
p3 = ggplot(data=eLog.sample, aes(x=timestamp, y=session.diff/5, shape=group)) 
p4 = p3 + labs(title="Length of session from March 1st - 8th \nFor Groups a and b ",x=" March 1st - 8th 2016",y="Minutes") + theme(plot.title = element_text(hjust = 0.5))
p5 = p4 + geom_point(aes(colour = group, size = n_results))  +ylim(0,250)   # + geom_smooth(method='lm')
p5





