
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)


head(wash)

head(chi)

# check the dimension of each file
dim(ny)
dim(wash)
dim(chi)

nrow(wash)/(nrow(ny)+nrow(wash)+nrow(chi))

# select only relevant columns
sel_cols <- c("Start.Time",
              "Start.Station","End.Station",
              "Trip.Duration","User.Type")

# List of files
list_of_files <- list(ny,wash,chi)

#  List of cities
name_of_cities <- c('New York','Washington','Chicago')

# function to keep only selected columns in a specified dataframe
mod_df <- function(file,cols){
  new_file <- subset(file,select = (names(file) %in% sel_cols))
  return(new_file)
}

# function to concatenate files in a given list of dataframes
# and only keep specified columns
mer_df <- function(files,cities,cols){
  new_file <- NULL
  for (i in (1:length(files))) {
       temp_file <- mod_df(files[[i]],cols)
       temp_file$city <- cities[i]
       new_file <- rbind(new_file,temp_file)
       print(dim(new_file))       
  }
  return(new_file)
}

# concatenating the three files into one using the mer_df function
cities3 <- mer_df(list_of_files,name_of_cities,sel_cols)

# check the contents of the new file
head(cities3)

library(ggplot2)
library(dplyr)

# Set up required variables (month, weekday, hour) in the dataset
cities3$month <- as.integer(format(as.Date(cities3$Start.Time,
                                           format="%Y-%m-%d"),"%m"))
cities3$weekday <- format(as.Date(cities3$Start.Time,
                                  format="%Y-%m-%d"),"%a")
cities3$hour <- as.integer(substr(cities3$Start.Time, 12, 13))

# check that the new variables have been created correctly
head(cities3)

# create the function, check_for_na, to check for any NA values
# in a specified dataset
check_for_na <- function(filenm){
  for (i in (1:length(filenm))) {
    if (any(is.na(filenm[,i]))) {
     print(names(filenm[i]))
     } 
  }
}


# check to see if there any NA values in the specified dataset
# by running the function, check_for_na 
check_for_na(cities3)

# investigate rows with a NA value in the variable, Trip.Duration 
subset(cities3,is.na(cities3$Trip.Duration))

# investigate rows with a NA value in the variable, month 
subset(cities3,is.na(cities3$month))

# remove one row where there are NAs values in the month field
dim(cities3)
cities3_orig  <- cities3 # keep a copy of the unmodified cities3 file just in case we need it later
cities3 <- subset(cities3,!is.na(cities3$month))
dim(cities3)

check_for_na(cities3)

# check out some count statistics
print('count by month')
table(cities3$month)
print('count by week day')
table(cities3$weekday)
print('count by hour')
table(cities3$hour)
print('count by month and city')
table(cities3$month,cities3$city)
print('count by week day and city')
table(cities3$weekday,cities3$city)
print('count by hour and city')
table(cities3$hour,cities3$city)

# Histogram plot function for continuous variable - not split by city
Hist_cont <- function(filenm,var,bin_width=1,break_no=31,x_desc){
  ggplot(aes(x = var), data = filenm) +
    geom_histogram(binwidth = bin_width, color='blue') +
    scale_x_continuous(breaks = 1:break_no) +
    ggtitle(paste('Number of trips by',x_desc)) +
    ylab('Number of Trips') +
    xlab(x_desc)  
}

# Histogram plot function for continuous variable - split by city
Hist_cont_groupby <- function(filenm,var,bin_width=1,break_no=31,x_desc){
  ggplot(aes(x = var), data = filenm) +
    geom_histogram(binwidth = bin_width, color='blue') +
    scale_x_continuous(breaks = 1:break_no) +
    facet_wrap(~filenm$city,ncol=2) +
    ggtitle(paste('Number of trips by City and',x_desc)) +
    ylab('Number of Trips') +
    xlab(x_desc)  
 }

# Histogram plot function for discrete variable - not split by city
Hist_disc <-  function(filenm,var,x_desc) {
  ggplot(aes(x = var), data = filenm) +
  geom_bar() +
  ggtitle(paste('Number of trips by',x_desc)) +
  ylab('Number of Trips') +
  xlab(x_desc)  
}

# Histogram plot function for discrete variable - split by city
Hist_disc_groupby <-  function(filenm,var,x_desc) {
  ggplot(aes(x = var), data = filenm) +
  geom_bar() +
  facet_wrap(~filenm$city,ncol=2) +
  ggtitle(paste('Number of trips by City and',x_desc)) +
  ylab('Number of Trips') +
  xlab(x_desc)  
}

# function that uses the histogram plot function to create charts
chart1 <- function(filenm,var,bin_width=1,break_no=31,x_desc,citynm='all',
                   groupby=FALSE){
  
  if (citynm %in% c('New York','Washington','Chicago')){
    filenm <- subset(filenm,select = (city==citynm))
    print(citynm)
    }  

  if (class(var) %in% c("integer","numeric")){
    if (groupby == 'TRUE') {
    Hist_cont_groupby(filenm,var,bin_width=1,break_no=31,x_desc)
    } else {Hist_cont(filenm,var,bin_width=1,break_no=31,x_desc)
      }
    }
  else if (class(var) == "character"){
    if (groupby == 'TRUE') {
      Hist_disc_groupby(filenm,var,x_desc)
    } else {Hist_disc(filenm,var,x_desc)} 
  }
  # return()
}

# create a histogram plot for the variable month in each city 
chart1(filenm=cities3,var=cities3$month,bin_width=1,break_no=6,x_desc='Month',
       groupby=TRUE)


# lets see what would happen if we left the row with the NA value
# in the month variable in the dataset
# create a histogram plot for the variable month in each city 
chart1(filenm=cities3_orig,var=cities3_orig$month,bin_width=1,break_no=6,
       x_desc='Month',groupby=TRUE)


# create a histogram plot for the variable month 
chart1(filenm=cities3,var=cities3$month,bin_width=1,break_no=6,
       x_desc='Month',groupby=FALSE)


# create a histogram plot for the variable weekday in each city 
chart1(filenm=cities3,var=cities3$weekday,x_desc='Week Day',
       groupby=TRUE)

# create a histogram plot for the variable weekday 
chart1(filenm=cities3,var=cities3$weekday,x_desc='Week Day',
       groupby=FALSE)

# create a histogram plot for the variable weekday for Chicago only 
temp  <- cities3[cities3$city == "Chicago",]
ggplot(aes(x = weekday), data = temp) +
  geom_bar() +
  coord_cartesian(ylim = c(1000,1400)) +
  ggtitle(paste('Chicago - Number of trips by week day')) +
  ylab('Number of Trips') +
  xlab('Week Day')  

########################################### start - add new r code - additional change 1 ########
# additional 1: could use the function to chart the week day counts for Chicago as follows:
temp  <- cities3[cities3$city == "Chicago",]
chart1(filenm=temp,var=temp$weekday,x_desc='Week Day',
       groupby=TRUE)
########################################### end - add new r code - additional change 1 ########

########################################### start - add new r code - additional change 2 ########
# additional 2: For comparison, lets chart the week day counts for New York:
chart1(filenm=cities3[cities3$city == "New York",],var=cities3[cities3$city == "New York",]$weekday,x_desc='Week Day',
       groupby=TRUE)
########################################### end - add new r code - additional change 2 ########

########################################### start - add new r code - additional change 3 ########
# additional 3: For comparison, lets chart the week day counts for Washington:
chart1(filenm=cities3[cities3$city == "Washington",],var=cities3[cities3$city == "Washington",]$weekday,x_desc='Week Day',
       groupby=TRUE)
########################################### end - add new r code - additional change 3 ########


# create a histogram plot for the variable hour in each city 
chart1(filenm=cities3,var=cities3$hour,bin_width=1,break_no=24,
       x_desc='Hour',groupby=TRUE)


# create a histogram plot for the variable hour
chart1(filenm=cities3,var=cities3$hour,bin_width=1,break_no=24,
       x_desc='Hour',groupby=FALSE)


# remove one row where there are NAs values in the month field
dim(cities3_orig)
cities3 <- subset(cities3_orig,!is.na(cities3_orig$Trip.Duration))
dim(cities3)

# check file again
head(cities3)
dim(cities3)

# create a variable to indicate the trip
# and check that the variable values are what we expect
cities3$Common.Trip <- paste(cities3$Start.Station, '|', cities3$End.Station)
head(cities3$Common.Trip)

# check how many start stations in each city
cities3 %>%
  count(city, Start.Station, sort = TRUE) %>%
  count(city, SORT = TRUE)

# check how many end stations in each city
cities3 %>%
  count(city, End.Station, sort = TRUE) %>%
  count(city, SORT = TRUE)

# check how many trips in each city
cities3 %>%
  count(city, Common.Trip, sort = TRUE) %>%
  count(city, SORT = TRUE)

# code to display the most common starting station
# and the corresponding number of trips
head(cities3 %>%
  count(city, Start.Station, sort = TRUE) )

# code to display the most common ending station
# and the corresponding number of trips
head(cities3 %>%
  count(city, End.Station, sort = TRUE) )

# code to display the most common trips
# and the corresponding number of trips 
head(cities3 %>%
  count(city, Common.Trip, sort = TRUE) )

# As stated earlier, let validate that there are starting stations
# that are not ending stations
# lets get an idea of how many starting station are not ending stations
`%!in%` <- Negate(`%in%`)
a  <- cities3[cities3$Start.Station %!in% cities3$End.Station,]
a[1,]
dim(a)


cities3[cities3$End.Station == "Adventures NYC",]

cities3[cities3$Start.Station == "Adventures NYC",]

# how many trips that have the start station equal to the end station
cities3$compare  <- (as.character(cities3$Start.Station) == 
                     as.character(cities3$End.Station))
same_stations  <- cities3[cities3$compare == TRUE,]
print('number of trips')
dim(cities3)[1]
print('number of trips where start station = end station')
dim(same_stations)[1]
print('percentage of trips where start station = end station')
dim(same_stations)[1]/dim(cities3)[1]

# function to display the counts for the specified variable
# (e.g. Start.Station, End.Station, Common.Trip)
# by selected city
count_st <- function(filenm,citynm="All",var1=""){
  if (citynm %in% c("New York","Washington","Chicago")){
    filenm <- cities3[cities3$city ==citynm,] 
    print(paste("City Name: ", citynm))
  } else {print(paste("Cities: New York, Washington, Chicago"))} 
  
  if (var1 == "Start.Station"){  
    print("Count of trips for the most common starting station")
     head(filenm %>% count(city, Start.Station, sort = TRUE) )
  } else if (var1 == "End.Station"){ 
    print("Count of trips for the most common ending station")
    head(filenm %>% count(city, End.Station, sort = TRUE) )
  } else if (var1 == "Common.Trip"){     
    print("Count of the most common trips")
    head(filenm %>% count(city, Common.Trip, sort = TRUE) )       
  } else {
    print("No variable specified")
  }     
}

# counts for the most common starting station
count_st(filenm = cities3, var1 = "Start.Station")

# counts for the most common starting station for New York
count_st(filenm = cities3, city="New York", var1 = "Start.Station")

# counts for the most common starting station for Washington
count_st(filenm = cities3, city="Washington", var1 = "Start.Station")

# counts for the most common starting station for Chicago
count_st(filenm = cities3, city="Chicago", var1 = "Start.Station")

# counts for the most common ending station
count_st(filenm = cities3, var1 = "End.Station")

# counts for the most common ending station for New York
count_st(filenm = cities3, city="New York", var1 = "End.Station")

# counts for the most common ending station for Washington
count_st(filenm = cities3, city="Washington", var1 = "End.Station")

# counts for the most common ending station for Chicago
count_st(filenm = cities3, city="Chicago", var1 = "End.Station")

# counts for the most common trip
count_st(filenm = cities3, var1 = "Common.Trip")

# counts for the most common trip for New York
count_st(filenm = cities3, city="New York", var1 = "Common.Trip")

# counts for the most common trip for Washington
count_st(filenm = cities3, city="Washington", var1 = "Common.Trip")

# counts for the most common trip for Chicago
count_st(filenm = cities3, city="Chicago", var1 = "Common.Trip")

# check to see what happens if we put an incorrect field in the function
# counts for the most common trip for Chicago
count_st(filenm = cities3, city="Chicago", var1 = "Chiacgo")

# Graph for starting stations
ggplot(aes(x = Start.Station), data = cities3) +
  geom_bar() +
  ggtitle('Number of trips starting at certain stations') +
  ylab('Number of trips') +
  xlab('Starting station') 

ggplot(aes(x = Start.Station), data = cities3) +
  geom_bar() +
  coord_cartesian(ylim = c(500,2000)) +
  labs(title = expression("Number of Trips starting at certain stations"),
       subtitle = expression("with count greater than 500")) +
  ylab('Number of trips') +
  xlab('Starting station') 

# Lets create a variable to indicate the city and the station
cities3$city_Start.Station  <- paste(cities3$city, '_', cities3$Start.Station)
head(cities3)

high_no_of_trips  <- subset(count(cities3, city_Start.Station, sort = TRUE),n > 500)
head(high_no_of_trips)
dim(high_no_of_trips)


# Number of trips starting at certain stations
# where this number is greater than 500
ggplot(aes(x = city_Start.Station , y = n), data = high_no_of_trips) +
  geom_point() +
  coord_flip() +
  labs(title = expression("Number of Trips starting at certain stations"),
       subtitle = expression("with count greater than 500")) +
  xlab('City/Starting station') +
  ylab('Number of trips') +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=10,face="italic"))

# Graph for ending stations
ggplot(aes(x = End.Station), data = cities3) +
  geom_bar() +
  ggtitle('Number of trips ending at certain stations') +
  ylab('Number of trips') +
  xlab('Ending station') +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=10,face="italic"))

ggplot(aes(x = End.Station), data = cities3) +
  geom_bar() +
  coord_cartesian(ylim = c(500,2000)) +
  labs(title = expression("Number of Trips ending at certain stations"),
       subtitle = expression("with count greater than 500")) +
  ylab('Number of trips') +
  xlab('Ending station')  +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=10,face="italic"))

# Lets create a variable to indicate the city and the station
cities3$city_End.Station  <- paste(cities3$city, '_', cities3$End.Station)
head(cities3)

high_no_of_trips  <- subset(count(cities3, city_End.Station, sort = TRUE),n > 500)
head(high_no_of_trips)
dim(high_no_of_trips)

ggplot(aes(x = city_End.Station , y = n), data = high_no_of_trips) +
  geom_point() +
  coord_flip() +
  labs(title = expression("Number of Trips"),
       subtitle = expression("with count greater than 500")) +
  xlab('City/Ending station') +
  ylab('Number of trips') +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=10,face="italic"))

# Graph for common trips
ggplot(aes(x = Common.Trip), data = cities3) +
  geom_bar() +
  ggtitle('Number of trips') +
  ylab('Number of common trips') +
  xlab('Starting station / Ending station') 

# Lets create a variable to indicate the city and the common tripn
cities3$city_Common.Trip  <- paste(cities3$city, '_', cities3$Common.Trip)
head(cities3)

high_no_of_trips  <- subset(count(cities3, Common.Trip, sort = TRUE),n > 50)
head(high_no_of_trips)
dim(high_no_of_trips)

# Number of common trips where the count is greater than 50
ggplot(aes(x =Common.Trip , y = n), data = high_no_of_trips) +
  geom_point() +
  coord_flip() +
  labs(title = expression("Number of Trips"),
       subtitle = expression("with count greater than 50")) +
  xlab('Starting station/Ending station') +
  ylab('Number of trips') +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=10,face="italic"))

# calculate some statistics
by(cities3_orig$Trip.Duration,cities3_orig$city,summary)

# check for NAs in the relevant field
by(cities3$Trip.Duration,cities3$city,summary)

# Note that this duration time is in seconds
by(cities3$Trip.Duration,cities3$city,sum)

# lets convert the duration time into days
by(cities3$Trip.Duration/360/24,cities3$city,sum)

# Note that this duration time is in seconds
by(cities3$Trip.Duration,cities3$city,mean)

# lets convert the duration time into hours
by(cities3$Trip.Duration/360,cities3$city,mean)

# Note that this duration time is in seconds
by(cities3$Trip.Duration,cities3$city,summary)

# lets convert the duration time into minutes
by(cities3$Trip.Duration/60,cities3$city,summary)

ggplot(aes(x = city, y = Trip.Duration), data = cities3) +
  geom_boxplot()   +
  ggtitle('Distribution of trip durations by city') +
  ylab('Trip Duration (in seconds)') +
  xlab('City') +
  geom_point(stat='summary',
             fun.y = mean,
             aes(color = factor(city)),
             size =5
            ) +
    labs(color="City - Mean Trip Duration")


ggplot(aes(x = city, y = Trip.Duration), data = cities3) +
  geom_boxplot()   +
  coord_cartesian(ylim = c(0,4000)) +
  ggtitle('Distribution of trip durations by city') +
  ylab('Trip Duration (in seconds)') +
  xlab('City') +
  geom_point(stat='summary',
             fun.y = mean,
             aes(color = factor(city)),
             size =5
            ) +
    labs(color="City - Mean Trip Duration")

ggplot(aes(x = city, y = Trip.Duration), data = cities3) +
  geom_boxplot()   +
  coord_cartesian(ylim = c(0,1500)) +
  ggtitle('Distribution of trip durations by city') +
  ylab('Trip Duration (in seconds)') +
  xlab('City') +
  geom_point(stat='summary',
             fun.y = mean,
             aes(color = factor(city)),
             size =5
            ) +
    labs(color="City - Mean Trip Duration")

system('python -m nbconvert Explore_bikeshare_data.ipynb')
