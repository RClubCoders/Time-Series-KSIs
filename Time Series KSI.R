

library(dygraphs)
library(jsonlite)
library(dplyr)

myAppId <- "your app key"
myAppKey <- "your app id"

d_list<- list()


#we create a loop to call data from multiple years from the API
for (i in c("2013","2014","2015")){

  requestUrl <- paste0("https://api.tfl.gov.uk/AccidentStats/",
                       i, "?app_id=", myAppId, "&app_key=", myAppKey)


 l = readLines(requestUrl, encoding="UTF-8", warn=FALSE)
 d = fromJSON(l)
  
 d_list[[i]] <- d
}

View(d)

## now we use the dplyr function to bind them together into one data frame

d<- rbind_all(d_list)

## date is not in the format we'd like so let's fix it and select just attendant data for now

d_clean<-d%>% select(id, lat,lon, location, dateandTime=date, severity, borough)

d_clean$date<-substr(d_clean$date,1,10)

#this is a useful reference for dates
#?strptime

d_clean$date<-as.Date(d_clean$date, "%Y-%m-%d")

#R is a little bit annoying with sorting date data so we create a new year month column

d_clean$yearMonth<-format(d_clean$date, "%Y-%m")

# you'll see it's reverted back into a character vector
class(d_clean$yearMonth)

# so we use a function from zoo that will convert it to year month by adding a day suffix

library(zoo)

d_clean$yearMonth<-as.Date(as.yearmon(d_clean$yearMonth))

# now you'll notice it's back into data
class(d_clean$yearMonth)


### for this analysis we'll stick to KSI collisions

d_clean_KSI<- d_clean%>% filter(severity == "Serious" | severity == "Fatal")

# let's table the the KSI collisions by month
table(d_clean_KSI$yearMonth)

# for a time series we only want the values, we can extract just the values
#we extract these using the as.vector function
ksi_cols<-as.vector(table(d_clean_KSI$yearMonth))

#now we create our time series
ksi_ts<-ts(data =ksi_cols, start = c(2013,1) , end =c(2015,12), frequency= 12)

# let's do a simple plot and see what the data looks like
plot(ksi_ts, col = "blue", lwd =2, ylab = "KSI", 
        main = "Collisions resulting 2013-2015")
grid()

# we could of course make a fancy html widget using dygraph instead
dygraph(ksi_ts)


# we can now use a decompose function to see whether there is any trend
#or seasonality in the data
ksi_decomp<- decompose(ksi_ts)


# if we plot it we see there is clearly both and the trend is thankfully downwards
plot(ksi_decomp)

# we note that decompose defaults to 'Additive ', you can specify multiplicativ if you,
# this mean we expect a multplicative relationship in the trend and volume 

# to get a better idea we can split the seasonality out by month and see which month
# we expect an increase or decrease. We see October is a particualr problem for KSIs
ksi_decomp$seasonal

#There is another function for decomposing a time series called  stl
# it uses a loess function and the documentation is found here
#?stl

# now let's see if we can use this data to forecast
# decompose is telling us there is trend and seasonality in the data so we
# there is also likely to be auto -correlation so let's check

#we need to load the library forecast

library(forecast)

# the box pierce test tells us whether there is autocorrelation
# there nearly always is in a time series!

Box.test(ksi_ts)

#interestingly our results are non-significant! 

# we can also check using the acf function and confirm it.
# lag 12 is probably a result of chance. 
Acf(ksi_ts)

# this is means we may be able to use a simpler forecasting strategy
# predict data based on 

# for instance the mean forecast based on the historic data
meanf(ksi_ts, h =6)

# a naive forecast simply based on the last observed value
naive(ksi_ts, h =6)


# seaosnally naive from the same value for the last season
# so same as the previous six months
snaive(ksi_ts, h =6)

# or a drift method where we use the last value and the average change
rwf(ksi_ts, drift = T, h =6)

# 

