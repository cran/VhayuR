
## 1. Take two instruments - one very liquid and the other not so liquid.
## 2. For every trade of the illiquid instrument, get the last trade price of the
##    liquid instrument.
## 3. Compute the spread
## 4. If the spread is more than the threshold value, in this case the median
##    difference, include it in the results
##    data set.
## 5. Do this with three months of data

###########################################################################

# load VhayuR package
library(VhayuR)
library(chron)

# set defaults

# sever IP address - connection to it occurs at 1st read
# uncomment and replace IP address with address you use to connect to Vhayu
# vh.options(server = "10.10.1.50")

# data retrieval defaults (layout, start/end, columns, aggregation)
# These options can alternately be used as arguments to vh.get.* command.
vh.options(frDef = "VhTrade",  
	startTime = "20051201 09:30:00", 
	endTime = "20051201 16:00:00", 
	fieldNames = "VhExchgTime VhPrice",
	aggregate = vh.tail1,
	FUN = as.chron) # R class to use to represent times

x <- vh.get.zoo("GOOG")
y <- vh.get.zoo("IP")

# merge values inserting NAs filling NAs with last occurrence carried forward
xy <- na.locf(merge(x, y))

dif <- coredata(abs(xy$x - xy$y))

# subseries of y whose values are > indicated distance from x at same time
z <- xy[dif > median(dif), "y"]

head(z) # look at first few values
plot(z)

