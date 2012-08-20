# TODO: Add comment
# 
# Author: millaco
###############################################################################

if (0) # read in the raw data and process
{

	dataset <- "2012-07-17"
	readData <- function(x, ...) read.csv( paste(x, "_", dataset, ".csv", sep = ""), ... )

	SpeciesCode <- 164758 # whiting
	
	# haul info
	hh <- readData("HH")
	hh <- hh[c("Year", "Quarter","Country","Ship","Gear","HaulNo","month","Day",
					   "TimeShot","HaulDur","ShootLat","ShootLong")]
	names(hh) <- c("year","quart","country","ship","gear","haul","month","day",
		  	         "time","dur","lat","lon")
	hh $ id <- 1:nrow(hh)

	# age info
	ca <- readData("CA")
	ca <- subset(ca, SpecCode == SpeciesCode)
	ca <- ca[c("Quarter","Country","Ship","Gear","HaulNo","Year","LngtClas",
					   "Maturity","Age","Sex","NoAtALK","IndWgt")]
	names(ca) <- c("quart","country","ship","gear","haul","year","len","mat","age",
			           "sex", "n","wt")
	ca <- merge(ca, hh[c("quart","country","ship","gear","haul","year","id")])
					 
	# length info
	hl <- readData("HL")
	hl <- subset(hl, SpecCode == SpeciesCode & LngtClass != -9)
	hl <- hl[c("Quarter","Country","Ship","Gear","HaulNo","Year","LngtClass","HLNoAtLngt")]
	names(hl) <- c("quart","country","ship","gear","haul","year","len","n")
	hl <- merge(hl, hh[c("quart","country","ship","gear","haul","year","id")])

	## preprocessing
	# sort hl
	hl <- hl[order(hl $ len),]
	hl <- hl[order(hl $ id),]

  # make corrections in hl
	filt <- with(hl, (country == "ENG" & year >= 2004) |
					(country == "DEN" & year %in% c(1973:1977, 1984)) |
					(country == "NED" & year == 2006 & len > 60) |
					(country == "DEN" & year %in% c(1972, 1979, 1982, 1983, 1987) & len > 60))
	
	hl[filt, "len"] <- hl[filt, "len"] / 10
	
	# save data
	save(hh, hl, ca, file = "haulData.rData")

	# tidy up
	rm(readData, dataset, SpeciesCode, filt)
}

if (0) # add some data summaries
{
	load("haulData.rData")
	
	# add mean lengths to haul info
	tmp <- by(hl, hl $ id, function(x) sum(x $ len * x $ n) / sum(x $ n))
	tmp2 <- data.frame(id = as.numeric(dimnames(tmp)[[1]]))
	rownames(tmp2) <- NULL
	attributes(tmp) <- NULL
	tmp2 $ meanlen <- tmp
  hh <- merge(hh, tmp2, all = TRUE)
	rm(tmp, tmp2)
	
	hl $ dur <- hh $ dur[hl $ id]
	
	save(hh, hl, ca, file = "haulData.rData")
}

# begin trying some things in ernest
load("haulData.rData")


if (0) # look into hl data - spatial temporal changes in length
{
xyplot(meanlen ~ lat | factor(year), subset(hh, !is.na(meanlen))

p <- ggplot(subset(hh, !is.na(meanlen) & meanlen > 20 & year > 1976 & quart == 1), aes(lon, lat)) 
p2 <- p + geom_point(aes(colour = meanlen), alpha = 0.5)

p2 + facet_wrap(~ year) 


p <- ggplot(subset(hl, year %in% 2000:2011), aes(x = len, y = n/dur, group = id)) 
p2 <- p + geom_line(aes(colour = id))

#p2 + facet_wrap(~ country) 
p2 + facet_grid(country ~ year) 

library(plyr)
ds <- ddply(hl, .(id), summarise, mode = len[which.max(n)][1])

hh $ mode <- NA
hh $ mode[ds $ id] <- ds $ mode

hh $ lenclass <- factor(floor(hh $ mode / 10) * 10)
p <- ggplot(subset(hh, !is.na(mode) & year > 1976), aes(lon, lat)) 
p2 <- p + geom_point(aes(colour = lenclass), alpha = 0.5)
p2
p2 + facet_wrap(~ year) 

comp <- with(hh, table(year, lenclass))
pcomp <- round(sweep(comp, 1, rowSums(comp), "/") * 100, 1)
pcomp
}

if (0) # look into ca data - first changes in weight length...
{
	ca $ mat[ca $ mat == -9] <- NA
	ca $ wt[ca $ wt == -9] <- NA
	ca $ sex[ca $ sex == -9] <- "U"
	ca $ age[ca $ age == -9] <- NA
	
	ca $ sex <- ca $ sex[drop = TRUE]
	ca $ mat <- ca $ mat[drop = TRUE]
	
	filt <- ca $ len > 70
	ca $ len[filt] <- ca $ len[filt] / 10
	
	ca $ lat <- hh $ lat[ca $ id]
	
	xyplot(len ~ age |  factor(year), group = factor(lat < 55), data = subset(ca, wt < 2000 & year > 2006))
}