### Molly Gallagher
### Data Scientist Exercise 02
### Interview process: RTI International - Infectious Disease Modeler

### first I did some exploratory analysis of the xml file and selected a few results to include in my report
### I then extracted and organized the narrative data from the .json files, but I did not do any further analysis with them

# local working directory:
setwd("C:/Users/mgalla9/Dropbox/data-scientist-exercise02-master/data-scientist-exercise02-master/data")

install.packages('viridis')
require(viridis)

# normally I would use the XML or xml2 package to extract this file and make a dataframe, but this structure 
# is a little different from what I'm used to, so I ended up generating a .csv file to work from instead
# https://www.convertcsv.com/xml-to-csv.htm

data = read.csv("AviationData.csv")
ncol(data)	# 31 total columns 
total = nrow(data)	# 77275 total entries

anyDuplicated(data[,1])	# row 7 has a duplicated event ID - rows 6 and 7 are reporting two different aircrafts involved in the same accident
				# so event IDs are not necessarily unique

anyDuplicated(data[,3])	# the accident numbers, however, are unique

# a lot of datapoints are missing, here; to start, let's see how many entries are complete:
clean_data = data[complete.cases(data), ]
nrow(data); nrow(clean_data)	# only 602 out of 77257 recorded incidents have no 'NA' entries
nrow(clean_data)/nrow(data)	# this doesn't tell you everything, though - some of the supposedly complete entries are still missing string fields

# incidents vs. accidents?	
unique(data[,2])		# this field is completed for all entries, so it's a convenient place to start
accidents = nrow(data[data[,2]=="Accident",]); incidents = nrow(data[data[,2]=="Incident",])
accidents/total; incidents/total

# preliminary vs. completed?
status = unique(data$ReportStatus)
statusCount = c()
for(i in 1:length(status)){
	statusCount[i] = length(which(data$ReportStatus==status[i]))
	}
cbind(status, statusCount/total)

nrow(data[data$ReportStatus=='Foreign' & data$Country=="United States",])	# did any of the foreign reports actually occur in the US?

# what about the 'country' field?
countries = unique(data$Country)
length(countries)	# 174 different 'countries' - but note that "" is included (135th element) - and misspelled entries are likely
countryCount = c()
for(i in 1:length(countries)){
	countryCount[i] = length(which(data$Country==countries[i]))
	}
countryCount[1]/total	# fraction that occurred in the US
countryCount[135]/total # fraction that are missing this entry

head(data[data$Country == "",])	# a lot of the entries with missing country data are over water

# let's skip those with "" in the country field:
subCountries = c(countries[2:134], countries[136:length(countries)]); subCountryCount = c(countryCount[2:134], countryCount[136:length(countries)]);
subCountryArray = cbind(subCountries, subCountryCount)
countryPlotArray = subCountryArray[order(-(as.numeric(subCountryArray[,2]))),]
# and plot just the top 20 countries:
countriesToPlot = 20
par(mfrow = c(1, 1))
par(mai = c(2, 1, 1, .2))
barplot(as.numeric(countryPlotArray[1:countriesToPlot,2]), names.arg=countryPlotArray[1:countriesToPlot,1], cex.names = 1, las = 2, ylab = "number of entries in top 20 non-US countries")

# what about the 'location' field?
locations = unique(data$Location)
length(locations)	# 24702 unique locations (including "" and "Unknown, UN")

# amateur build?
built = unique(data$AmateurBuilt)
builtCount = c()
for(i in 1:length(built)){
	builtCount[i] = length(which(data$AmateurBuilt==built[i]))
	}
builtCount[1]/sum(builtCount)  # less than 1% of entries is missing the "Amateur Built?" field
			
# the data are more complete here, and pro vs. amateur build is interesting; let's explore that further:

unique(data$InjurySeverity)

# is one safer than the other?

proNonfatal = nrow(data[data$AmateurBuilt==built[2] & data$InjurySeverity=="Non-Fatal",])	# not amateur built, with non-fatal injuries
amateurNonfatal = nrow(data[data$AmateurBuilt==built[3] & data$InjurySeverity=="Non-Fatal",])	# amateur built, with non-fatal injuries

proFatal = nrow(data[data$AmateurBuilt==built[2] & data$InjurySeverity!="Non-Fatal" & data$InjurySeverity!="Unavailable" & data$InjurySeverity!="",])	# not amateur built, with non-fatal injuries
amateurFatal = nrow(data[data$AmateurBuilt==built[3] & data$InjurySeverity!="Non-Fatal" & data$InjurySeverity!="Unavailable" & data$InjurySeverity!="",])	# amateur built, with fatal injuries

fatalitiesByBuild = c(proNonfatal, amateurNonfatal, proFatal, amateurFatal)



fracFatalitiesByBuild = c(proNonfatal/sum(fatalitiesByBuild), 
				proFatal/sum(fatalitiesByBuild), 
				 amateurNonfatal/sum(fatalitiesByBuild),
				amateurFatal/sum(fatalitiesByBuild))
par(mfrow = c(2, 1))
par(mai = c(1, 1.6, 0.5, .2))
cols = viridis(1, begin = .5, end = .65)

# first, plot the raw fractions:
barplot(fracFatalitiesByBuild, ylab = "fraction of accidents", main = "", col = cols, space = .05, cex.lab = 2, cex.axis = 2, cex.main = 1.5, cex.names = 1.15, names.arg = c("professional\n nonfatal", "professional\n fatal", "amateur\n nonfatal", "amateur\n fatal"))
text(1.6, 1.25*fracFatalitiesByBuild[2], paste(round(fracFatalitiesByBuild[2],2)), cex = 2) 
text(3.7, 2.5*fracFatalitiesByBuild[4], paste(round(fracFatalitiesByBuild[4],2)), cex = 2)

# then adjust for the relative frequency of 'professional' vs. 'amateur' build aircrafts
fracFatalitiesByBuild_adj = c(proNonfatal/sum(proNonfatal + proFatal), 
				proFatal/sum(proNonfatal + proFatal), 
				 amateurNonfatal/sum(amateurNonfatal + amateurFatal),
				amateurFatal/sum(amateurNonfatal + amateurFatal))
barplot(fracFatalitiesByBuild_adj, ylab = "proportional fraction\nof accidents", space = .05, cex.lab = 2, cex.axis = 2, col = cols, cex.main = 1.5, cex.names = 1.15, names.arg = c("professional\n nonfatal", "professional\n fatal", "amateur\n nonfatal", "amateur\n fatal"))
text(1.6, 1.25*fracFatalitiesByBuild_adj[2], paste(round(fracFatalitiesByBuild_adj[2],2)), cex = 2) 
text(3.7, 1.2*fracFatalitiesByBuild_adj[4], paste(round(fracFatalitiesByBuild_adj[4],2)), cex = 2)

# save a pdf - just the second plot:
pdf("construction_fig.pdf")
par(mfrow = c(1, 1))
par(mai = c(1, 1.5, 0.3, .2))
barplot(fracFatalitiesByBuild_adj, ylab = "proportional fraction\nof accidents", space = .05, cex.lab = 2, cex.axis = 2, col = cols, cex.main = 1.5, cex.names = 1.15, names.arg = c("professional\n nonfatal", "professional\n fatal", "amateur\n nonfatal", "amateur\n fatal"))
text(1.6, 1.25*fracFatalitiesByBuild_adj[2], paste(round(fracFatalitiesByBuild_adj[2],2)), cex = 2) 
text(3.7, 1.2*fracFatalitiesByBuild_adj[4], paste(round(fracFatalitiesByBuild_adj[4],2)), cex = 2)
dev.off()

# check for statistical significance with a chi-square test:

fatBuildMat = matrix(nrow = 2, ncol = 2, fatalitiesByBuild)	# put into a matrix; columns are nonfatal and fatal, rows are professional and amateur
fatBuildMat = matrix(nrow = 2, ncol = 2, fatalitiesByBuild, byrow = T)	# put into a matrix; columns are nonfatal and fatal, rows are professional and amateur

chisq.test(fatBuildMat)	# definitely a significant difference between the non-fatal vs. fatal accidents in professional build vs. amateur build aircrafts


####################   consider some longitudinal data:

### accidents over time
dates = as.Date(data$EventDate, "%m/%d/%Y")

hist(na.omit(dates), breaks = 50)	# very little data prior to ~1982
length(dates)

# cut off the few entries with missing dates (found at the end of the file)
cutna = length(dates) - length(na.omit(dates))	

# cut off the first few years also - a small number of outliers separated from the bulk of the data
cut = length(which(na.omit(dates) < "1980-01-01")) + cutna

maindata = data[1:(nrow(data)-cut), ]

hist(na.omit(as.Date(maindata$EventDate, "%m/%d/%Y")), breaks = (2015 - 1980), xlab = "year", ylab = "recorded accidents/incidents")  

### consider the dates: how do fatalities change over time?

plot(as.Date(maindata$EventDate, "%m/%d/%Y"), maindata$TotalFatalInjuries, xlab = "year", ylab = "total fatal injuries", pch = 20)
abline(lm(maindata$TotalFatalInjuries~(as.Date(maindata$EventDate, "%m/%d/%Y"))), col = 'red')
summary(lm(maindata$TotalFatalInjuries~(as.Date(maindata$EventDate, "%m/%d/%Y"))))

### no - the slope is basically zero

sum(na.omit(data$TotalFatalInjuries)) # only 44017 fatalities since the US started keeping records - that doesn't seem too bad
max(na.omit(data$TotalFatalInjuries))
hist(log10(data$TotalFatalInjuries))
fatal = data$TotalFatalInjuries
presentFatal = length(total - na.omit(fatal))	# how many entries have fatality data?
naFatal = total - presentFatal				# how many entries do not?

### have accidents gotten more or less dangerous over the years?

# fatalities only:
plot(as.Date(maindata$EventDate, "%m/%d/%Y"), maindata$TotalFatalInjuries, xlab = "date", ylab = "fatalities", pch = 20)
# fatalities + all inuries:
plot(as.Date(maindata$EventDate, "%m/%d/%Y"), maindata$TotalFatalInjuries+maindata$TotalSeriousInjuries+maindata$TotalMinorInjuries,
     xlab = 'date', ylab = 'fatalities and all injuries', pch =20)


################################

### what about the type of aircraft?
aircrafts = unique(data$AircraftCategory)
fatalitiesByAircraft = c()
accidentsByAircraft = c()
for(i in 1:length(aircrafts)){
	fatalitiesByAircraft[i] = sum(na.omit(data$TotalFatalInjuries[data$AircraftCategory == aircrafts[i]]))
	accidentsByAircraft[i] = nrow(data[data$InvestigationType == "Accident" & data$AircraftCategory == aircrafts[i],])
	}
lm(fatalitiesByAircraft~aircrafts)
cbind(aircrafts, accidentsByAircraft, accidentsByAircraft/sum(accidentsByAircraft), fatalitiesByAircraft, fatalitiesByAircraft/sum(fatalitiesByAircraft))
cbind(aircrafts[2:13], accidentsByAircraft[2:13], accidentsByAircraft[2:13]/sum(accidentsByAircraft[2:13]), fatalitiesByAircraft[2:13], fatalitiesByAircraft[2:13]/sum(fatalitiesByAircraft[2:13]))

# assume that the missing values are all airplanes:
subAircrafts = aircrafts[2:13]
subFatalitiesByAircraft = c(fatalitiesByAircraft[1] + fatalitiesByAircraft[2], fatalitiesByAircraft[3:13])
subAccidentsByAircraft = c(accidentsByAircraft[1] + accidentsByAircraft[2], accidentsByAircraft[3:13])

cbind(subAircrafts, subAccidentsByAircraft, subAccidentsByAircraft/sum(subAccidentsByAircraft), subFatalitiesByAircraft, subFatalitiesByAircraft/sum(subFatalitiesByAircraft))

# it's tempting to recommend a Blimp or Powered-Lift (whatever that may be) as the safest form of transport, 
# but again, we can't really know the risks 
# without knowing how many flights each of these typically makes
# and how many passengers they carry

# and perhaps more importantly - the details of the 'aircraft category' field change over time:
# ignore the last three categories, with negligible counts (rocket - 1; powered-lift - 2; blimp - 1)
keyAircrafts = aircrafts[1:10]
aircraftArray = array(dim = c(length(keyAircrafts), nrow(maindata)))

par(mfrow=c(1,1))
cols = c("black", viridis(length(keyAircrafts)-1, begin = 0.2, end = 1) )
pdf("aircraft_over_time.pdf")
par(mai = c(1, 1.4, .5, .5))
plot(as.Date(maindata$EventDate, "%m/%d/%Y"), rep(0, nrow(maindata)), cex.lab = 1.5, cex.axis = 1.5, ylim = c(0, log10(nrow(maindata))), 
	cex = 0, xlab = "year", ylab = expression('aircraft category: cumulative entries (log'[10]*'  scale)'))
for(i in 1:length(keyAircrafts)){
	aircraftArray[i,] = c(cumsum(maindata$AircraftCategory==aircrafts[i]))
	lines(as.Date(maindata$EventDate, "%m/%d/%Y"), rev(log10(aircraftArray[i,])), col = cols[i], lwd = 2)
	}
legend("bottomright", legend = c("No data", keyAircrafts[2:10]), col = cols, lwd = 2, cex = 0.9, ncol = 2, bty = "n")
dev.off()

### make of the airplanes:
make = unique(data$Make)
makeCount = c()
for(i in 1:length(make)){
	makeCount[i] = length(which(data$Make==make[i]))
	}
#hist(makeCount, xlim = c(0, 500), breaks = 100)

make[which(makeCount > 10000)]
make[which(makeCount > 500)]

makeArray = cbind(make, makeCount)

# it becomes clear that there are duplicate entries, due to differences in capitalization
# there is also one entry for 'Cesna", presumed to be a misspelling of 'CESSNA'
# if you check out the NTSB website, you can see that there are drop-down menus for aircraft category, and other things like 
# amateur built y/n; but the make and model are entered manually, so there will likely be more inconsistency and error here
# let's look through the single entries in order to check for other possible misspelled versions
make[which(makeCount == 1)]	# given the enormous number of single entries, presumably unique names are common, and 
					# not terribly remarkable in comparison to the large number of CESSNA, BOEING, PIPER, etc. entries

make[which(makeCount != 1)]

###############

# how long does it take from the date of an accident to the date of publication?
timeToPublication =  as.Date(data$PublicationDate, "%m/%d/%Y") - as.Date(data$EventDate, "%m/%d/%Y")
plot(as.Date(data$EventDate, "%m/%d/%Y"), timeToPublication, xlab = 'year', ylab = 'days from incident to publication', pch = 20)

# how many reports are still preliminary?
unique(data$ReportStatus)
prelims = which(data$ReportStatus=="Preliminary")

###############
###############
############### read in the data from the json files, to get narratives and probable causes

# note: one of the solutions for using json files in R is the 'rjson' package, but based on errors I found trying to use this (in June 2020),
# and similar issues documented on StackOverflow, it seems to have a bug in how files are read -- stick with the RJSONIO package
# unless the problem with rjson is resolved

#install.packages('RJSONIO'); 
require(RJSONIO)

# call the narrative files (json type)
narrativeFiles = list.files(pattern="Narrative*")	

# open a loop over the narrative files & extract interesting data 
bigDataFrame = array(dim = c(0, 3))
for(i in 1:length(narrativeFiles)){  
	data = fromJSON(narrativeFiles[i])	# read the data
	frameData <- t(as.data.frame(data))	# transpose into a data frame - 3 columns are "EventId", "narrative", and "probable_cause"
	bigDataFrame = rbind(bigDataFrame, frameData)
	}

which(bigDataFrame[,1]=='20150825X61602')

# there are lots of interesting details to work with here as well

###  end 