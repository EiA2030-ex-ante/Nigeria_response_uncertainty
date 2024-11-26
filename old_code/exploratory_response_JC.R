setwd("C:/DATA/Nigeria/EiA")

library(leaflet)
library(geodata)
library(terra)

list.files()

#d <- readRDS("NGA_Jordan.rds")

d <- read.csv("NGA_Jordan.csv")
s <- vect(d, geom=c("longitude", "latitude"))


# Nigeria code NGA
adm0 <- gadm(country="NGA", level=0, path=getwd())
adm1 <- gadm(country="NGA", level=1, path=getwd())
adm2 <- gadm(country="NGA", level=2, path=getwd())
#adm3 <- gadm(country="NGA", level=3, path=getwd())


# map point locations
plot(adm1)
plot(s, add=TRUE, pch=20, col="Red")
  # looks like pretty good coverage

# bring in raster layers for prediction
#source("Nigeria_raster_stack.R") 
# # or just get the output from that script:
rain      <- rast(paste(getwd(),"rain","Nigeria_rain_summaries.tif", sep="/"))
rain.30as <- rast(paste(getwd(),"rain","Nigeria_rain_summaries_30as.tif", sep="/"))
soil      <- rast(paste(getwd(), "soil_af_isda", "Nigeria_soil_layers.tif", sep="/"))
soil.30as <- rast(paste(getwd(), "soil_af_isda", "Nigeria_soil_layers_30as.tif", sep="/"))



#### merge all raster layers ####
stack <- c(soil, rain)
names(stack)


#### extract raster values to point locations ####
# # view points on raster data
# plot(stack["rain.sum.2017"])
# plot(s, add=TRUE, pch=20, col="Red")

xtra <- extract(stack, s)
s <- cbind(s,xtra)


#### prepare dataset for model estimation ####

# keep only years from 2001-2020 (last year is 2017)
sort(unique(s$year))
s.recent <- s[s$year > 2000]
dim(s.recent) # we still have 5187 obs, so not bad

# define new variable with correct rainfall to use for each year
# NOTE: I'M QUITE SURE THERE'S A MORE EFFICIENT WAY TO DO THIS, BUT FOR NOW...
s.recent[,"rain.sum"] <- 0
s.recent[s.recent$year==2001 ,"rain.sum"] <- s.recent$rain.sum.2001[s.recent$year==2001]
s.recent[s.recent$year==2002 ,"rain.sum"] <- s.recent$rain.sum.2002[s.recent$year==2002]
s.recent[s.recent$year==2003 ,"rain.sum"] <- s.recent$rain.sum.2003[s.recent$year==2003]
s.recent[s.recent$year==2004 ,"rain.sum"] <- s.recent$rain.sum.2004[s.recent$year==2004]
s.recent[s.recent$year==2005 ,"rain.sum"] <- s.recent$rain.sum.2005[s.recent$year==2005]
s.recent[s.recent$year==2006 ,"rain.sum"] <- s.recent$rain.sum.2006[s.recent$year==2006]
s.recent[s.recent$year==2007 ,"rain.sum"] <- s.recent$rain.sum.2007[s.recent$year==2007]
s.recent[s.recent$year==2008 ,"rain.sum"] <- s.recent$rain.sum.2008[s.recent$year==2008]
s.recent[s.recent$year==2009 ,"rain.sum"] <- s.recent$rain.sum.2009[s.recent$year==2009]
s.recent[s.recent$year==2010 ,"rain.sum"] <- s.recent$rain.sum.2010[s.recent$year==2010]
s.recent[s.recent$year==2011 ,"rain.sum"] <- s.recent$rain.sum.2011[s.recent$year==2011]
s.recent[s.recent$year==2012 ,"rain.sum"] <- s.recent$rain.sum.2012[s.recent$year==2012]
s.recent[s.recent$year==2013 ,"rain.sum"] <- s.recent$rain.sum.2013[s.recent$year==2013]
s.recent[s.recent$year==2014 ,"rain.sum"] <- s.recent$rain.sum.2014[s.recent$year==2014]
s.recent[s.recent$year==2015 ,"rain.sum"] <- s.recent$rain.sum.2015[s.recent$year==2015]
s.recent[s.recent$year==2016 ,"rain.sum"] <- s.recent$rain.sum.2016[s.recent$year==2016]
s.recent[s.recent$year==2017 ,"rain.sum"] <- s.recent$rain.sum.2017[s.recent$year==2017]
s.recent[s.recent$year==2018 ,"rain.sum"] <- s.recent$rain.sum.2018[s.recent$year==2018]
s.recent[s.recent$year==2019 ,"rain.sum"] <- s.recent$rain.sum.2019[s.recent$year==2019]
s.recent[s.recent$year==2020 ,"rain.sum"] <- s.recent$rain.sum.2020[s.recent$year==2020]

s.recent[,"rain.avg"] <- 0
s.recent[s.recent$year==2001 ,"rain.avg"] <- s.recent$rain.avg.2001[s.recent$year==2001]
s.recent[s.recent$year==2002 ,"rain.avg"] <- s.recent$rain.avg.2002[s.recent$year==2002]
s.recent[s.recent$year==2003 ,"rain.avg"] <- s.recent$rain.avg.2003[s.recent$year==2003]
s.recent[s.recent$year==2004 ,"rain.avg"] <- s.recent$rain.avg.2004[s.recent$year==2004]
s.recent[s.recent$year==2005 ,"rain.avg"] <- s.recent$rain.avg.2005[s.recent$year==2005]
s.recent[s.recent$year==2006 ,"rain.avg"] <- s.recent$rain.avg.2006[s.recent$year==2006]
s.recent[s.recent$year==2007 ,"rain.avg"] <- s.recent$rain.avg.2007[s.recent$year==2007]
s.recent[s.recent$year==2008 ,"rain.avg"] <- s.recent$rain.avg.2008[s.recent$year==2008]
s.recent[s.recent$year==2009 ,"rain.avg"] <- s.recent$rain.avg.2009[s.recent$year==2009]
s.recent[s.recent$year==2010 ,"rain.avg"] <- s.recent$rain.avg.2010[s.recent$year==2010]
s.recent[s.recent$year==2011 ,"rain.avg"] <- s.recent$rain.avg.2011[s.recent$year==2011]
s.recent[s.recent$year==2012 ,"rain.avg"] <- s.recent$rain.avg.2012[s.recent$year==2012]
s.recent[s.recent$year==2013 ,"rain.avg"] <- s.recent$rain.avg.2013[s.recent$year==2013]
s.recent[s.recent$year==2014 ,"rain.avg"] <- s.recent$rain.avg.2014[s.recent$year==2014]
s.recent[s.recent$year==2015 ,"rain.avg"] <- s.recent$rain.avg.2015[s.recent$year==2015]
s.recent[s.recent$year==2016 ,"rain.avg"] <- s.recent$rain.avg.2016[s.recent$year==2016]
s.recent[s.recent$year==2017 ,"rain.avg"] <- s.recent$rain.avg.2017[s.recent$year==2017]
s.recent[s.recent$year==2018 ,"rain.avg"] <- s.recent$rain.avg.2018[s.recent$year==2018]
s.recent[s.recent$year==2019 ,"rain.avg"] <- s.recent$rain.avg.2019[s.recent$year==2019]
s.recent[s.recent$year==2020 ,"rain.avg"] <- s.recent$rain.avg.2020[s.recent$year==2020]

s.recent[,"rain.std"] <- 0
s.recent[s.recent$year==2001 ,"rain.std"] <- s.recent$rain.std.2001[s.recent$year==2001]
s.recent[s.recent$year==2002 ,"rain.std"] <- s.recent$rain.std.2002[s.recent$year==2002]
s.recent[s.recent$year==2003 ,"rain.std"] <- s.recent$rain.std.2003[s.recent$year==2003]
s.recent[s.recent$year==2004 ,"rain.std"] <- s.recent$rain.std.2004[s.recent$year==2004]
s.recent[s.recent$year==2005 ,"rain.std"] <- s.recent$rain.std.2005[s.recent$year==2005]
s.recent[s.recent$year==2006 ,"rain.std"] <- s.recent$rain.std.2006[s.recent$year==2006]
s.recent[s.recent$year==2007 ,"rain.std"] <- s.recent$rain.std.2007[s.recent$year==2007]
s.recent[s.recent$year==2008 ,"rain.std"] <- s.recent$rain.std.2008[s.recent$year==2008]
s.recent[s.recent$year==2009 ,"rain.std"] <- s.recent$rain.std.2009[s.recent$year==2009]
s.recent[s.recent$year==2010 ,"rain.std"] <- s.recent$rain.std.2010[s.recent$year==2010]
s.recent[s.recent$year==2011 ,"rain.std"] <- s.recent$rain.std.2011[s.recent$year==2011]
s.recent[s.recent$year==2012 ,"rain.std"] <- s.recent$rain.std.2012[s.recent$year==2012]
s.recent[s.recent$year==2013 ,"rain.std"] <- s.recent$rain.std.2013[s.recent$year==2013]
s.recent[s.recent$year==2014 ,"rain.std"] <- s.recent$rain.std.2014[s.recent$year==2014]
s.recent[s.recent$year==2015 ,"rain.std"] <- s.recent$rain.std.2015[s.recent$year==2015]
s.recent[s.recent$year==2016 ,"rain.std"] <- s.recent$rain.std.2016[s.recent$year==2016]
s.recent[s.recent$year==2017 ,"rain.std"] <- s.recent$rain.std.2017[s.recent$year==2017]
s.recent[s.recent$year==2018 ,"rain.std"] <- s.recent$rain.std.2018[s.recent$year==2018]
s.recent[s.recent$year==2019 ,"rain.std"] <- s.recent$rain.std.2019[s.recent$year==2019]
s.recent[s.recent$year==2020 ,"rain.std"] <- s.recent$rain.std.2020[s.recent$year==2020]

s.recent[,"rain.cv"] <- 0
s.recent[s.recent$year==2001 ,"rain.cv"] <- s.recent$rain.cv.2001[s.recent$year==2001]
s.recent[s.recent$year==2002 ,"rain.cv"] <- s.recent$rain.cv.2002[s.recent$year==2002]
s.recent[s.recent$year==2003 ,"rain.cv"] <- s.recent$rain.cv.2003[s.recent$year==2003]
s.recent[s.recent$year==2004 ,"rain.cv"] <- s.recent$rain.cv.2004[s.recent$year==2004]
s.recent[s.recent$year==2005 ,"rain.cv"] <- s.recent$rain.cv.2005[s.recent$year==2005]
s.recent[s.recent$year==2006 ,"rain.cv"] <- s.recent$rain.cv.2006[s.recent$year==2006]
s.recent[s.recent$year==2007 ,"rain.cv"] <- s.recent$rain.cv.2007[s.recent$year==2007]
s.recent[s.recent$year==2008 ,"rain.cv"] <- s.recent$rain.cv.2008[s.recent$year==2008]
s.recent[s.recent$year==2009 ,"rain.cv"] <- s.recent$rain.cv.2009[s.recent$year==2009]
s.recent[s.recent$year==2010 ,"rain.cv"] <- s.recent$rain.cv.2010[s.recent$year==2010]
s.recent[s.recent$year==2011 ,"rain.cv"] <- s.recent$rain.cv.2011[s.recent$year==2011]
s.recent[s.recent$year==2012 ,"rain.cv"] <- s.recent$rain.cv.2012[s.recent$year==2012]
s.recent[s.recent$year==2013 ,"rain.cv"] <- s.recent$rain.cv.2013[s.recent$year==2013]
s.recent[s.recent$year==2014 ,"rain.cv"] <- s.recent$rain.cv.2014[s.recent$year==2014]
s.recent[s.recent$year==2015 ,"rain.cv"] <- s.recent$rain.cv.2015[s.recent$year==2015]
s.recent[s.recent$year==2016 ,"rain.cv"] <- s.recent$rain.cv.2016[s.recent$year==2016]
s.recent[s.recent$year==2017 ,"rain.cv"] <- s.recent$rain.cv.2017[s.recent$year==2017]
s.recent[s.recent$year==2018 ,"rain.cv"] <- s.recent$rain.cv.2018[s.recent$year==2018]
s.recent[s.recent$year==2019 ,"rain.cv"] <- s.recent$rain.cv.2019[s.recent$year==2019]
s.recent[s.recent$year==2020 ,"rain.cv"] <- s.recent$rain.cv.2020[s.recent$year==2020]

#### predict yields ####

# first simple model
maizeyield.lm <- lm(yield ~ N_fertilizer + P_fertilizer + K_fertilizer + OC + pH + sand + clay + log(rain.sum) + rain.cv, data=s.recent)
summary(maizeyield.lm)
  # not terrible; 

# random forest model 
library(randomForest)

mypredictors <- c("N_fertilizer", "P_fertilizer", "K_fertilizer", "OC", "pH", "sand", "clay", "rain.sum", "rain.cv")
mydepvar <- c("yield") 
myvars <- append(mydepvar, mypredictors)
names(s.recent)

df <- as.data.frame(s.recent)
df <- df[complete.cases(df[, myvars]), myvars]

# tune the forest
trf <- tuneRF(x=df[, -1], y=df[,1])
trf
mt <- trf[which.min(trf[,2]),1]
mt

# fit the model
crf <- randomForest(x=df[, -1], y=df[,1],
                    mtry=mt,importance = TRUE)
crf
plot(crf)
importance(crf)
varImpPlot(crf)

# predict using raster stack
# note: we must set the rainfall variables equal to a given year
rain.sum <- stack["rain.sum.2020"]
names(rain.sum) <- c("rain.sum")
rain.cv  <- stack["rain.cv.2020"]
names(rain.cv) <- c("rain.cv")

newstack <- c(stack, rain.sum, rain.cv)

pr <- predict(newstack, crf, const=data.frame(N_fertilizer=100, P_fertilizer=50, K_fertilizer=15), na.rm=TRUE)

plot(pr)
#plot(lakes, col="Turquoise", border = NA, add=TRUE)
plot(adm0, border="Grey", add=TRUE)
plot(s, pch=20, col="Red", add=TRUE)

# this is the predicted value 
writeRaster(pr, "maizeyld_rf_pred.tif", overwrite=TRUE)

#### estimate variability in predicted value coming from rainfall uncertainty ####

set.seed(1492)

# list of rainfall seasonal totals 
tlist <- c("rain.sum.2001", "rain.sum.2002", "rain.sum.2003", "rain.sum.2004", "rain.sum.2005", "rain.sum.2006", "rain.sum.2007", "rain.sum.2008", "rain.sum.2009", "rain.sum.2010", "rain.sum.2011", "rain.sum.2012", "rain.sum.2013", "rain.sum.2014", "rain.sum.2015", "rain.sum.2016", "rain.sum.2017", "rain.sum.2018", "rain.sum.2019", "rain.sum.2020")
# list of rainfall seasonal dekadal CVs 
clist <- c("rain.cv.2001", "rain.cv.2002", "rain.cv.2003", "rain.cv.2004", "rain.cv.2005", "rain.cv.2006", "rain.cv.2007", "rain.cv.2008", "rain.cv.2009", "rain.cv.2010", "rain.cv.2011", "rain.cv.2012", "rain.cv.2013", "rain.cv.2014", "rain.cv.2015", "rain.cv.2016", "rain.cv.2017", "rain.cv.2018", "rain.cv.2019", "rain.cv.2020")
# probabilities for each year
# defined such that oldest year is half as likely to be chosen as most recent year, and where all probabilities sum to 1
# see C:\DATA\Nigeria\EiA\rainfall_year_probabillities.xlsx
plist <- c(0.033, 0.035, 0.037, 0.039, 0.040, 0.042, 0.044, 0.046, 0.047, 0.049, 0.051, 0.053, 0.054, 0.056, 0.058, 0.060, 0.061, 0.063, 0.065, 0.067)

# test this 
sample(tlist, 100, replace=TRUE, prob=plist)

rm(pr2)
pr2 <- pr
for (i in 2:300) {
  print(paste("Iteration:", i))  
  # predict using raster stack
  # note: we set the rainfall variables with random draws from last 20 years, as defined in the vectors above
  
  tchoice <-  sample(tlist, 1, replace=TRUE, prob=plist)
  cchoice <-  clist[which(tchoice==tlist)]
  
  print(tchoice)
  print(cchoice)
  
  rain.sum <- stack[tchoice]
  names(rain.sum) <- c("rain.sum")
  rain.cv  <- stack[cchoice]
  names(rain.cv) <- c("rain.cv")
  
  # replace newstack for each iteration 
  newstack <- c(stack, rain.sum, rain.cv)
  # predict and send each prediction as new layer of output stack
  add(pr2) <- predict(newstack, crf, const=data.frame(N_fertilizer=100, P_fertilizer=50, K_fertilizer=15), na.rm=TRUE)
}

summary(pr2)

# calculate average and variance/standard deviation of predictions
pr.avg = app(pr2, fun=mean) 
pr.var = app(pr2, fun=var)
  names(pr.var) <- "var"
pr.sd = app(pr2, fun=sd)
pr.cv = pr.sd/pr.avg
  names(pr.cv) <- "CV"

plot(c(pr.avg, pr.var))
plot(c(pr.avg, pr.cv))

writeRaster(pr2, "tmp_pr2_first10_100N_50P_15K_seed1492.tif")

