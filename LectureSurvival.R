## ----HIVdata --------------
# ls(pos = "package:bmBiostat")
# data(package = bmBiostat)


## ----HIVdata --------------
hiv <- read.table(
  "http://www.ats.ucla.edu/stat/R/examples/asa/hmohiv.csv",
  sep=",", header = TRUE)

head(hiv)

## ----P2

b <- c("A", "B", "C")
b
data(cars)
cars
a<-lm(speed~dist, cars)

## ----Lexis
hiv_L <- cal.yr( hiv, format="%m/%d/%Y", wh=6:7 )

Lcoh <- Lexis( entry = list( per=as.numeric(entdate) ),
               exit = list( per=as.numeric(enddate)),
               exit.status = censor,
               data = hiv )