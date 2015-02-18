## ----chunk0 --------------
# ls(pos = "package:bmBiostat")
# data(package = bmBiostat)

## ----HEARTdata --------------
require(survival, quietly = TRUE)
data(heart)
head(heart)

## ----HIVdata --------------
hiv <- read.table(
  "http://www.ats.ucla.edu/stat/R/examples/asa/hmohiv.csv",
  sep=",", header = TRUE)

head(hiv)


## ----SampleHIV

# as.Date(hiv$enddate,format='%m/%d/%Y')
hiv$enddate <- as.character(hiv$enddate)
hiv$entdate <- as.character(hiv$entdate)

#Select a sample of the subjects
set.seed(4)
hiv_s <- hiv[sample(1:dim(hiv)[1], 6),]
Ltime <- cal.yr( hiv_s, format="%m/%d/%Y", wh=6:7 )


## ----Lexis1
Lex <- Lexis( entry = list( year=entdate ),
               exit = list( year=enddate),
               exit.status = censor,
               data = Ltime )

plot(Lex, type="b",  pch=c(NA,16)[Lex$censor+1], col=c("red", "blue")[Lex$drug+1])
# plot(Lex, type="b",  pch=c(NA,16)[Lex$censor+1])
abline(v=sort(Lex[Lex$censor==1,]$enddate)[1])
abline(v=sort(Lex[Lex$censor==1,]$enddate)[2])
abline(v=sort(Lex[Lex$censor==1,]$enddate)[3])
abline(v=sort(Lex[Lex$censor==1,]$enddate)[4])

## ----Lexis2
# Change origin to entdate
Lex <- Lexis( exit = list( year=enddate-entdate),
              exit.status = censor,
              data = Ltime )

plot(Lex, type="b",  pch=c(NA,16)[Lex$censor+1], col=c("red", "blue")[Lex$drug+1])
abline(v=sort(Lex[Lex$censor==1,]$enddate - Lex[Lex$censor==1,]$entdate)[1])
abline(v=sort(Lex[Lex$censor==1,]$enddate - Lex[Lex$censor==1,]$entdate)[2])
abline(v=sort(Lex[Lex$censor==1,]$enddate - Lex[Lex$censor==1,]$entdate)[3])
abline(v=sort(Lex[Lex$censor==1,]$enddate - Lex[Lex$censor==1,]$entdate)[4])
abline(v=sort(Lex[Lex$censor==1,]$enddate - Lex[Lex$censor==1,]$entdate))
