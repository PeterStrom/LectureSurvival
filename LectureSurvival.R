## ----chunk0 --------------
# ls(pos = "package:bmBiostat")
require(Epi, quietly = TRUE)
require(survival, quietly = TRUE)
# require(tikzDevice, quietly = TRUE)
# require(bmBiostat, quietly = TRUE)


## ----HEARTdata --------------
data(heart)
head(heart)

## ----HIVdata --------------
hiv <- read.table(
  "http://www.ats.ucla.edu/stat/R/examples/asa/hmohiv.csv",
  sep=",", header = TRUE)

## ----HIVdata_show --------------
hiv <- read.table(
  "http://www.ats.ucla.edu/stat/R/examples/asa/hmohiv.csv",
  sep=",", header = TRUE)

head(hiv)

## ----HIVdataShort --------------
print(hiv[6:1, c("ID", "time", "drug", "censor")], row.names=FALSE)


## ----SampleHIV
# as.Date(hiv$enddate,format='%m/%d/%Y')
hiv$enddate <- as.character(hiv$enddate)
hiv$entdate <- as.character(hiv$entdate)

# Select a sample of the subjects
# Lexis over time since enter study
# Lex1 - No censoring
# Lex2 - Censoring
set.seed(4)
# hiv_s <- hiv[sample(1:dim(hiv)[1], 6),]
hiv_nc <- head(hiv[hiv$censor==1,])
Ltime <- cal.yr( hiv_nc, format="%m/%d/%Y", wh=6:7 )
Lex1 <- Lexis( duration = list( months=time),
              exit.status = censor,
              data = Ltime )

hiv_s <- head(hiv)
Ltime <- cal.yr( hiv_s, format="%m/%d/%Y", wh=6:7 )
Lex2 <- Lexis( duration = list( months=time),
              exit.status = censor,
              data = Ltime )

## ----Censor


## ----Lexis0
plot(Lex2, type="b",  pch=c(NA,16)[Lex2$censor+1], col=c("red", "blue")[Lex2$drug+1])
abline(v=sort(Lex2[Lex2$censor==1,]$enddate - Lex2[Lex2$censor==1,]$entdate)[1])
abline(v=sort(Lex2[Lex2$censor==1,]$enddate - Lex2[Lex2$censor==1,]$entdate)[2])
abline(v=sort(Lex2[Lex2$censor==1,]$enddate - Lex2[Lex2$censor==1,]$entdate)[3])
abline(v=sort(Lex2[Lex2$censor==1,]$enddate - Lex2[Lex2$censor==1,]$entdate)[4])
abline(v=sort(Lex2[Lex2$censor==1,]$enddate - Lex2[Lex2$censor==1,]$entdate))

# ## ----Lexis1
# # Lexis over calender period
# Lex <- Lexis( entry = list( year=entdate ),
#               exit = list( year=enddate),
#               exit.status = censor,
#               data = Ltime )
# 
# plot(Lex, type="b",  pch=c(NA,16)[Lex$censor+1], col=c("red", "blue")[Lex$drug+1])
# # plot(Lex, type="b",  pch=c(NA,16)[Lex$censor+1])
# abline(v=sort(Lex[Lex$censor==1,]$enddate)[1])
# abline(v=sort(Lex[Lex$censor==1,]$enddate)[2])
# abline(v=sort(Lex[Lex$censor==1,]$enddate)[3])
# abline(v=sort(Lex[Lex$censor==1,]$enddate)[4])

## ----Surv1
plot(Lex1, type="b", pch=c(NA,16)[Lex1$censor+1], xlab="t (months)", ylab="")
abline(v=5)

## ----Surv2
Lex1$lex.dur[Lex1$time >= 8 ] <- 8
Lex1$censor[Lex1$time >= 8 ] <- 0
plot(Lex1, type="b", pch=c(NA,16)[Lex1$censor+1], xlab="t (months)", ylab="")
abline(v=8, lty=2)
abline(v=5)

## ----Surv3
plot(Lex2, type="b", pch=c(NA,16)[Lex2$censor+1], xlab="t (months)", ylab="")
abline(v=5)

## ----Surv4
plot(survfit(Surv(time=time, event=censor) ~ drug, data=hiv_s), col=c("red", "blue")[hiv_s$drug+1],
     main="Kaplan-Meier for drug=0 (red) and drug=1 (blue)", xlab="Time (months)", ylab="S(t)")

## ----Surv5
plot(survfit(Surv(time=time, event=censor) ~ drug, data=hiv), col=c("red", "blue")[hiv$drug+1],
     main="Kaplan-Meier for drug=0 (red) and drug=1 (blue)", xlab="Time (months)", ylab="S(t)")

## ----LogRank1
survdiff(Surv(time=time, event=censor) ~ drug, data=hiv)

## ----plotHIV
plot(Lex2, type="b",  pch=c(NA,16)[Lex2$censor+1], col=c("red", "blue")[Lex2$drug+1], xlab="")

## ----plotChisq
curve( dchisq(x, df=1), col='red', main = "Chi-Square Density Graph",
       from=0,to=6, ylab="")
abline(v=0.1667)
text(x=0.6,y=1.5,"p=0.8")
abline(v=qchisq(.95, df=1))

## ----LogRankAgeCat
hiv$agecat <- cut(hiv$age, c(min(hiv$age), 29, 34, 39, 
                             max(hiv$age)), include.lowest=T)
survdiff(Surv(time=time, event=censor) ~ agecat, data=hiv)
