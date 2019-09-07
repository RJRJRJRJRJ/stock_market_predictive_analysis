# Monthly to Weekly
# Load DAta
mydata <- read.csv("MONTHLY_INDICATORS.csv")

# Load Library
library(zoo)

# Data Subset
mydata1 <- mydata[,1:2]
mydata2 <- mydata[,3:5]
mydata3 <- mydata[,6:8]
mydata4 <- mydata[,9:11]
mydata5 <- mydata[,12:17]
mydata6 <- mydata[,18:27]

# Quarter in year-month-day format
mydata1$qvar = as.Date(mydata1$NEW.DATE,format = "%m/%d/%Y")
mydata2$qvar = as.Date(mydata2$NEW.DATES,format = "%m/%d/%Y")
mydata3$qvar = as.Date(mydata3$NEW.DATES.1,format = "%m/%d/%Y")
mydata4$qvar = as.Date(mydata4$NEW.DATE.1,format = "%m/%d/%Y")
mydata5$qvar = as.Date(mydata5$NEW.DATES.2,format = "%m/%d/%Y")
mydata6$qvar = as.Date(mydata6$Lag_1_Month,format = "%m/%d/%Y")

# Creating a daily sequence for the quarterly range
Daily1 = seq(mydata1$qvar[1], tail(mydata1$qvar,1), by="day")
Daily2 = seq(mydata2$qvar[1], tail(mydata1$qvar,1), by="day")
Daily3 = seq(mydata3$qvar[1], tail(mydata1$qvar,1), by="day")
Daily4 = seq(mydata4$qvar[1], tail(mydata1$qvar,1), by="day")
Daily5 = seq(mydata5$qvar[1], tail(mydata1$qvar,1), by="day")
Daily6 = seq(mydata6$qvar[1], tail(mydata1$qvar,1), by="day")


# Cubic interpolation using spline()
Survey.OECD = data.frame(qvar=Daily1, Survey.OECD=spline(mydata1[c("qvar","COS_OECD")], method="fmm", xout=Daily1)$y)
Exports = data.frame(qvar=Daily2, Exports=spline(mydata2[c("qvar","EXPORTS")], method="fmm", xout=Daily2)$y)
Imports = data.frame(qvar=Daily2, Imports=spline(mydata2[c("qvar","IMPORTS")], method="fmm", xout=Daily2)$y)
PMI.Manu = data.frame(qvar=Daily3, PMI.Manu=spline(mydata3[c("qvar","M_PMI")], method="fmm", xout=Daily3)$y)
PMI.NonManu = data.frame(qvar=Daily3, PMI.NonManu=spline(mydata3[c("qvar","NONMAN_PMI")], method="fmm", xout=Daily3)$y)
M0 = data.frame(qvar=Daily4, M0=spline(mydata4[c("qvar","M0")], method="fmm", xout=Daily4)$y)
M1 = data.frame(qvar=Daily4, M0=spline(mydata4[c("qvar","M1")], method="fmm", xout=Daily4)$y)
Policy.Uncertainty = data.frame(qvar=Daily5, Policy.Uncertainty=spline(mydata5[c("qvar","CHIEPUINDXM")], method="fmm", xout=Daily5)$y)
Manufact.CI = data.frame(qvar=Daily5, Manufact.CI=spline(mydata5[c("qvar","MANU_CI")], method="fmm", xout=Daily5)$y)
Manufact.Ex = data.frame(qvar=Daily5, Manufact.Ex=spline(mydata5[c("qvar","EXCH_MANUCPI")], method="fmm", xout=Daily5)$y)
Building.Sale = data.frame(qvar=Daily5, Building.Sale=spline(mydata5[c("qvar","TSALE_AMOUNT")], method="fmm", xout=Daily5)$y)
Building.Sale.Rate = data.frame(qvar=Daily5, Building.Sale.Rate=spline(mydata5[c("qvar","TSALE_RATE")], method="fmm", xout=Daily5)$y)
CPI = data.frame(qvar=Daily6, CPI=spline(mydata6[c("qvar","CPI")], method="fmm", xout=Daily6)$y)
CPI.Residence = data.frame(qvar=Daily6, CPI.Residence=spline(mydata6[c("qvar","CPI_RESIDENCE")], method="fmm", xout=Daily6)$y)
CPI.House = data.frame(qvar=Daily6, CPI.House=spline(mydata6[c("qvar","CPI_HOUSEHOLD")], method="fmm", xout=Daily6)$y)
CPI.Transportation = data.frame(qvar=Daily6, CPI.Transportation=spline(mydata6[c("qvar","CPI_TRANSPORTATION")], method="fmm", xout=Daily6)$y)
CPI.Education = data.frame(qvar=Daily6, CPI.Education=spline(mydata6[c("qvar","CPI_EDUCATION")], method="fmm", xout=Daily6)$y)
CPI.Health = data.frame(qvar=Daily6, CPI.Health=spline(mydata6[c("qvar","CPI_HEALTH")], method="fmm", xout=Daily6)$y)

# Load DAta
weekly <- read.csv("Weekly_Indicator_Duplicate2.csv")
Date <- as.Date(weekly$Date,format = "%m/%d/%Y")

New.Weekly <- data.frame(Date)
New.Weekly$Survey.OECD <- Survey.OECD$Survey.OECD[match(New.Weekly$Date,Survey.OECD$qvar)]
New.Weekly$Export <- Exports$Exports[match(New.Weekly$Date,Exports$qvar)]
New.Weekly$Import <- Imports$Imports[match(New.Weekly$Date,Imports$qvar)]
New.Weekly$PMI.Manu <- PMI.Manu$PMI.Manu[match(New.Weekly$Date,PMI.Manu$qvar)]
New.Weekly$PMI.NonManu <- PMI.NonManu$PMI.NonManu[match(New.Weekly$Date,PMI.NonManu$qvar)]
New.Weekly$M0 <- M0$M0[match(New.Weekly$Date,M0$qvar)]
New.Weekly$M1 <- M1$M0[match(New.Weekly$Date,M1$qvar)]
New.Weekly$Policy.Uncertainty <- Policy.Uncertainty$Policy.Uncertainty[match(New.Weekly$Date,Policy.Uncertainty$qvar)]
New.Weekly$Manufact.CI <- Manufact.CI$Manufact.CI[match(New.Weekly$Date,Manufact.CI$qvar)]
New.Weekly$Manufact.Ex <- Manufact.Ex$Manufact.Ex[match(New.Weekly$Date,Manufact.Ex$qvar)]
New.Weekly$Building.Sale <- Building.Sale$Building.Sale[match(New.Weekly$Date,Building.Sale$qvar)]
New.Weekly$Building.Sale.Rate <- Building.Sale.Rate$Building.Sale.Rate[match(New.Weekly$Date,Building.Sale.Rate$qvar)]
New.Weekly$CPI <- CPI$CPI[match(New.Weekly$Date,CPI$qvar)]
New.Weekly$CPI.Residence <- CPI.Residence$CPI.Residence[match(New.Weekly$Date,CPI.Residence$qvar)]
New.Weekly$CPI.Transportation <- CPI.Transportation$CPI.Transportation[match(New.Weekly$Date,CPI.Transportation$qvar)]
New.Weekly$CPI.Household <- CPI.House$CPI.House[match(New.Weekly$Date,CPI.House$qvar)]
New.Weekly$CPI.Health <- CPI.Health$CPI.Health[match(New.Weekly$Date,CPI.Health$qvar)]
New.Weekly$CPI.Education <- CPI.Education$CPI.Education[match(New.Weekly$Date,CPI.Education$qvar)]

write.csv(New.Weekly,"Weekly_Indicator_Spline.csv", row.names = FALSE)
