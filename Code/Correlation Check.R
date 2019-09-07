###=======================###
###                       ### 
###   Correlation Check   ###
###     Data Combined     ###
###                       ###
###=======================###

# Load Data
Duplicate <- read.csv("Weekly_Indicator_Duplicate2.csv", stringsAsFactors = FALSE)
FadeOut <- read.csv("Weekly_Indicator_FadeOut2.csv", stringsAsFactors = FALSE)
WeekScale <- read.csv("Weekly_Indicator_WeekScale2.csv", stringsAsFactors = FALSE)

# check correlations
correlationMatrix.1 <- cor(Duplicate[,-c(1,26,27)])
# find attributes that are highly corrected (ideally > 0.70)
print(correlationMatrix.1)
# Total.Share.Price, M1, CPI of Health, Manufact.CI delete

correlationMatrix.2 <- cor(FadeOut[,-c(1,26,27)])
# find attributes that are highly corrected (ideally > 0.70)
print(correlationMatrix.2)
# PMI of Manufacture, PMI of Non Manufacture, M1, Manufact.Ex delete
# CPI related indicators only keep CPI

correlationMatrix.3 <- cor(WeekScale[,-c(1,26,27)])
# find attributes that are highly corrected (ideally > 0.70)
print(correlationMatrix.3)
# Total.Share.Price, Policy.Uncertainty, Building.Sale.Rate, CPI, SSE, SSECBI, Exchange.Rate, ETF.Volatility, Bond.YTM keep

