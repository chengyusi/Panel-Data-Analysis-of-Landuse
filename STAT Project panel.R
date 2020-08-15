### STAT PROJECT SUMMER 2019
# Panel Data Analysis

rm(list = ls())

# import data
library(haven)
newdata <- read_dta("C:/Users/scy_a/Dropbox/round3/data/newdata.dta")

newdata <- read_dta("C:/Users/czs0066/Dropbox/summer 2019/newdata.dta")

# newdata0<-newdata[which(crps <1),] 
# newdata1<-newdata0[which(crps >0),] 
# Set data as panel data  # plm.data
library(plm)
pdata <- pdata.frame(newdata, index=c("id","time"))
pdim(pdata)$balanced # check if balanced: FALSE 

# Set as Balanced
bdata <- make.pbalanced(pdata) 
pdim(bdata)$balanced # check if balanced: TRUE 
attach(bdata)


# generate variables
crps <- cropland/landarea
asub <- sub/cropland
antic <- netincome/farmland
acashrent <- cashrent/farmland
albexp <- lbexpense/farmland
afert <- fertilizer/farmland
ache <- chemical/farmland

Y <- cbind(crps)
X <- cbind(asub,antic,acashrent,albexp,afert,ache)



# Descriptive statistics
summary(Y)
summary(X)

# box plot
library(npmv)
nonpartest(crps | asub | antic | acashrent | albexp | afert | ache ~ year, data = bdata,permreps = 1000)


# log
lasub<- log(asub)
lantic<- log(antic)
lacashrent<- log(acashrent)
lalbexp<- log(albexp)
lafert<- log(afert)
lache<- log(ache)








# Pooled OLS estimator
pooling <- plm(crps ~ asub+antic+acashrent+albexp+afert+ache, data=bdata, model= "pooling")
summary(pooling)
plot(pooling$residuals)

# Between estimator
between <- plm(crps ~ asub+antic+acashrent+albexp+afert+ache, data=bdata, model= "between")
summary(between)

# First differences estimator
firstdiff <- plm(crps ~ asub+antic+acashrent+albexp+afert+ache, data=bdata, model= "fd")
summary(firstdiff)

# Fixed effects or within estimator
fixed <- plm(crps ~ asub+antic+acashrent+albexp+afert+ache, data=bdata, model= "within")
summary(fixed)

# Random effects estimator
random <- plm(crps ~ asub+antic+acashrent+albexp+afert+ache, data=bdata, model= "random")
summary(random)

# LM test for random effects versus OLS
plmtest(pooling)

# LM test for fixed effects versus OLS
pFtest(fixed, pooling)

# Hausman test for fixed versus random effects model
phtest(random, fixed)




# library(frmpd)
# frmpd(id,time,Y,X,type="GMMbgw")



# with lags
# Fixed effects or within estimator
fixed_lag <- plm(crps ~ lag(asub,-1)+lag(antic,-1)+lag(acashrent,-1)+lag(albexp,-1)+lag(afert,-1)+lag(ache,-1), data=bdata, model= "within")
summary(fixed_lag)

# Random effects estimator
random_lag <- plm(crps ~ lag(asub,-1)+lag(antic,-1)+lag(acashrent,-1)+lag(albexp,-1)+lag(afert,-1)+lag(ache,-1), data=bdata, model= "random")
summary(random_lag)
