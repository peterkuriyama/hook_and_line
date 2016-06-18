##
# Hook and Line Analysis

#Startup
setwd('/Users/peterkuriyama/School/Research/hook_and_line/')

source('R/hl_startup.R')

#----------------------------------------------------------------------------------------------------
#Mixed Effects Model
library(lme4)
library(TMB)

# dat %>% group_by(Year, SiteName, VesName, DropNum, AngNum) %>% 
#   summarize(lastname = unique(LastName),
#     onbsec = mean(OnBSec, na.rm = TRUE),
#     brsec = mean(BRSec, na.rm = TRUE),
#     fishtime = mean(FishTime, na.rm = TRUE),
#     fbsec = mean(FBSec, na.rm = TRUE),
#     droptime = mean(DropTime, na.rm = TRUE),
#     lat = mean(Lat.DD),
#     lon = mean(Lon.DD),
#     depth = mean(Depth.m, na.rm = TRUE),
#     avg_weight = mean(Weight.kg, na.rm = TRUE),
#     avg_length = mean(Length.cm, na.rm = TRUE), 
#     wave_height = mean(WaveHt.m, na.rm = TRUE),
#     num_boc = sum(NumBoc), 
#     num_fish = sum(SurvFish),
#     num_hook = length(HookNum), 
#     cpue_boc = num_boc / num_hook, 
#     cpue_fish = num_fish / num_hook) %>% as.data.frame -> 
# drop_cpue

# drop_cpue %>% group_by(SiteName) %>% summarize(nyears = length(unique(Year))) %>% 
#     as.data.frame()

#--------------------------------------------------------------------------------------
#Do just one year and one vessel

# test <- subset(drop_cpue, SiteName == "2" )
# test$Year <- as.factor(test$Year)
# test <- test[-which(is.na(test$onbsec)), ]
# test <- subset(test, )

# glmRes <- glm(cpue_boc ~ onbsec, family = binomial, weights = num_hook, data = test)

# drop_cpue$SiteName <- as.factor(drop_cpue$SiteName)
# drop_cpue$Year <- as.factor(drop_cpue$Year)

dat$SiteName <- as.factor(dat$SiteName)
dat$Year <- as.factor(dat$Year)

glmYr <- glm(NumBoc ~ Year - 1, family = binomial, data = dat)
glmYrS <- glm(NumBoc ~ Year - 1 + SiteName, family = binomial, data = dat)

coefs <- coef(glmYrS)

#--------------------------------------------------------------------------------------
#Add in Design Matrix with TMB

#--------------------
#Format Data entry

#Check format of data, make sure they're numeric
dat$Year <- as.numeric(as.character(dat$Year))
dat$SiteName <- as.numeric(as.character(dat$SiteName))

#Format year design matrix
years <- dat$Year
dmat <- matrix(nrow = length(years), ncol = length(unique(years)))
yrz <- unique(years)

for(zz in 1:length(yrz)){    
    dmat[which(years == yrz[zz]), zz] <- 1
    dmat[which(years != yrz[zz]), zz] <- 0
}

#Do the same for sites
sites <- dat$SiteName
dmat2 <- matrix(nrow = length(sites), ncol = length(unique(sites)))
stz <- unique(sites)

for(zz in 1:length(stz)){
    dmat2[which(sites == stz[zz]), zz] <- 1
    dmat2[which(sites != stz[zz]), zz] <- 0
}

compile("hl_glm.cpp") 

# Data <- list("X_ij" = cbind(rep(1, nrow(dmat)), dmat, dmat2),
Data <- list("X_ij" = cbind(dmat, dmat2),
             "num_boc" = dat$NumBoc,
             "num_hooks" = rep(1, length(dat$NumBoc)),
             "Options_z" = 0)

Parameters <- list("b_j" = rep(0, ncol(Data$X_ij)), "theta_z" = .5)
# Parameters <- list("b_j" = rep(0, ncol(Data$X_ij)))
dyn.load(dynlib("hl_glm"))

Obj <- MakeADFun(data = Data, parameters = Parameters)
Opt <- nlminb(start = Obj$par, objective = Obj$fn, gradient = Obj$gr)

Report <- Obj$report()


#
#Check AIC Calculation
2 * length(coef(glmYrS)) - 2 * as.numeric(logLik(glmYrS))
AIC(glmYrS)



2 * (length(Report$b_j) + length(Report$theta_z)) + 2 * Report$jnll
Obj$fn(Obj$par)





dyn.load(dynlib("hl_glm"))

#Run again using estimates as 
Parameters <- list("b_j" = Report$b_j)
dyn.load(dynlib("hl_glm"))
Obj <- MakeADFun(data = Data, parameters = Parameters)

Opt <- nlminb(start = Obj$par, objective = Obj$fn, gradient = Obj$gr)
Report <- Obj$report()

Obj$fn(Obj$par)
Obj$gr(Obj$par)
#format parameters into readable thing
cbind(c("slope", unique(drop_cpue$Year), unique(drop_cpue$SiteName)), Opt$par)
  

dmultinom(c(1, 1, 1, 1, 1), prob = c(.1, .2, .3, .4, .5))

logLik(glmYrS)
#---------------------------------------------------------------------------------
#Check the glm and tmb glm
# Obj$fn(Obj$par)
# Obj$gr(Obj$par)
# Obj$gr(Report$b_j)
# Map <- NULL
# Map <- list("b_j" = rep(NA, length(coefs)))
# Map[["b_j"]] <- factor(Map[["b_j"]])

-logLik(glmYrS)
Report$jnll

coefs
Report$b_j

min(predict(glmYrS) - Report$linpred_i)

round(unique(predict(glmYrS)), digits = 3) == round(unique(Report$linpred_i), digits = 3)


#Look at the results of glmYr
#Reconstruct the 


SD <- sdreport(Obj)



logLik(glmYr)
coefs <- coef(glmYr)
names(coefs) <- NULL

#Compare Paraeters
Report$b_j
coefs




logLik(glmYr)
Report$jnll

Obj$fn(coefs)




logLik(glmYr)
Report$jnll


#Compare coefficients
comp_coefs <- data.frame("from_glm" = coef(glmYr), "from_tmb" = c(Report$b_j[1], Report$b_j[2:12],
    Report$b_j[13] ))


coef(glmYr)
Report$b_j






#--------------------------------------------------------------------------------------
#Fitted values 
preds <- predict(glmRes) #linear predictions in logit space

#Fitted values which are in normal space
#Two function calls are equivalent
exp(preds) / (1 + exp(preds))
fits <- fitted(glmRes)

#Now code this myself to make sure I understand it. 
cpue <- test$cpue_boc
expl <- test$onbsec
interc <- coef(glmRes)[1]
slope <- coef(glmRes)[2]


my_glm <- function(interc, slope, cpue = test$num_boc, expl = test$onbsec,
    nhooks = test$num_hook){
    pred <- interc + slope * expl
    probs <- exp(pred) / (1 + exp(pred))

    likes <- dbinom(x = cpue , size = nhooks, prob = probs, log = FALSE)

    nll <- sum(-log(likes), na.rm = TRUE)
    cat("inter =", interc, "slope =", slope, "\n")
    return(nll)
}

my_glm_pred <- function(interc, slope, cpue = test$num_boc, expl = test$onbsec,
    nhooks = test$num_hook){
    pred <- interc + slope * expl
    probs <- exp(pred) / (1 + exp(pred))

    likes <- dbinom(x = cpue , size = nhooks, prob = probs, log = FALSE)

    nll <- sum(-log(likes), na.rm = TRUE)
    cat("inter =", interc, "slope =", slope, "\n")
    return(list("nll" = nll, "preds" = pred, "probs" = probs))
}

###Optimize now
ests <- mle(my_glm, start = list(interc = 0, slope = 0), fixed = list(cpue = test$num_boc,
            expl = test$onbsec, nhooks = test$num_hook), method = "Nelder-Mead")

#run again
estsCoefs <- coef(ests)
names(estsCoefs) <- NULL

ests2 <- mle(my_glm, start = list(interc = estsCoefs[1], slope = estsCoefs[2]), 
                     fixed = list(cpue = test$num_boc, expl = test$onbsec, 
                        nhooks = test$num_hook), method = "Nelder-Mead")

#Compare answers
coef(glmRes)
coef(ests)[1:2]
coef(ests2)[1:2]




#Check that all my models have the same likelihood
ests2

#with my likelihood
my_glm(coef(glmRes)[1], coef(glmRes)[2])

#Compare PRedictions and probabilites
my_glm_pred(coef(glmRes)[1], coef(glmRes)[2])
Report

coef(glmRes)[1] + (onbsec * coef(glmRes)[2])
coef(glmRes)[2]

preds <- vector(length = length(test$onbsec))
for(ii in 1:length(test$onbsec)){
    preds[ii] <- coef(glmRes)[1] + (test$onbsec[ii] * coef(glmRes)[2])
}

exp(-52.62557) / (1 + exp(-52.62557))


-logLik(ests)

Obj$fn(Obj$par)


#--------------------------------------------------------------------------------------
#Do same thing with categorical and continuous explanatory variables

ff <- log(Volume) ~ log(Height) + log(Girth)
utils::str(m <- model.frame(ff, trees))
mat <- model.matrix(ff, m)

xx <- num_boc ~ Year + brsec
utils::str(yy <- model.frame(xx, drop_cpue))
mat <- model.matrix(xx, yy)

glmYr <- glm(cpue_boc ~ Year + onbsec, family = binomial, weights = num_hook, data = test)
glmYr2 <- glm(cpue_boc ~ onbsec + Year, family = binomial, weights = num_hook, data = test)

glmRes <- glm(cpue_boc ~ onbsec, family = binomial, weights = num_hook, data = test)

#Create design matrix based on years used
dmat <- 

cpue = test$num_boc
expl = test$onbsec
nhooks = test$num_hook
years = test$Year


y1 = .1; y2 = .1; y3 = .1; y4 = .1; y5 = .1; 
y6 = .1; y7 = .1; y8 = .1; y9 = .1; y10 = .1; y11 = .1;

interc <- -.1
slope <- .1

cat_glm <- function(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11,
    interc, slope, cpue = test$num_boc, expl = test$onbsec, 
    nhooks = test$num_hook, years = test$Year){

    #Specify design matrix
    years <- as.integer(years)
    dmat <- matrix(nrow = length(years), ncol = length(unique(years)))

    for(zz in 1:length(unique(years))){    
        dmat[which(years == zz), zz] <- 1
        dmat[which(years != zz), zz] <- 0
    }

    #other parameters
    # others <- data.frame('int' = rep(1, length = nrow(dmat)),
    #     'slope' = expl)
    # dmat <- cbind(dmat) #full design matrix

    # pars <- c(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, interc, slope)


    preds <- y1 * dmat[, 1] + y2 * dmat[, 2] + y3 * dmat[, 3] + y4 * dmat[, 4] + y5 * dmat[, 5] + 
             y6 * dmat[, 6] + y7 * dmat[, 7] + y8 * dmat[, 8] + y9 * dmat[, 9] + y10 * dmat[, 10] + y11 * dmat[, 11] + 
            interc + expl * slope
    preds

    # preds <- rowSums(dmat * pars)

    probs <- exp(preds) / (1 + exp(preds))

    likes <- dbinom(x = cpue, size = nhooks, prob = probs, log = FALSE)

    nll <- sum(-log(likes), na.rm = TRUE)
    cat("year_pars=", c(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11), "interc =", interc, 
        "slope =", slope, '\n')
    return(nll)

}
coefsYr <- coef(glmYr)

cat_glm(y1 = 0, y2 = coefsYr[2], y3 = coefsYr[3], y4 = coefsYr[4], y5 = coefsYr[5], 
    y6 = coefsYr[6], y7 = coefsYr[7], y8 = coefsYr[8], y9 = coefsYr[9], y10 = coefsYr[10], y11 = coefsYr[11], 
                                     interc = coefsYr[1], slope = coefsYr[12])


cat_res <- mle(cat_glm, start = list(y2 = .0345, y3 = 1.14, y4 = 2.34, y5 = 0.63, y6 = -0.69, 
                                     y7 = -16.5, y8 = -17.1509, y9 = 2.2819, y10 = 2.0177, y11 = 2.068, 
                                     interc = -.24, slope = 0.048), 
                        fixed = list(y1 = 0, cpue = test$num_boc,
                                     expl = test$onbsec, 
                                     nhooks = test$num_hook), 
                        method = "Nelder-Mead")


















