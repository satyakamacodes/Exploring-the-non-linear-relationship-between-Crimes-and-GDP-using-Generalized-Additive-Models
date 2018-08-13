cat("\f")
closeAllConnections()
rm(list = ls())


setwd("C:/Users/satyakama.paul/Documents/R_projects/sa_crime_gdp")

master_data <- read.csv("master_data.csv",
                        header = T,
                        sep = ",",
                        stringsAsFactors = T)

dim(master_data)

str(master_data)

sum(is.na(master_data))

master_data <- subset(master_data,
                      select = -c(PROVINCE,
                                  YR,
                                  OTHER.CRIME.CATEGORIES))

dim(master_data)



master_data <- master_data[complete.cases(master_data),]

names(master_data) <- tolower(names(master_data))

colnames(master_data) <- c("contact",
                           "contact.re",
                           "property.re",
                           "police.ac",
                           "other.serious",
                           "agg.robbery",
                           "GDP")


print(require(car))

fig2 <- scatterplotMatrix(~contact
                  +contact.re
                  +property.re
                  +police.ac
                  +other.serious
                  +agg.robbery
                  +GDP,
                  data = master_data,
                  pch = 16,
                  diagonal = T,
                  col = "black",
                  cex = 0.5)

print(require(caret))

set.seed(1)

inTrain <- createDataPartition(y = master_data$GDP,
                               p = 0.8,
                               list = F)

train.data <- master_data[inTrain,]

dim(train.data)


test.data <- master_data[-inTrain,]

dim(test.data)

print(require(mgcv))


mod_lm2 <- gam(GDP ~ contact 
                         + contact.re
                         + property.re
                         + police.ac
                         + other.serious
                         + agg.robbery, 
               data = train.data)
summary(mod_lm2)


mod_gam2 <- gam(GDP ~ s(contact, bs="cr") 
               + s(contact.re, bs="cr")
               + s(property.re, bs="cr")
               + s(police.ac, bs="cr")
               + s(other.serious, bs="cr")
               + s(agg.robbery, bs="cr"), 
               data = train.data)
summary(mod_gam2)

par(mfrow = c(2,3))
plot(mod_gam2,
     cex.axis = 1,
     cex.lab = 1.7,
     ylab = "GDP")

par(mfrow = c(1,1))

anova(mod_lm2,
      mod_gam2,
      test = "F")

test.data.x <- test.data[,1:6]

pred <- predict.gam(mod_gam2,
                    test.data.x)

par(mfrow = c(1,1))
plot(pred,
     pch = 15,
     type = "b",
     ylim = c(range(pred, test.data$GDP)[1],
              range(pred, test.data$GDP)[2]),
     ylab = "GDP")

par(new = T)
plot(test.data$GDP,
     pch = 16,
     type = "b",
     ylim = c(range(pred, test.data$GDP)[1],
              range(pred, test.data$GDP)[2]),
     ylab = "GDP")

legend("topright", 
       legend=c("predicted GDP", 
                "actual GDP from test data"),
       
       pch = c(15,16),
       cex=0.9)


# Function that returns Root Mean Squared Error
rmse <- function(error)
{
     sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
     mean(abs(error))
}

error <- pred - test.data$GDP

rmse(error)

mae(error)

print(require(visreg))

#contact
#contact.re
#police.ac

print(require(rgl))

visreg2d(mod_gam2, 
         xvar='contact', 
         yvar='contact.re', 
         scale='response')

visreg2d(mod_gam2, 
         xvar='contact', 
         yvar='police.ac', 
         scale='response')

visreg2d(mod_gam2, 
         xvar='contact.re', 
         yvar='police.ac', 
         scale='response')



