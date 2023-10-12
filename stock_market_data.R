# 4.7.1 THE STOCK MARKET DATA
# We will begin by examining some numerical and graphical summaries of the 
# Smarket data, which is part of the ISLR2 library. This data set consistss of
# percentage returns for the S&P 500 stock index over 1,250 days, from the beginning of
# 2001 until the end of 2005. For each date, we have recorded the percentage returns for
# each of the five previous trading days, Lag1 through Lag5. We have also recorded Volume
# (the number of shares traded on the previous day, in billions), Today (the percentage return
# on the date in question) and Direction (whether the market was Up or Down on this date).
# our goal is to predict Direction (a qualitative response) using the other features.

library(ISLR2)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)

cor(Smarket[, -9])
attach(Smarket)
plot(Volume, type="l", col="lightgreen", frame.plot=FALSE, lwd=0.5)

# 4.7.2 Logistic Regression
glm_fits <- glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
  data = Smarket, family = binomial
)
coef(glm_fits)
summary(glm_fits)$coef[, 4]

glm_probs <- predict(glm_fits, type="response")
glm_probs[1:10]
contrasts(Direction)

glm_pred <- rep("Down", 1250)
glm_pred[glm_probs > 0.5] = "Up"

table(glm_pred, Direction)
calculation_mean <- (507 + 145) / 1250
mean_var <- mean(glm_pred == Direction)

train <- (Year < 2005)
Smarket_2005 <- Smarket[!train, ]
dim(Smarket_2005)
Direction_2005 <- Direction[!train]
glm_fits <- glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
  data = Smarket, family = binomial, subset = train
)
glm_probs <- predict(glm_fits, Smarket_2005, type = "response")
glm_pred <- rep("Down", 252)
glm_pred[glm_probs > 0.5] <- "Up"
table(glm_pred, Direction_2005)
mean(glm_pred == Direction_2005)
mean(glm_pred != Direction_2005)

glm_fits <- glm(Direction ~ Lag1 + Lag2, data = Smarket, family=binomial, subset=train)
glm_probs <- predict(glm_fits, Smarket_2005, type="response")
glm_pred <- rep("Down", 252)
glm_pred[glm_probs > 0.5] <- "Up"
table(glm_pred, Direction_2005)
mean(glm_pred == Direction_2005)
106/(106+76)

predict(glm_fits,
        newdata <- data.frame(Lag1=c(1.2, 1.5), Lag2=c(1.1,-0.8)),
        type="response")

# Linear Discriminant Analysis
library(MASS)
lda_fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda_fit
plot(lda_fit)

lda_pred <- predict(lda_fit, Smarket_2005)
names(lda_pred)
lda_class <- lda_pred$class
table(lda_class, Direction_2005)
mean(lda_class==Direction_2005)
sum(lda_pred$posterior[, 1] >= 0.5)
sum(lda_pred$posterior[, 1] < 0.5)


lda_pred$posterior[1:20, 1]
lda_class[1:20]

sum(lda_pred$posterior[, 1] > 0.90)


# 4.7.4 Quadratic Discrimant Analysis
qda_fit <- qda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
qda_fit

qda_class <- predict(qda_fit, Smarket_2005)$class
table(qda_class, Direction_2005)
mean(qda_class == Direction_2005)

# 4.7.5 Naive Bayes
library(e1071)
nb_fit <- naiveBayes(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
nb_fit
mean(Lag1[train][Direction[train] == "Down"])
sd(Lag1[train][Direction[train] == "Down"])

nb_class <- predict(nb_fit, Smarket_2005)
table(nb_class, Direction_2005)
mean(nb_class == Direction_2005)
nb_preds <- predict(nb_fit, Smarket_2005, type="raw")
nb_preds[1:5,]


# 4.7.6 K-nearest Neighbour
library(class)
train_x <- cbind(Lag1, Lag2)[train, ]
test_x <- cbind(Lag1, Lag2)[!train, ]
train_direction <- Direction[train]

set.seed(1)
knn_pred <- knn(train_x, test_x, train_direction, k=1)
table(knn_pred, Direction_2005)
(83+43)/252

knn_pred <- knn(train_x, test_x, train_direction, k=3)
table(knn_pred, Direction_2005)
mean(knn_pred == Direction_2005)

# New Caravan Example
dim(Caravan)
attach(Caravan)
summary(Purchase)

standardized_x <- scale(Caravan[, -86])
var(Caravan[, 1])
var(Caravan[, 2])
var(standardized_x[, 1])
var(standardized_x[, 2])

test <- 1:1000
train_X <- standardized_x[-test, ]
test_X <- standardized_x[test, ]
train_Y <- Purchase[-test]
test_Y <- Purchase[test]
set.seed(1)
knn_pred <- knn(train_X, test_X, train_Y, k=1)
plot(knn_pred)
mean(test_Y != knn_pred)
mean(test_Y != "No")

table(knn_pred, test_Y)
9/(68+9)
knn_pred <- knn(train_X, test_X, train_Y, k=3)
plot(knn_pred)
table(knn_pred, test_Y)


knn_pred <- knn(train_X, test_X, train_Y, k=5)
plot(knn_pred)
table(knn_pred, test_Y)

glm_fits <- glm(Purchase ~ ., data=Caravan, family=binomial, subset=-test)
glm_probs <- predict(glm_fits, Caravan[test, ], type="response")
glm_pred <- rep("No", 1000)
glm_pred[glm_probs > 0.5] <- "Yes"
table(glm_pred, test_Y)
glm_pred <- rep("No", 1000)
glm_pred[glm_probs > 0.25] <- "Yes"
table(glm_pred, test_Y)
11/(22+11)


# 4.7.7 Poisson Regression
attach(Bikeshare)
dim(Bikeshare)
names(Bikeshare)

mod_lm <- lm(bikers ~ mnth + hr + workingday + temp + weathersit, data = Bikeshare)
plot(mod_lm)
summary(mod_lm)

contrasts(Bikeshare$hr) = contr.sum(24)
contrasts(Bikeshare$mnth) = contr.sum(12)
mod_lm2 <- lm(bikers ~ mnth + hr + workingday + temp + weathersit, data=Bikeshare)
summary(mod_lm2)

sum((predict(mod_lm) - predict(mod_lm2))^2)
all.equal(predict(mod_lm), predict(mod_lm2))

coef_months <- c(coef(mod_lm2)[2:12],
                 -sum(coef(mod_lm2)[2:12]))
plot(coef_months, xlab = "Month", ylab = "Coefficient", xaxt = "n", col = "lightblue",
     pch = 19, type = "o", frame.plot = FALSE)
axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A", "M", "J", "J",
                                     "A", "S", "O", "N", "D"))

coef_hours <- c(coef(mod_lm2)[13:35],
                - sum(coef(mod_lm2)[13:35]))
plot(coef_hours, xlab = "Hour", ylab = "Coefficient", col = "lightblue",
     pch = 19, type = "o")

mod_pois <- glm(
  bikers ~ mnth + hr + workingday + temp + weathersit, data = Bikeshare,
  family = poisson
)
summary(mod_pois)

coef_mnth <- c(coef(mod_pois)[2:12],
               -sum(coef(mod_pois)[2:12]))
plot(coef_mnth, xlab = "Month", ylab = "Coefficient", xaxt = "n", col = "lightblue",
     pch = 19, type = "o")
axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A", "M", "J", "J",
                                     "A", "S", "O", "N", "D"))
coef_hours <- c(coef(mod_pois)[13:35],
                -sum(coef(mod_pois)[13:35]))
plot(coef_hours, xlab="Hours", ylab="Coefficient", col="lightblue", pch=19,
     type="o")

plot(predict(mod_lm2), predict(mod_pois, type="response"))
abline(0, 1, col=2, lwd=3)


















