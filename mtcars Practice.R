library(ggplot2)
library(GGally)
library(ggpubr)
library(datasets)
library(dplyr)
data(mtcars)
all <- lm(mpg ~ ., mtcars)
summary(all)$coefficients
summary(lm(mpg ~ am, mtcars))

require(stats); require(graphics)
pairs(mtcars, panel = panel.smooth, main = "mtcars data",
      col = 3 + mtcars$mpg > 25)
summary(lm(mpg ~ ., data = mtcars))


data(mtcars); par(mfrow = c(2, 2))
fit_mt <- lm(mpg ~ ., data = mtcars); plot(fit_mt)

fit1 <- lm(mpg ~ am, data = mtcars)
fit3 <- update(fit, mpg ~ am + cyl + disp)
fit5 <- update(fit, mpg ~ am + cyl + disp + hp + carb)
anova(fit1, fit3, fit5)

summary(lm(mpg ~ ., data = mtcars))$coef

g = ggplot(data = mtcars, aes(y = mpg, x = factor(am), fill = factor(cyl)))
g = g + geom_violin(colour = "black", size = 2)
g

summary(lm(mpg ~ factor(am), data = mtcars))$coef
summary(lm(mpg ~ factor(am) + disp, data = mtcars))$coef
summary(lm(mpg ~ factor(am) + factor(cyl), data = mtcars))$coef

mdl <- lm(mpg ~ am + cyl + disp + hp, data = mtcars)
vif(mdl)

mdl2 <- lm(mpg ~ am + cyl + hp, data = mtcars)
vif(mdl2)


# From Quiz 3, we know that the interaction between number of cylinders and
# weight is likely not necessary for investigation of mpg.
fit1 <- lm(mpg ~ factor(cyl) + disp, data = mtcars)
fit2 <- lm(mpg ~ factor(cyl) * disp, data = mtcars)
summary(fit1)$coef
summary(fit2)$coef

anova(fit1, fit2)
# yields P-value of 0.001 - disp is a covariant for mpg.


###########
library(dplyr)
library(ggplot2)
count(mtcars$am) # returns 19 automatic; 13 manual

g = ggplot(data = mtcars, aes(y = mpg, x = disp, colour = factor(am), shape = factor(cyl)))
g = g + geom_point(size = 6, colour = "black") + geom_point(size = 4)
g = g + xlab("displacement") + ylab("mpg")
g
fit = lm(mpg ~ disp, data = mtcars)
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
g1
fit_t <- lm(mpg ~ disp * factor(am), data = mtcars)
summary(fit_t)$coef
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit_t)[1], slope = coef(fit_t)[2], size = 2,
                      colour = "salmon")
g1 = g1 + geom_abline(intercept = coef(fit_t)[1] + coef(fit_t)[3],
                      slope = coef(fit_t)[2] + coef(fit_t)[4], size =2,
                      colour = "light blue")
g1
### Add regression after excluding disp < 100


# remove smallest 5 displacement cars
auto <- subset(mtcars, am==0)
min(auto$disp) # [1] 120.1
manual <- subset(mtcars, am==1)
max(manual$disp) # [1] 351

mtcars1 <- subset(mtcars, disp > 120)
mtcars1 <- subset(mtcars1, disp < 352)

h = ggplot(data = mtcars1, aes(y = mpg, x = disp, colour = factor(am), shape = factor(cyl)))
h = h + geom_point(size = 6, colour = "black") + geom_point(size = 4)
h = h + xlab("displacement") + ylab("mpg")
h
fit = lm(mpg ~ disp, data = mtcars1)
h1 = h
h1 = h1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
h1
fit_t1 <- lm(mpg ~ disp * factor(am), data = mtcars1)
summary(fit_t1)$coef
h1 = h
h1 = h1 + geom_abline(intercept = coef(fit_t1)[1], slope = coef(fit_t1)[2], size = 2,
                      colour = "salmon")
h1 = h1 + geom_abline(intercept = coef(fit_t1)[1] + coef(fit_t1)[3],
                      slope = coef(fit_t1)[2] + coef(fit_t1)[4], size =2,
                      colour = "light blue")
h1

fit1 <- lm(mpg ~ disp + factor(am), data = mtcars1)
fit2 <- lm(mpg ~ disp * factor(am), data = mtcars1)
anova(fit1, fit2)
# P-value = 0.5197 --- P-value (with min/max disp) = 0.8637

manual <- subset(mtcars, am==1)
x <- manual$disp
y <- manual$mpg
z <- (x > 100) * x
fit <- lm(y ~ x + z) 
sum(coef(fit)[2:3])# [1] -0.0363255

opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0))
plot(fit_t, las = 1)
par(opar)

opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0))
plot(fit_t1, las = 1)
par(opar)
############

fit1 <- lm(mpg ~ disp + factor(am), data = mtcars)
fit2 <- lm(mpg ~ disp * factor(am), data = mtcars)
anova(fit1, fit2)
# P-value = 0.01044

fit1 <- lm(mpg ~ factor(cyl) + factor(vs), data = mtcars)
fit2 <- lm(mpg ~ factor(cyl) * factor(vs), data = mtcars)
anova(fit1, fit2)[6]
# P-value = 0.618

fit1_c <- lm(mpg ~ factor(am) + factor(cyl), data = mtcars)
fit2_c <- lm(mpg ~ factor(am) * factor(cyl), data = mtcars)
cyl <- anova(fit1_c, fit2_c)[6]
# P-value = 0.269

fit1_d <- lm(mpg ~ factor(am) + disp, data = mtcars)
fit2_d <- lm(mpg ~ factor(am) * disp, data = mtcars)
disp <- anova(fit1_d, fit2_d)[6]
# P-value = 0.0104

fit1 <- lm(mpg ~ factor(am) + hp, data = mtcars)
fit2 <- lm(mpg ~ factor(am) * hp, data = mtcars)
anova(fit1, fit2)
# P-value = 0.9806

fit1 <- lm(mpg ~ factor(am) + factor(carb), data = mtcars)
fit2 <- lm(mpg ~ factor(am) * factor(carb), data = mtcars)
anova(fit1, fit2)
# P-value = 0.5614

fit1 <- lm(mpg ~ factor(am) + drat, data = mtcars)
fit2 <- lm(mpg ~ factor(am) * drat, data = mtcars)
anova(fit1, fit2)
# P-value = 0.4538

fit1 <- lm(mpg ~ factor(am) + qsec, data = mtcars)
fit2 <- lm(mpg ~ factor(am) * qsec, data = mtcars)
anova(fit1, fit2)
# P-value = 0.07012

fit1 <- lm(mpg ~ factor(am) + wt, data = mtcars)
fit2 <- lm(mpg ~ factor(am) * wt, data = mtcars)
anova(fit1, fit2)
# P-value = 0.001017

fit1 <- lm(mpg ~ factor(am) + factor(vs), data = mtcars)
fit2 <- lm(mpg ~ factor(am) * factor(vs), data = mtcars)
anova(fit1, fit2)
# P-value = 0.2589

fit1 <- lm(mpg ~ factor(am) + factor(gear), data = mtcars)
fit2 <- lm(mpg ~ factor(am) * factor(gear), data = mtcars)
anova(fit1, fit2)
# P-value = ---

z <- mtcars$disp
lm(mpg ~ . + z, data = mtcars)


#########################
g = ggplot(data = mtcars, aes(y = mpg, x = wt, colour = factor(am), shape = factor(cyl)))
g = g + geom_point(size = 6, colour = "black") + geom_point(size = 4)
g = g + xlab("weight") + ylab("mpg")
g
fit = lm(mpg ~ wt, data = mtcars)
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
g1
fit_t <- lm(mpg ~ wt * factor(am), data = mtcars)
summary(fit_t)$coef
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit_t)[1], slope = coef(fit_t)[2], size = 2,
                      colour = "salmon")
g1 = g1 + geom_abline(intercept = coef(fit_t)[1] + coef(fit_t)[3],
                      slope = coef(fit_t)[2] + coef(fit_t)[4], size =2,
                      colour = "light blue")
g1
#################################
mtcars$Transmission <- mtcars$am
mtcars$Transmission <- dplyr::recode(mtcars$Transmission, 
                                     `0` = 'Automatic', `1` = 'Manual')
mtcars$Transmission <- as.factor(mtcars$Transmission)

auto <- subset(mtcars, am==0)
min(auto$wt) # [1] 2.465
manual <- subset(mtcars, am==1)
max(manual$wt) # [1] 3.57

mtcars1 <- subset(mtcars, wt > 2.46)
mtcars1 <- subset(mtcars1, wt < 3.571)
qq3 <- ggqqplot(data = mtcars1, x = "mpg", color = "Transmission")

ggpairs(mtcars1, columns = 1:7, lower = list(continuous = wrap("smooth", method = "lm")))
bound <- lm(mpg ~ ., data = mtcars1) # r-sq = 0.959; adjusted r-sq = 0.8907
summary(bound)
tail(cbind(sort(summary(mtcars1)$coefficients[,4])),1) ## $ operator is invalid for atomic vectors
# gear has greatest p-value = 0.5363
bound2 <- lm(mpg ~ ., -gear, data = mtcars1) # r-sq = 0.9829; adjusted r-sq = 0.9258
summary(bound2)

bound3 <- lm(mpg ~ am, data = mtcars1)
summary(bound3)

bp2 <- ggplot(mtcars1, aes(x=Transmission, y=mpg, fill=Transmission, 
                          group=Transmission)) + geom_boxplot()
bp2 <- bp2 + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
bp2 <- bp2 + labs(x="Transmission", y="Miles per Gallon (mpg)", 
                  title="Miles per Gallon (mpg) by Transmission")

table_2 <- mtcars2 %>%
  group_by(Transmission) %>%
  summarise(n = n(),
            min = min(mpg),
            q1 = quantile(mpg, 0.25),
            median = median(mpg),
            mean_mpg = mean(mpg),
            q3 = quantile(mpg, 0.75),
            max = max(mpg),
            sd_mpg = sd(mpg))
kable(table_1, digits = 2)

mpg_a1 <- mtcars1[mtcars1$Transmission == "Automatic",]$mpg
mpg_m1 <- mtcars1[mtcars1$Transmission == "Manual",]$mpg
t.test(mpg_a1, mpg_m1, alternative = "greater")

sx <- sort(mpg_m1); sy <- sort(mpg_a1)
lenx <- length(sx)
leny <- length(sy)
if (leny < lenx)sx <- approx(1L:lenx, sx, n = leny)$y
if (leny > lenx)sy <- approx(1L:leny, sy, n = lenx)$y
fit1_xy <- lm(sy ~ sx)
require(ggplot2)
qq4 = ggplot() + geom_point(aes(x=sx, y=sy))
qq4 = qq4 + geom_abline(intercept = coef(fit1_xy)[1], slope = coef(fit1_xy)[2], size = 1)
qq4 = qq4 + labs(x="mpg_Manual", y="mpg_Automatic")

require(gridExtra)
grid.arrange(qq3, qq4, ncol=2)

mv_mt1 <- step(lm(mpg ~ ., data=mtcars1),direction="both", trace=FALSE)
summary(mv_mt1)

mtcars2 <- subset(mtcars, wt >= min(auto$wt))
mtcars2 <- subset(mtcars2, wt <= max(manual$wt))

#########################
fit <- lm(mpg ~ ., data = mtcars)
fit1 <- lm(mpg ~ wt, data = mtcars)
fit3 <- update(fit, mpg ~ wt + disp)
fit5 <- update(fit, mpg ~ wt + disp + qsec)
anova(fit1, fit3, fit5)

h = ggplot(data = mtcars1, aes(y = mpg, x = wt, colour = factor(am), shape = factor(cyl)))
h = h + geom_point(size = 6, colour = "black") + geom_point(size = 4)
h = h + xlab("weight") + ylab("mpg")
h
fit = lm(mpg ~ wt, data = mtcars1)
h1 = h
h1 = h1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
h1
fit_t1 <- lm(mpg ~ wt * factor(am), data = mtcars1)
summary(fit_t1)$coef
h1 = g + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
h1 = h1 + geom_abline(intercept = coef(fit_t1)[1], slope = coef(fit_t1)[2], size = 2,
                      colour = "salmon")
h1 = h1 + geom_abline(intercept = coef(fit_t1)[1] + coef(fit_t1)[3],
                      slope = coef(fit_t1)[2] + coef(fit_t1)[4], size =2,
                      colour = "light blue")
h1 = h1 + geom_vline(xintercept = 2.46, colour = "black", size = 2, linetype = "dashed")
h1 = h1 + geom_vline(xintercept = 3.57, colour = "black", size = 2, linetype = "dashed")
h1

fit1 <- lm(mpg ~ wt + factor(am), data = mtcars1)
fit2 <- lm(mpg ~ wt * factor(am), data = mtcars1)
anova(fit1, fit2)

######################
library(ggpubr)
data(mtcars); par(mfrow = c(2, 2))
fit <- lm(mpg ~ ., data = mtcars); plot(fit)

c = ggplot(data=mtcars, aes(y=mpg, x=cyl, colour=factor(am))) + geom_point(size=2)
d = ggplot(data=mtcars, aes(y=mpg, x=disp, colour=factor(am))) + geom_point(size=2)
h = ggplot(data=mtcars, aes(y=mpg, x=hp, colour=factor(am))) + geom_point(size=2)
dr = ggplot(data=mtcars, aes(y=mpg, x=drat, colour=factor(am))) + geom_point(size=2)
w = ggplot(data=mtcars, aes(y=mpg, x=wt, colour=factor(am))) + geom_point(size=2)
q = ggplot(data=mtcars, aes(y=mpg, x=qsec, colour=factor(am))) + geom_point(size=2)
v = ggplot(data=mtcars, aes(y=mpg, x=vs, colour=factor(am))) + geom_point(size=2)
g = ggplot(data=mtcars, aes(y=mpg, x=gear, colour=factor(am))) + geom_point(size=2)
ca = ggplot(data=mtcars, aes(y=mpg, x=carb, colour=factor(am))) + geom_point(size=2)
test <- ggarrange(c, d, h, dr, w, q, v, g, ca, ncol = 3, nrow = 3)
test

par(mfrow = c(1, 2))
with(mtcars, {
  plot(mpg~cyl,
       ylab = "mpg", xlab = "Cylinders")
  plot(mpg~disp,
       ylab = "mpg", xlab = "Engine Displacement (cu. in.)")
})

par(mfrow = c(1, 2))
{
  ggplot(data=mtcars, aes(y=mpg, x=cyl, colour=factor(am))) + geom_point(size=2)
  ggplot(data=mtcars, aes(y=mpg, x=disp, colour=factor(am))) + geom_point(size=2)

}

mtcars1$Transmission <- dplyr::recode(mtcars1$Transmission, 
                                     `0` = 'Automatic', `1` = 'Manual')
mtcars1$Transmission <- as.factor(mtcars1$Transmission)
table <- mtcars1 %>%
  group_by(transmission) %>%
  summarise(n = n(),
            min = min(mpg),
            q1 = quantile(mpg, 0.25),
            median = median(mpg),
            mean_mpg = mean(mpg),
            q3 = quantile(mpg, 0.75),
            max = max(mpg),
            sd_mpg = sd(mpg))
ggplot(mtcars1, aes(y = mpg, x = transmission)) + 
  geom_boxplot() +
  geom_dotplot(binaxis = 'y', stackdir = 'center', fill = 'red') +
  labs(x = "Transmission", y = "Miles Per Gallon (MPG)",
       title = "Miles Per Gallon (MPG) by Transmission")

all_1 <- (lm(mpg ~ ., data = mtcars1))
tail(cbind(sort(summary(all_1)$coefficients[,4])),1)
tail(cbind(sort(summary(all)$coefficients[,4])))

pi <- data.frame(Transmission = c("Automatic", "Manual"),
                 Lower = c(pi_a[2], pi_m[2]),
                 Fit = c(pi_a[1], pi_m[1]),
                 Upper = c(pi_a[3], pi_m[3]))
kable(pi, digits = 3, align = 'c', caption = "Simple Linear Regression: MPG Prediction Intervals")


#####################################################
mtcars$Transmission <- dplyr::recode(mtcars$Transmission, 
                                      `0` = 'Automatic', `1` = 'Manual')
mtcars$Transmission <- as.factor(mtcars$Transmission)
qq1 <- ggqqplot(data = mtcars, x = "mpg", color = "Transmission")


mtcarsa <- subset(mtcars, am == 0)
mtcarsm <- subset(mtcars, am == 1)
t.test(mtcarsm$mpg, mtcarsa$mpg, alternative = "greater", paired = FALSE, var.equal = FALSE)

mpg_a <- mtcars[mtcars$Transmission == "Automatic",]$mpg
mpg_m <- mtcars[mtcars$Transmission == "Manual",]$mpg
t.test(mpg_a, mpg_m, alternative = "greater")

sx <- sort(mpg_m); sy <- sort(mpg_a)
lenx <- length(sx)
leny <- length(sy)
if (leny < lenx)sx <- approx(1L:lenx, sx, n = leny)$y
if (leny > lenx)sy <- approx(1L:leny, sy, n = lenx)$y
fit_xy <- lm(sy ~ sx)
require(ggplot2)
qq2 = ggplot() + geom_point(aes(x=sx, y=sy))
qq2 = g + geom_abline(intercept = coef(fit_xy)[1], slope = coef(fit_xy)[2], size = 1)
qq2 = qq2 + labs(x="mpg_Manual", y="mpg_Automatic")
t.test(sx, sy, alternative = "greater")
## a Q-Q plot that is reasonable linear indicates that the 2 data sets have distributions
## with similar shapes
require(gridExtra)
grid.arrange(qq1, qq2, ncol=2)


model <- lm(mpg ~ Transmission - am, data = mtcars)
summary(model)

step(lm(mpg~.,data=df),direction="both")

mtcars$Transmission_Type[mtcars$am==0] <- "Automatic"
mtcars$Transmission_Type[mtcars$am==1] <- "Manual"
df <- mtcars[,-9, drop=FALSE]
mv <- step(lm(mpg ~ ., data=df),direction="both", trace=FALSE)
summary(mv)$coef

confint(mv)[2,]
