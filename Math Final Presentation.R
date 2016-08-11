
library(hflights)
library(plotly)
library(ggplot2)
library(MASS)

#Random variable X = Distance
dist <-hflights$Distance
summary(dist)
#distance is skewed left as meadian>mean

# Random variable Y = Arrival Delay 
aDel <-hflights$ArrDelay
summary(aDel)
# Skewed right


# x <- distance, 75 percentile 
x<-quantile(dist)["75%"]
# y <- arr delay, 50 percentile
y<-quantile(aDel,na.rm = TRUE)["50%"]


# Part 1 BASIC STATISTICS
total<-nrow(hflights)

#a.P(X>x|Y>y)
xg_yg<-nrow(subset(hflights,dist>x & aDel>y))
yg<-nrow(subset(hflights,aDel>y))
p1<-xg_yg/total
p2<-yg/total
a<-p1/p2
a
#a = 0.2502806,
#Analysis: The probability of a distance to be above third quartile given that 
#the arrival delay is greater than 2d quartile, is 0.2502806

#b. P(X>x,Y>y)
xg<-nrow(subset(hflights,dist>x))
yg<-nrow(subset(hflights,aDel>y))
p3 <-xg/total
p4<-yg/total
b<-p3*p4
b
#b= 0.1171846
#Analysis:The probability of a distance to be greater than third quartile and 
#the arrival delay is greater than 2d quartile, is 0.1171846

#c. P(X<x | Y>y)
xl_yl<-nrow(subset(hflights,dist<x & aDel>y))
p5<-xl_yl/total
c<-p5/p4
c
#c=0.7417321
#Analysis:The probability of a distance to be below third quartile given that 
#the arrival delay is greater than 2d quartile, is 0.7417321

#Fill the table values
c11<-nrow(subset(hflights,dist<=x & aDel<=y))
c12<-nrow(subset(hflights,dist<=x & aDel>y))
c13<-c11+c12
c21<-nrow(subset(hflights,dist>x & aDel<=y))
c22<-nrow(subset(hflights,dist>x & aDel>y))
c23<-c21+c22
c31<-c11+c21
c32<-c12+c22
c33<-c13+c23

tabValues<- matrix(c(c11,c12,c13,c21,c22,c23,c31,c32,c33),3,3)
colnames(tabValues) <- c("<=2d quartile",">2d quartile","Total")
rownames(tabValues) <- c('<=3d quartile', '>3d quartile','Total')
tabValues          
#Analysis: It is difficult to comment by just looking at the data, whether 
#splitting has made them independent.

# Check P(A|B) = P(A).P(B)
A<-xg
B<-yg
p6=p1/p4
p7=p3*p4
check<-(p6==p7)
check
#Analysis: Mathematically P(A|B) NE P(A).P(B)

# chi-square test for independence
tbl = table(hflights$Distance, hflights$ArrDelay)
chisq.test(tbl)
#Analysis:
# p value 2.2e-16 is very small, so rejecting null hypothesis, meaning,there is 
# relationship b/w distance and arrival delay



# Part 2 DESCRIPTIVE AND INFERENTIAL STATISTICS

# univariate descriptive of statistics
summary(dist)
summary(aDel)

# Histogram
hist(hflights$Distance,xlab= "Distance", main = "Distance of flights")
hist(hflights$ArrDelay,xlab= "Arrival Delay", main = "Delay in Arrival")

# Density plot
d1 <- density(dist) 
plot(d1)
d2 <-density(aDel, na.rm = TRUE)
plot(d2)

# Scatter plot
plot(dist,aDel)
qqplot(dist,aDel, xlab="Distance", ylab="Arrival Delay")

# 95% CI for the difference of the two means
t.test(dist, aDel) 
#Analysis: The diiference of the mean of random variable distance and
#arrival delay lies between 787.783245 and 7.094334.This can be stated with 95%
# confidence.

# Correlation matrix
Dist_aDelay<- hflights[,c("Distance","ArrDelay")]
Dist_aDelay <- Dist_aDelay[complete.cases(Dist_aDelay), ]
mat<-cor(Dist_aDelay)
mat
#Analysis: Correlation matrix shows their is a slight negative correlation 
#between distance and arrival delay


#Test the hypothesis that the correlation b/w these variables is 0 
#and provide a 99% confidence interval. 
cor.test(dist,aDel, method = "pearson" , conf.level = 0.99)
# Analysis:1.As the P value is too small, there is enough evidence to reject the null
#hypothesis.
#2.At alpha level 0.01 we say that correlation between distance and 
# arrival delay is not 0, i.e there is correlation b/w these two variables



# Part 3 LINEAR ALGEBRA AND CORRELATION

# Inverse correlation matrix
inv<-solve(mat)

# multiply correlation matrix with precision matrix
matrix1 <-mat  %*% inv
matrix2<- inv %*% mat
matrix1
matrix2
#Analysis: The correlation matrix shows a negative correlation between
#distance and arrrival delay, meaning when distance increases arrival delay
#decreases and vice versa. But this correlation is very small(-0.004434254)  


#Part 4. CALCULUS BASED PROBABILITY AND STATISTICS

# For variable skewed to right, shift so that the min value is above 0.
# Arr Delay is skewed right
min_aDel <- min(aDel,na.rm = TRUE)
# The min value is -70
aDel_new<-na.omit(aDel + 71)

# Fit exponential probability function and calculate lambda
expdist<-fitdistr(aDel_new,"exponential")
l<-expdist$estimate
samp<-rexp(1000, l)

#Plot Histogram
hist(samp,xlab= "Arriavl Delay", main = "Arrival Delay Data")
hist(aDel,xlab= "Arriavl Delay", main = "Arrival Delay Data")
#Analysis: comparing the histograms we see the data is still positively skewed 
#as in the original dataset, but with the estimations, it is more spread out.


#Using the exponential pdf, find the 5th and 95th percentiles
#using the cumulative distribution function (CDF).
qexp(0.05,rate = l) #  4.005716
qexp(0.95,rate = l) # 233.9497

#95% confidence interval from the empirical data, assuming normality.
error<-qnorm(0.975)*expdist$sd/sqrt(expdist$n)
left<- l-error  # 0.01280491 
right<-l+error  # 0.01280514 

#5th percentile and 95th percentile of the data
quantile(aDel, c(.05, .95),na.rm = TRUE)
# 5% 95% 
#-18  57 
#Analysis:There is a considerable difference between the empirical data
# and the original data
