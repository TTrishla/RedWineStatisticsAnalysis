## Wine Assignment

#1. Suppose the population mean of the variable "density" is μ , do the following inferences:

**#a. Provide an estimate of μ based on the sample;**
  
  ```{r}
{library(readr)}
df<- read.table(file="data/wine+quality/winequality-red.csv",sep=';',header =T)
attach(df)
```

```{r}
str(df)
mu_density = mean(df$density)
```

**#b. Use the Central Limit Theorem (CLT) to quantify the variability of your estimate;**
  
  ```{r}
dim(df)
mu_density = mean(df$density) 
```

```{r}
hist(df$density,col = "pink",main = "Histogram for Density ",xlab = "Density")
abline(v=12.8,col="red",lty=1)
```

```{r}
#We will take sample size=30, 50 & 500 samples=1599
#Calculate the arithmetice mean and plot the mean of sample 1599 times

s30 <- c()
s50 <- c()
s500 <- c()
n =1599
for ( i in 1:n){
  s30[i] = mean(sample(df$density,30, replace = TRUE))
  s50[i] = mean(sample(df$density,50, replace = TRUE))
  s500[i] = mean(sample(df$density,500, replace = TRUE))
}
par(mfrow=c(1,3))
hist(s30, col ="lightblue",main="Sample size=30",xlab ="density")
abline(v = mean(s30), col = "red")

hist(s50, col ="lightgreen", main="Sample size=50",xlab ="density")
abline(v = mean(s50), col = "red")

hist(s500, col ="orange",main="Sample size=500",xlab ="density")
abline(v = mean(s500), col = "red")

#Here, we get a good bell-shaped curve, and the sampling distribution approaches the normal distribution 
#as the sample sizes increase.Therefore, we can consider the sampling distributions as normal.

#The central limit theorem says that the sampling distribution of the mean will always be normally distributed until the sample size is large enough.
#Sampling should be random. The samples should not relate to one another. One sample shouldn’t affect the others.


```

```{r}
standev_density <- sd(density)
standev_density
mu_density = mean(df$density) 
mu_density

#As seen by these values, the means are very close to 1 and standard deviations are very close to 0. The Central Limit Theorem is showcased here as the density sample reflects a normal distribution

```

**#c. Use the CLT to give a 95% confidence interval for μ;**
  
  ```{r}

lower <- mean(density) - 2*(sd(density)/sqrt(length(density)))
upper <- mean(density) + 2*(sd(density)/sqrt(length(density)))

lower
upper

#As seen above, the 95% interval falls between 0.9966523 to 0.9968411

```

**#d. Use the bootstrap method to do parts b and c, and compare the results with those;**
  
  ```{r}
density_mu.set <- NULL
for(k in 1:3500) {
  bootstrap <- sample(density, size=1599, replace=T)
  mu_density <- mean(bootstrap)
  density_mu.set[k] <- mu_density
}
mean(density_mu.set)
sd(density_mu.set)

conf_quantile <- quantile(density_mu.set, probs = c(0.025,0.975))
conf_quantile

#Utilizing the bootstrap method, we can see that the results come to very similar answers as the CLT. Both ways are effective to prove the normal distribution of density.
```

#2. Suppose the population mean of the variable "residual sugar" is μ ,

#a. Provide an estimate of μ based on the sample;


```{r}
mean_resid_sugar <- mean(residual.sugar)
mean_resid_sugar  
#2.538806
```

```{r}
hist(df$residual.sugar,col = "pink",main = "Histogram for Density ",xlab = "Density")
abline(v=12.8,col="red",lty=1)
```

b\. Noting that the sample distribution of \"residual sugar\" is highly skewed, can we use the CLT to quantify the variability of your estimate? Can we use the CLT to give a 95% confidence interval for µ? If yes, please give your solution. If no, explain why.

```{r}
standev_sugar <- sd(`residual.sugar`)
standev_sugar
```

```{r}
lower_sugar <- mean(`residual.sugar`) - 2*(sd(`residual.sugar`)/sqrt(length(`residual.sugar`)))
upper_sugar <- mean(`residual.sugar`) + 2*(sd(`residual.sugar`)/sqrt(length(`residual.sugar`)))
lower_sugar
upper_sugar

#Yes, we can use the CLT to quantify the variability of residual sugar. In addition we can use it to give a 95% confidence interval. As shown in the code, the bounds come to 2.468 and 2.609.
```

c\. Use the bootstrap method to do part b. Is the bootstrap confidence interval\
symmetric?

```{r}
mu_sugar.set <- NULL
for(k in 1:3500) {
  sugar.bootstrap <- sample(`residual.sugar`, size=1599, replace=T)
mu_sugar <- mean(sugar.bootstrap)
mu_sugar.set[k] <- mu_sugar
}
mean(mu_sugar.set)

sd(mu_sugar.set)

conf_quantile <- quantile(mu_sugar.set, probs = c(0.025,0.975))
conf_quantile
```

```{r}
hist(mu_sugar.set, freq = FALSE)
lines(density(mu_sugar.set), lwd=5, col='blue')

#Yes, we can use the bootstrap method as well. As seen by the histrogram, the distribution is mostly symmetric.
```

3.  We classify those wines as \"excellent\" if their rating is at least 7. Suppose the population proportion of excellent wines is p. Do the following: proportion of excellent wines is p. Do the following:

```{=html}
<!-- -->
```
1.  Use the CLT to derive a 95% confidence interval for p;

```{# {r}
red_wine$excellent <- as.numeric(red_wine$quality > 6)

hat <- mean(red_wine$excellent)

variation_excel <- sqrt(hat*(1 - hat) / length(red_wine$excellent))
variation_excel
## [1] 0.008564681
lower_qual <- hat - 2*(variation_excel)
upper_qual <- hat + 2*(variation_excel)
lower_qual
## [1] 0.1185805
upper_qual
## [1] 0.1528392



The 95% confidence interval gives us bounds of 0.1186 and 0.1528.

Use the bootstrap method to derive a 95% confidence interval for p;
mu_quality.set <- NULL
for(k in 1:3500) {
  quality.bootstrap <- sample(red_wine$excellent, size=1599, replace=T)
mu_quality <- mean(quality.bootstrap)
mu_quality.set[k] <- mu_quality
}
mean(mu_quality.set)
## [1] 0.1358249
sd(mu_quality.set)
## [1] 0.008656594
conf_quantile_qual <- quantile(mu_quality.set, probs = c(0.025,0.975))
conf_quantile_qual
##      2.5%     97.5% 
## 0.1194497 0.1532208
Bootstrap gives us a confidence interval that is similar to the interval seen above for every time the loop is run.

Compare the two intervals. Is there any difference worth our attention?
The intervals are mostly the same and do not require further attention.
```
