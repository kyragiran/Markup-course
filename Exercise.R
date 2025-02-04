---
  title: "Assignment 1"
author: "Kyra Giran"
date: "`r Sys.Date()`"
output: pdf_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(lavaan)
```

```{r}
data = read.table("C:\\Users\\giran\\OneDrive\\Dokumentumok\\UU MSBBSS\\II. semester\\psychometrics\\Assignment 1\\Assign1.dat", header = TRUE)

summary(data)
```

1. Model fit

```{r}
model <- 'f1 =~ x1 + x2 + x7 + x10 + x13 + x16
          f2 =~ x3 + x4 + x8 + x11 + x14 + x17
          f3 =~ x5 + x6 + x9 + x12 + x15 + x18'

model1 <- cfa(model, data = data)

fitmeasures(model1, c("rmsea","nfi", "chisq", "df", "pvalue", "tli", "cfi"))
summary(model1)
```

Yes, the model fits the data quite well. The NFI is higher than 0.95, the RMSEA is below 0.06, CFI and TLI are higher than .90. Allof these meausres infrom us, that we have a good model fit. Moreover, the chi-square is also non-significant. 

2. Are the latent variables measured independent?
  
  The latent variables are not independent. 

The correlation is set to zero by setting the argument orthogonal = TRUE. The models are nested so we comapre them by using the likelihood ratio test. The more constrained model has a significantly worse fit than the previous one. We conclude model with correlated covariances has a better fit.

```{r}

#fitting the second model
model2 <- cfa(model, data = data, orthogonal = TRUE)
summary(model2)

#testing the difference between the two model fits
anova(model1, model2)
lavTestLRT(model1, model2)
```

3. What is the estimate of the total variance explained?
  
  ```{r}

1 - (sum(diag(inspect(model1, "est")$theta))/sum(diag(fitted(model1)$cov)))

theta<- inspect(model1, "est")$theta
sigma<- fitted(model1, "est")$cov

#explained variace calculation
exp_var<- 1-sum(diag(theta))/sum(diag(sigma))
exp_var

lambda<-inspect(model1,'est')$lambda
psi<-inspect(model1,'est')$psi
sigma <- fitted(model1)$cov
theta <- inspect(model1, 'est')$theta


# total variance explained calculation
psi11 <- psi[1,1]
psi22 <- psi[2,2]
psi33 <- psi[3,3]
lambda1 <- lambda[,1]
lambda2 <- lambda[,2]
lambda3 <- lambda[,3]

diag(sigma)

# 0.54
```

4. Give the estimates of the validities of the three unweighted subtest scores as measures of the latent variables.

```{r, eval = FALSE}
lambda_sub1 <- lambda[1:6,]
lambda_sub2 <- lambda[7:12,]
lambda_sub3 <- lambda[13:18,]

theta_sub1 <- theta[1:6, 1:6]
theta_sub2 <- theta[7:12, 7:12]
theta_sub3 <- theta[13:18, 13:18]

identity_matrix <- matrix(c(1,0,0,0,1,0,0,0,1), nrow = 3)

eu1 = identity_matrix[,1]
eu2 <- identity_matrix[,2]
eu3 <- identity_matrix[,3]

weights <- c(1,1,1,1,1,1)

sigma_1 <- lambda_sub1%*%psi%*%t(lambda_sub1) + theta_sub1
sigma_2 <- lambda_sub2%*%psi%*%t(lambda_sub2) + theta_sub2
sigma_3 <- lambda_sub3%*%psi%*%t(lambda_sub3) + theta_sub3

(weights%*%lambda_sub1%*%psi%*%eu1)^2
weights%*%sigma_1%*%weights%*%psi11

w1 <- c(rep(1,6), rep(0, 12))
w2 <- c(rep(0,6), rep(1,6), rep(0,6))
w3 <- c(rep(0,12), rep(1,6))

# test 1 and factor 1
(t(w1)%*%lambda%*%psi%*%eu1)^2/(t(w1)%*%sigma%*%w1%*%psi11)
# subtest 2, factor 1
(t(w2)%*%lambda%*%psi%*%eu1)^2/(t(w2)%*%sigma%*%w2%*%psi11)
# subtest 3, factor 1
(t(w3)%*%lambda%*%psi%*%eu1)^2/(t(w3)%*%sigma%*%w3%*%psi11) 


# subtest 1 and factor 2
(t(w1)%*%lambda%*%psi%*%eu2)^2/(t(w1)%*%sigma%*%w1%*%psi22)
# test 2, factor 2
(t(w2)%*%lambda%*%psi%*%eu2)^2/(t(w2)%*%sigma%*%w2%*%psi22) 
# subtest 3, factor 2
(t(w3)%*%lambda%*%psi%*%eu2)^2/(t(w3)%*%sigma%*%w3%*%psi22) 


# subtest 1 and factor 3
(t(w1)%*%lambda%*%psi%*%eu3)^2/(t(w1)%*%sigma%*%w1%*%psi33)
# subtest 2, factor 3
(t(w2)%*%lambda%*%psi%*%eu3)^2/(t(w2)%*%sigma%*%w2%*%psi33) 
# test 3, factor 3
(t(w3)%*%lambda%*%psi%*%eu3)^2/(t(w3)%*%sigma%*%w3%*%psi33) 

```

5. Give the estimates of the communalities of the three unweighted subtest scores. What do you notice when you compare these to the corresponding validity estimates? Does that make sense? Explain.

They are the same, therefore they are measuring the same underlying concept, which is how much proportion of Y is explained by one common factor. Since they load on the same factor, they measure the same thing. 

```{r}
#Weights
weights1 <- c(rep(1,6), rep(0, 12))
weights2 <- c(rep(0,6), rep(1,6), rep(0,6))
weights3 <- c(rep(0,12), rep(1,6))

omega <- lambda%*%psi%*%t(lambda)

# communality 1
communality1 = (weights1%*%omega%*%weights1)/(weights1%*%sigma%*%weights1)
communality1
# communality 2
communality2 = (weights2%*%omega%*%weights2)/(weights2%*%sigma%*%weights2)
communality2
# communality 3
communality3 =(weights3%*%omega%*%weights3)/(weights3%*%sigma%*%weights3)
communality3

```

6. Give the estimates of the validities of Thurstone factor scores as measures of the latent variables.

```{r}

identity_matrix <- matrix(c(1,0,0,0,1,0,0,0,1), nrow = 3)

eu1 = identity_matrix[,1]
eu2 <- identity_matrix[,2]
eu3 <- identity_matrix[,3]

eu1 = identity_matrix[,1]
options(scipen=999)
thurnstone <- psi%*%t(lambda)%*%solve(sigma)

#dividing the thrustone factors
thurnstone1 <- thurnstone[1,]
thurnstone2 <- thurnstone[2,]
thurnstone3 <- thurnstone[3,]

# test 1 and factor 1
(t(thurnstone1)%*%lambda%*%psi%*%eu1)^2/(t(thurnstone1)%*%sigma%*%thurnstone1%*%psi11)
# subtest 2, factor 1
(t(thurnstone2)%*%lambda%*%psi%*%eu1)^2/(t(thurnstone2)%*%sigma%*%thurnstone2%*%psi11) 
# subtest 3, factor 1
(t(thurnstone3)%*%lambda%*%psi%*%eu1)^2/(t(thurnstone3)%*%sigma%*%thurnstone3%*%psi11) 

# test 1 and factor 2
(t(thurnstone1)%*%lambda%*%psi%*%eu2)^2/(t(thurnstone1)%*%sigma%*%thurnstone1%*%psi22)
# subtest 2, factor 2
(t(thurnstone2)%*%lambda%*%psi%*%eu2)^2/(t(thurnstone2)%*%sigma%*%thurnstone2%*%psi22)
# subtest 3, factor 2
(t(thurnstone3)%*%lambda%*%psi%*%eu2)^2/(t(thurnstone3)%*%sigma%*%thurnstone3%*%psi22) 

# test 1 and factor 3
(t(thurnstone1)%*%lambda%*%psi%*%eu3)^2/(t(thurnstone1)%*%sigma%*%thurnstone1%*%psi33) 
# subtest 2, factor 3
(t(thurnstone2)%*%lambda%*%psi%*%eu3)^2/(t(thurnstone2)%*%sigma%*%thurnstone2%*%psi33)
# subtest 3, factor 3
(t(thurnstone3)%*%lambda%*%psi%*%eu3)^2/(t(thurnstone3)%*%sigma%*%thurnstone3%*%psi33) 

```

7. Calculate estimates of the correlations among the Thurstone factor scores.

```{r}
#reordering so it matches the order dataframe's order
reorder <- c('x1','x2','x7','x10', 'x13', 'x16', 'x3', 'x4','x8', 'x11', 'x14', 'x17', 'x5', 'x6', 'x9', 'x12', 'x15', 'x18')

cent_data <- scale(data[, reorder], center = TRUE, scale = FALSE) 

thurnstornes <- cent_data %*%solve(sigma)%*%lambda%*%psi

cor(thurnstornes) #correlarion between thurstone scores
```

8. Calculate the communality of the unweighted total test score. What does this estimate tell you about the reliability of the unweighted total test score?
  
  It is a lower bound of the reliability.
Therefore reliability is at least 0.93, which can be considered a good reliability.

```{r}
w_total_score <- rep(1, 18)
# communality 1
(w_total_score%*%omega%*%w_total_score)/(w_total_score%*%sigma%*%w_total_score)
```

9. Cronbach's alpha

```{r}
covariance_cron<-cov(data)
k <- ncol(data)
