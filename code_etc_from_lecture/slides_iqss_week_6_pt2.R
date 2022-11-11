## ----setup, include=FALSE-------------------------------------------------------------------------------
library(tidyverse)
set.seed(60637)
options(width = 60)


## .small-output .remark-code{

##   font-size: small;

## }

## 

## .white { color: white; }

## .red { color: red; }

## .blue { color: blue; }

## 

## # .show-only-last-code-result pre + pre:not(:last-of-type) code[class="remark-code"] {

## #     display: none;

## # }


## ---- fig.width = 6, fig.height=5, fig.align = 'center', echo=FALSE-------------------------------------
result_n <- rnorm(n = 10000)
plotdata <- tibble(
  x = result_n,
  Fx = pnorm(result_n),
  fx = dnorm(result_n)
)

g <- ggplot(plotdata, aes(x = x, y = fx)) +
  geom_line() +
  coord_cartesian(xlim = c(-2.5, 2.5),
                  ylim = c(0,0.5)) +
  ggtitle('PDF of Standard Normal Distribution')

g +
  geom_vline(xintercept = 0, lty = 'dashed', color = 'skyblue') + 
  geom_segment(aes(x = 0, xend = -1, y = 0.2, yend = 0.2), 
               arrow = arrow(length = unit(0.25, "cm")), color = 'skyblue') +
  geom_segment(aes(x = 0, xend = 1, y = 0.2, yend = 0.2), 
               arrow = arrow(length = unit(0.25, "cm")), color = 'skyblue') +
  geom_point(aes(x = 0, y = 0.2), color = 'skyblue') + 
  annotate(geom="text", x = 0.5, y = .19, label = as.character(expression(sigma)), parse = TRUE, color = 'steelblue') + 
  annotate(geom="text", x = -0.5, y = .19, label = as.character(expression(sigma)), parse = TRUE, color = 'steelblue') + 
  annotate(geom="text", x = 0.075, y = .42, label = as.character(expression(theta)), parse = TRUE, color = 'steelblue')



## ---- fig.width = 6, fig.height=5, fig.align = 'center', echo=FALSE-------------------------------------
g +
  stat_function(fun = dnorm,
                geom = "area",
                fill = "skyblue",
                xlim = c(-10, qnorm(0.05))) +
  geom_vline(xintercept = qnorm(0.05), lty = 'dashed', color = 'skyblue') +  
  annotate(geom="text", x = qnorm(0.05), y = .2, label = round(qnorm(0.05), 3), parse = TRUE, color = 'steelblue')



## ---- fig.width = 6, fig.height=5, fig.align = 'center', echo=FALSE-------------------------------------
g +
  stat_function(fun = dnorm,
                geom = "area",
                fill = "skyblue",
                xlim = c(-10, qnorm(0.95))) +
  geom_vline(xintercept = qnorm(0.95), lty = 'dashed', color = 'skyblue') +  
  annotate(geom="text", x = qnorm(0.95), y = .2, label = round(qnorm(0.95), 3), parse = TRUE, color = 'steelblue')




## -------------------------------------------------------------------------------------------------------
qnorm(0.05)
qnorm(0.95)


## ---- fig.width = 6, fig.height=5, fig.align = 'center', echo=FALSE-------------------------------------
g +
  stat_function(fun = dnorm,
                geom = "area",
                fill = "skyblue",
                xlim = c(qnorm(0.025), qnorm(0.975))) +
  geom_vline(xintercept = qnorm(0.975), lty = 'dashed', color = 'skyblue') + 
  geom_vline(xintercept = qnorm(0.025), lty = 'dashed', color = 'skyblue') +  
  annotate(geom="text", x = qnorm(0.025), y = .2, label = round(qnorm(0.025), 3), parse = TRUE, color = 'steelblue') +  
  annotate(geom="text", x = qnorm(0.975), y = .2, label = round(qnorm(0.975), 3), parse = TRUE, color = 'steelblue')




## -------------------------------------------------------------------------------------------------------
X <- c(0, 1, 2)
fx <- c(1/16, 3/8, 9/16)
(Ex <- sum(X*fx))


## -------------------------------------------------------------------------------------------------------
n <- 100
x_observed <- sample(X, prob = fx, replace = TRUE, size = n)

head(x_observed)


## -------------------------------------------------------------------------------------------------------
(theta_hat <- mean(x_observed))
(se_hat <- sd(x_observed)/sqrt(n))


## -------------------------------------------------------------------------------------------------------
(z975 <- qnorm(0.975))


## -------------------------------------------------------------------------------------------------------
(CI95 <- c(theta_hat + c(-1,1)*z975*se_hat))


## ---- echo=FALSE, out.width = "80%", fig.align="center"-------------------------------------------------
ggplot(tibble(conf_lower = CI95[1], conf_upper = CI95[2], mean = theta_hat), 
       aes(y = 1, x = mean)) + 
  geom_point(color = 'skyblue') +
  geom_linerange(aes(xmin=conf_lower,xmax=conf_upper), color = 'skyblue', alpha = 0.85) +
  coord_cartesian(xlim = c(1.25, 1.75),) +
  geom_vline(xintercept = Ex, color = 'black', lty = 'dashed') +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle('95% Normal Approximation-Based CI, 2 Weighted Coin Flips, Sample Size = 100')
  


## -------------------------------------------------------------------------------------------------------
n_iter <- 50
x_list <- map(1:n_iter, ~ sample(X, prob = fx, replace = TRUE, 
                                 size = n))


## -------------------------------------------------------------------------------------------------------
CI_95f <- function(x){
  theta_hat <- mean(x)
  se_hat <- sd(x)/sqrt(n)
  CI_hat <- theta_hat + 
    c('conf_lower' = -1, 'conf_upper' = 1)*qnorm(0.975)*se_hat
}
  
sample_CIs <- map(x_list, CI_95f) 

head(sample_CIs, 3)


## -------------------------------------------------------------------------------------------------------
CI_n <- bind_rows(sample_CIs)

CI_n


## ---- echo=FALSE, out.width = "80%", fig.align="center"-------------------------------------------------
ggplot(CI_n, 
       aes(y = seq(from = 0, to = 2, length.out = n_iter), x = 1)) + 
  geom_linerange(aes(xmin=conf_lower,xmax=conf_upper, color = factor(1 * ((Ex >= conf_lower) & (Ex <= conf_upper)))), alpha = 0.85) +
  coord_cartesian(xlim = c(1.25, 1.75)) +
  geom_vline(xintercept = Ex, color = 'black', lty = 'dashed') +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle('95% Normal Approximation-Based CI, 2 Weighted Coin Flips, Sample Size = 100') +
  theme(legend.position = 'none')


## -------------------------------------------------------------------------------------------------------

CI_n |> 
  summarize(coverage = mean((Ex >= conf_lower) & (Ex <= conf_upper)))


## -------------------------------------------------------------------------------------------------------
x_list <- map(1:5000, ~ sample(X, prob = fx, replace = TRUE, 
                                 size = n))
CI_n <- map(x_list, CI_95f) |> bind_rows()


## -------------------------------------------------------------------------------------------------------
CI_n |> 
  summarize(coverage = mean((Ex >= conf_lower) & (Ex <= conf_upper)))


## ----bootstrap------------------------------------------------------------------------------------------
head(x_observed)
mean(x_observed)
var(x_observed)
sd(x_observed)



## -------------------------------------------------------------------------------------------------------
n_boot <- 1000 # number of bootstrap iterations

boot_ests <- map_dbl(1:n_boot, # for n_boot number of times
                 # resample w/replacement
                 ~ sample(x_observed, replace = TRUE) |>
                   mean()) # and calculate the resampled mean

head(boot_ests)


## -------------------------------------------------------------------------------------------------------

sd(boot_ests)


## -------------------------------------------------------------------------------------------------------
(var_est <- sum((X - Ex)^2*fx))
(n_sample <- length(x_observed))

sqrt(var_est/n_sample)


## ---- message=FALSE-------------------------------------------------------------------------------------
mcr <- tibble(
  black = rep(c(0, 1), times = c(300, 400)),
  record = c(rep(c(0, 1), each = 150), 
             rep(c(0, 1), each = 200)),
  call_back = c(
    # whites without criminal records
    rep(c(0, 1), times = c(99, 51)), # 150
    # whites with criminal records
    rep(c(0, 1), times = c(125, 25)), # 150: could be 25 or 26
    # blacks without criminal records
    rep(c(0, 1), times = c(172, 28)), # 200
    # blacks with criminal records
    rep(c(0, 1), times = c(190, 10)) # 200 
  )
)



## ---- message = FALSE-----------------------------------------------------------------------------------
mcr |> 
  group_by(black, record) |> 
  summarize(n = n(), 
            call_back = mean(call_back))


## ----pdb_fig, echo=FALSE, out.width = "80%", fig.align="center"-----------------------------------------
knitr::include_graphics('assets/pager_2003.png')


## -------------------------------------------------------------------------------------------------------
(theta_hat <- mcr |> 
   filter(black == 1) |> 
   summarize(mean(call_back)) |> 
   pull())


## -------------------------------------------------------------------------------------------------------
(se_hat <- mcr |> 
   filter(black == 1) |> 
   summarize(sqrt(var(call_back)/length(call_back))) |> 
   pull())



## -------------------------------------------------------------------------------------------------------
(CI <- c(theta_hat + c(-1,1)*qnorm(1-0.025)*se_hat))


## -------------------------------------------------------------------------------------------------------

boot_ests <- map(1:1000, # for 1000 times
                 # resample w/replacement
                 ~ sample(mcr |> filter(black == 1) 
                          |> pull(call_back), 
                          replace = TRUE) |>
                   mean()) # and calculate the resampled mean

boot_vec <- unlist(boot_ests)

(boot_se_hat <- sd(boot_vec))


## -------------------------------------------------------------------------------------------------------
(CI <- c(theta_hat + c(-1,1)*qnorm(1-0.025)*boot_se_hat))


## -------------------------------------------------------------------------------------------------------
theta_hat


## -------------------------------------------------------------------------------------------------------
se_hat


## ---- fig.width = 10, fig.height=5, fig.align = 'center', echo=FALSE------------------------------------
result_n <- rnorm(n = 10000, mean = 0.10, sd = se_hat)
plotdata <- tibble(
  x = result_n,
  Fx = pnorm(result_n, mean = 0.10, sd = se_hat),
  fx = dnorm(result_n, mean = 0.10, sd = se_hat)
)

g <- ggplot(plotdata, aes(x = x, y = fx)) +
  geom_line() +
  coord_cartesian(xlim = c(qnorm(0.000001, mean = 0.10, sd = se_hat), 
                           qnorm(0.999999, mean = 0.10, sd = se_hat))) +
  ggtitle('Null Distribution of the Sample Mean')

g +
  geom_vline(xintercept = 0.10, lty = 'dashed', color = 'skyblue') + 
  geom_segment(aes(x = 0.10, xend = 0.10-se_hat, y = 15, yend = 15), 
               arrow = arrow(length = unit(0.25, "cm")), color = 'skyblue') +
  geom_segment(aes(x = 0.10, xend = 0.10 + se_hat, y = 15, yend = 15), 
               arrow = arrow(length = unit(0.25, "cm")), color = 'skyblue') +
  geom_point(aes(x = 0.10, y = 15), color = 'skyblue') + 
  annotate(geom="text", x = 0.10 - se_hat/2, y = 14, label = as.character(expression(sigma)), parse = TRUE, color = 'steelblue') + 
  annotate(geom="text", x = 0.10 + se_hat/2, y = 14, label = as.character(expression(sigma)), parse = TRUE, color = 'steelblue') + 
  annotate(geom="text", x = 0.10 + .001, y = 20, label = as.character(expression(theta)), parse = TRUE, color = 'steelblue')



## -------------------------------------------------------------------------------------------------------
(tstat <- (theta_hat - 0.10)/se_hat)


## -------------------------------------------------------------------------------------------------------
(lower_p_value <- pnorm(tstat))


## ---- fig.width = 10, fig.height=5, fig.align = 'center', echo=FALSE------------------------------------
g +
  stat_function(fun = dnorm,
                args = list(mean = 0.10, sd = se_hat),
                geom = "area",
                fill = "skyblue",
                xlim = c(0, qnorm(lower_p_value, mean = 0.10, sd = se_hat))) +
  geom_vline(xintercept = theta_hat, color = 'skyblue', lty= 'dashed') +
  geom_segment(aes(x = 0.09, xend = 0.04 + se_hat, y = 15, yend = 15), 
               arrow = arrow(length = unit(0.25, "cm")), color = 'darkgray') +
  annotate('text', x = .06, y = 12.5, color = 'darkgray', 
           label = 'area under the curve in this direction', size = 6)



## -------------------------------------------------------------------------------------------------------
(two_tailed_p_value <- 2*(1 - pnorm(abs(tstat))))


## ---- fig.width = 10, fig.height=5, fig.align = 'center', echo=FALSE------------------------------------
g +
  stat_function(fun = dnorm,
                args = list(mean = 0.10, sd = se_hat),
                geom = "area",
                fill = "skyblue",
                xlim = c(0, qnorm(lower_p_value, mean = 0.10, sd = se_hat))) +
  stat_function(fun = dnorm,
                args = list(mean = 0.10, sd = se_hat),
                geom = "area",
                fill = "skyblue",
                xlim = c(qnorm(1-lower_p_value, mean = 0.10, sd = se_hat), 1)) +
  geom_vline(xintercept = theta_hat, color = 'skyblue', lty= 'dashed') +
  geom_vline(xintercept = 0.10+0.10-theta_hat, color = 'skyblue', lty= 'dashed') +
  geom_segment(aes(x = 0.09, xend = 0.04 + se_hat, y = 15, yend = 15), 
               arrow = arrow(length = unit(0.25, "cm")), color = 'darkgray') +
  annotate('text', x = .06, y = 12.5, color = 'darkgray', 
           label = 'area under the curve in this direction', size = 6) +
  geom_segment(aes(x = 0.10+0.10-0.09, xend = 0.10+0.10-(0.04 + se_hat), y = 15, yend = 15), 
               arrow = arrow(length = unit(0.25, "cm")), color = 'darkgray') +
  annotate('text', x = 0.10+0.10-.06, y = 8.5, color = 'darkgray', 
           label = 'and this direction', size = 6)



## -------------------------------------------------------------------------------------------------------
(tstat <- (theta_hat - 0)/se_hat)


## -------------------------------------------------------------------------------------------------------
(two_tailed_p_value <- 2*(1 - pnorm(abs(tstat))))


## ---- fig.width = 10, fig.height=5, fig.align = 'center', echo=FALSE, message = FALSE-------------------
result_n <- rnorm(n = 10000, mean = 0, sd = se_hat)
plotdata <- tibble(
  x = result_n,
  Fx = pnorm(result_n, mean = 0, sd = se_hat),
  fx = dnorm(result_n, mean = 0, sd = se_hat)
)

g <- ggplot(plotdata, aes(x = x, y = fx)) +
  geom_line() +
  coord_cartesian(xlim = c(qnorm(0.000001, mean = 0, sd = se_hat), 
                           qnorm(0.999999, mean = 0, sd = se_hat))) +
  ggtitle('Null Distribution of the Sample Mean')


## ---- fig.width = 10, fig.height=5, fig.align = 'center', echo=FALSE, message = FALSE-------------------
g +
  coord_cartesian(xlim = c(-.1, .1))


## ---- fig.width = 10, fig.height=5, fig.align = 'center', echo=FALSE, message = FALSE-------------------
g +
  coord_cartesian(xlim = c(-.1, .1)) +
  geom_vline(xintercept = -theta_hat, col = 'skyblue', lty = 'dashed') +
  geom_vline(xintercept = theta_hat, col = 'skyblue', lty = 'dashed')



## ---- fig.width = 10, fig.height=5, fig.align = 'center', echo=FALSE, message = FALSE-------------------
g +
  coord_cartesian(xlim = c(-.1, .1)) +
  geom_vline(xintercept = -theta_hat, col = 'skyblue', lty = 'dashed') +
  geom_vline(xintercept = theta_hat, col = 'skyblue', lty = 'dashed') +
  geom_segment(aes(x = -0.06, xend = -0.12 + se_hat, y = 15, yend = 15), 
               arrow = arrow(length = unit(0.25, "cm")), color = 'darkgray') +
  annotate('text', x = -.06, y = 12.5, color = 'darkgray', 
           label = 'area under the curve in this direction', size = 6) +
  geom_segment(aes(x = 0.06, xend = 0.1, y = 15, yend = 15), 
               arrow = arrow(length = unit(0.25, "cm")), color = 'darkgray') +
  annotate('text', x = .06, y = 8.5, color = 'darkgray', 
           label = 'and this direction', size = 6)


