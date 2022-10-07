## ----setup, include=FALSE------------------------------------------------------------------------------------------------
library(tidyverse)
library(gridExtra)
set.seed(60637)


## .small-output .remark-code{

##   font-size: x-small;

## }

## 

## # .show-only-last-code-result pre + pre:not(:last-of-type) code[class="remark-code"] {

## #     display: none;

## # }


## ----coinflipRV----------------------------------------------------------------------------------------------------------
X <- c(0, 1, 2)
probs <- c(0.25, 0.5, 0.25)

sample(x = X,
       size = 1,
       prob = probs)



## ----coinflip_manyRV-----------------------------------------------------------------------------------------------------
n <- 1000
result_n <- sample(x = X,
                   size = n,
                   prob = probs,
                   replace = TRUE)

table(result_n)


## ----coinflip_massRV-----------------------------------------------------------------------------------------------------

prop.table(table(result_n))



## ----fig.width = 6, fig.height=6, fig.align='center'---------------------------------------------------------------------

ggplot(tibble(result_n), aes(x = result_n)) +
  geom_histogram(bins = 3, position = 'identity', color = 'white')



## ---- fig.width = 5, fig.height=5, fig.align = 'center', echo=FALSE------------------------------------------------------
plotdata <- tibble(
  x = c(-1, 0, 1, 2),
  xend = c(0, 1, 2, 3),
  fx = c(0, 1/4, 1/2, 1/4),
  Fx = cumsum(fx)
)

ggplot(plotdata, aes(x = x, y = fx)) +
  geom_point() +
  coord_cartesian(xlim = c(-0.5, 2.5),
                  ylim = c(0,1)) +
  geom_segment(aes(x = x, y = c(0,0,0,0), xend = x, yend = fx)) +
  ggtitle('PMF of X as number of heads in 2 fair coin flips')


## ----coinflip_plotRV, fig.width = 5, fig.height=5, fig.align = 'center', echo=FALSE--------------------------------------
ggplot(plotdata, aes(x = x, y = Fx)) +
  geom_segment(aes(x = x, y = Fx, xend = xend, yend = Fx)) + 
  geom_point() +
  geom_point(aes(x = xend, y = Fx), shape= 21, fill = 'white') +
  coord_cartesian(xlim = c(-0.5, 2.5),
                  ylim = c(0,1)) +
  ggtitle('CDF of X as number of heads in 2 fair coin flips')


## ----coinflip_simRV, fig.width = 5, fig.height=5, fig.align = 'center'---------------------------------------------------

ggplot(tibble(result_n), aes(x = result_n)) +
  stat_ecdf() +
  coord_cartesian(xlim = c(-0.5, 2.5)) +
  ylab('Empirical Fx') +
  ggtitle('ECDF of X as number of heads in 2 fair coin flips')



## ----coinflip, fig.width = 6, fig.height=3.5, fig.align = 'center'-------------------------------------------------------
Omega <- c('HH', 'HT', 'TH', 'TT')
probs <- c(0.25, 0.25, 0.25, 0.25)

result_n <- sample(x = Omega,
                   size = n,
                   prob = probs,
                   replace = TRUE)

result_mat <- tibble(omega = result_n,
                     x = case_when(result_n == 'TT' ~ 0, TRUE ~ 1),
                     y = case_when(result_n == 'HH' ~ 1, TRUE ~ 0))

options <- list(theme(panel.grid.minor = element_blank()), scale_x_continuous(breaks = c(0, 1))) # save some style options

p1 <- ggplot(result_mat) + geom_histogram(aes(x = x), bins = 3, position = 'identity', color = 'white') + options
  
p2 <- ggplot(result_mat) + geom_histogram(aes(x = y), bins = 3, position = 'identity', color = 'white') + options

grid.arrange(p1, p2, ncol = 2)



## ---- fig.width = 5, fig.height=5, fig.align = 'center', echo=FALSE------------------------------------------------------
hist_top <- p1 
empty <- ggplot()+geom_point(aes(1,1), colour="white")+
  theme(axis.ticks=element_blank(), 
        panel.background=element_blank(), 
        axis.text.x=element_blank(), axis.text.y=element_blank(),           
        axis.title.x=element_blank(), axis.title.y=element_blank())

count_mat <- result_mat %>% 
  group_by(x, y) %>% 
  summarize(count = n(), .groups = 'keep')

scatter <- ggplot(result_mat, aes(x = x, y = y, color = omega)) +
  geom_jitter(width = 0.25, height = 0.25, alpha = 0.5) + 
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(breaks = c(0,1))+
  theme(panel.grid.minor = element_blank(), legend.position = 'none') 

hist_right <- p2 + coord_flip()

grid.arrange(hist_top, empty, scatter, hist_right, ncol=2, nrow=2, widths=c(4, 2), heights=c(2, 4))



## ----pdb_fig0, echo=FALSE, out.width = "75%", fig.align="center"---------------------------------------------------------
knitr::include_graphics('assets/pdb.png')


## ----pdb_fig, echo=FALSE, out.width = "75%", fig.align="center"----------------------------------------------------------
knitr::include_graphics('assets/pdb_7-AUG-65.jpg')


## ----pdb_fig2, echo=FALSE, out.width = "75%", fig.align="center"---------------------------------------------------------
knitr::include_graphics('assets/pdb_7-AUG-65.jpg')


## ----carson, message = FALSE---------------------------------------------------------------------------------------------
file <- "https://raw.githubusercontent.com/UChicago-pol-methods/IntroQSS-F21/main/data/carsonPDB.csv"
df_pdb <- read_csv(file)

df_pdb



## ------------------------------------------------------------------------------------------------------------------------
df_pdb %>% 
  select(!c(PDBid, date, President)) %>% 
  summarize(across(.fns = list("mean"= ~ mean(., na.rm = TRUE), 
                               "var"= ~ var(., na.rm = TRUE))))


## ----standard_uniform----------------------------------------------------------------------------------------------------
runif(n = 1, min = 0, max = 1)


## ----standard_uniform_many, fig.width = 6, fig.height=4, fig.align = 'center'--------------------------------------------
result_n <- runif(n, min = 0, max = 1)

ggplot(tibble(result_n), aes(x = result_n)) +
  geom_histogram(breaks = seq(0, 1, length.out = 15),  
                 position = 'identity', color = 'white')


## ----uniform_plot, fig.width = 6, fig.height=4, fig.align = 'center', echo=FALSE-----------------------------------------


plotdata <- tibble(
  x = c(-1, 0, 1, 2),
  Fx = c(0, 0, 1, 1)
)

ggplot(plotdata, aes(x = x, y = Fx)) +
  geom_line() + 
  coord_cartesian(xlim = c(-0.5, 1.5),
                  ylim = c(0,1)) +
  ggtitle('CDF of Standard Uniform Distribution')


## ---- fig.width = 4, fig.height=4, fig.align = 'center', echo=FALSE------------------------------------------------------
plotdata <- tibble(
  x = c(-1, 0, 1, 1),
  xend = c(0, 1, 1, 2),
  fx = c(0, 1, 1, 0)
)

ggplot(plotdata, aes(x = x, y = fx)) +
  geom_segment(aes(x = x, y = fx, xend = xend, yend = fx)) + 
  geom_point() +
  geom_point(aes(x = 0, y = 0), shape= 21, fill = 'white') +
  geom_point(aes(x = 1, y = 0), shape= 21, fill = 'white') +
  coord_cartesian(xlim = c(-0.5, 1.5),
                  ylim = c(0,1)) +
  ggtitle('PDF of Standard Uniform Distribution')


## ---- fig.width = 4, fig.height=4, fig.align = 'center', echo=FALSE------------------------------------------------------
datapoly <- tibble(x = c(0, 0, 1, 1),
                   y = c(0, 1, 1, 0))

ggplot(plotdata, aes(x = x, y = fx)) +
  geom_segment(aes(x = x, y = fx, xend = xend, yend = fx)) + 
  geom_point() +
  geom_point(aes(x = 0, y = 0), shape= 21, fill = 'white') +
  geom_point(aes(x = 1, y = 0), shape= 21, fill = 'white') +
  coord_cartesian(xlim = c(-0.5, 1.5),
                  ylim = c(0,1)) +
  ggtitle('PDF of Standard Uniform Distribution')



## ---- fig.width = 4, fig.height=4, fig.align = 'center', echo=FALSE------------------------------------------------------
datapoly <- tibble(x = c(0, 0, 1, 1),
                   y = c(0, 1, 1, 0))

ggplot(plotdata, aes(x = x, y = fx)) +
  geom_segment(aes(x = x, y = fx, xend = xend, yend = fx)) + 
  geom_point() +
  geom_point(aes(x = 0, y = 0), shape= 21, fill = 'white') +
  geom_point(aes(x = 1, y = 0), shape= 21, fill = 'white') +
  coord_cartesian(xlim = c(-0.5, 1.5),
                  ylim = c(0,1)) +
  geom_polygon(data = datapoly, aes(x = x, y = y), fill = 'blue', alpha = 0.5) + 
  ggtitle('PDF of Standard Uniform Distribution')


## ---- fig.width = 5, fig.height=5, fig.align = 'center', echo=FALSE------------------------------------------------------
datapoly <- tibble(x = c(0, 0, 0.75, 0.75),
                   y = c(0, 1, 1, 0))

ggplot(plotdata, aes(x = x, y = fx)) +
  geom_segment(aes(x = x, y = fx, xend = xend, yend = fx)) + 
  geom_point() +
  geom_point(aes(x = 0, y = 0), shape= 21, fill = 'white') +
  geom_point(aes(x = 1, y = 0), shape= 21, fill = 'white') +
  coord_cartesian(xlim = c(-0.5, 1.5),
                  ylim = c(0,1)) +
  geom_polygon(data = datapoly, aes(x = x, y = y), fill = 'blue', alpha = 0.5) + 
  ggtitle('PDF of Standard Uniform Distribution')


## ---- fig.width = 6, fig.height=4, fig.align = 'center', echo=FALSE------------------------------------------------------
result_n <- rnorm(n = 10000)
plotdata <- tibble(
  x = result_n,
  Fx = pnorm(result_n),
  fx = dnorm(result_n)
)

ggplot(plotdata, aes(x = x, y = fx)) +
  geom_line() +
  coord_cartesian(xlim = c(-2.5, 2.5),
                  ylim = c(0,0.5)) +
  geom_vline(xintercept = 0, lty = 'dashed', color = 'skyblue') +
  geom_segment(aes(x = 0, xend = -1, y = 0.2, yend = 0.2), 
               arrow = arrow(length = unit(0.25, "cm")), color = 'skyblue') +
  geom_segment(aes(x = 0, xend = 1, y = 0.2, yend = 0.2), 
               arrow = arrow(length = unit(0.25, "cm")), color = 'skyblue') +
  geom_point(aes(x = 0, y = 0.2), color = 'skyblue') + 
  annotate(geom="text", x = 0.5, y = .19, label = as.character(expression(sigma)), parse = TRUE, color = 'skyblue') + 
  annotate(geom="text", x = -0.5, y = .19, label = as.character(expression(sigma)), parse = TRUE, color = 'skyblue') + 
  annotate(geom="text", x = 0.075, y = .42, label = as.character(expression(mu)), parse = TRUE, color = 'skyblue') +
  ggtitle('PDF of Standard Normal Distribution')


## ---- fig.width = 6, fig.height=4, fig.align = 'center', echo=FALSE------------------------------------------------------
ggplot(plotdata, aes(x = x, y = Fx)) +
  geom_line() +
  coord_cartesian(xlim = c(-2.5, 2.5),
                  ylim = c(0,1)) +
  geom_vline(xintercept = 0, lty = 'dashed', color = 'skyblue') +
  ggtitle('CDF of Standard Normal Distribution')


## ---- eval = F-----------------------------------------------------------------------------------------------------------
## ~/Dropbox/courses/PLSC_30500_2021/slides_21.Rmd
## C:\Schoolwork\stats_courses\PLSC_30500\File.txt


## ---- eval = F-----------------------------------------------------------------------------------------------------------
## PLSC_305000_2021/slides_21.Rmd
## .\..\PLSC_30500\File.txt  # .. means "go up one level"


## ----knit, echo=FALSE, message=FALSE, warning=FALSE, eval=FALSE----------------------------------------------------------
## knitr::purl("slides_iqss_week_2_pt2")

