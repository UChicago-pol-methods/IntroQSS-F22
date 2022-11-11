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


## ----pdb_fig, echo=FALSE, out.width = "50%", fig.align="center"-----------------------------------------
knitr::include_graphics('assets/Youngronaldfisher2.jpeg')


## ---- eval = FALSE--------------------------------------------------------------------------------------
## # install.packages('ri')
## library(ri)
## 
## 


## ---- echo = FALSE--------------------------------------------------------------------------------------
source('assets/ri/desmat.sanitize.R')
source('assets/ri/gendist.R')
source('assets/ri/genperms.R')
source('assets/ri/estate.R')
source('assets/ri/dispdist.R')



## -------------------------------------------------------------------------------------------------------

df <- tibble(
  # our initial treatment vector
  D = c(1, 0, 0, 0, 0, 0, 1),
  # our initial response vector
  Y = c(15, 15, 20, 20, 10, 15, 30),
  # treatment assignment probability
  probs = rep(2/7, 7)
)

df


## -------------------------------------------------------------------------------------------------------
dm_hat <- df |> 
  group_by(D) |>
  summarize(mean = mean(Y)) |>
  pivot_wider(names_from = D, values_from = mean) |>
  summarise(diff = `1` -`0`) |> 
  pull(diff)

dm_hat



## -------------------------------------------------------------------------------------------------------

df <- df |> 
  mutate(
    # Y(0) under the sharp null of no effect
    Y0 = Y,
    # Y(1) under the sharp null of no effect
    Y1 = Y)

df


## -------------------------------------------------------------------------------------------------------
(perms <- genperms(df |> pull(D)))


## -------------------------------------------------------------------------------------------------------

Ys_null <- list(
  Y0 = df |> pull(Y0),
  Y1 = df |> pull(Y1)
)

dm <- gendist(Ys_null,
              perms, 
              prob=df$probs)
dm

null_dist <- tibble(dm)



## -------------------------------------------------------------------------------------------------------
null_dist |> 
  summarize(mean(dm))


## ---- fig.height=4, fig.width = 5, fig.align = "center", echo=FALSE-------------------------------------

gg_bins <- null_dist |> 
  group_by(dm) |> 
  count(name = 'Relative frequency') |> 
  mutate(col = abs(dm) >= dm_hat)

ggplot(gg_bins, aes(x = dm, y = `Relative frequency`)) +
  geom_col() +
  geom_vline(xintercept = 0, color = 'red')



## -------------------------------------------------------------------------------------------------------
prop.table(table(null_dist))


## -------------------------------------------------------------------------------------------------------
(pval <- mean(abs(null_dist) >= dm_hat))


## ---- fig.height=4, fig.width = 5, fig.align = "center", echo=FALSE-------------------------------------
ggplot(gg_bins, aes(x = dm, y = `Relative frequency`, fill = col)) +
  geom_col() +
  geom_vline(xintercept = 0, color = 'red') + 
  scale_fill_manual(values=c("grey35", "#619CFF")) +
  theme(legend.position = 'none')


## -------------------------------------------------------------------------------------------------------
(pval <- mean(abs(null_dist) >= dm_hat))


## -------------------------------------------------------------------------------------------------------

dispdist(distout = dm, 
         ate = dm_hat, 
         display.plot = FALSE)$two.tailed.p.value.abs


## ---- message=FALSE-------------------------------------------------------------------------------------

file <- '../data/Butler_Broockman_AJPS_2011_public.csv'
df <- read_csv(file)



## ---- echo = FALSE--------------------------------------------------------------------------------------
df


## -------------------------------------------------------------------------------------------------------
df |> 
  group_by(treat_deshawn) |> 
  summarize(n())


## -------------------------------------------------------------------------------------------------------
df |> 
  group_by(reply_atall) |> 
  summarize(n())


## -------------------------------------------------------------------------------------------------------
df <- df |> 
  mutate(D = treat_deshawn,
         Y = reply_atall)


## -------------------------------------------------------------------------------------------------------
dm_hat <- df |> 
  group_by(D) |>
  summarize(mean = mean(Y)) |>
  pivot_wider(names_from = D, values_from = mean) |>
  summarise(diff = `1` -`0`) |> 
  pull(diff)


## -------------------------------------------------------------------------------------------------------
dm_hat


## -------------------------------------------------------------------------------------------------------
# randomization inference function
my_ri <- function(df){
  ri_out <- df |>
    # create a new column newD that samples from D
    mutate(newD = sample(D)) |> 
    # difference in means estimate under newD
    group_by(newD) |>
    summarize(mean = mean(Y)) |>
    pivot_wider(names_from = newD, values_from = mean) |>
    summarise(diff = `1` -`0`) |> 
    pull(diff) # note this line!
  
  return(ri_out)
}


## -------------------------------------------------------------------------------------------------------
my_ri(df)


## -------------------------------------------------------------------------------------------------------
my_ri(df)


## -------------------------------------------------------------------------------------------------------
n_iter <- 1000

null_dist <- map_dbl(1:n_iter,
                     ~ my_ri(df))

null_dist


## ---- fig.height=4, fig.width = 5, fig.align = "center", echo=FALSE-------------------------------------

gg_bins <- tibble(dm = null_dist) |> 
  count(bins = cut(dm, breaks = 50)) |> 
  mutate(bin_min = as.numeric(gsub(".?(-?[0-9]+[.]+[0-9]+).*", "\\1", bins)),
         bin_max = as.numeric(gsub(".*,(-?[0-9]+[.]+[0-9]+)]$", "\\1", bins)),
         bin_mid = (bin_max - bin_min)/2 + bin_min,
         col = abs(bin_min) >= abs(dm_hat)) 


ggplot(gg_bins, aes(x = bin_mid, y = n)) +
  geom_col() +
  geom_vline(xintercept = 0, color = 'red')


## -------------------------------------------------------------------------------------------------------
(pval <- mean(abs(null_dist) >= abs(dm_hat)))


## ---- fig.height=4, fig.width = 5, fig.align = "center", echo=FALSE-------------------------------------

ggplot(gg_bins, aes(x = bin_mid, y = n, fill = col)) +
  geom_col() +
  geom_vline(xintercept = 0, color = 'red') + 
  scale_fill_manual(values=c("grey35", "#619CFF")) +
  theme(legend.position = 'none')



## -------------------------------------------------------------------------------------------------------
(pval <- mean(abs(null_dist) >= abs(dm_hat)))


## ---- fig.height=4, fig.width = 5, fig.align = "center", echo=FALSE-------------------------------------

gg_bins <- gg_bins |> 
  mutate(col2 = bin_min <= dm_hat)

ggplot(gg_bins, aes(x = bin_mid, y = n)) +
  geom_col() +
  geom_vline(xintercept = 0, color = 'red') 



## -------------------------------------------------------------------------------------------------------
(pval <- mean(null_dist <= dm_hat))




## ---- fig.height=4, fig.width = 5, fig.align = "center", echo=FALSE-------------------------------------

gg_bins <- gg_bins |> 
  mutate(col2 = bin_mid <= dm_hat)

ggplot(gg_bins, aes(x = bin_mid, y = n, fill = col2)) +
  geom_col() +
  geom_vline(xintercept = 0, color = 'red') + 
  scale_fill_manual(values=c("grey35", "#619CFF")) +
  theme(legend.position = 'none')



## -------------------------------------------------------------------------------------------------------
(pval <- mean(null_dist <= dm_hat))




## ---- fig.height=4, fig.width = 5, fig.align = "center", echo=FALSE-------------------------------------

ggplot(gg_bins, aes(x = bin_mid, y = n, fill = col2)) +
  geom_col() +
  geom_vline(xintercept = 0, color = 'red') + 
  scale_fill_manual(values=c("grey35", "#619CFF")) +
  theme(legend.position = 'none')



## ---- fig.height=4, fig.width = 5, fig.align = "center", echo=FALSE-------------------------------------

gg_bins <- gg_bins |> 
  mutate(col3 = bin_min >= dm_hat)

ggplot(gg_bins, aes(x = bin_mid, y = n, fill = col3)) +
  geom_col() +
  geom_vline(xintercept = 0, color = 'red') + 
  scale_fill_manual(values=c("grey35", "#619CFF")) +
  theme(legend.position = 'none')



## ---- fig.height=4, fig.width = 5, fig.align = "center", echo=FALSE-------------------------------------


ggplot(gg_bins, aes(x = bin_mid, y = n, fill = col)) +
  geom_col() +
  geom_vline(xintercept = 0, color = 'red') + 
  scale_fill_manual(values=c("grey35", "#619CFF")) +
  theme(legend.position = 'none')



## ----knit, echo=FALSE, message=FALSE, warning=FALSE, eval=FALSE-----------------------------------------
## knitr::purl("slides_71.Rmd")

