## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(ggdag)
set.seed(60637)


## .small-output .remark-code{

## font-size: small;

## }

## 

## .white { color: white; }

## 

## # .show-only-last-code-result pre + pre:not(:last-of-type) code[class="remark-code"] {

## #     display: none;

## # }


## ---- fig.height=3, fig.width = 4, fig.align = "center", echo = F, message = F, out.width = "50%"------------------------------------------
library(ggdag)
coords <- tibble::tribble(
  ~name, ~x,  ~y,
  "D", 0,   0,
  "X", -.25,   .5,
  "Y",  1,   0
)

dagify(Y ~ X + D,
       D ~ X,
       coords = coords)|> 
  tidy_dagitty() |> 
  ggdag() + 
  theme_void()



## ---- echo = FALSE, out.width = "72%", fig.align='center'----------------------------------------------------------------------------------
knitr::include_graphics('assets/eee.jpg')


## ---- fig.height=3, fig.width = 4, fig.align = "center", echo = FALSE, message = FALSE, out.width = "50%"----------------------------------
library(ggdag)
coords <- tibble::tribble(
  ~name, ~x,  ~y,
  "D", 0,   0,
  "X", -.25,   .5,
  "Y",  1,   0
)

dagify(Y ~ X + D,
       D ~ X,
       coords = coords)|> 
  tidy_dagitty() |> 
  ggdag() + 
  theme_void()



## ---- fig.height=3, fig.width = 4, fig.align = "center", echo = FALSE, message = FALSE, out.width = "50%"----------------------------------
coords <- tribble(
  ~name, ~x,  ~y,
  "D", 0,   0,
  "X", -.25,   .5,
  "Y",  1,   0
)

dagify(Y ~ X + D,
       coords = coords)|>
  tidy_dagitty() |>
  ggdag() +
  theme_void()


## ------------------------------------------------------------------------------------------------------------------------------------------
df_villages <- tibble(
  Y0 = c(NA, 15, 20, 20, 10, 15,NA),
  Y1 = c(15, NA,NA,NA,NA,NA, 30),
  D = c(1, 0,0,0,0,0,1)
)


## ------------------------------------------------------------------------------------------------------------------------------------------
df_villages <- df_villages |> 
  mutate(Y = case_when(D == 1 ~ Y1, 
                       TRUE ~ Y0))

df_villages


## ---- message=FALSE------------------------------------------------------------------------------------------------------------------------
file <- '../data/Butler_Broockman_AJPS_2011_public.csv'
df <- read_csv(file)


## ------------------------------------------------------------------------------------------------------------------------------------------
head(df)


## ------------------------------------------------------------------------------------------------------------------------------------------
table(df$treat_deshawn)

# or
df |> 
  group_by(treat_deshawn) |> 
  summarize(n())


## ------------------------------------------------------------------------------------------------------------------------------------------
table(df$reply_atall)


## ------------------------------------------------------------------------------------------------------------------------------------------

df <- df |> 
  mutate(Y1 = case_when(treat_deshawn == 1 ~ reply_atall,
                        TRUE ~ NA_real_),
         Y0 = case_when(treat_deshawn == 0 ~ reply_atall,
                        TRUE ~ NA_real_))

head(df[, c('Y0', 'Y1')])


## ------------------------------------------------------------------------------------------------------------------------------------------
df |> 
  summarize(mean(Y1, na.rm = TRUE) - mean(Y0, na.rm = TRUE))
# Note that I'm just taking *observed* means here; unobserved 
# values are counted as NAs. If we had the full potential 
# outcomes table, we would have to account for which Y0s and 
# Y1s we are using based on whether we want the true 
# estimand value or an estimate under random assignment. 


## ------------------------------------------------------------------------------------------------------------------------------------------
library(estimatr)

difference_in_means(reply_atall ~ treat_deshawn, data = df)

