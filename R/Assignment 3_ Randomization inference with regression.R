library(tidyverse)
library(haven)

read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

hiv <- read_data("thornton_hiv.dta")

# creating the permutations

tb <- NULL

permuteHIV <- function(df, random = TRUE){
  tb <- df
  first_half <- ceiling(nrow(tb)/2)
  second_half <- nrow(tb) - first_half
  
  if(random == TRUE){
    tb <- tb %>%
      sample_frac(1) %>%  # change the order in the sample: two ways of random sampling, this one looks better
      mutate(any = c(rep(1, first_half), rep(0, second_half)))
  }
 
  lmfit <- lm(got ~ any + male, data = tb)
  delta <- lmfit$coefficients[[2]]
  
  ate <-  delta
  
  return(ate)
}

permuteHIV(hiv, random = FALSE)

p_value <- vector("double", length = 3L)
for (i in seq(1,3)){
  iterations = 10^(i+1)
  permutation <- tibble(
    iteration = c(seq(iterations)), 
    ate = as.numeric(
      c(permuteHIV(hiv, random = FALSE), map(seq(iterations-1), ~permuteHIV(hiv, random = TRUE)))
    )
  )
  
  #calculating the p-value
  p_value[i] <-  permutation %>% 
    mutate(comparison = rep(permuteHIV(hiv, random = FALSE), iterations)) %>% 
    summarise(mean(ate >= comparison))
  
}
p_value

permutation %>% 
  ggplot(aes(ate)) +
  geom_histogram(bins = 100) +
  theme_bw()
  

