#make sample data
library(tidyverse)

gen_person <- function(yrfrom,yrto,meanbmi,trend, baseage, gender){
  
  n <- length(yrfrom:yrto)
  
  d <- tibble(
    yr = yrfrom:yrto,
    bmi = rnorm(n ,mean = meanbmi, sd = 1),
    age = baseage:(baseage+n-1),
    gender = gender
  )
  
  deltabmi <- seq(from=0,to=trend,length=n) |> 
    map_dbl(~{rnorm(1,.,2)})
  
  d <- d |> 
    mutate(bmi = bmi + deltabmi)
  
  return(d)
}


oa <- tribble(
  ~yr, ~agegrp, ~gender, ~meanbmi, ~trend, ~n,
  2017, 20, "男性", 21, 0, 20,
  2017, 30, "男性", 22, 6, 40,
  2017, 40, "男性", 22, -1, 50,
  2017, 50, "男性", 24, 2, 30,
  
  2017, 20, "女性", 18, 0, 30,
  2017, 30, "女性", 19, 0, 30,
  2017, 40, "女性", 22, 0, 10,
  2017, 50, "女性", 23, 2, 10
)

set.seed(123456)
i <- 1
oa2 <- oa |> 
  mutate(inddat = pmap(list(yr,agegrp,gender,meanbmi,trend,n), ~{
    ayr     <- ..1
    aage    <- ..2
    agender <- ..3
    amean   <- ..4
    atrend  <- ..5
    an      <- ..6
    
    aage <- runif(1,aage, aage+5) |> round()
    
    map_dfr(1:an, ~{
      
      r <- gen_person(ayr, ayr+5, rnorm(1,amean,4), atrend, aage, agender) |> 
        mutate(id = i)
      
      i <<- i + 1
      return(r)
    })
  }))

oa2 <- oa2 |> 
  select(inddat) |> 
  unnest(inddat)

oa3 <- oa2 |> 
  mutate(bmi25 = bmi >= 25) |> 
  mutate(bmi30 = bmi >= 30) |> 
  mutate(agegrp = case_when(
    between(age,0,29) ~ "~29歳",
    between(age,30,39) ~ "30代",
    between(age,40,49) ~ "40代",
    between(age,50,99) ~ "50代~"
  ))


oa3 |> 
  relocate(yr, id, gender, age, bmi, bmi25) |> 
  arrange(yr,id)

oa3 |> count(yr, bmi25) |> 
  group_by(yr) |> 
  mutate(tot = sum(n)) |> 
  mutate(perc = n/tot) |> 
  filter(bmi25) |> 
  ggplot() +
  geom_col(aes(x = yr, y = perc))

oa3 |> count(yr, agegrp, gender, bmi25) |>
  group_by(yr, gender, agegrp) |>
  mutate(tot = sum(n)) |>
  mutate(perc = n/tot) |>
  filter(bmi25) |>
  ggplot() +
  geom_col(aes(x = yr, y = perc)) +
  facet_wrap(agegrp~gender)
# 
# 
# ggplot(oa3) +
#   geom_boxplot(aes(x = as.factor(yr), y = bmi)) +
#   facet_wrap(agegrp~gender)


res <- oa3 |> 
  select(yr, id, gender, age, bmi) |> 
  arrange(yr)

write_csv(res,"data/sampledata.csv")
