
library(EpiEstim)

MCMC_seed <- 1
overall_seed <- 2

res<- estimate_R(incid = peru$cases,
  method = "uncertain_si",
  config = make_config(list(
           mean_si = 3.96, std_mean_si = 4.75,
          min_mean_si = 1, max_mean_si = 4.2,
         std_si = 1.5, std_std_si = 0.5, 
          min_std_si = 0.5, max_std_si = 2.5,
          n1 = 100, n2 = 100)))
res
plot(res)
