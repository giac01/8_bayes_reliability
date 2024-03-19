# DEPRECIATED - NOT USING THIS APPROACH ANYMORE JUST STAN APPROACH

# Load Stuff -------------------------------------------------------------------
rm(list=ls(all.names = T))
gc()
library(brms)
library(tidyverse)
library(future)
library(future.apply)

# Source all simulation functions 
# List all .R files in the folder
list.files(file.path("helper_functions"), pattern = "\\.R$", full.names = TRUE) %>%
  lapply(., function(x) {print(x);source(x)})

# Simulation Parameters --------------------------------------------------------

# tau_equivalence = c(TRUE)
# 
# # set n_items
# # randomise loadings 
# seed_val = 1
# 
# sample_sizes = c(50, 500, 2000)
# 
# run_repetitions = 1

set.seed(10)

# Compile brms model -----------------------------------------------------------


# prior1 <- set_prior("normal(0, 5)", class = "b")
# prior3 <- set_prior("normal(0, 5)", dpar = "sigma")   # THIS SEEMS TO BREAK THE MODEL! 
# 
# # prior2 <- set_prior("student_t(3, 0, 10)", class = "Intercept")
# prior2 <- set_prior("cauchy(0, 2)", class = "sd")
# 
# prior2 <- set_prior("student_t(3, 0, 2.5)", class = "sd")
# prior4 <- set_prior("student_t(3, 0, 1.5)", class = "sd")

# prior3 <- set_prior("normal(0, 1)", class = "sigma")


sim_model = brms::brm(
  formula = bf(
    value ~ 0 + name + (1 | pps), 
    sigma ~ 0 + name
    ),
  data = data_frame(value = 1,name=1,pps=1),
  # data = dat,
  chains = 2,
  cores = 1,
  iter = 2500,
  prior = c(set_prior("normal(0, 1)", class = "b"),
            set_prior("normal(0, 1)", class = "sd")),
  # sample_prior = "only"
  # backend = "cmdstanr",
  # threads = threading(4)
)

# Create Parameter Table ---------------------------------------------------

count_so_far = function(x){
  # x = params_list$n_items
  out = sapply(1:length(x), function(i) length(which(x[1:i]==x[i])))
  return(out)
}

# Example of creating a list of all combinations
params_list <- expand.grid(
  tau_equivalence = c(TRUE),
  sample_sizes = c(50, 250, 500, 1000),
  n_items = c(3, 4, 6, 12),
  run_rep = 1:250
) 

params_list$loadings_set = count_so_far(params_list$sample_sizes)

loadings_list = list()

for(i in 1:max(params_list$loadings_set)){
  x = params_list %>%
    filter(loadings_set == i) 
  
  if (length(unique(x$n_items)) !=1 ) {stop("ERROR")}
  if (length(unique(x$tau_equivalence)) !=1 ) {stop("ERROR")}
  
  if (unique(x$tau_equivalence)==TRUE)  { loadings_list[[i]] = rep(runif(1,.01,.99), unique(x$n_items)) }
  if (unique(x$tau_equivalence)==FALSE)  { loadings_list[[i]]  = runif(unique(x$n_items), .01, .99) }
}

params_list = params_list %>%
              arrange(loadings_set)

saveRDS(params_list, file = file.path("results","3_params_list.Rds"))

# seeds = sample(1:10000, nrow(params_list), replace = FALSE)

# Future Approach --------------------------------------------------------------

future::plan(future::multisession(workers = 8))

time_a = Sys.time()
results <- future.apply::future_lapply(future.seed = 10, 1:nrow(params_list), function(i) {
  # results <- pbapply::pblapply(cl = cl, 1:6, function(i) {
  run_factor_sim(
    i = i,
    n = params_list$sample_sizes[i], 
    n_items = params_list$n_items[i], 
    loadings = loadings_list[[params_list$loadings_set[i]]]
  )
}
)
time_b = Sys.time()
time_b - time_a

future::plan(future::sequential())
save(results, file = file.path("results","results_b.Rds"))


# 4000 runs - 18.4 hours

# Parallel Approach OLD! ------------------------------------------------------------

# parallel::stopCluster(cl)
cl <- parallel::makeCluster(4)
parallel::clusterExport(cl, varlist = c("run_factor_sim", "loadings_list", "params_list", "sim_model",
                                        "sim_factor_stnd","calc_r_brms"))
parallel::clusterEvalQ(cl, {
  library(tidyverse)
  library(brms)
  library(tidybayes)
  library(ggdist)
  
})


time_a = Sys.time()
# This approach is inefficient: Time difference of 1.021054 hours
results <- parallel::parLapply(cl, 1:nrow(params_list), function(i) {
  run_factor_sim(
    i = i,
    n = params_list$sample_sizes[i], 
    n_items = params_list$n_items[i], 
    loadings = loadings_list[[params_list$loadings_set[i]]]
  )
}
)
time_b = Sys.time()
time_b - time_a
parallel::stopCluster(cl)
save(results, file = file.path("results","results_a.Rds"))





# Check results -----------------------------------------------------------------

results[[1]]

results[[3]]$

lapply(results, function(x) x$diagnostics)

i = 8
n = params_list$sample_sizes[i]
n_items = params_list$n_items[i]
loadings = loadings_list[[params_list$loadings_set[i]]]
loadings = c(.1,.1, .9)
set.seed(11)

n = 400
n_items = 3
loadings = c(.1,.1,.9)

# FIND A WAY TO GET WARNINGS FROM MODEL! 



test = brms::brm(
  formula = bf(
    value ~ 0 + name + (0 + name | pps), 
    sigma ~ 0 + name
  ),
  data = dat_long,
  # data = dat,
  chains = 2,
  cores = 2,
  iter = 2500,
  prior = c(set_prior("normal(0, 1)", class = "b"),
            set_prior("normal(0, 1)", class = "sd")),
  # sample_prior = "only"
  backend = "cmdstanr",
  threads = threading(4)
)

test = brms::brm(
  formula = bf(
    value ~ 0 + name + (0 + name | pps), 
    sigma ~ 0 + name
  ),
  data = dat_long,
  # data = dat,
  chains = 2,
  cores = 1,
  iter = 2500,
  prior = c(set_prior("normal(0, 1)", class = "b"),
            set_prior("normal(0, 1)", class = "sd")),
  # sample_prior = "only"
  # backend = "cmdstanr",
  # threads = threading(4)
)

test




params_list
loadings_list
# Ensure future and furrr are loaded and the plan is set
future::plan(future::multisession(workers = 4))

# Run simulations in parallel
results <- future_map(params_list, ~{
  purrr::map(.x$sample_sizes, ~ run_factor_sim(n = .x, n_items = .x$n_items, loadings = loadings_list[[.x$loadings]]))
}, .progress = TRUE) %>%
  unlist(recursive = FALSE)



future_lapply










# Run a simulation NOT PARALLEL-------------------------------------------------------------
# rm(n, n_items, loadings,error_variances, weights, intercepts, dat, dat_long,out, internal_results)
set.seed(seed_val)
results = list()
for (i in 1:run_repetitions){
  n_items    = sample(c(2,4,8,16),1)
  for (t in tau_equivalence){
    if (t==TRUE)  { loadings = rep(runif(1,.01,.99), n_items) }
    if (t==FALSE) { loadings = runif(n_items, .01, .99) }
    for(n in sample_sizes){
      if (F){
        cat("\nn = ", n,
            "\nn_items = ", n_items,
            "\nloadings = ", loadings,
            "\n")
      }
      results[[length(results)+1]] = run_factor_sim(
        n        = n,
        n_items  = n_items,
        loadings = loadings
      )
      print(length(results))
    }
  } 
}



# Testing area DO NOT RUN ------------------------------------------------------

if (FALSE){
  internal_results0 =  brms::brm(
    formula = bf(
      value ~ 0 + name + (1 | pps), 
      sigma ~ 0 + name
    ),
    # data = data_frame(value = 1,name=1,pps=1),
    data = dat_long,
    chains = 2,
    cores = 2,
    iter = 3000,
    # prior = c(prior1, prior2, prior3),
    # sample_prior = "only"
    # backend = "cmdstanr",
    # threads = threading(4)
  )
  
  calc_r_brms(internal_results0)
  prior_summary(sim_model)
}

dat %>%
  select(starts_with("X")) %>%
  psych::fa()

dat %>%
  select(starts_with("X")) %>%
  psych::alpha()

# l = loadings[2]
# var(dat$X2) = l^2*0.54^2 + exp(-0.22)
# 
# sqrt((var(dat$X2) - exp(-0.22))/0.54^2)

print(internal_results, digits =4)

x = summary(internal_results)
sd_intercept = summary(internal_results)$random$pps[1] %>% as.numeric()

error_variances = 
x$fixed %>% 
  rownames_to_column() %>%
  slice(grep("sigma",.$rowname)) %>%
  pull(Estimate) %>%
  exp() %>%
  `^`(.,2)

sqrt(sd_intercept^2/(sd_intercept^2+error_variances))

sqrt(0.4094^2/(0.4094^2+exp(-0.0300)^2))
sqrt(0.4094^2/(0.4094^2+exp(-0.1088)^2))
sqrt(0.4094^2/(0.4094^2+exp(-0.1547)^2))
# sqrt(0.4094^2/(0.4094^2+exp(-0.2239)^2))

exp(-0.0300)^2
exp(-0.1088)^2
exp(-0.1547)^2


0.5378^2/(0.5378^2+exp(0.1917))


loadings^2
