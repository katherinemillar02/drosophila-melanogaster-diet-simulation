
#  starting with basics 
# generating random numbers from 10 flie s

x <- 1:10

sample(x, 100, replace = TRUE)

# taking 100 values from mated female (8:1 diet preference) experiment 

rnorm(n = 100, mean = 2.74, sd = 1.69)



# Set seed for reproducibility
set.seed(123)

# Define sample size
n <- 10

# Simulate rolling a fair die n times
sample <- sample(1:10, size = n, replace = TRUE)

# Print the results
print(sample)




# Set seed for reproducibility
set.seed(123)

# Define sample size and diets
n <- 5
diets <- c("8:1", "2:1", "1:2", "1:8", "nodiet")

# Randomly assign individuals to diets
assign <- sample(diets, size = n, replace = TRUE)

# Simulate weight loss for each individual on their assigned diet
preference <- numeric(n)
for (i in 1:n) {
  if (assign[i] == "8:1") {
    preference[i] <- rnorm(1, mean = 2.74, sd = 1.69)
  } else if (assign[i] == "2:1") {
    preference[i] <- rnorm(1, mean = 1.73, sd = 1.26)
  } else if (assign[i] == "1:2") {
    preference[i] <- rnorm(1, mean = 0.98, sd = 1.05)
  } else if (assign[i] == "1:8") {
    preference[i] <- rnorm(1, mean = 1.36, sd = 1.27)
  } else if (assign[i] == "nodiet") {
    preference[i] <- rnorm(1, mean = 3.19, sd = 10.088)
  }
  }


# Print the results
results <- data.frame(diet = assign, preference = preference)
print(results)



# Set seed for reproducibility
set.seed(123)

# Define number of replicates, diets, and flies
n_replicates <- 10
n_diets <- 4
n_flies <- 10

# Simulate diet preference for each fly in each replicate
preference <- array(dim = c(n_replicates, n_flies, n_diets))
for (r in 1:n_replicates) {
  for (f in 1:n_flies) {
    # Simulate initial preference for each diet
    initial_pref <- runif(n_diets)
    initial_pref <- initial_pref / sum(initial_pref)
    
    # Simulate change in preference after exposure to each diet
    for (d in 1:n_diets) {
      if (d == 8:1) {
        preference[r,f,d] <- rnorm(10, mean = initial_pref[d] + 2.74, sd = 1.69)
      } else if (d == 2:1) {
        preference[r,f,d] <- rnorm(10, mean = initial_pref[d] + 1.73, sd = 1.26)
      } else if (d == 1:2) {
        preference[r,f,d] <- rnorm(10, mean = initial_pref[d] - 0.98, sd = 1.05)
      } else  if (d == 1:8)  {
        preference[r,f,d] <- rnorm(10, mean = initial_pref[d] - 1.36, sd = 1.27)
      }
    }
    
    # Normalize preference values
    preference[r,f,] <- preference[r,f,] / sum(preference[r,f,])
  }
}

# Print the results for the first replicate and fly
print(preference[1,1,])










# Set seed for reproducibility
set.seed(123)

# Define number of replicates, diets, and flies
n_replicates <- 10
n_diets <- 4
n_flies <- 10

# Set diet ratios
diet_ratios <- c("8/1", "2/1", "1/2", "1/8")

# Simulate diet preference for each fly in each replicate
preference <- array(dim = c(n_replicates, n_flies))
for (r in 1:n_replicates) {
  for (f in 1:n_flies) {
    # Simulate initial preference for each diet
    initial_pref <- runif(n_diets)
    initial_pref <- initial_pref / sum(initial_pref)
    
    # Simulate change in preference after exposure to each diet
    for (d in 1:n_diets) {
      preference[r,f] <- preference[r,f] + rnorm(1, mean = initial_pref[d] * diet_ratios[d], sd = 0.05)
    }
  }
}

# Compute the preferred diet for each fly over all replicates
preferred_diet <- apply(preference, MARGIN = 2, FUN = function(x) {
  which.max(x)
})

# Count the number of flies that prefer each diet
diet_counts <- table(preferred_diet)

# Print the diet counts
print(diet_counts)


