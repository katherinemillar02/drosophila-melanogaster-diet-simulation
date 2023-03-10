
#------- OLD SCRIPT DO NOT USE 









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
n <- 10
diets <- c("8:1", "2:1", "1:2", "1:8", "nodiet")


assign <- sample(diets, size = n, replace = TRUE)


preference <- numeric(n)
for (i in 1:n) {
  if (assign[i] == "8:1") {
    preference[i] <- rnorm(n = 10, mean = 2.74, sd = 1.69)
  } else if (assign[i] == "2:1") {
    preference[i] <- rnorm(n = 10, mean = 1.73, sd = 1.26)
  } else if (assign[i] == "1:2") {
    preference[i] <- rnorm(n = 10, mean = 0.98, sd = 1.05)
  } else if (assign[i] == "1:8") {
    preference[i] <- rnorm(n = 10, mean = 1.36, sd = 1.27)
  } else if (assign[i] == "nodiet") {
    preference[i] <- rnorm(n = 10, mean = 3.19, sd = 10.088)
  }
}

# [i] is used to index or subset a vector or a list. It selects the element at position i in the vector or lis


# Print the results
diet_simulation <- data.frame(diet = assign, preference = preference)

diet_simulation

#set.seed means numbers are the same each time 


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


#-----------------------------------------


# Define the parameters of the simulation
diets <- c("8:1", "2:1", "1:2", "1:8", "0:0")
meansofdiets <- c(2.74, 1.73, 0.98, 1.36, 3.19)
replicateplates <- 10

numberflies <- 10

# Create an empty matrix to store the number of times each choice was made
results <- matrix(0, nrow = replicateplates, ncol = length(diets), dimnames = list(NULL, choices))

# Loop over each replicate
for (i in 1:n_replicates) {
  # Generate a set of random choices based on the probabilities
  choices_made <- sample(diets, size = numberflies, replace = TRUE, prob = meansofdiets)
  
  # Count the number of times each choice was made
  for (j in 1:length(diets)) {
    results[i, j] <- sum(choices_made == diets[j])
  }
}

# Calculate the proportion of times each choice was made
prop_choices <- colMeans(results)

# Print the estimated preference for each choice
print(prop_choices)

prop_choices











#  simulation which includes no diet option 
# Define the diets and their probabilities
diets <- c("8:1", "2:1", "1:8", "1:2", "nodiet")
probs <- c(2.74, 1.73, 1.36, 0.98, 3.19)

# Define the number of replicates and flies per replicate
n_replicates <- 10
n_flies <- 10

# Initialize an empty matrix to store the results
results <- matrix(0, nrow = n_replicates, ncol = length(diets), dimnames = list(NULL, diets))

# Loop over each replicate
for (i in 1:n_replicates) {
  
  # Generate a set of random choices for each fly based on the probabilities
  choices <- sample(diets, size = n_flies, replace = TRUE, prob = probs)
  
  # Count the number of flies on each diet patch and store the results
  for (j in 1:length(diets)) {
    results[i, j] <- sum(choices == diets[j])
  }
}

# Calculate the mean number of flies on each diet patch across all replicates
mean_results <- colMeans(results)

# Identify the preferred diet
preferred_diet <- diets[which.max(mean_results)]

# Print the results
cat("Mean number of flies on each diet patch:", mean_results, "\n")
cat("Preferred diet:", preferred_diet, "\n")

mean_results

preferred_diet



preferred_diet

 
