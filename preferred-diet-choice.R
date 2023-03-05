# --- Preferred dietary choice 

#  A simulation which includes 4 protein: carbohydrate diets and a no diet option 


# Reading the different diets in 
diets <- c("8:1", "2:1", "1:8", "1:2", "nodiet")

# The mean average of each of the diets from a previous experiment 
meanpreference <- c(2.74, 1.73, 1.36, 0.98, 3.19)

# the amount of replicate feeding assays 
replicates <- 10

# the number of flies in each feeding assay 
flies <- 10

# putting the data into columns 
dietpreference <- matrix(0, nrow = replicates, ncol = length(diets), dimnames = list(NULL, diets))

# Loop over each replicate
for (i in 1:replicates) {
  
  # Generate a set of random choices for each fly based on the probabilities
  choices <- sample(diets, size = flies, replace = TRUE, prob = meanpreference)
  
  # Count the number of flies on each diet patch and store the results
  for (j in 1:length(diets)) {
    dietpreference[i, j] <- sum(choices == diets[j])
  }
}

# Calculate the mean number of flies on each diet patch across all replicates
mean_dietpreference <- colMeans(dietpreference)

# Identify the preferred diet
preferred_diet <- diets[which.max(mean_dietpreference)]

# Print the results
cat("Mean number of flies on each diet patch:", mean_results, "\n")
cat("Preferred diet:", preferred_diet, "\n")

mean_dietpreference

preferred_diet
