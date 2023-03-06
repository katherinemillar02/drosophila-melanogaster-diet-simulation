# --- Preferred dietary choice 

#  A simulation which includes 4 protein: carbohydrate diets and a no diet option 




#-------------- Mated females ---------


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
cat("Mean number of flies on each diet patch:", mean_dietpreference, "\n")
cat("Preferred diet:", preferred_diet, "\n")

#  cat converts its arguments to character strings, concatenates them, separating
#  them by the given sep= string, and then prints them

mean_dietpreference

#-------------- Virgin females ------------

# Reading the different diets in 
dietsv <- c("8:1", "2:1", "1:8", "1:2", "nodiet")

# The mean average of each of the diets from a previous experiment 
meanpreferencev <- c(1.64, 1.81, 2.55, 1.46, 2.54)

# the amount of replicate feeding assays 
replicatesv <- 10

# the number of flies in each feeding assay 
fliesv <- 10

# putting the data into columns 
dietpreferencev <- matrix(0, nrow = replicatesv, ncol = length(dietsv), dimnames = list(NULL, dietsv))

# Loop over each replicate
for (i in 1:replicatesv) {   
  choicesv <- sample(dietsv, size = fliesv, replace = TRUE, prob = meanpreferencev)
  for (j in 1:length(dietsv)) {
    dietpreferencev[i, j] <- sum(choicesv == dietsv[j])
  }
}


# Calculate the mean number of flies on each diet patch across all replicates
mean_dietpreferencev <- colMeans(dietpreferencev)

# Identify the preferred diet
preferred_dietv <- dietsv[which.max(mean_dietpreferencev)]





# Print the results
cat("Mean number of flies on each diet patch:", mean_dietpreferencev, "\n")
cat("Preferred diet:", preferred_dietv, "\n")

#  cat converts its arguments to character strings, concatenates them, separating
#  them by the given sep= string, and then prints them

cat("Preferred diet:", preferred_diet, "\n")
mean_dietpreference
cat("Preferred diet:", preferred_dietv, "\n")

mean_dietpreferencev


