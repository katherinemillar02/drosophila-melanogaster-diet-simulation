# --- Preferred dietary choice 

#  A simulation which includes 4 protein: carbohydrate diets and a no diet option 




#-------------- Mated females ---------------


# Reading the different diets in 
diets <- c("8:1", "2:1", "1:8", "1:2", "nodiet")

# The mean average of each of the diets from a previous experiment 
meanpreference <- c(2.74, 1.73, 1.36, 0.98, 3.19)

# the amount of replicate feeding assays? or the amount of times to run the simulation?? 
replicates <- 10
# the number of flies in each feeding assay 
flies <- 10

# matrix = creates a matrix from the given set of values.
# putting the data into a 'matrix' with amount of times to repeat, diet names to include 
dietpreference <- matrix(0, nrow = replicates, ncol = length(diets), dimnames = list(NULL, diets))

# think can make it loop x amount of times here 


# Loop over each replicate

 for (i in 1:replicates) {choices <- sample(diets, size = flies, replace = TRUE, prob = meanpreference)
  for (j in 0:length(diets)) {
    dietpreference[i, j] <- sum(choices == diets[j])}}

# the simulation including diet names, overall amount of flies with the known mean preferences of a fly on each patch

# using the matrix to find the the mean diet preference of the replicates (10 or 100) using colMeans
mean_dietpreference <- colMeans(dietpreference)

# a simulation which gives the name of the preferred diet for a simulation 
preferred_diet <- diets[which.max(mean_dietpreference)]


# using cat function to print the simulation results 
cat("Mean number of flies on each diet patch:", mean_dietpreference, "\n")
cat("Preferred diet:", preferred_diet, "\n")

#  cat converts its arguments to character strings, concatenates them, separating
#  them by the given sep= string, and then prints them

# simulation for a mean diet preference of each diet 
mean_dietpreference
#------ proportion results for one fly? 
#  trying to repeat simulation 100 times 
proportionresults <- replicate(1000, choices)

# diet prportion 
prop.results <- prop.table(table(proportionresults))

preferred_diet
mean_dietpreference
prop.results


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


for (i in 1:replicatesv) {choicesv <- sample(dietsv, size = fliesv, replace = TRUE, prob = meanpreferencev)
  for (j in 1:length(dietsv)) {dietpreferencev[i, j] <- sum(choicesv == dietsv[j])}}


# Calculate the mean number of flies on each diet patch across all replicates
mean_dietpreferencev <- colMeans(dietpreferencev)

# Identify the preferred diet
preferred_dietv <- dietsv[which.max(mean_dietpreferencev)]


cat("Mean number of flies on each diet patch:", mean_dietpreferencev, "\n")
cat("Preferred diet:", preferred_dietv, "\n")

#  cat converts its arguments to character strings, concatenates them, separating
#  them by the given sep= string, and then prints them

cat("Preferred diet:", preferred_diet, "\n")
mean_dietpreference
cat("Preferred diet virgin:", preferred_dietv, "\n")

mean_dietpreferencev

sample(preferred_dietv, 100, replace = TRUE)

sample(preferred_dietv, 100, replace = TRUE)

replicate(100, mean_dietpreferencev)

replicate(100, preferred_dietv)

# issue is replicating the same code 

