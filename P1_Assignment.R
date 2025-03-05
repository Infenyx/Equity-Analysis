### Assignment 1: Project 1 ###

rm(list = ls()) # Clean up the memory

# Step 1: Set up input parameters: 
mu <- 0.2
sigma <- 0.5
W0 <- 1
N_Rounds <- 2500
N_Simulation <- 50
Weight_Min <- 0
Weight_Max <- 1
Search_Step <- 0.01

# Step 2: Set up a vector to store the average terminal wealth for each weight 
vec_All_Weights <- seq(Weight_Min, Weight_Max, by = Search_Step)
N_Of_Weights <- length(vec_All_Weights)

# Step 3: Triple loop
# Loop 1 goes over each possible weight. For each weight, get the average 
# terminal wealth after multiple simulations
# Set up a vector to store all the terminal wealth for each simulation 
vec_Avg_Terminal_Wealth <- matrix(0, N_Of_Weights, 1) 

for (i in 1:N_Of_Weights) {
  This_Weight <- vec_All_Weights[i] # Grab the i'th weight in vec_All_Weights
  
  # Loop 2 repeats the simulation for N_Simulation times
  vec_Terminal_Wealth <- matrix(0, N_Simulation, 1) # To store simulated terminal wealth
  
  for (j in 1:N_Simulation) {
    # Loop 3 goes over each round of bet with double or nothing
    vec_Wealth <- matrix(0, N_Rounds, 1)
    vec_Wealth[1] <- W0 # Starting wealth
    
    for (k in 2:N_Rounds) {
      # Simulate the return for this period: Normal with (mu, sigma)
      today_return <- rnorm(1, mean = mu, sd = sigma)
      
      # Update the wealth: Wealth * (1 + w * today_return)
      vec_Wealth[k] <- vec_Wealth[k-1] * (1 + This_Weight * today_return)
      
      # Wipe out test: If wealth is negative, set it to 0
      if (vec_Wealth[k] < 0) {
        vec_Wealth[k] <- 0
        break  # Stop the simulation if wealth is wiped out
      }
    }  # end of loop k
    
    # Save the terminal wealth of this simulation
    vec_Terminal_Wealth[j] <- vec_Wealth[length(vec_Wealth)] # The last wealth value
  }  # end of loop j
  
  # Calculate the average terminal wealth of all these simulations for a particular weight
  vec_Avg_Terminal_Wealth[i] <- mean(vec_Terminal_Wealth)
  print(i)
}  # end of loop i


# Step 4: Report the avg terminal wealth for different weights
# Plot the avg terminal wealth over different weights
plot(vec_All_Weights, vec_Avg_Terminal_Wealth, 
     xlab = "Weight", 
     ylab = "Average Terminal Wealth",
     main = "Terminal Wealth vs. Weight")
lines(vec_All_Weights, vec_Avg_Terminal_Wealth, col = "red", lwd = 3)


# Report the optimal weight
Opt_Weight_Index <- which.max(vec_Avg_Terminal_Wealth) # Find the index of opt weight
Opt_Weight <- vec_All_Weights[Opt_Weight_Index]  # Fetch the value of opt weight 
print(Opt_Weight)