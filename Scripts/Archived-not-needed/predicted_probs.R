# Extract fixed effects
fixef_m2 <- fixef(m2)

# Extract random effects for a species (e.g., "American Crow")
ranef_common_crow <- ranef(m2, condVar = TRUE)$cond$common["American Crow", ]

# Calculate the log-odds for "American Crow" given fixed and random effects
log_odds_crow <- fixef_m2[1]$cond[1] + ranef_common_crow[1] + ranef_common_crow[2]  # intercept + eclipse effect

# Convert log-odds to probability
prob_crow <- 1 / (1 + exp(-log_odds_crow))
prob_crow


# Extract fixed effects for the conditional model
fixef_m2_cond <- fixef(m2)$cond

# Extract random effects for each species (all common species)
ranef_common <- ranef(m2, condVar = TRUE)$cond$common

# Create a data frame to store the results
pred_probs <- data.frame(species = rownames(ranef_common), prob_eclipse_0 = NA, prob_eclipse_1 = NA)

# Loop through each species and calculate probabilities
for (i in 1:nrow(ranef_common)) {
  species <- rownames(ranef_common)[i]
  ranef_values <- ranef_common[i, ]
  
  # Calculate log-odds for both levels of eclipse (0 and 1)
  log_odds_eclipse_0 <- fixef_m2_cond[1] + ranef_values[1]  # Intercept for eclipse = 0
  log_odds_eclipse_1 <- fixef_m2_cond[1] + fixef_m2_cond[2] + ranef_values[1] + ranef_values[2]  # Intercept + eclipse effect for eclipse = 1
  
  # Convert log-odds to probabilities
  prob_eclipse_0 <- 1 / (1 + exp(-log_odds_eclipse_0))
  prob_eclipse_1 <- 1 / (1 + exp(-log_odds_eclipse_1))
  
  # Store the probabilities
  pred_probs$prob_eclipse_0[i] <- prob_eclipse_0
  pred_probs$prob_eclipse_1[i] <- prob_eclipse_1
}

# Ensure the columns are numeric, not lists
pred_probs$prob_eclipse_0 <- unlist(pred_probs$prob_eclipse_0)
pred_probs$prob_eclipse_1 <- unlist(pred_probs$prob_eclipse_1)

# View the corrected data frame
head(pred_probs)


# View the results
head(pred_probs)

