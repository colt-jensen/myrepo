library(pwr)
library(MonteCarlo)
library(ggplot2)
library(tidyverse)

#2x2
pwr.anova.test(k = 4,  # 4 groups in 2x2 factorial
               f = 0.1,  # Medium effect size
               sig.level = 0.05,  # Alpha level
               power = 0.8)  # Desired power

#concise test
pwr.anova.test(k = 8,  # 8 groups in 2x2x2 factorial
               f = 0.1,  # Medium effect size
               sig.level = 0.05,
               power = 0.8)

#Interpreting Results - See the Note: n is the number in each group
#For example 274 x 4 = 1,094 observations for the 2x2 experiment 
# OR 180 x 8 = 1,440 for a 2x2x2 experiment



#source code: https://philippmasur.de/2024/05/28/what-is-statistical-power-and-how-can-i-conduct-power-analysis-in-r/

# Estimate necessary sample size
pwr.r.test(r = .1,                      # Assumed effect size
           sig.level = .05,             # alpha error
           power = .95,                 # Power (1 - beta error) 
           alternative = "two.sided")   # Type of test


# Ensure reproducibility
set.seed(42)

# Define parameters
n <- 500
means <- c(2.5, 2.75, 3, 4)  # a1b1, a2b1, a1b2, a2b2
sd <- 1

# Simulate data
dv <- rnorm(n, mean = means , sd = sd) %>% round(0)
iv1 <- rep(c("a1", "a2"),each = 1, n/2)
iv2 <- rep(c("b1", "b2"), each = 2, n/4)
d <-data.frame(dv, iv1, iv2)


# Check data simulation
d %>%
  group_by(iv1, iv2) %>%
  summarise(m = mean(dv)) %>%
  ggplot(aes(x = factor(iv1), y = m, color = iv2, group = iv2)) +
  geom_point(size = 4) +
  geom_line(size = 1) +
  ylim(1, 5) +
  theme_classic() +
  labs(y = "DV", x = "IV 1",
       color = "IV 2")

# Simulation function
sim_func <- function(n = 500, 
                     means = c(2.5, 2.75, 3, 4),
                     sd = 1) {
  
  # Simulate data
  dv <- round(rnorm(n, mean = means , sd = sd),0)
  iv1 <- rep(c("a1", "a2"),each = 1, n/2)
  iv2 <- rep(c("b1", "b2"), each = 2, n/4)
  d <-data.frame(dv, iv1, iv2)
  
  # Fit models
  fit1 <- lm(dv ~ iv1, d)     # Main effet of IV1
  fit2 <- lm(dv ~ iv2, d)     # Main effect of IV2
  fit3 <- lm(dv ~ iv1*iv2, d) # Interaction between both
  
  # Extract p-values and compute significance
  p_1 <- summary(fit1)$coef[2,4]
  sig_1 <- ifelse(p_1 < .05, TRUE, FALSE)
  p_2 <- summary(fit2)$coef[2,4]
  sig_2 <- ifelse(p_2 < .05, TRUE, FALSE)
  p_3 <- summary(fit3)$coef[4,4]
  sig_3 <- ifelse(p_3 < .05, TRUE, FALSE)
  
  
  # return values as list
  return(list("p_1" = p_1,
              "sig_1" = sig_1,
              "p_2" = p_2,
              "sig_2" = sig_2,
              "p_3" = p_3,
              "sig_3" = sig_3))
}

# Parameters that should vary across simulations
n_grid <- seq(100, 600, 100) 
sd_grid <- c(1, 1.5, 2) 

# Collect simulation parameters in list
(param_list <- list("n" = n_grid,
                    "sd" = sd_grid))

result <- MonteCarlo(func = sim_func,             # pass test function
                     nrep = 1000,                 # number of tests
                     ncpus = 4,                   # number of cores to be used
                     param_list = param_list, )

df <- MakeFrame(result)
head(df)

df %>%
  dplyr::select(-contains("p")) %>%
  gather(key, value, -n, -sd) %>%
  group_by(n, sd, key) %>%
  summarize(power = sum(value)/length(value)*100) %>%
  ggplot(aes(x = n, y = power, color = key)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 80, linetype = "dashed") +
  geom_hline(yintercept = 95, linetype = "dashed") +
  facet_wrap(~sd) +
  theme_bw() +
  labs(title = "Power Curves",
       x = "sample size (n)", 
       y = "power (1-beta)", 
       color = "type of effect",
       caption = "Note: facets represent different standard deviations")

#modified for 2x2x2 factorial experiment

# Ensure reproducibility
set.seed(42)

# Define parameters
n <- 500
means <- c(2.5, 2.75, 3, 3.5, 4, 4.25, 4.5, 5)  # Means for 2x2x2 design
sd <- 1

# Simulate data for 2x2x2 design
dv <- round(rnorm(n, mean = rep(means, each = n/8), sd = sd), 0)
iv1 <- rep(c("a1", "a2"), each = n/2)
iv2 <- rep(c("b1", "b2"), times = n/2)
iv3 <- rep(c("c1", "c2"), times = n/4)
d <- data.frame(dv, iv1, iv2, iv3)

# Check data simulation
d %>%
  group_by(iv1, iv2, iv3) %>%
  summarise(m = mean(dv)) %>%
  ggplot(aes(x = factor(iv1), y = m, color = interaction(iv2, iv3), group = interaction(iv2, iv3))) +
  geom_point(size = 4) +
  geom_line(size = 1) +
  ylim(1, 5) +
  theme_classic() +
  labs(y = "DV", x = "IV 1", color = "IV2 x IV3")

# Simulation function for 2x2x2 factorial
sim_func_2x2x2 <- function(n = 500, 
                           means = c(2.5, 2.75, 3, 3.5, 4, 4.25, 4.5, 5), 
                           sd = 1) {
  
  # Simulate DV with correct length
  dv <- round(rnorm(n, mean = rep(means, each = n/length(means)), sd = sd), 0)
  
  # Ensure correct factor assignment
  iv1 <- rep(c("a1", "a2"), each = n/2)
  iv2 <- rep(c("b1", "b2"), each = n/2)
  iv3 <- rep(c("c1", "c2"), each = n/4)
  
  # Convert to factors
  d <- data.frame(dv, iv1 = factor(iv1), iv2 = factor(iv2), iv3 = factor(iv3))
  
  # Fit models
  fit1 <- lm(dv ~ iv1, d)     
  fit2 <- lm(dv ~ iv2, d)     
  fit3 <- lm(dv ~ iv3, d)     
  fit4 <- lm(dv ~ iv1*iv2*iv3, d) # Full factorial model
  
  # Extract p-values dynamically
  coef_names <- rownames(summary(fit4)$coef)
  three_way_interaction <- grep("iv1a2:iv2b2:iv3c2", coef_names)  # Adjust factor labels if necessary
  
  p_1 <- summary(fit1)$coef[2, 4]
  sig_1 <- ifelse(!is.na(p_1) & p_1 < .05, TRUE, FALSE)
  
  p_2 <- summary(fit2)$coef[2, 4]
  sig_2 <- ifelse(!is.na(p_2) & p_2 < .05, TRUE, FALSE)
  
  p_3 <- summary(fit3)$coef[2, 4]
  sig_3 <- ifelse(!is.na(p_3) & p_3 < .05, TRUE, FALSE)
  
  # Ensure sig_4 is always defined
  if (length(three_way_interaction) > 0) {
    p_4 <- summary(fit4)$coef[three_way_interaction, 4]
    sig_4 <- ifelse(!is.na(p_4) & p_4 < .05, TRUE, FALSE)
  } else {
    p_4 <- NA
    sig_4 <- FALSE  # Change from NA to FALSE to ensure plotting
  }
  
  return(list("p_1" = p_1, "sig_1" = sig_1,
              "p_2" = p_2, "sig_2" = sig_2,
              "p_3" = p_3, "sig_3" = sig_3,
              "p_4" = p_4, "sig_4" = sig_4))
}


# Parameters that should vary across simulations
n_grid <- seq(100, 600, 100) 
sd_grid <- c(1, 1.5, 2) 

# Collect simulation parameters in list
(param_list <- list("n" = n_grid, "sd" = sd_grid))

# Run Monte Carlo simulation for 2x2x2 factorial
result_2x2x2 <- MonteCarlo(func = sim_func_2x2x2, 
                           nrep = 1000, 
                           ncpus = 4, 
                           param_list = param_list)

df_2x2x2 <- MakeFrame(result_2x2x2)

# Power Analysis Comparison
df_2x2x2 %>%
  dplyr::select(-contains("p")) %>%
  gather(key, value, -n, -sd) %>%
  group_by(n, sd, key) %>%
  summarize(power = sum(value)/length(value)*100) %>%
  ggplot(aes(x = n, y = power, color = key)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 80, linetype = "dashed") +
  geom_hline(yintercept = 95, linetype = "dashed") +
  facet_wrap(~sd) +
  theme_bw() +
  labs(title = "Power Curves for 2x2x2 Factorial Design",
       x = "sample size (n)", 
       y = "power (1-beta)", 
       color = "type of effect",
       caption = "Note: facets represent different standard deviations")
 