#' **Distribution Examples with Mosaic Package**
# Load necessary libraries

library(mosaic)

## Normal Distribution (Section B.3.5)

### Visualizing Normal Distributions


# Plot standard normal distribution
gf_dist("norm", mean = 0, sd = 1) %>%
  gf_labs(title = "Standard Normal Distribution N(0,1)") + theme_minimal()

# Compare different normal distributions
gf_dist("norm", mean = 0, sd = 1, color = "blue") %>%
  gf_dist("norm", mean = 2, sd = 1, color = "red") %>%
  gf_dist("norm", mean = 0, sd = 2, color = "green") %>%
  gf_labs(title = "Comparing Normal Distributions") + theme_minimal()


### Computing Probabilities


# P(X <= 1.96) for standard normal
xpnorm(1.96, mean = 0, sd = 1)

# This shows both the probability and the visualization
# Result: 0.975 (matches the textbook example)

# P(X > 1.96) 
1 - pnorm(1.96, mean = 0, sd = 1)
# or equivalently:
xpnorm(1.96, mean = 0, sd = 1, lower.tail = FALSE)


### Finding Quantiles

# Find the 97.5th percentile of standard normal
xqnorm(0.975, mean = 0, sd = 1)
# Result: 1.96

# Find the 95th percentile of N(100, 15²)
xqnorm(0.95, mean = 100, sd = 15)


## Chi-Square Distribution (Section B.3.6)

### Visualizing Chi-Square Distributions

# Plot chi-square with different degrees of freedom
gf_dist("chisq", df = 4, color = "blue") %>%
  gf_dist("chisq", df = 10, color = "red") %>%
  gf_dist("chisq", df = 30, color = "green") %>%
  gf_labs(title = "Chi-Square Distributions with df = 4, 10, 30") + theme_minimal()


### Computing Chi-Square Probabilities

# Find P(X > 9.31) for chi-square with df = 2 (like Image 3)
xpchisq(9.31, df = 2, lower.tail = FALSE)

# Find the 95th percentile of chi-square with df = 10
xqchisq(0.95, df = 10)


## t-Distribution (Section B.3.7)

### Comparing t and Normal

# Create the data for the plot
# We need a sequence of x-values where we will calculate the density
x_vals <- seq(-5, 5, length.out = 500)

# Create a data frame holding the x-values and the calculated densities
# for both the t-distribution and the normal distribution.
plot_data <- data.frame(
  x = x_vals,
  t_density = dt(x_vals, df = 3),
  norm_density = dnorm(x_vals, mean = 0, sd = 1)
)

# Create the plot
ggplot(plot_data, aes(x = x)) +
  # Add the line for the t-distribution
  geom_line(aes(y = t_density, color = "t (df=3)"), linewidth = 1) +
  
  # Add the line for the normal distribution
  geom_line(aes(y = norm_density, color = "Standard Normal"), linewidth = 1) +
  
  # Set the colors manually for clarity
  scale_color_manual(values = c("t (df=3)" = "red", "Standard Normal" = "blue")) +
  
  # Add titles and labels
  labs(
    title = "t(3) vs Standard Normal",
    x = "Value",
    y = "Density",
    color = "Distribution" # This renames the legend title
  ) + theme_minimal()


### t-Distribution Calculations

# Critical value for 95% confidence with df = 20
xqt(0.975, df = 20)  # Two-tailed
# Result: 2.086

# P-value calculation
xpt(2.5, df = 15, lower.tail = FALSE) * 2  # Two-tailed test


## F-Distribution (Section B.3.8)

### F-Distribution Visualization

# Plot F-distribution
gf_dist("f", df1 = 8, df2 = 20) %>%
  gf_labs(title = "F-Distribution with df1=8, df2=20") + theme_minimal()

# Find 95th percentile
xqf(0.95, df1 = 8, df2 = 20)
# Result: 2.447 (matches textbook)


## Binomial Distribution (Section B.3.2)

### Binomial Calculations

# P(X = 3) for Binomial(n=10, p=0.3)
dbinom(3, size = 10, prob = 0.3)

# P(X <= 3) for Binomial(n=10, p=0.3)
pbinom(3, size = 10, prob = 0.3)

# Visualize binomial distribution
gf_dist("binom", size = 10, prob = 0.3) %>%
  gf_labs(title = "Binomial Distribution n=10, p=0.3") + theme_minimal()


## Simulation Examples

### Central Limit Theorem Demonstration

# Simulate sample means from uniform distribution
sample_means <- do(1000) * mean(runif(30, min = 0, max = 1))

# Plot the distribution of sample means
gf_histogram(~mean, data = sample_means, bins = 30) %>%
  gf_labs(title = "Distribution of Sample Means (n=30)",
          subtitle = "Population: Uniform(0,1)") + theme_minimal()

# Compare to theoretical normal
favstats(~mean, data = sample_means)


### Bootstrap Simulation

# Using a dataset (example with built-in data)
data(Galton, package = "mosaicData")

# Bootstrap sampling
bootstrap_means <- do(1000) * mean(~height, 
                                   data = resample(Galton))

# Visualize bootstrap distribution
gf_histogram(~mean, data = bootstrap_means) %>%
  gf_labs(title = "Bootstrap Distribution of Sample Mean Height")

# 95% Confidence interval
quantile(~mean, data = bootstrap_means, probs = c(0.025, 0.975))

# Regression Towards the Mean Example
# For the x-axis, we'll create the "mid-parent" height, which is the
# average of the father's and mother's height.
galton_data <- Galton %>%
  mutate(midparent_height = (father + mother) / 2)

# Step 3: Create the scatter plot and add the regression line
ggplot(galton_data, aes(x = midparent_height, y = height)) +
  
  # Add the points for the scatter plot.
  # 'alpha = 0.3' makes the points semi-transparent to help visualize dense areas.
  geom_point(alpha = 0.3) +
  
  # Add the regression line.
  # 'method = "lm"' specifies a linear model.
  # 'se = FALSE' hides the confidence interval ribbon for a cleaner look.
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  
  # Add labels and a title for clarity
  labs(
    title = "Galton's Parent and Child Height Data",
    subtitle = "Illustrating Regression Towards the Mean",
    x = "Mid-Parent Height (inches)",
    y = "Adult Child Height (inches)",
    caption = "Data source: Galton dataset from the mosaicData package"
  ) +
  
  # Force the plot to have a 1:1 aspect ratio (a square plotting area).
  # This makes it visually clear that the regression line is not at a 45-degree angle.
  coord_fixed(ratio = 1) +
  
  # Apply a clean, minimal theme to the plot
  theme_minimal()

#' The Galton data is the famous, original example of regression towards the mean.
#' The concept and the term "regression" itself originate from Sir Francis Galton's work on this very dataset in the 1880s.
#' Galton collected data on the heights of adult children and their parents to study heredity.
#' His key finding, published in his 1886 paper "Regression Towards Mediocrity in Hereditary Stature," 
#' was a phenomenon he described as "regression towards mediocrity," which is now known as regression to the mean.
#' Here’s what he observed from the data:
#' Tall parents tended to have tall children, but on average, those children were shorter than their parents.
#' Short parents tended to have short children, but on average, those children were taller than their parents.
#' In other words, the children's heights tended to "regress" or drift back towards the average height of the population.
#' An exceptionally tall parent was likely to have a child who was taller than average, but less exceptionally tall than the parent.


### Random Sampling Demonstration

# Flip coins and see approach to theoretical probability
coin_flips <- do(100) * rflip(50)
str(coin_flips)
# create index from row numbers
coin_flips$index <- 1:nrow(coin_flips)

gf_point(heads ~ index, data = coin_flips) %>%
  gf_hline(yintercept = 25, color = "red") %>%
  gf_labs(title = "Number of Heads in 50 Flips",
          y = "Number of Heads", x = "Trial Number")


### Law of Large Numbers

# Demonstrate law of large numbers
sample_sizes <- c(10, 50, 100, 500, 1000)
results <- NULL

for(n in sample_sizes) {
  trials <- do(100) * rflip(n)
  trials$n <- n
  results <- rbind(results, trials)
}

gf_boxplot(prop ~ factor(n), data = results) %>%
  gf_hline(yintercept = 0.5, color = "red") %>%
  gf_labs(title = "Proportion of Heads vs Sample Size",
          x = "Sample Size", y = "Proportion of Heads")

