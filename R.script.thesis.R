################################################################################
######### R-script master's thesis on ChatGPT in Geography education ###########
################################################################################



############################# respondents ######################################
# Load library
library(dplyr)
library(readr)

# for visualising raw imported data and putting it into another excel sheet to use for analysis
rawresults <- read.csv("raw_survey_data.csv", header = T, sep = ",", quote = "\"")
age.gender <- rawresults[, c(9,981,982)]

# load data with the correct format for analysis, that we manually crafter with the previous code
d <- read.csv("survey.data.final.csv", header=T, sep = ";", quote = "\"")
d[d == -99 ] <- NA

# rename column names
age.gender <- age.gender %>%
  rename(
    ResponseID = ResponseId,  # Assuming the original column name is 'ID'
    age = Q2.2,        # Assuming the original column name is 'Age'
    gender = Q2.3   # Assuming the original column name is 'Gender'
  )

#  Extract unique ResponseIDs from dataset `d`
unique_responses <- d %>%
  select(ResponseID) %>%
  distinct()

# Merge unique responses with the age.gender dataframe
age.gender.respondents <- unique_responses %>%
  inner_join(age.gender, by = "ResponseID")

# remove -99 responses ;
age.gender.respondents[age.gender.respondents == -99 ] <- NA

# there was someone that filled in 3 for age. We removed this
age.gender.respondents[age.gender.respondents == 3 ] <- NA

# median age
age.gender.respondents$age <- as.numeric(age.gender.respondents$age)
summary(age.gender.respondents$age, na.rm=T)

# how many male?
age.gender.respondents %>%
  filter(gender == "Male") %>%
  summarize(count = n())

# how many female?
age.gender.respondents %>%
  filter(gender == "Female") %>%
  summarize(count = n())

# how many did not want to disclose it? 
age.gender.respondents %>%
  filter(gender == "I don't want to disclose it") %>%
  summarize(count = n())

# how many did not fill it out? 
age.gender.respondents %>%
  filter(gender == "") %>%
  summarize(count = n())

# how many answers were rated
Amount.Questions <- d %>%
  select(QuestionID) %>%
  distinct()

print(Amount.Questions)

# how many ratings (since every answer is rated 3 times, we divide the nr of rows by 3)
nrow(d)/3






###################### descriptive statistics table ############################
# Load libraries
library(dplyr)
library(tidyr)

# load data
d <- read.csv("survey.data.final.csv", header=T, sep = ";", quote = "\"")
d[d == -99 ] <- NA
#d<-na.omit(d)

str(d)

# make the scores from Integer to Numeric
d$Correctness <- as.numeric(d$Correctness)
d$Relevance <- as.numeric(d$Relevance)
d$Clarity <- as.numeric(d$Clarity)
d$Completeness <- as.numeric(d$Completeness)
d$Transparency <- as.numeric(d$Transparency)
# make independent variables as factors
d$QuestionID <- factor(d$QuestionID)
d$Domain <- factor(d$Domain)
d$Subdomain <- factor(d$Subdomain, levels = c("urban geography", "economic geography", "ids", "geomorphology", "hydrology", "oceanography", ordered = TRUE))
d$Bloom <- factor(d$Bloom, levels = c("remember", "understand", "apply", "analyze", "evaluate", "create"), ordered = TRUE)
d$Questiontype <- factor(d$Questiontype)
d$Source <- factor(d$Source)
d$Complexity <- factor(d$Complexity)

# Load necessary libraries
library(dplyr)
library(tidyr)


# Create a table with counts of each combination of Bloom and Subdomain, then divide counts by 3
bloom_subdomain_table <- d %>%
  count(Bloom, Subdomain) %>%
  mutate(n = n / 3) %>%  # Divide the counts by 3
  # Spread the data to create a wide format table with Bloom levels as columns
  pivot_wider(names_from = Bloom, values_from = n, values_fill = list(n = 0))

# Display the table
print(bloom_subdomain_table)


# Create a table with counts of each combination of Questiontype and Subdomain, then divide counts by 3
Questiontype_subdomain_table <- d %>%
  count(Questiontype, Subdomain) %>%
  mutate(n = n / 3) %>%  # Divide the counts by 3
  # Spread the data to create a wide format table with Bloom levels as columns
  pivot_wider(names_from = Questiontype, values_from = n, values_fill = list(n = 0))

# Display the table
print(Questiontype_subdomain_table)





############################### histograms #####################################
library(ggplot2)
library(dplyr)


# Reload the data
d <- read.csv("survey.data.final.csv", header = TRUE, sep = ";", quote = "\"")
d[d == -99] <- NA

# List of dependent variables (DVs)
dvs <- c("Correctness", "Relevance", "Clarity", "Completeness", "Transparency")

# Independent variable (IV)
iv <- "Source"

# Get unique levels of the Source variable and define distinct colors
sources <- unique(d[[iv]])
colors <- c("expert answer" = "lightblue", "chatgpt3.5" = "lightyellow", "chatgpt4o" = "lightpink")

# Filter rows where any cell contains the word "chatgpt4o, and replace with other levels of source
d_filtered <- d %>% filter(apply(., 1, function(row) any(grepl("chatgpt4o", row, ignore.case = TRUE))))

# Loop through each level of Source and create histograms for each DV
for (source in sources) {
  # Subset the data for the current source
  d_subset <- d_filtered %>% filter(!!sym(iv) == source)
  
  # Loop through each DV and create histograms
  for (dv in dvs) {
    # Check if the subset is not empty and contains non-missing values for the DV
    if (nrow(d_subset) > 0 && sum(!is.na(d_subset[[dv]])) > 0) {
      # Create the histogram
      p <- ggplot(d_subset, aes_string(x = dv)) +
        geom_histogram(binwidth = 1, alpha = 1, fill = colors[source], color = "black", center = 0.5) +
        labs(title = paste("Histogram of", dv, "for chatgpt3.5 question by", source), x = dv, y = "Count") +
        theme_minimal() +
        scale_x_continuous(breaks = seq(0, 5, by = 1), limits = c(0, 5)) +  
        scale_y_continuous(breaks = seq(0, ceiling(max(table(d_subset[[dv]])) / 5) * 5, by = 5)) +  
        theme(
          plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),  
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14)
        )
      
      print(p)
      
      # Save the plot (uncomment the line below to save the plots)
      # ggsave(paste0(dv, "_histogram_chatgpt3.5_", source, ".png"), plot = p, width = 8, height = 6, dpi = 300)
    } else {
      message(paste("Skipping", dv, "for source", source, "due to no non-missing values."))
    }
  }
}



############################### barplot ########################################
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Assuming your data is stored in a dataframe called 'd'
# List of dependent variables (DVs)
dvs <- c("Correctness", "Relevance", "Clarity", "Completeness", "Transparency")

# Independent variable (IV)
iv <- "Source"

# Define colors for the IV levels
colors <- c("expert answer" = "lightblue", "chatgpt3.5" = "lightyellow", "chatgpt4o" = "lightpink")

# Function to calculate means and standard errors
calculate_summary <- function(data, dv) {
  data %>%
    group_by(!!sym(iv)) %>%
    summarise(
      mean = mean(!!sym(dv), na.rm = TRUE),
      se = sd(!!sym(dv), na.rm = TRUE) / sqrt(n())
    ) %>%
    mutate(DV = dv)
}

# Calculate summary statistics for each DV
summary_data <- bind_rows(lapply(dvs, calculate_summary, data = d))

# Ensure the DVs are in the specified order
summary_data$DV <- factor(summary_data$DV, levels = dvs)

# Create the combined bar plot
p <- ggplot(summary_data, aes(x = DV, y = mean, fill = Source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.7, color = "black") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), position = position_dodge(width = 0.9), width = 0.25) +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line.y = element_line(color = "black"),  
    axis.ticks.y = element_line(color = "black"),  
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20), 
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 14),   
    legend.text = element_text(size = 14),  
    legend.title = element_text(size = 16) 
  ) +
  scale_fill_manual(values = colors)

# Print the plot
print(p)

# Save the plot as a high-quality PNG file
#ggsave("barplot_plot.png", plot = p, width = 10, height = 6, dpi = 500)


######################## Kruskal Wallis for RQ3 ################################
library(dunn.test)

# load data
d <- read.csv("survey.data.final.csv", header=T, sep = ";", quote = "\"")
d[d == -99 ] <- NA

# remove rows containing NA 
#d <- na.omit(d)

# Dependent variables
dependent_vars <- c("Correctness", "Relevance", "Clarity", "Completeness", "Transparency")

# Independent variables
independent_vars <- c("Source")

# Initialize an empty list to store the results
kruskal_results <- list()

# Initialize an empty list to store the Dunn test results
dunn_results <- list()

# Loop through each dependent variable and each independent variable
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    # Create the formula dynamically
    formula <- as.formula(paste(dv, "~", iv))
    
    # Perform the Kruskal-Wallis test
    test_result <- kruskal.test(formula, data = d)
    
    # Store the Kruskal-Wallis test result in the list
    result_name <- paste("Kruskal_Wallis", dv, "vs", iv, sep = "_")
    kruskal_results[[result_name]] <- test_result
    
    # Perform Dunn test with Holm-Bonferroni correction
    dunn_test_result <- dunn.test(d[[dv]], d[[iv]], method = "bonferroni", list = TRUE)
    
    # Store the Dunn test result in the list
    dunn_name <- paste("Dunn_Test_Bonferroni", dv, "vs", iv, sep = "_")
    dunn_results[[dunn_name]] <- dunn_test_result
    
    # Calculate and print median ranks for interpretation
    median_ranks <- aggregate(d[[dv]] ~ d[[iv]], data = d, FUN = median)
    colnames(median_ranks) <- c(iv, dv)
    print(paste("Median ranks for", dv, "across", iv))
    print(median_ranks)
  }
}

# Print the Kruskal-Wallis test results
print(kruskal_results)

# Print the Dunn test results
print(dunn_results)

# Function to format and print Dunn test results
format_dunn_results <- function(dunn_test_result) {
  # Extract relevant components
  comparisons <- dunn_test_result$comparisons
  Z_scores <- dunn_test_result$Z
  p_values <- dunn_test_result$P
  p_adjusted <- dunn_test_result$P.adjusted
  
  # Create a data frame to organize the results
  results_df <- data.frame(
    Comparison = comparisons,
    Z_Score = Z_scores,
    P_Value = p_values,
    P_Adjusted = p_adjusted
  )
  
  # Print the formatted results
  print(results_df)
}

# Loop through the Dunn test results and format them
for (result_name in names(dunn_results)) {
  cat("\nDunn Test Results for:", result_name, "\n")
  format_dunn_results(dunn_results[[result_name]])
}




######################## Kruskal Wallis for RQ4 ################################
library(dunn.test)
library(dplyr)

# Load data
d <- read.csv("survey.data.final.csv", header = TRUE, sep = ";", quote = "\"")
d[d == -99] <- NA

# Define a function to filter rows based on a specific word
filter_rows <- function(data, column_name, word) {
  data %>%
    filter(get(column_name) == word)
}

# Example usage: filter rows where the 'column_name' is 'Bloom level/question type/subdomain'
filtered_data <- filter_rows(d, "Bloom", "remember")

# Define dependent and independent variables
dependent_vars <- c("Correctness", "Relevance", "Clarity", "Completeness", "Transparency")
independent_vars <- c("Source")

# Initialize lists to store results
kruskal_results <- list()
dunn_results <- list()

# Loop through each dependent variable and each independent variable
for (dv in dependent_vars) {
  for (iv in independent_vars) {
    # Create the formula dynamically
    formula <- as.formula(paste(dv, "~", iv))
    
    # Perform the Kruskal-Wallis test
    test_result <- kruskal.test(formula, data = filtered_data)
    
    # Store the Kruskal-Wallis test result in the list
    result_name <- paste("Kruskal_Wallis", dv, "vs", iv, sep = "_")
    kruskal_results[[result_name]] <- test_result
    
    # Perform Dunn test with Bonferroni correction regardless of p-value
    dunn_test_result <- dunn.test(filtered_data[[dv]], filtered_data[[iv]], method = "bonferroni", list = TRUE)
    
    # Store the Dunn test result in the list
    dunn_name <- paste("Dunn_Test_Bonferroni", dv, "vs", iv, sep = "_")
    dunn_results[[dunn_name]] <- dunn_test_result
    
    # Calculate and print median ranks for interpretation
    median_ranks <- aggregate(filtered_data[[dv]] ~ filtered_data[[iv]], data = filtered_data, FUN = median)
    colnames(median_ranks) <- c(iv, dv)
    print(paste("Median ranks for", dv, "across", iv))
    print(median_ranks)
  }
}

# Print the Kruskal-Wallis test results
print(kruskal_results)

# Print the Dunn test results
print(dunn_results)

# Function to format and print Dunn test results
format_dunn_results <- function(dunn_test_result) {
  # Extract relevant components
  comparisons <- dunn_test_result$comparisons
  Z_scores <- dunn_test_result$Z
  p_values <- dunn_test_result$P
  p_adjusted <- dunn_test_result$P.adjusted
  
  # Create a data frame to organize the results
  results_df <- data.frame(
    Comparison = comparisons,
    Z_Score = Z_scores,
    P_Value = p_values,
    P_Adjusted = p_adjusted
  )
  
  # Print the formatted results
  print(results_df)
}

# Loop through the Dunn test results and format them
for (result_name in names(dunn_results)) {
  cat("\nDunn Test Results for:", result_name, "\n")
  format_dunn_results(dunn_results[[result_name]])
}



############################ OLR for RQ3 #######################################
library(MASS)
library(broom)
library(dplyr)


d <- read.csv("survey.data.final.csv", header=T, sep = ";", quote = "\"")
d[d == -99 ] <- NA

# Define the dependent variables (DVs)
DVs <- c("Correctness", "Relevance", "Clarity", "Completeness", "Transparency")

d$QuestionID <- factor(d$QuestionID)
d$Domain <- factor(d$Domain)
d$Subdomain <- factor(d$Subdomain, levels = c("urban geography", "economic geography", "ids", "geomorphology", "hydrology", "oceanography", ordered = TRUE))
d$Bloom <- factor(d$Bloom, levels = c("remember", "understand", "apply", "analyze", "evaluate", "create"), ordered = TRUE)
d$Questiontype <- factor(d$Questiontype)
d$Source <- factor(d$Source)
d$Complexity <- factor(d$Complexity)

# Convert the dependent variables to ordered factors
for (dv in DVs) {
  d[[dv]] <- factor(d[[dv]], ordered = TRUE)
}

# Relevel the 'Source' factor to change the baseline to 'expert answer'
d$Source <- relevel(d$Source, ref = "expert answer")

# Function to calculate and store model details
get_model_details <- function(model) {
  # Tidy the model for coefficients, including standard errors
  tidy_model <- tidy(model)
  
  # Calculate p-values
  tidy_model <- tidy_model %>%
    mutate(p_value = 2 * (1 - pnorm(abs(statistic))))
  
  # Calculate odds ratios and confidence intervals
  tidy_model <- tidy_model %>%
    mutate(OR = exp(estimate),
           CI_lower = exp(estimate - 1.96 * std.error),
           CI_upper = exp(estimate + 1.96 * std.error))
  
  return(tidy_model)
}

# Initialize a list to store the models
models <- list()

# Loop through each DV and fit the model
for (dv in DVs) {
  # Construct the formula dynamically
  formula <- as.formula(paste(dv, "~ Source"))
  
  # Fit the model
  model <- polr(formula, data = d, method = "logistic", Hess = TRUE)
  
  # Get model details and store in the list
  models[[dv]] <- get_model_details(model)
  
  # Print the summary of the model including OR and CI
  cat("Summary of model for DV:", dv, "\n")
  summary(model)
  print(models[[dv]])
  cat("\n\n")
}




############################ OLR for RQ4 ####################################### 
# Load necessary packages
library(MASS)
library(broom)
library(dplyr)
library(tidyr)
library(car)

d <- read.csv("survey.data.final.csv", header=T, sep = ";", quote = "\"")
d[d == -99 ] <- NA

# turn into IVs into factors
d$QuestionID <- factor(d$QuestionID)
d$Domain <- factor(d$Domain)
d$Subdomain <- factor(d$Subdomain, levels = c("urban geography", "economic geography", "ids", "geomorphology", "hydrology", "oceanography", ordered = TRUE))
d$Bloom <- factor(d$Bloom, levels = c("remember", "understand", "apply", "analyze", "evaluate", "create"), ordered = TRUE)
d$Questiontype <- factor(d$Questiontype)
d$Source <- factor(d$Source)
d$Complexity <- factor(d$Complexity)

d$Correctness <- as.numeric(d$Correctness)
d$Relevance <- as.numeric(d$Relevance)
d$Clarity <- as.numeric(d$Clarity)
d$Completeness <- as.numeric(d$Completeness)
d$Transparency <- as.numeric(d$Transparency)


# Relevel the 'Source' factor to change the baseline to 'expert answer'
d$Source <- relevel(d$Source, ref = "expert answer")

# Define the dependent variables (DVs) and independent variables (IVs)
DVs <- c("Correctness", "Relevance", "Clarity", "Completeness", "Transparency")
IVs <- c("Questiontype", "Bloom", "Subdomain", "Complexity", "Domain")

# Convert the dependent variables to ordered factors
for (dv in DVs) {
  d[[dv]] <- factor(d[[dv]], ordered = TRUE)
}

# Filter the data based on some criteria : select independent variable and level
filtered_data <- d %>% filter(Subdomain == "geomorphology")

# Function to calculate and store model details
get_model_details <- function(model) {
  # Tidy the model for coefficients, including standard errors
  tidy_model <- tidy(model)
  
  # Calculate p-values
  tidy_model <- tidy_model %>%
    mutate(p_value = 2 * (1 - pnorm(abs(statistic))))
  
  # Calculate odds ratios and confidence intervals
  tidy_model <- tidy_model %>%
    mutate(OR = exp(estimate),
           CI_lower = exp(estimate - 1.96 * std.error),
           CI_upper = exp(estimate + 1.96 * std.error))
  
  return(tidy_model)
}

# Initialize a list to store the models
models <- list()

# Loop through each DV and fit the model
for (dv in DVs) {
  # Fit the model
  model <- polr(as.formula(paste(dv, "~ Source")), data = filtered_data, method = "logistic", Hess = TRUE)
  
  # Get model details and store in the list
  models[[dv]] <- get_model_details(model)
}

# Print the model summaries
for (dv in DVs) {
  cat("Summary of model for DV:", dv, "\n")
  print(models[[dv]])
  cat("\n\n")
}


########################## Boxplots for RQ 3 ###################################
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

#reload the data
d <- read.csv("survey.data.final.csv", header=T, sep = ";", quote = "\"")
d[d == -99 ] <- NA

# Define colors for each Source level
source_colors <- c("expert answer" = "lightblue", "chatgpt3.5" = "lightyellow", "chatgpt4o" = "lightpink")
source_scale <- scale_fill_manual(name = "Source", values = source_colors)

# Pivot the data to long format
d_long <- d %>%
  pivot_longer(cols = c(Correctness, Relevance, Clarity, Completeness, Transparency), 
               names_to = "DV", 
               values_to = "Score")

# Set the order of the DVs
d_long$DV <- factor(d_long$DV, levels = c("Correctness", "Relevance", "Clarity", "Completeness", "Transparency"))

# Create the boxplot
p <- ggplot(d_long, aes(x = DV, y = as.numeric(Score), fill = Source)) +
  geom_boxplot(position = position_dodge(width = 0.75), width = 0.6, outlier.shape = 8) +
  labs(x = "", y = "Score", title = "Quality scores per answer source") +
  source_scale +
  scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
  theme_minimal(base_family = "sans") +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title.y = element_text(size = 16)
  )

print(p)

# Save the plot as a high-quality PNG file
# ggsave("source_plot.png", plot = p, width = 10, height = 6, dpi = 500)








############################# boxplots for RQ4 ################################
# Load necessary libraries
library(ggplot2)
library(dplyr)

#reload the data
d <- read.csv("survey.data.final.csv", header=T, sep = ";", quote = "\"")
d[d == -99 ] <- NA

# Convert columns to appropriate types
d$QuestionID <- factor(d$QuestionID)
d$Domain <- factor(d$Domain)
d$Subdomain <- factor(d$Subdomain, levels = c("urban geography", "economic geography", "ids", "geomorphology", "hydrology", "oceanography", ordered = TRUE))
d$Bloom <- factor(d$Bloom, levels = c("remember", "understand", "apply", "analyze", "evaluate", "create"), ordered = TRUE)
d$Questiontype <- factor(d$Questiontype)
d$Source <- factor(d$Source)
d$Complexity <- factor(d$Complexity)
#d$Correctness <- as.numeric(d$Correctness)
#d$Relevance <- as.numeric(d$Relevance)
#d$Clarity <- as.numeric(d$Clarity)
#d$Completeness <- as.numeric(d$Completeness)
#d$Transparency <- as.numeric(d$Transparency)

# Define colors for each Source level
source_colors <- c("expert answer" = "lightblue", "chatgpt3.5" = "lightyellow", "chatgpt4o" = "lightpink")
source_scale <- scale_fill_manual(name = "Source", values = source_colors)

# List of dependent variables (DVs)
dvs <- c("Correctness", "Relevance", "Clarity", "Completeness", "Transparency")

# List of independent variables (IVs)
ivs <- c("Subdomain", "Bloom", "Questiontype")

# Function to create and display boxplots for each DV and IV combination
create_boxplots <- function(data, dvs, ivs, colors, color_scale) {
  for (iv in ivs) {
    for (dv in dvs) {
      # Filter the data to include only the current DV and IV
      d_filtered <- data %>%
        dplyr::select(all_of(c("Source", iv, dv)))
      
      # Create the boxplot
      p <- ggplot(d_filtered, aes_string(x = iv, y = dv, fill = "Source")) +
        geom_boxplot(position = position_dodge(width = 0.75), width = 0.6, outlier.shape = 8) +
        labs(x = "", y = paste("Score"), title = paste(dv, "scores by", iv, "and answer source")) +
        color_scale +
        scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
        theme_minimal(base_family = "sans") +
        theme(
          legend.position = "right",
          axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Increase x-axis text size and adjust angle
          axis.text.y = element_text(size = 14),  # Increase y-axis text size
          axis.title.y = element_text(size = 14),  # Increase y-axis title size
          plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  # Center and bold the title
          legend.text = element_text(size = 14),  # Increase legend text size
          legend.title = element_text(size = 14)  # Increase legend title size
        )
      
      # Display the plot
      print(p)
      
      # Save the plot as a high-quality PNG file
      # ggsave(paste0(dv, "_plot_", iv, ".png"), plot = p, width = 10, height = 6, dpi = 500)
    }
  }
}

# Apply the function to the dataset
create_boxplots(d, dvs, ivs, source_colors, source_scale)
