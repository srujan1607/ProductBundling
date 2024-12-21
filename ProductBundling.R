# ==================================
# 1. Install and Load Required Libraries
# ==================================
if (!require(arules)) install.packages("arules", dependencies = TRUE)
if (!require(arulesViz)) install.packages("arulesViz", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)

library(arules)
library(arulesViz)
library(dplyr)

# ==================================
# 2. Load Dataset
# ==================================
# Sample dataset: Online Retail Data (UCI Repository or similar)
# Replace the file path with your dataset.
data_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00352/Online%20Retail.xlsx"

if (!require(openxlsx)) install.packages("openxlsx")
library(openxlsx)

# Read Excel file
temp_file <- tempfile(fileext = ".xlsx")
download.file(data_url, temp_file, mode = "wb")
df <- read.xlsx(temp_file, sheet = 1)

# View the structure of the dataset
str(df)

# ==================================
# 3. Data Preprocessing
# ==================================
# Clean the dataset
df <- df %>%
  filter(!is.na(InvoiceNo), !is.na(Description), Quantity > 0) %>%
  select(InvoiceNo, Description)

# Create a transactions dataset
transactions <- as(split(df$Description, df$InvoiceNo), "transactions")

# Check summary of the transactions
summary(transactions)

# ==================================
# 4. Apply Apriori Algorithm
# ==================================
# Set parameters for the Apriori algorithm
# Support: Minimum frequency of items in transactions
# Confidence: Strength of association
rules <- apriori(transactions, 
                 parameter = list(supp = 0.01, conf = 0.5))

# View rules
inspect(rules[1:10])

# ==================================
# 5. Filter and Visualize Rules
# ==================================
# Filter rules for higher confidence and lift
filtered_rules <- subset(rules, lift > 1 & confidence > 0.6)

# Visualize the top 10 rules
plot(filtered_rules[1:10], method = "graph", engine = "htmlwidget")

# Plot support vs confidence
plot(rules, measure = c("support", "confidence"), shading = "lift")

# ==================================
# 6. Identify Product Bundles
# ==================================
# Extract the most frequent itemsets (product bundles)
frequent_itemsets <- eclat(transactions, parameter = list(supp = 0.01))
inspect(sort(frequent_itemsets, by = "support")[1:10])

# Generate product bundles based on rules
bundles <- lhs(filtered_rules)
inspect(bundles)

# ==================================
# 7. Save Results
# ==================================
# Save the rules to a CSV file
write(rules, file = "product_bundles.csv", sep = ",", quote = TRUE, row.names = FALSE)

cat("\nProduct Bundle Identification Complete! Results saved to 'product_bundles.csv'.")
