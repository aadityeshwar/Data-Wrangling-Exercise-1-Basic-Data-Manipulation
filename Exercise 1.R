# Importing the data into R
df <- read.csv("~/Desktop/Data Science/3 Data Wranging/Exercise 1/refine_original.csv")

# Viewing the data set
View(df)

#Installing the required packages
install.packages("dplyr")
suppressMessages( library(dplyr) )

install.packages("tidyr")
suppressMessages( library(tidyr) )

#Looking at the structure of the data set
glimpse(df)

# Converting the observations of variable "company" to Lower Case
df <- df %>% mutate_each(funs(tolower), company)

#Task 1: Cleaning up the 'company' column, so all of the misspellings of the brand names are standardized. 
df1_1 <- df %>% mutate(company=ifelse(grepl('^ph|fi', company), 'phillips', company))
df1_2 <- df1_1 %>% mutate(company=ifelse(grepl('^a', company), 'akzo', company))
df1_3 <- df1_2 %>% mutate(company=ifelse(grepl('^v', company), 'van houten', company))
df1_4 <- df1_3 %>% mutate(company=ifelse(grepl('^u', company), 'unilever', company))

# Task 2: Separate product code and number
df2 <- df1_4 %>% separate(Product.code...number, c("product_code", "product_number"), sep = "-")

# Task 3: Add product categories - p = Smartphone, v = TV, x = Laptop, q = Tablet
df3_1 <- df2 %>% mutate(product_category = gsub("p", "Smartphone", product_code))
df3_2 <- df3_1 %>% mutate(product_category = gsub("v", "TV", product_category))
df3_3 <- df3_2 %>% mutate(product_category = gsub("x", "Laptop", product_category))
df3_4 <- df3_3 %>% mutate(product_category = gsub("q", "Tablet", product_category))

#Task 4: Add full address for geocoding
df4 <- df3_4 %>% unite("full_address", address, city, country, sep = ",")

# Task 5: Create dummy variables for company and product category
df5_1 <- df4 %>% mutate( company_philips = ifelse(company == "philips", 1, 0))
df5_2 <- df5_1 %>% mutate( company_akzo = ifelse(company == "akzo", 1, 0))
df5_3 <- df5_2 %>% mutate( company_van_houten = ifelse(company == "van", 1, 0))
df5_4 <- df5_3 %>% mutate( company_unilever = ifelse(company == "unilever", 1, 0))
df5_5 <- df5_4 %>% mutate( product_smartphone = ifelse(product_category == "Smartphone", 1, 0))
df5_6 <- df5_5 %>% mutate( product_laptop = ifelse(product_category == "Laptop", 1, 0))
df5_7 <- df5_6 %>% mutate( product_tv = ifelse(product_category == "TV", 1, 0))
df5_8 <- df5_7 %>% mutate( product_tablet = ifelse(product_category == "Tablet", 1, 0))

# Final Data Frame saved as refine_clean and arranged by group for "company" variable.
refine_clean <- df5_8 %>% group_by(company)

# Saving the final data frame
write.csv(refine_clean, file="~/Desktop/Data Science/3 Data Wranging/Exercise 1/refine_clean.csv")