# Describe the sample in terms of:
# - socio-demographic variables
# - interest about naturalistic activities

source('script/functions/clean_data_arguments_functions.R')
source('script/functions/clean_data_functions.R')

data <- load_data()

# print the answers to the question "Do you practice a naturalist activity?" (to see the proportion of entomologists)
print("Distribution of the naturalist_activity variable (in %)")
print(round(table(data$Q77_CODED) / nrow(data) * 100))

data <- rename_and_simplify_var(data)

# Distribution of categorical variables within the whole sample
fmt_colstat <- function(col) {
  table_fmt <- t(as.data.frame(round(table(data[[col]], useNA = "always") / nrow(data) * 100)))
  table_fmt <- cbind(c(col, ""), c("Modality", "Percentage"), table_fmt)
  return(table_fmt)

}

cat_cols <- c("gender", "education_level", "naturalist_activity", "biodiv_activity")
cat_table <- do.call(rbind, lapply(cat_cols, fmt_colstat))

prop_no_biodiv_no_naturalist <- round(nrow(data[(data$naturalist_activity == "not_naturalist") & (data$biodiv_activity == "no_biodiv_profession"),]) / nrow(data) * 100)
print(sprintf("Percentage of respondents who are neither naturalist, nor have a job related to biodiversity: %s",
              prop_no_biodiv_no_naturalist))
prop_biodiv_and_naturalist <- round(nrow(data[(data$naturalist_activity == "naturalist") & (data$biodiv_activity == "biodiv_profession"),]) / nrow(data) * 100)
print(sprintf("Percentage of respondents who are both naturalist, and have a job related to biodiversity: %s",
              prop_biodiv_and_naturalist))
write.csv(cat_table, file.path("results", "sample_characteristics.csv"), row.names = FALSE)

# Age distribution within the sample
# age distribution for France
df_age_insee <- read.csv(file.path("data", "demo-pop-sexe-age-effectif-insee.csv"), sep = ",", skip = 2, nrows = 15)
colnames(df_age_insee) <- c("age_group", "women", "men", "all")

# compare age distribution in France and for the sample
df_age <- data.frame(matrix(ncol = 2, nrow = nrow(df_age_insee) - 1))
colnames(df_age) <- c("france", "sample")

# compute the percentage of French people in each age group
df_age$france <- round(df_age_insee$all[seq(nrow(df_age))] / df_age_insee[df_age_insee$age_group == "Ensemble", "all"] * 100, 1)

# age groups detailed in df_age_insee (see first column of the csv file)
age_group <- c(0, seq(15, 75, 5), 100)

# Compute the percentage of respondents in each age group
df_age$sample <- sapply(seq(length(age_group) - 1),
                    function(i) { round(sum(data$age >= age_group[i] & data$age < age_group[i + 1]) / nrow(data) * 100, 1) })

# Combine age groups together to obtain age groups of 10 years instead of 5 (except first and last groups)
df_age <- do.call(rbind, lapply(seq(1, nrow(df_age), 2), function(i) { apply(df_age[i : (i + 1), ], 2, sum)}))
row.names(df_age) <- c("Below 20 years old", "20 to 30 years old", "30 to 40 years old", "40 to 50 years old",
                     "50 to 60 years old", "60 to 70 years old", "Above 70 years old")
write.csv(df_age, file.path("results", "sample_characteristics_age.csv"), row.names = TRUE)
