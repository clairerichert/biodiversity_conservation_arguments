# - For biodiv and insect conservation:
#   - Select rows were there is no NAs for the argument and other variables
#   - Test the association between the category of arguments and each variable
#   - Plot the results

source('script/functions/clean_data_arguments_functions.R')
source('script/functions/plot_link_category_other_var.R')
library(ggplot2)

cat_vars <- c("gender", "education_level", "biodiv_activity", "naturalist_activity", "inclusion_of_nature_in_self")
interval_vars <- c("age", "rural_urban")

for (question in c(3, 18)) {
  cols_to_keep <- c('id_final', paste0('Q', question, '_category_arguments'),
                    paste0('Q', c(67, 68, 69, 72, 73, 76, '77_CODED')))
  d <- clean_data_for_arguments_other_variables_analysis(question, cols_to_keep)

  # plot data to see if they are balanced
  for (var in c(interval_vars, cat_vars)) {
    print(ggplot(d) + geom_bar(aes(x = d[[var]])) + xlab(var))
  }
  plot_link_category_other_var(paste0("Q", question), d, cat_vars, interval_vars, paste0("_", nrow(d), "resp"), PLOTSIGNIFICANT = TRUE)
}
