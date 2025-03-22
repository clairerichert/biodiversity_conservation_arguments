# For both biodiversity and insect conservation, compute:
# - the number of observations by premise
# - the distribution of arguments
# - the distribution of categories of arguments
# Plot Sankeys diagrams to see how arguments shifts from a category to another when the focus is
# on insect conservation

source('script/functions/clean_data_functions.R')
source('script/functions/clean_data_arguments_functions.R')
library(RColorBrewer)
library(ggsankey)
library(ggplot2)
library(dplyr)
library(ggpubr)

pdf(NULL)

data <- load_data()

# clean data
data_q3_premises_q3 <- clean_data_insect_or_biodiv_need_for_protection_question(data, 3)
data_q3 <- data_q3_premises_q3[[1]]
premises_q3 <- data_q3_premises_q3[[2]]
print(sprintf("Nb obs for biodiv: %s", nrow(data_q3)))

data_q18_premises_q18 <- clean_data_insect_or_biodiv_need_for_protection_question(data, 18)
data_q18 <- data_q18_premises_q18[[1]]
premises_q18 <- data_q18_premises_q18[[2]]
print(sprintf("Nb obs for insects: %s", nrow(data_q18)))

# number of obs by premise
all_premises <- unique(c(premises_q3, premises_q18))
obs_by_premise <- matrix(nrow = length(all_premises), ncol = 2, dimnames = list(all_premises, c('biodiversity', 'insects')))
for (prem in all_premises) {
  if (prem %in% premises_q3) {
    obs_by_premise[prem, 'biodiversity'] <- sum(data_q3[[prem]])
  } else {
    obs_by_premise[prem, 'biodiversity'] <- 0
  }
  if (prem %in% premises_q18) {
    obs_by_premise[prem, 'insects'] <- sum(data_q18[[prem]])
  } else {
    obs_by_premise[prem, 'insects'] <- 0
  }
}

obs_by_premise <- obs_by_premise[order(obs_by_premise[, 'biodiversity'], decreasing = TRUE),]
write.csv(obs_by_premise, 'results/number_obs_by_premise.csv')

# analysis of how the premises are combined
argument_analysis <- function(question, argument) {
  # count the number of time each association of premises is found
  arguments <- sort(table(argument), decreasing = TRUE)
  print(sprintf("nb of different arguments for %s: %s", question, length(arguments)))
  arguments <- data.frame(arguments[arguments > 15])
  arguments$Prop <- round(arguments$Freq / length(argument), 2)
  write.csv(arguments, sprintf('results/most_found_combinations_%s.csv', question), row.names = FALSE)
  return(arguments)
}

data_q3 <- build_arguments(data_q3)
data_q18 <- build_arguments(data_q18)
comb_q3 <- argument_analysis('q3', data_q3$arguments)
comb_q18 <- argument_analysis('q18', data_q18$arguments)

# plot the number of respondents by argument
all_args <- merge(comb_q3, comb_q18, by = "argument", suffixes = c(".biodiv", ".insects"), all.x = TRUE, all.y = TRUE)
all_args[is.na(all_args)] <- 0
all_args <- all_args[all_args$Prop.biodiv >= 0.05 | all_args$Prop.insects >= 0.05,]
insects <- data.frame(frequency = all_args$Prop.insects, argument = all_args$argument, protection_type = "insects")
biodiv <- data.frame(frequency = all_args$Prop.biodiv, argument = all_args$argument, protection_type = "biodiversity")
obs_by_arg <- rbind(insects, biodiv)
values_for_ordering <- obs_by_arg[obs_by_arg$protection_type == "biodiversity", "frequency"]
obs_by_arg$col_to_reorder <- c(values_for_ordering, values_for_ordering)

plot_premise_frequency <- ggplot(data = obs_by_arg,
                                 aes(x = frequency,
                                     y = reorder(argument, col_to_reorder), fill = protection_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ylab("argument") +
  xlab("frequency") +
  scale_fill_grey(name = waiver(), start = 0.2, end = 0.8, aesthetics = "fill") +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 14), text = element_text(size = 14))

ggsave(file.path("results", "argument_frequency.png"), width = 12, height = 9)

# Categorize the arguments
data_q3 <- categorize_arguments(data_q3)
data_q18 <- categorize_arguments(data_q18)

# Plot the frequency of each category for biodiversity and insects protection
cat <- c('INSTRUMENTAL_HUMANS', 'INSTRUMENTAL_GENERAL', 'INSTRUMENTAL_UNSPECIFIED', 'NON_INSTRUMENTAL')

build_df_protection_type_frequency_cat <- function(df, protection_type) {
  return(data.frame(
    frequency = apply(df[, cat], 2, function(x) { sum(x == 1) / nrow(df) }),
    protection_type = rep(protection_type, length(cat)),
    category = cat))
}

df_freq <- do.call(rbind, mapply(function(d, pt) { build_df_protection_type_frequency_cat(d, pt) },
                                 list(data_q3, data_q18), c("biodiversity", "insects"), SIMPLIFY = FALSE))

plot_cat_frequency <- ggplot(data = df_freq, aes(x = frequency, y = category, fill = protection_type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_grey(name = waiver(), start = 0.2, end = 0.8, aesthetics = "fill") +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 14), text = element_text(size = 14))

# sankey for categories between Q3 and Q18
common <- merge(data_q3[, c(cat, 'id_final')], data_q18[, c(cat, 'id_final')], by = 'id_final', suffixes = c('_biodiversity', '_insects'))

define_argument_selected <- function(df, suffix) {
  sdf <- df[, grep(suffix, colnames(df))]
  colnames(sdf) <- gsub(suffix, '', colnames(sdf))
  return(apply(sdf, 1, function(x) { colnames(sdf)[x == 1] }))
}

sankey_df <- data.frame('biodiversity' = define_argument_selected(common, '_biodiversity'),
                        'insects' = define_argument_selected(common, '_insects'))
sankey_df <- sankey_df %>%
  make_long(biodiversity, insects)

plot_sankey <- ggplot(sankey_df, aes(x = x, next_x = next_x, node = node, next_node = next_node,
                                     fill = factor(node), label = node)) +
  geom_sankey(show.legend = FALSE) +
  theme_sankey(base_size = 14) +
  geom_sankey_label(color = "black", fill = "white") +
  labs(x = NULL) +
  theme(legend.position = "none", axis.text.x = element_text(size = 16)) +
  scale_fill_grey(name = waiver(), start = 0.2, end = 0.8, aesthetics = "fill")

ggarrange(plotlist = list(plot_cat_frequency, plot_sankey), ncol = 1, nrow = 2, labels = list("a.", "b."), label.x = -0.02) +
  theme(plot.margin = margin(0, 0, 0, 0.5, "cm"))
ggsave(file.path("results", "sankey_and_barplot_categories_of_arguments.png"), width = 12, height = 9)
