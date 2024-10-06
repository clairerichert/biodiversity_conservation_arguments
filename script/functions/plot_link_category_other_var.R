library(ggpubr)

# function to analyze the link between the pre-selected independent variables and the categories 2 by 2
plot_link_category_other_var <- function(question, data, cat_vars, interval_vars, sample_name, PLOTSIGNIFICANT = FALSE) {
  if (question == 3) {
    focus <- "biodiversity"
  } else {
    focus <- "insect"
  }
  # choose a "reference modality"
  question_name <- paste0(question, "_category_arguments")
  data[[question_name]] <- relevel(factor(data[[question_name]]), "NON_INSTRUMENTAL")

  # test link between argument categories and categorical and ordinal variables
  plot_list_cat_ordi <- lapply(cat_vars, function(x) { plot_cat_ordi_variables(x, question_name, d,
                                                                               paste0("Categories of arguments for ", focus, " conservation"),
                                                                                      PLOTSIGNIFICANT) })
  plot_list_cat_ordi <- plot_list_cat_ordi[!sapply(plot_list_cat_ordi, is.null)]
  if (length(plot_list_cat_ordi) > 0) {
    if (length(plot_list_cat_ordi) > 1) { ncol <- 2 } else { ncol <- 1 }
    nrow <- floor(length(plot_list_cat_ordi) / ncol) + length(plot_list_cat_ordi) %% ncol

    ggpubr::ggarrange(plotlist = plot_list_cat_ordi, ncol = ncol, nrow = nrow, common.legend = TRUE, legend = "bottom")
    ggsave(file.path("results", paste0(question, "link_catarguments_catvar", sample_name, ".png")), width = ncol * 17, height = nrow * 10, units = "cm")
  }

  # test link between argument categories and interval variables
  plot_list_inter <- lapply(interval_vars, function(x) { plot_cat_interval_variables(question_name, x, d,
                                                                                      paste0("Categories of arguments for ", focus, " conservation"),
                                                                                             PLOTSIGNIFICANT) })
  plot_list_inter <- plot_list_inter[!sapply(plot_list_inter, is.null)]
  if (length(plot_list_inter) > 0) {
    ggpubr::ggarrange(plotlist = plot_list_inter, ncol = 1, nrow = length(plot_list_inter))
    ggsave(file.path("results", paste0(question, "_link_catarguments_intervar", sample_name, ".png")), width = 12, height = 5 * length(plot_list_inter))
  }
  return(data)
}

plot_cat_interval_variables <- function(cat, var, data, cat_name, PLOTSIGNIFICANT) {
  data$cat <- data[[cat]]
  data$inter <- data[[var]]
  anova <- aov(inter ~ cat, data = data)
  anova_pvalue <- summary(anova)[[1]]['Pr(>F)'][[1]][[1]]
  aov_pvalue <- if (anova_pvalue < 0.05) {"< 0.05"} else {">= 0.05"}
  p <- ggplot(data = data, mapping = aes(x = inter, y = cat)) +
    geom_boxplot(fill = "grey") + xlab(sprintf("%s (anova pvalue: %s)", var, aov_pvalue)) + ylab(cat_name) +
    theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 14), text = element_text(size = 14))

  if ((PLOTSIGNIFICANT & anova_pvalue < 0.05) | (!PLOTSIGNIFICANT)){
    return(p)
  }
}

plot_cat_ordi_variables <- function(cat, var, data, var_name, PLOTSIGNIFICANT) {
  data$vari <- as.factor(data[[cat]])
  data$varj <- as.factor(data[[var]])
  chi2 <- chisq.test(data$vari, data$varj)
  chi2pvalue <- if (chi2$p.value < 0.05) { "< 0.05" } else { ">= 0.05" }
  p <- ggplot(data) +
           geom_bar(aes(x = .data$vari, y = after_stat(prop), fill = .data$varj, group = .data$varj), position = "dodge") +
           scale_fill_grey(name = waiver(), start = 0.2, end = 0.8, aesthetics = "fill") +
           xlab(cat) +
           guides(fill = guide_legend(title = var_name, nrow = 1, theme(legend.title.position = "top",
                                                                        legend.text.position = "bottom",
                                                                        legend.position = "horizontal"))) +
           annotate("text", label = sprintf("p-value of chi-squared test: %s", chi2pvalue),
                    x = levels(data$vari)[1], y = 0.9, hjust = 0, vjust = 1, size = 6) +
           theme(legend.text = element_text(size = 16), legend.title = element_text(size = 16),
             axis.text.y = element_text(size = 16), axis.text.x = element_text(size = 16), text = element_text(size = 16))

  if ((PLOTSIGNIFICANT & chi2$p.value < 0.05) | (!PLOTSIGNIFICANT)) {
    return(p)
  }
}

