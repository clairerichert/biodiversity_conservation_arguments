source('script/functions/clean_data_functions.R')

clean_data_insect_or_biodiv_need_for_protection_question <- function(data, question) {
  #' Clean the data related to the premises that justify the need for biodiversity or insects protection
  #'
  #' Function that:
  #'    - Select the answers of the respondents who think biodiversity or insects should be protected and explained why
  #'      they think so
  #'    - Select only the columns with the id and the premises (each premise is a column with 0 and 1)
  #'    - Rename the columns to remove Q3 and Q18 before the premise
  # select useful columns
  question_bef <- sprintf('Q%s', question - 1)
  df <- data[, c(question_bef, grep(sprintf('id_final|Q%s_', question), colnames(data), value = TRUE))]

  # select only rows of respondents who agree to protect biodiversity and who give an explanation
  df <- df[(df[[question_bef]] > 2),]
  df <- df[rowSums(is.na(df)) == 0,]

  # remove question that asks whether biodiversity or insects should be protected
  df <- df[ , !(colnames(df) == question_bef)]

  # list of existing premises (each column of the dataframe except id_final is a premise)
  list_premises <- sapply(colnames(df)[!(colnames(df) == 'id_final')],
                      function(x) { strsplit(x, sprintf('%s_', question))[[1]][2] })

  # remove Q3 or Q18 from the column names
  colnames(df)[!(colnames(df) == 'id_final')] <- list_premises
  return(list(df, list_premises))
}

# build arguments
build_arguments <- function(data_question) {
  #' data_question is a dataframe with 1 row by respondent and 1 column by premise (and an id column).
  #' Each premise can take the values 0 (if the respondent did not use it) and 1 (if the respondent used it).
  #' For each respondent, this function selects the premises he/she used and aggregates their names to build an
  #' argument.
  df <- data_question[, !(colnames(data_question) == 'id_final')]
  data_question$arguments <- sapply(seq(nrow(df)), function(i) { paste(colnames(df)[df[i, ] == 1], collapse = '_')})
  return(data_question[ , c('id_final', 'arguments')])
}

instrumental_humans <- c('HUMAN_PROTECTION', 'FUTURE_GENERATIONS')
instrumental_general <- c('LIFE_PROTECTION', 'ECOSYSTEM_PROTECTION', 'SPECIES_PROTECTION')
instrumental_unspecified <- c('SERVICES_RESOURCES', 'INTERCONNECTED', 'RESILIENCE', 'INDICATOR')

categorize_arguments <- function(df, insects = FALSE) {
  #' We define 4 categories of arguments: INSTRUMENTAL_HUMANS, INSTRUMENTAL_GENERAL, INSTRUMENTAL_UNSPECIFIED, and
  #' NON_INSTRUMENTAL. This function:
  #'   - builds 1 column by category of arguments
  #'   - for each column and each row, it assigns the value 0 if the respondent did not use an argument that belong to
  #'     this category and 1 otherwise

  # SPECIES_PROTECTION is NON_INSTRUMENTAL for biodiversity protection and INSTRUMENTAL_GENERAL for insects protection
  if (insects) {
    instrumental_general <- c('LIFE_PROTECTION', 'ECOSYSTEM_PROTECTION', 'SPECIES_PROTECTION')
  } else {
    instrumental_general <- c('LIFE_PROTECTION', 'ECOSYSTEM_PROTECTION')
  }

  # Initialize a column by category of argument
  df$INSTRUMENTAL_HUMANS <- df$INSTRUMENTAL_GENERAL <- df$INSTRUMENTAL_UNSPECIFIED <- df$NON_INSTRUMENTAL <- 0

  # Assign the value 1 for the column INSTRUMENTAL_HUMANS to rows that contain at least one INSTRUMENTAL_HUMANS premise
  df[sapply(df$arguments, function(x) { grepl(paste(instrumental_humans, collapse = '|'), x) }), 'INSTRUMENTAL_HUMANS'] <- 1

  # Assign the value 1 for the column INSTRUMENTAL_GENERAL to rows that are not INSTRUMENTAL_HUMANS and that contain
  # at least one INSTRUMENTAL_GENERAL premise
  df[sapply(df$arguments, function(x) { grepl(paste(instrumental_general, collapse = '|'), x) }) &
       (df$INSTRUMENTAL_HUMANS == 0), 'INSTRUMENTAL_GENERAL'] <- 1

  # Assign the value 1 for the column INSTRUMENTAL_UNSPECIFIED to rows that are neither INSTRUMENTAL_HUMANS nor
  # INSTRUMENTAL_GENERAL and that contain at least one INSTRUMENTAL_UNSPECIFIED premise
  df[sapply(df$arguments, function(x) { grepl(paste(instrumental_unspecified, collapse = '|'), x) }) &
       (df$INSTRUMENTAL_HUMANS == 0) &
       (df$INSTRUMENTAL_GENERAL == 0), 'INSTRUMENTAL_UNSPECIFIED'] <- 1

  # Assign the value 1 for the column NON_INSTRUMENTAL to rows that are neither INSTRUMENTAL_HUMANS, nor
  # INSTRUMENTAL_GENERAL, nor INSTRUMENTAL_UNSPECIFIED
  df[(df$INSTRUMENTAL_HUMANS == 0) &
       (df$INSTRUMENTAL_GENERAL == 0) &
       (df$INSTRUMENTAL_UNSPECIFIED == 0), 'NON_INSTRUMENTAL'] <- 1
  return(df)
}

clean_data_for_arguments_other_variables_analysis <- function(questions, cols_to_keep) {
  data <- load_data()

  categories <- c("INSTRUMENTAL_HUMANS", "INSTRUMENTAL_GENERAL", "INSTRUMENTAL_UNSPECIFIED", "NON_INSTRUMENTAL")

  for (question in questions){
    # obtain categories
    data_args <- clean_data_insect_or_biodiv_need_for_protection_question(data, question)
    df <- data_args[[1]]
    df <- build_arguments(df)
    df <- categorize_arguments(df)

    # factorize categories
    cat_question <- sprintf('Q%s_category_arguments', question)
    df[cat_question] <- NA
    for (cat in categories) {
      df[df[[cat]] == 1, cat_question] <- cat
    }
    df[[cat_question]] <- as.factor(df[[cat_question]])

    # merge categories with other columns
    data <- merge(data, df, by = "id_final")
  }

  # select useful columns
  data <- data[, cols_to_keep]

  # keep only rows with no NA and no empty cells
  data <- data[(rowSums(is.na(data)) == 0) & (rowSums(data == '') == 0), ]
  
  # remove NA levels
  for (col in colnames(data)){
    if(is.factor(data[[col]])) {
      data[[col]] <- factor(data[[col]])
    }
  }

  data <- rename_and_simplify_var(data)

  for (var in c("gender", "education_level", "naturalist_activity", "biodiv_activity", "nb_insects_among_threatened", "nb_known_insects", "inclusion_of_nature_in_self")) {
    if (var %in% colnames(data)) {
      data[[var]] <- as.factor(data[[var]])
    }
  }
  return(data)
}

rename_and_simplify_var <- function(data) {
  # rename columns
  colmap <- list("Q12_NB_TAXON_INSECTS" = "nb_insects_among_threatened", "Q14_NB_INSECTS" = "nb_known_insects",
                  "Q67" = "age", "Q68" = "gender", "Q69" = "rural_urban", "Q72" = "inclusion_of_nature_in_self",
                 "Q73" = "diploma", "Q76" = "biodiv_activity", "Q77_CODED" = "naturalist_activity")
  colnames(data) <- sapply(colnames(data), function(x){if (x %in% names(colmap)){colmap[x]} else{x}})


  # simplify diploma variable
  if ("diploma" %in% colnames(data)) {
    data$education_level <- "above Bachelor"
    data$education_level[data$diploma <= 3] <- "Bachelor or below"
  }

  # rebalance naturalist activity
  if ("naturalist_activity" %in% colnames(data)) {
    data[data$naturalist_activity %in% c(1, 2), "naturalist_activity"] <- "naturalist"
    data[(data$naturalist_activity != "naturalist") & (!is.na(data$naturalist_activity)), "naturalist_activity"] <- "not_naturalist"
  }

  # rebalance biodiv activity
  if ("biodiv_activity" %in% colnames(data)) {
    data[data$biodiv_activity %in% c(1, 2), "biodiv_activity"] <- "biodiv_profession"
    data[(data$biodiv_activity != "biodiv_profession") & (!is.na(data$biodiv_activity)), "biodiv_activity"] <- "no_biodiv_profession"
  }

  # categorize nb_insects_among_threatened
  if ("nb_insects_among_threatened" %in% colnames(data)) {
    data[(data$nb_insects_among_threatened > 0) & (!is.na(data$nb_insects_among_threatened)), "nb_insects_among_threatened"] <- "at_least_one_insect"
    data[(data$nb_insects_among_threatened != "at_least_one_insect") & (!is.na(data$nb_insects_among_threatened)), "nb_insects_among_threatened"] <- "no_insect"
  }

  # categorize nb_known_insects
  if ("nb_known_insects" %in% colnames(data)){
    data[(data$nb_known_insects >= 9) & (!is.na(data$nb_known_insects)), "nb_known_insects"] <- "at_least_nine"
    data[(data$nb_known_insects != "at_least_nine") & (!is.na(data$nb_known_insects)), "nb_known_insects"] <- "less_than_nine"
  }

  # categorize inclusion_of_nature_in_self
  if ("inclusion_of_nature_in_self" %in% colnames(data)) {
    data[data$inclusion_of_nature_in_self > 5, "inclusion_of_nature_in_self"] <- "above_five"
    data[data$inclusion_of_nature_in_self != "above_five", "inclusion_of_nature_in_self"] <- "five_or_less"
  }

  return(data)
}