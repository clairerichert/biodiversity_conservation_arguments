load_data <- function() {
  #' Load data
  #'
  #' Function that:
  #'    - binds the three survey together
  #'    - merges raw data with coded data
  #'    - defines factor and numeric data
  #'    - code likert responses

  # load data
  data <- do.call(rbind,
                      lapply(list('1', '2', '3'),
                             function(i) { read.csv(sprintf("raw data/raw_survey%s.csv", i),
                                                    header = TRUE, encoding = 'UTF-8', sep = ';') }))

  # remove first raw (question description
  data <- data[-1, ]

  # merge data which have been manually recoded (q3, q18, q74, q77) and raw_data
  for (question in list('Q3','Q18', 'Q74', 'Q77')) {
    question_df <- read.csv(sprintf('data/%s.csv', question), header = TRUE, sep = ';', skip = 1)
    # keep only columns with id and coding
    if (question %in% c('Q3', 'Q18')){
      question_df <- question_df[ , -2]
    }
    colnames(question_df)[2] <- question
    # merge with raw_data
    # remove non coded question
    data <- data[, !names(data) == question]
    data <- merge(data, question_df, by = 'id_final')
  }

  # code Q68 (gender)
  data[!(data$Q68 %in% c('Féminin', 'Masculin')), 'Q68'] <- NA

  # transform some columns into dummys (Q70: country of origin)
  data$country_france <- 0
  data$country_france[grep('france', tolower(data$Q70))] <- 1

  # coerce numeric columns
  q_numeric <- c('Q67', 'Q69', 'Q72')
  data[, q_numeric] <- lapply(data[, q_numeric], as.numeric)

  # code all columns
  for (col in colnames(data)[3:dim(data)[2]]){
    data[[col]] <- replace_value(data[[col]])
  }

  # coerce factor columns
  q_factors <- paste0('Q', c(68, '74_CODED'))
  data[, q_factors] <- lapply(data[, q_factors], as.factor)

  return(data)
}

mapping <- read.csv('data/mapping.csv', header = TRUE, encoding = 'UTF-8', sep = ';')
mapping <- mapping[!is.na(mapping$code),]

replace_value <- function(column) {
  if (is.character(column)) {
    return(mapping$code[match(column, mapping$french)])
  } else {
    return(column)
  }
}

main_causes_of_decline <- c(
  'Destruction et fragmentation des habitats',
  ' Changement climatique',
  ' Utilisation de produits chimiques à usage agricole')

code_causes_of_decline <- function(answer) {
  list_causes <- unlist(strsplit(answer, ','))
  return(length(intersect(list_causes, main_causes_of_decline)))

}
