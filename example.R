library(readr)
source('score_questionnaire.R')

my_data <- readr::read_csv('STARCHILD_DATA_2022-09-23_1924.csv',
                           na = c('', '999'))

scoring_params_list <- list(name = 'ctq_physneg',
                            cols = c('ctq_1', 'ctq_2', 'ctq_4', 'ctq_6', 'ctq_26'),
                            rev_score_cols = c('ctq_2', 'ctq_26'),
                            min = 1,
                            max = 5)

ctq_physneg <- score_questionnaire(x = my_data,
                                   cols = scoring_params_list$cols,
                                   rev_score_cols = scoring_params_list$rev_score_cols,
                                   min = scoring_params_list$min,
                                   max = scoring_params_list$max)

hist(ctq_physneg)

