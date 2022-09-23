library(readr)
# The goal of readr is to provide a fast and friendly way to read rectangular
# data from delimited files, such as comma-separated values (CSV) and
# tab-separated values (TSV).

library(dplyr) #This helps m
# dplyr is a grammar of data manipulation, providing a consistent set of verbs
# that help you solve the most common data manipulation challenges

library(tidyr)
# The goal of tidyr is to help you create tidy data. Tidy data is data where:
#
# Every column is variable.
# Every row is an observation.
# Every cell is a single value.


# Read the data in with the read_csv function in the readr package. For now we
# will just give it one argument (i.e., option), which is the `file` which it
# will read. Since this script is part of an R project, and we've opened the R
# project, our working directory is already set.

#Questions: Missing value codes??
all_data <- readr::read_csv('STARCHILD_DATA_2022-09-23_1924.csv',
                            na = c('', '999'))

#Hopefully you know what variables you need for the questionnaire you're scoring.
ctq_data <- all_data %>%
  select(record_id, redcap_event_name, n_month,
         matches('ctq.*')) %>%
  filter(!is.na(ctq_timestamp))

# Reverse coding some items. Note below I don't recode "3" because its recoded
# value would be 3.
ctq_rev_coded_items <- c('ctq_2', 'ctq_5', 'ctq_7', 'ctq_13', 'ctq_19', 'ctq_26', 'ctq_28')
ctq_data_rev_coded <- ctq_data %>%
  mutate(across(all_of(ctq_rev_coded_items),
                ~ recode(., `1` = 5, `2` = 4,
                         `4` = 2, `5` = 1),
                .names = '{.col}_r'))

# Now, for each row, we compute an average score.
ctq_scored <- ctq_data_rev_coded %>%
  rowwise() %>%
  mutate(ctq_physneg = mean(c_across(all_of(c('ctq_1', 'ctq_2_r', 'ctq_4', 'ctq_6', 'ctq_26_r'))), 
                            na.rm = TRUE),
         ctq_emoneg = mean(c_across(all_of(paste0('ctq_', c('5_r', '7_r', '13_r', '19_r', '28_r')))),
                           na.rm = TRUE),
         ctq_emoab = mean(c_across(all_of(paste0('ctq_', c(3, 8, 14, 18, 25)))),
                           na.rm = TRUE),
         ctq_physab = mean(c_across(all_of(paste0('ctq_', c(9, 11, 12, 15, 17)))),
                           na.rm = TRUE),
         ctq_sexab = mean(c_across(all_of(paste0('ctq_', c(20, 21, 23, 24, 27)))),
                           na.rm = TRUE),
         ctq_neglect = mean(c_across(all_of(paste0('ctq_', c(1, '2_r', 4, '5_r', 6, '7_r', 
                                                             '13_r', '19_r', '26_r', '28_r')))),
                            na.rm = TRUE),
         ctq_abuse = mean(c_across(all_of(paste0('ctq_', c(3, 8, 9, 11, 12, 14, 15, 17, 18,
                                                           20, 21, 23, 24, 25, 27)))),
                          na.rm = TRUE),
         ctq_avg = mean(c_across(all_of(paste0('ctq_', c(1, '2_r', 4, '5_r', 6, '7_r', 
                                                         '13_r', '19_r', '26_r', '28_r',
                                                         3, 8, 9, 11, 12, 14, 15, 17, 18,
                                                         20, 21, 23, 24, 25, 27)))),
                        na.rm = TRUE))

library(ggplot2)
ctq_long_by_var_and_month <- ctq_scored %>%
  select(record_id, redcap_event_name,
         ctq_physneg, ctq_emoneg,
         ctq_emoab, ctq_physab,
         ctq_sexab, ctq_neglect,
         ctq_abuse, ctq_avg) %>%
  pivot_longer(matches('ctq')) 

ctq_long_by_var_and_month %>%
  ggplot(aes(x = value)) + 
  geom_histogram(bins = 10) + 
  facet_grid(redcap_event_name ~ name)

ctq_data %>%
  select(redcap_event_name, record_id, matches('^ctq_\\d+$')) %>%
  pivot_longer(matches('ctq')) %>%
  summarize(unique(value))
  
# It's a good idea to put this function at the end of your scripts so that you
# can easily retrieve information about the version of R, and what packages are
# loaded. When you have a problem, the info here can be helpful for solving it
# (and for others to help you solve it).
sessionInfo()

