library(tidyverse)

Sys.setlocale(locale = "hebrew")

# crime <- read_csv("data/criminal_files_detailed.csv", 
#                   locale = locale(encoding = "ISO-8859-8-I"))

# crime %>% 
#   write_excel_csv("data/crime_fixed_encoding.csv")

crime <- read_csv("data/crime_fixed_encoding.csv")

# Very useful method for identifying encoding...!
#readr::guess_encoding("data/criminal_files_detailed.csv")

crime %>% glimpse()

# Identifying different types of offences

crime %>% 
  group_by(StatisticCrimeType) %>% 
  summarize(total_offences = sum(TikimSum)) %>% 
  arrange(desc(total_offences)) %>% 
  View()

# Focusing on specific types of offences (over time? area?)

selected_offences <- crime %>% 
  filter(StatisticCrimeType == "התפרצות לבתי עסק ומוסדות" | 
           StatisticCrimeType == "התפרצות לבית דירה" | 
           StatisticCrimeType == "קטטות והפרעות ברחובות" | 
           StatisticCrimeType == "מרמה ועושק")

public_employee_offences <- crime %>% 
  filter(StatisticCrimeType == "תקיפת עובדי צבור בתפקיד")

offences_over_time <- selected_offences %>% 
  bind_rows(public_employee_offences) %>% 
  group_by(Quarter, StatisticCrimeType) %>% 
  summarize(num_offences = sum(TikimSum))

offences_over_time %>% 
  ggplot(aes(x = Quarter, y = num_offences, fill = StatisticCrimeType)) + 
  geom_col()

offences_over_time %>% 
  ggplot(aes(x = Quarter, y = num_offences)) + 
  geom_col() +
  facet_wrap(~StatisticCrimeType, scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90))

# Distribution of offences by district/precinct

offences_district_comparison <- selected_offences %>% 
  bind_rows(public_employee_offences) %>% 
  group_by(PoliceDistrict, StatisticCrimeType) %>% 
  summarize(num_offences = sum(TikimSum))

offences_district_comparison %>% 
  ggplot(aes(x = PoliceDistrict, y = num_offences, fill = StatisticCrimeType)) + 
  geom_col(position = position_fill()) + 
  scale_y_continuous(labels = scales::percent_format(1)) + 
  xlab("מחוז") + 
  ylab("\u202bשיעור העברות [%]") + 
  ggtitle("התפלגות עברות לפי מחוז 2017-2022")


# Independence test -------------------------------------------------------

tot_offences_overall <- sum(offences_district_comparison$num_offences)

district_marginal <- offences_district_comparison %>% 
  group_by(PoliceDistrict) %>% 
  summarize(tot_offences_per_district = sum(num_offences)/tot_offences_overall)
  
offence_marginal <- offences_district_comparison %>% 
  group_by(StatisticCrimeType) %>% 
  summarize(tot_offences_per_type = sum(num_offences)/tot_offences_overall)

prep_chisq <- offences_district_comparison %>% 
  left_join(district_marginal) %>% 
  left_join(offence_marginal) %>% 
  mutate(expected = tot_offences_per_district*tot_offences_per_type*tot_offences_overall) %>% 
  mutate(chisq = (expected - num_offences)^2/expected)


pchisq(q = sum(prep_chisq$chisq), df = 7*4)


# W/ chisq.test function (easier...)

chisq_x <- offences_district_comparison %>% 
  pivot_wider(id_cols = PoliceDistrict, names_from = StatisticCrimeType, values_from = num_offences) %>% 
  ungroup() %>% 
  select(-PoliceDistrict) %>% 
  as.matrix()

chisq.test(x = chisq_x)

# Example of chisq distribution

tibble(x = seq(0, 100, 1)) %>% 
  mutate(pchisq = dchisq(x, df = 7*4)) %>% 
  ggplot(aes(x = x, y = pchisq)) + 
  geom_line()


# Addendum - str_detect

str_detect(crime$StatisticCrimeType[100], iconv("גניבת", to = "UTF-8"))


stringi::stri_detect(crime$StatisticCrimeType[100], 
                     regex = iconv("גניבת", to = "UTF-8"))
