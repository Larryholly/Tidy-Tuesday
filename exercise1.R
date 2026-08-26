library(tidyverse)



demo_language <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/demo_by_first_language.csv')
demo_nationality <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/demo_by_nationality.csv')
demo_reasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/demo_by_reasons.csv')
performance_language <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/performance_by_first_language.csv')
performance_nationality <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/performance_by_nationality.csv')

names(demo_by_first_language)
# "type"     "language" "band"     "percent"  "year"  
names(demo_by_nationality)
#"type"        "nationality" "band"        "percent"     "year"
names(performance_by_first_language)
#"type"     "language" ""     "score"    "year"
names(performance_by_nationality)
# "type"        "nationality" "part"        "score"       "year"  

# Which parts of the test do test takers find the most difficult? Is it the same for all test takers?

performance_language$score


#difficulty by type of test general training vs academic by by_first_language pt 1
#performance_by_first_language 

performance_language |> 
  group_by(type) |> 
  summarise(
    mean_score = mean(score,na.rm=TRUE),  #
    median_score = median(score,na.rm=TRUE),
    good_score = sum(6 < score,na.rm=TRUE),
    low_score = sum(6 > score,na.rm=TRUE)
  )

#academic: average score 6.42 , good score 482 , low score 105
#gentraining average score 6.15, good score 368, low score 215
 
performance_language |> 
  group_by(part) |> 
  summarise(
    mean_score = mean(score,na.rm=TRUE),  #
    median_score = median(score,na.rm=TRUE),
    good_score = sum(6 < score,na.rm=TRUE),
    low_score = sum(6 > score,na.rm=TRUE) ) |> 
      arrange(desc(mean_score))



# average score by the  type of part of test by performance_language
# type      mean(score)
#listening        6.41
#speaking        6.38 
# overall         6.34
#reading         6.28
#writing         6.04
  

#pt 2

#difficulty by type of test general training vs academic  by nationality
# performance_nationality  "type"        "nationality" "part"        "score"       "year" 

performance_nationality |> 
  group_by(type) |> 
  summarise(
    mean_score = mean(score, na.rm=TRUE),
    median_score= median(score, na.rm = TRUE),
    good_score = sum( 6 < score, na.rm = TRUE),
    low_score = sum(6 > score, na.rm = TRUE)
                      )
  
#academic: average score 6.34 , good score 427 , low score 152
#gentraining average score 6.18, good score 363, low score 211

performance_nationality |> 
  group_by(part) |> 
  summarise(
    mean_score = mean(score, na.rm=TRUE),
    median_score= median(score, na.rm = TRUE),
    good_score = sum( 6 < score, na.rm = TRUE),
    low_score = sum(6 > score, na.rm = TRUE)
  )

# average score by the  type of part of test by performance_nationality 
# type        mean(score)    good_Score   low_score

# Listening      6.37       166          65   
#overall         6.31       175          61
#reading         6.27       159         72
#speaking        6.35       169           64
#writing         5.99       121           101


 # Do English speakers consistently get top marks on the English language test?
# English speakers tied with Portuguese for 4th best (6.88 score), otherwise italian(6.92),greek(7.12),german(7.54)

performance_language$language
performance_language$type
performance_language$score

performance_language |> 
  group_by(language) |> 
  filter(type %in% c("Academic")) |>
  summarise(lan_mean = mean(score, na.rm=TRUE)) |> 
  arrange(desc(lan_mean)) |> 
  print(n=8)
  





 
# demo_reasons 
# "type"    "reason"  "band"    "percent" "year" 

demo_reasons$reason

#recatogorize band as numeric
demo_reasons <- demo_reasons |> 
  mutate(band = as.numeric(band))


# recat band into low, middle, high

demo_reasons <- demo_reasons |> 
  mutate(
    band_category = case_when(
      band >= 4.0 & band < 6.0 ~ "Low",
      band >= 6.0 & band < 8.0 ~ "Middle",
      band >= 8.0 & band <= 9.0 ~ "High",
      is.na(band) ~ "Missing",
      TRUE ~ "Other"
    )
  )

#recat reason variables 

demo_reasons <- demo_reasons |> 
  mutate(
    reason_category = case_when(
      reason == "For employment" ~ "Employment",
      
      reason %in% c(
        "For higher education extended course (three months or more)",
        "For higher education short course (three months or less)",
        "For other education purposes"
      ) ~ "Education",
      
      reason == "For immigration" ~ "Immigration",
      
      reason == "For personal reasons" ~ "Personal",
      
      reason == "For professional registration (NOT medical)" ~ 
        "Professional Registration",
      
      reason %in% c(
        "For registration as a dentist",
        "For registration as a doctor",
        "For registration as a nurse (including CGFNS)"
      ) ~ "Medical Registration",
      
      reason == "(Other)" ~ "Other",
      
      TRUE ~ NA_character_
    )
  )



demo_reasons$reason_category

demo_reasons |> 
  count(reason_category, band_category)

demo_reasons |> 
  filter(is.na(band_category)) |> 
  count(reason_category, band, sort = TRUE)


p1 <- ggplot(demo_reasons, aes(x = reason_category, fill = band_category)) +
  geom_bar(position = "stack") +
  labs(
    x = "Reason Category",
    y = "Count",
    fill = "Band Category"
  )


