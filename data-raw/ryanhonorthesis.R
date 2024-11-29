## code to prepare `ryanhonorthesis` dataset goes here
#' @source Data collected in fall 2022 for Ryan Dobson's undergrad honors thesis on
#' jealousy in opposite-sex friendships
#' @format Data are in wide format where the scenario people are responding to emotions
#' on is depicted by "osf." ssf." or "rom." as a variable prefix.

usethis::use_data(ryanhonorthesis, overwrite = TRUE)

ryanhonorthesis |>
  rename_all(tolower) |>  #renaming all to lowercase for ease of editing
  select(
    -meeting.type:-email.communication.type #removing some variables I don't plan to analyze
  ) |>
  mutate(
    pid = 1:nrow(ryanhonorthesis) #creating a participant id variable
  ) |>
  select(pid, everything()) |>  #bringing the pid variable to the front of the dataset
  mutate(single_or_dating = relationship.status.participant)|>
  mutate(single_or_dating = replace(single_or_dating, single_or_dating == 2, 1),
         single_or_dating = replace(single_or_dating, single_or_dating == 4, 2),
         single_or_dating = replace(single_or_dating, single_or_dating == 3, 2),
  )  |>  #above code adds in an additional variable for single versus mated people
  mutate(age.participant = replace(age.participant, age.participant == 211, 21)) |>
  filter(age.participant <= 24, #include all people who are less than or equal to 24 years old
         gender != 3) |>  #include all people who are not a "3"
  filter(pid != 3) |>  #include all of the people who aren't pid = 3
  mutate(gender = factor(gender) |> #below code is making all of the variables factors
           recode_factor("1" = "Men",
                         "2" = "Women"),
         single_or_dating = factor(single_or_dating) |>
           recode_factor("1" = "Single",
                         "2" = "Relationship"),
         relationship_status_participant = factor(relationship.status.participant) |>
           recode_factor("1" = "Single",
                         "2" = "Casually Dating",
                         "3" = "Commited Relationship",
                         "4" = "Engaged/Married"),
         sexual_orientation_participant = factor(sexual.orientation.participant) |>
           recode_factor("1" = "Bisexual",
                         "2" = "Heterosexual",
                         "3" = "Homosexual",
                         "4" = "Non-binary/other"),
         sexual_orientation_friend = factor(sexual.orientation.friend) |>
           recode_factor("1" = "Bisexual",
                         "2" = "Heterosexual",
                         "3" = "Homosexual",
                         "4" = "Non-binary/other"),
         osf_closer_ssf = factor(osf.closer.ssf) |>
           recode_factor("1" = "not.closer",
                         "2" = "closer")
  ) |>
  select(  #removing some duplicates that were created when making new variables with mutate
    -osf.closer.ssf, -sexual.orientation.friend, -sexual.orientation.participant,
    -relationship.status.participant
  )
