

#> ChatGPT tells me its fine to call library here
library(tidyverse)
library(labelled) #for adding labels to save to SPSS
library(haven) #for reading/saving SPSS (.sav) file
library(usethis)


#> Updated Data Frame:
df <- read_csv("data-raw/5ll_ALL_dec24_2024_clean.csv")
#> For whatever reason, I forgot to remove the "age float" variable when
#> doing my python cleaning. The "age_part" is correct and I can safely remove
#> the age_float column
df <- df |> dplyr::select(-age_float)

#> GOALS -----
#> (1) I need to make variables factors and make sure the factor names are
#> correct.
#> (2) Make specific variable selections so I can grab different
#> variables for analyses easier.
#> (3) Label variables with questions
#> (4) Make relationship satisfaction composite

#> Making variables factors: ------

df <- df |>
  mutate(
    sex_bio = factor(sex_bio) |>
      recode_factor(
        "1" = "Male",
        "2" = "Female"),
    gender = factor(gender) |>
      recode_factor(
        "1" = "Man",
        "2" = "Woman",
        "3" = "Non-binary",
        "4" = "Other"),
    rel_status_1 = factor(rel_status_1) |>
      recode_factor(
        "1" = "Single",
        "2" = "Casually dating one or more person",
        "3" = "Seriously dating one person",
        "4" = "Seriously dating more than one person (polyamorous)",
        "6" = "Engaged or married, and exclusive to that one person",
        "7" = "Engaged or married, and in an open, swinging, or polyamorous relationship with that person.",
        "8" = "Other" ),
    children = factor(children) |>
      recode_factor(
        "1" = "None",
        "2" = "One",
        "3" = "Two",
        "4" = "Three",
        "5" = "Four",
        "6" = "Five",
        "7" = "Six",
        "8" = "Seven or more"),
    sex_or_part = factor(sex_or_part) |>
      recode_factor(
        "1" = "Heterosexual",
        "2" = "Homosexual",
        "3" = "Bisexual",
        "4" = "Asexual",
        "5" = "Other"
      ),
    ethnic_1 = factor(ethnic_1) |>
      recode_factor(
        "1" = "Hispanic or Latino",
        "2" = "White",
        "3" = "Black or African American",
        "4" = "American Indian or Alaska Native",
        "5" = "Asian",
        "6" = "Native Hawaiian or Pacific Islander",
        "7" = "Other",
        "8" = "Multi-ethnic"
      ),
    sample = factor(sample)

  )

#> Fixing the coding of a variable: ----

#This variable is two options and has answers 26 and 27.
#df$taken_before
#> 26 = No
#> 27 = Yes
#> I am going to code "No" as "0" and "Yes" as "1"
#df |> count(taken_before)
# 161 missing responses for whatever reason. I guess lots of people didn't
# care to respond to the variable

df <- df |> mutate(
  taken_before = if_else(taken_before == 26, 0,
                         if_else(taken_before == 27, 1, taken_before))
)

#> Sanity check here suggests that variable was edited perfectly
df |> count(taken_before)
df <- df |> mutate(
  taken_before = factor(taken_before) |>
    recode_factor(
      "0" = "No",
      "1" = "Yes"
    )
)

#> Creating a more simple relationship status variable: -----

df <- df |> mutate(
  rel_status_2 = if_else(rel_status_1 == "Casually dating one or more person", "Other",
                         if_else(rel_status_1 == "Seriously dating one person" |
                                   rel_status_1 == "Engaged or married, and exclusive to that one person",
                                 "Committed Relationship",
                                 if_else(rel_status_1 != "Comitted Relationship" & rel_status_1 != "Single", "Other", rel_status_1)))
)

# if relationship status 1 = casually dating one or more people, then rename it "Other"
# if FALSE: if relationship status 1 = Seriously Dating or Engaged or married, then rename it "Committed Relationship",
# if FALSE: if relationship status 1 is not "committed relationship" and is not "Single", then rename it "Other".

#> Making the variable a factor:

df <- df |> mutate(
  rel_status_2 = factor(rel_status_2))


#> Removing "-" from variable names ----

df <- df |> rename_with(~ gsub("-", "_", .))

#> Variable Labels -----

var_label(df) <- list(
  "pid" = "Unique participant ID",
  "sample" = "Which data collection the participant came from",
  "time_collected" = "Time period the specific sample was collected",
  "duration_minutes" = "Time taken to complete the survey (minutes)",
  "duration_seconds" = "Time taken to complete the survey (seconds)",
  "rel_leng_months" = "Participant romantic relationship length in months",
  "rel_leng_years" = "Participant romantic relationship length in years",
  "ethnic_1" = "Recoded ethnic variable where multiple ethnicities coded as 8",
  "rec_woa_rev_1" = "Being rude to me and hurting my feelings.",
  "ex_woa_rev_1"="Being rude to my partner and hurting their feelings.",
  "rec_woa_1"="Complimenting my appearance.",
  "ex_woa_1"="Complimenting my partner's appearance.",
  "read"="Did you read the questions?This question will not be included in your final results; the question is only used to control data quality and your response will be kept confidential.",
  "participant_rq"="Do you have any research questions related to the 5 Love Languages? In a sentence, or more, please tell us what research questions you want us to explore!",
  "ex_aos_2"="Doing little things for my partner to make them happy.",
  "rec_aos_2"="Doing little things to make me happy.",
  "rec_aos_4"="Finishing a chore for me when I don't have the time to do it.",
  "e_aos_4"="Finishing a chore for them when they don't have the time to do it.",
  "rec_pt_3"="Giving me a kiss.",
  "rec_g_4"="Giving me a surprise present when there's no special occasion.",
  "rec_g_3"="Giving me a thoughtful birthday gift.",
  "ex_qt_2"="Giving my full attention to my partner when I see them.",
  "e_pt_3"="Giving my partner a kiss.",
  "rec_qt_2"="Giving their full attention to me when they see me.",
  "e_g_4"="Giving them a surprise present when there's no special occasion.",
  "e_g_3"="Giving them a thoughtful birthday gift.",
  "taken_before"="Have you ever taken a five love language test before?",
  "rec_pt_1"="Having sex with me.",
  "ex_pt_1"="Having sex with my partner",
  "e_qt_5"="Having a quality conversation.",
  "rec_qt_5"="Having a quality conversation.",
  "rec_pt_1"="Having sex with me.",
  "ex_pt_1"="Having sex with my partner.",
  "rec_aos_5"="Helping me keep things cleaned up.",
  "e_aos_5"="Helping my partner keep things cleaned up.",
  "rec_pt_4"="Holding my hand.",
  "e_pt_4"="Holding my partner's hand.",
  "grs_com_commit"="How committed are you to your relationship?",
  "grs_int_connected"="How connected are you to your partner?",
  "recieved_survey"="How did you find this survey?",
  "grs_sat_happy"="How happy are you with your relationship?",
  "honest"="How honest were you when taking this survey?This question will not be included in your final results; the question is only used to control data quality and your response will be kept confidential.",
  "children"="How many children do you have?",
  "grs_tru_count"="How much can you count on your partner?",
  "grs_lov_love"="How much do you love your partner?",
  "grs_pas_intense"="How sexually intense is your relationship?",
  "rec_pt_2"="Hugging me.",
  "e_pt_2"="Hugging my partner.",
  "pref_recieve"="In your current (or most recent) romantic relationship, what way did your partner> most express their love for you?",
  "pref_express"="In your current (or most recent) romantic relationship, what way have you> most preferred to express love to your partner?",
  "first_time"="Is this your first time taking this survey?",
  "first_time"="Is this your first time taking this survey?",
  "rec_qt_3"="Making a sacrifice to spend extra time with me.",
  "ex_qt_3"="Making a sacrifice to spend extra time with my partner.",
  "rec_qt_1"="Making a special effort to spend time with me. ",
  "ex_qt_1"="Making a special effort to spend time with my partner. ",
  "rec_aos_5"="Making dinner for me when I'm busy.",
  "ex_g_2"="Randomly getting my partner a small gift",
  "rec_g_5"="Paying for my favorite snack.",
  "ex_g_1"="Paying for our dates.",
  "rec_g_1"="Paying for our dates.",
  "ex_g_1"="Paying for our evening entertainment.",
  "rec_g_1"="Paying for our evening entertainment.",
  "e_g_5"="Paying for their favorite snack.",
  "rec_aos_pt_1"="Performing oral sex on me.",
  "ex_aos_pt_1"="Performing oral sex on my partner.",
  "rec_aos_pt_2"="Performing sexual acts for me.",
  "ex_aos_pt_2"="Performing sexual acts for my partner.",
  "rec_qt_4"="Really listening to me",
  "rec_g_2"="Randomly getting me a small gift.",
  "ex_g_2"="Randomly getting my partner a small gift.",
  "rec_qt_4"="Really listening to me.",
  "e_qt_4"="Really listening to my partner.",
  "rec_aos_pt_3"="Rubbing my back.",
  "ex_aos_pt_3"="Rubbing my partners back.",
  "rec_aos_3"="Running errands for me.",
  "e_aos_3"="Running errands for my partner.",
  "e_qt_6"="Spending my free time with them.",
  "rec_qt_6"="Spending their free time with me.",
  "ex_aos_1"="Taking time out of my day to support my partner.",
  "rec_aos_1"="Taking time out of their day to support me.",
  "rec_woa_6"="Telling me how important I am to them.",
  "rec_woa_4"="Telling me that they are committed to me.",
  "rec_woa_2"="Telling me that they love me.",
  "rec_woa_3"="Telling me that they miss me.",
  "ex_woa_5"="Telling my partner how important they are to me.",
  "ex_woa_4"="Telling my partner I am committed to them.",
  "ex_woa_2"="Telling my partner I love them.",
  "ex_woa_3"="Telling my partner I miss them.",
  "rec_woa_6"="Telling other people how great I am.",
  "ex_woa_6"="Writing sweet notes to my partner.",
  "age_part"="What is your age? (years)",
  "sex_bio"="What is your biological sex?",
  "rel_status_1"="What is your current relationship status?",
  "rel_status_2" = "Simplified current relationship status variable.",
  "ethnic"="What is your ethnic/racial background? (Please select all that apply)",
  "code_relation"="What is your relationship to the person who sent you the survey?",
  "gender"="What is your self-identified gender?",
  "sex_or_part"="What is your sexual orientation?",
  "top_ll"="What was your top love language when you previously took the test?",
  "year_born"="What year were you born? (For example: 1992)",
  "circ_sat"="Which of the following circumstances would make you most satisfied in a romantic relationship?",
  "rec_woa_5"="Writing sweet notes to me."
)



#> Updating participant ID variable

df$pid <- 1:950

#> Saving Updated data frame in different formats ------

fivelovelanguages <- df

usethis::use_data(fivelovelanguages, overwrite = TRUE)


