library(readxl)
library(tidyverse)
library(writexl)
setwd("C:/Users/francis.fayolle/OneDrive - United Nations Development Programme/Documents/World Cup Betting")
gameschedule <- read_excel("./gameschedule.xlsx")
other <- read_excel("./otherquest.xlsx", sheet = "survey")%>%
  mutate( required = as.character(required))
players <- read_excel("./otherquest.xlsx", sheet = "choices")

##### Group Stage Games #####

choices <- gameschedule %>%
  # Filter out rows with "TBD" in ISO Code (knockout stage placeholder matches)
  filter(round == "1st" |round == "2nd" |round == "3rd") %>%
  # Create the two rows per match
  rowwise() %>%
  do({
    # Split the ISO Code column into two country codes
    iso_codes <- str_split(.$`ISO Code`, " ")[[1]]
    # Split the Match column into two country names
    countries <- str_split(.$Match, " vs ")[[1]]
    
    # Create two rows
    data.frame(
      list_name = rep(.$matchnumber, 2),
      round = rep(.$round, 2),
      phase = rep(.$phase, 2),
      name = iso_codes,
      label = countries,
      stringsAsFactors = FALSE
    )
  }) %>%
  arrange(phase)%>%
  ungroup()

# View the result
head(choices)

survey <- gameschedule %>%
  # Filter out TBD matches (optional - remove if you want to keep them)
  filter(round == "1st" |round == "2nd" |round == "3rd") %>%
  arrange(phase)%>%
  # Create three rows per game
  rowwise() %>%
  do({
    # Extract ISO codes for default value
    iso_codes <- .$`ISO Code`
    
    # Create the three rows for each game
    data.frame(
      type = c("text", 
               "calculate",
               "calculate",
               "calculate",
               "note"),
      name = c(paste0("score", .$matchnumber), 
               paste0("sumgoal", .$matchnumber),
               paste0("diffgoal", .$matchnumber),
               paste0("resultgame", .$matchnumber),
               paste0("note", .$matchnumber)),
      label = c(  paste0( .$phase, " : ", .$Match),
                  "calcsumgoal",
                  "calcdiffgoal",
                  "resultgame",  
                  paste0("${", paste0("resultgame", .$matchnumber), "}", " ${score", .$matchnumber, "}")),
      hint = c("Register the score in the order of the teams in the title. 
Only numbers separated by a - i.e.:  0-0  ;  3-1 ", 
               NA, 
               NA,
               NA,
               NA),
      constraint_message = c("Use the format number-number",
                             NA,
                             NA,
                             NA,
                             NA),
      required = c("TRUE",
                   NA,
                   NA,
                   NA,
                   NA),
      relevant = c( paste0("${round} = '",.$round,"'" ),
                    paste0("${round} = '",.$round,"'" ),
                    paste0("${round} = '",.$round,"'" ),
                    paste0("${round} = '",.$round,"'" ),
                    paste0("${round} = '",.$round,"'" )),
      constraint = c("regex(., '^[0-9+]-[0-9+]$')",
                     NA, 
                     NA,
                     NA,
                     NA),
      calculation = c(NA, 
                      paste0("substr(${score", .$matchnumber, "}, 0, 1) + substr(${score", .$matchnumber, "}, 2, 4)"), 
                      paste0("substr(${score", .$matchnumber, "}, 0, 1) - substr(${score", .$matchnumber, "}, 2, 4)"),
                      paste0("if(${diffgoal", .$matchnumber,"} > 0, '", .$hometeam, "  wins!!! ', if(${diffgoal", .$matchnumber,"} = 0, '", .$hometeam, " and ", .$awayteam , "  draw!!!  ', if(${diffgoal", .$matchnumber,"} < 0, '", .$awayteam, "  wins!!!  by  ', 'Please enter a score')))"),
                      NA),
      stringsAsFactors = FALSE
    )
  }) %>%
  ungroup()

# View the result
head(survey, 10)


survey <- bind_rows(other,
                    survey)

choices <- bind_rows(players,
                     choices)

questionnaire <- list(survey = survey,
                      choices = choices)

write_xlsx(questionnaire, "./surveyWCgroup.xlsx")

##### Knockout Stage Games #####

# 
# choices <- gameschedule %>%
#   # Filter out rows with "TBD" in ISO Code (knockout stage placeholder matches)
#   filter(!grepl("TBD", `ISO Code`)) %>%
#   # Create the two rows per match
#   rowwise() %>%
#   do({
#     # Split the ISO Code column into two country codes
#     iso_codes <- str_split(.$`ISO Code`, " ")[[1]]
#     # Split the Match column into two country names
#     countries <- str_split(.$Match, " vs ")[[1]]
#     
#     # Create two rows
#     data.frame(
#       list_name = rep(.$matchnumber, 2),
#       round = rep(.$round, 2),
#       phase = rep(.$phase, 2),
#       name = iso_codes,
#       label = countries,
#       stringsAsFactors = FALSE
#     )
#   }) %>%
#   arrange(phase)%>%
#   ungroup()
# 
# # View the result
# head(choices)
# 
# survey <- gameschedule %>%
#   # Filter out TBD matches (optional - remove if you want to keep them)
#   filter(!grepl("TBD", `ISO Code`)) %>%
#   arrange(phase)%>%
#   # Create three rows per game
#   rowwise() %>%
#   do({
#     # Extract ISO codes for default value
#     iso_codes <- .$`ISO Code`
#     
#     # Create the three rows for each game
#     data.frame(
#       type = c("text", 
#                "calculate",
#                "hidden", 
#                "select_multiple players"),
#       name = c(paste0("score", .$matchnumber), 
#                paste0("final", .$matchnumber), 
#                .$matchnumber, 
#                paste0(.$matchnumber, "scorer")),
#       label = c(  paste0( .$phase, " : ", .$Match),
#                   "calc",
#                   NA, 
#                   "Select scorers"),
#       hint = c("Register the score in the order of the teams in the title. 
# Only numbers separated by a space example:  0 0  ;  3 1 ", 
#                NA, 
#                NA,
#                paste0("Select scorers, be careful of the number of goals per team you register. 
# For this game you can only select ${",  paste0("final", .$matchnumber), "} players.")),
#       constraint_message = c(NA,
#                              NA,
#                              NA,
#                              paste0("Review, you must select ${", paste0("final", .$matchnumber), "} players!" ) ) ,  
#       default = c(NA, 
#                   NA,
#                   iso_codes, 
#                   NA),
#       relevant = c( paste0("${round} = '",.$round,"'" ),
#                     paste0("${round} = '",.$round,"'" ),
#                     paste0("${round} = '",.$round,"'" ),
#                     paste0("${round} = '",.$round,"'" )
#       ),
#       constraint = c("regex(., '^[0-9+] [0-9+]$')",
#                      NA, 
#                      NA, 
#                      paste0("count-selected(.)=${final", .$matchnumber, "}" ) ),
#       choice_filter = c(NA, 
#                         NA,
#                         NA, 
#                         paste0("selected(${", .$matchnumber, "}, squad)")),
#       required = c("TRUE", 
#                    "TRUE",
#                    "TRUE",
#                    "TRUE"),
#       calculation = c(NA, 
#                       paste0("substr(${score", .$matchnumber, "}, 0, 2) + substr(${score", .$matchnumber, "}, 2, 4)"), 
#                       NA,
#                       NA),
#       appearance = c(NA,
#                      NA,
#                      NA,
#                      "minimal"),
#       stringsAsFactors = FALSE
#     )
#   }) %>%
#   ungroup()
# 
# # View the result
# head(survey, 10)
# 
# 
# survey <- bind_rows(other,
#                     survey)
# 
# choices <- bind_rows(players,
#                      choices)
# 
# questionnaire <- list(survey = survey,
#                       choices = choices)
# 
# write_xlsx(questionnaire, "C:/Users/francis.fayolle/Downloads/surveyWCgroup.xlsx")

######  Calculation of results ##########

# 1. Download kobo results
# 2. Perform join: match per game code + case_when 
# 3. do an html output: summary of points + summary of predictions\

rm(other)