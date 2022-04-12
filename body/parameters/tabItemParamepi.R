##################################
##################################
### Epidemiological parameters ###
##################################
##################################

tabItemParamepi <- function(){

  tabItem("epidemio",
          fluidRow(
            box(title = "Specificities in your facility",
                solidHeader = T,
                column(width = 6,
                       br(),
                       h4("Health care workers"),
                       #
                       sliderInput("pSL",
                                   label = 'Probability that professionals developping mild symptoms takes sick leave',
                                   min = 0, max = 100,
                                   value = 30,
                                   post  = " %"),
                       sliderInput("pESL", label = 'Probability that HCWS developping severe symptoms takes extended sick leave',
                                   min = 0,
                                   max = 100,
                                   value = 100, post  = " %"),
                       #
                       conditionalPanel(
                         condition = "input.pSL > 0",
                       sliderInput("tSLs",
                                   label = 'On average, how many days do sick leave and extended sick leave last?',
                                   min = 0,
                                   max = 90,
                                   value = c(14, 28), post = " days")),
                       sliderInput("pSLT", label = 'Probability that to take sick leave after positive test',
                                   min = 0,
                                   max = 100,
                                   value = 10, post  = " %")
                       ),
                         column(width = 6,
                                h4("Patients"),
                                sliderInput("pIC", label = 'When developing severe symptoms, what is the probability of transfer in another facility (eg. intensive care)?',
                                            min = 0, max = 100,
                                            value = 30, post  = " %"),
                                conditionalPanel(
                                  condition = "input.pT > 0",
                                  numericInput('tIC',
                                               'Average number of days outside the facility (eg. intensive care)',
                                               value = 15,
                                               min = 1,
                                               step = 0.5)),
                                helper(checkboxInput("comorbidities", "Do your patients have comorbidities?", value = FALSE, width = NULL),
                                       icon = "question-circle",
                                       colour = "red",
                                       type = "markdown",
                                       content = "HelpBoxComorbidities"),
                                conditionalPanel(
                                  condition = "input.comorbidities == 1",
                                  helper(
                                    numericInput("rsymp",
                                                label = paste('Ratio adjusting probability of symptoms for patients compared to general population (professionals)'),
                                                min = 0,
                                                value = 1,
                                                step = 0.01),
                                         icon = "exclamation-triangle",
                                         colour = "orange",
                                         type = "inline",
                                         content = paste("For the professionals, this probability is",params_dataset$p2 * 100,"%.") # FIX ME give an example
                                         ),
                                  helper(
                                    numericInput("rsev",
                                                 label = paste('Ratio adjusting probability of severity if symptoms for patients compared to general population (professionals)'),
                                                 min = 0,
                                                 value = 1,
                                                 step = 0.01),
                                    icon = "exclamation-triangle",
                                    colour = "orange",
                                    type = "inline",
                                    content = paste("For the professionals, this probability is",params_dataset$p2 * 100,"%.") # FIX ME give an example
                                  ),
                                sliderInput("pdieIC",
                                            label = 'Probability of dying in intensive care',
                                            min = 0,
                                            max = 100,
                                            value = 0.5,
                                            step = 0.1,
                                            post = " %"))
                         )
                ),
            box(
              title = "Characterize person-to-person contacts within your facility",
              solidHeader = T,
              column(width = 6,
                     h4("Patients-to-Patient"),
                     sliderInput("n_ctcP_PW",
                                 label = 'How many patients on average each healthecare worker comes in contact with on a daily basis?',
                                 min = 0, max = 15,
                                 value = 4),
                     conditionalPanel(
                       condition = "input.n_ctcP_PW > 0",
                       timeInput("t_ctcP_PW",
                                 'Average duration of those contacts (H:M)',
                                 seconds = FALSE,
                                 value = strptime("00:30", "%R")),
                     radioButtons("epsPPW", "During those contacts, how would you characterize the level of infection control?",
                                  choiceNames =
                                    list("low", "regular","high"),
                                  choiceValues =
                                    list(0.2, 0.5, 0.8),
                                  inline = TRUE
                     )),
                     hr(),
                     h4("Caregiver-to-Caregiver"),
                     sliderInput("n_ctcH_H",
                                 label = 'How many healthecare worker on average each healthecare worker comes in contact with on a daily basis?',
                                 min = 0, max = 15,
                                 value = 5),
                     conditionalPanel(
                       condition = "input.n_ctcH_H > 0",
                       timeInput("t_ctcH_H",
                                 'Average duration of those contacts (H:M)',
                                 seconds = FALSE,
                                 value = strptime("00:03", "%R")),
                       radioButtons("epsHHW", "During those contacts, how would you characterize the level of infection control?",
                                    choiceNames =
                                      list("low", "regular","high"),
                                    choiceValues =
                                      list(0.2, 0.5, 0.8),
                                    inline = TRUE
                       ))),
              column(width = 6,
                     h4("Patients-to-Caregivers"),
                     sliderInput("n_ctcH_PW",
                                 label = 'How many healthecare worker on average each patient comes in contact with on a daily basis?',
                                 min = 0, max = 15,
                                 value = 4),
                     conditionalPanel(
                       condition = "input.n_ctcH_PW > 0",
                       timeInput("t_ctcH_PW",
                                 'Average duration of those contacts (H:M)',
                                 seconds = FALSE,
                                 value = strptime("00:15", "%R")),
                       radioButtons("epsHPW", "During those contacts, how would you characterize the level of infection control for patients?",
                                    choiceNames =
                                      list("low", "regular","high"),
                                    choiceValues =
                                      list(0.2, 0.5, 0.8),
                                    inline = TRUE
                       ),
                       radioButtons("epsPHW", "During those contacts, how would you characterize the level of infection control for professionals?",
                                    choiceNames =
                                      list("low", "regular","high"),
                                    choiceValues =
                                      list(0.2, 0.5, 0.8),
                                    inline = TRUE
                       )),
                     hr(),
                     h4("Patients-to-Visitors"),
                     timeInput("t_ctcV_PW",
                               'Average duration of one visit (H:M)',
                               seconds = FALSE,
                               value = strptime("00:20", "%R")),
                     radioButtons("epsVPW", "During visits, how would you characterize the level of infection control for patients?",
                                  choiceNames =
                                    list("low", "regular","high"),
                                  choiceValues =
                                    list(0.2, 0.5, 0.8),
                                  inline = TRUE
                     )
                     )
            )
          )
  )
}
