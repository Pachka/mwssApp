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
                       sliderInput("pSL", label = 'Probability that HCWS developping middle symptoms takes sick leave', min = 0, max = 100,
                                   value = params_dataset$pSL*100, post  = " %"),
                       conditionalPanel(
                         condition = "input.pSL > 0",
                         numericInput('gammaBSL', 'In average, how many days before sick leave when mild symptoms?', value = params_dataset$gammaBSL, min = 0, step = 0.5)),
                       sliderInput("pESL", label = 'Probability that HCWS developping severe symptoms takes extended sick leave', min = 0, max = 100,
                                   value = params_dataset$pESL*100, post  = " %"),
                       conditionalPanel(
                         condition = "input.pSL > 0",
                       sliderInput("gammaSL", label = 'On average, how many days do sick leave and extended sick leave last?', min = 0, max = 90,
                                   value = c(params_dataset$gammaSL, params_dataset$gammaESL), post = " days"))),
                         column(width = 6,
                                h4("Patients"),
                                sliderInput("pT", label = 'When developing severe symptoms, what is the probability of transfer in another facility (eg. intensive care)?', min = 0, max = 100,
                                            value = params_dataset$pT*100, post  = " %"),
                                conditionalPanel(
                                  condition = "input.pT > 0",
                                numericInput('gammaT', 'Average number of days before transfer', value = params_dataset$gammaT, min = 0, step = 0.5)),
                                helper(checkboxInput("comorbidities", "Do your patients have comorbidities?", value = FALSE, width = NULL),
                                       icon = "question-circle",
                                       colour = "red",
                                       type = "markdown",
                                       content = "HelpBoxComorbidities"),
                                conditionalPanel(
                                  condition = "input.comorbidities == 1",

                                  helper(sliderInput("p2p", label = paste('Probability of severity if symptoms') , min = 0, max = 100,
                                                     value = params_dataset$p2*100, post  = " %"),
                                         icon = "exclamation-triangle",
                                         colour = "orange",
                                         type = "inline",
                                         content = paste("For the HCWS, this probability is",params_dataset$p2 * 100,"%.")
                                         ),
                                sliderInput("pDIC", label = 'Probability of dying in intensive care', min = 0, max = 100,
                                            value = params_dataset$pDIC*100, post  = " %"))
                         )
                ),
            box(
              title = "Characterize person-to-person contacts within your facility",
              solidHeader = T,
              column(width = 6,
                     h4("Health care workers"),
                     sliderInput("nCPP", label = 'How many patients on average each healthecare worker comes in contact with on a daily basis?', min = 0, max = 15,
                                 value = 0),
                     numericInput("dCPP", label = 'Average duration of a contact with a patient (mintutes)', min = 0,
                                 value = 0),
                     radioButtons("iCPP", "How would you characterize the level of distancing?",
                                        choiceNames =
                                          list("low", "regular","high"),
                                        choiceValues =
                                          list("1", "2", "3"),
                                        inline = TRUE
                     ),
                     hr(),
                     sliderInput("nCPH", label = 'How many other healthecare worker on average each healthecare worker comes in contact with on a daily basis?', min = 0, max = 15,
                                 value = 0),
                     numericInput("dCPH", label = 'Average duration of a contact with an healthcare worker (mintutes)', min = 0,
                                 value = 0),
                     radioButtons("iCPH", "How would you characterize the level of distancing?",
                                  choiceNames =
                                    list("low", "regular","high"),
                                  choiceValues =
                                    list("1", "2", "3"),
                                  inline = TRUE
                     )),
              column(width = 6,
                     h4("Patients"),
                     sliderInput("nCHP", label = 'How many other patients on average each patient comes in contact with on a daily basis?', min = 0, max = 15,
                                 value = 0),
                     numericInput("dCHP", label = 'Average duration of a contact with another patient (mintutes)', min = 0,
                                 value = 0),
                     radioButtons("iCHP", "How would you characterize the level of distancing?",
                                  choiceNames =
                                    list("low", "regular","high"),
                                  choiceValues =
                                    list("1", "2", "3"),
                                  inline = TRUE
                     ),
                     hr(),
                     sliderInput("nCHH", label = 'How many healthcare workers on average each patient comes in contact with on a daily basis?', min = 0, max = 15,
                                 value = 0),
                     numericInput("dCHH", label = 'Average duration of a contact with an healthcare worker (mintutes)', min = 0,
                                 value = 0),
                     radioButtons("iCHH", "How would you characterize the level of distancing?",
                                  choiceNames =
                                    list("low", "regular","high"),
                                  choiceValues =
                                    list("1", "2", "3"),
                                  inline = TRUE
                     )
                     )
            )
          )
  )
}
