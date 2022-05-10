###################
###################
### Simulations ###
###################
###################


tabItemSim <- function() {
  tabItem("SIM",
          tabsetPanel(
            id = "tabsSIM",
            tabPanel(
              title = "How to use",
              icon = icon("question-circle"),
              strong("Introduction")
            ),
            tabPanel(
              title = "Simulations",
              icon = icon("sliders-h"),
              fluidRow(
                box(
                  width = 12,
                  checkboxGroupInput(
                    "CSprotocols",
                    "Control and surveillance:",
                    c(
                      "Impose confinement/contact restriction to detected patients?" = "ISO",
                      "Implement random tests at regular intervals?" = "regscreen",
                      "Implement a screening area at patient admission including contact restriction/clinical examination/test?" = "SA"
                    )
                  )
                ),
                conditionalPanel(condition = "input.CSprotocols.includes('ISO')",
                                 box(
                                   numericInput(
                                     'tISO',
                                     'Average duration of confinement (days)',
                                     value = 7,
                                     min = 0.5,
                                     step = 0.5
                                   )
                                 )),
                conditionalPanel(condition = "input.CSprotocols.includes('regscreen')",
                                 box(
                                   checkboxGroupInput(
                                     "regscreenPop",
                                     "Implement random tests at regular intervals to:",
                                     c("Patients" = "screenstrP",
                                       "Professionals" = "screenstrH")
                                   ),
                                   column(
                                     6,
                                     conditionalPanel(
                                       condition = "input.regscreenPop.includes('screenstrP')",
                                       numericInput(
                                         'tbtwtestP',
                                         'Interval between two screening tests for patients (days)',
                                         value = 14,
                                         min = 1,
                                         step = 1
                                       ),
                                       box(
                                         width = 12,
                                         title = "Proportion of the population sampled to be tested:",
                                         solidHeader = TRUE,
                                         sliderInput(
                                           "ptestPWNI",
                                           label = 'Patients with no history of infection or vaccination',
                                           min = 0,
                                           max = 1,
                                           value = .75,
                                           step = 0.01
                                         ),
                                         sliderInput(
                                           "ptestPWLI",
                                           label = 'Patients with old (>3 month) history of infection or vaccination',
                                           min = 0,
                                           max = 1,
                                           value = .5,
                                           step = 0.01
                                         ),
                                         sliderInput(
                                           "ptestPWHI",
                                           label = 'Patients with recent (<= 3 month) history of infection or vaccination',
                                           min = 0,
                                           max = 1,
                                           value = 0.1,
                                           step = 0.01
                                         )
                                       )
                                     )
                                   ),
                                   column(
                                     6,
                                     conditionalPanel(
                                       condition = "input.regscreenPop.includes('screenstrH')",
                                       numericInput(
                                         'tbtwtestH',
                                         'Interval between two screening tests for professionals (days)',
                                         value = 28,
                                         min = 1,
                                         step = 1
                                       ),
                                       box(
                                         width = 12,
                                         title = "Proportion of the population sampled to be tested:",
                                         solidHeader = TRUE,
                                         sliderInput(
                                           "ptestHNI",
                                           label = 'Professionals with no history of infection or vaccination',
                                           min = 0,
                                           max = 1,
                                           value = .75,
                                           step = 0.01
                                         ),
                                         sliderInput(
                                           "ptestHLI",
                                           label = 'Professionals with old (>3 month) history of infection or vaccination',
                                           min = 0,
                                           max = 1,
                                           value = .5,
                                           step = 0.01
                                         ),
                                         sliderInput(
                                           "ptestHHI",
                                           label = 'Professionals with recent (<= 3 month) history of infection or vaccination',
                                           min = 0,
                                           max = 1,
                                           value = .2,
                                           step = 0.01
                                         )
                                       )
                                     ),
                                     style = 'border-left: 1px solid'
                                   )
                                 )),
                conditionalPanel(condition = "input.CSprotocols.includes('SA')",
                                 box(
                                   width = 12,
                                   column(
                                     6,
                                     timeInput(
                                       "tSA",
                                       'Average duration of the whole screening process (H:M)',
                                       seconds = FALSE,
                                       value = strptime("02:00", "%R")
                                     ),
                                     numericInput(
                                       "nH_SA",
                                       label = 'Limit the number of healthcare professionals allocated to the screening area?',
                                       min = 1,
                                       value = 1
                                     ),
                                     h4('Probability to test patients:'),
                                     splitLayout(
                                       sliderInput(
                                         inputId = 'ptestPSAsymp',
                                         label = HTML('Symptomatic <br/> &nbsp;'),
                                         min = 0,
                                         max = 1,
                                         value = 1,
                                         step = 0.01
                                       ),
                                       sliderInput(
                                         inputId = 'ptestPSANI',
                                         label = HTML('With no history of <br/> vaccination or infection?'),
                                         min = 0,
                                         max = 1,
                                         value = 0.75,
                                         step = 0.1
                                       ),
                                       sliderInput(
                                         inputId = 'ptestPSALI',
                                         label = HTML('With old history of <br/> vaccination or infection?'),
                                         min = 0,
                                         max = 1,
                                         value = 0.50,
                                         step = 0.01
                                       ),
                                       sliderInput(
                                         inputId = 'ptestPSAHI',
                                         label = HTML('With recent history of <br/> vaccination or infection?'),
                                         min = 0,
                                         max = 1,
                                         value = 0.25,
                                         step = 0.01
                                       )
                                     ),
                                     timeInput(
                                       "ttestSA",
                                       'Average duration of test used during the screening (H:M)',
                                       seconds = FALSE,
                                       value = strptime("02:00", "%R")
                                     ),
                                     radioButtons(
                                       'testSA',
                                       "What kind of test are you using to test asymptomatic patients?",
                                       choiceNames =
                                         list("Ag-RDT", "RT-PCR"),
                                       choiceValues =
                                         list("Ag-RDT", "RT-PCR"),
                                       inline = TRUE
                                     )
                                   ),
                                   column(
                                     6,
                                     h4("Contacts"),
                                     numericInput(
                                       "n_ctcH_PSA",
                                       label = 'How many healthcare professionals on average each patient comes in contact with during the screening?',
                                       min = 1,
                                       value = 2
                                     ),
                                     timeInput(
                                       "t_ctcH_PSA",
                                       'Average duration of a contact with an healthcare professional (H:M)',
                                       seconds = FALSE,
                                       value = strptime("00:10", "%R")
                                     ),
                                     radioButtons(
                                       "epsHPSA",
                                       "During those contacts, how would you characterize the level of infection control for patients?",
                                       choiceNames =
                                         list("low", "regular", "high"),
                                       choiceValues =
                                         list(0.2, 0.5, 0.8),
                                       inline = TRUE
                                     ),
                                     radioButtons(
                                       "epsPHSA",
                                       "During those contacts, how would you characterize the level of infection control for healthcare professionals?",
                                       choiceNames =
                                         list("low", "regular", "high"),
                                       choiceValues =
                                         list(0.2, 0.5, 0.8),
                                       inline = TRUE
                                     ),
                                     numericInput(
                                       "n_ctcP_PSA",
                                       label = 'How many other patients on average each patient comes in contact with during the screening?',
                                       min = 0,
                                       value = 0
                                     ),
                                     conditionalPanel(
                                       condition = "input.n_ctcP_PSA > 0",
                                       timeInput(
                                         "t_ctcP_PSA",
                                         'Average duration of a contact with another patient (H:M)',
                                         seconds = FALSE,
                                         value = strptime("00:05", "%R")
                                       ),
                                       radioButtons(
                                         "epsPPSA",
                                         "How would you characterize the level of infection control for patients in contact with other patients?",
                                         choiceNames =
                                           list("low", "regular", "high"),
                                         choiceValues =
                                           list(0.2, 0.5, 0.8),
                                         inline = TRUE
                                       )
                                     )
                                   )
                                 )),
                box(
                  title = "Simulations parameters",
                  solidHeader = T,
                  width = 12,
                  status = "primary",
                  # Only show this panel if the plot type is a histogram
                  numericInput(
                    'n_sim',
                    'Number of simulations',
                    value = 50,
                    min = 1,
                    step = 1
                  ),
                  numericInput(
                    'n_days',
                    'Number of simulated days',
                    value = 60,
                    min = 0,
                    step = 1
                  ),
                  # checkboxInput(
                  #   "simP",
                  #   "Do you want to use simulated prevalences and proportion of vaccinated individuals?",
                  #   value = FALSE,
                  #   width = NULL
                  # ),
                  # conditionalPanel(
                  #   condition = "input.simP == 1",
                  #   dateInput(
                  #     "startSimP",
                  #     "Simulate prevalences and vacc. proportions from:",
                  #     value = "2022-01-01",
                  #     format = "dd/mm/yy"
                  #   )
                  # ),
                  conditionalPanel(
                    "output.atleastoneward == true",
                    actionButton(
                      "runmodel",
                      "Run",
                      icon = icon("play"),
                      style = "color: #fff; background-color: #063567; border-color: #2e6da4"
                    ),
                    div(
                      style = "display: inline-block;vertical-align:top;",
                      conditionalPanel("output.simoutput == true",
                                       synthreportUI("report_exp"),
                                       exporttrajUI("export_traj"))
                    )
                  )
                )
              ),
              conditionalPanel(
                "output.simoutput == true",
                valueboxoutputUI("simulation"),
                plotsoutputUI("simulationPlots")
              )
            )
          ))

}
