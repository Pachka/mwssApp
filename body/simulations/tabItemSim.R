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
              h3("Simulation panel"),
              HTML(
                "In this panel, you can run various surveillance and control scenarios to assess their impact on the disease spread."
              ),
              h3("Surveillance and control"),
              HTML(
                "At this stage of development, the following measures have been implemented:
                  <ol>
                    <li>contact restriction for patients with positive test (ISO compartment at the subpopulation level). You can adjust the average duration of this restriction.</li>
                    <li>regular screening in patient and/or professionals populations. You can adjust the frequency of test-screening events, as well as the targeted population and subpopulation (immunity-based).</li>
                    <li>systematic screening of patients at the admission. This measure implies the absence of contact with most of the professionals and all patients before test result. You can adjust the various parameters such as the type of test used, the level of infection control in patient/professional interactions, etc.</li>
                  </ol>"
              ),
              h3("Simulation parameters"),
              HTML(
                "You can define the duration of the simulation (days) and the number of simulations before running the model.
                The number of simulations should be defined as a tradeoff between running time and standard deviation of the output.
                During a run, any click is prevented by the app.
                Once you clicked the \"Run\" button, be patient, simulations can take time.
                Do not close the window."
              ),
              h3("Simulation output"),
              HTML(
                "Various output are proposed.
                You can download a synthetic report and raw data ready to be imported and explore in R.
                All figures are editable and downloadable either in png of pdf.
                "
              ),
              br(),
              br(),
              img(
                src = 'compartmentalModel.png',
                title = "Multilevel compartmental model",
                width = "70%"
              )
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
                    conditionalPanel(
                      condition = "$(\'html\').hasClass(\'shiny-busy\')",
                      # tags$div(class = "loader"),
                      tags$div(class = "prevent_click")
                    ),
                    actionButton(
                      "runmodel",
                      "Run",
                      # span("Run", id = "UpdateAnimate", class = "loading dots"),
                      icon = icon("play"),
                      style = "color: #fff; background-color: #063567; border-color: #2e6da4"
                    ),
                    div(
                      style = "display: inline-block;vertical-align:top;",
                      conditionalPanel(
                        "output.simoutput == true",
                        synthreportUI("report_exp"),
                        exporttrajUI("export_traj")
                      )
                    )
                  ),
                  # display load spinner when shiny is busy
                  conditionalPanel(
                    condition = "$(\'html\').hasClass(\'shiny-busy\')",
                    tags$div("Simulation in progress. This may take a while...",
                             id = "loadmessage")
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
