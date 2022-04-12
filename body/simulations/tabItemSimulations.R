###################
###################
### Simulations ###
###################
###################

tabItemSimmulations <- function(){
  tabItem("SI",
          fluidRow(
            box("Control and surveillance mesures",
                solidHeader = T,
                width = 12,
                checkboxInput(inputId = "ISO",
                              label = "Adjust the duration of confinement/contact restriction imposed on detected patients?",
                              value = FALSE,
                              width = NULL),
                conditionalPanel(
                  condition = "input.ISO == 1",
                  numericInput(
                    'tISO',
                    'Average duration of confinement (days)',
                    value = 7,
                    min = 0.5,
                    step = 0.5
                  )),
                checkboxInput(inputId = "SA",
                              label = "Implement a screening area at patient admission including contact restriction/clinical examination/test?",
                              value = FALSE,
                              width = NULL),
                conditionalPanel(
                  condition = "input.SA == 1",
                  timeInput("tSA",
                            'Average duration of the whole screening process (H:M)',
                            seconds = FALSE,
                            value = strptime("02:00", "%R")),
                  sliderInput("nH_SA",
                              label = 'Limit the number of healthcare professionals allocated to the screening area?',
                              min = 1,
                              max = 15, # FIX ME: set as the smallest number of HCWS among wards min(data$str$H).
                              value = 1),
                  sliderInput("n_ctcH_PSA",
                              label = 'How many healthcare professionals on average each patient comes in contact with during the screening?',
                              min = 1,
                              max = 15,
                              value = 2),
                  timeInput("t_ctcH_PSA",
                            'Average duration of a contact with an healthcare professional (H:M)',
                            seconds = FALSE,
                            value = strptime("00:10", "%R")),
                  radioButtons("epsHPSA", "During those contacts, how would you characterize the level of infection control for patients?",
                               choiceNames =
                                 list("low", "regular","high"),
                               choiceValues =
                                 list(0.2, 0.5, 0.8),
                               inline = TRUE
                  ),
                  radioButtons("epsPHSA", "During those contacts, how would you characterize the level of infection control for healthcare professionals?",
                               choiceNames =
                                 list("low", "regular","high"),
                               choiceValues =
                                 list(0.2, 0.5, 0.8),
                               inline = TRUE
                  ),
                  sliderInput("n_ctcP_PSA",
                              label = 'How many other patients on average each patient comes in contact with during the screening?',
                              min = 0,
                              max = 15,
                              value = 0),
                  conditionalPanel(
                    condition = "input.n_ctcP_PSA > 0",
                    timeInput("t_ctcP_PSA",
                              'Average duration of a contact with another patient (H:M)',
                              seconds = FALSE,
                              value = strptime("00:05", "%R")),
                    radioButtons("epsPPSA", "How would you characterize the level of infection control for patients in contact with other patients?",
                                 choiceNames =
                                   list("low", "regular","high"),
                                 choiceValues =
                                   list(0.2, 0.5, 0.8),
                                 inline = TRUE
                    )),
                  h4('Probability to test:'),
                  splitLayout(
                    sliderInput(
                      inputId = 'ptestPSAsymp',
                      label = 'Symptomatic patients?',
                      min = 0,
                      max = 1,
                      value = 1,
                      step = 0.01
                    ),
                    sliderInput(
                      inputId = 'ptestPSANI',
                      label = 'Patients with no history of vaccination or infection?',
                      min = 0,
                      max = 1,
                      value = 0.75,
                      step = 0.1
                    ),
                    sliderInput(
                      inputId = 'ptestPSALI',
                      label = 'Patients with ancient history of vaccination or infection?',
                      min = 0,
                      max = 1,
                      value = 0.50,
                      step = 0.01
                    ),
                    sliderInput(
                      inputId = 'ptestPSAHI',
                      label = 'Patients with recent history of vaccination or infection?',
                      min = 0,
                      max = 1,
                      value = 0.25,
                      step = 0.01
                    )),
                  timeInput("ttestSA",
                            'Average duration of test used during the screening (H:M)',
                            seconds = FALSE,
                            value = strptime("02:00", "%R")),
                  radioButtons('testSA',
                               "What kind of test are you using to test symptomatic (both patients and professionals)?",
                               choiceNames =
                                 list("Ag-RDT", "RT-PCR"),
                               choiceValues =
                                 list("Ag-RDT", "RT-PCR"),
                               inline = TRUE
                  )
                ),
                h4("Implement random tests at regular intervals"),
                checkboxInput(inputId = "screenstrP",
                              label = "For patients",
                              value = FALSE,
                              width = NULL),
                checkboxInput(inputId = "screenstrH",
                              label = "For professionals",
                              value = FALSE,
                              width = NULL),
                column(6,
                conditionalPanel(
                  condition = "input.screenstrP == 1",
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
                    sliderInput("ptestPWNI",
                                label = 'Patients with no history of infection or vaccination',
                                min = 0,
                                max = 1,
                                value = .75,
                                step = 0.01),
                    sliderInput("ptestPWLI",
                                label = 'Patients with old (>3 month) history of infection or vaccination',
                                min = 0,
                                max = 1,
                                value = .5,
                                step = 0.01),
                    sliderInput("ptestPWHI",
                                label = 'Patients with recent (<= 3 month) history of infection or vaccination',
                                min = 0,
                                max = 1,
                                value = 0.1,
                                step = 0.01)
                    )
                )),
                column(6,
                conditionalPanel(
                  condition = "input.screenstrH == 1",
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
                    sliderInput("ptestHNI",
                                label = 'Professionals with no history of infection or vaccination',
                                min = 0,
                                max = 1,
                                value = .75,
                                step = 0.01),
                    sliderInput("ptestHLI",
                                label = 'Professionals with old (>3 month) history of infection or vaccination',
                                min = 0,
                                max = 1,
                                value = .5,
                                step = 0.01),
                    sliderInput("ptestHHI",
                                label = 'Professionals with recent (<= 3 month) history of infection or vaccination',
                                min = 0,
                                max = 1,
                                value = .2,
                                step = 0.01)
                  )
                ),
                style = 'border-left: 1px solid')
            ),
            box(
              title = "Simulations parameters",
              solidHeader = T,
              width = 12,
              status = "primary",
              # Only show this panel if the plot type is a histogram
              numericInput(
                'n_sim',
                'Number of simulations',
                value = 10,
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
              checkboxInput("simP",
                            "Do you want to use simulated prevalences and proportion of vaccinated individuals?",
                            value = FALSE,
                            width = NULL),
              conditionalPanel(
                condition = "input.simP == 1",
                dateInput("startSimP", "Simulate prevalences and vacc. proportions from:", value = "2022-01-01", format = "dd/mm/yy")
              ),
              actionButton("run", "Run", icon = icon("play"), style="color: #fff; background-color: #063567; border-color: #2e6da4"),
              downloadButton("report", "Generate report")
            )),
          splitLayout(
            valueBoxOutput("nosoH", width=NULL),
            valueBoxOutput("nosoP", width=NULL),
            valueBoxOutput("nSev", width=NULL)
          ),
          splitLayout(
            valueBoxOutput("ntestP", width=NULL),
            valueBoxOutput("ntestH", width=NULL),
            valueBoxOutput("ISO", width=NULL),
            valueBoxOutput("SL", width=NULL)
          ),
          fluidRow(
            column(2,
            numericInput(
              'outb_Thhold',
              'Probability to have at least n nosocomial infection:',
              value = 1,
              min = 0,
              step = 1
            )),
            column(5,
            plotOutput("pOutbreak")),
            column(5,
            plotOutput("nosoHazard")),
            br(),
            column(4,
            selectInput(
              inputId = "scaleInc",
              label = "Display incidence for (scale):",
              choices = c("The whole facility" = 0,
                          "Each ward" = 1),
              selected = FALSE
            ),
            conditionalPanel(
              condition = "input.scaleInc == 1",
              checkboxInput(inputId = "wardInc",
                            label = "Display incidence for a specific ward",
                            value = FALSE,
                            width = NULL),
              conditionalPanel(
                condition = "input.wardInc == 1",
                uiOutput("ward_choiceInc")
              )),
            selectInput(
              inputId = "popInc",
              label = "Display incidence for (population):",
              choices = c("Both: patients and professionals" = "P+H",
                          "Patients" = "P",
                          "Professionals" = "H"),
              selected = FALSE
            ),
            checkboxInput(inputId = "iterInc",
                          label = "Display incidence for only one simulation",
                          value = FALSE,
                          width = NULL),
            conditionalPanel(
              condition = "input.iterInc == 1",
              uiOutput("iter_choiceInc")
            ),
            checkboxInput(inputId = "display_sdInc",
                          label = "Display standard deviation",
                          value = FALSE,
                          width = NULL)),
            column(8,
            plotOutput("plotIncidence")),
            br(),
            column(4,
            selectInput(
              inputId = "scaleTest",
              label = "Display number of tests for (scale):",
              choices = c("The whole facility" = 0,
                          "Each ward" = 1),
              selected = FALSE
            ),
            conditionalPanel(
              condition = "input.scaleTest == 1",
              checkboxInput(inputId = "wardTest",
                            label = "Display number of tests for a specific ward",
                            value = FALSE,
                            width = NULL),
              conditionalPanel(
                condition = "input.wardTest == 1",
                uiOutput("ward_choiceTest")
              )),
            selectInput(
              inputId = "popTest",
              label = "Display number of tests for (population):",
              choices = c("Both: patients and professionals" = "P+H",
                          "Patients" = "P",
                          "Professionals" = "H"),
              selected = FALSE
            ),
            checkboxInput(inputId = "iterTest",
                          label = "Display incidence for only one simulation",
                          value = FALSE,
                          width = NULL),
            conditionalPanel(
              condition = "input.iterTest == 1",
              uiOutput("iter_choiceTest")
            ),
            checkboxInput(inputId = "agrtest",
                          label = "Median over aggregated period",
                          value = FALSE,
                          width = NULL),
            conditionalPanel(
              condition = "input.agrtest == 1",
              uiOutput("daysint_choiceTest")
            )),
            column(8,
            plotOutput("plottest"))
          )
  )
}
