###################
###################
### Simulations ###
###################
###################

tabItemSimmulations <- function(){
  tabItem("SI",
          fluidRow(
            box("Control and surveillance mesures -- temporary",
                checkboxInput(inputId = "isoWard", label = "Account for the implementation of an isolated ward?", value = FALSE, width = NULL),
                checkboxInput(inputId = "airlockEffective", label = "Account for the implementation of contact restriction at the admission (airlock)?", value = FALSE, width = NULL),
                selectInput(
                  inputId = "scenario",
                  label = "Scenario",
                  choices = c("No specific control measures are implemented" = FALSE,
                              "Symptomatic patients are isolated from other patients" = "IsoPatSymp",
                              "A general airlock has been implemented" = "airlockGlobal"),
                  selected = FALSE
                ),
                selectInput(
                  inputId = "test_str",
                  label = "Test strategy",
                  choices = c("No one is tested" = "none",
                              "All new patients are tested" = "all",
                              "Symptomatic new patients are tested" = "symp",
                              "In each unit, all new patients are tested in a pool at the end of the day, if the result is positive, all new patients are tested" = "poolunit",
                              "All new patients are tested in a pool at the end of the day, if the result is positive, all new patients are tested" = "poolhosp",
                              "Dispatch daily number of tests and tests all symptomatic patients then go from most at risk to least at risk ward" = "atriskwards"
                              ),
                  selected = "None"
                ),
                checkboxInput(inputId = "Vaccination", label = "Account for vaccination?", value = FALSE, width = NULL)
                ),
            box(
              title = "Simulations parameters",
              solidHeader = T,
              width = 12,
              # Only show this panel if the plot type is a histogram
              numericInput(
                'n_sim',
                'Number of simulations',
                value = 3,
                min = 1,
                step = 1
              ),
              numericInput(
                'n_days',
                'Number of simulated days',
                value = 15,
                min = 0,
                step = 1
              ),
              checkboxInput("simP", "Do you want to use simulated prevalences and proportion of vaccinated individuals?", value = FALSE, width = NULL),
              conditionalPanel(
                condition = "input.simP == 1",
                dateInput("startSimP", "Simulate prevalences and vacc. proportions from:", value = "2022-01-01", format = "dd/mm/yy")
              ),
              actionButton("run", "Run", icon = icon("play"), color = "orange")
            )),
          fluidRow(
            valueBoxOutput("nTransmission"),
            valueBoxOutput("incidence"),
            valueBoxOutput("nTest")
          ),
          fluidRow(
            plotOutput("pOutbreak"),
            plotOutput("plotDailyTest")
            # box(
            #   title = "Probability of outbreak",
            #   solidHeader = T,
            #   plotOutput("pOutbreak")
            # )
          ),
          fluidRow(
            box(title = "Output at ward scale",
                solidHeader = T,
                h3("Tranmission events"),
                div(DT::DTOutput("transmission"), style = "font-size: 70%;"),
                h3("Incidence among patients"),
                div(DT::DTOutput("incidenceP"), style = "font-size: 70%;"),
                h3("Incidence among healthcare workers"),
                div(DT::DTOutput("incidenceH"), style = "font-size: 70%;")
            )
          )
  )
}
