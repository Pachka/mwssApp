###################
##################
### Parameters ###
##################
##################


tabItemParams <- function() {
  tabItem(
    "PARAMS",
    tabsetPanel(
      id = "tabsPARAMS",
      header = tagList(
        br(),
        div(style = "display: inline-block;vertical-align:top;",
            updateParamsUI("variant")),
        div(
          style = "display: inline-block;vertical-align:top;",
          fileInput(
            "loadparams",
            "Upload a pre-recorded set of parameters",
            accept = c("rda", ".Rda")
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top;",
          conditionalPanel(
            "output.paramsUploaded == true",
            actionButton(
              inputId = "applyParamsLoad",
              label = "Upload",
              icon = icon("upload"),
              style = "color: #fff; background-color: red; border-color: #fff; padding: 5px 5px 5px 5px; margin: 10px 5px 5px 5px; "
            )
          )
        ),
        div(style = "display: inline-block;vertical-align:top;",
            downloadParamsUI("dwloadParams")),
        hr()
      ),
      tabPanel(
        title = "How to use",
        strong("Introduction"),
        p(
          "Parameters appearing here were define using litterature review and parameters estimation statistical approaches."
        ),
        # br(),
        # strong("Consumers"),
        # p(
        #   "You earn money by creating products that consumers like. Consumers are the
        #   blocks at the top row. The number on each block tells you how many consumers are in it. A consumer block's
        #   position indicates its dream product. All consumers like products with a high technology level.
        #   But there are also differences in taste, shown by their different positions on the preference fit axis."
        # ),
        # strong("Products"),
        # p(
        #   "Filled circles represent products. Your first product (filled green circle) is already on the market. You can count the distance from a product to a consumer block. The closer a product, the more a consumer block
        #   likes it. If a new product is closer to a consumer block than any previous product, the consumer block buys the product
        #   and the product maker receives money equal to the number of consumers in the block."
        # ),
        # strong("Moves"),
        # p(
        #   "New products can be developed by improving the technology of the product
        #   that you already have, or by changing its marketing. You can also imitate your competitor's latest product.
        #   Possible moves are indicated by empty circles. The number atop indicates the cost."
        # ),
        # strong("End of the game"),
        # p(
        #   "The game ends when one producer makes a product that has the highest technology level (10). It
        #   also ends if both producers decide they don't want to do anything one after another."
        # ),
        icon = icon("question-circle")
      ),
      tabPanel(
        title = "Epidemiological parameters",
        icon = icon("sliders-h"),
        fluidRow(
          box(
            title = "Specificities in your facility",
            solidHeader = T,
            column(
              width = 6,
              br(),
              h4("Health care workers"),
              #
              sliderInput(
                "pSL",
                label = 'Probability that professionals developping mild symptoms takes sick leave',
                min = 0,
                max = 100,
                value = 30,
                post  = " %"
              ),
              sliderInput(
                "pESL",
                label = 'Probability that HCWS developping severe symptoms takes extended sick leave',
                min = 0,
                max = 100,
                value = 100,
                post  = " %"
              ),
              #
              conditionalPanel(
                condition = "input.pSL > 0",
                sliderInput(
                  "tSLs",
                  label = 'On average, how many days do sick leave and extended sick leave last?',
                  min = 0,
                  max = 90,
                  value = c(14, 28),
                  post = " days"
                )
              ),
              sliderInput(
                "pSLT",
                label = 'Probability that to take sick leave after positive test',
                min = 0,
                max = 100,
                value = 10,
                post  = " %"
              )
            ),
            column(
              width = 6,
              h4("Patients"),
              sliderInput(
                "pIC",
                label = 'When developing severe symptoms, what is the probability of transfer in another facility (eg. intensive care)?',
                min = 0,
                max = 100,
                value = 30,
                post  = " %"
              ),
              conditionalPanel(
                condition = "input.pIC > 0",
                numericInput(
                  'tIC',
                  'Average number of days outside the facility (eg. intensive care)',
                  value = 15,
                  min = 1,
                  step = 0.5
                )
              ),
              helper(
                checkboxInput(
                  "comorbidities",
                  "Do your patients have comorbidities?",
                  value = FALSE,
                  width = NULL
                ),
                icon = "question-circle",
                colour = "red",
                type = "markdown",
                content = "HelpBoxComorbidities"
              ),
              conditionalPanel(
                condition = "input.comorbidities == 1",
                helper(
                  numericInput(
                    "rsymp",
                    label = paste(
                      'Ratio adjusting probability of symptoms for patients compared to general population (professionals)'
                    ),
                    min = 0,
                    value = 1,
                    step = 0.01
                  ),
                  icon = "exclamation-triangle",
                  colour = "orange",
                  type = "inline",
                  content = paste(
                    "For the professionals, the percent probabilities are currently set at ... % for non immune, ... % for individuals with old vaccination or infection history and ... % for those with recent vaccination or infection history."
                  ) # FIX ME give an example
                ),
                helper(
                  numericInput(
                    "rsev",
                    label = paste(
                      'Ratio adjusting probability of severity if symptoms for patients compared to general population (professionals)'
                    ),
                    min = 0,
                    value = 1,
                    step = 0.01
                  ),
                  icon = "exclamation-triangle",
                  colour = "orange",
                  type = "inline",
                  content = paste(
                    "For the professionals, the percent probabilities are currently set at ... % for non immune, ... % for individuals with old vaccination or infection history and ... % for those with recent vaccination or infection history."
                  ) # FIX ME give an example
                ),
                sliderInput(
                  "pdieIC",
                  label = 'Probability of dying in intensive care',
                  min = 0,
                  max = 100,
                  value = 0.5,
                  step = 0.1,
                  post = " %"
                )
              )
            )
          ),
          box(
            title = "Characterize person-to-person contacts within your facility",
            solidHeader = T,
            column(
              width = 6,
              h4("Patients-to-Patient"),
              sliderInput(
                "n_ctcP_PW",
                label = 'How many patients on average each healthecare worker comes in contact with on a daily basis?',
                min = 0,
                max = 15,
                value = 4
              ),
              conditionalPanel(
                condition = "input.n_ctcP_PW > 0",
                timeInput(
                  "t_ctcP_PW",
                  'Average duration of those contacts (H:M)',
                  seconds = FALSE,
                  value = strptime("00:30", "%R")
                ),
                radioButtons(
                  "epsPPW",
                  "During those contacts, how would you characterize the level of infection control?",
                  choiceNames =
                    list("low", "regular", "high"),
                  choiceValues =
                    list(0.2, 0.5, 0.8),
                  inline = TRUE
                )
              ),
              hr(),
              h4("Caregiver-to-Caregiver"),
              sliderInput(
                "n_ctcH_H",
                label = 'How many healthecare worker on average each healthecare worker comes in contact with on a daily basis?',
                min = 0,
                max = 15,
                value = 5
              ),
              conditionalPanel(
                condition = "input.n_ctcH_H > 0",
                timeInput(
                  "t_ctcH_H",
                  'Average duration of those contacts (H:M)',
                  seconds = FALSE,
                  value = strptime("00:03", "%R")
                ),
                radioButtons(
                  "epsHHW",
                  "During those contacts, how would you characterize the level of infection control?",
                  choiceNames =
                    list("low", "regular", "high"),
                  choiceValues =
                    list(0.2, 0.5, 0.8),
                  inline = TRUE
                )
              )
            ),
            column(
              width = 6,
              h4("Patients-to-Caregivers"),
              sliderInput(
                "n_ctcH_PW",
                label = 'How many healthecare worker on average each patient comes in contact with on a daily basis?',
                min = 0,
                max = 15,
                value = 4
              ),
              conditionalPanel(
                condition = "input.n_ctcH_PW > 0",
                timeInput(
                  "t_ctcH_PW",
                  'Average duration of those contacts (H:M)',
                  seconds = FALSE,
                  value = strptime("00:15", "%R")
                ),
                radioButtons(
                  "epsHPW",
                  "During those contacts, how would you characterize the level of infection control for patients?",
                  choiceNames =
                    list("low", "regular", "high"),
                  choiceValues =
                    list(0.2, 0.5, 0.8),
                  inline = TRUE
                ),
                radioButtons(
                  "epsPHW",
                  "During those contacts, how would you characterize the level of infection control for professionals?",
                  choiceNames =
                    list("low", "regular", "high"),
                  choiceValues =
                    list(0.2, 0.5, 0.8),
                  inline = TRUE
                )
              ),
              hr(),
              h4("Patients-to-Visitors"),
              timeInput(
                "t_ctcV_PW",
                'Average duration of one visit (H:M)',
                seconds = FALSE,
                value = strptime("00:20", "%R")
              ),
              radioButtons(
                "epsVPW",
                "During visits, how would you characterize the level of infection control for patients?",
                choiceNames =
                  list("low", "regular", "high"),
                choiceValues =
                  list(0.2, 0.5, 0.8),
                inline = TRUE
              )
            )
          )
        )
      ),
      tabPanel(
        title = "Test-related parameters",
        icon = icon("sliders-h"),
        fluidPage(
          box(
            width = 9,
            title = "Test effectiveness",
            solidHeader = TRUE,
            column(
              width = 4,
              radioButtons(
                "testPW",
                "What kind of test are you using to test patients?",
                choiceNames =
                  list("Ag-RDT", "RT-PCR"),
                choiceValues =
                  list("Ag-RDT", "RT-PCR"),
                inline = TRUE
              ),
              radioButtons(
                'testH',
                "What kind of test are you using to test professionals?",
                choiceNames =
                  list("Ag-RDT", "RT-PCR"),
                choiceValues =
                  list("Ag-RDT", "RT-PCR"),
                inline = TRUE
              ),
              radioButtons(
                'testsymp',
                "What kind of test are you using to test symptomatic (both patients and professionals)?",
                choiceNames =
                  list("Ag-RDT", "RT-PCR"),
                choiceValues =
                  list("Ag-RDT", "RT-PCR"),
                inline = TRUE
              )
            ),
            br(),
            column(
              width = 4,
              conditionalPanel(
                'input.testPW == "Ag-RDT" | input.testH == "Ag-RDT" | input.testsymp == "Ag-RDT"',
                h5(
                  "Here you can adjust sensitivity and specificity of the Ag-RDT tests."
                ),
                numericInput(
                  'sensAg',
                  'Sensitivity',
                  value = 0.85,
                  max = 1,
                  min = 0,
                  step = 0.01
                ),
                numericInput(
                  'speAg',
                  'Specificity',
                  value = 0.95,
                  max = 1,
                  min = 0,
                  step = 0.01
                ),
                timeInput(
                  'tAg',
                  'Duration from test to action (H:M)',
                  seconds = FALSE,
                  value = strptime("00:30", "%R")
                )
              )
            ),
            column(
              width = 4,
              conditionalPanel(
                'input.testPW == "RT-PCR" | input.testH == "RT-PCR" | input.testsymp == "RT-PCR"',
                h5(
                  "Here you can adjust sensitivity and specificity of the RT-PCR tests."
                ),
                numericInput(
                  'sensPCR',
                  'Sensitivity',
                  value = 0.85,
                  max = 1,
                  min = 0,
                  step = 0.01
                ),
                numericInput(
                  'spePCR',
                  'Specificity',
                  value = 0.95,
                  max = 1,
                  min = 0,
                  step = 0.01
                ),
                timeInput(
                  'tPCR',
                  'Duration from test to action (H:M)',
                  seconds = FALSE,
                  value = strptime("00:30", "%R")
                )
              )
            )
          ),
          box(
            width = 3,
            title = "Test of symptomatic individuals",
            solidHeader = TRUE,
            sliderInput(
              "ptestPWsymp",
              label = 'Probability to test symptomatic patients',
              min = 0,
              max = 100,
              value = 100
            ),
            conditionalPanel(
              'input.ptestPWsymp > 0',
              numericInput(
                'tbeftestPsymp',
                'Average duration before test for symptomatic patients (hours)',
                value = 2,
                min = 0.5,
                step = 0.5
              )
            ),
            sliderInput(
              "ptestHsymp",
              label = 'Probability to test symptomatic professionals',
              min = 0,
              max = 100,
              value = 85
            ),
            conditionalPanel(
              'input.ptestHsymp > 0',
              numericInput(
                'tbeftestHsymp',
                'Average duration before test for symptomatic professionals (hours)',
                value = 24,
                min = 0.5,
                step = 0.5
              )
            )
          )
        )
      ),
      tabPanel(
        title = "Immunity-related parameters",
        icon = icon("sliders-h"),
        fluidRow(
          box(
            title = "Here you can define the initial immunity state of your facility",
            width = 12,
            solidHeader = T,
            conditionalPanel(
              "output.atleastoneward == true",
              column(
                12,
                setIMMstateUI(
                  "ImmstateP",
                  "Patients"
                )
              ),
              column(
                12,
                setIMMstateUI(
                  "ImmstateH",
                  "Healthcare workers"
                )
              )
            )
          ),
          conditionalPanel(
            "output.atleastoneward == true",
            box(
              title = "Visualize the immunity levels of each ward and remove entries",
              width = 12,
              solidHeader = T,
              div(style = "display: inline-block;vertical-align:top;",
                  div(DT::DTOutput("IMMstateTab"), style = "font-size: 70%;")),
              div(
                style = "display: inline-block;vertical-align:top;",
                selectInput(
                  "popimm_plot",
                  label = "Display initial immunity state for (population):",
                  choices = c(
                    "Both: patients and professionals" = "P+H",
                    "Patients" = "P",
                    "Professionals" = "H"
                  )
                ),
                plotOutput("imm_plot")),
              # div(
              #   style = "display: inline-block;vertical-align:top;"
                  # )
            )
          )
        )
      ),
      tabPanel(
        title = "Expert corner",
        icon = icon("sliders-h"),
        fluidRow(
          box(
            width = 2,
            numericInput(
              'I',
              'Daily incidence for 100,000 persons',
              # FIX ME: add help (https://www.gouvernement.fr/info-coronavirus/carte-et-donnees)
              value = 185,
              min = 0,
              max = 100000
            ),
            numericInput(
              'R0',
              'Basic reproduction number',
              # FIX ME: add help  https://www.gouvernement.fr/info-coronavirus/carte-et-donnees
              value = 1.29,
              min = 0,
              step = 0.01
            ),
            numericInput(
              'd',
              'Average disease duration',
              # FIX ME: add help (https://www.gouvernement.fr/info-coronavirus/carte-et-donnees)
              value = 10,
              min = 0,
              max = 150
            )
          ),
          box(
            width = 5,
            column(
              6,
              sliderInput(
                'pLI_NL',
                'Proportion of the population with low immunity (probability to have low immunity level at the admission)',
                value =  0.20,
                min = 0,
                max = 1,
                step = 0.01
              ),
              sliderInput(
                'rinfLI',
                'Low immunity efficiency (probability to be infected compared to non immune)',
                value =  0.70,
                min = 0,
                max = 1,
                step = 0.01
              ),
              sliderInput(
                'hNI2LI',
                'Daily probability to acquire low level of immunity when non immune',
                value =  1 / 30,
                min = 0,
                max = 1,
                step = 0.01
              ),
              numericInput(
                'tLI',
                'Average duration of low immunity (days)',
                value = 60,
                min = 1,
                step = 1
              )
            ),
            column(
              6,
              sliderInput(
                'pHI_NL',
                'Proportion of the population with high immunity (probability to have high immunity level at the admission)',
                value =  0.50,
                min = 0,
                max = 1,
                step = 0.01
              ),
              sliderInput(
                'rinfHI',
                'High immunity efficiency (probability to be infected compared to non immune)',
                value =  0.50,
                min = 0,
                max = 1,
                step = 0.01
              ),
              sliderInput(
                'hLI2HI',
                'Daily probability to acquire high level of immunity when lowly immune',
                value =  1 / 60,
                min = 0,
                max = 1,
                step = 0.01
              ),
              numericInput(
                'tHI',
                'Average duration of high immunity (days)',
                value = 150,
                min = 1,
                step = 1
              ),
              style = 'border-left: 1px solid'
            )
          ),
          box(
            width = 5,
            column(
              6,
              h4("Probability to develop symptoms"),
              sliderInput(
                'psympNI',
                'With no history of infection or vaccination',
                value = 0.5,
                min = 0,
                max = 1,
                step = 0.01
              ),
              sliderInput(
                'psympLI',
                'With old (>3 month) history of infection or vaccination',
                value = 0.2,
                min = 0,
                max = 1,
                step = 0.01
              ),
              sliderInput(
                'psympHI',
                'With recent (<= 3 month) history of infection or vaccination',
                value = 0.2,
                min = 0,
                max = 1,
                step = 0.01
              )
            ),
            column(
              6,
              h4(
                "Probability to develop severe symptoms when symptomatic (conditional probability)"
              ),
              sliderInput(
                'psevNI',
                'With no history of infection or vaccination',
                value = 0.5,
                min = 0,
                max = 1,
                step = 0.01
              ),
              sliderInput(
                'psevLI',
                'With old (>3 month) history of infection or vaccination',
                value = 0.3,
                min = 0,
                max = 1,
                step = 0.01
              ),
              sliderInput(
                'psevHI',
                'With recent (<= 3 month) history of infection or vaccination',
                value = 0.1,
                min = 0,
                max = 1,
                step = 0.01
              ),
              style = 'border-left: 1px solid'
            )
          )
        ),
        diseasetimelineUI("covid"),
      ),
      tabPanel(
        title = "References",
        p(
          "This business game was part of a study at Aalborg University on human and AI business decision making.
          Read more about the study at ",
          tags$a(
            href = "https://projekter.aau.dk/projekter/da/studentthesis/human-and-ai-decision-making-in-a-game-of-innovation-and-imitation(9121a1ed-d5d7-4cf0-b725-41f822533544).html",
            "https://projekter.aau.dk/projekter/da/studentthesis/human-and-ai-decision-making-in-a-game-of-innovation-and-imitation(9121a1ed-d5d7-4cf0-b725-41f822533544).html"
          )
        ),
        br(),
        p(
          "Source code is available at ",
          tags$a(href = "https://github.com/psimm/businessgame", "https://github.com/psimm/businessgame")
        ),
        br(),
        p("Contact: Paul Simmering (paul.simmering@gmail.com)"),
        icon = icon("book"),
      )
    )
  )
}
