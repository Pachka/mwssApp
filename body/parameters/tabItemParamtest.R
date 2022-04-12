##################################
##################################
### Epidemiological parameters ###
##################################
##################################

tabItemParamtest <- function(){

  tabItem("test",
          fluidPage(
            box(
              width = 9,
              title = "Test effectiveness",
              solidHeader = TRUE,
              column(width = 4,
                     radioButtons("testPW",
                                  "What kind of test are you using to test patients?",
                                  choiceNames =
                                    list("Ag-RDT", "RT-PCR"),
                                  choiceValues =
                                    list("Ag-RDT", "RT-PCR"),
                                  inline = TRUE
                     ),
                     radioButtons('testH',
                                  "What kind of test are you using to test professionals?",
                                  choiceNames =
                                    list("Ag-RDT", "RT-PCR"),
                                  choiceValues =
                                    list("Ag-RDT", "RT-PCR"),
                                  inline = TRUE
                     ),
                     radioButtons('testsymp',
                                  "What kind of test are you using to test symptomatic (both patients and professionals)?",
                                  choiceNames =
                                    list("Ag-RDT", "RT-PCR"),
                                  choiceValues =
                                    list("Ag-RDT", "RT-PCR"),
                                  inline = TRUE
                     )),
              br(),
              column(width = 4,
                     conditionalPanel('input.testPW == "Ag-RDT" | input.testH == "Ag-RDT" | input.testsymp == "Ag-RDT"',
                                      h5("Here you can adjust sensitivity and specificity of the Ag-RDT tests."),
                                      numericInput(
                                        'sensAg',
                                        'Sensitivity',
                                        value = 0.85, max = 1, min = 0, step = 0.01
                                      ),
                                      numericInput(
                                        'speAg',
                                        'Specificity',
                                        value = 0.95, max = 1, min = 0, step = 0.01
                                      ),
                                      timeInput(
                                        'tAg',
                                        'Duration from test to action (H:M)',
                                        seconds = FALSE,
                                        value = strptime("00:30", "%R"))
                     )),
              column(width = 4,
                     conditionalPanel('input.testPW == "RT-PCR" | input.testH == "RT-PCR" | input.testsymp == "RT-PCR"',
                                      h5("Here you can adjust sensitivity and specificity of the RT-PCR tests."),
                                      numericInput(
                                        'sensPCR',
                                        'Sensitivity',
                                        value = 0.85, max = 1, min = 0, step = 0.01
                                      ),
                                      numericInput(
                                        'sepPCR',
                                        'Specificity',
                                        value = 0.95, max = 1, min = 0, step = 0.01
                                      ),
                                      timeInput(
                                        'tPCR',
                                        'Duration from test to action (H:M)',
                                        seconds = FALSE,
                                        value = strptime("00:30", "%R"))
                     ))),
            box(width = 3,
                title = "Test of symptomatic individuals",
                solidHeader = TRUE,
                   sliderInput("ptestPWsymp",
                               label = 'Probability to test symptomatic patients',
                               min = 0,
                               max = 100,
                               value = 100),
                   conditionalPanel('input.ptestPWsymp > 0',
                                    numericInput(
                                      'tbeftestPsymp',
                                      'Average duration before test for symptomatic patients (hours)',
                                      value = 2,
                                      min = 0.5,
                                      step = 0.5)),
                   sliderInput("ptestHsymp",
                               label = 'Probability to test symptomatic professionals',
                               min = 0,
                               max = 100,
                               value = 85),
                   conditionalPanel('input.ptestHsymp > 0',
                                    numericInput(
                                      'tbeftestHsymp',
                                      'Average duration before test for symptomatic professionals (hours)',
                                      value = 24,
                                      min = 0.5,
                                      step = 0.5)
                   )
            )
          ))

}
