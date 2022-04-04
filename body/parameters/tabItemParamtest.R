##################################
##################################
### Epidemiological parameters ###
##################################
##################################

tabItemParamtest <- function(){

  tabItem("test",
          fluidPage(
            box(
              width = 12,
              title = "Test effectiveness",
              solidHeader = TRUE,
              checkboxGroupInput('testuse', "What kind of test are you using?", choices = c("Ag-RDT", "RT-PCR")),
              em("Multiple choices are possible."),
              br(),
              column(width = 6,
                     conditionalPanel('input.testuse[0] == "Ag-RDT" ',
                                      h5("Here you can adjust sensitivity and specificity of the Ag-RDT tests."),
                                      numericInput(
                                        'sensAg',
                                        'Sensitivity',
                                        value = params_dataset$sensInfAnt, max = 1, min = 0, step = 0.01
                                      ),
                                      numericInput(
                                        'speAg',
                                        'Specificity',
                                        value = params_dataset$sensIncInfAnt, max = 1, min = 0, step = 0.01
                                      )
                     )),
              column(width = 6,
                     conditionalPanel('input.testuse[0] == "RT-PCR" || input.testuse[1] == "RT-PCR" ',
                                      h5("Here you can adjust sensitivity and specificity of the RT-PCR tests."),
                                      numericInput(
                                        'sensPCR',
                                        'Sensitivity',
                                        value = params_dataset$sensInf, max = 1, min = 0, step = 0.01
                                      ),
                                      numericInput(
                                        'sepPCR',
                                        'Specificity',
                                        value = params_dataset$sensNInf, max = 1, min = 0, step = 0.01
                                      )
                     ))
            )))

}
