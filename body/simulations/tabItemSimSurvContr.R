###################
###################
### Simulations ###
###################
###################

tabItemSimSurvContr <- function(){
  tabItem("CS",
          fluidRow(
            box(
              title = "Control and surveillance options",
              solidHeader = T,
              checkboxInput("isoWard", "Specific isolated ward dedicated to covid patients", value = FALSE, width = NULL),
              conditionalPanel(
                condition = "input.iso_ward == 1",
                numericInput(
                  'gammapreIso',
                  'duration between first symptoms and transfer in a specific ward designated to isolate symptomatic patients',
                  value = params_dataset$gammapreIso, min = 0.5, step = 0.5
                ),
                numericInput(
                  'gammaIso',
                  'duration of stay in a specific ward designated to isolate symptomatic patients',
                  value = params_dataset$gammaIso, min = 0.5, step = 0.5
                )),
              checkboxInput("airlock", "Contact restriction at the admission", value = FALSE, width = NULL),
              conditionalPanel(
                condition = "input.airlock == 1",
                selectizeInput("AL_test", "Is the clinical examination systematically followed by a test?",
                               choices = c("No, only symptomatic by antigenic",
                                           "No, only symptomatic by PCR",
                                           "Yes, by antigenic",
                                           "Yes, by PCR",
                                           "Yes, in each ward, a pooled PCR-test is performed daily with samples from all new patients. In case of positive results, each admission is individually tested.",
                                           "Yes, a pooled PCR-test is performed daily at the hospital scale with samples from all new patients. In case of positive results, each admission is individually tested.")

                ),
                numericInput(
                  'nHCWS_AL',
                  'Number of healthcare workers in contact with patients before test results',
                  value = 1,
                  min = 1,
                  step = 1
                ),
                numericInput(
                  'timeExCli',
                  'Duration of stay in the admission airlock for clinical examination in hours',
                  value = params_dataset$timeExCli, min = 0, step = 0.5
                ),
                numericInput(
                  'timeTest',
                  'Duration of stay in the admission  airlock for PCR testing in hours',
                  value = params_dataset$timeTest, min = 0, step = 0.5
                ),
                numericInput(
                  'betaPP_AL',
                  'From patient to patient in the airlock',
                  value = params_dataset$betaPP_AL, max = 1, min = 0, step = 0.01
                ),
                checkboxInput("allBetaAL", "Define all contact rates", value = FALSE, width = NULL),
                conditionalPanel(condition = 'input.allBetaAL == 1',
                                 numericInput(
                                   'betaPH_AL',
                                   'From patient to healthcare worker in the airlock',
                                   value = params_dataset$betaPH_AL, max = 1, min = 0, step = 0.01
                                 ),
                                 numericInput(
                                   'betaHH_AL',
                                   'From healthcare worker to healthcare worker in the airlock',
                                   value = params_dataset$betaHH_AL, max = 1, min = 0, step = 0.01
                                 ),
                                 numericInput(
                                   'betaHP_AL',
                                   'From healthcare worker to patient in the airlock',
                                   value = params_dataset$betaHP_AL, max = 1, min = 0, step = 0.01
                                 ))
              ),
              checkboxInput("test_visit", "Antigenic test of visitors", value = FALSE, width = NULL),
              checkboxInput("regHt", "Healthcare workers are tested at regulare time intervals", value = FALSE, width = NULL),
              conditionalPanel(
                condition = "input.regHt == 1",numericInput(
                  'regHtint',
                  'Interval between two tests (days)',
                  value = 1,
                  min = 1,
                  step = 1
                )),
              checkboxInput("regPt", "Patients are tested at regulare time intervals", value = FALSE, width = NULL),
              conditionalPanel(
                condition = "input.regPt == 1",numericInput(
                  'regPtint',
                  'Interval between two tests (days)',
                  value = 1,
                  min = 1,
                  step = 1
                )),
              selectizeInput("ctr", "In case of detection of one positive patient or HCWS",
                             choices = c("Nothing specific",
                                         "Test of all patients and HCWS of the ward",
                                         "Test of all patients and HCWS of the hospital"))
            )))
  }
