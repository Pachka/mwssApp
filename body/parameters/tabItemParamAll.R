##################################
##################################
###       All parameters       ###
##################################
##################################

tabItemParamAll <- function(){

  tabItem("allparams",
          fluidRow(
            h3("The parameters appearing here were define using litterature review and parameters estimation statistical approaches."),
            box(title = "Epidemiological parameters in the general population",
                checkboxInput("simP", "Use simulated prevalences and proportion of vaccinated individuals", value = FALSE, width = NULL),
                conditionalPanel(
                  condition = "input.simP == 0",
                  numericInput(
                    'PrevCom',
                    'Prevalence',
                    value = params_dataset$PrevCom, min = 0, max = 1, step = 0.01
                  ),
                  numericInput(
                    'pRecov',
                    'Proportion of individuals who recovered in the last 2 months',
                    value = 0, min = 0, max = 100, step = 1
                  )),
                conditionalPanel(
                  condition = "input.simP == 1",
                  dateInput("startSimP", "Simulate prevalences and vacc. proportions from:", value = "2022-01-01", format = "dd/mm/yy")
                )
            ),
            box(title = "Epidemiological probabilities specific",
                solidHeader = T,
                sliderInput("p", label = 'Probability of symptoms', min = 0, max = 100,
                                   value = params_dataset$p*100, post  = " %"),
                column(width = 6,
                       br(),
                       h4("Health care workers"),
                       sliderInput("p2", label = 'Probability of severity if symptoms', min = 0, max = 100,
                                   value = params_dataset$p2*100, post  = " %"),
                       sliderInput("pSL", label = 'Probability that HCWS takes SL after mild symptoms', min = 0, max = 100,
                                   value = params_dataset$pSL*100, post  = " %"),
                       sliderInput("pESL", label = 'Probability that HCWS takes ESL after mild symptoms', min = 0, max = 100,
                                   value = params_dataset$pESL*100, post  = " %"),
                       sliderInput("pD", label = 'Probability of dying after severe symptoms for HCWS', min = 0, max = 100,
                                   value = params_dataset$pD*100, post  = " %")),
                column(width = 6,
                       br(),
                       h4("Patients"),
                       sliderInput("p2p", label = 'Probability of severity if symptoms for patients', min = 0, max = 100,
                                   value = params_dataset$p2p*100, post  = " %"),
                       sliderInput("pT", label = 'Probability of being transferred after severe symptoms', min = 0, max = 100,
                                   value = params_dataset$pT*100, post  = " %"),
                       sliderInput("pDIC", label = 'Probability of dying in intensive care after severe symptoms for patient', min = 0, max = 100,
                                   value = params_dataset$pDIC*100, post  = " %"))
            ),
            box(
              title = "Durations (day)",
              solidHeader = T,
              column(width = 6,
                     numericInput(
                       'gamma1',
                       'Non-contagious incubation period',
                       value = params_dataset$gamma1, min = 0, step = 0.5
                     ),
                     numericInput(
                       'gamma2',
                       'Contagious incubation period',
                       value = params_dataset$gamma2, min = 0, step = 0.5
                     ), numericInput(
                       'gamma3',
                       'Contagious period when asymptomatic and non severe symptoms',
                       value = params_dataset$gamma3, min = 0, step = 0.5
                     ),
                     numericInput(
                       'gamma4',
                       'Contagious period when severe symptoms',
                       value = params_dataset$gamma4, min = 0, step = 0.5
                     ),
                     numericInput(
                       'gamma5',
                       'Stay in intensive care before recovery or death',
                       value = params_dataset$gamma5, min = 0, step = 0.5
                     )),
              column(width = 6,
                     numericInput(
                       'gammaT',
                       'Contagious period before transfer when severe symptoms',
                       value = params_dataset$gammaT, min = 0, step = 0.5
                     ),
                     numericInput(
                       'gammaBSL',
                       'Period before sick leave when mild symptoms',
                       value = params_dataset$gammaBSL, min = 0, step = 0.5
                     ),
                     chooseSliderSkin("Flat"),
                     sliderInput("gammaSL", label = 'Sick leave (simple and extended)', min = 0, max = 90,
                                 value = c(params_dataset$gammaSL, params_dataset$gammaESL)),
                     h3("Epidemiological parameter"),
                     numericInput(
                       'Ta',
                       'Relative risk of transmission of asymptomatics compared to symptomatics',
                       value = params_dataset$Ta, min = 0, step = 0.5
                     ))
            ),
            box(
              title = "Effective contact rates",
              solidHeader = T,
              numericInput(
                'betaPP',
                'From patient to patient',
                value = params_dataset$betaPP, max = 1, min = 0, step = 0.01
              ),
              checkboxInput("allBetaW", "Define all contact rates", value = FALSE, width = NULL),
              conditionalPanel(condition = 'input.allBetaW == 1',
                               numericInput(
                                 'betaPH',
                                 'From patient to healthcare worker',
                                 value = params_dataset$betaPH, max = 1, min = 0, step = 0.01
                               ),
                               numericInput(
                                 'betaHH',
                                 'From healthcare worker to healthcare worker',
                                 value = params_dataset$betaHH, max = 1, min = 0, step = 0.01
                               ),
                               numericInput(
                                 'betaHP',
                                 'From healthcare worker to patient',
                                 value = params_dataset$betaHP, max = 1, min = 0, step = 0.01
                               )),
              numericInput(
                'betaVP',
                'From visitor to patient',
                value = params_dataset$betaVP, max = 1, min = 0, step = 0.01
              )),
            column(width = 6,

                                    h5("Here you can adjust sensitivity and specificity of the Ag-RDT tests."),
                                    numericInput(
                                      'sensInfAnt',
                                      'sensitivity of Ag-RDT test when testing contagious individuals',
                                      value = params_dataset$sensInfAnt, max = 1, min = 0, step = 0.01
                                    ),
                                    numericInput(
                                      'sensIncInfAnt',
                                      'sensitivity of Ag-RDT test when testing contagious individuals in incubation',
                                      value = params_dataset$sensIncInfAnt, max = 1, min = 0, step = 0.01
                                    )),
            column(width = 6,
                                    h5("Here you can adjust sensitivity and specificity of the RT-PCR tests."),
                                    numericInput(
                                      'sensInf',
                                      'sensitivity of RT-PCR test when testing contagious individuals',
                                      value = params_dataset$sensInf, max = 1, min = 0, step = 0.01
                                    ),
                                    numericInput(
                                      'sensNInf',
                                      'sensitivity of RT-PCR and Ag-RDT tests when testing non-contagious individuals',
                                      value = params_dataset$sensNInf, max = 1, min = 0, step = 0.01
                                    ),
                                    numericInput(
                                      'sensIncInf',
                                      'sensitivity of RT-PCR test when testing contagious individuals in incubation',
                                      value = params_dataset$sensIncInf, max = 1, min = 0, step = 0.01
                                    ))
          ,box(
            title = "Vaccination",
            solidHeader = T,
            numericInput(
              'PrevVacc',
              'Proportion of vaccinated individuals',
              value = params_dataset$PrevVacc, min = 0, max = 100, step = 1
            ),
            numericInput(
              'propV1',
              'among those vaccinated, propotion with one dose (or can be interpreted as two doses',
              value = params_dataset$propV1, min = 0, max = 100, step = 1
            ),
            numericInput(
              'propV2',
              'among those vaccinated, proportion with two doses  (or can be interpreted as a booster shot)',
              value = params_dataset$propV2, min = 0, max = 100, step = 1
            ),
            numericInput(
              'pv1p',
              'probability of taking first dose of vaccine for patients',
              value = params_dataset$pv1p, min = 0, max = 100, step = 1
            ),
            numericInput(
              'pv1h',
              'probability of taking first dose of vaccine for HCWs',
              value = params_dataset$pv1h, min = 0, max = 100, step = 1
            ),
            numericInput(
              'pv2p',
              'probability of taking second dose of vaccine for patients',
              value = params_dataset$pv2p, min = 0, max = 100, step = 1
            ),
            numericInput(
              'pv2h',
              'probability of taking second dose of vaccine for HCWs',
              value = params_dataset$pv2h, min = 0, max = 100, step = 1
            ),
            numericInput(
              'gammav1',
              'duration (in days)  to attain V1 immunity',
              value = params_dataset$gammav1, min = 0, max = 100, step = 1
            ),
            numericInput(
              'gammav2',
              'duration (in days)  to attain V2 immunity',
              value = params_dataset$gammav2, min = 0, max = 100, step = 1
            ),
            numericInput(
              'pInfV1',
              'probability of infection for partially vaccinated patients / HCWs',
              value = params_dataset$pInfV1, min = 0, max = 100, step = 1
            ),
            numericInput(
              'pInfV2',
              'probability of infection for fully vaccinated patients / HCWs',
              value = params_dataset$pInfV2, min = 0, max = 100, step = 1
            ),
            numericInput(
              'psv',
              'probability of developing symptoms for those vaccinated',
              value = params_dataset$psv, min = 0, max = 100, step = 1
            ),
            numericInput(
              'p2pv',
              'probability of severe symptoms for vaccinated patients',
              value = params_dataset$p2pv, min = 0, max = 100, step = 1
            ),
            numericInput(
              'p2v',
              'probability of severe symptoms for vaccinated HCWs',
              value = params_dataset$p2v, min = 0, max = 100, step = 1
            ),
            numericInput(
              'pDv',
              'probability of death for vaccinated HCWs with severe symptoms',
              value = params_dataset$pDv, min = 0, max = 100, step = 1
            ),
            numericInput(
              'pTv',
              'probability of transfer to ICU for vaccinated patients with severe symptoms',
              value = params_dataset$pTv, min = 0, max = 100, step = 1
            )
          ),
            box(
              title = "Test effectiveness",
              solidHeader = T,
              numericInput(
                'sensInf',
                'sensitivity of RT-PCR test when testing contagious individuals',
                value = params_dataset$sensInf, max = 1, min = 0, step = 0.01
              ),
              numericInput(
                'sensNInf',
                'sensitivity of RT-PCR and Ag-RDT tests when testing non-contagious individuals',
                value = params_dataset$sensNInf, max = 1, min = 0, step = 0.01
              )
            ),
          box(
            title = "visitors",
            solidHeader = T,
            numericInput(
              'gammaVin',
              'daily number of visitors',
              value = params_dataset$gammaVin, min = 0, step = 1
            ),
            numericInput(
              'gammaVout',
              'duration of stay of visitors in the ward',
              value = params_dataset$gammaVout, min = 0, step = 1
            )
          ))
  )
}

