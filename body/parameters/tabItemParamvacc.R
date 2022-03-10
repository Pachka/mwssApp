##################################
##################################
### Epidemiological parameters ###
##################################
##################################

tabItemParamvacc <- function(){


  tabItem("vaccination",
          fluidRow(
            box(title = "Here you can define the initial immunity state of your facility",
                width = 12,
                solidHeader = T,
                column(12,
                       h3('Patients'),
                       radioButtons('immuStatP', "", choices = c("Use the national levels of immunity" = "NL",
                                                                 "Modify levels of immunity" = "ML",
                                                                 "Fix the immunity levels of patients in each ward" = "I"),
                                    character(0)),
                       conditionalPanel('input.immuStatP == "NL"',
                                        uiOutput("updated_pFI"),
                                        uiOutput("updated_pPI"),
                                        uiOutput("updated_pNI_NL")
                                        ),
                       conditionalPanel('input.immuStatP == "ML"',
                                        fluidRow(
                                          align="center",
                                          column(4,
                                                 knobInput(
                                                   inputId = "pFI",
                                                   label = "Population fully immunized",
                                                   value = 0,
                                                   min = 0,
                                                   max = 100,
                                                   post = "%",
                                                   displayPrevious = TRUE,
                                                   lineCap = "round",
                                                   fgColor = "#D9FF66",
                                                   inputColor = "#C6FF1A"
                                                 )),
                                          column(4, uiOutput("updated_slider_pPI")),
                                          column(4,uiOutput("updated_pNI_ML"))
                                        ),
                                        actionButton("updateimmuStatPML",
                                                     "Use those proportions",
                                                     icon = icon("play"))
                       ),
                       conditionalPanel('input.immuStatP == "I"',
                                        actionButton("addIMMPs", "Specify immunity state",
                                                     tags$i(
                                                       class = "fa fa-plus",
                                                       style = "color: rgb(0,166,90)"
                                                     ))
                       )),
                column(12,
                       h3('Healthcare workers')),
                radioButtons('immuStatH', "", choices = c("Use the national levels of immunity for health care workers" = "NL",
                                                          "Modify levels of immunity for health care workers" = "ML",
                                                          "Fix the immunity levels of health care workers in each ward" = "I"),
                             character(0)
                             ),
                conditionalPanel('input.immuStatH == "NL"',
                                 uiOutput("updated_hFI"),
                                 uiOutput("updated_hPI"),
                                 uiOutput("updated_hNI_NL")
                ),
                conditionalPanel('input.immuStatH == "ML"',
                                 fluidRow(
                                   align="center",
                                   column(4,
                                          knobInput(
                                            inputId = "hFI",
                                            label = "Population fully immunized",
                                            value = 0,
                                            min = 0,
                                            max = 100,
                                            post = "%",
                                            displayPrevious = TRUE,
                                            lineCap = "round",
                                            fgColor = "#D9FF66",
                                            inputColor = "#C6FF1A"
                                          )),
                                   column(4, uiOutput("updated_slider_hPI")),
                                   column(4,uiOutput("updated_hNI_ML"))
                                 ),
                                 actionButton("updateimmuStatHML",
                                              "Use those proportions",
                                              icon = icon("play"))
                ),
                conditionalPanel('input.immuStatH == "I"',
                                 actionButton("addIMMHs", "Specify immunity state",
                                              tags$i(
                                                class = "fa fa-plus",
                                                style = "color: rgb(0,166,90)"
                                              ))
                ),
            ),
            box(title = "Visualize the immunity levels of each ward and remove entries",
                width = 6,
                solidHeader = T,
                div(DT::DTOutput("IMMstateDT"), style = "font-size: 70%;"),
                br(),
                actionButton("deleteIMMstate", "Remove entry")
            ),
            box(title = "Define the initial epidemiological state of your facility",
                width = 6,
                solidHeader = T,
                actionButton("addinf", "Add specific epidemiological state",
                             tags$i(
                               class = "fa fa-plus",
                               style = "color: rgb(0,166,90)"
                             )),
                br(),
                div(DT::DTOutput("intro"), style = "font-size: 70%;"),
                br(),
                actionButton("deleteintro", "Remove entry")
            )


          )
  )
}
