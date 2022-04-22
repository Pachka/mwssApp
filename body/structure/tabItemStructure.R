#################
#################
### Structure ###
#################
#################

tabItemStructure <- function() {
  tabItem(tabName = "STR",
          tabsetPanel(
            id = "tabsSTR",
            header = tagList(
              div(style = "display: inline-block;vertical-align:top;",
                  fileInput("loadwards",
                            "", # FIX ME: Explain in How to use: Upload a pre-recorded set of wards/buildings
                            accept = c("rda", ".Rda"))),
              div(
                style = "display: inline-block;vertical-align:top;",
                conditionalPanel(
                  "output.fileUploaded == true",
                  actionButton(
                    inputId = "uploadSTR",
                    label = "Upload",
                    icon = icon("upload"),
                    style = "color: #fff; background-color: red; border-color: #fff; width:130; margin: 20px 5px 5px 5px;"
                  )
                )
              ),
              div(
                style = "display: inline-block;vertical-align:top;",
                conditionalPanel(
                  "output.atleastoneward == true",
                  # h5("Download the current structure.", style = "margin: 15px 5px 5px 5px; "),
                  downloadButton("downloadData",
                                 "Download",
                                 style = "color: #fff; background-color: #27ae60; border-color: #fff; width:130; margin: 20px 5px 5px 5px;"),
                  resetreactivesUI("resetall")
                )
              ),
              hr()
            ),
            tabPanel(title = "How to use",
                     icon = icon("question-circle")),
            tabPanel(
              title = "Structure",
              icon = icon("sliders-h"),
              box(
                title = "General structure",
                solidHeader = T,
                conditionalPanel(
                  "output.atleastoneward == true",
                  div(DT::DTOutput("facilitystr"), style = "font-size: 70%;")
                ),
                h5("Add, edit or delete a ward/building"),
                div(style = "display: inline-block;vertical-align:top;",
                    addbuttonUI("addward", "")),
                div(style = "display: inline-block;vertical-align:top;",
                    wardEditUI("editward", "")),
                div(style = "display: inline-block;vertical-align:top;",
                    wardRemoveUI("removeward", "")),
                conditionalPanel(
                  "output.atleastoneward == true",
                  hr(),
                  h4("Health care workers shared time"),
                  h5(
                    "The work time of HCWS is not constrain to 100%.
                 Some HCWS can have a part time (appearing in yellow) and other can to exceeding hours (appearing in red).
                 Neverthess 1% of work time must correspond to the same duration for all HCWS."
                  ),
                  br(),
                  div(dataTableOutput("contacts"), style = "font-size:70%"),
                  editbuttonUI("editplanning", "Modify time distribution")
                ),
              ),
              box(
                title = "Connectivity",
                solidHeader = T,
                plotOutput("network_plot")
              )
            )
          ))



}
