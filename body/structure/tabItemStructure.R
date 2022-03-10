#################
#################
### Structure ###
#################
#################

tabItemStructure <- function(){

  tabItem(tabName = "hiddenSTR",
          fluidRow(
            box(
              title = "General structure",
              solidHeader = T,
              div(DT::DTOutput("data"), style = "font-size: 70%;"),
              addbuttonUI("addward", "Add ward/building"),
              editbuttonUI("editward", "Modify ward/building"),
              # deletebuttonUI("deleteward", "Remove ward/building")
              actionButton("deletebut", "Remove Ward",
                           tags$i(
                             class = "fa fa-minus",
                             style = "color: rgb(255,0,0)"
                           ))
            ),
            box(
              title = "Health care workers shared time",
              solidHeader = T,
              h5("Note that HCWS are affiliated to the ward where they spend the most time."),
              br(),
              div(DT::DTOutput("TS"), style = "font-size: 70%;")
            ),
            box(
              title = "Connectivity",
              solidHeader = T,
              plotOutput("network_plot")
            )
          )
  )
}
