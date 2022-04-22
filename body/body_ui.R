
body <- dashboardBody(

  tags$head(tags$style(
    HTML('.shiny-notification {position:fixed; top: 30%;left: 40%;right: 40%;}
         .box {margin: 5px;}
         .small-box {height: 125px}'    )
  )),
  tags$style(HTML("

                    .box.box-solid.box-primary>.box-header {
                    color:#fff;
                    background:#f1d031
                    }

                    .box.box-solid.box-primary{
                    border-bottom-color:#222d32;
                    border-left-color:#222d32;
                    border-right-color:#222d32;
                    border-top-color:#222d32;
                    background:#f1d031
                    }

                    ")),
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),

  setSliderColor(c("#FF3300 ", "#CC0000","#FF3300 ", "#CC0000", "#CC0000", "black"), c(1:6)),
  useShinyjs(),
  tabItems(
    tabItemPresentation(),
    tabItemStructure(),
    tabItemParams(),
    tabItemSim(),
    tabItemAbout()
  )
)
