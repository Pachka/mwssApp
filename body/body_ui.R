
body <- dashboardBody(


  tags$head(tags$style(
    HTML('.shiny-notification {position:fixed; top: 30%;left: 40%;right: 40%;}
         .box {margin: 5px;}'    )
  )),

  setSliderColor(c("#FF3300 ", "#CC0000","#FF3300 ", "#CC0000", "#CC0000", "black"), c(1:6)),
  useShinyjs(),
  tabItems(
    tabItemPresentation(),
    tabItemStructure(),
    tabItemParamDesc(),
    tabItemParamepi(),
    tabItemParamtest(),
    tabItemParamvacc(),
    tabItemParamAll(),
    tabItemSimuDescription(),
    # tabItemSimSurvContr(),
    tabItemSimmulations(),
    tabItemAbout()
  )
)
