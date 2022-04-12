##################################
##################################
###       All parameters       ###
##################################
##################################

tabItemParamAll <- function() {
  tabItem("allparams",
          fluidRow(
            h3(
              "The parameters appearing here were define using litterature review and parameters estimation statistical approaches."
            ),
            tabsetPanel(
              id = "tabs",
              tabPanel(
                title = "How to play",
                strong("Introduction"),
                p(
                  "You are the CEO of a company that researches and sells consumer technology goods.
          A new product has been invented. You, along with one competitor, are the first to market."
                ),
                img(src = 'howtoplay.png', id = "img_howtoplay"),
                br(),
                br(),
                strong("Consumers"),
                p(
                  "You earn money by creating products that consumers like. Consumers are the
          blocks at the top row. The number on each block tells you how many consumers are in it. A consumer block's
          position indicates its dream product. All consumers like products with a high technology level.
          But there are also differences in taste, shown by their different positions on the preference fit axis."
                ),
                strong("Products"),
                p(
                  "Filled circles represent products. Your first product (filled green circle) is already on the market. You can count the distance from a product to a consumer block. The closer a product, the more a consumer block
          likes it. If a new product is closer to a consumer block than any previous product, the consumer block buys the product
          and the product maker receives money equal to the number of consumers in the block."
                ),
                strong("Moves"),
                p(
                  "New products can be developed by improving the technology of the product
          that you already have, or by changing its marketing. You can also imitate your competitor's latest product.
          Possible moves are indicated by empty circles. The number atop indicates the cost."
                ),
                strong("End of the game"),
                p(
                  "The game ends when one producer makes a product that has the highest technology level (10). It
          also ends if both producers decide they don't want to do anything one after another."
                ),
                icon = icon("question-circle")
              ),
              tabPanel(title = "Settings",
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
                           )
                         ),
                         box(
                           width = 5,
                           column(
                             6,
                             sliderInput(
                               'pLI',
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
                               'pHI',
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
                             h4("Probability to develop severe symptoms when symptomatic (conditional probability)"),
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
                       fluidRow(
                         box(
                           width = 12,
                           column(
                             2,
                             numericInput(
                               'tE',
                               'Average duration of non contagious incubating period (days)',
                               value = 5,
                               min = 0,
                               step = 0.5
                             ),
                             numericInput(
                               'tEA',
                               'Average duration of contagious incubating period for futur asymptomatic (days)',
                               value = 2,
                               min = 0,
                               step = 0.5
                             ),
                             numericInput(
                               'tIA',
                               'Average duration of infectious period when asymptomatic (days)',
                               value = 7,
                               min = 0,
                               step = 0.5
                             )
                           ),
                           column(
                             2,
                             numericInput(
                               'tES',
                               'Average duration of contagious incubating period for futur symptomatic (days)',
                               value = 2,
                               min = 0,
                               step = 0.5
                             ),
                             numericInput(
                               'tIM',
                               'Average duration of infectious period with mild symptoms (days)',
                               value = 8,
                               min = 0,
                               step = 0.5
                             ),
                             numericInput(
                               'tIS',
                               'Average duration of infectious period with severe symptoms (days)',
                               value = 9,
                               min = 0,
                               step = 0.5
                             )
                           ),
                           column(
                             8,
                           plotOutput("disease_states")
                           )
                         )
                       )),
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
          ))
}
