###############
### sidebar ###
###############


sidebar <- dashboardSidebar(

  sidebarMenu(
    id = "sidebarMenu",

    menuItem("Presentation", icon = icon("book"), tabName = "PRS"),

    #################
    #################
    ### Structure ###
    #################
    #################

    menuItem("Structure", icon = icon("users"), id = "STRID", tabName = "STR", expandedName = "STR",
             fileInput("loadwards", "Upload a pre-recorded set of wards/buildings",
                       accept = c("rda", ".Rda")),
             actionButton(inputId = "apply", label = "Load new dataset", icon = icon("play"),
                          style = "color: #fff; background-color: red; border-color: #fff;width:130;padding: 5px 5px 5px 5px;margin: -20px 5px 5px 15px; "),
             h5("Download the current structure.", style = "margin: 15px 5px 5px 5px; "),
             downloadButton("downloadData", "Download",
                            style = "color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 5px 5px 5px;margin: 5px 5px 5px 15px; "),
    hidden(menuSubItem("hiddenSTR", tabName = "hiddenSTR"))
    ),

    ##################################
    ##################################
    ### Epidemiological parameters ###
    ##################################
    ##################################


    menuItem("Parameters", icon = icon("fas fa-sliders-h"), id = "EPID", tabName = "EPI", expandedName = "EPI",
             selectizeInput("variant_id", "SARS-CoV-2 variants",
                            choices = c("delta", "omicron"),
                            selected = "omicron"
                            # options = list(
                            #   placeholder = 'Choose a SARS-CoV-2 variant',
                            #   onInitialize = I('function() { this.setValue(""); }')
                            # )
             ),
             menuSubItem("Epidemiology", tabName = "epidemio"),
             menuSubItem("Test", tabName = "test"),
             menuSubItem("Immunity/Vaccination", tabName = "vaccination"),
             menuSubItem("Modeling expert corner", tabName = "allparams", icon = icon("cogs"))),
    hidden(menuItem("hiddenEPI", tabName = "hiddenEPI")),

    ###################
    ###################
    ### Simulations ###
    ###################
    ###################

    menuItem("Simulation", icon = icon("play"), tabName = "SIM", expandedName = "SIM",
             # menuSubItem("Control and surveillance options", tabName = "CS"),
             menuSubItem("Simulations", tabName = "SI")
    ),
    hidden(menuItem("hiddenSIM", tabName = "hiddenSIM")),


    menuItem("About", icon = icon("sticky-note"), tabName = "ABOUT") # icon could also be "book


  )



)
