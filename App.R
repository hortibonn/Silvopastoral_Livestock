#Mindrum DA model

# Holistic decision analysis model of agrosilvopastoral agroforestry system of Mindrum farm ####
# 14.9744 ha of two strip SRC operation measuring 200 x 20 m each 
# Scenario of changing silvoarable to silvopastoral is yet to be introduced, this will be first to introduce livestock elements
# Institutional support needs to reflect UK conditions
# env vs eco vs social influence can be plotted as in the banana paper or smooth graphs back-to-back using compared to the baseline:
# https://github.com/hortibonn/Plotting-High-Dimensional-Data

#### Install Libraries ####
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}
library(shiny)

if (!requireNamespace("bslib", quietly = TRUE)) {
  install.packages("bslib")
}
library(bslib)

if (!requireNamespace("shinythemes", quietly = TRUE)) {
  install.packages("shinythemes")
}
library(shinythemes)

if (!requireNamespace("shinyWidgets", quietly = TRUE)) {
  install.packages("shinyWidgets")
}
library(shinyWidgets)

if (!requireNamespace("decisionSupport", quietly = TRUE)) {
  install.packages("decisionSupport")
}
library(decisionSupport)

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
library(readr)  # For reading and writing CSV files

if (!requireNamespace("ggridges", quietly = TRUE)) {
  install.packages("ggridges")
}
library(ggridges)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)


# Define user project file count limit
max_files <- 5

# Define list of admin users
admin_users <- c("Adrian", "Prajna", "Christine")

# Pre-requisites to UI ####
# Countries and states ####
country_states <-
  list(
    "None" = c("none"),
    # "Germany" = c("Baden-Württemberg", "Bavaria", "Berlin", "Brandenburg", "Bremen",
    #               "Hamburg", "Hesse", "Lower Saxony", "Mecklenburg-Vorpommern",
    #               "North Rhine-Westphalia", "Rhineland-Palatinate", "Saarland",
    #               "Saxony", "Saxony-Anhalt", "Schleswig-Holstein", "Thuringia"),
    # "Czech Republic" = c("Prague", "Central Bohemian", "South Bohemian", "Plzeň",
    #                      "Karlovy Vary", "Ústí nad Labem", "Liberec", "Hradec Králové",
    #                      "Pardubice", "Vysočina", "South Moravian", "Olomouc",
    #                      "Zlín", "Moravian-Silesian"),
    # "Belgium" = c("Flanders", "Wallonia", "Brussels"),
    # "Bulgaria" = c("Blagoevgrad", "Burgas", "Varna"),
    # "Hungary" = c("Budapest", "Pest", "Csongrád"),
    # "Denmark" = c("North Jutland", "Central Jutland", "Southern Denmark"),
    # "Spain" = c("Andalusia", "Catalonia", "Madrid"),
    "England" = c("Any")
    # , "Berkshire", "Bristol", "Buckinghamshire",
    #               "Cambridgeshire", "Cheshire", "Cornwall", "Cumbria", "Derbyshire",
    #               "Devon", "Dorset", "Durham", "East Sussex", "Essex", "Gloucestershire",
    #               "Greater London", "Greater Manchester", "Hampshire", "Herefordshire",
    #               "Hertfordshire", "Isle of Wight", "Kent", "Lancashire",
    #               "Leicestershire", "Lincolnshire", "Merseyside", "Norfolk", "North Yorkshire",
    #               "Northamptonshire", "Northumberland", "Nottinghamshire", "Oxfordshire",
    #               "Rutland", "Shropshire", "Somerset", "South Yorkshire", "Staffordshire",
    #               "Suffolk", "Surrey", "Tyne and Wear", "Warwickshire", "West Midlands",
    #               "West Sussex", "West Yorkshire", "Wiltshire", "Worcestershire"),
    # "Scotland" = c("Edinburgh", "Glasgow", "Aberdeen"),
    # "Wales" = c("Cardiff", "Swansea", "Newport"),
    # "Northern Ireland" = c("Belfast", "Derry", "Lisburn"),
    # "France" = c("Île-de-France", "Auvergne-Rhône-Alpes", "Nouvelle-Aquitaine"),
    # "Italy" = c("Lombardy", "Lazio", "Campania")
  )

# Funding data per country and state and state ####
# For demonstration purposes, I'll add example funding schemes for a few countries and states
funding_data <- 
  list(
    "None" = list(
      "none" = list(
        one_time_funding_schemes = c(),
        funding_onetime_values_named = setNames(c(),
                                                c()),
        funding_onetime_per_plan_schemes = c(),
        funding_onetime_per_tree_schemes = c(),
        funding_onetime_percentage_schemes = c(),
        annual_funding_schemes = c(),
        funding_yearly_values_named = setNames(c(),c())
      )
    ),
    "England" = list(
      "Any" = list(
        # one-time
        one_time_funding_schemes = c(
          "Countryside Stewardship - PA4 Agroforestry [GBP/ha]",
          "Countryside Stewardship - AF Woodland trees [GBP/tree]",
          "Countryside Stewardship - AF Fruit trees [GBP/tree]",
          "Tree guard (TE6) [GBP/guard/tree]",
          "Species Diversity Bonus (if more than 5 diff. species) [GBP/tree]",
          "Woodland Trust - MOREwoods Scheme (min. 0.5 ha woodland with 500+ trees) [% of initial costs]",
          "Woodland Trust - MOREwoods Scheme with contractor (min. 1 ha woodland) [% of initial costs]",
          "Woodland Trust - Trees For Your Farm scheme (500+trees, for agroforestry schemes benefiting the business of productive farms + site visit & tree planting assessment) [% of initial costs]",
          "Woodland Trust - Trees For Your Farm scheme [% of consultation costs]"
        ),
        funding_onetime_values_named = setNames(c(1268.08, 5.40, 17.83, 3.95, 1.16, 0.75, 0.6,0.01, 0.01),
                                                c("Countryside Stewardship - PA4 Agroforestry [GBP/plan]",
                                                  "Countryside Stewardship - AF Woodland trees [GBP/tree]",
                                                  "Countryside Stewardship - AF Fruit trees [GBP/tree]",
                                                  "Tree guard (TE6) [GBP/guard/tree]",
                                                  "Species Diversity Bonus (if more than 5 diff. species) [GBP/tree]",
                                                  "Woodland Trust - MOREwoods Scheme (min. 0.5 ha woodland with 500+ trees) [% of initial costs]",
                                                  "Woodland Trust - MOREwoods Scheme with contractor (min. 1 ha woodland) [% of initial costs]",
                                                  "Woodland Trust - Trees For Your Farm scheme (500+trees, for agroforestry schemes benefiting the business of productive farms + site visit & tree planting assessment) [% of initial costs]",
                                                  "Woodland Trust - Trees For Your Farm scheme [% of consultation costs]"
                                                  
                                                ) 
        ),
        funding_onetime_per_plan_schemes = c("Countryside Stewardship - PA4 Agroforestry [GBP/plan]"),
        funding_onetime_per_tree_schemes = c("Countryside Stewardship - AF Woodland trees [GBP/tree]",
                                             "Countryside Stewardship - AF Fruit trees [GBP/tree]",
                                             "Tree guard (TE6) [GBP/guard/tree]",
                                             "Species Diversity Bonus (if more than 5 diff. species) [GBP/tree]"
        ),
        funding_onetime_percentage_schemes = c("Woodland Trust - MOREwoods Scheme (min. 0.5 ha woodland with 500+ trees) [% of initial costs]",
                                               "Woodland Trust - MOREwoods Scheme with contractor (min. 1 ha woodland) [% of initial costs]",
                                               "Woodland Trust - Trees For Your Farm scheme (500+trees, for agroforestry schemes benefiting the business of productive farms + site visit & tree planting assessment) [% of initial costs]",
                                               "Woodland Trust - Trees For Your Farm scheme [% of consultation costs]"
        ),
        #annual
        annual_funding_schemes = c("SFI Premium payment - very low density AF on less sensitive land [GBP/ha/year]",
                                   "SFI Premium payment - low density AF on less sensitive land [GBP/ha/year]",
                                   "Countryside Stewardship - very low density AF on more sensitive land [GBP/ha/year]",
                                   "Countryside Stewardship - low density AF on more sensitive land [GBP/ha/year]",
                                   "Countryside Stewardship - medium density in-field AF [GBP/ha/year]",
                                   "Countryside Stewardship - high density in-field AF [GBP/ha/year]"
        ),
        funding_yearly_values_named = setNames(c(248,385,248, 385, 595, 849),
                                               c("SFI Premium payment - very low density AF on less sensitive land [GBP/ha/year]",
                                                 "SFI Premium payment - low density AF on less sensitive land [GBP/ha/year]",
                                                 "Countryside Stewardship - very low density AF on more sensitive land [GBP/ha/year]",
                                                 "Countryside Stewardship - low density AF on more sensitive land [GBP/ha/year]",
                                                 "Countryside Stewardship - medium density in-field AF [GBP/ha/year]",
                                                 "Countryside Stewardship - high density in-field AF [GBP/ha/year]"
                                               ) 
        )
      )
      # not real subsidy schemes!!!
      # "Kent" = list(
      #   one_time_funding_schemes = c("Kent Scheme - Tree Planting [GBP/tree]",
      #                                "Kent Scheme - Land Preparation [GBP/ha]"),
      #   funding_onetime_values_named = setNames(c(10, 500),
      #                                           c("Kent Scheme - Tree Planting [GBP/tree]",
      #                                             "Kent Scheme - Land Preparation [GBP/ha]")),
      #   funding_onetime_per_ha_schemes = c("Kent Scheme - Land Preparation [GBP/ha]"),
      #   funding_onetime_per_tree_schemes = c("Kent Scheme - Tree Planting [GBP/tree]"),
      #   funding_onetime_percentage_schemes = c(),
      #   annual_funding_schemes = c("Kent Annual Support [GBP/ha/year]" ),
      #   funding_yearly_values_named = setNames(c(100),c("Kent Annual Support [GBP/ha/year]"))
      # )
    )
    # Add more countries and states with their funding schemes here - T6.3
    # "Germany" = list(
    #   "Bavaria" = list(one_time_funding_schemes = c("Bavaria Start-up Aid [EURO/ha]",
    #                                                 "Bavaria Tree Grant [EURO/tree]" ),
    #                    funding_onetime_values_named = setNames(c(800, 15),
    #                                                            c( "Bavaria Start-up Aid [EURO/ha]",
    #                                                               "Bavaria Tree Grant [EURO/tree]")),
    #                    funding_onetime_per_ha_schemes = c("Bavaria Start-up Aid [EURO/ha]"),
    #                    funding_onetime_per_tree_schemes = c("Bavaria Tree Grant [EURO/tree]"),
    #                    funding_onetime_percentage_schemes = c(),
    #                    annual_funding_schemes = c("Bavaria Annual Payment [EURO/ha/year]"),
    #                    funding_yearly_values_named = setNames(c(200),c("Bavaria Annual Payment [EURO/ha/year]") )
    #   )
    # )
    
  )

### UI ######

ui <- fluidPage(
  theme = bs_theme(version = 5,
                   bootswatch = 'flatly',
                   base_font = font_google("Roboto")), 
  
  # Head: add title for browser tab and favicon + custom CSS
  tags$head(
    tags$title("Holistic Decision Analysis Tool"),  # Browser tab title
    tags$link(rel = "shortcut icon", href = "INRES.png"),  # Optional favicon
    tags$style(HTML("
      /* Custom scrollbar styling */
      ::-webkit-scrollbar {
        width: 10px;}
      ::-webkit-scrollbar-track {
        background: #f1f1f1;}
      ::-webkit-scrollbar-thumb {
        background: skyblue;
      }
      ::-webkit-scrollbar-thumb:hover {
        background: linear-gradient(lightblue, skyblue, dodgerblue, blue, darkblue);}
      /* For Firefox */
      body {
        scrollbar-width: thin;
        scrollbar-color: skyblue #f1f1f1;}
        
      /* Header styling */
      .app-header {
        background-color:#228B22;
        color: white;
        padding: 15px 0;
        position: relative;
      }
      .app-title {
        font-size: 48px;
        font-weight: bold;
        text-align: center;
        margin: 0;
      }

      /* Accordion header styling */
      .accordion-button {
        background-color: #007BFF;
        color: white;
      }
      .accordion-button:not(.collapsed) {
        background-color: #0056b3;
      }
      .accordion-button:hover {
        background-color: #0056b3;
      }
      .accordion-button:focus {
        box-shadow: none;
      }

      .my-btn {
        display: block;
        text-align: center;
        background: #238a21; 
        color: white;
        border-radius: 20px;
        position: relative;
        margin-top: 25px;
        margin-bottom: 25px;
      }
    "))
  ),
  
  # Custom header layout (not using titlePanel)
  div(class = "app-header",
      fluidRow(
        column(width = 2, align = "right",
               tags$a(href = "https://agroreforest.eu/", target = "_blank",
                      tags$img(src = "ReFOREST_logo_horizontal_transparent.png", 
                               style = "max-width: 100%; height: auto;")
               )
        ),
        column(width = 8, align = "center",
               h2(class = "app-title",
                  "Holistic Decision Analysis for a Silvopastoral Agroforestry System")
        ),
    #tags$style(HTML("
    #/* Scroll wrapper: scrolls horizontally *and* vertically only when needed */
    #.scroll-xy {
      #overflow-x: auto;                 /* left–right scroll  */
      #overflow-y: auto;                 /* top–bottom scroll  */
      #-webkit-overflow-scrolling: touch;/* smooth on iOS      */
      #max-height: 80vh;                 /* optional: stop it taking more than
       #                                  80 % of the viewport height       */
  #}
  #/* Keep any Shiny plot inside that wrapper from shrinking */
  #.scroll-xy .shiny-plot-output {
   # min-width: 900px;                 /* choose your desktop width */
  #}
   #                 ")
    #           )
  #),
        column(width = 2, align = "left",
               tags$a(href = "https://www.gartenbauwissenschaften.uni-bonn.de/", target = "_blank",
                      tags$img(src = "UniBonnHortiBonn_logo_transparent.png", 
                               style = "max-width: 100%; height: auto;")
               )
        )
      )
  ),
  sidebarLayout(
    sidebarPanel(width = 4,
                 # style = "height: 100vh; overflow-y: auto",
                 style = "height: 100%; overflow-y: auto",
                 # Collapsible sections
                 accordion(
                   id = "collapseSidebar",
                   open = FALSE,
                   # Basic Information Panel
                   accordion_panel(
                     title = "Save / Load project",
                     icon = icon("info-circle"),
                     
                     ########### save load delete logic ##############
                     
                     # Admin user selection UI
                     uiOutput("admin_user_select"),
                     
                     textInput("project_name", "Project Name:", value = ""),
                     actionButton("save", "Save Settings"),
                     
                     # column(width = 6,
                     #        textInput("version", label = "Version", value = "1")),
                     
                     selectInput("version_select", "Select Version to Load:", choices = NULL),
                     actionButton("load",  "Load Basics"),
                     actionButton("load2", "Load Conditions"),
                     actionButton("load3", "Load Funding"),
                     
                     selectInput("delete_version_select", "Select Version to Delete:", choices = NULL),
                     actionButton("delete", "Delete Selected Version"),
                     
                     ########### save load delete logic ##############
                     
                     # dateInput("date", label = "Date input", value = Sys.Date()), # format(Sys.time(), "%Y-%m-%d_%H-%M" for naming the downloaded file
                     # 
                     # downloadButton("save_data", "Save project data"), # !! enable save local machine and our server 
                     br(),
                     br(),
                     actionButton("run_simulation", "Run Model", icon = icon("play"), class = "btn-primary"),
                     textOutput("validation_message") # To display validation messages
                   ),
                   ### Data entry by users ######
                   # Basic Farm and Decision Parameters Panel
                   accordion_panel(
                     title = "General Parameters",
                     icon = icon("tractor"),
                     # System modulators and risks
                     h4("System Modulators"),
                     numericInput("num_simulations_c", "Number of Simulations",
                                  min = 10, max = 50000, value = 100),
                     numericInput("n_years_c", "Simulation period in years",
                                  min = 1, max = 100, value = 40),
                     sliderInput("discount_rate_p", "Discount rate (%)",
                                 min = 1, max = 50, value = c(1.8,5), step = 0.01),
                     sliderInput("var_CV_p", "Coefficient of Variation",
                                 min = 1, max = 50, value = c(5,10), step = 1),
                     br(),
                     h4("Funding and Farm Details"),
                     # Dropdown menu of countries, state and funding schemes
                     selectInput("country", "Select Country:", choices = names(country_states), selected = NULL),
                     uiOutput("state_ui"),
                     uiOutput("funding_one_ui"),
                     uiOutput("funding_yearly_ui"),
                     numericInput("annual_external_support_c", "Annual private support [GBP/ha]",
                                  min = 1, max = 5000, value = 50),
                     numericInput("onetime_external_support_c", "One-time private support [GBP]",
                                  min = 1, max = 1000000, value = 500),
                     
                     numericInput("arable_area_c", "Size of the farm [ha]", min = 0.001, max = 1000, value = 14.9),
                     sliderInput("subsidy_application_p", "Time spent on application for subsidies [h]",
                                 min = 1, max = 500, value = c(10,40)),
                   ),
                   accordion_panel(
                     title = "Existing System",
                     icon = icon("tractor"),
                     # h4("Existing System"),
                     selectInput(
                       inputId = "treeless_crop_rotation", #"crop_rotation", 
                       label = "Choose a crop rotation scheme:", 
                       choices = c("Crop Rotation 1: 3 years of Herbal Ley followed by Winter wheat" = "rotation_1", 
                                   "Crop Rotation 2: 2 yeras of Herbal Ley followed by rotation with Winter wheat, Spring barley, summer beans and Winter oats" = "rotation_2"),
                       selected = NULL
                     ),
                     # Checkbox to introduce animals with numbers
                     # checkboxInput("include_animals_c", "Introduce Animals", FALSE),
                     # conditionalPanel(
                     #   condition = "input.include_animals_c == true",
                     #   numericInput("treeless_number_of_animals_c", "Number of Animals:", value = 1, min = 1)
                     # Checkbox to introduce animals with grazing %
                     checkboxInput("treeless_include_animals_c", "Introduce Animals", FALSE),
                     conditionalPanel(
                       condition = "input.treeless_include_animals_c == true",
                       selectInput("treeless_animal_type", "Select Animal",
                                   choices = c("cattle", "sheep"), #"Goats", "Chickens", "Turkeys"),
                                   selected = NULL,
                                   multiple = TRUE),
                       uiOutput("treeless_grazing_intensity"), # Dynamic UI for slider inputs (one per selected animal)
                       textOutput("treeless_grazing_warning"), # Display a warning message if the total exceeds 1
                     ),
                   ),
                   accordion_panel(
                     title = "Agroforestry Systems",
                     icon = icon("tree"),
                     h4("General preparation"),
                     sliderInput("tree_planting_p", "Cost of digging tree well & planting tree [GBP/tree]",
                                 min = 1, max = 5000, value = c(10,50)),
                     sliderInput("farmer_planning_time_p", "Time spent planning by farmer [h]",
                                 min = 0, max = 1000, value = c(10,40)),
                     sliderInput("planning_consulting_p", "Total payment of hired planner/consultant [GBP]",
                                 min = 0, max = 10000, value = c(50,1000)),
                     sliderInput("weed_protection_p", "Cost of controlling weeds during establishment [GBP]",
                                 min = 0, max = 5000, value = c(15,25)),
                     br(),
                     h4("Design 1"),
                     numericInput("AF1_tree_row_area_c", "Total area of tree rows [ha]",
                                  min = 0.001, max = 100, value = 0.8, step=0.01),
                     numericInput("AF1_num_trees_c", "Number of trees",
                                  min = 0, max = 10000, value = 5926),
                     sliderInput("AF1_plant_protection_p", "Cost of fencing [GBP/tree]",
                                 min = 0, max = 10, value = c(0.843,1.022),step = 0.0001),
                     sliderInput("SRC_cutting_price_p", "Price of willow cuttings [GBP]",
                                 min = 0, max = 10, value = c(0.1,0.6),step=0.01),
                     sliderInput("SRC_field_prep_p", "Subsoiling and harrowing cost [h/ha]",
                                 min = 1, max = 10, value = c(2.6,3.8), step=0.01),
                     sliderInput("SRC_planting_p", "Cost of planting cuttings mechanically [h/ha]",
                                 min = 1, max = 15, value = c(4,8)),
                     sliderInput("SRC_machine_rent_p", "Rent of planting machine [GBP]",
                                 min = 1, max = 500, value = c(150,300)),
                     sliderInput("harvest_interval_SRC_p", "Number of years between SRC harvest [ha]",
                                 min = 1, max = 60, value = c(3,5)),
                     sliderInput("af1_added_management_time_factor_p", "Extra time for managing AF vs. Baseline [%]",
                                 min = 1, max = 10, value = c(1.05,1.2), step=0.01),
                     sliderInput("AF1_soil_loss_water_p", "Soil loss due to water [tons/ha/year]",
                                 min = 1, max = 10, value = c(2,4)),
                     sliderInput("AF1_soil_loss_wind_p", "Soil loss due to wind [tons/ha/year]",
                                 min = 1, max = 10, value = c(2,4)),
                     sliderInput("af1_less_grazing_management_time_factor_p", "Reduced livestock mgmt from AF providing partial natural paddock [%]",
                                 min = 0, max = 1, value = c(0.8,0.9), step=0.01),
                     sliderInput("AF1_woody_benefit_windreduc_p", " Stress Reduction and Livestock Performance Enhancement [%]",
                                 min = 0, max = 1, value = c(0.01,0.02), step=0.001),
                     selectInput(
                       inputId = "AF1_crop_rotation", #"crop_rotation", 
                       label = "Choose a crop rotation scheme:", 
                       choices = c("Crop Rotation 1: 3 years of Herbal Ley followed by Winter wheat" = "rotation_1", 
                                   "Crop Rotation 2: 2 yeras of Herbal Ley followed by rotation with Winter wheat, Spring barley, summer beans and Winter oats" = "rotation_2"),
                       selected = NULL
                     ),
                     # Checkbox to introduce animals with numbers
                     # checkboxInput("AF1_include_animals_c", "Introduce Animals", FALSE),
                     # conditionalPanel(
                     #   condition = "input.AF1_include_animals_c == true",
                     #   numericInput("AF1_number_of_animals_c", "Number of Animals:", value = 1, min = 1)
                     
                     # Checkbox to introduce animals with grazing %
                     checkboxInput("AF1_include_animals_c", "Introduce Animals", FALSE),
                     conditionalPanel(
                       condition = "input.AF1_include_animals_c == true",
                       selectInput("AF1_animal_type", "Select Animal",
                                   choices = c("cattle", "sheep"), #"Goats", "Chickens", "Turkeys"),
                                   selected = NULL,
                                   multiple = TRUE),
                       uiOutput("AF1_grazing_intensity"), # Dynamic UI for slider inputs (one per selected animal)
                       textOutput("AF1_grazing_warning"), # Display a warning message if the total exceeds 1
                     ),
                     br(),
                     h4("Design 2"),
                     numericInput("AF2_tree_row_area_c", "Total area of tree rows [ha]",
                                  min = 0.001, max = 100, value = 0.45, step=0.001),
                     numericInput("num_oak_trees_c", "Number of oak trees",
                                  min = 0, max = 50, value = 30),
                     sliderInput("oak_tree_cost_p", "Price of an oak sapling [GBP]",
                                 min = 0., max = 500, value = c(1.77,3.49), step=0.001),
                     numericInput("num_birch_trees_c", "Number of birch trees",
                                  min = 0, max = 5000, value = 30),
                     sliderInput("birch_tree_cost_p", "Price of a birch sapling [GBP]",
                                 min = 0., max = 10, value = c(1.77,2.19), step=0.001),
                     numericInput("num_rowan_trees_c", "Number of rowan trees",
                                  min = 0, max = 100, value = 30),
                     sliderInput("rowan_tree_cost_p", "Price of a rowan sapling [GBP]",
                                 min = 0., max = 500, value = c(1.77,2.49),, step=0.001),
                     numericInput("num_hazel_trees_c", "Number of hazel trees",
                                  min = 0, max = 50, value = 30),
                     sliderInput("hazel_tree_cost_p", "Price of a hazel sapling [GBP]",
                                 min = 0., max = 15, value = c(1.77,2.6), step=0.1),
                     numericInput("num_damson_trees_c", "Number of damson trees",
                                  min = 0, max = 100, value = 30),
                     sliderInput("damson_tree_cost_p", "Price of a damson sapling [GBP]",
                                 min = 0., max = 50, value = c(15,21.6), step=0.1),
                     numericInput("num_bcherry_trees_c", "Number of bird cherry trees",
                                  min = 0, max = 100, value = 30),
                     sliderInput("bcherry_tree_cost_p", "Price of a bird cherry sapling [GBP]",
                                 min = 0., max = 10, value = c(1.77,1.99), step=0.001),
                     numericInput("AF2_num_shrubs_c", "Number of Shrubs",
                                  min = 0, max = 4000, value = 900),
                     sliderInput("shrub_price_p", "Price per shrub [GBP/shrub]",
                                 min = 0., max = 10, value = c(0.2,0.3), step=0.01),
                     sliderInput("AF2_plant_protection_p", "Cost of fencing [GBP/tree]",
                                 min = 1, max = 50, value = c(11.39,14.81), step=0.001),
                     sliderInput("af2_added_management_time_factor_p", "Extra time for managing AF vs. Baseline [%]",
                                 min = 1, max = 10, value = c(1.05,1.2), step=0.01),
                     sliderInput("AF2_soil_loss_water_p", "soil loss due to water [tons/ha/year]",
                                 min = 1, max = 50, value = c(2,4), step=0.1),
                     sliderInput("AF2_soil_loss_wind_p", "soil loss due to wind [tons/ha/year]",
                                 min = 1, max = 50, value = c(2,4), step=0.1),
                     sliderInput("af2_less_grazing_management_time_factor_p", "Reduced livestock mgmt from AF providing partial natural paddock [%]",
                                 min = 0, max = 1, value = c(0.8,0.9), step=0.01),
                     sliderInput("AF2_woody_benefit_windreduc_p", " Stress Reduction and Livestock Performance Enhancement [%]",
                                 min = 0, max = 1, value = c(0.03,0.055), step=0.0001),
                     
                     selectInput(
                       inputId = "AF2_crop_rotation", #"crop_rotation", 
                       label = "Choose a crop rotation scheme:", 
                       choices = c("Crop Rotation 1: 3 years of Herbal Ley followed by Winter wheat" = "rotation_1", 
                                   "Crop Rotation 2: 2 yeras of Herbal Ley followed by rotation with Winter wheat, Spring barley, summer beans and Winter oats" = "rotation_2"),
                       selected = NULL
                     ),
                     # Checkbox to introduce animals with number of animals
                     # checkboxInput("AF2_include_animals_c", "Introduce Animals", FALSE),
                     # conditionalPanel(
                     #   condition = "input.AF2_include_animals_c == true",
                     #   numericInput("AF2_number_of_animals_c", "Number of Animals:", value = 1, min = 1)
                     # ),
                     
                     # Checkbox to introduce animals with grazing %
                     checkboxInput("AF2_include_animals_c", "Introduce Animals", FALSE),
                     conditionalPanel(
                       condition = "input.AF2_include_animals_c == true",
                       selectInput("AF2_animal_type", "Select Animal",
                                   choices = c("cattle", "sheep"), #"Goats", "Chickens", "Turkeys"),
                                   selected = NULL,
                                   multiple = TRUE),
                       uiOutput("AF2_grazing_intensity"), # Dynamic UI for slider inputs (one per selected animal)
                       textOutput("AF2_grazing_warning"), # Display a warning message if the total exceeds 1
                     ),
                   ),
                   accordion_panel(
                     title = "Annual Crops",
                     icon = icon("pagelines"),
                     #h4("Annual Crops"),
                     h5 ("Wheat"),
                     sliderInput("winter_wheat_yield_p", "Yield [t/ha]",
                                 min = 0, max = 50, value = c(2,8)),
                     sliderInput("winter_wheat_value_p", "Sale price [GBP/t]",
                                 min = 0, max = 1000, value = c(450,520)),
                     h5 ("Barley"),
                     sliderInput("spring_barley_yield_p", "Yield [t/ha]",
                                 min = 0, max = 50, value = c(1.5,5.5), step=0.01),
                     sliderInput("spring_barley_value_p", "Sale price [GBP/t]",
                                 min = 0, max = 1000, value = c(350,470)),
                     h5 ("Herbal Ley"),
                     sliderInput("herbal_ley_yield_p", "Yield [t/ha]",
                                 min = 0, max = 100, value = c(5,12)),
                     sliderInput("herbal_ley_value_p", "Sale price [GBP/t]",
                                 min = 0, max = 100, value = c(5,10)),
                     # sliderInput("herbal_effect_p", "Beneficial effect on subsequent crops [%]",
                     #             min = 0, max = 10000, value = c(1,5)),
                     h5 ("Summer Beans"),
                     sliderInput("summer_beans_yield_p", "Yield [t/ha]",
                                 min = 0, max = 1000, value = c(1.5,4.5), step=0.01),
                     sliderInput("summer_beans_value_p", "Sale price [GBP/t]",
                                 min = 0, max = 1000, value = c(500,600)),
                     h5 ("Winter Oats"),
                     sliderInput("winter_oats_yield_p", "Yield [t/ha]",
                                 min = 0, max = 50, value = c(1.5,6), step=0.01),
                     sliderInput("winter_oats_value_p", "Sale price [GBP/t]",
                                 min = 0, max = 100, value = c(5,10)),
                     h5 ("Winter Cover Crops"),
                     sliderInput("winter_cover_crop_yield_p", "Yield [t/ha]",
                                 min = 0, max = 100, value = c(3,6)),
                   ),
                   accordion_panel(
                     title = "Animals",
                     icon = icon("cow"), 
                     #h4("Animals"),
                     sliderInput("grazing_efficiency_p", "Trampling selective grazing & areas not consumed [%]",
                                 min = 0, max = 1, value = c(0.5,0.8), step=0.01),
                     sliderInput("herbal_grazing_labour_p", "labour to accommodate livestock grazing [h/ha over a year]",
                                 min = 1, max = 50, value = c(2,5)),
                     sliderInput("beef_value_p", "Price of selling live cows [GBP/Kg]",
                                 min = 1, max = 50, value = c(4.2,5.8), step=0.01),
                     sliderInput("lamb_value_p", "Price of selling sheep [GBP/Kg]",
                                 min = 1, max = 50, value = c(7.3,8.7), step=0.01),
                   ),
                   accordion_panel(
                     title = "Benefits of AF",
                     icon = icon("envira"),
                     #h4("Benefits & Drawbacks of AF"),
                     # sliderInput("Nonmarket_ES_value_p", "Monetary value of ES in AF system GBP/ ha/ y",
                     #             min = 1, max = 10000, value = c(600,960)),
                     sliderInput("C_price_p", "Carbon Price [GBP/t C]",
                                 min = 1, max = 500, value = c(20,30)),
                     sliderInput("winter_grazing_effect_p", "Positive yield effect or fertilization effect because of winter grazing [%]",
                                 min = 1, max = 100, value = c(1.05,1.15), step=0.001),
                     # sliderInput("per_market_price_p", "% fluctuation in market price of all products [%]",
                     #             min = 1, max = 100, value = c(1,5)),
                   ),
                   accordion_panel(
                     title = "Other parameters", #"Need clarification",
                     icon = icon("question-circle"),
                     #h4("Need clarification"),
                     sliderInput("irrigation_sys_install_p", "Cost of installing irrigation system[GBP]",
                                 min = 1, max = 10000, value = c(1000,3000)),
                     sliderInput("irrigation_planting_shrub_p", "Water used soon after planting shrub[l/shrub]",
                                 min = 1, max = 50, value = c(1,1.5), step=0.01),
                     sliderInput("irrigation_planting_tree_p", "Water used soon after planting tree[l/tree]",
                                 min = 1, max = 90, value = c(10,20)),
                     # sliderInput("irrigation_123_p", "Water use in the frist three years of tree establishment[l/tree]",
                     #             min = 1, max = 10000, value = c(1,5)),
                     # sliderInput("irrigation_annual_p", "Annual water use after the first three years[l/tree]",
                     #             min = 1, max = 10000, value = c(1,5)),
                     sliderInput("compost_planting_shrub_p", "Compost used at the time of planting shrubs [l/shrub]",
                                 min = 0.1, max = 50, value = c(0.5,1), step=0.01),
                     sliderInput("compost_planting_tree_p", "Compost used at the time of planting tree [l/tree]",
                                 min = 0.1, max = 70, value = c(10,20)),
                     sliderInput("compost_price_p", "Price of compost [GBP/l]",
                                 min = 0.1, max = 50, value = c(0.1,0.25), step=0.01),
                     sliderInput("rowan_yield_max_p", "Max. yield of Rowan [kg/tree]",
                                 min = 1, max = 100, value = c(10,40)),
                     sliderInput("rowan_value_p", "Price of Rowan[GBP/kg]",
                                 min = 0, max = 10, value = c(0.5,0.75), step=0.01),
                     sliderInput("hazel_yield_max_p", "Max. yield of Hazel [kg/tree]",
                                 min = 1, max = 50, value = c(3,12)),
                     sliderInput("hazel_value_p", "Price of Hazel [GBP/kg]",
                                 min = 0, max = 20, value = c(0.89,3.08), step=0.001),
                     sliderInput("damson_yield_max_p", "Max. yield of Damson [kg/tree]",
                                 min = 1, max = 100, value = c(11.35,27.24), step=0.01),
                     sliderInput("damson_value_p", "Price of Damson [GBP/kg]",
                                 min = 0, max = 20, value = c(0.88,3.3), step=0.01),
                     sliderInput("tree_yield_max_p", "Max. volume of SRC tree growth [t/ha*yr]",
                                 min = 1, max = 50, value = c(7,12), step=0.1),
                     sliderInput("biomass_timber_price_p", "Price of selling one tonne of SRC willow [GBP/t]",
                                 min = 1, max = 1000, value = c(80,260)),
                   ),
                   br(),
                   br(),
                   actionButton("run_simulation", "Run Model", icon = icon("play"), class = "btn-primary"),
                 )
    ),
    # define the content of the main panel of the UI
    mainPanel(
      width = 8,
      # Display plots of DA
      fluidRow(
        column(width = 4),
        tags$h6(
          "This app simulates the present value of converting a treeless arable field into a silvopastoral agroforestry system, developed based on one of the UK living labs at Mindrum, the design specifications were shared by Mr. Tom Fairfax",
          tags$br(),
          "Design 1: 2 strips of 200 m x 15 m of short rotation coppice with Salix spp. (particular interest in", 
          tags$i("S. caprea"),
          "and a mixed native species tree guard area surrounding the strips. The strips are orientated southwest by northeast.",
          tags$br(),
          "Design 2: 3 strips of 300 m x 5 m of woody strips-oriented northwest by southeast along the slope with a wide variety of trees surrounded by a thick clumping of shrubby species also including",
           tags$i("Salix spp."),
          tags$br(),
          tags$br(),
          "Use the tabs on the left to adjust variable ranges based on your local conditions or design goals (including choice of crop rotation and introducing animals).",
          tags$br(),
          tags$br(),
          "Click ‘Run model’ to perform a Monte Carlo simulation using random combinations from your defined ranges. You can save/load inputs, and once the model runs, results will appear below and you can save these figures.",
          tags$br(),
          "We welcome your feedback and encourage you to suggest additional funding schemes for your region. Feel free to contact",
          tags$a(href = "mailto:pkasargo@uni-bonn.de", "Prajna Kasargodu Anebagilu"), "or", tags$a(href = "mailto:afuelle1@uni-bonn.de", "Adrain Fuelle")
        ),
        
        
        #textOutput("display_version_1"),
        # Added a button to open the URL
        # actionButton("open_url", "Click here for latest info on Sustainable Farming Incentive"), 
        column(width = 4,
               tags$a("Click here for latest info on Sustainable Farming Incentive",
                      href = "https://www.gov.uk/government/publications/sustainable-farming-incentive-scheme-expanded-offer-for-2024",
                      target="_blank",
                      class = "my-btn")),
        column(width = 4)
      ),
      h5('Figure 1. Probabilistic outcome distributions from Monte Carlo simulation for baseline and the intervention options.'),
      plotOutput("distPlot"),
      p(
        'The graphs above compare the net present value (NPV) distributions
        between baseline and intervention options, expressed in GBP/ha across
        the simulation period. The x-axis displays NPV values, representing the
        sum of discounted cash flows, while the y-axis presents the different
        scenarios being compared.'
      ),       # p('The graph above provides a visual comparison of the outcome distributions 
      #   for the baseline and intervention options in terms of net present value 
      #   (NPV in GBP/ha) over the simulated period. The x-axis represents the NPV, 
      #   calculated as the sum of discounted cash flows for each simulation year. 
      #   The y-axis shows the probability of occurrence, indicating the likelihood 
      #   of different NPV values. A higher value on the y-axis corresponds to a 
      #   greater probability of the associated NPV on the x-axis.'),
      downloadButton("save_plot1", "Download Figure 1"),
      br(), # blank line
      br(), # blank line
      # h5('2. Probabilistic outcome of the decision in terms of NPV over the simulation period.'),
      # plotOutput("distPlot2"),
      # p(
      #   'The graph above illustrates the outcome in terms of NPV in GBP/ha) over 
      #   the simulated years, comparing the baseline scenario with the intervention. 
      #   It highlights the differences in net cash flows between the two optioons. 
      #   The right-skewness of the graph suggests that the intervention is generally 
      #   favorable. However, since the distribution includes both positive and 
      #   negative values, there remains a non-zero probability that the intervention 
      #   may not always yield a more favorable outcome than the baseline. The 
      #   integrated box plot shows that while the interquartile range (IQR) is mostly positive, 
      #   it does include some negative values. The vertical line within the box plot represents the median NPV.'),
      # downloadButton("save_plot2", "Save Plot"), 
      # br(), # blank line
      # br(), # blank line
      h5('Probabilistic outcome of the decisions'),
      plotOutput("distPlot3"),
      p('The graph above illustrates NPV outcomes (GBP/ha) over the simulation
         period, directly comparing intervention scenarios against the baseline. 
        The x-axis shows the NPV differential, where negative values indicate 
        the baseline outperforming interventions and positive values show 
        interventions outperforming the baseline. The y-axis represents 
        probability density, with higher values indicating greater likelihood 
        of achieving the corresponding NPV difference.'),
      downloadButton("save_plot3", "Download Figure 2"), 
      
      br(), # blank line
      br(), # blank line
      
      h5('Figure 3. Net Present Value (NPV) Outcomes with and without fudning for the two designs'),
      plotOutput("distPlot4"),
      p('Figure 3 shows the comparison of net present value (NPV) outcomes for the decision of different agroforestry designs with and without funding schemes'),
      downloadButton("save_plot4", "Download Figure 3"), 
      
      h5("Figure 4. Annual cash-flow of the agroforestry design 1"),
      plotOutput("distPlot5"),
      p('Figure shows how annual cash-flow from an agroforestry intervention is expected to evolve, 
        based on a probabilistic simulation. The shaded areas represent uncertainty ranges (from lower to upper quantiles), 
        while the blue line shows the median outcome (expressed in €). While early years may involve negative cash flow, 
        profitability tends to improve over time, with increasing stability. The graph highlights the long-term financial potential and 
        risk spread of adopting agroforestry practices.'),
      downloadButton("save_plot5", "Download Figure 4"), 
      
      h5("Figure 6. Cumulative cash-flow of the agroforestry design 1"),
      plotOutput("distPlot6"),
      p("Figure illustrates how total cash-flow (expressed in €) accumulates over 
        time from the agroforestry intervention, based on a range of simulated outcomes. 
        The shaded areas represent uncertainty (spread of possible results), 
        and the blue line indicates the median trajectory. Cumulative returns grow steadily over time, 
        showing the long-term profitability potential of agroforestry. Despite initial variability, 
        the system trends positively, reinforcing the case for agroforestry as a viable financial investment over the long run."),
      downloadButton("save_plot6", "Download Figure 5"), 
      
      h5("Figure 7. Annual cash-flow of the agroforestry design 2"),
      plotOutput("distPlot7"),
      p('Figure shows how annual cash-flow from an agroforestry intervention is expected to evolve, 
        based on a probabilistic simulation. The shaded areas represent uncertainty ranges (from lower to upper quantiles), 
        while the blue line shows the median outcome (expressed in €). While early years may involve negative cash flow, 
        profitability tends to improve over time, with increasing stability. The graph highlights the long-term financial potential and 
        risk spread of adopting agroforestry practices.'),
      downloadButton("save_plot7", "Download Figure 6"), 
      
      h5("Figure 8. Cumulative cash-flow of the agroforestry design 2"),
      plotOutput("distPlot8"),
      p("Figure illustrates how total cash-flow (expressed in €) accumulates over 
        time from the agroforestry intervention, based on a range of simulated outcomes. 
        The shaded areas represent uncertainty (spread of possible results), 
        and the blue line indicates the median trajectory. Cumulative returns grow steadily over time, 
        showing the long-term profitability potential of agroforestry. Despite initial variability, 
        the system trends positively, reinforcing the case for agroforestry as a viable financial investment over the long run."),
      downloadButton("save_plot8", "Download Figure 7"), 

      tags$img(src = "Funding_declaration.png", height = "100px",
                       style = "margin-right: auto; max-width: 100%; height: auto; cursor: pointer;"),
                tags$div( style = "text-align:center;",
                tags$h6("Designed and Developed by INRES Horticultural Sciences, University of Bonn ",
                        br(),
                tags$p(
                  tags$a("Disclaimer", href = "https://agroreforest.eu/reforest-tools-disclaimer/",
                target = "_blank"),
                     " | ",  
                tags$a("View Source", href = "https://github.com/hortibonn/Silvopastoral_Livestock/",
                target = "_blank")
                  ),
                        )
                    ),

                        )
                    ),
    )
  )
)
#)
# Define Server
server <- function(input, output, session) {
  #### Conditional Reactive variables based on user selection ####
  # Funding ReactiveVaules ####
  
  # Ensure reactive values are used correctly
  tidy_funding_data <- reactive({
    if (exists("funding_data")) {
      return(isolate(funding_data()))
    }
    return(list())
  })
  
  funding_data_reactive <- reactive({ return (funding_data) })
  
  # Fix UI rendering to use tagList instead of list
  output$state_ui <- renderUI({
    if (is.null(input$country)) return(NULL)
    req(input$country)
    tagList(
      selectInput("state", "Select State:", choices = country_states[[input$country]], selected = NULL)
    )
  })
  # Show funding schemes dropdown after state selection
  # One-time funding Schemes
  output$funding_one_ui <- renderUI({
    req(input$country, input$state)
    country <- input$country
    state <- input$state
    
    if (!is.null(funding_data_reactive()[[country]]) && !is.null(funding_data_reactive() [[country]][[state]])) {
      state_data <- funding_data_reactive() [[country]][[state]]
      return(tagList(
        selectInput("funding_one_ui", "Select One-time Funding Scheme(s):", 
                    choices = if (!is.null(state_data$one_time_funding_schemes)) state_data$one_time_funding_schemes else list(),
                    multiple = TRUE)
      ))
    }
    return(NULL)
  })
  # Annual funding Schemes
  output$funding_yearly_ui <- renderUI({
    req(input$state, input$country)
    country <- input$country
    state <- input$state
    
    if (!is.null(funding_data_reactive() [[country]]) && !is.null(funding_data_reactive() [[country]][[state]])) {
      state_data <- funding_data_reactive()[[country]][[state]]
      return(tagList(
        selectInput("funding_yearly_ui", "Select Annual Funding Scheme(s):", 
                    choices = if (!is.null(state_data$annual_funding_schemes)) state_data$annual_funding_schemes else list(),
                    multiple = FALSE)
      ))
    }
    return(NULL)
  })
  
  # Ensure function names are not overwritten
  if (!exists("safe_list")) {
    safe_list <- list
  }
  ## Animals Reactive Values ####
  
  # Reactive value for introduce_animals
  
  ### Treeless System UI ###
  # include_animals_c <- reactive({
  #   if (input$include_animals_c) {1} else {0}
  # })
  # When Number of animals is one of the parameters required #
  # output$animal_quantity <- renderUI({
  #   req(input$animal_type)
  #   lapply(input$animal_type, function(animal) {
  #     numericInput(paste0("A_quantity_", animal, "_c"), paste("Quantity of", animal), value = 0, min = 0)
  #   })
  # })
  
  # When grazing intensity is one of the parameters required #
  # Dynamically generate a slider input for each selected animal.
  output$treeless_grazing_intensity <- renderUI({
    if (!input$treeless_include_animals_c || is.null(input$treeless_animal_type) || length(input$treeless_animal_type) == 0) {
      return(NULL)
    }
    # req(input$treeless_include_animals_c)
    tagList(
      lapply(input$treeless_animal_type, function(animal) {
        sliderInput(
          inputId = paste0("treeless_", animal, "_intensity_c"),
          label = paste("Percentage of grazing granted for", animal),
          min = 0, max = 1, value = 0, step = 0.01
        )
      })
    )
    #do.call(tagList, sliders)
  })
  
  treeless_total_grazing <- reactive({
    if (!input$treeless_include_animals_c || is.null(input$treeless_animal_type) || length(input$treeless_animal_type) == 0) {
      return(0)
    }
    vals <- sapply(input$treeless_animal_type, function(animal) {
      val <- input[[paste0("treeless_", animal, "_intensity_c")]]
      if (is.null(val)) 0 else val
    })
    sum(vals)
  })
  
  output$treeless_grazing_warning <- renderText({
    tg <- treeless_total_grazing()
    if (tg > 1) "Error: Total grazing percentage exceeds 1. Please adjust the grazing percentages." else ""
  })
  
  ### AF1 System UI ###
  # AF1_include_animals_c <- reactive({
  #   if (input$AF1_include_animals_c) {1
  #   } else {
  #     0
  #   }
  # })
  output$AF1_grazing_intensity <- renderUI({
    #req(input$AF1_include_animals_c)
    if (!input$AF1_include_animals_c || is.null(input$AF1_animal_type) || length(input$AF1_animal_type) == 0) {
      return(NULL)
    }
    sliders <- lapply(input$AF1_animal_type, function(animal) {
      sliderInput(
        inputId = paste0("AF1_", animal, "_intensity_c"),
        label = paste("Percentage of grazing granted for", animal),
        min = 0, max = 1, value = 0, step = 0.01
      )
    })
    do.call(tagList, sliders)
  })
  
  AF1_total_grazing <- reactive({
    if (!input$AF1_include_animals_c || is.null(input$AF1_animal_type) || length(input$AF1_animal_type) == 0) {
      return(0)
    }
    
    vals <- sapply(input$AF1_animal_type, function(animal) {
      val <- input[[paste0("AF1_", animal, "_intensity_c")]]
      if (is.null(val)) 0 else val
    })
    sum(vals)
  })
  output$AF1_grazing_warning <- renderText({
    tg <- AF1_total_grazing()
    if (tg > 1) "Error: Total grazing percentage exceeds 1. Please adjust the grazing percentages." else ""
  })
  
  ### AF1 System UI ###
  # AF2_include_animals_c <- reactive({
  #   if (input$AF2_include_animals_c) {
  #     1
  #   } else {
  #     0
  #   }
  # })
  output$AF2_grazing_intensity <- renderUI({
    #req(input$AF2_include_animals_c)
    if (!input$AF2_include_animals_c || is.null(input$AF2_animal_type) || length(input$AF2_animal_type) == 0) {
      return(NULL)
    }
    sliders <- lapply(input$AF2_animal_type, function(animal) {
      sliderInput(
        inputId = paste0("AF2_", animal, "_intensity_c"),
        label = paste("Percentage of grazing granted for", animal),
        min = 0, max = 1, value = 0, step = 0.01
      )
    })
    do.call(tagList, sliders)
  })
  AF2_total_grazing <- reactive({
    if (!input$AF2_include_animals_c || is.null(input$AF2_animal_type) || length(input$AF2_animal_type) == 0) {
      return(0)
    }
    vals <- sapply(input$AF2_animal_type, function(animal) {
      val <- input[[paste0("AF2_", animal, "_intensity_c")]]
      if (is.null(val)) 0 else val
    })
    sum(vals)
  })
  output$AF2_grazing_warning <- renderText({
    tg <- AF2_total_grazing()
    if (tg > 1) "Error: Total grazing percentage exceeds 1. Please adjust the grazing percentages." else ""
  })
  
  ########### save load delete logic ##############
  
  # Determine file directory based on OS
  file_dir <- if (Sys.info()[["sysname"]] == "Windows") {
    "user-states/Mindrum/"  # Use Windows path for local testing
  } else {
    "/srv/shiny-app-data/user-states/Mindrum/"  # Linux path for server
  }
  
  # Get user ID or set default for testing
  user_id <- session$user %||% "local_test_user"
  # user_id <- session$user %||% "Adrian"
  is_admin <- user_id %in% admin_users
  
  # Output to determine if user is admin
  output$isAdmin <- reactive(is_admin)
  outputOptions(output, "isAdmin", suspendWhenHidden = FALSE)
  
  # Admin user selection UI
  output$admin_user_select <- renderUI({
    if (is_admin) {
      user_folders <- list.dirs(file_dir, full.names = FALSE, recursive = FALSE)
      if (length(user_folders) == 0) {
        user_folders <- user_id
      }
      div(
        class = "admin-panel",
        h4("Admin Panel"),
        selectInput("admin_selected_user", "Select User Folder:", choices = user_folders, selected = user_id)
      )
    }
  })
  # Utility function to ensure directory exists
  ensureDirectoryExists <- function(dir) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
  # Reactive expression for current user directory
  current_user_dir <- reactive({
    user_folder <- if (is_admin && !is.null(input$admin_selected_user)) {
      input$admin_selected_user
    } else {
      user_id
    }
    dir <- file.path(file_dir, user_folder)
    ensureDirectoryExists(dir)
    dir
  })
  
  # Reactive value to store versions
  versions <- reactiveVal()
  
  # Function to update version selections
  updateVersionSelections <- function() {
    vers <- basename(list.files(current_user_dir(), full.names = TRUE, pattern = "csv"))
    versions(vers)
    updateSelectInput(session, "version_select", choices = versions())
    updateSelectInput(session, "delete_version_select", choices = versions())
  }
  
  # Update versions when current_user_dir changes
  observe({
    updateVersionSelections()
  })
  
  
  
  
  
  
  
  
  # Save settings
  # observeEvent(input$save, {
  #   files <- list.files(current_user_dir(), full.names = TRUE, pattern = "\\.csv$")
  #   
  #   if (length(files) >= max_files) {
  #     showNotification("Error: Maximum number of files reached.", type = "error", duration = 5)
  #     return()
  #   }
  #   
  #   if (input$project_name == "") {
  #     showNotification("Error: Please enter a project name before saving.", type = "error", duration = 5)
  #     return()
  #   }
  #   
  #   timestamp <- format(Sys.time(), "%y%m%d_%H%M%S")
  #   safe_project_name <- gsub("[^A-Za-z0-9_]+", "_", input$project_name)
  #   file_name <- paste0(timestamp, "_", safe_project_name, ".csv")
  #   file_path <- file.path(current_user_dir(), file_name)
  #   
  #   # Exclude non-relevant inputs
  #   exclude_inputs <- c("collapseSidebar", "save", "load", "delete", "confirm_delete", 
  #                       "admin_selected_user", "project_name", "version_select", "delete_version_select")
  #   
  #   # Define variables based on input names
  #   regex <- "(_c$|_p$|_t$|^country$|^state$)"
  #   variables <- setdiff(names(input)[grepl(regex, names(input))], exclude_inputs)
  #   
  #   # Extract lower and upper values
  #   lower_values <- sapply(variables, function(var) {
  #     value <- input[[var]]
  #     if (length(value) == 1) {
  #       as.numeric(value)
  #     } else {
  #       as.numeric(value[1])
  #     }
  #   })
  #   
  #   upper_values <- sapply(variables, function(var) {
  #     value <- input[[var]]
  #     if (length(value) == 1) {
  #       as.numeric(value)
  #     } else {
  #       as.numeric(value[2])
  #     }
  #   })
  #   # Determine distributions
  #   distributions <- sapply(variables, function(var) {
  #     if (grepl("_c$", var)) {
  #       "const"  # Constant distribution
  #     } else if (grepl("_p$", var)) {
  #       "posnorm"  # Positive normal distribution
  #     } else {
  #       "tnorm_0_1"  # Default truncated normal
  #     }
  #   })
  #   # Create the data frame
  #   df <- data.frame(
  #     variable = variables,
  #     lower = lower_values,
  #     upper = upper_values,
  #     distribution = distributions,
  #     stringsAsFactors = FALSE
  #   )
  #   # Write to CSV
  #   write.csv(df, file_path, row.names = FALSE)
  #   showNotification("Settings saved successfully.", type = "message", duration = 5)
  #   # Refresh version selections
  #   updateVersionSelections()
  # })
  
  
  observeEvent(input$save, {
    files <- list.files(current_user_dir(), full.names = TRUE, pattern = "\\.csv$")
    
    if (length(files) >= max_files) {
      showNotification("Error: Maximum number of files reached.", type = "error", duration = 5)
      return()
    }
    
    if (input$project_name == "") {
      showNotification("Error: Please enter a project name before saving.", type = "error", duration = 5)
      return()
    }
    
    timestamp <- format(Sys.time(), "%y%m%d_%H%M%S")
    safe_project_name <- gsub("[^A-Za-z0-9_]+", "_", input$project_name)
    file_name <- paste0(timestamp, "_", safe_project_name, ".csv")
    file_path <- file.path(current_user_dir(), file_name)
    
    # Exclude non-relevant inputs
    exclude_inputs <- c("collapseSidebar", "save", "load", "load2", "load3", "delete", "confirm_delete", 
                        "admin_selected_user", "project_name", "version_select", "delete_version_select")
    
    # Define variables based on input names
    regex <- "(_c$|_p$|_t$|^country$|^state_ui$|^state$|^funding_one_ui$|^funding_yearly_ui$|^treeless_animal_type$)"
    variables <- setdiff(names(input)[grepl(regex, names(input))], exclude_inputs)
    
    ## ---- 2. build the data-frame row–by–row ---------------------------------
    rows <- lapply(variables, function(id) {
      val <- input[[id]]
      
      # Skip NULLs (can happen with conditional UI)
      if (is.null(val)) return(NULL)
      
      # SLIDER ---------------------------------------------------------------
      if (is.numeric(val) && length(val) == 2) {
        data.frame(variable = id,
                   lower    = val[1],
                   upper    = val[2],
                   stringsAsFactors = FALSE)
        
        # MULTI-SELECT (character vector) -------------------------------------
      } else if (length(val) > 1) {
        ser <- paste(val, collapse = ";")
        data.frame(variable = id,
                   lower    = ser,
                   upper    = ser,
                   stringsAsFactors = FALSE)
        
        # EVERYTHING ELSE ------------------------------------------------------
      } else {
        data.frame(variable = id,
                   lower    = val,
                   upper    = val,
                   stringsAsFactors = FALSE)
      }
    })
    
    df <- do.call(rbind, rows)
    
    # Write to CSV
    write.csv(df, file_path, row.names = FALSE)
    showNotification("Settings saved successfully.", type = "message", duration = 5)
    # Refresh version selections
    updateVersionSelections()
  })
  
  # Load settings
  # observeEvent(input$load, {
  #   selected_file <- file.path(current_user_dir(), input$version_select)
  #   if (file.exists(selected_file)) {
  #     df <- read.csv(selected_file, stringsAsFactors = FALSE)
  #     # Update inputs
  #     for (i in 1:nrow(df)) {
  #       var <- df$variable[i]
  #       lower <- df$lower[i]
  #       upper <- df$upper[i]
  #       
  #       if (var %in% names(input)) {
  #         # Determine the type of input and update accordingly
  #         if (length(input[[var]]) == 1) {
  #           # Numeric or text input
  #           if (is.numeric(input[[var]])) {
  #             updateNumericInput(session, var, value = lower)
  #           } else if (is.character(input[[var]])) {
  #             updateTextInput(session, var, value = as.character(lower))
  #           }
  #         } else if (length(input[[var]]) == 2) {
  #           # Slider input with range
  #           updateSliderInput(session, var, value = c(lower, upper))
  #         }
  #       }
  #     }
  #     showNotification("Settings loaded successfully.", type = "message", duration = 5)
  #   } else {
  #     showNotification("Error: File could not be loaded.", type = "error", duration = 5)
  #   }
  # })
  
  
  do_pass <- function(data) {
    for (i in seq_len(nrow(data))) {
      
      var <- data$variable[i]
      lo  <- data$lower[i]
      up  <- data$upper[i]
      
      if (lo %in% c("TRUE", "FALSE")) {
        updateCheckboxInput(session, var, value = as.logical(lo))
        
      } else if (grepl(";", lo)) {                # multi-select stored "a;b;c"
        updateSelectInput(session, var, selected = strsplit(lo, ";")[[1]])
        
      } else if (!is.na(suppressWarnings(as.numeric(lo))) &&
                 !is.na(suppressWarnings(as.numeric(up))) &&
                 lo != up) {                      # slider
        updateSliderInput(session, var,
                          value = c(as.numeric(lo), as.numeric(up)))
        
      } else if (!is.na(suppressWarnings(as.numeric(lo)))) {
        updateNumericInput(session, var, value = as.numeric(lo))
        
      } else {
        updateSelectInput(session, var, selected = lo)   # plain select/text
      }
    }
  }
  
  # to_restore <- reactiveValues(values = NULL)   # named vector of target values
  # 
  # observeEvent(input$load, {
  #   selected_file <- file.path(current_user_dir(), input$version_select)
  #   req(file.exists(selected_file))
  #   
  #   df <- read.csv(selected_file, stringsAsFactors = FALSE)
  #   vals <- setNames(df$lower, df$variable)     # named character vector
  #   
  #   bslib::accordion_panel_open("collapseSidebar",TRUE,session)
  #   
  #   ## ---- 1st wave: push whatever widgets already exist --------------------
  #   existing <- intersect(names(vals), names(input))
  #   do_pass(df[df$variable %in% existing, ])
  #   
  #   ## ---- remember everything else for later -------------------------------
  #   to_restore$values <- vals[!names(vals) %in% existing]
  #   
  #   showNotification("Loading settings…", type = "message", duration = 4)
  # })
  
  observeEvent(input$load, {
    selected_file <- file.path(current_user_dir(), input$version_select)
    req(file.exists(selected_file))
    
    df <- read.csv(selected_file, stringsAsFactors = FALSE)
    
    bslib::accordion_panel_open("collapseSidebar",TRUE,session)
    
    do_pass(df)
    
    showNotification("Loading basic", type = "message", duration = 4)
  })
  observeEvent(input$load2, {
    selected_file <- file.path(current_user_dir(), input$version_select)
    req(file.exists(selected_file))
    
    df <- read.csv(selected_file, stringsAsFactors = FALSE)
    
    bslib::accordion_panel_open("collapseSidebar",TRUE,session)
    
    do_pass(df)
    
    showNotification("Loading conditions", type = "message", duration = 4)
  })
  observeEvent(input$load3, {
    selected_file <- file.path(current_user_dir(), input$version_select)
    req(file.exists(selected_file))
    
    df <- read.csv(selected_file, stringsAsFactors = FALSE)
    
    bslib::accordion_panel_open("collapseSidebar",TRUE,session)
    
    do_pass(df)
    
    showNotification("Loading funding", type = "message", duration = 4)
  })
  
  # observe({
  #   pending <- isolate(to_restore$values)
  #   if (is.null(pending) || length(pending) == 0) return()   # nothing left to do
  #   
  #   # if *any* of the still-missing widgets now exists, set it
  #   ready <- intersect(names(pending), names(input))
  #   if (length(ready) > 0) {
  #     df_ready <- data.frame(variable = ready,
  #                            lower    = pending[ready],
  #                            upper    = pending[ready],
  #                            stringsAsFactors = FALSE)
  #     do_pass(df_ready)
  #     
  #     # drop the ones we just handled
  #     to_restore$values <- pending[!names(pending) %in% ready]
  #   }
  #   
  #   # as long as something is still missing, re-check after 100 ms
  #   if (!is.null(to_restore$values) && length(to_restore$values) > 0) {
  #     invalidateLater(100, session)
  #   } else {
  #     showNotification("Settings loaded ✔︎", type = "message", duration = 3)
  #   }
  # })
  
  
  
  # Delete settings with confirmation
  observeEvent(input$delete, {
    req(input$delete_version_select)
    showModal(modalDialog(
      title = "Confirm Deletion",
      paste("Are you sure you want to delete the file:", input$delete_version_select, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete, {
    removeModal()
    file_to_delete <- file.path(current_user_dir(), input$delete_version_select)
    if (file.exists(file_to_delete)) {
      file.remove(file_to_delete)
      showNotification("File deleted successfully.", type = "message", duration = 5)
      # Refresh version selections
      updateVersionSelections()
    } else {
      showNotification("Error: File could not be deleted.", type = "error", duration = 5)
    }
  })
  ## End of save load delete logic ####
  
  ## Collect all data ####
  #### Data analysts' csv file ####
  # Read variables from an internal CSV file
  # Validate CSV file before reading
  manual_variables <- reactive({
    #message("Accessing analyst estimates...")
    csv_path <- here("manual_variables_AF.csv")
    if (!file.exists(csv_path)) {
      stop("Error: CSV file not found at ", csv_path)
    }
    manual_data <- read.csv(csv_path, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    
    required_columns <- c("variable", "lower", "upper", "distribution")
    if (!all(required_columns %in% names(manual_data))) {
      stop("Error: CSV must contain 'variable', 'lower', 'upper', and 'distribution' columns.")
    }
    manual_data <- manual_data[required_columns]
    manual_data$lower <- as.numeric(manual_data$lower)
    manual_data$upper <- as.numeric(manual_data$upper)
    return(manual_data)
  })
  #### End of Data analyst' csv file ####
  
  #### Animal estimates ####
  animal_estimates <- reactive({
    #message("Accessing animal estimates...")
    # When number of animals is required #
    # cows_selected_c <- if("Cows" %in% input$animal_type) 1 else 0  # Purchase, Vet, Feed
    # sheep_selected_c <- if("Sheep" %in% input$animal_type) 1 else 0
    # goats_selected_c <- if("Goats" %in% input$animal_type) 1 else 0
    # chickens_selected_c <- if("Chickens" %in% input$animal_type) 1 else 0
    # turkeys_selected_c <- if("Turkeys" %in% input$animal_type) 1 else 0
    # 
    # animal_parameters <- c("cows_selected_c", "sheep_selected_c", "goats_selected_c", "chickens_selected_c", "turkeys_selected_c")
    # selected_values_animals <- c(cows_selected_c, sheep_selected_c, goats_selected_c, chickens_selected_c, turkeys_selected_c)
    
    # when grazing intensity is required #
    # For each system, get the animal selections and add a prefix and the _c suffix.
    af2_animals      <- if (input$AF2_include_animals_c && !is.null(input$AF2_animal_type)) paste0("af2_", input$AF2_animal_type, "_intensity_c")      
    else {paste0("af2_", c("cattle", "sheep"), "_intensity_c")}
    af1_animals      <- if (input$AF1_include_animals_c && !is.null(input$AF1_animal_type)) paste0("af1_", input$AF1_animal_type, "_intensity_c")      
    else {paste0("af1_", c("cattle", "sheep"), "_intensity_c")}
    treeless_animals <- if (input$treeless_include_animals_c && !is.null(input$treeless_animal_type))  paste0("t_", input$treeless_animal_type, "_intensity_c")
    else {paste0("t_", c("cattle", "sheep"), "_intensity_c")}
    
    animal_list <- unique(c(af2_animals, af1_animals, treeless_animals))
    # For each prefixed animal (which now has the _c suffix), retrieve its corresponding intensity.
    # To reconstruct the input IDs we need to remove the suffix _c.
    #grazing_vals <- 0    
    grazing_vals <- sapply(animal_list, function(prefixed_animal) {
      if (startsWith(prefixed_animal, "af2_")) {
        base <- sub("^af2_", "", prefixed_animal)
        base <- sub("_intensity_c$", "", base)
        val <- if (input$AF2_include_animals_c && !is.null(input[[paste0("AF2_", base, "_intensity_c")]]))
          input[[paste0("AF2_", base, "_intensity_c")]] else 0
      } else if (startsWith(prefixed_animal, "af1_")) {
        base <- sub("^af1_", "", prefixed_animal)
        base <- sub("_intensity_c$", "", base)
        
        val <- if (input$AF1_include_animals_c && !is.null(input[[paste0("AF1_", base, "_intensity_c")]]))
          input[[paste0("AF1_", base, "_intensity_c")]] else 0
      } 
      else if (startsWith(prefixed_animal, "t_")) {
        base <- sub("^t_", "", prefixed_animal)
        base <- sub("_intensity_c$", "", base)
        
        val <- if (input$treeless_include_animals_c && !is.null(input[[paste0("treeless_", base, "_intensity_c")]]))
          input[[paste0("treeless_", base, "_intensity_c")]] else 0
      } else {
        val <- 0
      }
      val
    })
    animal_variables_df <- data.frame(
      variable = animal_list,
      lower = grazing_vals,
      upper  = grazing_vals,
      distribution = rep("const", length(animal_list)),
      stringsAsFactors = FALSE
    )
    #print(animal_variables_df)
  })
  #### End of Animal estimates ####
  
  # Crop rotation estimates####
  crop_estimates <- reactive({
    #message("Accessing crop estimates...")
    treeless_system_crop_rotation_1_c <- if (input$treeless_crop_rotation == "rotation_1") 1 else 0
    treeless_system_crop_rotation_2_c <- if (input$treeless_crop_rotation == "rotation_2") 1 else 0 
    AF1_system_crop_rotation_1_c <- if (input$AF1_crop_rotation == "rotation_1") 1 else 0
    AF1_system_crop_rotation_2_c <- if (input$AF1_crop_rotation == "rotation_2") 1 else 0
    AF2_system_crop_rotation_1_c <- if (input$AF2_crop_rotation == "rotation_1") 1 else 0
    AF2_system_crop_rotation_2_c <- if (input$AF2_crop_rotation == "rotation_2") 1 else 0
    
    crop_parameters <- c("treeless_system_crop_rotation_1_c", "treeless_system_crop_rotation_2_c", "AF1_system_crop_rotation_1_c", "AF1_system_crop_rotation_2_c", "AF2_system_crop_rotation_1_c", "AF2_system_crop_rotation_2_c")
    selected_values_crops <- c(treeless_system_crop_rotation_1_c, treeless_system_crop_rotation_2_c, AF1_system_crop_rotation_1_c, AF1_system_crop_rotation_2_c, AF2_system_crop_rotation_1_c, AF2_system_crop_rotation_2_c)
    
    crop_data <- data.frame(
      variable = crop_parameters,
      lower = selected_values_crops,
      upper = selected_values_crops,
      distribution = rep("const", length(crop_parameters)),
      stringsAsFactors = FALSE
    )
  })
  #### End of Crop estimates ####
  
  #### Funding estimates ####
  funding_variables <- reactive({
    #message("Accessing funding estimates...")
    country <- input$country
    state   <- input$state
    
    # Default funding values (0 when no country/state is selected)
    selected_percentage_c <- 0
    AF1_total_one_time_funding_c  <- 0
    AF1_total_annual_funding_c    <- 0
    AF1_percentage_values_c       <- 0
    AF2_total_one_time_funding_c  <- 0
    AF2_total_annual_funding_c    <- 0
    AF2_percentage_values_c       <- 0
    
    # Validate area calculations (default to 0 if inputs are missing)
    AF1_area <- max(0, as.numeric(input$arable_area_c) - as.numeric(input$AF1_tree_row_area_c))
    AF2_area <- max(0, as.numeric(input$arable_area_c) - as.numeric(input$AF2_tree_row_area_c))
    
    # Default funding values
    AF1_one_funding <- 0
    AF1_annual_funding <- 0
    AF2_one_funding <- 0
    AF2_annual_funding <- 0
    
    # Only process funding calculations if both country and state are selected
    if (!is.null(country) && !is.null(state) && country != "None" && state != "None") {
      if (!is.null(funding_data_reactive()[[country]]) &&
          !is.null(funding_data_reactive()[[country]][[state]])) {
        
        state_data <- funding_data_reactive()[[country]][[state]]
        
        # ---- AF1 One-Time Funding ----
        AF1_selected_per_ha <- intersect(input$funding_one_ui, state_data$funding_onetime_per_ha_schemes)
        if (length(AF1_selected_per_ha) > 0) {
          AF1_per_ha_values <- state_data$funding_onetime_values_named[AF1_selected_per_ha]
          AF1_one_funding <- sum(AF1_per_ha_values) * AF1_area
        }
        
        AF1_selected_per_tree <- intersect(input$funding_one_ui, state_data$funding_onetime_per_tree_schemes)
        if (length(AF1_selected_per_tree) > 0) {
          AF1_per_tree_values <- state_data$funding_onetime_values_named[AF1_selected_per_tree]
          AF1_one_funding <- AF1_one_funding + sum(AF1_per_tree_values) * input$AF1_num_trees_c
        }
        
        AF1_selected_percentage <- intersect(input$funding_one_ui, state_data$funding_onetime_percentage_schemes)
        if (length(AF1_selected_percentage) > 0) {
          selected_percentage_c <- 1
          AF1_percentage_values_c <- sum(state_data$funding_onetime_values_named[AF1_selected_percentage])
        }
        
        AF1_one_funding <- AF1_one_funding + input$onetime_external_support_c
        AF1_total_one_time_funding_c <- AF1_one_funding
        
        # ---- AF1 Annual Funding ----
        AF1_selected_annual <- intersect(input$funding_yearly_ui, state_data$annual_funding_schemes)
        if (length(AF1_selected_annual) > 0) {
          AF1_annual_values <- state_data$funding_yearly_values_named[AF1_selected_annual]
          AF1_annual_funding <- sum(AF1_annual_values) * AF1_area
        }
        
        AF1_annual_funding <- AF1_annual_funding + input$annual_external_support_c * AF1_area
        AF1_total_annual_funding_c <- AF1_annual_funding
        
        # ---- AF2 One-Time Funding ----
        AF2_selected_per_ha <- intersect(input$funding_one_ui, state_data$funding_onetime_per_ha_schemes)
        if (length(AF2_selected_per_ha) > 0) {
          AF2_per_ha_values <- state_data$funding_onetime_values_named[AF2_selected_per_ha]
          AF2_one_funding <- sum(AF2_per_ha_values) * AF2_area
        }
        
        AF2_selected_per_tree <- intersect(input$funding_one_ui, state_data$funding_onetime_per_tree_schemes)
        if (length(AF2_selected_per_tree) > 0) {
          AF2_per_tree_values <- state_data$funding_onetime_values_named[AF2_selected_per_tree]
          total_AF2_trees <- sum(input$num_oak_trees_c, input$num_birch_trees_c,
                                 input$num_rowan_trees_c, input$num_hazel_trees_c,
                                 input$num_damson_trees_c, input$num_bcherry_trees_c)
          AF2_one_funding <- sum(AF2_per_tree_values) * total_AF2_trees
        }
        
        AF2_selected_percentage <- intersect(input$funding_one_ui, state_data$funding_onetime_percentage_schemes)
        if (length(AF2_selected_percentage) > 0) {
          selected_percentage_c <- 1
          AF2_percentage_values_c <- sum(state_data$funding_onetime_values_named[AF2_selected_percentage])
        }
        
        AF2_one_funding <- AF2_one_funding + input$onetime_external_support_c
        AF2_total_one_time_funding_c <- AF2_one_funding
        
        # ---- AF2 Annual Funding ----
        AF2_selected_annual <- intersect(input$funding_yearly_ui, state_data$annual_funding_schemes)
        if (length(AF2_selected_annual) > 0) {
          AF2_annual_values <- state_data$funding_yearly_values_named[AF2_selected_annual]
          AF2_annual_funding <- sum(AF2_annual_values) * AF2_area
        }
        
        AF2_annual_funding <- AF2_annual_funding + input$annual_external_support_c * AF2_area
        AF2_total_annual_funding_c <- AF2_annual_funding
      }
    }
    
    # ---- Store Results in Data Frame ----
    funding_parameters <- c(
      "AF1_total_annual_funding_c", "AF2_total_annual_funding_c",
      "AF1_total_one_time_funding_c", "AF2_total_one_time_funding_c",
      "AF2_percentage_values_c", "AF1_percentage_values_c",
      "selected_percentage_c")
    selected_values_funding <- c(
      AF1_total_annual_funding_c, AF2_total_annual_funding_c,
      AF1_total_one_time_funding_c, AF2_total_one_time_funding_c,
      AF2_percentage_values_c, AF1_percentage_values_c,
      selected_percentage_c)
    funding_variables_df <- data.frame(
      variable = funding_parameters,
      lower = selected_values_funding,
      upper = selected_values_funding,
      distribution = rep("const", length(funding_parameters)),
      stringsAsFactors = FALSE
    )
    #message("Funding Variables Updated: ")
    #print(funding_variables_df)
    return(funding_variables_df)
  })
  #### End of funding estimates ####
  
  ######## Reactive UI data #####
  ui_estimates <- reactive({
    #message("Accessing user input estimates from te interface...")
    exclude_inputs <- c("collapseSidebar", "save", "load", "delete", "confirm_delete", 
                        "admin_selected_user", "project_name", "version_select", "delete_version_select")
    
    variables <- setdiff(names(input)[grepl("(_c$|_p$|_t$)", names(input))], exclude_inputs)
    req(variables) # Ensure variables exist
    
    lower_values <- sapply(variables, function(var) {
      value <- input[[var]]
      if (length(value) == 1) as.numeric(value) else as.numeric(value[1])
    })
    upper_values <- sapply(variables, function(var) {
      value <- input[[var]]
      if (length(value) == 1) as.numeric(value) else as.numeric(value[2])
    })
    distributions <- sapply(variables, function(var) {
      if (grepl("_c$", var)) "const" else if (grepl("_p$", var)) "posnorm" else "tnorm_0_1"
    })
    data.frame(
      variable = variables,
      lower = lower_values,
      upper = upper_values,
      distribution = distributions,
      stringsAsFactors = FALSE)
  })
  # Observing ui_estimates
  observe({
    req(ui_estimates()) # Validate reactive
    estimates <- ui_estimates()
  })
  
  # Merge dynamic data from the UI for funding, animals, crops and others with manual data from CSV
  input_estimates <- reactive({
    df1 <- ui_estimates()  # get ui variables
    df2 <- manual_variables()  # get analyst variables
    df3 <- animal_estimates() # get animal variables
    df4 <- crop_estimates() # get crop variables
    df5 <- funding_variables() # get funding variables
    
    req(df1, df2, df3, df4, df5)  # Ensure they are not NULL
    # print(names(df1))
    # print(names(df2))
    # print(names(df3))
    # print(names(df4))
    # print(names(df5))
    df_combined <- rbind(df1, df3, df4, df5, df2)
    # Convert to numeric where needed
    df_combined$lower <- as.numeric(df_combined$lower)
    df_combined$upper <- as.numeric(df_combined$upper)
    
    if (any(is.na(df_combined$lower)) || any(is.na(df_combined$upper))) {
      stop("Error: NA values found in 'lower' or 'upper' columns.")
    }
    # write.csv(df_combined, "my_data.csv", row.names = FALSE, fileEncoding = "UTF-8")  
    # #manual_data <- read.csv(csv_path, stringsAsFactors = FALSE)
    # input_file <- read.csv("my_data.csv", stringsAsFactors = FALSE)
    #message("All input estimates successfully generated.")
    #print(as.estimate(df_combined))
    rownames(df_combined) <- 1:nrow(df_combined)
    df_combined <- df_combined[df_combined[1] != "0",] ### remove line with "0; 0; const" variable
    saveRDS(df_combined,"data.rds")
    return(df_combined)
  })
  ### End of gathering all inputs ####
  
  # Display validation message
  # output$validation_message <- renderText({
  #   validate_input()
  # })
  # output$display_version_1 <- renderText({paste("This is version:", input$version)})
  # Open the URL in a browser when the button is clicked
  # observeEvent(input$open_url, {
  #   url <- "https://www.gov.uk/government/publications/sustainable-farming-incentive-scheme-expanded-offer-for-2024"
  #   utils::browseURL(url) })
  
  #DA model ####
  
  # Assign input table to object to easily use following "make_variables"-function
  
  #input_file <- read.csv(file.path(getwd(), "my_data.csv")) 
  
  #Use make_variables function to test chunks of the function code (AF_benefit) during the development process
  
  # make_variables <- function(est,n=1)
  # { x<-random(rho=est, n=n)
  # for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)
  # }
  # generated_variables <- make_variables(as.estimate(input_estimates())) #input_file    Works by randomly selecting values from each variable of the input table and storing them in the global environment.These fixed values are not the ones used later in the Monte Carlo simulation but serve the sole purpose of allowing to run parts of the model and thereby testing is part for part
  
  # Create the function for mcSimualtion ####
  # This functions contains all the mathematical formulations for all three systems and their components
  AF_benefit <- function(x, varnames) {
    # message("Starting DA function...")
    Get_crop_indices <- function(Crop_rotation, Crop_name, n_years_c) {
      Rotation_length <- length(Crop_rotation)
      Full_rotation <- rep(Crop_rotation, length.out = n_years_c)
      
      # Find positions where the specified crop occurs
      Crop_indices <- which(Full_rotation == Crop_name)
      return(Crop_indices)
    }
    #Variables, which are important for multiple crop rotation/AF-scenarios
    #Initial creation of the basic variables happens here, before conditional functions,
    #crop indices etc. are then added in the conditional functions, based on the scenario modelled
    Arable_area_AF1 <- arable_area_c - AF1_tree_row_area_c
    Arable_area_AF2 <- arable_area_c - AF2_tree_row_area_c
    
    Herbal_ley_yield <- rep(0, n_years_c)
    Herbal_ley_yield[1:n_years_c] <-vv(herbal_ley_yield_p, var_CV_p, length(n_years_c)) *  arable_area_c #[t]
    
    Winter_wheat_yield <- rep(0, n_years_c)
    Winter_wheat_yield[1:n_years_c] <-vv(winter_wheat_yield_p, var_CV_p, length(n_years_c)) * arable_area_c
    
    Winter_cover_crop_yield <- rep(0, n_years_c)
    Winter_cover_crop_yield[1:n_years_c] <- vv(winter_cover_crop_yield_p, var_CV_p, length(n_years_c)) * arable_area_c #[t]
    
    #Costs
    Labour_costs <- rep(0, n_years_c) 
    Labour_costs <- vv(labour_costs_p, var_CV = var_CV_p, n_years_c) #[EURO/h]
    
    Herbal_ley_management_cost <- rep(0, n_years_c)
    Herbal_ley_management_cost[1:n_years_c] <- vv(herbal_ley_management_p, var_CV = var_CV_p, length(n_years_c))*arable_area_c
    
    Winter_wheat_management_cost <- rep(0, n_years_c)
    Winter_wheat_management_cost[1:n_years_c] <- vv(winter_wheat_management_p, var_CV = var_CV_p, length(n_years_c))*arable_area_c
    
    Herbal_ley_labour_cost <- rep(0, n_years_c)
    Herbal_ley_labour_cost[1:n_years_c] <- vv(herbal_ley_labour_p, var_CV = var_CV_p, length(n_years_c))* arable_area_c * Labour_costs
    
    Herbal_ley_grazing_labour_cost <- rep(0, n_years_c)
    Herbal_ley_grazing_labour_cost[1:n_years_c] <- vv(herbal_grazing_labour_p, var_CV_p, length(n_years_c))* arable_area_c * Labour_costs
    
    Winter_wheat_labour_cost <- rep(0, n_years_c)
    Winter_wheat_labour_cost[1:n_years_c] <- vv(winter_wheat_labour_p, var_CV = var_CV_p, length(n_years_c))* arable_area_c * Labour_costs
    
    #Livestock metrics: 
    Daily_dry_matter_intake_cattle <- daily_dry_matter_intake_cattle_p # assigning it so that the same value is used for all three systems 
    Daily_dry_matter_intake_sheep <- daily_dry_matter_intake_sheep_p
    Daily_weight_gain_pasture_cattle <- daily_weight_gain_pasture_cattle_p
    Daily_weight_gain_pasture_sheep <- daily_weight_gain_pasture_sheep_p
    
    # Convert fresh matter to dry matter
    Herbal_ley_dry_matter <- Herbal_ley_yield  #[tDM]
    Available_dry_matter_herbal_ley <- Herbal_ley_dry_matter * grazing_efficiency_p #[tDM]
    
    # Herbal ley cattle estimate live weight gain per hectare based grazing efficiency, daily DM intake
    Days_of_grazing_herbal_ley_cattle <- (Available_dry_matter_herbal_ley * t_cattle_intensity_c)/Daily_dry_matter_intake_cattle #[d] #produces values over 365. Days of grazing means, how many days ONE cow could graze on the amount to forage available
    Total_saleable_beef_yield_herbal_ley <- Days_of_grazing_herbal_ley_cattle * Daily_weight_gain_pasture_cattle #[t]
    
    # Herbal ley sheep estimate live weight gain per hectare based grazing efficiency, daily DM intake
    Days_of_grazing_herbal_ley_sheep <- (Available_dry_matter_herbal_ley * t_sheep_intensity_c)/Daily_dry_matter_intake_sheep #[d] #produces values over 365. Days of grazing means, how many days ONE cow could graze on the amount to forage available
    Total_saleable_lamb_yield_herbal_ley <- Days_of_grazing_herbal_ley_sheep * Daily_weight_gain_pasture_sheep #[t]
    
    # Winter crop fresh matter to dry matter
    Winter_cover_crop_dry_matter <- Winter_cover_crop_yield * winter_cover_crop_dry_matter_p  # [t]
    
    # Winter crop cattle estimate live weight gain per hectare based grazing efficiency, daily DM intake
    Days_of_grazing_winter_cover_crop_cattle <- (Winter_cover_crop_dry_matter * t_cattle_intensity_c)/Daily_dry_matter_intake_cattle
    Winter_cover_crop_beef_yield <- Days_of_grazing_winter_cover_crop_cattle * Daily_weight_gain_pasture_cattle #[t]
    
    # Winter crop sheep estimate live weight gain per hectare based grazing efficiency, daily DM intake
    Days_of_grazing_winter_cover_crop_sheep <- (Winter_cover_crop_dry_matter * t_sheep_intensity_c)/Daily_dry_matter_intake_sheep
    Winter_cover_crop_lamb_yield <- Days_of_grazing_winter_cover_crop_sheep * Daily_weight_gain_pasture_sheep #[t]
    
    #Crop indices
    #CR1:
    Herbal_ley_indices1 <- Get_crop_indices(treeless_system_crop_rotation_1_c, "Herbal_ley", n_years_c)
    Winter_wheat_indices1 <- Get_crop_indices(treeless_system_crop_rotation_1_c, "Winter_wheat", n_years_c)
    
    #CR2:
    Herbal_ley_indices2 <- Get_crop_indices(treeless_system_crop_rotation_2_c, "Herbal_ley", n_years_c)
    Winter_wheat_indices2 <- Get_crop_indices(treeless_system_crop_rotation_2_c, "Winter_wheat", n_years_c)
    Spring_barley_indices <- Get_crop_indices(treeless_system_crop_rotation_2_c, "Spring_barley", n_years_c)
    Summer_beans_indices <- Get_crop_indices(treeless_system_crop_rotation_2_c, "Summer_beans", n_years_c)
    Winter_oats_indices <- Get_crop_indices(treeless_system_crop_rotation_2_c, "Winter_oats", n_years_c)
    
    Winter_cover_crop_indices <- sort(c(Winter_wheat_indices2, Spring_barley_indices))
    
    #Winter Crop
    Winter_cover_crop_yield[Winter_cover_crop_indices] <- vv(winter_cover_crop_yield_p, var_CV_p, length(Winter_cover_crop_indices)) *arable_area_c
    
    #Labour costs
    Herbal_ley_labour_cost_CR1 <- rep(0, n_years_c)
    Winter_wheat_labour_cost_CR1 <- rep(0, n_years_c)
    Herbal_ley_grazing_labour_cost_CR1 <- rep(0, n_years_c)
    
    #Treeless system ###############################################################
    #Crop rotation 1:#####----------------------------------------------------------
    #Benefits:
    if(treeless_system_crop_rotation_1_c ==1){  
      Herbal_ley_yield_CR1 <- rep(0, n_years_c)
      Herbal_ley_benefit_CR1 <- rep(0, n_years_c)
      Winter_wheat_yield_CR1 <- rep(0, n_years_c)
      
      #Crop rotation in AF system
      Herbal_ley_yield_CR1[Herbal_ley_indices1] <- Herbal_ley_yield[Herbal_ley_indices1]
      Winter_wheat_yield_CR1[Winter_wheat_indices1] <- Winter_wheat_yield[Winter_wheat_indices1] * herbal_effect_p #herbal ley fertilization effect increases wheat yield 
      
      #Benefits: 
      if (treeless_include_animals_c == 0) {
        Herbal_ley_benefit_CR1 <-  vv(herbal_ley_value_p, var_CV_p, n_years_c) * Herbal_ley_yield_CR1
      }
      if (treeless_include_animals_c == 1) { 
        Herbal_ley_benefit_CR1[Herbal_ley_indices1] <- (Total_saleable_beef_yield_herbal_ley[Herbal_ley_indices1] * beef_value_p*1000) + 
          (Total_saleable_lamb_yield_herbal_ley[Herbal_ley_indices1] * lamb_value_p * 1000)
      }
      Winter_wheat_benefit_CR1 <- vv(winter_wheat_value_p, var_CV_p, n_years_c) * Winter_wheat_yield_CR1
      
      Total_benefit_treeless_CR1 <- Herbal_ley_benefit_CR1 + Winter_wheat_benefit_CR1
      
      #Costs:
      #Management costs
      Herbal_ley_management_cost_CR1 <- rep(0, n_years_c) #includes: seed, insurance, fixed+variable machine cost (Values from GER available)
      Winter_wheat_management_cost_CR1 <- rep(0, n_years_c)
      
      Herbal_ley_management_cost_CR1[Herbal_ley_indices1] <- Herbal_ley_management_cost[Herbal_ley_indices1]
      Winter_wheat_management_cost_CR1[Winter_wheat_indices1] <- Winter_wheat_management_cost[Winter_wheat_indices1]
      
      Herbal_ley_labour_cost_CR1[Herbal_ley_indices1] <- Herbal_ley_labour_cost[Herbal_ley_indices1]
      Herbal_ley_grazing_labour_cost_CR1[Herbal_ley_indices1] <- Herbal_ley_grazing_labour_cost[Herbal_ley_indices1]
      Winter_wheat_labour_cost_CR1[Winter_wheat_indices1] <- Winter_wheat_labour_cost[Winter_wheat_indices1]
      
      if(treeless_include_animals_c == 1) {
        Total_herbal_ley_cost_CR1 <- Herbal_ley_management_cost_CR1 + Herbal_ley_labour_cost_CR1 + Herbal_ley_grazing_labour_cost_CR1
      }
      if(treeless_include_animals_c == 0) {
        Total_herbal_ley_cost_CR1 <- Herbal_ley_management_cost_CR1 + Herbal_ley_labour_cost
      }
      Total_winter_wheat_cost_CR1 <- Winter_wheat_management_cost_CR1 + Winter_wheat_labour_cost
      
      Total_cost_treeless_CR1 <- Total_herbal_ley_cost_CR1 + Total_winter_wheat_cost_CR1
      
      #Bottom line treeless system:
      Treeless_bottom_line_benefit <- Total_benefit_treeless_CR1 - Total_cost_treeless_CR1
    }#will only be calculated, if User decides to check the box "Crop Rotation 1"
    
    #Crop_rotation_2:#####----------------------------------------------------------
    if(treeless_system_crop_rotation_2_c ==1){  
      #Treeless system
      Herbal_ley_yield_CR2 <- rep(0, n_years_c)
      Winter_wheat_yield_CR2 <- rep(0, n_years_c)
      Spring_barley_yield <- rep(0, n_years_c)
      Summer_beans_yield <- rep(0, n_years_c)
      Winter_oats_yield <- rep(0, n_years_c)
      Winter_cover_crop_yield <- rep(0, n_years_c)
      Herbal_ley_benefit_CR2 <- rep(0, n_years_c)
      Winter_wheat_benefit_CR2 <- rep(0, n_years_c)
      
      #Crop rotation in treeless system
      Herbal_ley_yield_CR2[Herbal_ley_indices2] <- Herbal_ley_yield[Herbal_ley_indices2]
      Herbal_ley_benefit_CR2 <- vv(herbal_ley_value_p, var_CV_p, n_years_c) * Herbal_ley_yield_CR2
      
      Winter_wheat_yield_CR2[Winter_wheat_indices2] <- Winter_wheat_yield[Winter_wheat_indices2] * herbal_effect_p #herbal ley fertilization effect increases wheat yield 
      Winter_wheat_benefit_CR2 <- vv(winter_wheat_value_p, var_CV_p, n_years_c) * Winter_wheat_yield_CR2
      
      Spring_barley_yield[Spring_barley_indices] <-
        vv(spring_barley_yield_p, var_CV_p, length(Spring_barley_indices)) *  arable_area_c * winter_grazing_effect_p
      Spring_barley_benefit <- vv(spring_barley_value_p, var_CV_p, n_years_c) * Spring_barley_yield
      
      Summer_beans_yield[Summer_beans_indices] <- 
        vv(summer_beans_yield_p, var_CV_p, length(Summer_beans_indices)) *  arable_area_c * winter_grazing_effect_p
      Summer_beans_benefit <- vv(summer_beans_value_p, var_CV_p, n_years_c) * Summer_beans_yield
      
      Winter_oats_yield[Winter_oats_indices] <- 
        vv(winter_oats_yield_p, var_CV_p, length(Winter_oats_indices)) *  arable_area_c * summer_beans_effect_p 
      Winter_oat_benefit <- vv(winter_oats_value_p, var_CV_p, n_years_c) * Winter_oats_yield 
      
      Winter_cover_crop_yield[Winter_cover_crop_indices] <- vv(winter_cover_crop_yield_p, var_CV_p, length(Winter_cover_crop_indices)) *arable_area_c
      
      #Livestock metrics:
      Herbal_ley_livestock_yield_CR2 <- rep(0, n_years_c)
      Herbal_ley_livestock_yield_CR2[Herbal_ley_indices2] <- (Total_saleable_beef_yield_herbal_ley[Herbal_ley_indices2] * beef_value_p * 1000) + 
        (Total_saleable_lamb_yield_herbal_ley[Herbal_ley_indices2] * lamb_value_p * 1000)
      
      Winter_cover_crop_benefit <- rep(0, n_years_c)
      Winter_cover_crop_benefit[Winter_cover_crop_indices] <- (Winter_cover_crop_beef_yield[Winter_cover_crop_indices] * beef_value_p * 1000) + 
        (Winter_cover_crop_lamb_yield[Winter_cover_crop_indices] * lamb_value_p * 1000)
      
      Livestock_benefit_CR2 <- (Herbal_ley_livestock_yield_CR2 + Winter_cover_crop_benefit) 
      
      # Spring_barley_dry_matter <- Spring_barley_yield * barley_dry_matter_content_p # 
      # Spring_barley_beef_yield <- Spring_barley_dry_matter * cattle_feed_conversion_efficiency_p # kg live weight gain/ha
      # 
      # Summer_beans_dry_matter <- Summer_beans_yield * 1000 * beans_dry_matter_content_p_p # kg DM per ha
      # Summer_beans_beef_yield <- Summer_beans_dry_matter * cattle_feed_conversion_efficiency_p # kg live weight gain/ha
      # 
      # Winter_oats_dry_matter <- Winter_oats_yield * 1000 * oats_dry_matter_content_p # kg DM per ha
      # Winter_oats_beef_yield <- Winter_oats_dry_matter * cattle_feed_conversion_efficiency_p # kg live weight gain/ha
      
      if(treeless_include_animals_c == 1){
        Total_benefit_treeless_CR2 <- Livestock_benefit_CR2 + Winter_wheat_benefit_CR2 + + Spring_barley_benefit + Summer_beans_benefit + Winter_oat_benefit
      }
      if(treeless_include_animals_c == 0){
        Total_benefit_treeless_CR2 <- Herbal_ley_benefit_CR2 + Winter_wheat_benefit_CR2 + Spring_barley_benefit + Summer_beans_benefit + Winter_oat_benefit
      }
      
      #Costs:
      #Management costs
      Herbal_ley_management_cost_CR2 <- rep(0, n_years_c) #includes: seed, insurance, fixed+variable machine cost (Values from GER available)
      Winter_wheat_management_cost_CR2 <- rep(0, n_years_c)
      Spring_barley_management_cost <- rep(0, n_years_c)
      Summer_beans_management_cost <- rep(0, n_years_c)
      Winter_oats_management_cost <- rep(0, n_years_c)
      
      Winter_cover_crop_management_cost <- rep(0, n_years_c)
      
      Herbal_ley_management_cost_CR2[Herbal_ley_indices2] <- Herbal_ley_management_cost[Herbal_ley_indices2]
      Winter_wheat_management_cost_CR2[Winter_wheat_indices2] <- Winter_wheat_management_cost[Winter_wheat_indices2]
      Spring_barley_management_cost[Spring_barley_indices] <- vv(spring_barley_management_p, var_CV = var_CV_p, length(Spring_barley_indices))*arable_area_c
      Summer_beans_management_cost[Summer_beans_indices] <- vv(summer_beans_management_p, var_CV = var_CV_p, length(Summer_beans_indices))*arable_area_c
      Winter_oats_management_cost[Winter_oats_indices] <- vv(winter_oats_management_p, var_CV = var_CV_p, length(Winter_oats_indices))*arable_area_c
      
      Winter_cover_crop_management_cost[Winter_cover_crop_indices] <- vv(winter_cover_crop_management_p, var_CV = var_CV_p, length(Winter_cover_crop_indices))*arable_area_c
      
      #Labour costs
      Herbal_ley_labour_cost_CR2 <- rep(0, n_years_c)
      Winter_wheat_labour_cost_CR2 <- rep(0, n_years_c)
      Spring_barley_labour_cost <- rep(0, n_years_c)
      Summer_beans_labour_cost <- rep(0, n_years_c)
      Winter_oats_labour_cost <- rep(0, n_years_c)
      
      Winter_cover_crop_labour_cost <- rep(0, n_years_c)
      
      Herbal_ley_grazing_labour_cost_CR2 <- rep(0, n_years_c)
      Winter_CC_grazing_labour <- rep(0, n_years_c)
      
      Herbal_ley_labour_cost_CR2[Herbal_ley_indices2] <- Herbal_ley_labour_cost[Herbal_ley_indices2]
      Winter_wheat_labour_cost_CR2[Winter_wheat_indices2] <- Winter_wheat_labour_cost[Winter_wheat_indices2]
      Spring_barley_labour_cost[Spring_barley_indices] <- vv(spring_barley_labour_p, var_CV = var_CV_p, length(Spring_barley_indices))*arable_area_c * Labour_costs[Spring_barley_indices]
      Summer_beans_labour_cost[Summer_beans_indices] <- vv(summer_beans_labour_p, var_CV = var_CV_p, length(Summer_beans_indices))*arable_area_c * Labour_costs[Summer_beans_indices]
      Winter_oats_labour_cost[Winter_oats_indices] <- vv(winter_oats_labour_p, var_CV = var_CV_p, length(Winter_oats_indices))*arable_area_c * Labour_costs[Winter_oats_indices]
      
      Winter_cover_crop_labour_cost[Winter_cover_crop_indices] <- vv(winter_cover_crop_labour_p, var_CV = var_CV_p, length(Winter_cover_crop_indices))*arable_area_c * Labour_costs[Winter_cover_crop_indices]
      
      Herbal_ley_grazing_labour_cost_CR2[Herbal_ley_indices2] <- Herbal_ley_grazing_labour_cost[Herbal_ley_indices2]
      Winter_CC_grazing_labour[Winter_cover_crop_indices] <- vv(cover_crop_grazing_labour_p, var_CV_p, length (Winter_cover_crop_indices))*arable_area_c * Labour_costs[Winter_cover_crop_indices]
      
      Total_herbal_ley_cost_CR2 <- Herbal_ley_management_cost_CR2 + Herbal_ley_labour_cost_CR2
      Total_winter_wheat_cost_CR2 <- Winter_wheat_management_cost_CR2 + Winter_wheat_labour_cost_CR2
      Total_spring_barley_cost <- Spring_barley_management_cost + Spring_barley_labour_cost
      Total_summer_beans_cost <- Summer_beans_management_cost + Summer_beans_labour_cost
      Total_winter_oats_cost <- Winter_oats_management_cost + Winter_oats_labour_cost
      
      Total_winter_cover_crop_cost <- Winter_cover_crop_management_cost + Winter_cover_crop_labour_cost
      
      Total_grazing_cost <- Herbal_ley_grazing_labour_cost_CR2 + Winter_CC_grazing_labour
      
      if(treeless_include_animals_c == 1){
        Total_cost_treeless_CR2 <- Total_herbal_ley_cost_CR2 + Total_winter_wheat_cost_CR2 + Total_spring_barley_cost + Total_summer_beans_cost + Total_winter_oats_cost + Total_winter_cover_crop_cost + Total_grazing_cost
      }
      if(treeless_include_animals_c == 0){
        Total_cost_treeless_CR2 <- Total_herbal_ley_cost_CR2 + Total_winter_wheat_cost_CR2 + Total_spring_barley_cost + Total_summer_beans_cost + Total_winter_oats_cost + Total_winter_cover_crop_cost
      }
      #Bottom line treeless system:
      Treeless_bottom_line_benefit <- Total_benefit_treeless_CR2 - Total_cost_treeless_CR2
    }
    
    #AGROFORESTRY SYSTEM 1 - #######################################################
    
    #Implementation cost variables
    AF1_planning_cost <- rep(0, n_years_c) #FE;Invoice of service provider (planners/consultants), planning the AF system + measuring tree strips using GPS[EURO]
    AF1_tree_cost <- rep(0, n_years_c) #FE; Cost per tree [EURO]
    
    AF1_plant_tree_cost <- rep(0, n_years_c) #FE; Labour cost for planting one tree [EURO] -
    AF1_protect_cost <- rep(0, n_years_c) #FE; Material cost of tree protection mesh [EURO]
    AF1_field_prep_cost <- rep(0, n_years_c) #cost for subsoiling and harrowing [EURO/ha]
    AF1_weed_protect_cost <- rep(0, n_years_c) #Material cost of weed suppressing fleece [EURO]
    #AF1_compost_cost <- rep(0, n_years_c) #FE; Cost of compost used during planting [EURO]
    #AF1_irrigation_system_cost <- rep(0, n_years_c) #FE; Material and labour cost of installing a drip irrigation system in the tree rows [EURO]
    #AF1_irrigation_planting_cost <- rep(0, n_years_c) #FE; Cost for watering in newly planted trees [EURO]
    AF1_total_planting_cost <- rep(0, n_years_c)
    AF1_planning_cost[1] <- planning_consulting_p + (farmer_planning_time_p * Labour_costs[1])
    
    #Field prep
    AF1_tree_cost[1] <- SRC_cutting_price_p * AF1_num_trees_c
    AF1_field_prep_cost[1] <- SRC_field_prep_p * AF1_tree_row_area_c * Labour_costs[1]
    AF1_plant_tree_cost[1] <- SRC_machine_rent_p + SRC_planting_p * AF1_tree_row_area_c * Labour_costs[1]
    AF1_protect_cost[1] <- AF1_plant_protection_p * AF1_num_trees_c
    AF1_weed_protect_cost[1] <- weed_protection_p * AF1_tree_row_area_c * Labour_costs[1]
    #AF1_compost_cost[1] <- compost_planting_tree_p * compost_price_p * AF1_num_trees_c
    #AF1_irrigation_system_cost[1] <- irrigation_sys_install_p
    #AF1_irrigation_planting_cost[1] <- irrigation_planting_tree_p * water_price_p * AF1_num_trees_c
    
    AF1_total_planting_cost <- AF1_tree_cost + AF1_field_prep_cost + AF1_plant_tree_cost + AF1_protect_cost + AF1_weed_protect_cost
    AF1_total_investment_cost <- AF1_planning_cost + AF1_total_planting_cost #Investment cost of AF system implementation
    
    #Running cost variables
    AF1_subsidy_application <- rep(0, n_years_c) #FE; Time (regarded as labour cost) spent for application of Eco Scheme subsidy [EURO]
    AF1_annual_irrigation <- rep(0, n_years_c) #FE; Cost of annual irrigation of tree rows [EURO]
    AF1_timber_harvest <- rep(0, n_years_c)
    #create indices for harvest timing 
    harvest_interval_SRC_p <- ifelse(harvest_interval_SRC_p<1,1,harvest_interval_SRC_p)
    Timber_harvest_indices <- round(seq(from = time_to_first_timber_c, to = n_years_c, by = round(harvest_interval_SRC_p)))
    
    AF1_subsidy_application <- vv(subsidy_application_p, var_CV_p, n_years_c) * Labour_costs #application subsidy has to be repeated annually 
    
    #AF1_annual_irrigation[1:3] <- vv(irrigation_123_p, var_CV = var_CV_p, 3)
    #AF1_annual_irrigation[4:n_years_c] <- vv(irrigation_annual, var_CV = var_CV_p, length(4:n_years_c))
    #AF1_annual_irrigation_cost <- AF1_annual_irrigation * water_price_p
    
    AF1_timber_harvest[Timber_harvest_indices] <-
      vv(timber_harvest_p, var_CV = var_CV_p, length(Timber_harvest_indices)) #Labour hours required to harvest SRC
    AF1_timber_harvest_cost <- AF1_timber_harvest * AF1_tree_row_area_c * Labour_costs
    AF1_total_running_cost <- AF1_subsidy_application + AF1_timber_harvest_cost #+ AF1_annual_irrigation_cost
    AF1_total_treerow_cost <- AF1_total_investment_cost + AF1_total_running_cost
    
    # account for yield reduction due to shading and competition from trees 
    AF1_perc_yield_reduction <- gompertz_yield(
      max_harvest = yield_reduc_max_p,
      time_to_first_yield_estimate = time_to_first_reduction_c,
      time_to_second_yield_estimate = time_to_second_reduction_c,
      first_yield_estimate_percent = perc_max_first_reduction_p,
      second_yield_estimate_percent = perc_max_second_reduction_p,
      n_years = n_years_c)
    
    # Calculate timber yield
    AF1_tot_timber_yield <- rep(0, n_years_c)  # Initialize a vector of zeros for each year
    AF1_tot_timber_yield[Timber_harvest_indices] <- AF1_tree_row_area_c * tree_yield_max_p * round(harvest_interval_SRC_p) #linear growth in SRC is assumed
    #annual biomass production (tree_yield_max_p) is multiplied by the number of years between the harvest years (harvest_interval_SRC_p)
    #more accurate growth dynamics would be appreciated
    
    #total benefit calculation
    AF1_biomass_timber_benefit <- 
      vv(biomass_timber_price_p, var_CV_p, n_years_c) * AF1_tot_timber_yield 
    
    #Subsidy in AF system
    #AF1_subsidy[1:n_years_c] <- AF1_subsidy * AF1_tree_row_area_c
    
    # Intangible benefits from ESS like soil erosion control, improved soil quality and biodiversity, change in microclimate, reduced impact of extreme events
    #carbon sequestration in T C/ha/yr
    
    AF1_GW_benefit <- rep(0, n_years_c)
    AF1_erosion_control_benefit <- rep(0, n_years_c)
    # groundwater storage and erosion control not realised in the first few years 
    #- can also make a variable and add vv fn.
    NMES_indices <- seq(from = 5, to = n_years_c)
    AF1_GW_benefit[NMES_indices] <-
      vv(pc_ground_water_recharge_p, var_CV_p, length(NMES_indices)) * arable_area_c
    
    AF1_erosion_control_benefit[NMES_indices] <- (vv(AF1_soil_loss_water_p, var_CV_p, length(NMES_indices)) + vv(AF1_soil_loss_wind_p, var_CV_p, length(NMES_indices))) * 
      vv(pc_soil_loss_p, var_CV_p, length(NMES_indices)) * arable_area_c  
    #pollinator_benefit yet to be added
    
    AF1_Nonmarket_ES_benefit <- AF1_GW_benefit + AF1_erosion_control_benefit 
    # from Porter et al. 2009
    #Nonmarket_ES_benefit <- vv(Nonmarket_ES_value_p, var_CV_p, n_years_c) * tree_row_area
    
    # Funding benefits
    # check if support with percenatge of investment cost is selected by the user
    if (selected_percentage_c == 1) {
      AF1_percentage_funding <- sum(AF1_percentage_values_c * AF1_total_investment_cost)
      AF1_total_one_time_funding <- AF1_total_one_time_funding_c + AF1_percentage_funding
    } else {
      AF1_total_one_time_funding <- AF1_total_one_time_funding_c
    }
    AF1_farm_benefit <- AF1_biomass_timber_benefit + AF1_Nonmarket_ES_benefit + AF1_total_one_time_funding + AF1_total_annual_funding_c
    #AF1_farm_benefit <- AF1_biomass_timber_benefit + AF1_total_one_time_funding + AF1_total_annual_funding_c
    
    #no funding
    AF1_farm_benefit_nofund <- AF1_biomass_timber_benefit + AF1_Nonmarket_ES_benefit
    #AF1_farm_benefit_nofund <- AF1_biomass_timber_benefit
    
    #woody benefit for livestock  
    AF1_total_woody_benefit <- rep(0, n_years_c) 
    AF1_total_woody_benefit <- vv(woody_benefit_shade_p, var_CV_p, n_years_c) + vv(woody_benefit_nutrition_p, var_CV_p, n_years_c)+
      vv(AF1_woody_benefit_windreduc_p, var_CV_p, n_years_c) #used in crop rotation part of code  
    
    #Crop rotation 1:#####----------------------------------------------------------
    if(AF1_system_crop_rotation_1_c == 1) {
      
      #Annual arable crop component
      AF1_herbal_ley_yield_CR1 <- rep(0, n_years_c)
      AF1_winter_wheat_yield_CR1 <- rep(0, n_years_c)
      AF1_herbal_ley_beef_yield <- rep(0, n_years_c)
      
      #Crop rotation in AF system
      AF1_herbal_ley_yield_CR1[Herbal_ley_indices1] <- ((Herbal_ley_yield[Herbal_ley_indices1]/arable_area_c)*Arable_area_AF1) *(1 - AF1_perc_yield_reduction[Herbal_ley_indices1])
      
      # Convert fresh matter to dry matter
      AF1_Herbal_ley_dry_matter_CR1 <- AF1_herbal_ley_yield_CR1 #[tDM]
      AF1_Available_dry_matter_herbal_ley_CR1 <- AF1_Herbal_ley_dry_matter_CR1 * grazing_efficiency_p #[tDM]
      
      # Herbal ley cattle estimate live weight gain per hectare based grazing efficiency, daily DM intake
      Days_of_grazing_herbal_ley_cattle <- (AF1_Available_dry_matter_herbal_ley_CR1 * af1_cattle_intensity_c)/Daily_dry_matter_intake_cattle #[d] #produces values over 365. Days of grazing means, how many days ONE cow could graze on the amount to forage available
      AF1_Total_saleable_beef_yield_herbal_ley_CR1 <- Days_of_grazing_herbal_ley_cattle * Daily_weight_gain_pasture_cattle #[t/ha]
      
      # Herbal ley sheep estimate live weight gain per hectare based grazing efficiency, daily DM intake
      Days_of_grazing_herbal_ley_sheep <- (AF1_Available_dry_matter_herbal_ley_CR1 * af1_sheep_intensity_c)/Daily_dry_matter_intake_sheep #[d] #produces values over 365. Days of grazing means, how many days ONE cow could graze on the amount to forage available
      AF1_Total_saleable_lamb_yield_herbal_ley_CR1 <- Days_of_grazing_herbal_ley_sheep * Daily_weight_gain_pasture_sheep #[t/ha]
      
      #Livestock metrics: 
      # Estimate live weight gain per hectare based on FCE
      AF1_herbal_ley_livestock_yield <- rep(0, n_years_c) 
      AF1_herbal_ley_livestock_yield[Herbal_ley_indices1] <- (AF1_Total_saleable_beef_yield_herbal_ley_CR1[Herbal_ley_indices1] * beef_value_p*1000) + 
        (AF1_Total_saleable_lamb_yield_herbal_ley_CR1[Herbal_ley_indices1] * lamb_value_p*1000)
      
      AF1_winter_wheat_yield_CR1[Winter_wheat_indices1] <- (Winter_wheat_yield[Winter_wheat_indices1]/arable_area_c)*Arable_area_AF1* (1 - AF1_perc_yield_reduction[Winter_wheat_indices1])
      
      #Benefits: 
      if (AF1_include_animals_c == 0) {
        AF1_herbal_ley_benefit_CR1 <-  vv(herbal_ley_value_p, var_CV_p, n_years_c) * AF1_herbal_ley_yield_CR1
      }
      if (AF1_include_animals_c == 1) { 
        AF1_herbal_ley_benefit_CR1 <- AF1_herbal_ley_livestock_yield * (1 + AF1_total_woody_benefit) #AF1_herbal_ley_beef_yield is already in [EURO]
      }
      
      AF1_winter_wheat_benefit_CR1 <- vv(winter_wheat_value_p, var_CV_p, n_years_c) * AF1_winter_wheat_yield_CR1
      AF1_total_benefit_CR1 <- AF1_farm_benefit + AF1_herbal_ley_benefit_CR1 + AF1_winter_wheat_benefit_CR1
      
      #no fund
      AF1_total_benefit_CR1_nofund <- AF1_farm_benefit_nofund + AF1_herbal_ley_benefit_CR1 + AF1_winter_wheat_benefit_CR1
      
      #Costs:
      #Management costs
      AF1_herbal_ley_management_cost <- rep(0, n_years_c) #includes: seed, insurance, fixed+variable machine cost (Values from GER available)
      AF1_winter_wheat_management_cost <- rep(0, n_years_c)
      
      AF1_herbal_ley_management_cost[Herbal_ley_indices1] <- (Herbal_ley_management_cost[Herbal_ley_indices1]/arable_area_c)*Arable_area_AF1
      AF1_winter_wheat_management_cost[Winter_wheat_indices1] <- (Winter_wheat_management_cost[Winter_wheat_indices1]/arable_area_c)*Arable_area_AF1
      
      #Labour costs
      AF1_herbal_ley_labour_cost_CR1 <- rep(0, n_years_c)
      AF1_winter_wheat_labour_cost <- rep(0, n_years_c)
      
      if (AF1_include_animals_c == 0) {
        AF1_herbal_ley_labour_cost_CR1[Herbal_ley_indices1] <- (Herbal_ley_labour_cost[Herbal_ley_indices1]/arable_area_c) * Arable_area_AF1 * af1_added_management_time_factor_p
      }
      if (AF1_include_animals_c == 1) {
        AF1_herbal_ley_labour_cost_CR1[Herbal_ley_indices1] <- (Herbal_ley_grazing_labour_cost_CR1[Herbal_ley_indices1]/arable_area_c)* Arable_area_AF1 * af1_less_grazing_management_time_factor_p
      }
      
      AF1_winter_wheat_labour_cost <- (Winter_wheat_labour_cost_CR1/arable_area_c) * Arable_area_AF1 * af1_added_management_time_factor_p
      
      AF1_total_herbal_ley_cost_CR1 <- AF1_herbal_ley_management_cost + AF1_herbal_ley_labour_cost_CR1
      AF1_winter_wheat_cost_CR1 <- AF1_winter_wheat_management_cost + AF1_winter_wheat_labour_cost
      
      AF1_total_cost_CR1 <- AF1_total_treerow_cost + AF1_total_herbal_ley_cost_CR1 + AF1_winter_wheat_cost_CR1
      
      ##Bottom line AF system 1:
      AF1_bottom_line_benefit <- AF1_total_benefit_CR1 - AF1_total_cost_CR1
      
      #no fund
      AF1_bottom_line_benefit_nofund <- AF1_total_benefit_CR1_nofund - AF1_total_cost_CR1
    }#Will only be calculated if user checks the box "Crop rotation 1" for AF 1
    
    #Crop rotation 2:#####----------------------------------------------------------
    #AF type 1, Crop rotation 2
    if(AF1_system_crop_rotation_2_c == 1) {
      
      #Annual arable crop component
      AF1_herbal_ley_yield_CR2 <- rep(0, n_years_c)
      AF1_Winter_wheat_yield_CR2 <- rep(0, n_years_c)
      AF1_Spring_barley_yield <- rep(0, n_years_c)
      AF1_Summer_beans_yield <- rep(0, n_years_c)
      AF1_Winter_oats_yield <- rep(0, n_years_c)
      AF1_winter_crop_yield <- rep(0, n_years_c)
      
      #Crop rotation 2 in AF system 1
      AF1_herbal_ley_yield_CR2 <- (Herbal_ley_yield/arable_area_c) *  Arable_area_AF1 * (1 - AF1_perc_yield_reduction)
      AF1_herbal_ley_benefit_CR2 <- vv(herbal_ley_value_p, var_CV_p, n_years_c) * AF1_herbal_ley_yield_CR2 
      
      AF1_winter_wheat_yield_CR2 <- (Winter_wheat_yield_CR2/arable_area_c) * Arable_area_AF1 * (1 - AF1_perc_yield_reduction)  
      AF1_winter_wheat_benefit_CR2 <- vv(winter_wheat_value_p, var_CV_p, n_years_c) * AF1_Winter_wheat_yield_CR2
      
      AF1_spring_barley_yield <- (Spring_barley_yield/arable_area_c) * Arable_area_AF1 * (1 - AF1_perc_yield_reduction)
      AF1_spring_barley_benefit <- vv(spring_barley_value_p, var_CV_p, n_years_c) * AF1_Spring_barley_yield
      
      AF1_summer_beans_yield <- (Summer_beans_yield/arable_area_c) * Arable_area_AF1 * (1 - AF1_perc_yield_reduction)
      AF1_summer_beans_benefit <- vv(summer_beans_value_p, var_CV_p, n_years_c) * AF1_summer_beans_yield
      
      AF1_winter_oats_yield <- (Winter_oats_yield/arable_area_c) * Arable_area_AF1 * (1 - AF1_perc_yield_reduction)
      AF1_winter_oat_benefit <- vv(winter_oats_value_p, var_CV_p, n_years_c) * AF1_Winter_oats_yield 
      
      AF1_winter_crop_yield <- (Winter_cover_crop_yield/arable_area_c) *  Arable_area_AF1 * (1 - AF1_perc_yield_reduction)
      
      #Livestock metrics: 
      ### Herbal Ley
      AF1_herbal_ley_dry_matter_CR2 <- AF1_herbal_ley_yield_CR2 # [t]
      
      #Cattle metrics
      AF1_days_of_grazing_herbal_ley_cattle_CR2 <- (AF1_herbal_ley_dry_matter_CR2 * af1_cattle_intensity_c)/Daily_dry_matter_intake_cattle
      AF1_herbal_ley_beef_yield_CR2 <- AF1_days_of_grazing_herbal_ley_cattle_CR2 * Daily_weight_gain_pasture_cattle #[t]
      
      #Sheep metrics
      AF1_days_of_grazing_herbal_ley_sheep_CR2 <- (AF1_herbal_ley_dry_matter_CR2 * af1_sheep_intensity_c)/Daily_dry_matter_intake_sheep
      AF1_herbal_ley_lamb_yield_CR2 <- AF1_days_of_grazing_herbal_ley_sheep_CR2 * Daily_weight_gain_pasture_sheep #[t]
      
      ### Winter Crop
      AF1_winter_crop_dry_matter <- AF1_winter_crop_yield * winter_cover_crop_dry_matter_p  # [t]
      
      #Cattle metrics
      AF1_days_of_grazing_winter_crop_cattle <- (AF1_winter_crop_dry_matter * af1_cattle_intensity_c)/Daily_dry_matter_intake_cattle
      AF1_winter_crop_beef_yield <- AF1_days_of_grazing_winter_crop_cattle * Daily_weight_gain_pasture_cattle #[t]
      
      #Sheep metrics
      AF1_days_of_grazing_winter_crop_sheep <- (AF1_winter_crop_dry_matter * af1_sheep_intensity_c)/Daily_dry_matter_intake_sheep
      AF1_winter_crop_lamb_yield <- AF1_days_of_grazing_winter_crop_sheep * Daily_weight_gain_pasture_sheep #[t]
      
      # AF1_spring_barley_dry_matter <- AF1_spring_barley_yield * 1000 * barley_dry_matter_content_p # kg DM per ha
      # AF1_spring_barley_beef_yield <- AF1_spring_barley_dry_matter * cattle_feed_conversion_efficiency_p  # kg live weight gain/ha
      # 
      # AF1_summer_beans_dry_matter <- AF1_summer_beans_yield * 1000 * beans_dry_matter_content_p # kg DM per ha
      # AF1_summer_beans_beef_yield <- AF1_summer_beans_dry_matter * cattle_feed_conversion_efficiency_p  # kg live weight gain/ha
      # 
      # Af1_winter_oats_dry_matter <- AF1_winter_oats_yield * 1000 * oats_dry_matter_content_p # kg DM per ha
      # AF1_winter_oats_beef_yield <- Af1_winter_oats_dry_matter * cattle_feed_conversion_efficiency_p  # kg live weight gain/ha
      
      AF1_livestock_benefit_CR2 <- ((AF1_herbal_ley_beef_yield_CR2 + AF1_winter_crop_beef_yield) * beef_value_p * 1000 * (1 + AF1_total_woody_benefit)) +
        ((AF1_herbal_ley_lamb_yield_CR2 + AF1_winter_crop_lamb_yield) * lamb_value_p * 1000 * (1 + AF1_total_woody_benefit))
      
      if(AF1_include_animals_c == 1){
        AF1_total_benefit_CR2 <- AF1_farm_benefit + AF1_livestock_benefit_CR2 + AF1_winter_wheat_benefit_CR2 + AF1_spring_barley_benefit + AF1_summer_beans_benefit + AF1_winter_oat_benefit
        
        #no fund
        AF1_total_benefit_CR2_nofund <- AF1_farm_benefit_nofund + AF1_livestock_benefit_CR2 + AF1_winter_wheat_benefit_CR2 + AF1_spring_barley_benefit + AF1_summer_beans_benefit + AF1_winter_oat_benefit
      }
      if(AF1_include_animals_c == 0){
        AF1_total_benefit_CR2 <- AF1_farm_benefit + AF1_herbal_ley_benefit_CR2 + AF1_winter_wheat_benefit_CR2 + AF1_spring_barley_benefit + AF1_summer_beans_benefit + AF1_winter_oat_benefit
        
        #no fund
        AF1_total_benefit_CR2_nofund <- AF1_farm_benefit_nofund + AF1_herbal_ley_benefit_CR2 + AF1_winter_wheat_benefit_CR2 + AF1_spring_barley_benefit + AF1_summer_beans_benefit + AF1_winter_oat_benefit
      }
      
      #Costs:
      #Management costs
      AF1_herbal_ley_management_cost_CR2 <- rep(0, n_years_c) #includes: seed, insurance, fixed+variable machine cost (Values from GER available)
      AF1_winter_wheat_management_cost_CR2 <- rep(0, n_years_c)
      AF1_spring_barley_management_cost <- rep(0, n_years_c)
      AF1_summer_beans_management_cost <- rep(0, n_years_c)
      AF1_winter_oats_management_cost <- rep(0, n_years_c)
      
      AF1_winter_cover_crop_management_cost <- rep(0, n_years_c)
      
      AF1_herbal_ley_management_cost_CR2[Herbal_ley_indices2] <- (Herbal_ley_management_cost_CR2[Herbal_ley_indices2]/arable_area_c)*Arable_area_AF1
      AF1_winter_wheat_management_cost_CR2[Winter_wheat_indices2] <- (Winter_wheat_management_cost_CR2[Winter_wheat_indices2]/arable_area_c)*Arable_area_AF1
      AF1_spring_barley_management_cost <- (Spring_barley_management_cost/arable_area_c)*Arable_area_AF1
      AF1_sumer_beans_management_cost <- (Summer_beans_management_cost/arable_area_c)*Arable_area_AF1
      AF1_winter_oats_management_cost <- (Winter_oats_management_cost/arable_area_c)*Arable_area_AF1
      
      AF1_winter_cover_crop_management_cost <- (Winter_cover_crop_management_cost/arable_area_c)*Arable_area_AF1
      
      #Labour costs
      AF1_herbal_ley_labour_cost_CR2 <- rep(0, n_years_c)
      AF1_herbal_ley_grazing_labour_cost_CR2 <- rep(0, n_years_c)
      AF1_winter_wheat_labour_cost_CR2 <- rep(0, n_years_c)
      AF1_sping_barley_labour_cost <- rep(0, n_years_c)
      AF1_summer_beans_labour_cost <- rep(0, n_years_c)
      AF1_winter_oats_labour_cost <- rep(0, n_years_c)
      
      AF1_winter_cover_crop_labour_cost <- rep(0, n_years_c)
      
      AF1_herbal_ley_labour_cost_CR2[Herbal_ley_indices2] <- (Herbal_ley_labour_cost_CR2[Herbal_ley_indices2]/arable_area_c) * Arable_area_AF1 * af1_added_management_time_factor_p
      AF1_winter_wheat_labour_cost_CR2[Winter_wheat_indices2] <- (Winter_wheat_labour_cost_CR2[Winter_wheat_indices2]/arable_area_c) * Arable_area_AF1 * af1_added_management_time_factor_p
      AF1_spring_barley_labour_cost <- (Spring_barley_labour_cost/arable_area_c) * Arable_area_AF1 * af1_added_management_time_factor_p
      AF1_summer_beans_labour_cost <- (Summer_beans_labour_cost/arable_area_c) * Arable_area_AF1 * af1_added_management_time_factor_p
      AF1_winter_oats_labour_cost <- (Winter_oats_labour_cost/arable_area_c) * Arable_area_AF1 * af1_added_management_time_factor_p
      
      AF1_winter_cover_crop_labour_cost <- (Winter_cover_crop_labour_cost/arable_area_c) * Arable_area_AF1 * af1_added_management_time_factor_p
      
      AF1_herbal_ley_grazing_labour_cost_CR2[Herbal_ley_indices2] <- (Herbal_ley_grazing_labour_cost_CR2[Herbal_ley_indices2]/arable_area_c) * Arable_area_AF1 * af1_less_grazing_management_time_factor_p
      AF1_winter_CC_grazing_labour <- (Winter_CC_grazing_labour/arable_area_c) * Arable_area_AF1 * af1_less_grazing_management_time_factor_p
      
      AF1_total_herbal_ley_cost_CR2 <- AF1_herbal_ley_management_cost_CR2 + AF1_herbal_ley_labour_cost_CR2
      AF1_total_winter_wheat_cost_CR2 <- AF1_winter_wheat_management_cost_CR2 + AF1_winter_wheat_labour_cost_CR2
      AF1_total_spring_barley_cost <- AF1_spring_barley_management_cost + AF1_spring_barley_labour_cost
      AF1_total_summer_beans_cost <- AF1_summer_beans_management_cost + AF1_summer_beans_labour_cost
      AF1_total_winter_oats_cost <- AF1_winter_oats_management_cost + AF1_winter_oats_labour_cost
      
      AF1_total_winter_cover_crop_cost <- AF1_winter_cover_crop_management_cost +  AF1_winter_cover_crop_labour_cost
      
      AF1_total_grazing_cost_CR2 <- AF1_herbal_ley_grazing_labour_cost_CR2 + AF1_winter_CC_grazing_labour
      
      if(AF1_include_animals_c == 1){
        AF1_total_cost_CR2 <- AF1_total_treerow_cost + AF1_total_herbal_ley_cost_CR2 + AF1_total_winter_wheat_cost_CR2 + AF1_total_spring_barley_cost + AF1_total_summer_beans_cost + AF1_total_winter_cover_crop_cost + AF1_total_grazing_cost_CR2
      }
      if(AF1_include_animals_c == 0){
        AF1_total_cost_CR2 <- AF1_total_treerow_cost + AF1_total_herbal_ley_cost_CR2 + AF1_total_winter_wheat_cost_CR2 + AF1_total_spring_barley_cost + AF1_total_summer_beans_cost + AF1_total_winter_cover_crop_cost
      }
      
      #Bottom line AF 1 system:
      AF1_bottom_line_benefit <- AF1_total_benefit_CR2 - AF1_total_cost_CR2
      
      # no fund
      AF1_bottom_line_benefit_nofund <- AF1_total_benefit_CR2_nofund - AF1_total_cost_CR2
    }
    
    #AFGROFORESTRY SYSTEM 2 ########################################################
    
    # account for yield reduction due to shading and competition from trees 
    AF2_perc_yield_reduction <- gompertz_yield(
      max_harvest = yield_reduc_max_p,
      time_to_first_yield_estimate = time_to_first_reduction_c,
      time_to_second_yield_estimate = time_to_second_reduction_c,
      first_yield_estimate_percent = perc_max_first_reduction_p,
      second_yield_estimate_percent = perc_max_second_reduction_p,
      n_years = n_years_c)
    
    # account for yield increase in pasture due to protection from wind stress 
    AF2_perc_yield_increase <- gompertz_yield(
      max_harvest = yield_increase_max_p,
      time_to_first_yield_estimate = time_to_first_increase_c,
      time_to_second_yield_estimate = time_to_second_increase_c,
      first_yield_estimate_percent = perc_max_first_increase_p,
      second_yield_estimate_percent = perc_max_second_increase_p,
      n_years = n_years_c
    )
    #AF costs:
    #Implementation cost variables
    AF2_planning_cost <- rep(0, n_years_c) #FE;Invoice of service provider (planners/consultants), planning the AF system + measuring tree strips using GPS[EURO]
    AF2_shrub_cost <- rep(0, n_years_c) #FE; Cost per tree [EURO]
    AF2_tree_cost <- rep(0, n_years_c) #FE; Cost per tree [EURO]
    AF2_tree_planting <- rep(0, n_years_c) #FE; Labour cost for planting one tree [EURO] -
    AF2_protect_cost <- rep(0, n_years_c) #FE; Material cost of tree protection mesh [EURO]
    AF2_weed_protect_cost <- rep(0, n_years_c) #Material cost of weed suppressing fleece [EURO]
    AF2_compost_cost <- rep(0, n_years_c) #FE; Cost of compost used during planting [EURO]
    AF2_irrigation_system_cost <- rep(0, n_years_c) #FE; Material and labour cost of installing a drip irrigation system in the tree rows [EURO]
    AF2_irrigation_planting_cost <- rep(0, n_years_c) #FE; Cost for watering in newly planted trees [EURO]
    AF2_total_planting_cost <- rep(0, n_years_c)
    
    AF2_planning_cost[1] <-    planning_consulting_p + (farmer_planning_time_p * Labour_costs[1])
    #Field prep
    AF2_tree_planting[1] <- tree_planting_p * AF2_num_trees_c * Labour_costs[1]
    AF2_tree_cost[1] <- (oak_tree_cost_p * num_oak_trees_c) + (birch_tree_cost_p * num_birch_trees_c) + (rowan_tree_cost_p * num_rowan_trees_c) + 
      (hazel_tree_cost_p * num_hazel_trees_c) + (damson_tree_cost_p * num_damson_trees_c) + (bcherry_tree_cost_p * num_bcherry_trees_c)
    AF2_shrub_cost[1] <- shrub_price_p * AF2_num_shrubs_c #this includes all planting costs and labour
    AF2_protect_cost[1] <- AF2_plant_protection_p * (AF2_num_trees_c + AF2_num_shrubs_c)
    AF2_weed_protect_cost[1] <- weed_protection_p * AF2_tree_row_area_c * Labour_costs[1]
    AF2_compost_cost[1] <- (compost_planting_tree_p * compost_price_p * AF2_num_trees_c) + (compost_planting_shrub_p * compost_price_p * AF2_num_shrubs_c) 
    AF2_irrigation_system_cost[1] <- irrigation_sys_install_p
    AF2_irrigation_planting_cost[1] <- (irrigation_planting_tree_p * water_price_p * AF2_num_trees_c) + (irrigation_planting_shrub_p * water_price_p * AF2_num_shrubs_c)  
    
    AF2_total_planting_cost <- AF2_tree_planting + AF2_tree_cost + AF2_shrub_cost + AF2_protect_cost + AF2_weed_protect_cost  +  AF2_compost_cost +
      AF2_irrigation_system_cost + AF2_irrigation_planting_cost
    
    AF2_total_investment_cost <- AF2_planning_cost + AF2_total_planting_cost #Investment cost of AF system implementation
    
    #Running cost variables
    AF2_subsidy_application <- rep(0, n_years_c) #FE; Time (regarded as labour cost) spent for application of Eco Scheme subsidy [EURO]
    #AF2_annual_irrigation <- rep(0, n_years_c) #FE; Cost of annual irrigation of tree rows [EURO]
    
    AF2_rowan_harvest <- rep(0, n_years_c)
    AF2_hazel_harvest <- rep(0, n_years_c)
    AF2_damson_harvest <- rep(0, n_years_c)
    
    
    AF2_subsidy_application <- vv(subsidy_application_p, var_CV_p, n_years_c) * Labour_costs
    #AF2_annual_irrigation[1:3] <- vv(irrigation_123_p, var_CV = var_CV_p, 3)
    #AF2_annual_irrigation_cost <- AF2_annual_irrigation * water_price_p * AF2_num_trees_c
    
    #Woody fruits and nuts benefit needed to calculate harvest costs
    #Rowan
    AF_rowan_yield <- rep(0, n_years_c)
    AF2_rowan_yield <- gompertz_yield(max_harvest = rowan_yield_max_p,
                                      time_to_first_yield_estimate = time_to_first_rowan_c,
                                      time_to_second_yield_estimate = time_to_second_rowan_p,
                                      first_yield_estimate_percent = rowan_yield_first_p,
                                      second_yield_estimate_percent = rowan_yield_second_p,
                                      n_years=n_years_c,
                                      var_CV = var_CV_p,
                                      no_yield_before_first_estimate = TRUE)
    
    #Yield from all rowan fruit trees [kg] considering risks
    AF2_tot_rowan_yield <- AF2_rowan_yield * num_rowan_trees_c #* AF_Chance_perc_weather_fail * AF_chance_perc_crop_fail
    AF2_rowan_benefit <-  AF2_tot_rowan_yield * rowan_value_p 
    
    #Hazelnut
    AF2_hazel_yield <- rep(0, n_years_c)
    AF2_hazel_yield <- gompertz_yield(max_harvest = hazel_yield_max_p,
                                      time_to_first_yield_estimate = time_to_first_hazel_c,
                                      time_to_second_yield_estimate = time_to_second_hazel_p,
                                      first_yield_estimate_percent = hazel_yield_first_p,
                                      second_yield_estimate_percent = hazel_yield_second_p,
                                      n_years=n_years_c,
                                      var_CV = var_CV_p,
                                      no_yield_before_first_estimate = TRUE)
    
    #Yield from all hazelnut fruit trees [kg] considering risks
    AF2_tot_hazel_yield <- AF2_hazel_yield * num_hazel_trees_c #* AF_Chance_perc_weather_fail * AF_chance_perc_crop_fail
    AF2_hazel_benefit <-  AF2_tot_hazel_yield * hazel_value_p 
    
    # Damson tree
    #Yield of one damson tree in the tree row [kg/tree]
    AF2_damson_yield <- rep(0, n_years_c)
    AF2_damson_yield <- gompertz_yield(max_harvest = damson_yield_max_p,
                                       time_to_first_yield_estimate = time_to_first_damson_c,
                                       time_to_second_yield_estimate = time_to_second_damson_p,
                                       first_yield_estimate_percent = damson_yield_first_p,
                                       second_yield_estimate_percent = damson_yield_second_p,
                                       n_years=n_years_c,
                                       var_CV = var_CV_p,
                                       no_yield_before_first_estimate = TRUE)
    
    #Yield from all rowan fruit trees [kg] considering risks
    AF2_tot_damson_yield <- AF2_damson_yield * num_damson_trees_c #* AF_Chance_perc_weather_fail * AF_chance_perc_crop_fail
    AF2_damson_benefit <-  AF2_tot_damson_yield * damson_value_p
    
    # Harvests
    AF2_rowan_harvest[time_to_first_rowan_c:n_years_c] <-
      vv(rowan_harvest_cost_p, var_CV = var_CV_p, length(time_to_first_rowan_c:n_years_c))*AF2_rowan_yield[time_to_first_rowan_c:n_years_c]*Labour_costs[time_to_first_rowan_c:n_years_c]
    AF2_hazel_harvest[time_to_first_hazel_c:n_years_c] <-
      vv(hazel_harvest_cost_p, var_CV = var_CV_p, length(time_to_first_hazel_c:n_years_c))*AF2_hazel_yield[time_to_first_hazel_c:n_years_c]*Labour_costs[time_to_first_hazel_c:n_years_c]
    AF2_damson_harvest[time_to_first_damson_c:n_years_c] <-
      vv(damson_harvest_cost_p, var_CV = var_CV_p, length(time_to_first_damson_c:n_years_c))*AF2_damson_yield[time_to_first_damson_c:n_years_c]*Labour_costs[time_to_first_damson_c:n_years_c]
    
    AF2_total_running_cost <- AF2_subsidy_application + AF2_rowan_harvest + 
      AF2_hazel_harvest + AF2_damson_harvest #+ AF2_annual_irrigation_cost
    
    AF2_total_treerow_cost <- AF2_total_investment_cost + AF2_total_running_cost
    
    #Tree row benefits
    #Woody yield
    #no actual yield, benefit to livestock considered for now in 'livestock component'
    #carbon sequestration in T C/ha/yr
    AF2_C_sequestration <- gompertz_yield(
      max_harvest = C_sequeter_max_p,
      time_to_first_yield_estimate = time_to_first_C_sequester_c,
      time_to_second_yield_estimate = time_to_second_C_sequester_c,
      first_yield_estimate_percent = C_sequester_first_p,
      second_yield_estimate_percent = C_sequester_second_p,
      n_years = n_years_c,
      var_CV = var_CV_p,
      no_yield_before_first_estimate = TRUE
    )
    
    AF2_C_benefit <- vv(C_price_p, var_CV_p, n_years_c) * AF2_C_sequestration * AF2_tree_row_area_c
    #Ecosystem Service benefits 
    
    AF2_GW_benefit <- rep(0, n_years_c)
    AF2_erosion_control_benefit <- rep(0, n_years_c)
    # groundwater storage and erosion control not realised in the first few years 
    #- can also make a variable and add vv fn.
    NMES_indices <- seq(from = 5, to = n_years_c)
    AF2_GW_benefit[NMES_indices] <-
      vv(pc_ground_water_recharge_p, var_CV_p, length(NMES_indices)) * arable_area_c
    
    AF2_erosion_control_benefit[NMES_indices] <- (vv(AF2_soil_loss_water_p, var_CV_p, length(NMES_indices)) + vv(AF2_soil_loss_wind_p, var_CV_p, length(NMES_indices))) * 
      vv(pc_soil_loss_p, var_CV_p, length(NMES_indices)) * arable_area_c  
    #pollinator_benefit yet to be added
    
    AF2_Nonmarket_ES_benefit <- AF2_GW_benefit + AF2_erosion_control_benefit 
    # from Porter et al. 2009
    #Nonmarket_ES_benefit <- vv(Nonmarket_ES_value_p, var_CV_p, n_years_c) * tree_row_area
    
    #Subsidy in AF system
    #AF2_subsidy[1:n_years_c] <- AF2_subsidy * AF2_tree_row_area_c 
    
    # Funding benefits
    # check if support with percentage of investment cost is selected by the user
    if (selected_percentage_c == 1) {
      AF2_percentage_funding <- sum(AF2_percentage_values_c * AF2_total_investment_cost)
      AF2_total_one_time_funding <- AF2_total_one_time_funding_c + AF2_percentage_funding
    } else {
      AF2_total_one_time_funding <- AF2_total_one_time_funding_c
    }
    
    AF2_tree_benefit <- AF2_C_benefit + AF2_rowan_benefit + AF2_hazel_benefit + AF2_damson_benefit + 
      AF2_Nonmarket_ES_benefit + AF2_total_annual_funding_c + AF2_total_one_time_funding
    
    AF2_farm_benefit <- AF2_C_benefit + AF2_rowan_benefit + AF2_hazel_benefit + AF2_damson_benefit + AF2_total_annual_funding_c + AF2_total_one_time_funding #plus woody benefit to livestock in 'crop rotation portion'
    
    #no fund
    AF2_farm_benefit_nofund <- AF2_C_benefit + AF2_rowan_benefit + AF2_hazel_benefit + AF2_damson_benefit #plus woody benefit to livestock in 'crop rotation portion'
    
    
    #woody benefit for livestock  
    AF2_total_woody_benefit <- rep(0, n_years_c) 
    AF2_total_woody_benefit <- vv(woody_benefit_shade_p, var_CV_p, n_years_c) + vv(woody_benefit_nutrition_p, var_CV_p, n_years_c)+
      vv(AF2_woody_benefit_windreduc_p, var_CV_p, n_years_c) #to be used in crop rotation part of code
    
    #Crop rotation 1:#####----------------------------------------------------------
    if(AF2_system_crop_rotation_1_c == 1){
      
      #Annual arable crop component
      AF2_herbal_ley_yield_CR1 <- rep(0, n_years_c)
      AF2_winter_wheat_yield_CR1 <- rep(0, n_years_c)
      
      #Crop rotation in AF system
      AF2_herbal_ley_yield_CR1[Herbal_ley_indices1] <- (Herbal_ley_yield[Herbal_ley_indices1]/arable_area_c)*Arable_area_AF2*(1+AF2_perc_yield_increase[Herbal_ley_indices1]) *
        (1 - AF2_perc_yield_reduction[Herbal_ley_indices1])
      
      #Livestock metrics: 
      # Estimate live weight gain per hectare based on FCE
      AF2_herbal_ley_dry_matter_CR1 <- AF2_herbal_ley_yield_CR1 # [t]
      
      #cattle
      AF2_days_of_grazing_herbal_ley_cattle <- (AF2_herbal_ley_dry_matter_CR1 * af2_cattle_intensity_c)/Daily_dry_matter_intake_cattle
      AF2_herbal_ley_beef_yield_CR1 <-  AF2_days_of_grazing_herbal_ley_cattle * Daily_weight_gain_pasture_cattle #[t]
      
      #sheep
      AF2_days_of_grazing_herbal_ley_sheep <- (AF2_herbal_ley_dry_matter_CR1 * af2_sheep_intensity_c)/Daily_dry_matter_intake_sheep
      AF2_herbal_ley_sheep_yield_CR1 <-  AF2_days_of_grazing_herbal_ley_sheep * Daily_weight_gain_pasture_sheep #[t]
      
      AF2_winter_wheat_yield_CR1[Winter_wheat_indices1] <- (Winter_wheat_yield[Winter_wheat_indices1]/arable_area_c)*Arable_area_AF2* (1 - AF2_perc_yield_reduction[Winter_wheat_indices1])
      
      #Benefits: 
      if (AF2_include_animals_c == 0) {
        AF2_herbal_ley_benefit_CR1 <-  vv(herbal_ley_value_p, var_CV_p, n_years_c) * AF2_herbal_ley_yield_CR1
      }
      if (AF2_include_animals_c == 1) { 
        AF2_herbal_ley_benefit_CR1 <- AF2_herbal_ley_beef_yield_CR1 * beef_value_p * 1000 * (1 + AF2_total_woody_benefit) + 
          AF2_herbal_ley_sheep_yield_CR1 * lamb_value_p * 1000 * (1 + AF2_total_woody_benefit)
      }
      
      AF2_winter_wheat_benefit_CR1 <- vv(winter_wheat_value_p, var_CV_p, n_years_c) * AF2_winter_wheat_yield_CR1
      
      AF2_total_benefit_CR1 <- AF2_farm_benefit + AF2_herbal_ley_benefit_CR1 + AF2_winter_wheat_benefit_CR1
      
      #no fund
      AF2_total_benefit_CR1_nofund <- AF2_farm_benefit_nofund + AF2_herbal_ley_benefit_CR1 + AF2_winter_wheat_benefit_CR1
      
      #Costs:
      #Arable field management costs
      AF2_herbal_ley_management_cost <- rep(0, n_years_c) #includes: seed, insurance, fixed+variable machine cost (Values from GER available)
      AF2_winter_wheat_management_cost <- rep(0, n_years_c)
      
      AF2_herbal_ley_management_cost[Herbal_ley_indices1] <- (Herbal_ley_management_cost[Herbal_ley_indices1]/arable_area_c)*Arable_area_AF2
      AF2_winter_wheat_management_cost[Winter_wheat_indices1] <- (Winter_wheat_management_cost[Winter_wheat_indices1]/arable_area_c)*Arable_area_AF2
      
      #Labour costs
      AF2_herbal_ley_labour_cost_CR1 <- rep(0, n_years_c)
      AF2_winter_wheat_labour_cost <- rep(0, n_years_c)
      
      if (AF2_include_animals_c == 0) {
        AF2_herbal_ley_labour_cost_CR1[Herbal_ley_indices1] <- (Herbal_ley_labour_cost[Herbal_ley_indices1]/arable_area_c) * Arable_area_AF2 * af2_added_management_time_factor_p
      }
      if (AF2_include_animals_c == 1) {
        AF2_herbal_ley_labour_cost_CR1[Herbal_ley_indices1] <- (Herbal_ley_grazing_labour_cost_CR1[Herbal_ley_indices1]/arable_area_c)* Arable_area_AF2 * af2_less_grazing_management_time_factor_p
      }
      
      AF2_winter_wheat_labour_cost[Winter_wheat_indices1] <- (Winter_wheat_labour_cost[Winter_wheat_indices1]/arable_area_c) * Arable_area_AF2 * af2_added_management_time_factor_p
      
      AF2_total_herbal_ley_cost_CR1 <- AF2_herbal_ley_management_cost + AF2_herbal_ley_labour_cost_CR1
      AF2_winter_wheat_cost_CR1 <- AF2_winter_wheat_management_cost + AF2_winter_wheat_labour_cost
      
      AF2_total_cost_CR1 <- AF2_total_treerow_cost + AF2_total_herbal_ley_cost_CR1 + AF2_winter_wheat_cost_CR1
      
      ##Bottom line AF system 1:
      AF2_bottom_line_benefit <- AF2_total_benefit_CR1 - AF2_total_cost_CR1
      
      #no fund
      AF2_bottom_line_benefit_nofund <- AF2_total_benefit_CR1_nofund - AF2_total_cost_CR1
    }
    
    #Crop rotation 2:#####----------------------------------------------------------
    #AF type 2, Crop rotation 2
    if(AF2_system_crop_rotation_2_c == 1){
      
      #Annual arable crop component
      AF2_herbal_ley_yield_CR2 <- rep(0, n_years_c)
      AF2_winter_wheat_yield_CR2 <- rep(0, n_years_c)
      AF2_Spring_barley_yield <- rep(0, n_years_c)
      AF2_Summer_beans_yield <- rep(0, n_years_c)
      AF2_Winter_oats_yield <- rep(0, n_years_c)
      AF2_winter_crop_yield <- rep(0, n_years_c)
      
      #Crop rotation 2 in AF system 1
      AF2_herbal_ley_yield_CR2[Herbal_ley_indices2] <- (Herbal_ley_yield[Herbal_ley_indices2]/arable_area_c) *  Arable_area_AF2 * (1+AF2_perc_yield_increase[Herbal_ley_indices2]) * 
        (1-AF2_perc_yield_reduction[Herbal_ley_indices2])
      AF2_herbal_ley_benefit_CR2 <- vv(herbal_ley_value_p, var_CV_p, n_years_c) * AF2_herbal_ley_yield_CR2 
      
      AF2_winter_wheat_yield_CR2[Winter_wheat_indices2] <- (Winter_wheat_yield[Winter_wheat_indices2]/arable_area_c) * Arable_area_AF2 * (1 - AF2_perc_yield_reduction[Winter_wheat_indices2])  
      AF2_winter_wheat_benefit_CR2 <- vv(winter_wheat_value_p, var_CV_p, n_years_c) * AF2_winter_wheat_yield_CR2
      
      AF2_spring_barley_yield <- (Spring_barley_yield/arable_area_c) * Arable_area_AF2 * (1 - AF2_perc_yield_reduction)
      AF2_spring_barley_benefit <- vv(spring_barley_value_p, var_CV_p, n_years_c) * AF2_Spring_barley_yield
      
      AF2_summer_beans_yield <- (Summer_beans_yield/arable_area_c) * Arable_area_AF2 * (1 - AF2_perc_yield_reduction)
      AF2_summer_beans_benefit <- vv(summer_beans_value_p, var_CV_p, n_years_c) * AF2_summer_beans_yield
      
      AF2_winter_oats_yield <- (Winter_oats_yield/arable_area_c) * Arable_area_AF2 * (1 - AF2_perc_yield_reduction)
      AF2_winter_oat_benefit <- vv(winter_oats_value_p, var_CV_p, n_years_c) * AF2_Winter_oats_yield 
      
      AF2_winter_crop_yield <- (Winter_cover_crop_yield/arable_area_c) *  Arable_area_AF2 * (1 - AF2_perc_yield_reduction)
      
      #Livestock metrics: 
      # Estimate live weight gain per hectare based on FCE
      AF2_herbal_ley_dry_matter_CR2 <- AF2_herbal_ley_yield_CR2 # [t]
      
      #cattle
      AF2_days_of_grazing_herbal_ley_cattle <- (AF2_herbal_ley_yield_CR2 * af2_cattle_intensity_c)/Daily_dry_matter_intake_cattle
      AF2_herbal_ley_beef_yield_CR2 <-  AF2_days_of_grazing_herbal_ley_cattle * Daily_weight_gain_pasture_cattle #[t]
      
      #sheep
      AF2_days_of_grazing_herbal_ley_sheep <- (AF2_herbal_ley_yield_CR2 * af2_sheep_intensity_c)/Daily_dry_matter_intake_sheep
      AF2_herbal_ley_sheep_yield_CR2 <-  AF2_days_of_grazing_herbal_ley_sheep * Daily_weight_gain_pasture_sheep #[t]
      
      ### Winter Crop
      AF2_winter_crop_dry_matter <- AF2_winter_crop_yield * winter_cover_crop_dry_matter_p  # [t]
      
      #Cattle metrics
      AF2_days_of_grazing_winter_crop_cattle <- (AF2_winter_crop_dry_matter * af2_cattle_intensity_c)/Daily_dry_matter_intake_cattle
      AF2_winter_crop_beef_yield <- AF2_days_of_grazing_winter_crop_cattle * Daily_weight_gain_pasture_cattle #[t]
      
      #Sheep metrics
      AF2_days_of_grazing_winter_crop_sheep <- (AF2_winter_crop_dry_matter * af2_sheep_intensity_c)/Daily_dry_matter_intake_sheep
      AF2_winter_crop_sheep_yield <- AF2_days_of_grazing_winter_crop_sheep * Daily_weight_gain_pasture_sheep #[t]
      
      # AF2_spring_barley_dry_matter <- AF2_spring_barley_yield * 1000 * barley_dry_matter_content_p # kg DM per ha
      # AF2_spring_barley_beef_yield <- AF2_spring_barley_dry_matter * cattle_feed_conversion_efficiency_p # kg live weight gain/ha
      # 
      # AF2_summer_beans_dry_matter <- AF2_summer_beans_yield * 1000 * beans_dry_matter_content_p # kg DM per ha
      # AF2_summer_beans_beef_yield <- AF2_summer_beans_dry_matter * cattle_feed_conversion_efficiency_p # kg live weight gain/ha
      # 
      # AF2_winter_oats_dry_matter <- AF2_winter_oats_yield * 1000 * oats_dry_matter_content_p # kg DM per ha
      # AF2_winter_oats_beef_yield <- AF2_winter_oats_dry_matter * cattle_feed_conversion_efficiency_p # kg live weight gain/ha
      
      AF2_livestock_benefit_CR2 <- (AF2_herbal_ley_beef_yield_CR2 + AF2_winter_crop_beef_yield) * beef_value_p * 1000 * (1 + AF2_total_woody_benefit) + 
        (AF2_herbal_ley_sheep_yield_CR2 + AF2_winter_crop_sheep_yield) * lamb_value_p * 1000 * (1 + AF2_total_woody_benefit)  #+ AF2_spring_barley_beef_yield + AF2_summer_beans_beef_yield + AF2_winter_oats_beef_yield) 
      
      if(AF2_include_animals_c == 1){
        AF2_total_benefit_CR2 <- AF2_farm_benefit + AF2_livestock_benefit_CR2 + AF2_winter_wheat_benefit_CR2 + AF2_spring_barley_benefit + AF2_summer_beans_benefit + AF2_winter_oat_benefit
        # no fund
        AF2_total_benefit_CR2_nofund <- AF2_farm_benefit_nofund + AF2_livestock_benefit_CR2 + AF2_winter_wheat_benefit_CR2 + AF2_spring_barley_benefit + AF2_summer_beans_benefit + AF2_winter_oat_benefit
      }
      if(AF2_include_animals_c == 0){
        AF2_total_benefit_CR2 <- AF2_farm_benefit + AF2_herbal_ley_benefit_CR2 + AF2_winter_wheat_benefit_CR2 + AF2_spring_barley_benefit + AF2_summer_beans_benefit + AF2_winter_oat_benefit
        #no fund
        AF2_total_benefit_CR2_nofund <- AF2_farm_benefit_nofund + AF2_herbal_ley_benefit_CR2 + AF2_winter_wheat_benefit_CR2 + AF2_spring_barley_benefit + AF2_summer_beans_benefit + AF2_winter_oat_benefit
      }
      
      #Costs:
      #Management costs
      AF2_herbal_ley_management_cost_CR2 <- rep(0, n_years_c) #includes: seed, insurance, fixed+variable machine cost (Values from GER available)
      AF2_winter_wheat_management_cost_CR2 <- rep(0, n_years_c)
      AF2_spring_barley_management_cost <- rep(0, n_years_c)
      AF2_summer_beans_management_cost <- rep(0, n_years_c)
      AF2_winter_oats_management_cost <- rep(0, n_years_c)
      
      AF2_winter_cover_crop_management_cost <- rep(0, n_years_c)
      
      AF2_herbal_ley_management_cost_CR2[Herbal_ley_indices2] <- (Herbal_ley_management_cost[Herbal_ley_indices2]/arable_area_c)*Arable_area_AF2
      AF2_winter_wheat_management_cost_CR2[Winter_wheat_indices2] <- (Winter_wheat_management_cost[Winter_wheat_indices2]/arable_area_c)*Arable_area_AF2
      AF2_spring_barley_management_cost <- (Spring_barley_management_cost/arable_area_c)*Arable_area_AF2
      AF2_sumer_beans_management_cost <- (Summer_beans_management_cost/arable_area_c)*Arable_area_AF2
      AF2_winter_oats_management_cost <- (Winter_oats_management_cost/arable_area_c)*Arable_area_AF2
      
      AF2_winter_cover_crop_management_cost <- (Winter_cover_crop_management_cost/arable_area_c)*Arable_area_AF2
      
      #Labour costs
      AF2_herbal_ley_labour_cost_CR2 <- rep(0, n_years_c)
      AF2_winter_wheat_labour_cost_CR2 <- rep(0, n_years_c)
      AF2_sping_barley_labour_cost <- rep(0, n_years_c)
      AF2_summer_beans_labour_cost <- rep(0, n_years_c)
      AF2_winter_oats_labour_cost <- rep(0, n_years_c)
      
      AF2_winter_cover_crop_labour_cost <- rep(0, n_years_c)
      
      AF2_herbal_ley_labour_cost_CR2[Herbal_ley_indices2] <- (Herbal_ley_labour_cost[Herbal_ley_indices2]/arable_area_c) * Arable_area_AF2 * af2_added_management_time_factor_p
      AF2_winter_wheat_labour_cost_CR2[Winter_wheat_indices2] <- (Winter_wheat_labour_cost[Winter_wheat_indices2]/arable_area_c) * Arable_area_AF2 * af2_added_management_time_factor_p
      AF2_spring_barley_labour_cost <- (Spring_barley_labour_cost/arable_area_c) * Arable_area_AF2 * af2_added_management_time_factor_p
      AF2_summer_beans_labour_cost <- (Summer_beans_labour_cost/arable_area_c) * Arable_area_AF2 * af2_added_management_time_factor_p
      AF2_winter_oats_labour_cost <- (Winter_oats_labour_cost/arable_area_c) * Arable_area_AF2 * af2_added_management_time_factor_p
      
      AF2_winter_cover_crop_labour_cost <- (Winter_cover_crop_labour_cost/arable_area_c) * Arable_area_AF2 * af2_added_management_time_factor_p
      
      AF2_herbal_ley_grazing_labour_cost_CR2 <- (Herbal_ley_grazing_labour_cost_CR2/arable_area_c) * Arable_area_AF2 * af2_less_grazing_management_time_factor_p
      AF2_winter_CC_grazing_labour <- (Winter_CC_grazing_labour/arable_area_c) * Arable_area_AF2 * af2_less_grazing_management_time_factor_p
      
      AF2_total_herbal_ley_cost_CR2 <- AF2_herbal_ley_management_cost_CR2 + AF2_herbal_ley_labour_cost_CR2
      AF2_total_winter_wheat_cost_CR2 <- AF2_winter_wheat_management_cost_CR2 + AF2_winter_wheat_labour_cost_CR2
      AF2_total_spring_barley_cost <- AF2_spring_barley_management_cost + AF2_spring_barley_labour_cost
      AF2_total_summer_beans_cost <- AF2_summer_beans_management_cost + AF2_summer_beans_labour_cost
      AF2_total_winter_oats_cost <- AF2_winter_oats_management_cost + AF2_winter_oats_labour_cost
      
      AF2_total_winter_cover_crop_cost <- AF2_winter_cover_crop_management_cost +  AF2_winter_cover_crop_labour_cost
      
      AF2_total_grazing_cost_CR2 <- AF2_herbal_ley_grazing_labour_cost_CR2 + AF2_winter_CC_grazing_labour
      
      if(AF2_include_animals_c == 1){
        AF2_total_cost_CR2 <- AF2_total_treerow_cost + AF2_total_herbal_ley_cost_CR2 + AF2_total_winter_wheat_cost_CR2 + AF2_total_spring_barley_cost + AF2_total_summer_beans_cost + AF2_total_winter_cover_crop_cost + AF2_total_grazing_cost_CR2
      }
      if(AF2_include_animals_c == 0){
        AF2_total_cost_CR2 <- AF2_total_treerow_cost + AF2_total_herbal_ley_cost_CR2 + AF2_total_winter_wheat_cost_CR2 + AF2_total_spring_barley_cost + AF2_total_summer_beans_cost + AF2_total_winter_cover_crop_cost
      }
      
      #Bottom line AF 2 system:
      AF2_bottom_line_benefit <- AF2_total_benefit_CR2 - AF2_total_cost_CR2
      
      #no fund
      AF2_bottom_line_benefit_nofund <- AF2_total_benefit_CR2_nofund - AF2_total_cost_CR2
    }
    
    #Calculating NPVs and Cash Flows####
    #Treeless system
    #Treeless system bottomline####
    
    NPV_treeless_system <- discount(Treeless_bottom_line_benefit, discount_rate = discount_rate_p,
                                    calculate_NPV = TRUE) #NVP of monoculture arable system 
    Treeless_cash_flow <- discount(Treeless_bottom_line_benefit, discount_rate = discount_rate_p,
                                   calculate_NPV = FALSE) #Cash flow of monoculture system
    Treeless_cum_cash_flow <- cumsum(Treeless_cash_flow) #Cumulative cash flow of monoculture system
    
    #AF System 1
    AF1_NPV <- discount(AF1_bottom_line_benefit, discount_rate=discount_rate_p,
                        calculate_NPV = TRUE)#NVP of AF system
    AF1_cash_flow <- discount(AF1_bottom_line_benefit,discount_rate=discount_rate_p,
                              calculate_NPV = FALSE)#Cash flow of AF system
    AF1_cum_cash_flow <- cumsum(AF1_cash_flow) #Cumulative cash flow of AF system
    
    #no fund
    AF1_NPV_nofund <- discount(AF1_bottom_line_benefit_nofund, discount_rate=discount_rate_p,
                               calculate_NPV = TRUE)#NVP of AF system
    
    #AF System 2
    AF2_NPV <- discount(AF2_bottom_line_benefit, discount_rate=discount_rate_p,
                        calculate_NPV = TRUE)#NVP of AF system
    AF2_cash_flow <- discount(AF2_bottom_line_benefit,discount_rate=discount_rate_p,
                              calculate_NPV = FALSE)#Cash flow of AF system
    AF2_cum_cash_flow <- cumsum(AF2_cash_flow) #Cumulative cash flow of AF system
    
    #no fund
    AF2_NPV_nofund <- discount(AF2_bottom_line_benefit_nofund, discount_rate=discount_rate_p,
                               calculate_NPV = TRUE)#NVP of AF system
    
    #Tradeoff (difference between AF system and treeless system)
    AF1_tradeoff_benefit <- AF1_bottom_line_benefit - Treeless_bottom_line_benefit
    #Tradeoff_benefit_farm <- AF_bottom_line_benefit_farm - Treeless_bottom_line_benefit
    
    AF1_NPV_tradeoff <- discount(AF1_tradeoff_benefit, discount_rate = discount_rate_p,
                                 calculate_NPV = TRUE )
    AF1_tradeoff_benefit_nofund <- AF1_bottom_line_benefit_nofund - Treeless_bottom_line_benefit
    AF1_NPV_tradeoff_nofund <- discount(AF1_tradeoff_benefit_nofund, discount_rate = discount_rate_p,
                                        calculate_NPV = TRUE )
    
    # NPV_tradeoff_farm <- discount(Tradeoff_benefit_farm, discount_rate = discount_rate_p,
    #                               calculate_NPV = TRUE )
    
    AF2_tradeoff_benefit <- AF2_bottom_line_benefit - Treeless_bottom_line_benefit
    AF2_NPV_tradeoff <- discount(AF2_tradeoff_benefit, discount_rate = discount_rate_p,
                                 calculate_NPV = TRUE )
    
    AF2_tradeoff_benefit_nofund <- AF2_bottom_line_benefit_nofund - Treeless_bottom_line_benefit
    AF2_NPV_tradeoff_nofund <- discount(AF2_tradeoff_benefit_nofund, discount_rate = discount_rate_p,
                                        calculate_NPV = TRUE )
    
    
    #Defining what output variables the following Monte Carlo Simulation should create #####
    return(list(
      NPV_Agroforestry_System1 = AF1_NPV,
      NPV_Agroforestry_System2 = AF2_NPV, 
      NPV_Treeless_System = NPV_treeless_system,
      NPVtrade_off_AF1 = AF1_NPV_tradeoff,
      NPVtrade_off_AF2 = AF2_NPV_tradeoff,
      
      # NPV_nofund_Agroforestry_System1 = AF1_NPV_nofund,
      # NPV_nofund_Agroforestry_System2 = AF2_NPV_nofund,
      NPVtrade_off_nofund_AF1 = AF1_NPV_tradeoff_nofund,
      NPVtrade_off_nofund_AF2 = AF2_NPV_tradeoff_nofund,
      
      Cash_flow_AF1 = AF1_cash_flow,
      Cash_flow_AF2 = AF2_cash_flow,
      Cumulative_cashflow_AF1 = AF1_cum_cash_flow,
      Cumulative_cashflow_AF2 = AF2_cum_cash_flow,
      Cashflow_treeless = Treeless_cash_flow,
      Cumcashflow_treeless = Treeless_cum_cash_flow
    ))
  }
  #-------------------------------------------------------------------------------
  #Run the Monte Carlo analysis of the model
  mcSimulation_results <- eventReactive(input$run_simulation, {
    req(input_estimates())
    
    # # Debugging: Print structure of input estimates
    # print("Debug: Structure of input_estimates")
    # print(str(input_estimates()))
    # 
    # # Debugging: Ensure as.estimate() is returning valid data
    estimate_data <- as.estimate(input_estimates())
    # print("Debug: Structure of as.estimate(input_estimates())")
    # print(str(estimate_data))
    # 
    # # Debugging: Ensure AF_benefit function is valid
    # print("Debug: Structure of AF_benefit function")
    # print(str(AF_benefit))
    # 
    # # Debugging: Print number of simulations
    # print("Debug: num_simulations_c")
    # print(str(input$num_simulations_c))
    # 
    # Run Monte Carlo simulation
    data <- 
      mcSimulation(
        estimate = estimate_data,
        model_function = AF_benefit,
        numberOfModelRuns = input$num_simulations_c,
        functionSyntax = "plainNames"
      )
    
    dir_temp <- paste0(current_user_dir(),"/debug")
    if (!dir.exists(dir_temp)) dir.create(dir_temp)
    saveRDS(data,
            paste0(dir_temp,"/mcResults_debug.rds"))
    
    data
  })
  
  #print (mcSimulation_results)
  #Plot function to plot combined graphs  
  Plot_mc_output <- function(mc_output, prefixes, total_area_value, plot_type, input_estimates) {
    # Extract the "y" element from the Monte Carlo output list
    y_data <- mc_output$y
    # Create an empty list to hold the selected columns by prefix
    selected_cols <- list()
    # Loop over each prefix
    for (prefix in prefixes) {
      # Find columns with the prefix in their names
      cols_with_prefix <- y_data[, grep(paste0("^", prefix), colnames(y_data)), drop = F]
      
      # Only add to the list if columns with the prefix are found
      if (ncol(cols_with_prefix) > 0) {
        selected_cols[[prefix]] <- cols_with_prefix
      }
    }
    
    allnames <- setNames(c("NPV Agroforestry Design 1", "NPV Agroforestry Design 2", "NPV Treeless System",
                           "NPV Decision AF Design 1", "NPV Decision AF Design 2",
                           "NPV Decision AF Design 1 (no Fund)",  "NPV Decision AF Design 2 (no Fund)"),
                         names(mc_output$y)[1:7])
    selectednames <- selected_cols[[1]] %>% names()
    y_axis_names <- allnames[selectednames]
    # print(allnames)
    # print(selectednames)
    # print(y_axis_names)
    
    # Combine all selected columns into one data frame
    filtered_data <- bind_cols(selected_cols)
    
    # Stack data so it can be used in ggplot2 functions
    stacked_data <- stack(filtered_data)
    # Find the value associated with "total_area" in the input_table
    # total_area_value <- input_table %>%
    #   filter(variable == "arable_area_treeless") %>%
    #   pull(lower)
    
    # Retrieve the total area value dynamically from total_area_df
    total_area_value <- input_estimates() %>%
      filter(variable == "arable_area_c") %>%
      pull(lower)
    # Check if the total_area_value is empty or missing
    if (length(total_area_value) == 0 || is.na(total_area_value)) {
      stop(paste("Error: The variable", total_area_var, "was not found in input_estimates."))
    }
    # Convert values in the stacked data
    # Divide values by 1000 and by the total area (KEURO/ha)
    stacked_data <- stacked_data %>%
      #mutate(values = values / 1000) %>%
      mutate(values = values / total_area_value)
    
    # Calculate median values for each output group
    scenario_medians <- stacked_data %>%
      group_by(ind) %>%
      summarize(median_value = median(values, na.rm = TRUE))
    
    # Reorder 'ind' levels based on median values (from low to high)
    stacked_data$ind <- factor(stacked_data$ind, 
                               levels = scenario_medians$ind[order(scenario_medians$median_value, decreasing = FALSE)])
    
    # Generate unique colors for each 'ind' using a color palette
    unique_ind <- unique(stacked_data$ind)
    num_colors <- length(unique_ind)
    color_palette <- hcl.colors(num_colors, palette =  "Earth") # Adjust for up to 12 unique colors
    
    # Create the plot
    if(plot_type == "box"){
      plot <- ggplot(stacked_data, aes(x = ind, y = values, fill = ind)) +
        geom_boxplot(alpha = 0.4, outlier.size = 0.2, outlier.alpha = 0.2) + # Reduce outlier size and make them half transparent
        theme_minimal() +
        theme(
          axis.title = element_text(size = 18, colour = "black"),
          axis.text = element_text(size = 15,
                                   # angle = 360,
                                   # hjust = 0.5,
                                   colour = "black"),
          axis.ticks.y = element_line(), 
          panel.background = element_rect(fill = "white",
                                          colour = "white"),
          plot.background =  element_rect(fill = "white",
                                          colour = "white"),
          legend.position = "none") +
        scale_fill_manual(values = color_palette) +
        labs(
          x = "Decision Options",
          y = "Outcome distribution [GBP/ha]") +
        coord_flip() +
        stat_boxplot(geom = "errorbar", width = 0.2) +
        #scale_y_continuous(breaks = seq(-75, max(stacked_data$values, na.rm = TRUE), by = 50)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5) +
        scale_x_discrete(labels=y_axis_names)
    }
    
    # if(plot_type == "density"){
    #   plot <- ggplot(stacked_data, aes(x = values, fill = ind, color = ind)) +
    #     geom_density(alpha = 0.4, size = 0.7) + # Create density plot with transparency and line size
    #     xlim( (min(stacked_data$values) - max(stacked_data$values)/5),
    #           (max(stacked_data$values) + max(stacked_data$values)/5)
    #     ) +
    #     theme_minimal() +
    #     theme(
    #       axis.title = element_text(size = 15, colour = "black"),
    #       axis.text.x = element_text(size = 12, colour = "black"),
    #       axis.text.y = element_blank(),
    #       panel.background =  element_rect(fill = "white",
    #                                        colour = "white"),
    #       plot.background =  element_rect(fill = "white",
    #                                       colour = "white"),
    #       legend.position = "none") +
    #     scale_fill_manual(values = color_palette) +
    #     labs(
    #       x = "Outcome distribution [GBP/ha]",
    #       y = "") +
    #     #scale_x_continuous(breaks = seq(-75, max(stacked_data$values, na.rm = TRUE), by = 50)) +
    #     geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.5) +
    # scale_y_discrete(labels=y_axis_names)
    # }
    
    if(plot_type =="ridge"){
      plot <- ggplot(stacked_data, aes(x = values, y = ind, fill = ind)) +
        geom_density_ridges(alpha = 0.4, scale = 1, rel_min_height = 0.001) +# Ridgeline plot with density
        scale_y_discrete(expand = c(0.01, 0)) +
        theme_minimal() +
        theme(
          axis.title = element_text(size = 18, colour = "black"),
          axis.text.x = element_text(size = 15,
                                     colour = "black"),
          axis.text.y = element_text(size = 15,
                                     # angle = 360,
                                     vjust = -6,
                                     colour = "black"),
          axis.text = element_text(size = 10, colour = "black"),
          panel.background =  element_rect(fill = "white",
                                           colour = "white"),
          plot.background =  element_rect(fill = "white",
                                          colour = "white"),
          legend.position = "none") +
        scale_fill_manual(values = color_palette) +
        labs(
          x = "Outcome distribution [GBP/ha]",
          y = "Scenarios") +
        #scale_x_continuous(breaks = seq(-75, max(stacked_data$values, na.rm = TRUE), by = 50)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.5) +
        scale_y_discrete(labels=y_axis_names)
    }
    # Return the plot
    return(plot)
  }
  
  # Plot reactive plots using the plot function
  plot1 <- reactive({Plot_mc_output(mcSimulation_results(), "NPV_", "arable_area_c", "box", input_estimates) })
  #plot2 <- reactive({ Plot_mc_output(mcSimulation_results(), "NPVtrade_off_AF1", "arable_area_c", "density", input_estimates) })
  plot3 <- reactive({Plot_mc_output(mcSimulation_results(), "NPVtrade_off_AF", "arable_area_c", "ridge", input_estimates)})
  plot4 <- reactive({ Plot_mc_output(mcSimulation_results(), "NPVtrade_off_", "arable_area_c", "box", input_estimates) })
  
  ### Render the plots using the reactive expressions
  output$distPlot <- renderPlot({
    req(mcSimulation_results())
    plot1()
  })
  
  # output$distPlot2 <- renderPlot({
  #   req(mcSimulation_results())
  #   plot2()
  #})
  
  output$distPlot3 <- renderPlot({
    req(mcSimulation_results())
    plot3()
  })
  
  output$distPlot4 <- renderPlot({
    req(mcSimulation_results())
    plot4()
  })
  
  
  
  
  
  plot5 <- reactive({ decisionSupport::plot_cashflow(
    mcSimulation_results(), "Cash_flow_AF1",
    x_axis_name = "",
    y_axis_name = "Annual cash-flow from Agroforestry (€)",
    color_25_75 = "navajowhite",
    color_5_95 = "green4",
    color_median = "darkblue",
    facet_labels = "")
  })
  output$distPlot5 <- renderPlot({
    req(mcSimulation_results())
    plot5()
  })
  
  plot6 <- reactive({ decisionSupport::plot_cashflow(
    mcSimulation_results(), "Cash_flow_AF2",
    x_axis_name = "",
    y_axis_name = "Annual cash-flow from Agroforestry (€)",
    color_25_75 = "navajowhite",
    color_5_95 = "green4",
    color_median = "darkblue",
    facet_labels = "")
  })
  output$distPlot6 <- renderPlot({
    req(mcSimulation_results())
    plot6()
  })

  plot7 <- reactive({ decisionSupport::plot_cashflow(
    mcSimulation_results(), "Cumulative_cashflow_AF1",
    x_axis_name = "",
    y_axis_name = "Cumulative cash-flow from Agroforestry (€)",
    color_25_75 = "navajowhite",
    color_5_95 = "green4",
    color_median = "darkblue",
    facet_labels = "")
  })
  output$distPlot7 <- renderPlot({
    req(mcSimulation_results())
    plot7()
  })

  plot8 <- reactive({ decisionSupport::plot_cashflow(
    mcSimulation_results(), "Cumulative_cashflow_AF2",
    x_axis_name = "",
    y_axis_name = "Cumulative cash-flow from Agroforestry (€)",
    color_25_75 = "navajowhite",
    color_5_95 = "green4",
    color_median = "darkblue",
    facet_labels = "")
  })
  output$distPlot8 <- renderPlot({
    req(mcSimulation_results())
    plot8()
  })
  
  
  ### Helper function to create download handlers
  createDownloadHandler <- function(plot_reactive, filename_prefix) {
    downloadHandler(
      filename = function() {
        paste(filename_prefix, input$project_name_x, format(Sys.time(), "%Y-%m-%d_%H-%M"), ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plot_reactive(), device = "png",
               width = 14, height = 8,
               dpi = 500)
      }
    )
  }
  
  ### Create download handlers for each plot
  output$save_plot1 <- createDownloadHandler(plot1, "Comparison_NPV_outcome_")
  # output$save_plot2 <- createDownloadHandler(plot2, "Plot_decision_outcome_")
  output$save_plot3 <- createDownloadHandler(plot3, "Probabilistic_decision_outcome_")
  output$save_plot4 <- createDownloadHandler(plot4, "Comparison_NPV_NoFund_")
  
  output$save_plot5 <- createDownloadHandler(plot5, "Cashflow 1")
  output$save_plot6 <- createDownloadHandler(plot6, "Cashflow 2")
  output$save_plot7 <- createDownloadHandler(plot7, "Cumulative Cashflow 1")
  output$save_plot8 <- createDownloadHandler(plot8, "Cumulative Cashflow 2")
}

# Run the Shiny app
shinyApp(ui, server)



