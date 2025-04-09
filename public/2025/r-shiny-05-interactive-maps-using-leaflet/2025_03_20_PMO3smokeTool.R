# path setup ----
options(warn = -1)
options(rsconnect.max.bundle.size = 3221225472)
main_path <- getwd()

# Source load ----
load("src/data/Processed_data.RData")

source("src/ui/ui_style.R")
source("src/ui/ginfo_tabs_intro.R")
source("src/ui/ginfo_tabs_update.R")
source("src/ui/gam_manual_qs.R")
source("src/ui/gam_previous_qs.R")
source("src/ui/gam_manual_gam_setup.R")

source("src/func/ext/func_packages.R")
source("src/func/ext/func_date_select.R")
source("src/func/ext/func_lyr_map.R")
source("src/func/ext/func_hms_fire.R")
source("src/func/ext/func_hms_smoke.R")
source("src/func/ext/func_hms_smoke_batch.R")
source("src/func/ext/func_hms_smoke_count.R")
source("src/func/ext/func_epa_api.R")
source("src/func/ext/func_iem_asos.R")
source("src/func/ext/func_data_merging.R")
source("src/func/ext/func_regres_post.R")
source("src/func/ext/func_gam_prediction.R")
source("src/func/ext/func_smoke_detection.R")

source("src/func/int/func_parameters.R")
source("src/func/int/func_subscribe_to_email.R")
source("src/func/int/func_subscribe_list_from_email.R")
source("src/func/int/func_subscribe_email_notice.R")
source("src/func/int/func_render_map.R")
source("src/func/int/func_create_download_handler.R")
source("src/func/int/func_find_nearest_site.R")
source("src/func/int/func_layer_points.R")
source("src/func/int/func_layer_points_mark.R")
source("src/func/int/func_layer_raster.R")
source("src/func/int/func_kriging_impute.R")
source("src/func/int/func_load_and_process_data.R")
source("src/func/int/func_load_data_for_gam.R")
source("src/func/int/func_render_download_ui.R")
source("src/func/int/func_render_tbl.R")
source("src/func/int/func_site_location.R")
source("src/func/int/func_site_select_tbl.R")
source("src/func/int/func_toggle_sites.R")

guest_dataset <- admin_dataset <- c(
  "GAM v1 (Lee and Jaffe, 2024)" = "gam_v1",
  # "GAM v2 (in progress)" = "gam_v2",
  # "GAM v2-EDM (in progress)" = "gam_v2_edm",
  "GAM v3 (in progress)" = "gam_v3",
  "GAM v3-EDM (in progress)" = "gam_v3_edm",
  "EPA EMBER (Simon et al., 2024)" = "epa_ember",
  "Smoke PM2.5 (in progress)" = "pm_cbsa"
)

## Hide/Show dataset (2025-02-27)
# guest_dataset <- admin_dataset[!admin_dataset %in% c("gam_v2", "gam_v2_edm")] # use in the future

datacomp_dataset <- c(
  "GAM v1 (Lee and Jaffe, 2024)" = "gam_v1",
  # "GAM v2 (in progress)" = "gam_v2",
  # "GAM v2-EDM (in progress)" = "gam_v2_edm",
  "GAM v3 (in progress)" = "gam_v3",
  "GAM v3-EDM (in progress)" = "gam_v3_edm",
  "EPA EMBER (Simon et al., 2024)" = "epa_ember"
)



# Define UI ----
ui <- fluidPage(
  
  ## Use shiny json ----
  shinyjs::useShinyjs(),
  
  tags$head(
    tags$style(HTML(
      ui_style
    ))
  ),
  
  ## UI guest login page ----
  div(
    id = "login_page",
    
    fluidRow(
      column(4, 
             offset = 4,
             
             wellPanel(
               class = "login-panel",
               
               h2("Welcome to PM2.5 and O3 smoke tool!"),
               
               hr(style = "background-color: black; height: 1px; border: none;"),
               p("To receive advance notifications about future updates and schedules, 
                  please enter your information and click the [Subscribe] button (recommended). 
                  If not, enter your email and click [Go to app]."),
               p("We are collecting your information only to provide news on updates to this tool.
                  We will not use your information in any other way."),
               
               hr(style = "background-color: black; height: 1px; border: none;"),
               textInput(inputId = "subscribe_email", label = HTML('Email (<span style="color: red;">required</span>)'), value = ""),
               textInput(inputId = "subscribe_affil", label = "Affiliation (optional)", value = ""),
               fluidRow(
                 column(6,
                        textInput(inputId = "subscribe_name_first", 
                                  label = "First name (optional)",
                                  value = ""),
                 ),
                 column(6,
                        textInput(inputId = "subscribe_name_last",
                                  label = "Last name (optional)",
                                  value = ""),
                 )
               ),
               
               fluidRow(
                 column(6,
                        style = "text-align: left;",
                        actionButton(inputId = "subscribe_button", 
                                     label = "Subscribe")
                 ),
                 column(6,
                        style = "text-align: right;",
                        actionButton(inputId = "login_btn", 
                                     label = "Go to app")
                 )
               ),
               
               textOutput(outputId = "subscribe_status"),
               
               hr(style = "background-color: black; height: 1px; border: none;"),
               p(style = "font-size: 13px;",
                 br(),
                 "This app was created by Dan Jaffe and Haebum Lee, University of Washington (UW).
                 For any questions or feedbacks, please contact to:",
                 "Dan Jaffe",
                 " (",
                 a("djaffe@uw.edu", href = "mailto:djaffe@uw.edu"), 
                 ") or",
                 "Haebum Lee", 
                 " (",
                 a("haebum1991@gmail.com", href = "mailto:haebum1991@gmail.com"),
                 ")")
             )
      )
    )
  ),
  
  ## UI admin login page ----
  shinyjs::hidden(
    div(
      id = "login_page_admin",
      
      fluidRow(
        column(4, 
               offset = 4,
               
               wellPanel(
                 h2("Please authenticate for administrator access"),
                 textInput(inputId = "username", 
                           label = "Username:"),
                 passwordInput(inputId = "password",
                               label = "Password:"),
                 
                 fluidRow(
                   column(6,
                          style = "text-align: left;",
                          offset = 0,
                          actionButton(inputId = "back_to_page_admin", 
                                       label = "Back to page"),
                   ),
                   column(6,
                          style = "text-align: right;",
                          actionButton(inputId = "login_btn_admin", 
                                       label = "Login")
                   )
                 )
               )
        )
      )
    )
  ),
  
  ## UI main page ----
  shinyjs::hidden(
    div(
      id = "ui_main_page",
      
      dashboardPage(
        skin = "blue",
        
        ### dashboard header ----
        dashboardHeader(
          title = tagList("PMO3smokeTool"),
          
          tags$li(
            class = "dropdown",
            style = "margin-right: 10px; margin-top: 8px;",
            
            actionButton(inputId = "login_admin_btn",
                         label = "Admin",
                         icon = icon("user-shield")),
            actionButton(inputId = "logout_btn",
                         label = "Logout",
                         icon = icon("sign-out-alt"))
          )
        ),
        
        ### dashboard sidebar ----
        dashboardSidebar(
          sidebarMenu(
            id = "tabs", 
            
            menuItem("General information", 
                     tabName = "ginfo", 
                     icon = icon("info-circle")),
            menuItem("Tutorial", 
                     tabName = "tutorial_blog", 
                     icon = icon("book-open")),
            menuItem("GAM manual", 
                     tabName = "gam_manual",
                     icon = icon("project-diagram")),
            menuItem("GAM previous", 
                     tabName = "gam_previous", 
                     icon = icon("history")),
            menuItem("Discussion of statistics", 
                     tabName = "discussion_of_statistics", 
                     icon = icon("comments")),
            menuItem("Admin: mailing", 
                     tabName = "admin_mailing", 
                     icon = icon("envelope")),
            div(
              textOutput(outputId = "memory_show"),
              shiny::span(
                textOutput("user_count"),
                HTML('<span style="cursor: pointer;" 
                  title=
"
Notice: 
There are 4 independent data channels.
If concurrent users in one channel exceeds one, 
then it means that multiple users are sharing a channel and
the app will become less responsive.
">
                  <i class="fa fa-question-circle" style="color: white; padding-left: 5px;"></i>
                  </span>')
              ),
              style = "text-align: left; padding-top: 10px; padding-left: 15px;"
            )
          )
        ),
        
        ### dashboard body ----
        dashboardBody(
          shinyjs::useShinyjs(),
          
          div(
            id = "loading_content", 
            
            div(class = "spinner"),
            HTML("App loading, please wait... <br>
               It takes a few seconds...")
          ),
          
          tabItems(
            
            #### Sidebar 1: General Information ----
            tabItem(
              tabName = "ginfo",
              
              tabBox(
                id = "ginfo_tabs",
                width = 12,
                
                ##### Sub-tab: Introduction ----
                tabPanel(
                  title = "Introduction",
                  value = "ginfo_tabs_intro",
                  ginfo_tabs_intro
                ),
                
                tabPanel(
                  title = "Update news",
                  value = "ginfo_tabs_update",
                  ginfo_tabs_update
                )
              )
            ),
            
            #### Sidebar 2: Tutorial ----
            tabItem(
              tabName = "tutorial_blog",
              
              tags$p(
                "This tutorial is loaded from Dr. Lee's blog webpage",
                tags$a(href = "https://leestory.netlify.app/", 
                       "(https://leestory.netlify.app/)", 
                       target = "_blank"), 
                ". If the information provided in",
                tags$b("[Quick start]"),
                "from the",
                tags$b("[GAM manual]"),
                "and", 
                tags$b("[GAM previous]"),
                "is not sufficient, this tutorial will be helpful.",
                "Additional R-related tutorials, beyond",
                tags$b("PMO3smokeTool"),
                "will be updated in the future. We appreciate your interest and support!",
                style = "font-size: 18px;"
              ),
              
              box(
                width = 12,
                tags$iframe(
                  src = "https://leestory.netlify.app/categories/smoketool-tutorial/",
                  width = "100%", 
                  height = "800px",
                  style = "border:none;"
                )
              )
            ),
            
            #### Sidebar 3: GAM manual ----
            tabItem(
              tabName = "gam_manual",
              
              tabBox(
                id = "gam_manual_tabs",
                width = 12,
                
                ##### Sub-tab: Quick start ----
                tabPanel(
                  title = "Quick start",
                  value = "gam_manual_qs",
                  gam_manual_qs
                ),
                
                ##### Sub-tab: Data collection ----
                tabPanel(
                  title = "Data collection",
                  value = "gam_manual_data_collection",
                  
                  fluidRow(
                    box(
                      title = "Site map",
                      status = "primary",
                      solidHeader = T,
                      width = 6,
                      
                      p("Site information is provided as a tooltip"),
                      leaflet::leafletOutput(outputId = "map_site_col",
                                             height = "45vh"),
                      
                      checkboxInput(inputId = "gam_manual_show_epa_sites_o3",
                                    label = "Show EPA sites for O3 (red: 44201)",
                                    value = F),
                      checkboxInput(inputId = "gam_manual_show_epa_sites_pm",
                                    label = "Show EPA sites for PM (black: 88101, blue: 88502)",
                                    value = F),
                      checkboxInput(inputId = "gam_manual_show_iem_sites",
                                    label = "Show IEM sites for ASOS (green)",
                                    value = F),
                      
                      hr(),
                      p(tags$b("EPA site search (you can type the site name or AQS if you know it):")),
                      fluidRow(
                        column(6,
                               selectizeInput(inputId = "gam_manual_search_input",
                                              label = NULL,
                                              choices = NULL, 
                                              options = list(
                                                placeholder = "Type to search...",
                                                maxOptions = 50
                                              ))
                        ),
                        column(6,
                               div(style = "text-align: right;",
                                   actionButton(inputId = "gam_manual_button_add_to_list",
                                                label = "Add all to the list",
                                                icon = icon("plus"))
                               )
                        )
                      ),
                      
                      div(
                        style = "overflow-y: auto; overflow-x: auto;",
                        DT::dataTableOutput("gam_manual_search_results")
                      )
                    ),
                    
                    box(
                      title = "List of selected sites",
                      status = "primary",
                      solidHeader = T,
                      width = 6,
                      
                      p("Click the target site or add an item from the site search if you want to add it here."),
                      div(
                        style = "height: 45vh; overflow-y: auto; overflow-x: auto;",
                        DT::dataTableOutput("gam_manual_selected_sites")
                      ),
                      actionButton(inputId = "gam_manual_clear_table", 
                                   label = "Clear list",
                                   icon = icon("trash")),
                      
                      hr(),
                      fluidRow(
                        column(6, 
                               textInput(inputId = "epa_my_email",
                                         label = "Type your email for EPA-API",
                                         value = "",
                                         placeholder = "myemail@example.com")),
                        column(6, 
                               textInput(inputId = "epa_my_key",
                                         label = "API Key", 
                                         value = "",
                                         placeholder = "myapikey"))
                      ),
                      
                      fluidRow(
                        column(12, 
                               actionButton(inputId = "epa_my_get_key",
                                            label ="Get key (if you don't have)",
                                            icon = icon("key")))
                      ),
                      
                      hr(),
                      fluidRow(
                        column(6,
                               dateInput(inputId = "date_start_col",
                                         label = "Date start",
                                         value = "2023-05-01")),
                        column(6, 
                               dateInput(inputId = "date_end_col",
                                         label = "Date end", 
                                         value = "2023-09-30")),
                      ),
                      
                      strong("Which months would you like to collect data for?"),
                      fluidRow(
                        lapply(1:12, function(i) {
                          column(
                            width = 1,
                            checkboxInput(inputId = paste0("month_selection_", i), 
                                          label = i, 
                                          value = ifelse(i >= 5 & i <= 9, T, F))
                          )
                        })
                      ),
                      
                      fluidRow(
                        column(6,
                               actionButton(inputId = "gam_manual_run_col",
                                            label = "Run collection",
                                            icon = icon("play"))
                        )
                      )
                    )
                  ),
                  
                  fluidRow(
                    tabBox(
                      id = "data_output_tabs",
                      width = 12,
                      
                      tabPanel(
                        title = "O3 data",
                        
                        fluidRow(
                          box(
                            status = "primary",
                            solidHeader = T,
                            width = 12,
                            uiOutput(outputId = "download_ui_o3"),
                            uiOutput(outputId = "data_ui_o3")
                          )
                        )
                      ),
                      
                      tabPanel(
                        title = "PM2.5 data",
                        
                        fluidRow(  
                          box(
                            status = "primary",
                            solidHeader = T,
                            width = 12,
                            uiOutput(outputId = "download_ui_pm"),
                            uiOutput(outputId = "data_ui_pm")
                          )
                        )
                      ),
                      
                      tabPanel(
                        title = "ASOS data", 
                        
                        fluidRow(
                          box(
                            status = "primary",
                            solidHeader = T,
                            width = 12,
                            uiOutput(outputId = "download_ui_asos"),
                            uiOutput(outputId = "data_ui_asos")
                          )
                        )
                      ),
                      
                      tabPanel(
                        title = "Merge data",
                        
                        fluidRow(
                          box(
                            status = "primary",
                            solidHeader = T,
                            width = 12,
                            uiOutput(outputId = "download_ui_merge"),
                            uiOutput(outputId = "data_ui_merge")
                          )
                        )
                      )
                    ) 
                  )
                ),
                
                ##### Sub-tab: HMS & PM-crit ----
                tabPanel(
                  title = "HMS and PM2.5-criteria",
                  value = "gam_manual_hms_pmcrit",
                  
                  p(style = "font-weight: bold; color: red; font-size: 14px;",
                    "This tab is only active if PM2.5 data collection is enabled."
                  ),
                  p(style = "font-size: 16px;",
                    "If you want to include PM2.5-criteria, select PM2.5-criteria method.
                      This may take some time as it requires HMS data.
                      HMS data (by point-in-polygon) is also added to this process."
                  ),
                  
                  fluidRow(
                    column(3,
                           radioButtons(
                             inputId = "gam_manual_pmcrit_period_selection",
                             label = "Select the calculation period",
                             choices = c("All years" = "by_all",
                                         "By year" = "by_year",
                                         "By month" = "by_month"),
                             selected = "by_month"
                           )
                    ),
                    column(3,
                           radioButtons(
                             inputId = "gam_manual_pmcrit_method_selection",
                             label = "Select the calculation method",
                             choices = c("Mean + 1 SD" = "meansd",
                                         "Median + 1 MAD" = "pm1p0mad",
                                         "Median + 0.5 MAD" = "pm0p5mad"),
                             selected = "meansd"
                           )
                    ),
                    column(6,
                           p(style = "font-size: 15px;",
                             tags$b("Do you want to exclude indenpence day (July 4-5) when calculating PM2.5-criteria?")
                           ),
                           checkboxInput(inputId = "gam_manual_pmcrit_exc_indep",
                                         label = "Exclude independence day (July 4-5)",
                                         value = T),
                           actionButton(inputId = "gam_manual_pmcrit_run",
                                        label = "Run process",
                                        icon = icon("play"))
                    )
                  ),
                  p(style = "font-size: 14px;",
                    tags$b("All years: "),
                    "PM2.5-Crit is calculated based on the entire period of data collected for each site (Lee and Jaffe, 2024b).",
                    br(),
                    tags$b("By year: "),
                    "PM2.5-Crit is calculated separately for each year based on the data collected at each site (Lee and Jaffe, 2024a).",
                    br(),
                    tags$b("By month: "),
                    "PM2.5-Crit is calculated separately for each month based on the data collected at each site.",
                  ),
                  p(style = "font-size: 14px;",
                    tags$b("Mean + 1 SD: "),
                    "PM2.5-Crit is calculated as the sum of the mean and one standard deviation (1 SD) for that period (Lee and Jaffe, 2024a; 2024b).",
                    br(),
                    tags$b("Mean + 1 MAD: "),
                    "PM2.5-Crit is calculated as the sum of the median and one mean absolute deviation (1 MAD) for that period.",
                    br(),
                    tags$b("Mean + 0.5 MAD: "),
                    "PM2.5-Crit is calculated as the sum of the median and half-mean absolute deviation (0.5 MAD) for that period.",
                  ),
                  tags$ul(
                    tags$li(
                      "Lee, H. and Jaffe, D. A.:
                        Wildfire impacts on O3 in the continental United States using PM2.5 and a generalized additive model (2018–2023),
                        Environ. Sci. Technol., 58, 14764–14774, 2024.",
                      a("(Lee and Jaffe, 2024b)", href = "https://doi.org/10.1021/acs.est.4c05870", target = "_blank")
                    ),
                    tags$li(
                      "Lee, H. and Jaffe, D. A.:
                        Impact of wildfire smoke on ozone concentrations using a Generalized Additive model in Salt Lake City, Utah, USA, 2006–2022,
                        J. Air Waste Manag. Assoc., 74, 116–130, 2024.",
                      a("(Lee and Jaffe, 2024a)", href = "https://doi.org/10.1080/10962247.2023.2291197", target = "_blank")
                    )
                  ),
                  p(style = "font-size: 14px; color: red;",
                    tags$b("!Caveat"),
                    br(),
                    "To determine the HMS overhead smoke plume for each site (i.e., point-in-polygon (PIP)),
                      this app utilizes the 'sp' package for PIP analysis and 'sf' package for importing the polygon (HMS) data. 
                      In our previous studies, we used the 'rgeos' package for importing HMS data.
                      Consequently, slightly different PIP outcomes may be obtained for certain days (but the differences are minimal). 
                      These differences arise from variations in the processing methods and algorithms of each package,
                      which handle spatial precision and data in slightly different ways."),
                  fluidRow(
                    box(
                      status = "primary",
                      solidHeader = T,
                      width = 12,
                      uiOutput(outputId = "download_ui_pmcrit"),
                      uiOutput(outputId = "data_ui_pmcrit")
                    )
                  )
                ),
                
                ##### Sub-tab: Load dataset ----
                tabPanel(
                  title = "Load dataset",
                  value = "gam_manual_load_dataset",
                  
                  fluidRow(
                    box(
                      status = "primary",
                      solidHeader = T,
                      width = 12,
                      style = "overflow-x: auto;",
                      
                      h5("If you want to collect dataset manually, please go to [Data collection] section."),
                      br(),
                      fileInput(inputId = "gam_manual_file", 
                                label = "Choose a CSV file", 
                                buttonLabel = "Browse...", 
                                placeholder = "No file selected"),
                      fluidRow(
                        column(12, 
                               actionButton(inputId = "gam_manual_load_merged_data",
                                            label = "Load data from [Data collection: Merge data]",
                                            icon = icon("folder-open")),
                               actionButton(inputId = "gam_manual_load_sample",
                                            label = "Load sample data",
                                            icon = icon("folder-open")),
                               actionButton(inputId = "gam_manual_load_clear",
                                            label = "Clear all",
                                            icon = icon("trash")))
                      ),
                      hr(),
                      shiny::textOutput("csv_name"),
                      br(),
                      DT::dataTableOutput("csv_table")
                    )
                  )
                )
              )
            ),
            
            #### Sidebar 4: GAM previous ----
            tabItem(
              tabName = "gam_previous",
              
              tabBox(
                id = "gam_previous_tabs",
                width = 12,
                
                ##### Sub-tab: Quick start ----
                tabPanel(
                  title = "Quick start",
                  value = "gam_previous_qs",
                  gam_previous_qs
                ),
                
                ##### Sub-tab: Overview ----
                tabPanel(
                  title = "Overview",
                  value = "gam_previous_overview",
                  
                  fluidRow(
                    box(
                      title = "Site map",
                      status = "primary",
                      solidHeader = T,
                      width = 6,
                      
                      p("Site information is provided as a tooltip"),
                      leaflet::leafletOutput(outputId = "map_site_gam",
                                             height = "45vh"),
                      
                      checkboxInput(inputId = "gam_previous_show_gam_v1",
                                    label = "Show GAM v1 (Lee and Jaffe, 2024) (red)",
                                    value = F),
                      checkboxInput(inputId = "gam_previous_show_gam_v3", 
                                    label = "Show GAM v3 (in progress) (green)", 
                                    value = F),   
                      checkboxInput(inputId = "gam_previous_show_gam_v3_edm", 
                                    label = "Show GAM v3 EDM (in progress) (darkgreen)", 
                                    value = F),
                      
                      ## Hide/Show dataset (2025-02-27)
                      # checkboxInput(inputId = "gam_previous_show_gam_v2", 
                      #               label = "Show GAM v2 (in progress) (orange)", 
                      #               value = F),   
                      # checkboxInput(inputId = "gam_previous_show_gam_v2_edm", 
                      #               label = "Show GAM v2 EDM (in progress) (darkorange)", 
                      #               value = F),
                      
                      checkboxInput(inputId = "gam_previous_show_epa_ember", 
                                    label = "Show EPA EMBER (Simon et al., 2024) (yellow)", 
                                    value = F),
                      
                      checkboxInput(inputId = "gam_previous_show_pm_cbsa", 
                                    label = "Show Smoke PM2.5 (in progress) (magenta)", 
                                    value = F),
                      
                      checkboxInput(inputId = "gam_previous_show_gam_pm",
                                    label = "Show EPA sites for PM (black: 88101, blue: 88502)",
                                    value = F),
                      
                      hr(),
                      p(tags$b("Site search (you can type the site name or AQS if you know it):")),
                      fluidRow(
                        column(6,
                               selectizeInput(inputId = "gam_previous_search_input",
                                              label = NULL,
                                              choices = NULL, 
                                              options = list(
                                                placeholder = "Type to search...",
                                                maxOptions = 50
                                              ))
                        ),
                        column(6,
                               div(style = "text-align: right;",
                                   actionButton(inputId = "gam_previous_button_add_to_list",
                                                label = "Add all to the list",
                                                icon = icon("plus"))
                               )
                        )
                      ),
                      
                      div(
                        style = "overflow-y: auto; overflow-x: auto;",
                        DT::dataTableOutput("gam_previous_search_results")
                      )
                    ),
                    
                    box(
                      title = "Model summary & List of selected sites",
                      status = "primary",
                      solidHeader = T,
                      width = 6,
                      
                      p("Click the target site if you want to see detailed information"),
                      fluidRow(
                        tabBox(
                          id = "gam_previous_meta_tabs",
                          width = 12,
                          
                          tabPanel(
                            title = "Metadata",
                            value = "gam_previous_meta_tabs_table",
                            
                            div(
                              style = "height: 45vh; overflow-y: auto; overflow-x: auto;",
                              DT::dataTableOutput("gam_previous_meta_table_1"),
                              DT::dataTableOutput("gam_previous_meta_table_2")
                            ),
                          ),
                          
                          tabPanel(
                            title = "Obs vs Pred",
                            value = "gam_previous_meta_tabs_plots_obs_pred",
                            
                            div(
                              style = "height: 45vh; overflow-y: auto; overflow-x: auto;",
                              echarts4rOutput(outputId = "gam_plot_obs_vs_pred")
                            )
                          ),
                          
                          tabPanel(
                            title = "Time-series",
                            value = "gam_previous_meta_tabs_plots_time_series",
                            
                            div(
                              style = "height: 45vh; overflow-y: auto; overflow-x: auto;",
                              echarts4rOutput(outputId = "gam_plot_obs_vs_pred_time")
                            )
                          ),
                          
                          tabPanel(
                            title = "Monthly smoke",
                            value = "gam_previous_meta_tabs_plots_smoke_freq",
                            
                            div(
                              style = "height: 45vh; overflow-y: auto; overflow-x: auto;",
                              echarts4rOutput(outputId = "gam_plot_smoke_monthly")
                            )
                          ),
                          
                          tabPanel(
                            title = "Exceedance day",
                            value = "gam_previous_meta_tabs_plots_exceedance",
                            
                            div(
                              style = "height: 45vh; overflow-y: auto; overflow-x: auto;",
                              echarts4rOutput(outputId = "gam_plot_exceedance")
                            )
                          )
                        )
                      ),
                      
                      hr(),
                      p("Click the target site or add an item from the site search if you want to add it here."),
                      div(
                        style = "overflow-y: auto; overflow-x: auto;",
                        DT::dataTableOutput("gam_previous_selected_sites")
                      ),
                      
                      hr(),
                      fluidRow(
                        column(6, 
                               dateInput(inputId = "date_start_gam", 
                                         label = "Date start", 
                                         value = "2023-05-01")),
                        column(6, 
                               dateInput(inputId = "date_end_gam",
                                         label = "Date end", 
                                         value = "2023-09-30")),
                      ),
                      
                      fluidRow(
                        column(6, 
                               actionButton(inputId = "gam_previous_button_load",
                                            label = "Load data",
                                            icon = icon("folder-open"))),
                        column(6, 
                               actionButton(inputId = "gam_previous_clear_table", 
                                            label = "Clear list",
                                            icon = icon("trash")),
                        )
                      )
                    )
                  ),
                  
                  fluidRow(
                    tabBox(
                      id = "gam_previous_data_tabs",
                      width = 12,
                      
                      tabPanel(
                        title = "GAM v1",
                        value = "gam_previous_data_load_gam_v1",
                        
                        fluidRow(
                          box(
                            status = "primary",
                            solidHeader = T,
                            width = 12,
                            style = "overflow-x: auto;",
                            
                            uiOutput(outputId = "download_ui_gam_v1"),
                            uiOutput(outputId = "data_ui_gam_v1")
                          )
                        )
                      ),
                      
                      tabPanel(
                        title = "GAM v3",
                        value = "gam_previous_data_load_gam_v3",
                        
                        fluidRow(
                          box(
                            status = "primary",
                            solidHeader = T,
                            width = 12,
                            style = "overflow-x: auto;",
                            
                            uiOutput(outputId = "download_ui_gam_v3"),
                            uiOutput(outputId = "data_ui_gam_v3")
                          )
                        )
                      ),
                      
                      tabPanel(
                        title = "GAM v3 EDM",
                        value = "gam_previous_data_load_gam_v3_edm",
                        
                        fluidRow(
                          box(
                            status = "primary",
                            solidHeader = T,
                            width = 12,
                            style = "overflow-x: auto;",
                            
                            uiOutput(outputId = "download_ui_gam_v3_edm"),
                            uiOutput(outputId = "data_ui_gam_v3_edm")
                          )
                        )
                      ),
                      
                      ## Hide/Show dataset (2025-02-27)
                      # tabPanel(
                      #   title = "GAM v2",
                      #   value = "gam_previous_data_load_gam_v2",
                      #   
                      #   fluidRow(
                      #     box(
                      #       status = "primary",
                      #       solidHeader = T,
                      #       width = 12,
                      #       style = "overflow-x: auto;",
                      #       
                      #       uiOutput(outputId = "download_ui_gam_v2"),
                      #       uiOutput(outputId = "data_ui_gam_v2")
                      #     )
                      #   )
                      # ),
                      # 
                      # tabPanel(
                      #   title = "GAM v2 EDM",
                      #   value = "gam_previous_data_load_gam_v2_edm",
                      #   
                      #   fluidRow(
                      #     box(
                      #       status = "primary",
                      #       solidHeader = T,
                      #       width = 12,
                      #       style = "overflow-x: auto;",
                      #       
                      #       uiOutput(outputId = "download_ui_gam_v2_edm"),
                      #       uiOutput(outputId = "data_ui_gam_v2_edm")
                      #     )
                      #   )
                      # ),
                      
                      tabPanel(
                        title = "EPA EMBER", 
                        value = "gam_previous_data_load_epa_ember",
                        
                        fluidRow(
                          box(
                            status = "primary",
                            solidHeader = T,
                            width = 12,
                            style = "overflow-x: auto;",
                            
                            uiOutput(outputId = "download_ui_epa_ember"),
                            uiOutput(outputId = "data_ui_epa_ember")
                          )
                        )
                      ),
                      
                      tabPanel(
                        title = "Smoke PM2.5", 
                        value = "gam_previous_data_load_pm_cbsa",
                        
                        fluidRow(
                          box(
                            status = "primary",
                            solidHeader = T,
                            width = 12,
                            style = "overflow-x: auto;",
                            
                            uiOutput(outputId = "download_ui_pm_cbsa"),
                            uiOutput(outputId = "data_ui_pm_cbsa")
                          )
                        )
                      )
                    )
                  )
                ),
                
                ##### Sub-tab: Layer map ----
                tabPanel(
                  title = "Layer map",
                  value = "gam_previous_layer_map",
                  
                  fluidRow(
                    box(
                      status = "primary",
                      solidHeader = T,
                      width = 12,
                      
                      fluidRow(
                        column(8,
                               tabBox(
                                 id = "gam_previous_layer_map_tabs",
                                 width = 12,
                                 tabPanel(
                                   title = "Main map", 
                                   value = "gam_previous_layer_map_main",
                                   
                                   p(tags$b("The detailed information is provided in the tooltip,
                                          and please see the [Parameter descriptions]")),
                                   p(tags$b("[Raster layers] are for visualization purposes only (with a 0.5-degree resolution generated using Kriging interpolation)."),
                                     "Sometimes incorrect interpolation results may be displayed.
                                       Therefore, for accurate values, please use the tooltips on [point layers] or
                                       click the 'Export data' button to view the spatial data for the selected date."),
                                   
                                   leaflet::leafletOutput(outputId = "map_site_lyr",
                                                          height = "60vh"),
                                   br(),
                                   fluidRow(
                                     column(12,
                                            selectizeInput(inputId = "gam_previous_search_input_lyr",
                                                           label = "Site search (you can type the site name or AQS if you know it):",
                                                           choices = NULL,
                                                           options = list(
                                                             placeholder = "Type to search...",
                                                             maxOptions = 50
                                                           )
                                            ),
                                            div(
                                              style = "height: 30vh; overflow-y: auto; overflow-x: auto;",
                                              DT::dataTableOutput("gam_previous_search_results_lyr")
                                            )
                                     )
                                   ),
                                   actionButton(inputId = "gam_previous_lyr_export_map",
                                                label = "Export map",
                                                icon = icon("file-export"))
                                 ),
                                 
                                 tabPanel(
                                   title = "Comparison by state", 
                                   value = "gam_previous_layer_map_comp_by_state",
                                   
                                   echarts4rOutput(outputId = "lyr_plot_comp_by_state",
                                                   height = "60vh")
                                 ),
                                 
                                 tabPanel(
                                   title = "Exceedance day by state",
                                   value = "gam_previous_layer_map_plots_exceedance",
                                   
                                   echarts4rOutput(outputId = "lyr_plot_exceedance_by_state",
                                                   height = "60vh")
                                   
                                 ),
                                 
                                 tabPanel(
                                   title = "Comparison by AQS",
                                   value = "gam_previous_layer_map_comp_by_aqs",
                                   
                                   echarts4rOutput(outputId = "lyr_plot_comp_by_aqs",
                                                   height = "60vh")
                                   
                                 ),
                                 
                                 tabPanel(
                                   title = "Comparison by AQS: scat",
                                   value = "gam_previous_layer_map_comp_by_aqs_scat",
                                   
                                   echarts4rOutput(outputId = "lyr_plot_comp_by_aqs_scat",
                                                   height = "60vh")
                                   
                                 )
                               )
                        ),
                        
                        column(4,
                               selectInput(
                                 inputId = "gam_previous_lyr_gam_ver",
                                 label = "Select dataset:",
                                 choices = guest_dataset,
                                 selected = "gam_v1"
                               ),
                               
                               dateInput(inputId = "gam_previous_lyr_date",
                                         label = "Select date:",
                                         value = "2023-06-08"),
                               
                               div(style = "display: flex; justify-content: space-between; width: 100%; padding: 0px;",
                                   actionButton(inputId = "prev_year", label = "-1 y", style = "flex: 1; text-align: center;"),
                                   actionButton(inputId = "prev_month", label = "-1 m", style = "flex: 1; text-align: center;"),
                                   actionButton(inputId = "prev_week", label = "-7 d", style = "flex: 1; text-align: center;"),
                                   actionButton(inputId = "prev_day", label = "-1 d", style = "flex: 1; text-align: center;"),
                                   actionButton(inputId = "next_day", label = "+1 d", style = "flex: 1; text-align: center;"),
                                   actionButton(inputId = "next_week", label = "+7 d", style = "flex: 1; text-align: center;"),
                                   actionButton(inputId = "next_month", label = "+1 m", style = "flex: 1; text-align: center;"),
                                   actionButton(inputId = "next_year", label = "+1 y", style = "flex: 1; text-align: center;")
                               ),
                               
                               hr(),
                               fluidRow(
                                 column(4,
                                        p("Point layers:", 
                                          style = "font-weight: bold;"),
                                        checkboxInput(inputId = "gam_previous_lyr_pts_o3_obs", 
                                                      label = "Obs MDA8", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_pts_o3_pred", 
                                                      label = "Pred MDA8", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_pts_smo", 
                                                      label = "SMO", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_pts_res", 
                                                      label = "Residual", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_pts_pm_obs", 
                                                      label = "Obs PM2.5", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_pts_pm_quant", 
                                                      label = "Quant PM2.5", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_pts_pm_crit", 
                                                      label = "PM2.5-crit", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_pts_pm_crit_m0p5m", 
                                                      label = "PM2.5-crit m0p5m", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_pts_pm_crit_m1p0m", 
                                                      label = "PM2.5-crit m1p0m", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_pts_smoke_pm_m0p5m", 
                                                      label = "Smoke PM2.5 m0p5m", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_pts_smoke_pm_m1p0m", 
                                                      label = "Smoke PM2.5 m1p0m", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_pts_r2", 
                                                      label = "R2", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_pts_rank", 
                                                      label = "1st rank term", 
                                                      value = F)
                                 ),
                                 column(4,
                                        p("Raster layers:", 
                                          style = "font-weight: bold;"),
                                        checkboxInput(inputId = "gam_previous_lyr_ras_o3_obs", 
                                                      label = "Obs MDA8", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_ras_o3_pred", 
                                                      label = "Pred MDA8", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_ras_smo", 
                                                      label = "SMO", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_ras_res", 
                                                      label = "Residual", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_ras_pm_obs", 
                                                      label = "Obs PM2.5", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_ras_pm_quant", 
                                                      label = "Quant PM2.5", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_ras_pm_crit", 
                                                      label = "PM2.5-crit", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_ras_pm_crit_m0p5m", 
                                                      label = "PM2.5-crit m0p5m", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_ras_pm_crit_m1p0m", 
                                                      label = "PM2.5-crit m1p0m", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_ras_smoke_pm_m0p5m", 
                                                      label = "Smoke PM2.5 m0p5m", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_ras_smoke_pm_m1p0m", 
                                                      label = "Smoke PM2.5 m1p0m", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_ras_r2", 
                                                      label = "R2", 
                                                      value = F)
                                 ),
                                 column(4,
                                        p("HMS layers:", 
                                          style = "font-weight: bold;"),
                                        checkboxInput(inputId = "gam_previous_lyr_hms", 
                                                      label = "Smoke plume", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_fire", 
                                                      label = "Fire point", 
                                                      value = F),
                                        
                                        p("Mark options:", 
                                          style = "font-weight: bold;"),
                                        checkboxInput(inputId = "gam_previous_lyr_pts_smoke_mark", 
                                                      label = "Smoke day", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_pts_smoke_mark_m0p5m", 
                                                      label = "Smoke day m0p5m", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_pts_smoke_mark_m1p0m", 
                                                      label = "Smoke day m1p0m", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_pts_smo_pM1p0MAD", 
                                                      label = "SMO > ~83th", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_pts_smo_pM2p0MAD", 
                                                      label = "SMO > ~96th", 
                                                      value = F),
                                        checkboxInput(inputId = "gam_previous_lyr_pts_smo_p975", 
                                                      label = "SMO > 97.5th", 
                                                      value = F)
                                 )
                               ),
                               
                               hr(),
                               downloadButton(outputId = "gam_previous_lyr_export_data",
                                              label = "Export data",
                                              icon = icon("file-export")),
                               
                               hr(),
                               
                               p(style = "font-size: 16px;",
                                 tags$b("Parameter descriptions: "),
                               ),
                               
                               div(id = "param_desc_gam",
                                   p(style = "font-size: 14px;",
                                     tags$b("Obs MDA8: "),
                                     "Observed MDA8",
                                     br(),
                                     tags$b("Pred MDA8: "),
                                     "GAM predicted MDA8",
                                     br(),
                                     tags$b("SMO: "),
                                     "SMOs are equal to Residuals on smoke days (N/A for non-smoke day).",
                                     br(),
                                     tags$b("Residual: "),
                                     "Obs MDA8 - Pred MDA8",
                                     br(),
                                     tags$b("Obs PM2.5: "),
                                     "Observed PM2.5",
                                     br(),
                                     tags$b("PM2.5-crit: "),
                                     "PM2.5-criteria using Med + 1.0 MAD method",
                                     br(),
                                     tags$b("R2: "),
                                     "GAM R-squared for each site",
                                     br(),
                                     tags$b("1st rank term: "),
                                     "The most important predictor",
                                     br(),
                                     br(),
                                     tags$b("Smoke: "),
                                     "NOAA-HMS satellite product for smoke plume",
                                     br(),
                                     tags$b("Fire: "),
                                     "NOAA-HMS satellite product for fire points",
                                     br(),
                                     br(),
                                     tags$b("Smoke day: "),
                                     "Identified smoke day",
                                     br(),
                                     tags$b("~83th: "),
                                     "Med + 1 MAD residual (~83th quantile) of non-smoke days for each site",
                                     br(),
                                     tags$b("~96th: "),
                                     "Med + 2 MAD residual (~96th quantile) of non-smoke days for each site",
                                     br(),
                                     tags$b("97.5th: "),
                                     "97.5th residual quantile of non-smoking days for each site",
                                   )
                               ),
                               
                               div(id = "param_desc_ember",
                                   p(style = "font-size: 14px;",
                                     tags$b("Obs MDA8: "),
                                     "Observed MDA8",
                                     br(),
                                     tags$b("Pred MDA8: "),
                                     "EMBER predicted MDA8 from base simulation",
                                     br(),
                                     tags$b("SMO: "),
                                     "Smoke contribution to O3 MDA8 (SMO).  (Difference of base case results and no bb emissions results)",
                                     br(),
                                     tags$b("Residual: "),
                                     "Obs MDA8 - Pred MDA8",
                                     br(),
                                     tags$b("R2: "),
                                     "EMBER R-squared for each site",
                                     br(),
                                     br(),
                                     tags$b("Smoke: "),
                                     "NOAA-HMS satellite product for smoke plume",
                                     br(),
                                     tags$b("Fire: "),
                                     "NOAA-HMS satellite product for fire points"
                                   )
                               ),
                               
                               div(id = "param_desc_pm_cbsa",
                                   p(style = "font-size: 14px;",
                                     tags$b("Obs PM2.5: "),
                                     "Observed PM2.5",
                                     br(),
                                     tags$b("Quant PM2.5: "),
                                     "PM2.5 quantile based on non-smoke days",
                                     br(),
                                     tags$b("PM2.5-crit m0p5m: "),
                                     "PM2.5-criteria using Med + 0.5 MAD method",
                                     br(),
                                     tags$b("PM2.5-crit m1p0m: "),
                                     "PM2.5-criteria using Med + 1.0 MAD method",
                                     br(),
                                     tags$b("Smoke PM2.5 m0p5m: "),
                                     "Smoke PM2.5 = Obs PM2.5 - PM2.5-crit m0p5m for smoke day",
                                     br(),
                                     tags$b("Smoke PM2.5 m1p0m: "),
                                     "Smoke PM2.5 = Obs PM2.5 - PM2.5-crit m1p0m for smoke day",
                                     br(),
                                     br(),
                                     tags$b("Smoke: "),
                                     "NOAA-HMS satellite product for smoke plume",
                                     br(),
                                     tags$b("Fire: "),
                                     "NOAA-HMS satellite product for fire points",
                                     br(),
                                     br(),
                                     tags$b("Smoke day m0p5m: "),
                                     "Identified smoke day by PM2.5-crit m0p5m",
                                     br(),
                                     tags$b("Smoke day m1p0m: "),
                                     "Identified smoke day by PM2.5-crit m1p0m",
                                     br(),
                                   )
                               ),
                               
                               br(),
                               p(style = "color: red;",
                                 tags$b("Caveat: "),
                                 br(),
                                 "Unlike the GAM dataset, 'Residual' in the EMBER dataset is not the same as 'SMO'.
                                         This is because the model approach is different.
                                         For more details, please refer to the paper.")
                               
                        )
                      )
                    )
                  )
                ),
                
                ##### Sub-tab: Data comparison ----
                tabPanel(
                  title = "Data comparison",
                  value = "gam_previous_comp",
                  
                  fluidRow(
                    box(
                      status = "primary",
                      solidHeader = T,
                      width = 12,
                      
                      column(4,
                             fluidRow(
                               column(12,
                                      selectizeInput(inputId = "gam_previous_search_input_comp_state",
                                                     label = "State:",
                                                     choices = NULL,
                                                     options = list(
                                                       placeholder = "Type to search...",
                                                       maxOptions = 50
                                                     )
                                      ),
                                      selectizeInput(inputId = "gam_previous_search_input_comp_aqs",
                                                     label = "AQS O3:",
                                                     choices = NULL,
                                                     options = list(
                                                       placeholder = "Type to search...",
                                                       maxOptions = 50
                                                     )
                                      )
                               )
                             ),
                             
                             hr(),
                             
                             fluidRow(
                               column(6,
                                      selectInput(
                                        inputId = "gam_previous_comp_dataset_1",
                                        label = "Dataset 1:",
                                        choices = guest_dataset,
                                        selected = "gam_v1"
                                      ),
                                      p("Parameter 1:", 
                                        style = "font-weight: bold;"),
                                      checkboxInput(inputId = "gam_previous_comp_mda8_obs_1", 
                                                    label = "Obs MDA8", 
                                                    value = F),
                                      checkboxInput(inputId = "gam_previous_comp_mda8_pred_1", 
                                                    label = "Pred MDA8", 
                                                    value = T),
                                      checkboxInput(inputId = "gam_previous_comp_mda8_smo_1", 
                                                    label = "SMO", 
                                                    value = F),
                                      checkboxInput(inputId = "gam_previous_comp_mda8_resids_1", 
                                                    label = "Residual", 
                                                    value = F),
                                      checkboxInput(inputId = "gam_previous_comp_pm25_1", 
                                                    label = "Obs PM2.5", 
                                                    value = F)
                               ),
                               column(6,
                                      selectInput(
                                        inputId = "gam_previous_comp_dataset_2",
                                        label = "Dataset 2:",
                                        choices = guest_dataset,
                                        selected = "gam_v3"
                                      ),
                                      p("Parameter 2:", 
                                        style = "font-weight: bold;"),
                                      checkboxInput(inputId = "gam_previous_comp_mda8_obs_2", 
                                                    label = "Obs MDA8", 
                                                    value = F),
                                      checkboxInput(inputId = "gam_previous_comp_mda8_pred_2", 
                                                    label = "Pred MDA8", 
                                                    value = T),
                                      checkboxInput(inputId = "gam_previous_comp_mda8_smo_2", 
                                                    label = "SMO", 
                                                    value = F),
                                      checkboxInput(inputId = "gam_previous_comp_mda8_resids_2", 
                                                    label = "Residual", 
                                                    value = F),
                                      checkboxInput(inputId = "gam_previous_comp_pm25_2", 
                                                    label = "Obs PM2.5", 
                                                    value = F)
                               ),
                             ),
                             
                             hr(),
                             fluidRow(
                               column(12,
                                      p(style = "font-size: 14px; color: red;",
                                        "The word 'Unmatched' in the figure indicates that 
                                          the smoke day identifications in the two datasets are different.",
                                      )
                               )
                             )
                      ),
                      
                      column(8,
                             echarts4rOutput(outputId = "gam_previous_comp_plot_series"),
                             hr(),
                             fluidRow(
                               column(6,
                                      echarts4rOutput(outputId = "gam_previous_comp_plot_scatter_1")
                               ),
                               column(6,
                                      echarts4rOutput(outputId = "gam_previous_comp_plot_scatter_2")
                               )
                             )
                      )
                    )
                  )
                )
              )
            ),
            
            
            #### Sidebar 5: Discussion ----
            tabItem(
              tabName = "discussion_of_statistics",
              
              tags$iframe(
                src = "Stats_discussion_for_App_v5.pdf",
                class = "pdf-frame"
              )
            ),
            
            #### Sidebar 6: Admin mailing ----
            tabItem(
              tabName = "admin_mailing",
              
              fluidRow(
                column(6, 
                       p(style = "font-size: 16px;",
                         tags$b("Please add the people you want to send email to from the top list to the bottom list."),
                         br()
                       ),
                       hr(),
                       actionButton(inputId = "mailing_btn", 
                                    label = "Collect subscriber list",
                                    icon = icon("address-book")),
                       downloadButton(outputId = "download_mailing_subscribe_list",
                                      label = "Export data",
                                      icon = icon("file-export")),
                       tags$div(style = "margin-top: 10px;"),
                       dataTableOutput(outputId = "mailing_subscribe_list"),
                       actionButton(inputId = "mailing_subscribe_add_to_list",
                                    label = "Add all to the list",
                                    icon = icon("plus")),
                       hr(),
                       tags$div(style = "margin-top: 20px;"),
                       dataTableOutput(outputId = "mailing_subscribe_list_sel"),
                       actionButton(inputId = "mailing_subscribe_list_clear_table", 
                                    label = "Clear list",
                                    icon = icon("trash"))
                ),
                column(6, 
                       p(style = "font-size: 16px;",
                         tags$b("Compose email"),
                         br()
                       ),
                       hr(),
                       textOutput(outputId = "mailing_subscribe_from"),
                       textOutput(outputId = "mailing_subscribe_to"),
                       hr(),
                       textInput(inputId = "mailing_subscribe_title", 
                                 label = "Title", 
                                 placeholder = "Enter the email header here...",
                                 width = "100%",
                                 value = "PM and O3 smoke tool update (Jaffe group, UW)"),
                       textAreaInput(inputId = "mailing_subscribe_body", 
                                     label = "Body", 
                                     placeholder = "Enter the email body here...",
                                     rows = 10,
                                     width = "100%",
                                     value = "
Dear Users and Subscribers,

We hope everyhing goes well.

We will be updating the app between 11:30 AM and 12:00 AM PST today.
If you use the app during this time, the app may suddenly stop or the task may disappear.
If you are doing any work, please stop it in advance.

What's New in the updated Version.

-
-
-

If you have any comments or find any bugs, please feel free to contact us!

Best regards,
Jaffe research group, UW,
Dr. Dan Jaffe and Dr. Haebum Lee.
                                       "),
                       textAreaInput(inputId = "mailing_subscribe_footer", 
                                     label = "Footer", 
                                     placeholder = "Enter the email footer or signature here...",
                                     rows = 10,
                                     width = "100%",
                                     value = "
-------------------------------------------------------------
Dr. Dan Jaffe, Professor (He/him)
Physical Sciences Division
School of STEM
University of Washington-Bothell
18115 Campus Way NE
Bothell, WA 98011-8246
Email: djaffe@uw.edu
-------------------------------------------------------------
Dr. Haebum Lee, Postdoc (He/him)
School of STEM
University of Washington-Bothell
18115 Campus Way NE
Bothell, WA 98011-8246
Email: hblee@uw.edu / haebum1991@gmail.com
-------------------------------------------------------------
                                       "),
                       actionButton(inputId = "mailing_subscribe_send", 
                                    label = "Send email",
                                    icon = icon("paper-plane")),
                       textOutput(outputId = "mailing_subscribe_send_status")
                )
              )
            )
          )
        )
      )
    )
  )
)


# Define server logic ----
user_count <- reactiveVal(0)
server <- function(input, output, session) {
  
  ## Memory usage & Users ----
  observe({
    ps <- ps::ps_handle()
    memory_info <- ps::ps_memory_info(ps)
    used_memory <- memory_info[["rss"]] / 1024 / 1024 / 1024
    used_memory <- sprintf("%.2f", round(used_memory, 2))
    
    output$memory_show <- renderText({
      paste0("Memory usage: ", used_memory, " GB")
    })
    
    invalidateLater(3000, session)
  })
  
  observe({
    isolate({
      current_count <- user_count()
      user_count(current_count + 1)
    })
  })
  
  session$onSessionEnded(function() {
    isolate({
      current_count <- user_count()
      user_count(current_count - 1)
    })
  })
  
  output$user_count <- renderText({
    paste0("Concurrent users: ", user_count())
  })
  
  ## UI guest login page ----  
  data_col_stat <- reactiveVal({
    output_tmp <- rbind(
      data_epa_o3[, ..column_for_site_col],
      data_epa_pm[, ..column_for_site_col]
    )
    output_tmp$site_id <- gsub("aqs_", "", output_tmp$site_id)
    output_tmp
  })
  
  data_gam_stat_guest <- rbind(
    data_stat_gam_v1[, ..column_for_site_gam],
    data_stat_gam_v3[, ..column_for_site_gam],
    data_stat_gam_v3_edm[, ..column_for_site_gam],
    
    ## Hide/Show dataset (2025-02-27)
    # data_stat_gam_v2[, ..column_for_site_gam],
    # data_stat_gam_v2_edm[, ..column_for_site_gam],
    
    data_stat_epa_ember[, ..column_for_site_gam],
    data_stat_pm_cbsa[, ..column_for_site_gam]
  )
  
  data_gam_stat_admin <- rbind(
    data_stat_gam_v1[, ..column_for_site_gam],
    data_stat_gam_v3[, ..column_for_site_gam],
    data_stat_gam_v3_edm[, ..column_for_site_gam],
    
    ## Hide/Show dataset (2025-02-27)
    # data_stat_gam_v2[, ..column_for_site_gam],
    # data_stat_gam_v2_edm[, ..column_for_site_gam],
    
    data_stat_epa_ember[, ..column_for_site_gam],
    data_stat_pm_cbsa[, ..column_for_site_gam]
  )
  
  data_gam_stat <- reactiveVal(data_gam_stat_guest)
  
  user_status <- reactiveValues(show_only_for_admin = F,
                                logged_in = F,
                                logged_in_admin = F)
  
  is_valid_email <- function(email) {
    grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", email)
  }
  
  ### Guest login button ----
  observeEvent(input$login_btn, {
    
    if(input$subscribe_email == "") {
      
      shinyjs::runjs('
        $("#subscribe_email").addClass("highlighted");
        setTimeout(function() {
          $("#subscribe_email").removeClass("highlighted");
        }, 3000);
      ')
      showNotification("Please enter your email address before proceeding.", type = "error", duration = 3)
    } else if(!is_valid_email(input$subscribe_email)) {
      shinyjs::runjs('
        $("#subscribe_email").addClass("highlighted");
        setTimeout(function() {
          $("#subscribe_email").removeClass("highlighted");
        }, 3000);
      ')
      showNotification("Please enter a valid email address.", type = "error", duration = 3)
      
    } else {
      
      showPageSpinner()
      user_status$logged_in <- T
      user_status$show_only_for_admin <- F
      shinyjs::hide(id = "login_page")
      shinyjs::hide(selector = "a[data-value='admin_mailing']")
      shinyjs::hide(selector = "a[data-value='gam_previous_comp']")
      shinyjs::show(id = "ui_main_page")
      
      ## Hide/Show dataset (2025-02-27)
      # shinyjs::hide(id = "gam_previous_show_gam_v2")
      # shinyjs::hide(id = "gam_previous_show_gam_v2_edm")
      # shinyjs::hide(selector = "a[data-value='gam_previous_data_load_gam_v2']")
      # shinyjs::hide(selector = "a[data-value='gam_previous_data_load_gam_v2_edm']")
      # updateCheckboxInput(inputId = "gam_previous_show_gam_v2", value = F)
      # updateCheckboxInput(inputId = "gam_previous_show_gam_v2_edm", value = F)
      
      updateTextInput(session, "epa_my_email", value = "")
      updateTextInput(session, "epa_my_key", value = "")
      updateSelectInput(session, "gam_previous_lyr_gam_ver", choices = guest_dataset)
      updateSelectInput(session, "gam_previous_comp_dataset_1", choices = datacomp_dataset)
      updateSelectInput(session, "gam_previous_comp_dataset_2", choices = datacomp_dataset)
      
      data_gam_stat(data_gam_stat_guest)
      
      Sys.sleep(0.25)
      hidePageSpinner()
      
      tryCatch({
        func_subscribe_to_email(input = input,
                                title = "rsGAM login history",
                                email_from = "hbleershiny@gmail.com",
                                email_to = "haebum1991@gmail.com",
                                cred_file_name = "hbleershiny_credential")
      }, error = function(e) {
        NULL
      })
    }
  })
  
  ### Guest logout button ----
  observeEvent(input$logout_btn, {
    user_status$logged_in <- F
    user_status$logged_in_admin <- F
    shinyjs::hide(selector = "a[data-value='gam_previous_comp']")
  })
  
  ### Guest Login/out status ----
  observe({
    if(user_status$logged_in) {
      shinyjs::show(id = "ui_main_page")
      shinyjs::hide(id = "login_page")
    } else {
      shinyjs::hide(id = "ui_main_page")
      shinyjs::show(id = "login_page")
    }
  })
  
  ### Guest subscribe button ----
  observeEvent(input$subscribe_button, {
    
    if(input$subscribe_email == "") {
      
      shinyjs::runjs('
        $("#subscribe_email").addClass("highlighted");
        setTimeout(function() {
          $("#subscribe_email").removeClass("highlighted");
        }, 3000);
      ')
      showNotification("Please enter your email address before proceeding.", type = "error", duration = 3)
    } else if(!is_valid_email(input$subscribe_email)) {
      shinyjs::runjs('
        $("#subscribe_email").addClass("highlighted");
        setTimeout(function() {
          $("#subscribe_email").removeClass("highlighted");
        }, 3000);
      ')
      showNotification("Please enter a valid email address.", type = "error", duration = 3)
      
    } else {
      showPageSpinner()
      tryCatch({
        func_subscribe_to_email(input = input,
                                title = "New Subscriber Information",
                                email_from = "hbleershiny@gmail.com",
                                email_to = c("hbleershiny@gmail.com",
                                             "haebum1991@gmail.com"),
                                cred_file_name = "hbleershiny_credential")
        output$subscribe_status <- renderText("The information has been successfully sent to our email!")
      }, error = function(e) {
        output$subscribe_status <- renderText("Failed to send the email.")
      })
      hidePageSpinner()
    }
  })
  
  ## UI admin login page ----
  observeEvent(input$login_admin_btn, {
    user_status$logged_in_admin <- T
  })
  
  observe({
    if(user_status$logged_in_admin) {
      shinyjs::hide(id = "ui_main_page")
      shinyjs::hide(id = "login_page")
      shinyjs::show(id = "login_page_admin")
    }
  })
  
  observeEvent(input$back_to_page_admin, {
    user_status$logged_in_admin <- F
    shinyjs::hide(id = "login_page_admin")
    shinyjs::show(id = "ui_main_page")
  })
  
  ### Admin login button ----
  observeEvent(input$login_btn_admin, {
    
    if(input$username == "admin" && input$password == "dan&haebum") {
      
      guest_dataset <- admin_dataset
      
      user_status$logged_in_admin <- T
      user_status$show_only_for_admin <- T
      
      shinyjs::hide(id = "login_page_admin")
      shinyjs::hide(id = "login_page")
      shinyjs::show(selector = "a[data-value='admin_mailing']")
      shinyjs::show(selector = "a[data-value='gam_previous_comp']")
      shinyjs::show(id = "ui_main_page")
      shinyjs::show(selector = "a[data-value='ginfo']")
      
      ## Hide/Show dataset (2025-02-27)
      # shinyjs::show(id = "gam_previous_show_gam_v2")
      # shinyjs::show(id = "gam_previous_show_gam_v2_edm")
      # shinyjs::show(selector = "a[data-value='gam_previous_data_load_gam_v2']")
      # shinyjs::show(selector = "a[data-value='gam_previous_data_load_gam_v2_edm']")
      
      updateTextInput(session, "epa_my_email", value = "haebum1991@gmail.com")
      updateTextInput(session, "epa_my_key", value = "bolekit73")
      updateSelectInput(session, "gam_previous_lyr_gam_ver", choices = guest_dataset)
      updateSelectInput(session, "gam_previous_comp_dataset_1", choices = datacomp_dataset)
      updateSelectInput(session, "gam_previous_comp_dataset_2", choices = datacomp_dataset)
      
      data_gam_stat(data_gam_stat_admin)
      
    } else {
      showNotification("Invalid username or password", type = "error")
    }
  })
  
  ## Sidebar 5: Admin mailing ----
  subscribe_list <- reactiveVal(data.table::data.table(date = character(),
                                                       email = character(),
                                                       affiliation = character(), 
                                                       first = character(),
                                                       last = character()))
  subscribe_list_sel <- reactiveVal(data.table::data.table(date = character(),
                                                           email = character(),
                                                           affiliation = character(), 
                                                           first = character(),
                                                           last = character()))
  
  observe({
    func_site_select_tbl(output = output,
                         output_id = "mailing_subscribe_list",
                         selected_sites_func = subscribe_list(),
                         delete_input_prefix = NULL,
                         locate_input_prefix = NULL,
                         adlist_input_prefix = "adlist_row_email",
                         pageLength = 5)
  })
  
  observe({
    func_site_select_tbl(output = output,
                         output_id = "mailing_subscribe_list_sel",
                         selected_sites_func = subscribe_list_sel(),
                         delete_input_prefix = "delete_row_email",
                         locate_input_prefix = NULL,
                         adlist_input_prefix = NULL,
                         pageLength = 5)
  })
  
  observeEvent(input$mailing_btn, {
    showPageSpinner()
    collected_subscribe_list <- func_subscribe_list_from_email(email_from = "hbleershiny@gmail.com",
                                                               cred_file_name = "hbleershiny_credential")
    subscribe_list(collected_subscribe_list)
    hidePageSpinner()
  })
  
  output$download_mailing_subscribe_list <- downloadHandler(
    filename = function() {
      paste0("subscriber_list_", as.character(Sys.Date()), ".csv")
    },
    content = function(file) {
      tryCatch({
        cat("observe / GAM previous / Layer map / downloading...\n")
        data.table::fwrite(file = file,
                           x = subscribe_list(),
                           row.names = F)
      }, error = function(e) {
        cat("Error - observe / GAM previous / Layer map / downloading: ", e$message, "\n")
      })
    }
  )
  
  observeEvent(input$mailing_subscribe_list_clear_table, {
    func_render_tbl(output = output,
                    output_id = "mailing_subscribe_list_sel",
                    data = subscribe_list_sel(data.table::data.table(date = character(),
                                                                     email = character(),
                                                                     affiliation = character(), 
                                                                     first = character(),
                                                                     last = character())),
                    pageLength = 5)
  }, ignoreInit = T)
  
  observeEvent(input$delete_row_email, {
    tryCatch({
      cat("observeEvent / delete_row_email... \n")
      row_to_remove <- input$delete_row_email
      
      if(!is.null(row_to_remove)) {
        updated_sites <- subscribe_list_sel()
        if(row_to_remove <= nrow(updated_sites)) {
          updated_sites <- updated_sites[-row_to_remove, ]
          rownames(updated_sites) <- NULL
          subscribe_list_sel(updated_sites)
        }
        shinyjs::reset("delete_row_email")
      }
    }, error = function(e) {
      cat("Error - observeEvent / delete_row_email: ", e$message, "\n")
      NULL
    })
  }, ignoreNULL = T, ignoreInit = T)
  
  observeEvent(input$adlist_row_email, {
    tryCatch({
      row_to_add <- input$adlist_row_email
      
      if(!is.null(row_to_add)) {
        cat("Adding row to selected_sites_gam...\n")
        
        selected_site <- subscribe_list()[row_to_add, ]
        existing_sites <- subscribe_list_sel()
        updated_sites <- distinct(rbind(existing_sites, selected_site))
        rownames(updated_sites) <- NULL
        subscribe_list_sel(updated_sites)
      }
    }, error = function(e) {
      cat("Error - observeEvent / adlist_row_email: ", e$message, "\n")
      NULL
    })
  }, ignoreNULL = T, ignoreInit = T)
  
  observeEvent(input$mailing_subscribe_add_to_list, {
    if(nrow(subscribe_list()) > 0) {
      updated_sites <- distinct(rbind(subscribe_list(),
                                      subscribe_list_sel()))
      rownames(updated_sites) <- NULL
      subscribe_list_sel(updated_sites)
    }
  })
  
  observe({
    output$mailing_subscribe_from <- renderText("From: hbleershiny@gmail.com")
    output$mailing_subscribe_to <- renderText(c("To: ", paste0(subscribe_list_sel()$email, collapse = "; ")))
  })
  
  observeEvent(input$mailing_subscribe_send, {
    showPageSpinner()
    tryCatch({
      func_subscribe_email_notice(contents_title = input$mailing_subscribe_title,
                                  contents_body = input$mailing_subscribe_body,
                                  contents_footer = input$mailing_subscribe_footer,
                                  email_from = "hbleershiny@gmail.com",
                                  email_to = subscribe_list_sel()$email,
                                  cred_file_name = "hbleershiny_credential")
      output$mailing_subscribe_send_status <- renderText("Successfully completed sending mail!")
    }, error = function(e) {
      output$mailing_subscribe_send_status <- renderText("Failed to send the email.")
      showNotification("Please add email list...", type = "error", duration = 3)
    })
    hidePageSpinner()
  })
  
  
  ## Sidebar 3: GAM manual ----
  ### Get EPA API key ----
  observeEvent(input$epa_my_get_key, {
    if(input$epa_my_email == "") {
      shinyjs::runjs('
        $("#epa_my_email").addClass("highlighted");
        setTimeout(function() {
          $("#epa_my_email").removeClass("highlighted");
        }, 3000);
      ')
      showNotification("Please enter your email address to get EPA-API key.", type = "error", duration = 3)
    } else if(!is_valid_email(input$epa_my_email)) {
      shinyjs::runjs('
        $("#epa_my_email").addClass("highlighted");
        setTimeout(function() {
          $("#epa_my_email").removeClass("highlighted");
        }, 3000);
      ')
      showNotification("Please enter a valid email address format.", type = "error", duration = 3)
    } else if(input$epa_my_key != "") {
      shinyjs::runjs('
        $("#epa_my_key").addClass("highlighted");
        setTimeout(function() {
          $("#epa_my_key").removeClass("highlighted");
        }, 3000);
      ')
      showNotification("Please leave the [API key] box blank to receive an EPA-API key.", type = "error", duration = 3)
    } else {
      url <- paste0("https://aqs.epa.gov/data/api/signup?email=", input$epa_my_email)
      js_code <- sprintf("window.open('%s');", url)
      shinyjs::runjs(js_code)
      shinyjs::runjs('alert("Please check your email and enter your key in the [API key] box.");')
    }
  })
  
  ## Create and load maps ----
  output$map_site_col <- renderLeaflet(func_render_map(state_boundaries))
  output$map_site_gam <- renderLeaflet(func_render_map(state_boundaries))
  output$map_site_lyr <- renderLeaflet(func_render_map(state_boundaries))
  
  print("Load map_site_col...")
  outputOptions(output, "map_site_col", suspendWhenHidden = F, priority = 10)
  
  print("Load map_site_gam...")
  outputOptions(output, "map_site_gam", suspendWhenHidden = F, priority = 10)
  
  print("Load map_site_lyr...")
  outputOptions(output, "map_site_lyr", suspendWhenHidden = F, priority = 10)
  
  
  ## Site search reactive ----
  site_search_term_manual <- reactive(unique(c(
    data_col_stat()[order(state), state], 
    data_col_stat()[order(site_id), site_id], 
    data_col_stat()[order(site_name), site_name]
  )))
  
  site_search_term_previous <- reactive(unique(c(
    data_gam_stat()[order(state), state], 
    data_gam_stat()[order(AQS), AQS], 
    data_gam_stat()[order(site_name), site_name]
  )))
  
  ## Toggle site reactive ----
  func_toggle_sites_react <- reactiveValues(existing_groups = character(0))
  
  ## Sidebar 3: GAM manual ----
  ### Sub-tab: Data collection ----
  #### Site information ----
  observe({
    tryCatch({
      cat("observe / Data collection / Part 1...\n")
      
      func_toggle_sites(map_id = "map_site_col",
                        reactive_id = func_toggle_sites_react,
                        show = input$gam_manual_show_epa_sites_o3,
                        data = data_epa_o3,
                        radius = 8,
                        border_color = "black",
                        border_lwd = 0.5,
                        fill_color = adjustcolor(data_epa_o3$color, alpha.f = 0.75),
                        group = "epa_sites_o3")
      func_toggle_sites(map_id = "map_site_col",
                        reactive_id = func_toggle_sites_react,
                        show = input$gam_manual_show_epa_sites_pm,
                        data = data_epa_pm,
                        radius = 8,
                        border_color = adjustcolor(data_epa_pm$color, alpha.f = 1),
                        border_lwd = 3.5,
                        fill_color = adjustcolor(data_epa_pm$color, alpha.f = 0),
                        group = "epa_sites_pm")
      func_toggle_sites(map_id = "map_site_col", 
                        reactive_id = func_toggle_sites_react,
                        show = input$gam_manual_show_iem_sites, 
                        data = data_iem_asos, 
                        radius = 6,
                        border_color = "black",
                        border_lwd = 0.5,
                        fill_color = adjustcolor(data_iem_asos$color, alpha.f = 0.75),
                        group = "iem_sites")
      
    }, error = function(e) {
      cat("Error - observe / Data collection / Part 1: ", e$message, "\n")
      NULL
    })
  })
  
  #### Selected sites by click ----
  selected_sites_col <- reactiveVal(data.table::data.table(source = character(),
                                                           state = character(),
                                                           site_id = character(), 
                                                           lon = numeric(),
                                                           lat = numeric(),
                                                           site_name = character(),
                                                           EPA_id = character()))
  
  observeEvent(input$map_site_col_click, {
    tryCatch({
      cat("observeEvent / map_site_col_click... \n")
      
      click_coords <- c(input$map_site_col_click$lng, 
                        input$map_site_col_click$lat)
      
      site_info_list <- list(
        if(input$gam_manual_show_iem_sites) {func_find_nearest_site(click_coords,
                                                                    data_iem_asos[, ..column_for_site_col],
                                                                    click_radius)},
        if(input$gam_manual_show_epa_sites_o3) {func_find_nearest_site(click_coords,
                                                                       data_epa_o3[, ..column_for_site_col], 
                                                                       click_radius)},
        if(input$gam_manual_show_epa_sites_pm) {func_find_nearest_site(click_coords, 
                                                                       data_epa_pm[, ..column_for_site_col], 
                                                                       click_radius)}
      )
      
      site_info <- dplyr::bind_rows(site_info_list)
      
      if(nrow(site_info) > 0) {
        updated_sites <- distinct(rbind(selected_sites_col(), site_info))
        updated_sites$site_id <- gsub("aqs_", "", updated_sites$site_id)
        updated_sites <- distinct(updated_sites)
        rownames(updated_sites) <- NULL
        selected_sites_col(updated_sites)
      }
      
    }, error = function(e) {
      cat("Error - observeEvent / map_site_col_click: ", e$message, "\n")
      NULL
    })
    
  }, ignoreInit = T)
  
  observeEvent(input$gam_manual_clear_table, {
    
    leaflet::leafletProxy("map_site_col") %>%
      leaflet::clearGroup("highlight_marker")
    
    selected_sites_col(data.table::data.table(source = character(),
                                              state = character(),
                                              site_id = character(), 
                                              lon = numeric(),
                                              lat = numeric(),
                                              site_name = character(),
                                              EPA_id = character()))
  }, ignoreInit = T)
  
  observe({
    func_site_select_tbl(output = output, 
                         output_id = "gam_manual_selected_sites",
                         selected_sites_func = selected_sites_col(), 
                         delete_input_prefix = "delete_row_col",
                         locate_input_prefix = "locate_row_col", 
                         adlist_input_prefix = NULL,
                         pageLength = 5,
                         lengthChange = F)
  })
  
  observeEvent(input$delete_row_col, {
    tryCatch({
      cat("observeEvent / delete_row_col... \n")
      
      row_to_remove <- input$delete_row_col
      
      if(!is.null(row_to_remove)) {
        
        leaflet::leafletProxy("map_site_col") %>%
          leaflet::clearGroup("highlight_marker")
        
        updated_sites <- selected_sites_col()
        
        if(row_to_remove <= nrow(updated_sites)) {
          updated_sites <- updated_sites[-row_to_remove, ]
          rownames(updated_sites) <- NULL
          selected_sites_col(updated_sites)
        }
        shinyjs::reset("delete_row_col")
      }
      
    }, error = function(e) {
      cat("Error - observeEvent / delete_row_col: ", e$message, "\n")
      NULL
    })
  }, ignoreNULL = T, ignoreInit = T)
  
  func_site_location_status_col <- reactiveValues(is_highlighted = F, 
                                                  current_row = NULL)
  observeEvent(input$locate_row_col, {
    func_site_location(input = input, 
                       input_id = "locate_row_col", 
                       reactive_id = func_site_location_status_col,
                       selected_sites_func = selected_sites_col(), 
                       map_id = "map_site_col")
  }, ignoreNULL = T, ignoreInit = T)
  
  
  #### For advanced searching ----
  selected_sites_col_adv <- reactive({
    search_input <- input$gam_manual_search_input
    data_col_stat()[
      stri_detect_regex(state, search_input, case_insensitive = T) |
        stri_detect_regex(site_id, search_input, case_insensitive = T) |
        stri_detect_regex(site_name, search_input, case_insensitive = T)
    ]
  })
  
  observe({
    updateSelectizeInput(session,
                         inputId = "gam_manual_search_input",
                         choices = site_search_term_manual(),
                         selected = NULL,
                         server = F)
  })
  
  observe({
    func_site_select_tbl(output = output, 
                         output_id = "gam_manual_search_results", 
                         selected_sites_func = selected_sites_col_adv(), 
                         delete_input_prefix = NULL, 
                         locate_input_prefix = "locate_row_col_adv", 
                         adlist_input_prefix = "adlist_row_col_adv", 
                         pageLength = 2, 
                         lengthChange = F)
  })
  
  observeEvent(input$adlist_row_col_adv, {
    tryCatch({
      row_to_add <- input$adlist_row_col_adv
      
      if(!is.null(row_to_add)) {
        cat("Adding row to selected_sites_col...\n")
        
        selected_site <- selected_sites_col_adv()[row_to_add, ]
        existing_sites <- selected_sites_col()
        updated_sites <- distinct(rbind(existing_sites, selected_site))
        rownames(updated_sites) <- NULL
        selected_sites_col(updated_sites)
      }
    }, error = function(e) {
      cat("Error - observeEvent / adlist_row_col_adv: ", e$message, "\n")
      NULL
    })
  }, ignoreNULL = T, ignoreInit = T)
  
  observeEvent(input$gam_manual_button_add_to_list, {
    if(nrow(selected_sites_col_adv()) > 0) {
      updated_sites <- distinct(rbind(selected_sites_col(), 
                                      selected_sites_col_adv()))
      rownames(updated_sites) <- NULL
      selected_sites_col(updated_sites)
    }
  })
  
  func_site_location_status_col_adv <- reactiveValues(is_highlighted = F, 
                                                      current_row = NULL)
  observeEvent(input$locate_row_col_adv, {
    func_site_location(input = input, 
                       input_id = "locate_row_col_adv", 
                       reactive_id = func_site_location_status_col_adv,
                       selected_sites_func = selected_sites_col_adv(), 
                       map_id = "map_site_col")
  }, ignoreNULL = T, ignoreInit = T)
  
  
  #### Data downloading & merging ----
  src_col <- c("o3", "pm", "asos", "merge")
  dataCall_col <- reactiveValues(o3 = NULL,
                                 pm = NULL,
                                 asos = NULL,
                                 merge = NULL)
  dataCall_pmcrit <- reactiveValues(pm = NULL)
  
  observe({
    tryCatch({    
      sdate <- as.Date(input$date_start_col)
      edate <- as.Date(input$date_end_col)
      
      if(sdate > edate) {
        edate <- as.Date(input$date_start_col)
        sdate <- as.Date(input$date_end_col)
      }
      
      month_check <- seq.Date(from = sdate,
                              to = edate,
                              by = "month")
      month_list <- seq(1, 12)
      month_check <- lubridate::month(month_check) %>% unique()
      month_uncheck <- month_list[!month_list %in% month_check]
      
      lapply(month_check, function(i) {
        updateCheckboxInput(inputId = paste0("month_selection_", i), 
                            label = i, 
                            value = T)
      })
      lapply(month_uncheck, function(i) {
        updateCheckboxInput(inputId = paste0("month_selection_", i), 
                            label = i, 
                            value = F)
      })
    }, error = function(e) {
      NULL
    })
  })
  
  observeEvent(input$gam_manual_run_col, {
    
    if(input$epa_my_email == "" | input$epa_my_key == "") {
      shinyjs::runjs('
        $("#epa_my_email").addClass("highlighted");
        setTimeout(function() {
          $("#epa_my_email").removeClass("highlighted");
        }, 3000);
        $("#epa_my_key").addClass("highlighted");
        setTimeout(function() {
          $("#epa_my_key").removeClass("highlighted");
        }, 3000);
      ')
      showNotification("Please type your email and API key.", 
                       type = "error", duration = 3)
    } else {
      if(nrow(selected_sites_col()) < 1) {
        shinyjs::runjs('
        $("#gam_manual_selected_sites").addClass("highlighted");
        setTimeout(function() {
        $("#gam_manual_selected_sites").removeClass("highlighted");
        }, 3000);
        ')
        showNotification("No sites were selected.", 
                         type = "error", duration = 3)
      } else {
        
        sdate_format <- grepl("^\\d{4}-\\d{2}-\\d{2}$", input$date_start_col)
        edate_format <- grepl("^\\d{4}-\\d{2}-\\d{2}$", input$date_end_col)
        
        if(!sdate_format) {
          shinyjs::runjs('
          $("#date_start_col").addClass("highlighted");
          setTimeout(function() {
          $("#date_start_col").removeClass("highlighted");
          }, 3000);
          ')
          showNotification("Invalid date format. Please use yyyy-mm-dd format.", 
                           type = "error", duration = 3)
          return()
        }
        
        if(!edate_format) {
          shinyjs::runjs('
          $("#date_end_col").addClass("highlighted");
          setTimeout(function() {
          $("#date_end_col").removeClass("highlighted");
          }, 3000);
          ')
          showNotification("Invalid date format. Please use yyyy-mm-dd format.", 
                           type = "error", duration = 3)
          return()
        }
        
        if(as.Date(input$date_start_col) > as.Date(input$date_end_col)) {
          shinyjs::runjs('
          $("#date_start_col").addClass("highlighted");
          setTimeout(function() {
          $("#date_start_col").removeClass("highlighted");
          }, 3000);
          $("#date_end_col").addClass("highlighted");
          setTimeout(function() {
          $("#date_end_col").removeClass("highlighted");
          }, 3000);
          ')
          showNotification("Please check the date range. The range (start and end) is incorrect.", 
                           type = "error", duration = 3)
          return()
        }
        
        showPageSpinner()
        tryCatch({
          cat("observeEvent / gam_manual_run_col... \n")
          
          output_o3 <- NULL
          output_pm <- NULL
          output_asos <- NULL
          
          target_o3 = 
            if(!is.null(selected_sites_col()) && nrow(selected_sites_col()) > 0) {
              selected_sites_col()[selected_sites_col()$source == "EPA_o3",]
            } else {
              NULL
            }
          
          target_pm = 
            if(!is.null(selected_sites_col()) && nrow(selected_sites_col()) > 0) {
              selected_sites_col()[selected_sites_col()$source == "EPA_pm",]
            } else {
              NULL
            }
          
          target_asos = 
            if(!is.null(selected_sites_col()) && nrow(selected_sites_col()) > 0) {
              selected_sites_col()[selected_sites_col()$source == "IEM_asos",]
            } else {
              NULL
            }
          
          date_range <- c(input$date_start_col, 
                          input$date_end_col)
          
          sel_mt <- which(sapply(1:12, function(i) input[[paste0("month_selection_", i)]]))
          
          ## IEM ASOS
          tryCatch({
            if(!is.null(target_asos)) {
              
              output_asos <- func_iem_asos(id_asos = target_asos$site_id,
                                           date_range = date_range)
              output_asos <- output_asos[!is.na(output_asos$lat),]
              output_asos <- output_asos[!is.na(output_asos$lon),]
              rownames(output_asos) <- NULL
              
              colnames(output_asos)[colnames(output_asos) == "site_id"] <- "ASOS_id"
              output_asos <- func_date_select(output_asos, mt = sel_mt)
              
              if(nrow(output_asos) == 0) {
                output_asos <- NULL
              }
            } else {
              output_asos <- NULL
            }
          }, error = function(e) {
            output_asos <- NULL
          })
          
          ## EPA O3
          tryCatch({
            if(!is.null(target_o3)) {
              
              data_list <- c()
              for(i in 1:length(target_o3$site_id)) {
                output_tmp <- func_epa_api(
                  id_aqs = target_o3$site_id[i], 
                  param = target_o3$EPA_id[i],
                  date_range = date_range,
                  my_key = input$epa_my_key,
                  my_email = input$epa_my_email
                )
                output_tmp$var_code <- target_o3$EPA_id[i]
                
                colnames(output_tmp)[colnames(output_tmp) == "o3"] <- "MDA8O3"
                
                data_list[[i]] <- output_tmp
              }
              
              output_tmp <- do.call(rbind, data_list)
              output_tmp$AQS_code <- gsub("aqs_", "", output_tmp$AQS_code)
              colnames(output_tmp)[colnames(output_tmp) == "AQS_code"] <- "AQS_O3"
              colnames(output_tmp)[colnames(output_tmp) == "var_code"] <- "EPA_id"
              output_tmp <- output_tmp[output_tmp$event_type %in% c("Included", "None"),]
              rownames(output_tmp) <- NULL
              
              output_o3 <- output_tmp
              output_o3 <- func_date_select(output_o3, mt = sel_mt)
              
              if(nrow(output_o3) == 0) {
                output_o3 <- NULL
              }
              
              rm(output_tmp,
                 data_list)
              gc()
              
            } else{
              output_o3 <- NULL
            }
          }, error = function(e) {
            output_o3 <- NULL
          })
          
          ## EPA PM
          tryCatch({
            if(!is.null(target_pm)) {
              
              data_list <- c()
              for(i in 1:length(target_pm$site_id)) {
                output_tmp <- func_epa_api(
                  id_aqs = target_pm$site_id[i], 
                  param = target_pm$EPA_id[i],
                  date_range = date_range,
                  my_key = input$epa_my_key,
                  my_email = input$epa_my_email
                )
                output_tmp$var_code <- target_pm$EPA_id[i]
                
                if(target_pm$EPA_id[i] == 88101) {
                  colnames(output_tmp)[colnames(output_tmp) == "pm25_88101"] <- "PM2.5"
                }
                
                if(target_pm$EPA_id[i] == 88502) {
                  colnames(output_tmp)[colnames(output_tmp) == "pm25_88502"] <- "PM2.5"
                }
                
                data_list[[i]] <- output_tmp
              }
              
              output_tmp <- do.call(rbind, data_list)
              output_tmp$AQS_code <- gsub("aqs_", "", output_tmp$AQS_code)
              colnames(output_tmp)[colnames(output_tmp) == "AQS_code"] <- "AQS_PM"
              colnames(output_tmp)[colnames(output_tmp) == "var_code"] <- "EPA_id"
              output_tmp <- output_tmp[output_tmp$event_type %in% c("Included", "None"),]
              rownames(output_tmp) <- NULL
              
              output_pm <- output_tmp
              output_pm <- func_date_select(output_pm, mt = sel_mt)
              
              if(nrow(output_pm) == 0) {
                output_pm <- NULL
              }
              
              rm(output_tmp,
                 data_list)
              gc()
              
            } else{
              output_pm <- NULL
            }
            
          }, error = function(e) {
            output_pm <- NULL
          })
          
          ## rendering
          func_render_tbl(output = output, 
                          output_id = "data_col_o3",
                          data = output_o3, 
                          pageLength = 5)
          func_render_tbl(output = output, 
                          output_id = "data_col_pm",
                          data = output_pm, 
                          pageLength = 5)
          func_render_tbl(output = output, 
                          output_id = "data_col_asos",
                          data = output_asos, 
                          pageLength = 5)
          
          dataCall_col$o3 <- output_o3
          dataCall_col$pm <- output_pm
          dataCall_col$asos <- output_asos
          output_merge <- NULL
          
          ## Merge data
          if(!is.null(output_o3)) {
            output_o3_info <- output_o3[, c("AQS_O3", "cbsa", "cbsa_code")] %>% distinct()
            output_o3 <- output_o3[, !colnames(output_o3) %in% c("cbsa", "cbsa_code", "event_type", "poc")]
            output_o3 <- output_o3 %>%
              dplyr::group_by(date, state, AQS_O3, site_name, lon, lat, EPA_id) %>%
              dplyr::summarise_all(funs(mean), na.rm = T) %>%
              data.frame()
            output_o3 <- merge(output_o3,
                               output_o3_info,
                               by = "AQS_O3")
            colnames(output_o3)[colnames(output_o3) == "EPA_id"] <- "EPA_id_o3"
            output_o3$MDA8O3 <- round(output_o3$MDA8O3, 1) 
            
            output_o3 <- output_o3 %>% mutate(YEAR = lubridate::year(output_o3$date),
                                              MONTH = lubridate::month(output_o3$date),
                                              DOY = lubridate::yday(output_o3$date)-1,
                                              WDAY = lubridate::wday(output_o3$date)) %>% 
              data.frame()
          }
          
          if(!is.null(output_pm)) {
            output_pm_info <- output_pm[, c("AQS_PM", "cbsa", "cbsa_code")] %>% distinct()
            output_pm <- output_pm[, !colnames(output_pm) %in% c("cbsa", "cbsa_code", "event_type", "poc")]
            output_pm <- output_pm %>%
              dplyr::group_by(date, state, AQS_PM, site_name, lon, lat, EPA_id) %>%
              dplyr::summarise_all(funs(mean), na.rm = T) %>%
              data.frame()
            output_pm <- merge(output_pm,
                               output_pm_info,
                               by = "AQS_PM")
            colnames(output_pm)[colnames(output_pm) == "EPA_id"] <- "EPA_id_pm"
            output_pm$PM2.5 <- round(output_pm$PM2.5, 1) 
            output_pm$PM2.5[output_pm$PM2.5 <= 0] <- NA
            
            output_pm <- output_pm %>% mutate(YEAR = lubridate::year(output_pm$date),
                                              MONTH = lubridate::month(output_pm$date),
                                              DOY = lubridate::yday(output_pm$date)-1,
                                              WDAY = lubridate::wday(output_pm$date)) %>% 
              data.frame()
            dataCall_pmcrit$pm <- output_pm
          }
          
          if(!is.null(output_o3)) {
            output_merge <- output_o3
            
            if(!is.null(output_pm)) {
              untarget <- c("state", "site_name", "cbsa", "cbsa_code", "YEAR", "MONTH", "DOY", "WDAY")
              output_merge <- func_data_merging(output_merge, 
                                                output_pm[, !colnames(output_pm) %in% untarget])
              colnames(output_merge)[colnames(output_merge) == "dist_km"] <- "AQS_dist_km"
            }
            
            if(!is.null(output_asos)) {
              output_merge <- func_data_merging(output_merge, output_asos)
              colnames(output_merge)[colnames(output_merge) == "dist_km"] <- "ASOS_dist_km"
            }
            
          } else if(is.null(output_o3) & !is.null(output_pm)) {
            output_merge <- output_pm
            
            if(!is.null(output_asos)) {
              output_merge <- func_data_merging(output_merge, output_asos)
              colnames(output_merge)[colnames(output_merge) == "dist_km"] <- "ASOS_dist_km"
            }
          } else if(is.null(output_o3) & is.null(output_pm) & !is.null(output_asos)) {
            output_asos <- output_asos %>% mutate(YEAR = lubridate::year(output_asos$date),
                                                  MONTH = lubridate::month(output_asos$date),
                                                  DOY = lubridate::yday(output_asos$date)-1,
                                                  WDAY = lubridate::wday(output_asos$date)) %>% 
              data.frame()
            output_merge <- output_asos
          } else {
            output_merge <- NULL
          }
          
          if(!is.null(output_merge)) {
            existing_columns <- base::intersect(c("date", "YEAR", "MONTH", "DOY", "WDAY",
                                                  "state", "AQS_O3", "lat", "lon", "site_name",
                                                  "cbsa", "cbsa_code", "EPA_id_o3", "MDA8O3",
                                                  "AQS_PM", "AQS_dist_km", "EPA_id_pm", "PM2.5",
                                                  "ASOS_id", "ASOS_dist_km"),
                                                colnames(data))
            
            output_merge <- output_merge %>%
              dplyr::select(all_of(existing_columns),
                            everything()) %>% 
              data.frame()
            
            if("AQS_O3" %in% colnames(output_merge)) {
              output_merge <- output_merge[order(output_merge$AQS_O3, output_merge$date),]
            } else {
              if("AQS_PM" %in% colnames(output_merge)) {
                output_merge <- output_merge[order(output_merge$AQS_PM, output_merge$date,),]
              } else {
                if("ASOS_id" %in% colnames(output_merge)) {
                  output_merge <- output_merge[order(output_merge$ASOS_id, output_merge$date),]
                }
              }
            }
            rownames(output_merge) <- NULL
          } 
          
          func_render_tbl(output = output,
                          output_id = "data_col_merge",
                          data = output_merge, 
                          pageLength = 5)
          dataCall_col$merge <- output_merge
          
          rm(output_o3,
             output_pm,
             output_asos,
             output_merge)
          gc()
          
        }, error = function(e) {
          cat("Error - observeEvent / gam_manual_run_col: ", e$message, "\n")
          NULL
        })
        
        hidePageSpinner()      
      }
    }
  }, ignoreInit = T)
  
  
  observeEvent({
    list(input$gam_manual_run_col)
  }, {
    lapply(src_col, function(src) {
      
      output_id <- paste0("data_ui_", src)
      data_id <- paste0("data_col_", src)
      download_ui_id <- paste0("download_ui_", src)
      download_button_id <- paste0("download_", src)
      
      output[[output_id]] <- renderUI({
        data <- dataCall_col[[src]]
        
        if(is.null(data) || nrow(data) == 0) {
          h4("No data available in table",
             style = "text-align: center; color: black;")
        } else {
          div(
            style = "overflow-y: auto; overflow-x: auto;",
            DT::dataTableOutput(data_id)
          )
        }
      })
      
      output[[download_ui_id]] <- renderUI({
        func_render_download_ui(
          data = dataCall_col[[src]],
          button_id = download_button_id, 
          label = paste("Save data")
        )
      })
      
      output[[download_button_id]] <- func_create_download_handler(
        data = dataCall_col[[src]],
        date_range = c(input$date_start_col, input$date_end_col),
        prefix = data_id
      )
    })
  }, ignoreInit = T)
  
  ### Sub-tab: HMS & PM-crit ----
  observe({
    dataCall_col$pmcrit <- NULL
    func_render_tbl(output = output,
                    output_id = "data_col_pmcrit",
                    data = NULL,
                    pageLength = 20)
    if(!is.null(dataCall_col$pm)) {
      shinyjs::show(selector = "a[data-value='gam_manual_hms_pmcrit']")
    } else {
      shinyjs::hide(selector = "a[data-value='gam_manual_hms_pmcrit']")
    }
  })
  
  #### Calculation PM2.5-crit ----
  observeEvent(input$gam_manual_pmcrit_run, {
    showPageSpinner()
    
    output_pm <- dataCall_pmcrit$pm
    output_merge <- dataCall_col$merge
    
    output_pm$date <- as.character(output_pm$date)
    output_merge$date <- as.character(output_merge$date)
    
    date_list_pm <- as.character(unique(output_pm$date))
    date_list_merge <- as.character(unique(output_merge$date))
    
    point_sp_pm <- distinct(output_pm[, c("lon", "lat")]) %>%
      sf::st_as_sf(coords = c("lon", "lat")) %>%
      sf::st_set_crs(4326)
    
    point_sp_merge <- distinct(output_merge[, c("lon", "lat")]) %>%
      sf::st_as_sf(coords = c("lon", "lat")) %>%
      sf::st_set_crs(4326)
    
    point_sp_pm <- sf::as_Spatial(point_sp_pm)
    point_sp_merge <- sf::as_Spatial(point_sp_merge)
    
    hms_date <- unique(c(unique(date_list_pm),
                         unique(date_list_merge)))
    hms_year <- unique(c(unique(lubridate::year(date_list_pm)),
                         unique(lubridate::year(date_list_merge))))
    hms_file <- paste0("Processed_hms_list_", hms_year, ".rds")
    
    hms_data <- lapply(hms_file, function(q) {
      readRDS(file.path("src/data", q))
    })
    hms_data <- do.call(base::c, hms_data)
    
    hms_date_not_incl <- hms_date[!hms_date %in% names(hms_data)]
    if(length(hms_date_not_incl) != 0) {
      for(i_date in hms_date_not_incl) {
        hms_data[[i_date]] <- func_hms_smoke(date = i_date,
                                             base_path = main_path)
      }
    }
    
    output_hms_pm <- func_hms_smoke_count(date_list = date_list_pm, 
                                          point_sp = point_sp_pm, 
                                          hms_data = hms_data, 
                                          base_path = main_path)
    output_hms_merge <- func_hms_smoke_count(date_list = date_list_merge, 
                                             point_sp = point_sp_merge, 
                                             hms_data = hms_data, 
                                             base_path = main_path)
    
    output_pmcrit <- data.table::merge.data.table(output_pm[,  c("date", "lon", "lat", "PM2.5", "AQS_PM")],
                                                  output_hms_pm,
                                                  by = c("date", "lon", "lat"),
                                                  all.x = T)
    
    if(input$gam_manual_pmcrit_exc_indep) {
      exc_indep <- T
    } else {
      exc_indep <- F
    }
    
    output_pmcrit_list <- split(output_pmcrit,
                                output_pmcrit$AQS_PM)
    
    output_pmcrit_list <- lapply(output_pmcrit_list, function(q) {
      options(warn = -1)
      func_smoke_detection(q, 
                           mt = unique(month(q$date)),
                           method = input$gam_manual_pmcrit_period_selection,
                           exc_indep = exc_indep,
                           x_pm = "PM2.5", 
                           x_hms = "hms")
    })
    
    output_pmcrit <- do.call(rbind, 
                             output_pmcrit_list)
    
    if(input$gam_manual_pmcrit_method_selection == "meansd") {
      var_target <- c(c("date", "AQS_PM", "pm_crit_m1p0s", "pm_id_m1p0s")) 
    }
    if(input$gam_manual_pmcrit_method_selection == "pm1p0mad") {
      var_target <- c(c("date", "AQS_PM", "pm_crit_m1p0m", "pm_id_m1p0m")) 
    }    
    if(input$gam_manual_pmcrit_method_selection == "pm0p5mad") {
      var_target <- c(c("date", "AQS_PM", "pm_crit_m0p5m", "pm_id_m0p5m")) 
    }
    
    output_pmcrit <- output_pmcrit[, var_target]
    setnames(output_pmcrit, 3, "PM2.5_Crit")
    setnames(output_pmcrit, 4, "PM2.5_ID")
    output_pmcrit$PM2.5_Crit <- round(output_pmcrit$PM2.5_Crit, 2)
    
    output_merge <- data.table::merge.data.table(output_merge,
                                                 output_hms_merge,
                                                 by = c("date", "lon", "lat"),
                                                 all.x = T)
    output_pmcrit <- data.table::merge.data.table(output_merge,
                                                  output_pmcrit,
                                                  by = c("date", "AQS_PM"),
                                                  all.x = T)
    output_pmcrit$smoke <- ifelse(output_pmcrit$hms == 1 & output_pmcrit$PM2.5_ID == 1, 1, 0)
    
    if("MDA8O3" %in% colnames(output_pmcrit)) {
      output_pmcrit <- output_pmcrit[!is.na(output_pmcrit$MDA8O3),]
      output_pmcrit <- output_pmcrit[output_pmcrit$MDA8O3 > 0, ]
    }
    
    output_pmcrit <- output_pmcrit[!is.na(output_pmcrit$PM2.5),]
    output_pmcrit <- output_pmcrit[output_pmcrit$PM2.5 > 0, ]
    
    if("AQS_O3" %in% colnames(output_pmcrit)) {
      output_pmcrit <- output_pmcrit[order(output_pmcrit$AQS_O3, output_pmcrit$date),]
    } else {
      if("AQS_PM" %in% colnames(output_pmcrit)) {
        output_pmcrit <- output_pmcrit[order(output_pmcrit$AQS_PM, output_pmcrit$date,),]
      } else {
        if("ASOS_id" %in% colnames(output_pmcrit)) {
          output_pmcrit <- output_pmcrit[order(output_pmcrit$ASOS_id, output_pmcrit$date),]
        }
      }
    }
    rownames(output_pmcrit) <- NULL
    
    func_render_tbl(output = output,
                    output_id = "data_col_pmcrit",
                    data = output_pmcrit, 
                    pageLength = 10)
    dataCall_col$pmcrit <- output_pmcrit
    
    output_id <- "data_ui_pmcrit"
    data_id <- "data_col_pmcrit"
    download_ui_id <- "download_ui_pmcrit"
    download_button_id <- "download_pmcrit"
    
    output[[output_id]] <- renderUI({
      data <- dataCall_col[["pmcrit"]]
      
      if(is.null(data) || nrow(data) == 0) {
        h4("No data available in table",
           style = "text-align: center; color: black;")
      } else {
        div(
          style = "overflow-y: auto; overflow-x: auto;",
          DT::dataTableOutput(data_id)
        )
      }
    })
    
    output[[download_ui_id]] <- renderUI({
      func_render_download_ui(
        data = dataCall_col[["pmcrit"]],
        button_id = download_button_id, 
        label = paste("Save data")
      )
    })
    
    output[[download_button_id]] <- func_create_download_handler(
      data = dataCall_col[["pmcrit"]],
      date_range = c(input$date_start_col, input$date_end_col),
      prefix = data_id
    )
    
    rm(point_sp_merge, point_sp_pm,
       date_list_merge, date_list_pm,
       output_pm, output_merge, output_pmcrit,
       output_hms_merge, output_hms_pm,
       hms_year, hms_file)
    gc()
    
    hidePageSpinner()
  })
  
  
  ### Sub-tab: GAM setup & Run ----
  gam_data <- reactiveVal(NULL)
  gam_data_name <- reactiveVal("")
  
  observeEvent(input$gam_manual_load_sample, {
    gam_data(sample_v1)
    gam_data_name("gam_manual_sample.csv")
    gam_manual_gam_setup(session = session,
                         output = output)
  }, ignoreInit = T)
  
  observeEvent(input$gam_manual_load_merged_data, {
    if(!is.null(dataCall_col$pmcrit)) {
      gam_data(dataCall_col$pmcrit)
    } else {
      gam_data(dataCall_col$merge)
    }
    gam_data_name("Data collection: Merged_data")
    gam_manual_gam_setup(session = session,
                         output = output)
  }, ignoreInit = T)
  
  observeEvent(input$gam_manual_load_clear, {
    gam_data(NULL)
    gam_data_name("")
    x_int_inputs$inputs <- list()
    removeTab(inputId = "gam_manual_tabs",
              target = "gam_results",
              session = session)
    removeTab(inputId = "gam_manual_tabs", 
              target = "gam_setup",
              session = session)
  }, ignoreInit = T)
  
  observeEvent(input$gam_manual_file, {
    req(input$gam_manual_file)
    gam_data(data.table::fread(input$gam_manual_file$datapath))
    gam_data_name(input$gam_manual_file$name)
    gam_manual_gam_setup(session = session,
                         output = output)
  }, ignoreInit = T)
  
  output$csv_table <- DT::renderDataTable({
    req(gam_data())
    DT::datatable(gam_data())
  })
  
  output$csv_name <- renderText({
    paste("Loaded file:", gam_data_name())
  })
  
  #### Set y ----
  output$checkbox_y <- renderUI({
    req(gam_data())
    
    tryCatch({
      cat("renderUI / checkbox_y... \n")
      
      cols <- names(gam_data())[sapply(gam_data(), function(x) is.numeric(x) || is.integer(x))]
      cols <- cols[str_detect(cols, paste0(c("MDA8", 
                                             "o3", "O3",
                                             "ozone", "Ozone", 
                                             "pm", "PM"), collapse = "|"))]
      cols <- cols[!str_detect(cols, paste0(c("AQS_", "ASOS_", 
                                              "_id", "_ID",
                                              "_crit", "_Crit"), collapse = "|"))]
      
      radioButtons(
        inputId = "checkGroup_y",
        label = "Response (y):",
        choices = stats::setNames(cols, cols),
        selected = NULL
      )
      
    }, error = function(e) {
      cat("Error - renderUI / checkbox_y: ", e$message, "\n")
      NULL
    })
  })
  
  #### Set x single ----
  output$checkbox_x <- renderUI({
    req(gam_data())
    
    tryCatch({
      cat("renderUI / checkbox_x... \n")
      
      cols <- names(gam_data())
      cols <- cols[!str_detect(cols, paste0(c("date", "state", "site", "code", 
                                              "AQS_", "ASOS_", "_id", "cbsa"), collapse = "|"))]
      checkboxGroupInput(inputId = "checkGroup_x", 
                         label = "Predictors (x):", 
                         choices = stats::setNames(cols, cols),
                         selected = NULL)
      
    }, error = function(e) {
      cat("Error - renderUI / checkbox_x: ", e$message, "\n")
      NULL
    })
  })
  
  #### Category ----
  output$gamCategory <- renderUI({
    req(gam_data())
    
    tryCatch({
      cat("renderUI / gamCategory... \n")
      
      cols <- names(gam_data())
      cols <- cols[!str_detect(cols, paste0(c("date", "state", "site", "code", 
                                              "AQS_", "ASOS_", "_id", "cbsa"), collapse = "|"))]
      
      fluidRow(
        column(6,
               selectInput(inputId = "gamCategory_column",
                           label = "Which categories would you like to include?",
                           choices = c("None", cols),
                           selected = "None")
        ),
        column(6,
               selectInput(inputId = "gamCategory_category",
                           label = "Which values would you like to run the GAM?",
                           choices = "None",
                           selected = "None")
        )
      )
    }, error = function(e) {
      cat("Error - renderUI / gamCategory: ", e$message, "\n")
      NULL
    })
  })
  
  observe({
    req(gam_data())
    req(input$gamCategory_column)
    
    tryCatch({
      cat("renderUI / gamCategory... \n")
      
      if(input$gamCategory_column != "None") {
        cols_unique <- gam_data()[[input$gamCategory_column]] %>% unique()
        updateSelectInput(session, 
                          "gamCategory_category",
                          choices = c(cols_unique),
                          selected = NULL)
      } else {
        updateSelectInput(session, 
                          "gamCategory_category",
                          choices = "None",
                          selected = "None")
      }
    }, error = function(e) {
      cat("Error - renderUI / gamCategory: ", e$message, "\n")
      NULL
    })
  })
  
  #### Single parameter ----
  output$set_single <- renderUI({
    req(input$checkGroup_x)
    
    tryCatch({
      cat("renderUI / set_single... \n")
      
      lapply(seq_along(input$checkGroup_x), function(i) {
        
        x <- input$checkGroup_x[i]
        id_class <- paste0("class_", x)
        id_func <- paste0("func_", x)
        id_basis <- paste0("basis_", x)
        id_k <- paste0("k_", x)
        
        fluidRow(
          column(1, 
                 strong(br(), x)),
          column(2, 
                 selectInput(inputId = id_class,
                             label = if(i == 1) "Class" else "",
                             choices = c("numeric", "categorical"),
                             selected = "numeric")),
          column(2, 
                 selectInput(inputId = id_func,
                             label = if(i == 1) "f(x)" else "",
                             choices = c("s", "none"),
                             selected = "s")),
          column(2, 
                 selectInput(inputId = id_basis,
                             label = if(i == 1) "Basis" else "",
                             choices = c("cr", "cs", "cc", "tp", "bs", "ps", "ad"),
                             selected = "cr")),
          column(2, 
                 selectInput(inputId = id_k,
                             label = if(i == 1) "k" else "",
                             choices = c(-1, seq(1, 50)),
                             selected = -1)),
          column(3,
                 br(),
                 uiOutput(paste0("fx_", x)))
        )
      })
      
    }, error = function(e) {
      cat("Error - renderUI / set_single: ", e$message, "\n")
      NULL
    })
  })
  
  observeEvent(input$checkGroup_x, {
    lapply(seq_along(input$checkGroup_x), function(i) {
      x <- input$checkGroup_x[i]
      id_class <- paste0("class_", x)
      id_func <- paste0("func_", x)
      id_basis <- paste0("basis_", x)
      id_k <- paste0("k_", x)
      
      observeEvent(input[[id_class]], {
        if(input[[id_class]] == "categorical") {
          shinyjs::disable(id_func)
          shinyjs::disable(id_basis)
          shinyjs::disable(id_k)
        } else {
          shinyjs::enable(id_func)
          shinyjs::enable(id_basis)
          shinyjs::enable(id_k)
        }
      }, ignoreInit = T)
      
      observeEvent(input[[id_func]], {
        if(input[[id_func]] == "none") {
          shinyjs::disable(id_basis)
          shinyjs::disable(id_k)
        } else {
          shinyjs::enable(id_basis)
          shinyjs::enable(id_k)
        }
      }, ignoreInit = T)
    })
  }, ignoreInit = T)
  
  
  #### Set x interaction ----
  x_int_inputs <- reactiveValues(inputs = list())
  
  # Function to update x_int_inputs with current input values
  update_x_int_inputs <- function() {
    n <- length(x_int_inputs$inputs)
    for(i in seq_len(n)) {
      x1 <- input[[paste0("checkGroup_x_int1_", i)]]
      x2 <- input[[paste0("checkGroup_x_int2_", i)]]
      if(!is.null(x1) && !is.null(x2)) {
        x_int_inputs$inputs[[i]]$input1 <- x1
        x_int_inputs$inputs[[i]]$input2 <- x2
        
        x_int_bot <- paste0(i, "_and_", i)
        id_func <- paste0("int_func_", x_int_bot)
        id_class_1 <- paste0("int_class_1_", x_int_bot)
        id_class_2 <- paste0("int_class_2_", x_int_bot)
        id_basis_1 <- paste0("int_basis_1_", x_int_bot)
        id_basis_2 <- paste0("int_basis_2_", x_int_bot)
        id_k_1 <- paste0("int_k_1_", x_int_bot)
        id_k_2 <- paste0("int_k_2_", x_int_bot)
        
        x_int_inputs$inputs[[i]]$class1 <- input[[id_class_1]]
        x_int_inputs$inputs[[i]]$class2 <- input[[id_class_2]]
        x_int_inputs$inputs[[i]]$func <- input[[id_func]]
        x_int_inputs$inputs[[i]]$basis1 <- input[[id_basis_1]]
        x_int_inputs$inputs[[i]]$basis2 <- input[[id_basis_2]]
        x_int_inputs$inputs[[i]]$k1 <- input[[id_k_1]]
        x_int_inputs$inputs[[i]]$k2 <- input[[id_k_2]]
      }
    }
  }
  
  # Helper function to handle NULL values
  `%||%` <- function(a, b) if(!is.null(a)) a else b
  
  
  # Update addInt and rmvInt to store current inputs before modifying
  observeEvent(input$addInt, {
    tryCatch({
      cat("observeEvent / addInt... \n")
      cols <- names(gam_data())
      cols <- cols[!str_detect(cols, paste0(c("date", "state", "site", "code", 
                                              "AQS_", "ASOS_", "_id", "cbsa"), collapse = "|"))]
      
      update_x_int_inputs()
      x_int_inputs$inputs <- c(x_int_inputs$inputs, 
                               list(list(input1 = cols[1], 
                                         input2 = cols[1])))
    }, error = function(e) {
      cat("Error - observeEvent / addInt: ", e$message, "\n")
      NULL
    })
  }, ignoreInit = T)
  
  observeEvent(input$rmvInt, {
    tryCatch({
      cat("observeEvent / rmvInt... \n")
      if(length(x_int_inputs$inputs) > 0) {
        x_int_inputs$inputs <- x_int_inputs$inputs[-length(x_int_inputs$inputs)]
      }
      update_x_int_inputs()
    }, error = function(e) {
      cat("Error - observeEvent / rmvInt: ", e$message, "\n") 
      NULL
    })
  }, ignoreInit = T)
  
  output$checkbox_x_int <- renderUI({
    req(gam_data())
    
    tryCatch({
      cat("renderUI / checkbox_x_int... \n")
      
      cols <- names(gam_data())
      cols <- cols[!str_detect(cols, paste0(c("date", "state", "site", "code", 
                                              "AQS_", "ASOS_", "_id", "cbsa"), collapse = "|"))]
      
      inputList <- x_int_inputs$inputs
      lapply(seq_along(inputList), function(i) {
        fluidRow(
          column(6, 
                 selectInput(inputId = paste0("checkGroup_x_int1_", i),
                             label = if(i == 1) "Interaction (x1)" else "",
                             choices = c(cols),
                             selected = inputList[[i]]$input1)),
          column(6, 
                 selectInput(inputId = paste0("checkGroup_x_int2_", i),
                             label = if(i == 1) "Interaction (x2)" else "",
                             choices = c(cols),
                             selected = inputList[[i]]$input2)) 
        )
      })
    }, error = function(e) {
      cat("Error - renderUI / checkbox_x_int: ", e$message, "\n")
      NULL
    })
  })
  
  #### Interaction Parameter ----
  output$set_interaction <- renderUI({
    tryCatch({
      cat("renderUI / set_interaction... \n")
      if(length(x_int_inputs$inputs) != 0) {
        lapply(seq_along(x_int_inputs$inputs), function(i) {
          x1 <- input[[paste0("checkGroup_x_int1_", i)]]
          x2 <- input[[paste0("checkGroup_x_int2_", i)]]
          req(x1, x2)
          
          x_int <- paste0(x1, ", ", x2)
          x_int_bot <- paste0(i, "_and_", i)
          id_func <- paste0("int_func_", x_int_bot)
          id_class_1 <- paste0("int_class_1_", x_int_bot)
          id_class_2 <- paste0("int_class_2_", x_int_bot)
          id_basis_1 <- paste0("int_basis_1_", x_int_bot)
          id_basis_2 <- paste0("int_basis_2_", x_int_bot)
          id_k_1 <- paste0("int_k_1_", x_int_bot)
          id_k_2 <- paste0("int_k_2_", x_int_bot)
          
          class1_selected <- x_int_inputs$inputs[[i]]$class1 %||% "numeric"
          class2_selected <- x_int_inputs$inputs[[i]]$class2 %||% "numeric"
          func_selected <- x_int_inputs$inputs[[i]]$func %||% "ti"
          basis1_selected <- x_int_inputs$inputs[[i]]$basis1 %||% "cr"
          basis2_selected <- x_int_inputs$inputs[[i]]$basis2 %||% "cr"
          k1_selected <- x_int_inputs$inputs[[i]]$k1 %||% -1
          k2_selected <- x_int_inputs$inputs[[i]]$k2 %||% -1
          
          fluidRow(
            column(1,
                   strong(br(), x_int)),
            column(2,
                   selectInput(inputId = id_class_1,
                               label = if(i == 1) "Class" else "",
                               choices = c("numeric", "categorical"),
                               selected = class1_selected),
                   selectInput(inputId = id_class_2,
                               label = NULL,
                               choices = c("numeric", "categorical"),
                               selected = class2_selected)),
            column(2,
                   selectInput(inputId = id_func,
                               label = if(i == 1) "f(x)" else "",
                               choices = c("s", "ti", "te"),
                               selected = func_selected)), 
            column(2,
                   selectInput(inputId = id_basis_1,
                               label = if(i == 1) "Basis" else "",
                               choices = c("tp", "cr", "cs", "cc", "bs", "ps"),
                               selected = basis1_selected),
                   if(func_selected == "s") {
                     tagList(
                       selectInput(inputId = id_basis_2,
                                   label = NULL,
                                   choices = "",
                                   selected = "")
                     )
                   } else {
                     tagList(
                       selectInput(inputId = id_basis_2,
                                   label = NULL,
                                   choices = c("cr", "cs", "cc", "bs", "ps"),
                                   selected = basis2_selected)
                     )
                   }
            ),
            column(2,
                   selectInput(inputId = id_k_1,
                               label = if(i == 1) "k" else "",
                               choices = c(-1, seq(1, 50)),
                               selected = k1_selected),   
                   if(basis1_selected == "tp") {
                     tagList(
                       selectInput(inputId = id_k_2,
                                   label = NULL,
                                   choices = "",
                                   selected = "")
                     )
                   } else {
                     tagList(
                       selectInput(inputId = id_k_2,
                                   label = NULL,
                                   choices = c(-1, seq(1, 50)),
                                   selected = k2_selected)
                     )
                   }
            ),   
            column(3,
                   br(),
                   uiOutput(paste0("int_fx_", x_int_bot)))
          )
        })
      }
    }, error = function(e) {
      cat("Error - renderUI / set_interaction: ", e$message, "\n")
      NULL
    })
  })
  
  observe({
    lapply(seq_along(x_int_inputs$inputs), function(i) {
      x1 <- input[[paste0("checkGroup_x_int1_", i)]]
      x2 <- input[[paste0("checkGroup_x_int2_", i)]]
      req(x1, x2)
      
      x_int_bot <- paste0(i, "_and_", i)
      id_func <- paste0("int_func_", x_int_bot)
      id_basis_1 <- paste0("int_basis_1_", x_int_bot)
      id_basis_2 <- paste0("int_basis_2_", x_int_bot)
      id_k_1 <- paste0("int_k_1_", x_int_bot)
      id_k_2 <- paste0("int_k_2_", x_int_bot)
      
      req(input[[id_func]])
      
      print(input[[id_func]])
      print(input[[id_basis_1]])
      print(input[[id_basis_2]])
      print(id_func)
      print(id_basis_1)
      print(id_basis_2)
      
      if(input[[id_func]] == "s") {
        updateSelectInput(session, 
                          id_basis_1,
                          choices = "tp", 
                          selected = "tp")
        updateSelectInput(session, 
                          id_basis_2,
                          choices = "", 
                          selected = "")
        updateSelectInput(session, 
                          id_k_2,
                          choices = "", 
                          selected = "")
      } else {
        updateSelectInput(session, 
                          id_basis_1,
                          choices = c("tp", "cr", "cs", "cc", "bs", "ps"),
                          selected = input[[id_basis_1]])
      }
      
      if(input[[id_basis_1]] == "tp") {
        updateSelectInput(session, 
                          id_basis_2,
                          choices = "", 
                          selected = "")
        updateSelectInput(session, 
                          id_k_2,
                          choices = "", 
                          selected = "")
      } else {
        if(is.null(x_int_inputs$inputs[[i]]$k2)){
          x_int_inputs$inputs[[i]]$k2 <- -1
        }
        updateSelectInput(session, 
                          id_basis_2,
                          choices = c("cr", "cs", "cc", "bs", "ps"), 
                          selected = input[[id_basis_2]])
        updateSelectInput(session, 
                          id_k_2,
                          choices = c(-1, seq(1, 50)), 
                          selected = input[[id_k_2]])
      }
    })
  })
  
  #### Formula display ----
  gam_form <- reactiveValues(formula_x = list())
  
  x_seq_reactive <- reactive({
    tryCatch({
      cat("reactive / GAM manual / x_seq_reactive... \n")
      x_seq <- c(input$checkGroup_x)
      
      if(length(x_int_inputs$inputs) != 0) {
        x_int_bot <- sapply(seq_along(x_int_inputs$inputs), function(i) {
          paste0(i, "_and_", i)
        })
        x_seq <- c(x_seq, x_int_bot)
      }
      x_seq
    }, error = function(e) {
      cat("Error - reactive / GAM manual / x_seq_reactive: ", e$message, "\n")
      NULL
    })
  })
  
  observe({
    req(input$checkGroup_y)
    
    tryCatch({
      cat("observe / GAM manual / formula display...\n")
      gam_form$formula_x <- list()
      gam_form_inside <- reactiveValues(formula_x = list())
      
      if(length(input$checkGroup_x) == 0 && length(x_int_inputs$inputs) == 0) {
        output$gamFormula <- renderText({ "" })
      }
      
      for(x in x_seq_reactive()) {
        local({
          
          local_x <- x
          
          if(str_detect(local_x, "_and_")) {
            output[[paste0("int_fx_", local_x)]] <- renderText({
              i_n <- unlist(regmatches(local_x, gregexpr("\\d+", local_x)))
              i_n <- unique(i_n)
              x1 <- input[[paste0("checkGroup_x_int1_", i_n)]]
              x2 <- input[[paste0("checkGroup_x_int2_", i_n)]]
              local_x_in <- paste0(x1, "_and_", x2)
              local_x_in <- gsub("_and_", ", ", local_x_in)
              
              func <- input[[paste0("int_func_", local_x)]]
              basis_1 <- input[[paste0("int_basis_1_", local_x)]]
              basis_2 <- input[[paste0("int_basis_2_", local_x)]]
              k_1 <- input[[paste0("int_k_1_", local_x)]]
              k_2 <- input[[paste0("int_k_2_", local_x)]]
              
              req(func, basis_1, k_1)
              
              x_form <- if(func == "s") {
                if(k_1 == -1) {         
                  paste0(func, "(", local_x_in, ", bs=\"", basis_1, "\")")
                } else {
                  paste0(func, "(", local_x_in, ", bs=\"", basis_1, "\", k=", k_1, ")")
                }
              } else {
                if(basis_1 == "tp") {
                  if(k_1 == -1) {         
                    paste0(func, "(", local_x_in, ", bs=\"", basis_1, "\")")
                  } else {
                    paste0(func, "(", local_x_in, ", bs=\"", basis_1, "\", k=", k_1, ")")
                  }
                } else {
                  if(k_1 == -1 & k_2 == -1) {   
                    paste0(func, "(", local_x_in, ", bs=c(\"", basis_1, "\",\"", basis_2, "\"))")
                  } else {
                    paste0(func, "(", local_x_in, ", bs=c(\"", basis_1, "\",\"", basis_2, "\"), k=c(", k_1, ",", k_2, "))")
                  }
                }
              }
              gam_form$formula_x[[local_x]] <- x_form
              gam_form_inside$formula_x[[local_x]] <- x_form
            })
          } else {
            output[[paste0("fx_", local_x)]] <- renderText({
              class <- input[[paste0("class_", local_x)]]
              func <- input[[paste0("func_", local_x)]]
              basis <- input[[paste0("basis_", local_x)]]
              k <- input[[paste0("k_", local_x)]]
              req(class, func, basis, k)
              
              x_form <- if(class == "categorical") {
                local_x
              } else {
                if(func == "none") {
                  local_x
                } else {
                  if(k == -1) {
                    paste0(func, "(", local_x, ", bs=\"", basis, "\")")
                  } else {
                    paste0(func, "(", local_x, ", bs=\"", basis, "\", k=", k, ")")
                  }
                }
              }
              gam_form$formula_x[[local_x]] <- x_form
              gam_form_inside$formula_x[[local_x]] <- x_form
            })
          }
          output$gamFormula <- renderText({
            paste(input$checkGroup_y, 
                  "~", 
                  paste(unlist(gam_form_inside$formula_x), collapse = " + "))
          })
        })
      }
    }, error = function(e) {
      cat("Error - observe / GAM manual / formula display: ", e$message, "\n")
      NULL
    })
  })
  
  ### GAM Formula ----
  gam_formula <- reactive({
    tryCatch({
      cat("reactive / GAM manual / gam_formula... \n")
      req(input$checkGroup_y,
          unlist(gam_form$formula_x[x_seq_reactive()]))
      paste(input$checkGroup_y, 
            "~", 
            paste(unlist(gam_form$formula_x[x_seq_reactive()]), 
                  collapse = " + "))
    }, error = function(e) {
      cat("Error - reactive / GAM manual / gam_formula: ", e$message, "\n")
      NULL
    })
  })
  
  observeEvent(input$run_manual_gam, {
    showPageSpinner()
    removeTab(inputId = "gam_manual_tabs",
              target = "gam_results",
              session = session)
    
    tryCatch({
      cat("observeEvent / run_manual_gam... \n")
      
      req(gam_data(), 
          gam_formula())
      
      mgcvfunc <- input$gamFunc
      mgcvmet <- input$gamMethod
      mgcvgamma <- as.numeric(input$gamGamma)
      mgcvselect <- input$gamSelect
      
      if(mgcvfunc == "bam") {
        if(mgcvmet == "REML") {
          mgcvmet <- "fREML"
        }
      }
      
      if(input$gamFamily == "gaussian") {mgcvfamily = gaussian(link = input$gamLink)
      } else if(input$gamFamily == "poisson") {mgcvfamily = poisson(link = input$gamLink)
      } else if(input$gamFamily == "Gamma") {mgcvfamily = Gamma(link = input$gamLink)
      } else if(input$gamFamily == "binomial") {mgcvfamily = binomial()}
      
      
      if(input$gamCategory_column != "None" & input$gamCategory_category != "None") {
        gam_pred  <- gam_data() %>%
          mutate(remark = if_else(.data[[input$gamCategory_column]] == input$gamCategory_category, 
                                  "trained", "test"))
        gam_input <- gam_pred %>% filter(remark == "trained")
      } else {
        gam_input <- gam_pred <- gam_data()
      }
      
      
      if(mgcvfunc == "bam") {
        gam_model <- mgcv::bam(as.formula(gam_formula()), 
                               data = gam_input,
                               family = mgcvfamily,
                               gamma = mgcvgamma,
                               select = mgcvselect,
                               method = mgcvmet)
      } else {
        gam_model <- mgcv::gam(as.formula(gam_formula()), 
                               data = gam_input,
                               family = mgcvfamily,
                               gamma = mgcvgamma,
                               select = mgcvselect,
                               method = mgcvmet)
      }
      
      output$gamModelText <- renderText({
        paste0("GAM parameter setup summary:\n",
               "Function: ", mgcvfunc, "\n",
               "Family: ", input$gamFamily, "\n",
               "Link: ", input$gamLink, "\n",
               "Method: ", mgcvmet, "\n",
               "Gamma: ", mgcvgamma, "\n",
               "Select: ", mgcvselect, "\n")
      })
      
      gam_output <- func_gam_prediction(model = gam_model,
                                        new_data = gam_pred)
      gam_output <- gam_output$df_output
      
      src <- "gam_output"
      output_id <- paste0("data_ui_", src)
      data_id <- paste0("data_load_", src)
      download_ui_id <- paste0("download_ui_", src)
      download_button_id <- paste0("download_", src)
      
      output[[output_id]] <- renderUI({
        
        data <- gam_output
        
        if(is.null(data) || nrow(data) == 0) {
          h4("No data available in table",
             style = "text-align: center; color: black;")
        } else {
          div(
            style = "overflow-y: auto; overflow-x: auto;",
            DT::dataTableOutput(data_id)
          )
        }
      })
      
      output[[download_ui_id]] <- renderUI({
        func_render_download_ui( 
          data = gam_output,
          button_id = download_button_id,
          label = paste("Save data")
        )
      })
      
      output[[download_button_id]] <- func_create_download_handler(
        data = gam_output,
        date_range = NULL,
        prefix = data_id
      )
      
      func_render_tbl(output = output,
                      output_id = "data_load_gam_output", 
                      data = gam_output, 
                      pageLength = 20)
      
      insertTab(
        inputId = "gam_manual_tabs",
        tabPanel(
          title = "GAM results",
          value = "gam_results",
          
          fluidRow(
            box(
              title = "GAM summary",
              status = "primary",
              solidHeader = T,
              width = 5,
              verbatimTextOutput("gamModelText"),
              verbatimTextOutput("gamSum")
            ),
            box(
              title = "GAM check",
              status = "primary",
              solidHeader = T,
              width = 7,
              plotOutput("gamChk")
            )
          ),
          fluidRow(
            box(
              title = "GAM smooth plots",
              status = "primary",
              solidHeader = T,
              width = 12,
              plotOutput("gamPlot")
            )
          ),
          fluidRow(
            box(
              status = "primary",
              solidHeader = T,
              width = 12,
              style = "overflow-x: auto;",
              
              uiOutput(outputId = "download_ui_gam_output"),
              uiOutput(outputId = "data_ui_gam_output")
            )
          )
        ),
        target = "gam_setup",
        position = "after"
      )
      
      output$gamSum <- renderPrint({
        print(summary(gam_model))
      })
      
      output$gamChk <- renderPlot({
        par(mfrow = c(2, 2),
            mar = c(4.5, 4.5, 1, 1))
        mgcv::gam.check(gam_model)
      })
      
      output$gamPlot <- renderPlot({
        par(mar = c(4.5, 4.5, 1, 1))
        mgcv::plot.gam(gam_model, 
                       pages = 1,
                       residuals = T,
                       # envir = environment(),
                       # all.terms = T,
                       scale = 0)
      })
      
      rm(gam_pred,
         gam_input)
      gc()
      
    }, error = function(e) {
      cat("Error - observeEvent / run_manual_gam: ", e$message, "\n")
      showNotification(e$message, type = "error", duration = 3)
    })
    
    hidePageSpinner()
  }, ignoreInit = T)
  
  
  
  ## Sidebar 4: GAM previous ----
  ### Sub-tab: Overview ----
  #### Site information ----
  observe({
    tryCatch({
      cat("observe / GAM previous / Site information / Part 1...\n")
      
      func_toggle_sites(map_id = "map_site_gam", 
                        reactive_id = func_toggle_sites_react,
                        show = input$gam_previous_show_gam_v1, 
                        data = data_stat_gam_v1[, ..column_for_site_gam], 
                        radius = 8,
                        border_color = "black",
                        border_lwd = 0.5,
                        fill_color = adjustcolor("red", alpha.f = 0.75),
                        group = "gam_v1")
      func_toggle_sites(map_id = "map_site_gam",
                        reactive_id = func_toggle_sites_react,
                        show = input$gam_previous_show_gam_v3,
                        data = data_stat_gam_v3[, ..column_for_site_gam],
                        radius = 6,
                        border_color = "black",
                        border_lwd = 0.5,
                        fill_color = adjustcolor("green", alpha.f = 0.75),
                        group = "gam_v3")
      func_toggle_sites(map_id = "map_site_gam",
                        reactive_id = func_toggle_sites_react,
                        show = input$gam_previous_show_gam_v3_edm,
                        data = data_stat_gam_v3_edm[, ..column_for_site_gam],
                        radius = 8,
                        border_color = adjustcolor("darkgreen", alpha.f = 1),
                        border_lwd = 3.5,
                        fill_color = adjustcolor("darkgreen", alpha.f = 0),
                        group = "gam_v3_edm")  
      func_toggle_sites(map_id = "map_site_gam",
                        reactive_id = func_toggle_sites_react,
                        show = input$gam_previous_show_epa_ember,
                        data = data_stat_epa_ember[, ..column_for_site_gam],
                        radius = 6,
                        border_color = "black",
                        border_lwd = 0.5,
                        fill_color = adjustcolor("yellow", alpha.f = 0.75),
                        group = "epa_ember")
      func_toggle_sites(map_id = "map_site_gam",
                        reactive_id = func_toggle_sites_react,
                        show = input$gam_previous_show_pm_cbsa,
                        data = data_stat_pm_cbsa[, ..column_for_site_gam],
                        radius = 6,
                        border_color = "black",
                        border_lwd = 0.5,
                        fill_color = adjustcolor("magenta", alpha.f = 0.75),
                        group = "pm_cbsa")
      func_toggle_sites(map_id = "map_site_gam", 
                        reactive_id = func_toggle_sites_react,
                        show = input$gam_previous_show_gam_pm,
                        data = data_epa_pm,
                        radius = 4,
                        border_color = adjustcolor(data_epa_pm$color, alpha.f = 0),
                        border_lwd = 0.5,
                        fill_color = adjustcolor(data_epa_pm$color, alpha.f = 0.5),
                        group = "GAM_just_pm")
      
      ## Hide/Show dataset (2025-02-27)
      # if(user_status$show_only_for_admin == T) {
      #   func_toggle_sites(map_id = "map_site_gam",
      #                     reactive_id = func_toggle_sites_react,
      #                     show = input$gam_previous_show_gam_v2,
      #                     data = data_stat_gam_v2[, ..column_for_site_gam],
      #                     radius = 6,
      #                     border_color = "black",
      #                     border_lwd = 0.5,
      #                     fill_color = adjustcolor("orange", alpha.f = 0.75),
      #                     group = "gam_v2")
      #   func_toggle_sites(map_id = "map_site_gam",
      #                     reactive_id = func_toggle_sites_react,
      #                     show = input$gam_previous_show_gam_v2_edm,
      #                     data = data_stat_gam_v2_edm[, ..column_for_site_gam],
      #                     radius = 8,
      #                     border_color = adjustcolor("darkorange", alpha.f = 1),
      #                     border_lwd = 3.5,
      #                     fill_color = adjustcolor("darkorange", alpha.f = 0),
      #                     group = "gam_v2_edm") 
      # } else {
      #   leaflet::leafletProxy("map_site_gam") %>%
      #     leaflet::hideGroup(group = c("gam_v2", "gam_v2_edm"))
      # }
      
    }, error = function(e) {
      cat("Error - observe / GAM previous / Site information / Part 1: ", e$message, "\n")
      NULL
    })
  })
  
  
  #### Selected sites by click ----
  selected_sites_gam <- reactiveVal(data.table::data.table(source = character(),
                                                           state = character(),
                                                           AQS = character(), 
                                                           lon = numeric(),
                                                           lat = numeric(),
                                                           site_name = character()))
  
  #### For advanced searching ----
  selected_sites_gam_adv <- reactive({
    search_input <- input$gam_previous_search_input
    data_gam_stat()[
      stri_detect_regex(state, search_input, case_insensitive = T) |
        stri_detect_regex(AQS, search_input, case_insensitive = T) |
        stri_detect_regex(site_name, search_input, case_insensitive = T)
    ]
  })
  
  observe({
    updateSelectizeInput(session,
                         inputId = "gam_previous_search_input",
                         choices = site_search_term_previous(),
                         selected = NULL,
                         server = F)
  })
  
  observe({
    func_site_select_tbl(output = output, 
                         output_id = "gam_previous_search_results", 
                         selected_sites_func = selected_sites_gam_adv(), 
                         delete_input_prefix = NULL, 
                         locate_input_prefix = "locate_row_gam_adv", 
                         adlist_input_prefix = "adlist_row_gam_adv", 
                         pageLength = 3, 
                         lengthChange = F)
  })
  
  observeEvent(input$adlist_row_gam_adv, {
    tryCatch({
      row_to_add <- input$adlist_row_gam_adv
      
      if(!is.null(row_to_add)) {
        cat("Adding row to selected_sites_gam...\n")
        
        selected_site <- selected_sites_gam_adv()[row_to_add, ]
        existing_sites <- selected_sites_gam()
        updated_sites <- distinct(rbind(existing_sites, selected_site))
        rownames(updated_sites) <- NULL
        selected_sites_gam(updated_sites)
      }
    }, error = function(e) {
      cat("Error - observeEvent / adlist_row_gam_adv: ", e$message, "\n")
      NULL
    })
  }, ignoreNULL = T, ignoreInit = T)
  
  observeEvent(input$gam_previous_button_add_to_list, {
    if(nrow(selected_sites_gam_adv()) > 0) {
      updated_sites <- distinct(rbind(selected_sites_gam(), 
                                      selected_sites_gam_adv()))
      rownames(updated_sites) <- NULL
      selected_sites_gam(updated_sites)
    }
  })
  
  func_site_location_status_gam_adv <- reactiveValues(is_highlighted = F, 
                                                      current_row = NULL)
  observeEvent(input$locate_row_gam_adv, {
    func_site_location(input = input, 
                       input_id = "locate_row_gam_adv", 
                       reactive_id = func_site_location_status_gam_adv,
                       selected_sites_func = selected_sites_gam_adv(), 
                       map_id = "map_site_gam")
  }, ignoreNULL = T, ignoreInit = T)
  
  
  #### Click event: Selected site ----
  observeEvent(input$map_site_gam_click, {
    showPageSpinner()   
    
    tryCatch({
      cat("observeEvent / map_site_gam_click... \n")
      
      click_coords <- c(input$map_site_gam_click$lng, 
                        input$map_site_gam_click$lat)
      
      if(user_status$show_only_for_admin == T) {
        site_info_list <- list(
          
          if(input$gam_previous_show_gam_v1) {
            func_find_nearest_site(click_coords, 
                                   data_stat_gam_v1[, ..column_for_site_gam],
                                   click_radius)
          },
          
          if(input$gam_previous_show_gam_v3) {
            func_find_nearest_site(click_coords, 
                                   data_stat_gam_v3[, ..column_for_site_gam],
                                   click_radius)
          },
          
          if(input$gam_previous_show_gam_v3_edm) {
            func_find_nearest_site(click_coords, 
                                   data_stat_gam_v3_edm[, ..column_for_site_gam],
                                   click_radius)
          },
          
          if(input$gam_previous_show_epa_ember) {
            func_find_nearest_site(click_coords, 
                                   data_stat_epa_ember[, ..column_for_site_gam],
                                   click_radius)
          },
          
          if(input$gam_previous_show_pm_cbsa) {
            func_find_nearest_site(click_coords, 
                                   data_stat_pm_cbsa[, ..column_for_site_gam],
                                   click_radius)
          }
          
          
          ## Hide/Show dataset (2025-02-27)
          # if(input$gam_previous_show_gam_v2) {
          #   func_find_nearest_site(click_coords, 
          #                          data_stat_gam_v2[, ..column_for_site_gam],
          #                          click_radius)
          # },
          # 
          # if(input$gam_previous_show_gam_v2_edm) {
          #   func_find_nearest_site(click_coords, 
          #                          data_stat_gam_v2_edm[, ..column_for_site_gam],
          #                          click_radius)
          # }
        )
      } else{
        site_info_list <- list(
          
          if(input$gam_previous_show_gam_v1) {
            func_find_nearest_site(click_coords, 
                                   data_stat_gam_v1[, ..column_for_site_gam],
                                   click_radius)
          },
          
          if(input$gam_previous_show_gam_v3) {
            func_find_nearest_site(click_coords, 
                                   data_stat_gam_v3[, ..column_for_site_gam],
                                   click_radius)
          },
          
          if(input$gam_previous_show_gam_v3_edm) {
            func_find_nearest_site(click_coords, 
                                   data_stat_gam_v3_edm[, ..column_for_site_gam],
                                   click_radius)
          },
          
          if(input$gam_previous_show_epa_ember) {
            func_find_nearest_site(click_coords, 
                                   data_stat_epa_ember[, ..column_for_site_gam],
                                   click_radius)
          },
          
          if(input$gam_previous_show_pm_cbsa) {
            func_find_nearest_site(click_coords, 
                                   data_stat_pm_cbsa[, ..column_for_site_gam],
                                   click_radius)
          }
        )
      }
      
      site_info <- dplyr::bind_rows(site_info_list)
      site_info <- site_info[, ..column_for_site_gam]
      
      if(nrow(site_info) > 0) {
        updated_sites <- distinct(rbind(selected_sites_gam(), 
                                        site_info))
        rownames(updated_sites) <- NULL
        selected_sites_gam(updated_sites)
      }
      
      #### GAM metadata & plotting ----
      if(nrow(site_info) > 0) {
        
        data_source <- site_info$source[1]
        selected_aqs <- site_info$AQS[1]

        folder_name <- "data_by_aqs"
        file_name <- paste0("data_by_aqs_", selected_aqs, ".rds")
        data_loaded <- readRDS(file.path("src/data", folder_name, file_name))
        data_loaded <- data_loaded[[data_source]]
        
        if(data_source == "gam_v1") {
          gam_meta_df <- data_stat_gam_v1[AQS_O3 == selected_aqs]
          gam_term_df <- data_term_gam_v1[AQS_O3 == selected_aqs]
          gam_pred_df <- data_loaded[AQS_O3 == selected_aqs]
          gam_term_df <- gam_term_df[, ..column_for_gam_term]
          gam_meta_df$"Data source" <- "GAM v1"
          gam_meta_df$Period <- paste0(range(gam_pred_df$date), collapse = " ~ ")
          gam_meta_df$"Model used" <- "GAM"
          gam_meta_df$"Training data" <- "Non-smoke days (NSD)"
        }
        
        if(data_source == "gam_v3") {
          gam_meta_df <- data_stat_gam_v3[AQS_O3 == selected_aqs]
          gam_term_df <- data_term_gam_v3[AQS_O3 == selected_aqs]
          gam_pred_df <- data_loaded[AQS_O3 == selected_aqs]
          gam_term_df <- gam_term_df[, ..column_for_gam_term]
          gam_meta_df$"Data source" <- "GAM v3"
          gam_meta_df$Period <- paste0(range(gam_pred_df$date), collapse = " ~ ")
          gam_meta_df$"Model used" <- "GAM"
          gam_meta_df$"Training data" <- "Non-smoke days (NSD)"
        }
        
        if(data_source == "gam_v3_edm") {
          gam_meta_df <- data_stat_gam_v3_edm[AQS_O3 == selected_aqs]
          gam_term_df <- data_term_gam_v3_edm[AQS_O3 == selected_aqs]
          gam_pred_df <- data_loaded[AQS_O3 == selected_aqs]
          gam_term_df <- gam_term_df[, ..column_for_gam_term]
          gam_meta_df$"Data source" <- "GAM v3 EDM"
          gam_meta_df$Period <- paste0(range(gam_pred_df$date), collapse = " ~ ")
          gam_meta_df$"Model used" <- "GAM"
          gam_meta_df$"Training data" <- "Non-smoke days (NSD)"
        }
        
        ## Hide/Show dataset (2025-02-27)
        # if(data_source == "gam_v2") {
        #   gam_meta_df <- data_stat_gam_v2[AQS_O3 == selected_aqs]
        #   gam_term_df <- data_term_gam_v2[AQS_O3 == selected_aqs]
        #   gam_pred_df <- data_loaded[AQS_O3 == selected_aqs]
        #   gam_term_df <- gam_term_df[, ..column_for_gam_term]
        #   gam_meta_df$"Data source" <- "GAM v2"
        #   gam_meta_df$Period <- paste0(range(gam_pred_df$date), collapse = " ~ ")
        #   gam_meta_df$"Model used" <- "GAM"
        #   gam_meta_df$"Training data" <- "Non-smoke days (NSD)"
        # }
        # 
        # if(data_source == "gam_v2_edm") {
        #   gam_meta_df <- data_stat_gam_v2_edm[AQS_O3 == selected_aqs]
        #   gam_term_df <- data_term_gam_v2_edm[AQS_O3 == selected_aqs]
        #   gam_pred_df <- data_loaded[AQS_O3 == selected_aqs]
        #   gam_term_df <- gam_term_df[, ..column_for_gam_term]
        #   gam_meta_df$"Data source" <- "GAM v2 EDM"
        #   gam_meta_df$Period <- paste0(range(gam_pred_df$date), collapse = " ~ ")
        #   gam_meta_df$"Model used" <- "GAM"
        #   gam_meta_df$"Training data" <- "Non-smoke days (NSD)"
        # }
        
        if(data_source == "epa_ember") {
          gam_meta_df <- data_stat_epa_ember[AQS_O3 == selected_aqs]
          gam_term_df <- NULL
          gam_pred_df <- data_loaded[AQS_O3 == selected_aqs]
          gam_meta_df$"Data source" <- "EPA EMBER"
          gam_meta_df$Period <- paste0(range(gam_pred_df$date), collapse = " ~ ")
          gam_meta_df$"Model used" <- "CMAQ"
          gam_meta_df$"Training data" <- "All days"
        }
        
        if(data_source == "pm_cbsa") {
          gam_meta_df <- data_stat_pm_cbsa[AQS_PM == selected_aqs]
          gam_term_df <- NULL
          gam_pred_df <- data_loaded[AQS_PM == selected_aqs]
          gam_meta_df$"Data source" <- "Smoke PM2.5"
          gam_meta_df$Period <- paste0(range(gam_pred_df$date), collapse = " ~ ")
          gam_meta_df$"Model used" <- NA
          gam_meta_df$"Training data" <- NA
          gam_pred_df$"AQS_dist_km" <- 0
        }
        
        gam_aqss_df <- gam_pred_df[, c("AQS_PM", "AQS_dist_km")] %>% distinct()
        gam_aqss_df$AQS_PM <- paste0(gam_aqss_df$AQS_PM, " (", gam_aqss_df$AQS_dist_km, " km)")
        
        if(data_source != "pm_cbsa") {
          if(!is.null(gam_term_df)) {
            gam_meta_df$AQS_PM <- paste0(unique(gam_aqss_df$AQS_PM), collapse = "<br>")
            gam_meta_df$adj_rsq <- sprintf("%.3f", gam_meta_df$adj_rsq)
            gam_meta_df$adj_rsq_cv <- sprintf("%.3f", gam_meta_df$adj_rsq_cv)
            gam_meta_df$adj_rsq_te <- sprintf("%.3f", gam_meta_df$adj_rsq_te)
            gam_meta_df$resmu_cv <- paste0(sprintf("%.3f", gam_meta_df$resmu_cv),
                                           " ± ", sprintf("%.3f", gam_meta_df$ressd_cv))
            gam_meta_df$resmu_smk <- paste0(sprintf("%.3f", gam_meta_df$resmu_te),
                                            " ± ", sprintf("%.3f", gam_meta_df$ressd_te))
          } else {
            gam_meta_df$AQS_PM <- NA
            gam_meta_df$resmu_cv <- NA
            gam_meta_df$resmu_smk <- NA
            gam_meta_df$adj_rsq_cv <- NA
            gam_meta_df$adj_rsq_te <- NA
          }
        }
        
        if(data_source == "epa_ember") {
          gam_meta_df$"No. of days with SMO=0" <- nrow(gam_pred_df[gam_pred_df$smoke == "0",]) 
          gam_meta_df$"No. of days with SMO>0" <- nrow(gam_pred_df[gam_pred_df$smoke == "1",]) 
          gam_meta_df$"Days with SMO>0 frequency (%)" <- round(gam_meta_df$"No. of days with SMO=0"/
                                                                 (gam_meta_df$"No. of days with SMO=0" + gam_meta_df$"No. of days with SMO>0")*100, 1)
        } else if(data_source == "pm_cbsa") {
          gam_meta_df$"No. of non-smoke days (NSD) m0p5m" <- nrow(gam_pred_df[gam_pred_df$smoke_m0p5m == "0",]) 
          gam_meta_df$"No. of smoke days (SD) m0p5m" <- nrow(gam_pred_df[gam_pred_df$smoke_m0p5m == "1",]) 
          gam_meta_df$"Smoke days frequency (%) m0p5m" <- round(gam_meta_df$"No. of smoke days (SD) m0p5m"/
                                                                  (gam_meta_df$"No. of non-smoke days (NSD) m0p5m" + gam_meta_df$"No. of smoke days (SD) m0p5m")*100, 1)
          
          gam_meta_df$"No. of non-smoke days (NSD) m1p0m" <- nrow(gam_pred_df[gam_pred_df$smoke_m1p0m == "0",]) 
          gam_meta_df$"No. of smoke days (SD) m1p0m" <- nrow(gam_pred_df[gam_pred_df$smoke_m1p0m == "1",]) 
          gam_meta_df$"Smoke days frequency (%) m1p0m" <- round(gam_meta_df$"No. of smoke days (SD) m1p0m"/
                                                                  (gam_meta_df$"No. of non-smoke days (NSD) m1p0m" + gam_meta_df$"No. of smoke days (SD) m1p0m")*100, 1)
        } else {
          gam_meta_df$"No. of non-smoke days (NSD)" <- nrow(gam_pred_df[gam_pred_df$smoke == "0",]) 
          gam_meta_df$"No. of smoke days (SD)" <- nrow(gam_pred_df[gam_pred_df$smoke == "1",]) 
          gam_meta_df$"Smoke days frequency (%)" <- round(gam_meta_df$"No. of smoke days (SD)"/
                                                            (gam_meta_df$"No. of non-smoke days (NSD)" + gam_meta_df$"No. of smoke days (SD)")*100, 1)
        }
        
        if(!is.null(gam_term_df)) {
          gam_meta_df$"No. of predictors" <- length(gam_term_df$term) 
        } else {
          gam_meta_df$"No. of predictors" <- NA 
        }
        
        gam_meta_df <- dplyr::mutate_all(gam_meta_df, as.character)
        gam_meta_df$resmu <- paste0(gam_meta_df$resmu, " ± ", gam_meta_df$ressd)
        
        data.table::setnames(gam_meta_df, "site_name", "Site name")
        data.table::setnames(gam_meta_df, "state", "State")
        data.table::setnames(gam_meta_df, "lon", "Longitude")
        data.table::setnames(gam_meta_df, "lat", "Latitude")
        
        if(data_source != "pm_cbsa") {
          if(data_source == "epa_ember") {
            data.table::setnames(gam_meta_df, "adj_rsq", "R2")
            data.table::setnames(gam_meta_df, "resmu", "Residual")
          } else {
            data.table::setnames(gam_meta_df, "adj_rsq", "R2-NSD")
            data.table::setnames(gam_meta_df, "adj_rsq_cv", "R2-NSD-cv")
            data.table::setnames(gam_meta_df, "adj_rsq_te", "R2-SD")
            data.table::setnames(gam_meta_df, "resmu", "Residual-NSD")
            data.table::setnames(gam_meta_df, "resmu_cv", "Residual-NSD-cv")
            data.table::setnames(gam_meta_df, "resmu_smk", "Residual-SD")
          }
        }
        
        if(data_source == "epa_ember") {
          gam_meta_df_1 <- tidyr::pivot_longer(gam_meta_df,
                                               cols = c("Data source",
                                                        "Period",
                                                        "State", 
                                                        "Site name", 
                                                        "AQS_O3", 
                                                        "Longitude",
                                                        "Latitude"),
                                               names_to = "Contents",
                                               values_to = "Descriptions") %>% as.data.table()
          gam_meta_df_2 <- tidyr::pivot_longer(gam_meta_df,
                                               cols = c("Model used",
                                                        "R2", 
                                                        "Residual", 
                                                        "No. of days with SMO>0",
                                                        "No. of days with SMO=0",
                                                        "Days with SMO>0 frequency (%)"),
                                               names_to = "Contents",
                                               values_to = "Descriptions") %>% as.data.table()
        } else if(data_source == "pm_cbsa") {
          gam_meta_df_1 <- tidyr::pivot_longer(gam_meta_df,
                                               cols = c("Data source",
                                                        "Period",
                                                        "State", 
                                                        "Site name", 
                                                        "AQS_PM", 
                                                        "Longitude",
                                                        "Latitude"),
                                               names_to = "Contents",
                                               values_to = "Descriptions") %>% as.data.table()
          gam_meta_df_2 <- tidyr::pivot_longer(gam_meta_df,
                                               cols = c("No. of non-smoke days (NSD) m0p5m",
                                                        "No. of smoke days (SD) m0p5m",
                                                        "Smoke days frequency (%) m0p5m",
                                                        "No. of non-smoke days (NSD) m1p0m",
                                                        "No. of smoke days (SD) m1p0m",
                                                        "Smoke days frequency (%) m1p0m"),
                                               names_to = "Contents",
                                               values_to = "Descriptions") %>% as.data.table()
        } else {
          gam_meta_df_1 <- tidyr::pivot_longer(gam_meta_df,
                                               cols = c("Data source",
                                                        "Period",
                                                        "State", 
                                                        "Site name", 
                                                        "AQS_O3", 
                                                        "Longitude",
                                                        "Latitude", 
                                                        "AQS_PM",
                                                        "No. of smoke days (SD)",
                                                        "No. of non-smoke days (NSD)",
                                                        "Smoke days frequency (%)"),
                                               names_to = "Contents",
                                               values_to = "Descriptions") %>% as.data.table()
          gam_meta_df_2 <- tidyr::pivot_longer(gam_meta_df,
                                               cols = c("Model used",
                                                        "Training data",
                                                        "No. of predictors",
                                                        "R2-NSD", 
                                                        "R2-NSD-cv",
                                                        "R2-SD",
                                                        "Residual-NSD",
                                                        "Residual-NSD-cv",
                                                        "Residual-SD"),
                                               names_to = "Contents",
                                               values_to = "Descriptions") %>% as.data.table()
        }
        
        gam_meta_df_1 <- gam_meta_df_1[, .(Contents, Descriptions)]
        gam_meta_df_2 <- gam_meta_df_2[, .(Contents, Descriptions)]
        
        if(nrow(gam_meta_df_1) > nrow(gam_meta_df_2)) {
          gam_meta_df_2 <- rbind(gam_meta_df_2, 
                                 data.table::data.table(Contents = rep(NA, nrow(gam_meta_df_1) - nrow(gam_meta_df_2)),
                                                        Descriptions = rep(NA, nrow(gam_meta_df_1) - nrow(gam_meta_df_2))))
        } else if(nrow(gam_meta_df_1) < nrow(gam_meta_df_2)) {
          gam_meta_df_1 <- rbind(gam_meta_df_1, 
                                 data.table::data.table(Contents = rep(NA, nrow(gam_meta_df_2) - nrow(gam_meta_df_1)),
                                                        Descriptions = rep(NA, nrow(gam_meta_df_2) - nrow(gam_meta_df_1))))
        }
        gam_meta_df <- cbind(gam_meta_df_1, gam_meta_df_2)
        
        if(!is.null(gam_term_df)) {
          gam_term_df_1 <- gam_term_df[grepl("^s\\(", term)]
          gam_term_df_2 <- gam_term_df[grepl("^(ti|te)\\(", term)]
          
          if(nrow(gam_term_df_1) > nrow(gam_term_df_2)) {
            if(nrow(gam_term_df_2) != 0) {
              gam_term_df_2 <- rbind(gam_term_df_2,
                                     data.table::data.table(term = rep(NA, nrow(gam_term_df_1) - nrow(gam_term_df_2)),
                                                            edf = rep(NA, nrow(gam_term_df_1) - nrow(gam_term_df_2)),
                                                            "F" = rep(NA, nrow(gam_term_df_1) - nrow(gam_term_df_2))))
            }
          } else if(nrow(gam_term_df_1) < nrow(gam_term_df_2)) {
            gam_term_df_1 <- rbind(gam_term_df_1,
                                   data.table::data.table(term = rep(NA, nrow(gam_term_df_2) - nrow(gam_term_df_1)),
                                                          edf = rep(NA, nrow(gam_term_df_2) - nrow(gam_term_df_1)),
                                                          "F" = rep(NA, nrow(gam_term_df_2) - nrow(gam_term_df_1))))
          }
          
          if(nrow(gam_term_df_2) != 0) {
            gam_term_df <- cbind(gam_term_df_1, gam_term_df_2)
          } else {
            gam_term_df <- gam_term_df_1
          }
        }
        
        #### GAM metadata table ----
        output$gam_previous_meta_table_1 <- DT::renderDataTable({
          DT::datatable(gam_meta_df, 
                        options = list(pageLength = 30, 
                                       dom = "t", 
                                       ordering = F,
                                       scrollX = T,
                                       columnDefs = list(
                                         list(targets = "_all", 
                                              className = "dt-left")
                                       )), 
                        escape = F,
                        rownames = F) %>%
            formatStyle(
              columns = "Contents",
              target = "cell", 
              `border-left` = "2px solid black"
            ) %>%
            formatStyle(
              columns = "Descriptions",
              target = "cell",
              `border-right` = "2px solid black"
            )
        })
        
        output$gam_previous_meta_table_2 <- DT::renderDataTable({
          if(!is.null(gam_term_df)) {
            DT::datatable(gam_term_df,
                          options = list(pageLength = 30,
                                         dom = "t",
                                         ordering = F,
                                         scrollX = T,
                                         columnDefs = list(
                                           list(targets = "_all", 
                                                className = "dt-left")
                                         )),
                          escape = F,
                          rownames = F) %>%
              formatStyle(
                columns = "term",
                target = "cell",
                `border-left` = "2px solid black"
              ) %>%
              formatStyle(
                columns = "F",
                target = "cell",
                `border-right` = "2px solid black"
              )
          } else {
            NULL
          }
        })
        
        #### Plotting ----
        tryCatch({
          cat("observeEvent / run_gam / plotting... \n")
          if(!is.null(gam_pred_df)) {
            
            if(data_source == "pm_cbsa") {
              
              ##### PM2.5 plots ----
              gam_pred_df[, PM2.5_smk_m0p5m := PM2.5]
              gam_pred_df[, PM2.5_smk_m0p5m_mark := as.character(PM2.5_smk_m0p5m)]
              gam_pred_df[smoke_m0p5m == 0, PM2.5_smk_m0p5m := NA]
              gam_pred_df[smoke_m0p5m == 0, PM2.5_smk_m0p5m_mark := "N"]
              gam_pred_df[smoke_m0p5m == 1, PM2.5_smk_m0p5m_mark := "Y"]
              
              gam_pred_df[, PM2.5_smk_m1p0m := PM2.5]
              gam_pred_df[, PM2.5_smk_m1p0m_mark := as.character(PM2.5_smk_m1p0m)]
              gam_pred_df[smoke_m1p0m == 0, PM2.5_smk_m1p0m := NA]
              gam_pred_df[smoke_m1p0m == 0, PM2.5_smk_m1p0m_mark := "N"]
              gam_pred_df[smoke_m1p0m == 1, PM2.5_smk_m1p0m_mark := "Y"]
              
              gam_pred_df[, date := as.Date(date)]
              gam_pred_df <- gam_pred_df[!is.na(smoke_m0p5m)]
              gam_pred_df <- gam_pred_df[!is.na(smoke_m1p0m)]
              
              gam_pred_df[, exceedance := ifelse(PM2.5 > 9, T, F)]
              gam_pred_df[, cause_m0p5m := NA_character_]
              gam_pred_df[, cause_m1p0m := NA_character_]
              gam_pred_df[exceedance == T & smoke_m0p5m == 0, cause_m0p5m := "not_caused_by_smoke"]
              gam_pred_df[exceedance == T & smoke_m0p5m == 1, cause_m0p5m := "due_to_smoke"]
              gam_pred_df[exceedance == T & smoke_m1p0m == 0, cause_m1p0m := "not_caused_by_smoke"]
              gam_pred_df[exceedance == T & smoke_m1p0m == 1, cause_m1p0m := "due_to_smoke"]
              
              gam_pred_df[, PM2.5 := round(PM2.5, 1)]
              gam_pred_df[, PM2.5_smk_m0p5m := round(PM2.5_smk_m0p5m, 2)]
              gam_pred_df[, PM2.5_smk_m1p0m := round(PM2.5_smk_m1p0m, 2)]
              
              ## Obs vs Pred plot
              output$gam_plot_obs_vs_pred <-
                renderEcharts4r({
                  e_charts() %>%
                    e_title("This plot is not supported with this data.", left = "center", top = "middle") %>%
                    e_text_style(color = "red", fontSize = 15)
                })
              
              ## Obs vs Pred time-series
              output$gam_plot_obs_vs_pred_time <- tryCatch({
                renderEcharts4r({
                  e <- gam_pred_df %>%
                    e_charts(date) %>%
                    e_line(serie = PM2.5, 
                           name = "Observed PM2.5", 
                           lineStyle = list(width = 1, color = "black"), 
                           symbol = "rect",
                           smooth = F) %>%
                    e_scatter(serie = PM2.5_smk_m0p5m_mark,
                              symbolSize = 13,
                              legend = F,
                              name = "Smoke day m0p5m") %>%
                    e_scatter(serie = PM2.5_smk_m0p5m,
                              symbolSize = 13,
                              legend = T,
                              name = "Smoke day m0p5m (Y/N)") %>%
                    e_scatter(serie = PM2.5_smk_m1p0m_mark,
                              symbolSize = 10,
                              legend = F,
                              name = "Smoke day m1p0m") %>%
                    e_scatter(serie = PM2.5_smk_m1p0m,
                              symbolSize = 10,
                              legend = T,
                              name = "Smoke day m1p0m (Y/N)") %>% 
                    e_mark_line(data = list(yAxis = 9), 
                                title = "9 ug m-3",
                                lineStyle = list(type = "dashed", color = "grey"),
                                symbol = c("none", "none"),
                                name = "9 ug m-3") %>%
                    e_mark_line(data = list(yAxis = 35), 
                                title = "35 ug m-3",
                                lineStyle = list(type = "dashed", color = "grey"),
                                symbol = c("none", "none"),
                                name = "35 ug m-3") %>%
                    e_tooltip(trigger = "axis",
                              formatter = htmlwidgets::JS("
                            function(params) {
                            let result = params[0].value[0] + '<br/>';
                            params.forEach(function(item) {
                            if (item.seriesName !== 'Smoke day m0p5m (Y/N)' && item.seriesName !== 'Smoke day m1p0m (Y/N)') {
                            result += 
                            item.marker +
                            ' ' + 
                            item.seriesName +
                            ': ' + 
                            item.value[1] + '<br/>';
                            }}); return result;}"),
                              axisPointer = list(
                                type = "cross"
                              )) %>% 
                    e_title(text = paste0("Daily time-series (AQS PM: ", unique(gam_pred_df$AQS_PM), ")"),
                            left = "left",
                            textStyle = list(fontSize = 14)) %>%
                    e_x_axis(type = "time", 
                             name = "Date",
                             nameLocation = "middle",
                             nameGap = 30,
                             nameTextStyle = list(fontSize = 14, color = "black"),
                             axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
                    e_y_axis(name = "PM2.5 (ug m-3)", 
                             nameLocation = "middle",
                             nameGap = 40,
                             nameTextStyle = list(fontSize = 14, color = "black"),
                             axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
                    e_legend(top = "7%") %>%
                    e_color(c("black",
                              "deepskyblue",
                              "deepskyblue",
                              "red",
                              "red")) %>% 
                    e_datazoom(type = "inside") %>% 
                    e_datazoom(type = "slider",
                               start = 0,
                               end = 100) %>%
                    e_toolbox(feature = list(saveAsImage = list(name = paste0("plot_obs_pred_time_",
                                                                              unique(gam_pred_df$source), 
                                                                              "_aqs_",
                                                                              unique(gam_pred_df$AQS_PM))), 
                                             dataZoom = list(),
                                             restore = list())) %>%
                    e_grid(bottom = "25%") %>%
                    e_animation(duration = 500)
                  e
                })
              }, error = function(e) {
                renderEcharts4r({
                  e_charts() %>%
                    e_title("This plot is not supported with this data.", left = "center", top = "middle") %>%
                    e_text_style(color = "red", fontSize = 15)
                })
              })
              
              ## Monthly smoke frequency
              gam_monthly_smoke_df_yr_mt <- gam_pred_df[, 
                                                        .(N_all = .N), 
                                                        by = .(year_month = format(date, "%Y-%m"))]
              gam_monthly_smoke_df_m0p5m <- gam_pred_df[smoke_m0p5m == 1, 
                                                        .(N = .N), 
                                                        by = .(year_month = format(date, "%Y-%m"))]
              gam_monthly_smoke_df_m1p0m <- gam_pred_df[smoke_m1p0m == 1, 
                                                        .(N = .N), 
                                                        by = .(year_month = format(date, "%Y-%m"))]
              gam_monthly_smoke_df <- merge(gam_monthly_smoke_df_yr_mt,
                                            gam_monthly_smoke_df_m0p5m,
                                            all.x = T,
                                            by = "year_month")
              gam_monthly_smoke_df <- merge(gam_monthly_smoke_df,
                                            gam_monthly_smoke_df_m1p0m,
                                            all.x = T,
                                            by = "year_month")
              colnames(gam_monthly_smoke_df)[3] <- "N_m0p5m"
              colnames(gam_monthly_smoke_df)[4] <- "N_m1p0m"
              
              gam_monthly_smoke_df[is.na(N_m0p5m), N_m0p5m := 0]
              gam_monthly_smoke_df[is.na(N_m1p0m), N_m1p0m := 0]
              
              title_plot <- 
                paste0("Monthly smoke day count (AQS PM: ", unique(gam_pred_df$AQS_PM), ")")
              
              output$gam_plot_smoke_monthly <- tryCatch({
                renderEcharts4r({
                  e <- gam_monthly_smoke_df %>%
                    e_charts(year_month) %>%  
                    e_bar(serie = N_m0p5m, 
                          name = "No. of smoke days m0p5m", 
                          color = "deepskyblue") %>%
                    e_bar(serie = N_m1p0m, 
                          name = "No. of smoke days m1p0m", 
                          color = "red") %>%
                    e_title(text = title_plot,
                            left = "left",
                            textStyle = list(fontSize = 14)) %>%
                    e_x_axis(name = "Year-Month", 
                             nameLocation = "middle", 
                             nameGap = 40,
                             nameTextStyle = list(fontSize = 14, color = "black"),
                             axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
                    e_y_axis(name = "Count",
                             nameLocation = "middle", 
                             nameGap = 40,
                             nameTextStyle = list(fontSize = 14, color = "black"),
                             axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
                    e_tooltip(trigger = "axis",
                              axisPointer = list(
                                type = "cross"
                              )) %>%
                    e_legend(top = "7%") %>% 
                    e_color(c("deepskyblue", "red")) %>%
                    e_datazoom(type = "inside") %>% 
                    e_datazoom(type = "slider",
                               start = 0,
                               end = 100) %>%
                    e_toolbox(feature = list(saveAsImage = list(name = paste0("plot_smoke_freq_",
                                                                              unique(gam_pred_df$source), 
                                                                              "_aqs_",
                                                                              unique(gam_pred_df$AQS_PM))),
                                             dataZoom = list(), 
                                             restore = list())) %>%
                    e_grid(bottom = "25%") %>%
                    e_animation(duration = 500)
                  
                  e
                })
              }, error = function(e) {
                renderEcharts4r({
                  e_charts() %>%
                    e_title("This plot is not supported with this data.", left = "center", top = "middle") %>%
                    e_text_style(color = "red", fontSize = 15)
                })
              })
              
              ## Exceedance days
              gam_exc_df_m0p5m <- gam_pred_df[, .N, by = .(YEAR, cause_m0p5m)]
              gam_exc_df_m1p0m <- gam_pred_df[, .N, by = .(YEAR, cause_m1p0m)]
              gam_exc_df_wide_m0p5m <- data.table::dcast(gam_exc_df_m0p5m, YEAR ~ cause_m0p5m, value.var = "N", fill = 0)
              gam_exc_df_wide_m1p0m <- data.table::dcast(gam_exc_df_m1p0m, YEAR ~ cause_m1p0m, value.var = "N", fill = 0)
              
              for(col in c("due_to_smoke", "not_caused_by_smoke")) {
                if(!col %in% colnames(gam_exc_df_wide_m0p5m)) {
                  gam_exc_df_wide_m0p5m[, (col) := 0]
                }
              }
              
              for(col in c("due_to_smoke", "not_caused_by_smoke")) {
                if(!col %in% colnames(gam_exc_df_wide_m1p0m)) {
                  gam_exc_df_wide_m1p0m[, (col) := 0]
                }
              }
              
              gam_exc_df_wide <- merge(gam_exc_df_wide_m0p5m,
                                       gam_exc_df_wide_m1p0m,
                                       all = T,
                                       by = "YEAR")
              
              colnames(gam_exc_df_wide)[colnames(gam_exc_df_wide) == "due_to_smoke.x"] <- "due_to_smoke_m0p5m"
              colnames(gam_exc_df_wide)[colnames(gam_exc_df_wide) == "due_to_smoke.y"] <- "due_to_smoke_m1p0m"
              colnames(gam_exc_df_wide)[colnames(gam_exc_df_wide) == "not_caused_by_smoke.x"] <- "not_caused_by_smoke_m0p5m"
              colnames(gam_exc_df_wide)[colnames(gam_exc_df_wide) == "not_caused_by_smoke.y"] <- "not_caused_by_smoke_m1p0m"
              
              legend_1 <- "Days with smoke PM2.5=0 (m0p5m)"
              legend_2 <- "Days with smoke PM2.5>0 (m0p5m)"
              legend_3 <- "Days with smoke PM2.5=0 (m1p0m)"
              legend_4 <- "Days with smoke PM2.5>0 (m1p0m)"
              
              output$gam_plot_exceedance <- tryCatch({
                renderEcharts4r({
                  if(all(gam_exc_df_wide$`due_to_smoke_m0p5m` == 0) && 
                     all(gam_exc_df_wide$`due_to_smoke_m1p0m` == 0) && 
                     all(gam_exc_df_wide$`not_caused_by_m0p5m` == 0) && 
                     all(gam_exc_df_wide$`not_caused_by_m1p0m` == 0)) {
                    e <- e_charts() %>%
                      e_title("No exceedance days can be found in this site", left = "center", top = "middle") %>%
                      e_text_style(color = "red", fontSize = 15)
                  } else {
                    e <- gam_exc_df_wide %>%
                      e_charts(YEAR) %>%
                      e_bar(`not_caused_by_smoke_m0p5m`, 
                            stack = "smoke_m0p5m",
                            name = legend_1, 
                            color = "grey") %>%
                      e_bar(`due_to_smoke_m0p5m`, 
                            stack = "smoke_m0p5m", 
                            name = legend_2, 
                            color = "deepskyblue") %>%
                      e_bar(`not_caused_by_smoke_m1p0m`, 
                            stack = "smoke_m1p0m",
                            name = legend_3, 
                            color = "black") %>%
                      e_bar(`due_to_smoke_m1p0m`, 
                            stack = "smoke_m1p0m", 
                            name = legend_4, 
                            color = "red") %>%
                      e_title(paste0("Annual days with > 9 ug m-3 (AQS PM: ", unique(gam_pred_df$AQS_PM), ")"), 
                              left = "left",
                              textStyle = list(fontSize = 14)) %>%
                      e_x_axis(type = "category",
                               name = "Year",
                               nameLocation = "middle",
                               nameGap = 30,
                               nameTextStyle = list(fontSize = 14, color = "black"),
                               axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
                      e_y_axis(name = "No. of days",
                               nameLocation = "middle",
                               nameGap = 40,
                               nameTextStyle = list(fontSize = 14, color = "black"),
                               axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
                      e_tooltip(trigger = "axis",
                                axisPointer = list(
                                  type = "cross"
                                )) %>%
                      e_legend(show = T, 
                               top = "7%") %>%
                      e_datazoom(type = "inside") %>%
                      e_toolbox(feature = list(saveAsImage = list(name = paste0("plot_exceedance_day_",
                                                                                unique(gam_pred_df$source), 
                                                                                "_aqs_",
                                                                                unique(gam_pred_df$AQS_PM))), 
                                               dataZoom = list(),
                                               restore = list())) %>%
                      e_grid(top = "25%") %>%
                      e_animation(duration = 500)
                  } 
                  e
                })
              }, error = function(e) {
                renderEcharts4r({
                  e_charts() %>%
                    e_title("This plot is not supported with this data.", left = "center", top = "middle") %>%
                    e_text_style(color = "red", fontSize = 15)
                })
              })
              
            } else {
              
              ##### MDA8 plots ----
              gam_pred_df[, MDA8O3_smk := MDA8O3]
              gam_pred_df[, MDA8O3_smk_mark := as.character(MDA8O3_smk)]
              gam_pred_df[smoke == 0, MDA8O3_smk := NA]
              gam_pred_df[smoke == 0, MDA8O3_smk_mark := "N"]
              gam_pred_df[smoke == 1, MDA8O3_smk_mark := "Y"]
              gam_pred_df[, pSMO := SMO]
              gam_pred_df[smoke == 0, pSMO := NA]
              gam_pred_df[, date := as.Date(date)]
              gam_pred_df[, exceedance := ifelse(MDA8O3 > 70, T, F)]
              
              if(data_source == "epa_ember") {
                gam_pred_df[, cause := NA_character_]
                gam_pred_df[exceedance == T & smoke == 0, cause := "not_caused_by_smoke"]
                gam_pred_df[exceedance == T & smoke == 1, cause := "due_to_smoke"]
              } else {
                gam_pred_df[, cause := NA_character_]
                gam_pred_df[exceedance == T, cause := "not_caused_by_smoke"]
                gam_pred_df[exceedance == T & smoke == 1 & MDA8O3_resids > p975, cause := "due_to_smoke"]
              }
              
              gam_pred_df[, MDA8O3 := round(MDA8O3, 1)]
              gam_pred_df[, MDA8O3_pred := round(MDA8O3_pred, 1)]
              gam_pred_df[, MDA8O3_resids := round(MDA8O3_resids, 1)]
              gam_pred_df[, SMO := round(SMO, 1)]
              
              gam_pred_df$group <- factor(gam_pred_df$smoke,
                                          levels = c("0", "1"),
                                          labels = c("Non-smoke day", "Smoke day"))
              
              if(data_source == "epa_ember") {
                gam_pred_df$group <- "Data points"
              }
              
              gam_pred_df <- gam_pred_df[!is.na(smoke)]
              
              
              ## Obs vs Pred plot
              output$gam_plot_obs_vs_pred <- tryCatch({
                renderEcharts4r({
                  e <- gam_pred_df %>%
                    dplyr::group_by(group) %>% 
                    e_charts(MDA8O3_pred) %>%
                    e_scatter(MDA8O3, date, symbolSize = 8) %>% 
                    e_line(MDA8O3_pred, 
                           name = "1:1 line",
                           symbol = "line") %>% 
                    e_mark_line(data = list(yAxis = 70), 
                                title = "70 ppb",
                                lineStyle = list(type = "dashed", color = "grey"),
                                symbol = c("none", "none"),
                                name = "70 ppb") %>%
                    e_mark_line(data = list(xAxis = 70), 
                                title = "",
                                lineStyle = list(type = "dashed", color = "grey"),
                                symbol = c("none", "none"),
                                name = "70 ppb") %>%
                    e_tooltip(trigger = "item",
                              formatter = JS("function(params) {
                          return params.seriesName + '<br/>' +
                          'Date: ' + params.value[2] + '<br/>' +
                          'Observed MDA8 (ppb): ' + params.value[1] + '<br/>' +
                          'Predicted MDA8 (ppb): ' + params.value[0];
                                         }"),
                              axisPointer = list(
                                type = "cross"
                              ),
                              axisPointer = list(
                                type = "cross"
                              )) %>% 
                    e_title(text = paste0("Obs. vs. Pred. (AQS O3: ", unique(gam_pred_df$AQS_O3), ")"),
                            left = "left",
                            textStyle = list(fontSize = 14)) %>%
                    e_x_axis(name = "Predicted MDA8 (ppb)", 
                             scale = T, 
                             nameLocation = "middle", nameGap = 30,
                             nameTextStyle = list(fontSize = 14, color = "black"),
                             axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
                    e_y_axis(name = "Observed MDA8 (ppb)",
                             scale = T, 
                             nameLocation = "middle",
                             nameGap = 40,
                             nameTextStyle = list(fontSize = 14, color = "black"),
                             axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
                    e_legend(top = "7%") %>%
                    e_color(c("black", "red", "blue")) %>% 
                    e_datazoom(type = "inside") %>%
                    e_toolbox(feature = list(saveAsImage = list(name = paste0("plot_obs_vs_pred_",
                                                                              unique(gam_pred_df$source), 
                                                                              "_aqs_",
                                                                              unique(gam_pred_df$AQS_O3))), 
                                             dataZoom = list(),
                                             restore = list())) %>%
                    e_animation(duration = 500)
                  e
                })
              }, error = function(e) {
                renderEcharts4r({
                  e_charts() %>%
                    e_title("This plot is not supported with this data.", left = "center", top = "middle") %>%
                    e_text_style(color = "red", fontSize = 15)
                })
              })
              
              
              ## Obs vs Pred time-series
              output$gam_plot_obs_vs_pred_time <- tryCatch({
                renderEcharts4r({
                  e <- gam_pred_df %>%
                    e_charts(date) %>%
                    e_line(serie = MDA8O3, 
                           name = "Observed MDA8", 
                           lineStyle = list(width = 1, color = "black"), 
                           symbol = "rect",
                           smooth = F) %>%
                    e_line(serie = MDA8O3_pred,
                           name = "Predicted MDA8",
                           lineStyle = list(width = 1, color = "orange"),
                           symbol = "rect",
                           smooth = F) %>%
                    e_mark_line(data = list(yAxis = 70), 
                                title = "70 ppb",
                                lineStyle = list(type = "dashed", color = "grey"),
                                symbol = c("none", "none"),
                                name = "70 ppb") %>%
                    e_tooltip(trigger = "axis",
                              formatter = htmlwidgets::JS("
                            function(params) {
                            let result = params[0].value[0] + '<br/>';
                            params.forEach(function(item) {
                            if (item.seriesName !== 'Day with SMO>0 (Y/N)' && item.seriesName !== 'Smoke day (Y/N)') {
                            result += 
                            item.marker +
                            ' ' + 
                            item.seriesName +
                            ': ' + 
                            item.value[1] + '<br/>';
                            }}); return result;}"),
                              axisPointer = list(
                                type = "cross"
                              )) %>% 
                    e_title(text = paste0("Daily time-series (AQS O3: ", unique(gam_pred_df$AQS_O3), ")"),
                            left = "left",
                            textStyle = list(fontSize = 14)) %>%
                    e_x_axis(type = "time", 
                             name = "Date",
                             nameLocation = "middle",
                             nameGap = 30,
                             nameTextStyle = list(fontSize = 14, color = "black"),
                             axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
                    e_y_axis(name = "MDA8 and SMO (ppb)", 
                             nameLocation = "middle",
                             nameGap = 40,
                             nameTextStyle = list(fontSize = 14, color = "black"),
                             axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
                    e_legend(top = "7%") %>%
                    e_color(c("black", "orange", "blue", "red", "red")) %>% 
                    e_datazoom(type = "inside") %>% 
                    e_datazoom(type = "slider",
                               start = 0,
                               end = 100) %>%
                    e_toolbox(feature = list(saveAsImage = list(name = paste0("plot_obs_pred_time_",
                                                                              unique(gam_pred_df$source), 
                                                                              "_aqs_",
                                                                              unique(gam_pred_df$AQS_O3))), 
                                             dataZoom = list(),
                                             restore = list())) %>%
                    e_grid(bottom = "25%") %>%
                    e_animation(duration = 500)
                  
                  if(data_source == "epa_ember") {
                    e <- e %>%      
                      e_data(gam_pred_df) %>%     
                      e_line(serie = SMO,
                             name = "SMO",
                             lineStyle = list(width = 1, color = "blue"),
                             smooth = F) %>% 
                      e_scatter(serie = MDA8O3_smk_mark,
                                symbolSize = 12,
                                legend = F,
                                name = "Day with SMO>0") %>%
                      e_scatter(serie = MDA8O3_smk, 
                                symbolSize = 12,
                                legend = T,
                                name = "Day with SMO>0 (Y/N)")
                  } else {
                    e <- e %>%
                      e_data(gam_pred_df) %>%
                      e_line(serie = pSMO,
                             name = "SMO",
                             lineStyle = list(width = 1, color = "blue"),
                             smooth = F) %>%
                      e_scatter(serie = MDA8O3_smk_mark,
                                symbolSize = 12,
                                legend = F,
                                name = "Smoke day") %>%
                      e_scatter(serie = MDA8O3_smk,
                                symbolSize = 12,
                                legend = T,
                                name = "Smoke day (Y/N)")
                  }
                  
                  e
                })
              }, error = function(e) {
                renderEcharts4r({
                  e_charts() %>%
                    e_title("This plot is not supported with this data.", left = "center", top = "middle") %>%
                    e_text_style(color = "red", fontSize = 15)
                })
              })
              
              
              ## Monthly smoke frequency
              gam_monthly_smoke_df_yr_mt <- gam_pred_df[, 
                                                        .(N_all = .N), 
                                                        by = .(year_month = format(date, "%Y-%m"))]
              gam_monthly_smoke_df <- gam_pred_df[smoke == 1, 
                                                  .(N = .N, 
                                                    SMO = mean(SMO, na.rm = T)), 
                                                  by = .(year_month = format(date, "%Y-%m"))]
              gam_monthly_smoke_df[, SMO := round(SMO, 1)]
              gam_monthly_smoke_df[is.na(SMO), SMO := 0]
              
              gam_monthly_smoke_df <- merge(gam_monthly_smoke_df_yr_mt,
                                            gam_monthly_smoke_df,
                                            all.x = T,
                                            by = "year_month")
              gam_monthly_smoke_df[is.na(N), N := 0]
              
              if(data_source == "epa_ember") {
                label_bar <- "No. of days with SMO>0"
                title_plot <-
                  paste0("Monthly smoke influenced day count and SMO (AQS O3: ", unique(gam_pred_df$AQS_O3), ")")
              } else {
                label_bar <- "No. of smoke days"
                title_plot <- 
                  paste0("Monthly smoke day count and SMO (AQS O3: ", unique(gam_pred_df$AQS_O3), ")")
              }
              
              output$gam_plot_smoke_monthly <- tryCatch({
                renderEcharts4r({
                  e <- gam_monthly_smoke_df %>%
                    e_charts(year_month) %>%  
                    e_bar(serie = N, 
                          name = label_bar) %>%
                    e_line(serie = SMO,
                           name = "SMO (ppb)",
                           lineStyle = list(width = 1, color = "blue"),
                           symbolSize = 8,
                           smooth = F) %>% 
                    e_title(text = title_plot,
                            left = "left",
                            textStyle = list(fontSize = 14)) %>%
                    e_x_axis(name = "Year-Month", 
                             nameLocation = "middle", 
                             nameGap = 40,
                             nameTextStyle = list(fontSize = 14, color = "black"),
                             axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
                    e_y_axis(name = "Count and ppb",
                             nameLocation = "middle", 
                             nameGap = 40,
                             nameTextStyle = list(fontSize = 14, color = "black"),
                             axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
                    e_tooltip(trigger = "axis",
                              axisPointer = list(
                                type = "cross"
                              )) %>%
                    e_legend(top = "7%") %>% 
                    e_color(c("darkgrey", "blue")) %>%
                    e_datazoom(type = "inside") %>% 
                    e_toolbox(feature = list(saveAsImage = list(name = paste0("plot_smoke_freq_",
                                                                              unique(gam_pred_df$source), 
                                                                              "_aqs_",
                                                                              unique(gam_pred_df$AQS_O3))),
                                             dataZoom = list(), 
                                             restore = list())) %>%
                    e_animation(duration = 500)
                  
                  e
                })
              }, error = function(e) {
                renderEcharts4r({
                  e_charts() %>%
                    e_title("This plot is not supported with this data.", left = "center", top = "middle") %>%
                    e_text_style(color = "red", fontSize = 15)
                })
              })
              
              ## Exceedance days
              gam_exc_df <- gam_pred_df[, .N, by = .(YEAR, cause)]
              gam_exc_df_wide <- data.table::dcast(gam_exc_df, YEAR ~ cause, value.var = "N", fill = 0)
              
              for(col in c("due_to_smoke", "not_caused_by_smoke")) {
                if(!col %in% colnames(gam_exc_df_wide)) {
                  gam_exc_df_wide[, (col) := 0]
                }
              }
              
              if(data_source == "epa_ember") {
                legend_1 <- "Exceedance days with SMO=0"
                legend_2 <- "Exceedance days with SMO>0"
              } else {
                legend_1 <- "Not caused by smoke"
                legend_2 <- "Significant smoke contribution to MDA8"
              }
              
              output$gam_plot_exceedance <- tryCatch({
                renderEcharts4r({
                  if(all(gam_exc_df_wide$`due_to_smoke` == 0) && 
                     all(gam_exc_df_wide$`not_caused_by_smoke` == 0)) {
                    e <- e_charts() %>%
                      e_title("No exceedance days can be found in this site", left = "center", top = "middle") %>%
                      e_text_style(color = "red", fontSize = 15)
                  } else {
                    e <- gam_exc_df_wide %>%
                      e_charts(YEAR) %>%
                      e_bar(`not_caused_by_smoke`, 
                            stack = "smoke",
                            name = legend_1, 
                            color = "blue") %>%
                      e_bar(`due_to_smoke`, 
                            stack = "smoke", 
                            name = legend_2, 
                            color = "red") %>%
                      e_title(paste0("Annual exceedance days (> 70 ppb) (AQS O3: ", unique(gam_pred_df$AQS_O3), ")"), 
                              left = "left",
                              textStyle = list(fontSize = 14)) %>%
                      e_x_axis(type = "category",
                               name = "Year",
                               nameLocation = "middle",
                               nameGap = 30,
                               nameTextStyle = list(fontSize = 14, color = "black"),
                               axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
                      e_y_axis(name = "No. of exceedance days",
                               nameLocation = "middle",
                               nameGap = 40,
                               nameTextStyle = list(fontSize = 14, color = "black"),
                               axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
                      e_tooltip(trigger = "axis",
                                axisPointer = list(
                                  type = "cross"
                                )) %>%
                      e_legend(show = T, 
                               top = "7%") %>%
                      e_datazoom(type = "inside") %>%
                      e_toolbox(feature = list(saveAsImage = list(name = paste0("plot_exceedance_day_",
                                                                                unique(gam_pred_df$source), 
                                                                                "_aqs_",
                                                                                unique(gam_pred_df$AQS_O3))), 
                                               dataZoom = list(),
                                               restore = list())) %>%
                      e_animation(duration = 500)
                  } 
                  e
                })
              }, error = function(e) {
                renderEcharts4r({
                  e_charts() %>%
                    e_title("This plot is not supported with this data.", left = "center", top = "middle") %>%
                    e_text_style(color = "red", fontSize = 15)
                })
              })
            }
          }
        }, error = function(e) {
          cat("Error - observeEvent / run_gam / plotting: ", e$message, "\n")
          NULL
        })
        
        rm(gam_term_df_1,
           gam_term_df_2,
           gam_meta_df_1,
           gam_meta_df_2)
        gc()
        
      } else {
        
        output$gam_previous_meta_table_1 <- DT::renderDataTable({
          DT::datatable(data.table::data.table(), 
                        options = list(dom = "t"),
                        rownames = F)
        })
        
        output$gam_previous_meta_table_2 <- DT::renderDataTable({
          DT::datatable(data.table::data.table(), 
                        options = list(dom = "t"),
                        rownames = F)
        })
        
      }
      
    }, error = function(e) {
      cat("Error - observeEvent / map_site_gam_click: ", e$message, "\n")
      NULL
    })
    
    hidePageSpinner()
    
  }, ignoreInit = T)
  
  observeEvent(input$gam_previous_clear_table, {
    
    leaflet::leafletProxy("map_site_gam") %>%
      leaflet::clearGroup("highlight_marker")
    
    selected_sites_gam(data.table::data.table(source = character(),
                                              state = character(),
                                              site_name = character(),
                                              AQS = character(), 
                                              lon = numeric(),
                                              lat = numeric()))
  }, ignoreInit = T)
  
  observe({
    func_site_select_tbl(output = output, 
                         output_id = "gam_previous_selected_sites", 
                         selected_sites_func = selected_sites_gam(), 
                         delete_input_prefix = "delete_row_gam", 
                         locate_input_prefix = "locate_row_gam", 
                         adlist_input_prefix = NULL, 
                         pageLength = 3, 
                         lengthChange = F)
  })
  
  observeEvent(input$delete_row_gam, {
    tryCatch({
      cat("observeEvent / delete_row_gam... \n")
      
      row_to_remove <- input$delete_row_gam
      
      if(!is.null(row_to_remove)) {
        
        leaflet::leafletProxy("map_site_gam") %>%
          leaflet::clearGroup("highlight_marker")
        
        updated_sites <- selected_sites_gam()
        
        if(row_to_remove <= nrow(updated_sites)) {
          updated_sites <- updated_sites[-row_to_remove, ]
          rownames(updated_sites) <- NULL
          selected_sites_gam(updated_sites)
        }
        
        shinyjs::reset("delete_row_gam")
      }
      
    }, error = function(e) {
      cat("Error - observeEvent / delete_row_gam: ", e$message, "\n")
      NULL
    })
  }, ignoreNULL = T, ignoreInit = T)
  
  func_site_location_status_gam <- reactiveValues(is_highlighted = F, 
                                                  current_row = NULL)
  observeEvent(input$locate_row_gam, {
    func_site_location(input = input, 
                       input_id = "locate_row_gam", 
                       reactive_id = func_site_location_status_gam,
                       selected_sites_func = selected_sites_gam(), 
                       map_id = "map_site_gam")
  }, ignoreNULL = T, ignoreInit = T)
  
  
  #### Data loading & saving ----
  src_gam <- c(
    "gam_v1",
    "gam_v3",
    "gam_v3_edm",
    
    ## Hide/Show dataset (2025-02-27)
    # "gam_v2", 
    # "gam_v2_edm",
    
    "epa_ember",
    "pm_cbsa"
    
  )
  
  dataCall_gam <- reactiveValues(
    gam_v1 = NULL,
    gam_v3 = NULL,
    gam_v3_edm = NULL,
    
    ## Hide/Show dataset (2025-02-27)
    # gam_v2 = NULL,
    # gam_v2_edm = NULL,
    
    epa_ember = NULL,
    pm_cbsa = NULL
  )
  
  observeEvent(input$gam_previous_button_load, {
    
    if(nrow(selected_sites_gam()) < 1) {
      shinyjs::runjs('
      $("#gam_previous_selected_sites").addClass("highlighted");
      setTimeout(function() {
      $("#gam_previous_selected_sites").removeClass("highlighted");
      }, 3000);
      ')
      showNotification("No sites were selected.", 
                       type = "error", duration = 3)
    } else {
      
      sdate_format <- grepl("^\\d{4}-\\d{2}-\\d{2}$", input$date_start_gam)
      edate_format <- grepl("^\\d{4}-\\d{2}-\\d{2}$", input$date_end_gam)
      
      if(!sdate_format) {
        shinyjs::runjs('
        $("#date_start_gam").addClass("highlighted");
        setTimeout(function() {
        $("#date_start_gam").removeClass("highlighted");
        }, 3000);
      ')
        showNotification("Invalid date format. Please use yyyy-mm-dd format.", 
                         type = "error", duration = 3)
        return()
      }
      
      if(!edate_format) {
        shinyjs::runjs('
        $("#date_end_gam").addClass("highlighted");
        setTimeout(function() {
        $("#date_end_gam").removeClass("highlighted");
        }, 3000);
      ')
        showNotification("Invalid date format. Please use yyyy-mm-dd format.", 
                         type = "error", duration = 3)
        return()
      }
      
      if(as.Date(input$date_start_gam) > as.Date(input$date_end_gam)) {
        shinyjs::runjs('
        $("#date_start_gam").addClass("highlighted");
        setTimeout(function() {
          $("#date_start_gam").removeClass("highlighted");
        }, 3000);
        $("#date_end_gam").addClass("highlighted");
        setTimeout(function() {
          $("#date_end_gam").removeClass("highlighted");
        }, 3000);
      ')
        showNotification("Please check the date range. The range (start and end) is incorrect.", 
                         type = "error", duration = 3)
        return()
      }
      
      showPageSpinner()
      tryCatch({
        cat("observeEvent / gam_previous_button_load... \n")
        
        date_range <- c(input$date_start_gam,
                        input$date_end_gam)
        
        filter_sites_by_source <- function(src) {
          if(!is.null(selected_sites_gam()) && nrow(selected_sites_gam()) > 0) {
            selected_sites_gam()[selected_sites_gam()$source == src,]
          } else {
            NULL
          }
        }
        
        target_sites <- lapply(src_gam,
                               filter_sites_by_source)
        names(target_sites) <- src_gam

        for(src in src_gam) {
          output_data <- func_load_data_for_gam(target = target_sites[[src]],
                                                input_source = src, 
                                                date_range = date_range)
          
          if(src %in% c("gam_v1", "gam_v3", "gam_v3_edm")) {
            output_data <- output_data[, .SD, .SDcols = !colnames(output_data) %in% c("SMO")]
          }
          
          if(src == "epa_ember") {
            output_data <- output_data[, .SD, .SDcols = !colnames(output_data) %in% c("AQS_PM",
                                                                                      "AQS_dist_km",
                                                                                      "PM2.5",
                                                                                      "PM2.5_Crit",
                                                                                      "p975",
                                                                                      "pM1p0MAD",
                                                                                      "pM2p0MAD",
                                                                                      "HMS",
                                                                                      "SMO")]
          }
          
          func_render_tbl(output = output,
                          output_id = paste0("data_load_", src),
                          data = output_data, 
                          pageLength = 5)
          dataCall_gam[[src]] <- output_data
        }
        
        rm(target_sites,
           output_data)
        gc()
        
      }, error = function(e) {
        cat("Error - observeEvent / gam_previous_button_load: ", e$message, "\n")
        NULL
      })
      
      hidePageSpinner()
    }
  }, ignoreInit = T)
  
  observeEvent(input$gam_previous_button_load, {
    lapply(src_gam, function(src) {
      
      output_id <- paste0("data_ui_", src)
      data_id <- paste0("data_load_", src)
      download_ui_id <- paste0("download_ui_", src)
      download_button_id <- paste0("download_", src)
      
      output[[output_id]] <- renderUI({
        data <- dataCall_gam[[src]]
        
        if(is.null(data) || nrow(data) == 0) {
          h4("No data available in table",
             style = "text-align: center; color: black;")
        } else {
          div(
            style = "overflow-y: auto; overflow-x: auto;",
            DT::dataTableOutput(data_id)
          )
        }
      })
      
      output[[download_ui_id]] <- renderUI({
        func_render_download_ui( 
          data = dataCall_gam[[src]],
          button_id = download_button_id,
          label = paste("Save data")
        )
      })
      
      output[[download_button_id]] <- func_create_download_handler(
        data = dataCall_gam[[src]],
        date_range = c(input$date_start_gam, input$date_end_gam),
        prefix = data_id
      )
    })
  }, ignoreInit = T)
  
  
  ### Sub-tab: Layer Map ----
  observeEvent(input$prev_day, {
    current_date <- as.Date(input$gam_previous_lyr_date)
    updated_date <- current_date - 1
    updateDateInput(session, "gam_previous_lyr_date", value = updated_date)
  })
  
  observeEvent(input$next_day, {
    current_date <- as.Date(input$gam_previous_lyr_date)
    updated_date <- current_date + 1
    updateDateInput(session, "gam_previous_lyr_date", value = updated_date)
  })
  
  observeEvent(input$prev_week, {
    current_date <- as.Date(input$gam_previous_lyr_date)
    updated_date <- current_date - 7
    updateDateInput(session, "gam_previous_lyr_date", value = updated_date)
  })
  
  observeEvent(input$next_week, {
    current_date <- as.Date(input$gam_previous_lyr_date)
    updated_date <- current_date + 7
    updateDateInput(session, "gam_previous_lyr_date", value = updated_date)
  })
  
  observeEvent(input$prev_month, {
    current_date <- as.Date(input$gam_previous_lyr_date)
    updated_date <- current_date %m-% months(1)
    updateDateInput(session, "gam_previous_lyr_date", value = updated_date)
  })
  
  observeEvent(input$next_month, {
    current_date <- as.Date(input$gam_previous_lyr_date)
    updated_date <- current_date %m+% months(1)
    updateDateInput(session, "gam_previous_lyr_date", value = updated_date)
  })
  
  observeEvent(input$prev_year, {
    current_date <- as.Date(input$gam_previous_lyr_date)
    updated_date <- current_date %m-% years(1)
    updateDateInput(session, "gam_previous_lyr_date", value = updated_date)
  })
  
  observeEvent(input$next_year, {
    current_date <- as.Date(input$gam_previous_lyr_date)
    updated_date <- current_date %m+% years(1)
    updateDateInput(session, "gam_previous_lyr_date", value = updated_date)
  })
  
  category_val_o3 <- c(-Inf, 55, 71, 86, 106, 201, Inf)
  category_val_resi <- c(-Inf, -10, -5, 0, 5, 10, Inf)
  category_val_smo_gam <- c(-Inf, -10, -5, 0, 5, 10, Inf)
  category_val_smo_ember <- c(-Inf, 1, 3, 5, 7, 10, Inf)
  category_val_pm <- c(-Inf, 9.1, 35.5, 55.5, 125.5, 225.5, Inf)
  category_val_pm_quant <- c(-Inf, 10, 30, 50, 70, 90, Inf)
  category_val_pmcrit <- c(-Inf, 5, 7.5, 10, 12.5, 15, Inf)
  category_val_r2 <- c(0, 0.2, 0.4, 0.55, 0.7, 0.85, 1)
  
  category_name_o3 <- c("< 55", "55 to < 71", "71 to < 86", "86 to < 106", "106 to < 201", "> 201")
  category_name_resi <- c("< -10", "-10 to < -5", "-5 to < 0", "0 to < 5", "5 to < 10", "> 10")
  category_name_smo_gam <- c("< -10", "-10 to < -5", "-5 to < 0", "0 to < 5", "5 to < 10", "> 10")
  category_name_smo_ember <- c("< 1", "1 to < 3", "3 to < 5", "5 to < 7", "7 to < 10", "> 10")
  category_name_pm <- c("< 9.1", "9.1 to < 35.5", "35.5 to < 55.5", "55.5 to < 125.5", "125.5 to < 225.5", "> 225.5")
  category_name_pm_quant <- c("< 10", "10 to < 30", "30 to < 50", "50 to < 70", "70 to < 90", "> 90")
  category_name_pmcrit <- c("< 5", "5 to < 7.5", "7.5 to < 10", "10 to < 12.5", "12.5 to < 15", "> 15")
  category_name_r2 <- c("< 0.2", "0.2 to < 0.4", "0.4 to < 0.55", "0.55 to < 0.7", "0.7 to < 0.85", "> 0.85")
  
  lyr_dat <- reactive({
    
    lyr_tmp <- NULL

    tryCatch({
      data_source <- input$gam_previous_lyr_gam_ver
      input_date <- input$gam_previous_lyr_date    
      
      folder_name <- "data_by_date"
      file_name <- paste0("data_by_date_", input_date, ".rds")
      
      lyr_tmp <- readRDS(file.path("src/data", folder_name, file_name))
      lyr_tmp <- lyr_tmp[[data_source]]
      
      if(data_source == "epa_ember") {
        lyr_tmp <- lyr_tmp[, ..column_for_emb_lyr]
      } else if(data_source == "pm_cbsa") {
        lyr_tmp <- lyr_tmp
      } else {
        lyr_tmp <- lyr_tmp[, ..column_for_gam_lyr]
        lyr_tmp[, MDA8O3_pred := round(MDA8O3_pred, 1)]
        lyr_tmp[, MDA8O3_resids := round(MDA8O3_resids, 1)]
        lyr_tmp[smoke == 0, SMO := NA]
      }
      
      tryCatch({
        lyr_tmp[, MDA8O3_obs_category := cut(
          MDA8O3,
          breaks = category_val_o3,
          labels = category_name_o3,
          right = F
        )]
      }, error = function(e) {
        lyr_tmp[, MDA8O3_obs_category := NA]
      })
      
      tryCatch({
        lyr_tmp[, MDA8O3_pred_category := cut(
          MDA8O3_pred,
          breaks = category_val_o3,
          labels = category_name_o3,
          right = F
        )]
      }, error = function(e) {
        lyr_tmp[, MDA8O3_pred_category := NA]
      })
      
      tryCatch({
        lyr_tmp[, Resi_category := cut(
          MDA8O3_resids,
          breaks = category_val_resi,
          labels = category_name_resi,
          right = F
        )]
      }, error = function(e) {
        lyr_tmp[, Resi_category := NA]
      })
      
      if(data_source == "epa_ember") {
        tryCatch({
          lyr_tmp[, SMO_category := cut(
            SMO,
            breaks = category_val_smo_ember,
            labels = category_name_smo_ember,
            right = F
          )]
        }, error = function(e) {
          lyr_tmp[, SMO_category := NA]
        })
      } else {
        tryCatch({
          lyr_tmp[, SMO_category := cut(
            SMO,
            breaks = category_val_smo_gam,
            labels = category_name_smo_gam,
            right = F
          )]
        }, error = function(e) {
          lyr_tmp[, SMO_category := NA]
        })
      }
      
      tryCatch({
        lyr_tmp[, PM2.5_category := cut(
          PM2.5,
          breaks = category_val_pm,
          labels = category_name_pm,
          right = F
        )]
      }, error = function(e) {
        lyr_tmp[, PM2.5_category := NA]
      })
      
      tryCatch({
        lyr_tmp[, PM2.5_Crit_category := cut(
          PM2.5_Crit,
          breaks = category_val_pmcrit,
          labels = category_name_pmcrit,
          right = F
        )]
      }, error = function(e) {
        lyr_tmp[, PM2.5_Crit_category := NA]
      })
      
      ## For PM2.5 CBSA
      tryCatch({
        lyr_tmp[, Quant_PM2.5_category := cut(
          Quant_PM2.5,
          breaks = category_val_pm_quant,
          labels = category_name_pm_quant,
          right = F
        )]
      }, error = function(e) {
        lyr_tmp[, Quant_PM2.5_category := NA]
      })
      
      tryCatch({
        lyr_tmp[, smoke_PM2.5_m0p5m_category := cut(
          smoke_PM2.5_m0p5m,
          breaks = category_val_pm,
          labels = category_name_pm,
          right = F
        )]
      }, error = function(e) {
        lyr_tmp[, smoke_PM2.5_m0p5m_category := NA]
      })
      
      tryCatch({
        lyr_tmp[, smoke_PM2.5_m1p0m_category := cut(
          smoke_PM2.5_m1p0m,
          breaks = category_val_pm,
          labels = category_name_pm,
          right = F
        )]
      }, error = function(e) {
        lyr_tmp[, smoke_PM2.5_m1p0m_category := NA]
      })
      
      tryCatch({
        lyr_tmp[, PM2.5_Crit_m0p5m_category := cut(
          PM2.5_Crit_m0p5m,
          breaks = category_val_pmcrit,
          labels = category_name_pmcrit,
          right = F
        )]
      }, error = function(e) {
        lyr_tmp[, PM2.5_Crit_m0p5m_category := NA]
      })
      
      tryCatch({
        lyr_tmp[, PM2.5_Crit_m1p0m_category := cut(
          PM2.5_Crit_m1p0m,
          breaks = category_val_pmcrit,
          labels = category_name_pmcrit,
          right = F
        )]
      }, error = function(e) {
        lyr_tmp[, PM2.5_Crit_m1p0m_category := NA]
      })
    }, error = function(e) {
      lyr_tmp <- NULL
    })

    lyr_tmp
  })
  
  ##### For advanced searching ----
  selected_sites_gam_adv_lyr <- reactive({
    search_input <- input$gam_previous_search_input_lyr
    data_gam_stat()[
      stri_detect_regex(state, search_input, case_insensitive = T) |
        stri_detect_regex(AQS, search_input, case_insensitive = T) |
        stri_detect_regex(site_name, search_input, case_insensitive = T)
    ]
  })
  
  observe({
    updateSelectizeInput(session,
                         inputId = "gam_previous_search_input_lyr",
                         choices = site_search_term_previous(),
                         selected = NULL,
                         server = F)
  })
  
  observe({
    func_site_select_tbl(output = output, 
                         output_id = "gam_previous_search_results_lyr", 
                         selected_sites_func = selected_sites_gam_adv_lyr(), 
                         delete_input_prefix = NULL, 
                         locate_input_prefix = "locate_row_gam_adv_lyr",
                         adlist_input_prefix = NULL, 
                         pageLength = 3, 
                         lengthChange = F)
  })
  
  func_site_location_status_gam_adv_lyr <- reactiveValues(is_highlighted = F, 
                                                          current_row = NULL)
  observeEvent(input$locate_row_gam_adv_lyr, {
    func_site_location(input = input, 
                       input_id = "locate_row_gam_adv_lyr", 
                       reactive_id = func_site_location_status_gam_adv_lyr,
                       selected_sites_func = selected_sites_gam_adv_lyr(), 
                       map_id = "map_site_lyr")
  }, ignoreNULL = T, ignoreInit = T)
  
  
  ##### Reactive values ----
  lyr_pts <- reactiveValues()
  lyr_ras <- reactiveValues()
  lyr_hms <- reactiveValues()
  lyr_fire <- reactiveValues()
  
  observe({
    if(input$gam_previous_lyr_gam_ver == "epa_ember") {
      shinyjs::hide("param_desc_gam")
      shinyjs::show("param_desc_ember")
      shinyjs::hide("param_desc_pm_cbsa")
    } else if(input$gam_previous_lyr_gam_ver == "pm_cbsa") {
      shinyjs::hide("param_desc_gam")
      shinyjs::hide("param_desc_ember")
      shinyjs::show("param_desc_pm_cbsa")
    } else {
      shinyjs::show("param_desc_gam")
      shinyjs::hide("param_desc_ember")
      shinyjs::hide("param_desc_pm_cbsa")
    }
  })
  
  lyr_enable_ids <- c(
    "gam_previous_lyr_pts_o3_obs",
    "gam_previous_lyr_pts_o3_pred",
    "gam_previous_lyr_pts_res",
    "gam_previous_lyr_pts_smo",
    "gam_previous_lyr_pts_pm_obs",
    "gam_previous_lyr_pts_pm_quant",
    "gam_previous_lyr_pts_pm_crit",
    "gam_previous_lyr_pts_pm_crit_m0p5m",
    "gam_previous_lyr_pts_pm_crit_m1p0m",
    "gam_previous_lyr_pts_smoke_pm_m0p5m",
    "gam_previous_lyr_pts_smoke_pm_m1p0m",
    "gam_previous_lyr_pts_r2",
    "gam_previous_lyr_pts_rank",
    "gam_previous_lyr_ras_o3_obs",
    "gam_previous_lyr_ras_o3_pred",
    "gam_previous_lyr_ras_res",
    "gam_previous_lyr_ras_smo",
    "gam_previous_lyr_ras_pm_obs",
    "gam_previous_lyr_ras_pm_quant",
    "gam_previous_lyr_ras_pm_crit",
    "gam_previous_lyr_ras_pm_crit_m0p5m",
    "gam_previous_lyr_ras_pm_crit_m1p0m",
    "gam_previous_lyr_ras_smoke_pm_m0p5m",
    "gam_previous_lyr_ras_smoke_pm_m1p0m",
    "gam_previous_lyr_ras_r2",
    "gam_previous_lyr_hms",
    "gam_previous_lyr_fire",
    "gam_previous_lyr_pts_smoke_mark",
    "gam_previous_lyr_pts_smoke_mark_m0p5m",
    "gam_previous_lyr_pts_smoke_mark_m1p0m",
    "gam_previous_lyr_pts_smo_pM1p0MAD",
    "gam_previous_lyr_pts_smo_pM2p0MAD",
    "gam_previous_lyr_pts_smo_p975"
  )
  
  observe({
    if(!input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {
      lapply(lyr_enable_ids, function(id) {
        shinyjs::disable(id = id)
        shinyjs::runjs(sprintf("$('#%s').closest('.checkbox').find('label').addClass('disabled-label');", id))
      })
    } else {
      lapply(lyr_enable_ids, function(id) {
        shinyjs::enable(id = id)
        shinyjs::runjs(sprintf("$('#%s').closest('.checkbox').find('label').removeClass('disabled-label');", id))
      })
    }
  })
  
  lyr_hide_ids_o3 <- c(
    "gam_previous_lyr_pts_pm_quant",
    "gam_previous_lyr_pts_pm_crit_m0p5m",
    "gam_previous_lyr_pts_pm_crit_m1p0m",
    "gam_previous_lyr_pts_smoke_pm_m0p5m",
    "gam_previous_lyr_pts_smoke_pm_m1p0m",
    "gam_previous_lyr_ras_pm_quant",
    "gam_previous_lyr_ras_pm_crit_m0p5m",
    "gam_previous_lyr_ras_pm_crit_m1p0m",
    "gam_previous_lyr_ras_smoke_pm_m0p5m",
    "gam_previous_lyr_ras_smoke_pm_m1p0m",
    "gam_previous_lyr_pts_smoke_mark_m0p5m",
    "gam_previous_lyr_pts_smoke_mark_m1p0m"
  )
  
  lyr_hide_ids_epa_ember <- c(
    "gam_previous_lyr_pts_pm_obs", 
    "gam_previous_lyr_pts_pm_quant",
    "gam_previous_lyr_pts_pm_crit",
    "gam_previous_lyr_pts_smoke_pm_m0p5m",
    "gam_previous_lyr_pts_smoke_pm_m1p0m",
    "gam_previous_lyr_ras_pm_obs", 
    "gam_previous_lyr_ras_pm_quant",
    "gam_previous_lyr_ras_pm_crit", 
    "gam_previous_lyr_ras_smoke_pm_m0p5m",
    "gam_previous_lyr_ras_smoke_pm_m1p0m",
    "gam_previous_lyr_pts_rank", 
    "gam_previous_lyr_pts_smoke_mark",
    "gam_previous_lyr_pts_smoke_mark_m0p5m",
    "gam_previous_lyr_pts_smoke_mark_m1p0m",
    "gam_previous_lyr_pts_smo_pM1p0MAD",
    "gam_previous_lyr_pts_smo_pM2p0MAD", 
    "gam_previous_lyr_pts_smo_p975"
  )
  
  lyr_hide_ids_pm_cbsa <- c(
    "gam_previous_lyr_pts_o3_obs",
    "gam_previous_lyr_pts_o3_pred",
    "gam_previous_lyr_pts_res", 
    "gam_previous_lyr_pts_smo",
    "gam_previous_lyr_pts_pm_crit",
    "gam_previous_lyr_pts_r2", 
    "gam_previous_lyr_pts_rank",
    "gam_previous_lyr_ras_o3_obs", 
    "gam_previous_lyr_ras_o3_pred", 
    "gam_previous_lyr_ras_res",
    "gam_previous_lyr_ras_smo", 
    "gam_previous_lyr_ras_pm_crit",
    "gam_previous_lyr_ras_r2",
    "gam_previous_lyr_pts_smoke_mark", 
    "gam_previous_lyr_pts_smo_pM1p0MAD", 
    "gam_previous_lyr_pts_smo_pM2p0MAD", 
    "gam_previous_lyr_pts_smo_p975"
  )
  
  observe({
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {
      
      lapply(lyr_enable_ids, function(id) {
        shinyjs::show(id)
        shinyjs::runjs(sprintf("$('#%s').closest('.checkbox').find('label').removeClass('disabled-label');", id))
      })
      
      if(input$gam_previous_lyr_gam_ver != "pm_cbsa") {
        lapply(lyr_hide_ids_o3, function(id) {
          shinyjs::hide(id)
          shinyjs::runjs(sprintf("$('#%s').closest('.checkbox').find('label').addClass('disabled-label');", id))
        })
      }
      
      if(input$gam_previous_lyr_gam_ver == "epa_ember") {
        lapply(lyr_hide_ids_epa_ember, function(id) {
          shinyjs::hide(id)
          shinyjs::runjs(sprintf("$('#%s').closest('.checkbox').find('label').addClass('disabled-label');", id))
        })
      }
      
      if(input$gam_previous_lyr_gam_ver == "pm_cbsa") {
        lapply(lyr_hide_ids_pm_cbsa, function(id) {
          shinyjs::hide(id)
          shinyjs::runjs(sprintf("$('#%s').closest('.checkbox').find('label').addClass('disabled-label');", id))
        })
      }
    }
  })
  
  ##### Preprocessed raster ----
  processed_data_list <- list.files("src/data")
  
  observe({
    rast_source <- input$gam_previous_lyr_gam_ver
    r2_file <- paste0("Processed_rast_list_r2.rds")
    name_r2 <- paste0("2000-01-01", 
                      "_", rast_source, 
                      "_", "adj_rsq")
    
    if(r2_file %in% processed_data_list) {
      r2_proc <- readRDS(file.path("src/data", r2_file))
      lyr_ras[[name_r2]] <- r2_proc[[rast_source]]
    }
    rm(r2_proc)
  })
  
  observe({
    rast_source <- input$gam_previous_lyr_gam_ver
    gam_date <- as.character(input$gam_previous_lyr_date)
    rast_file <- paste0("Processed_rast_list_data_", rast_source, "_", lubridate::year(gam_date), ".rds")
    
    if(rast_source == "pm_cbsa") {
      
      name_pm25 <- paste0(gam_date, 
                          "_", rast_source, 
                          "_", "PM2.5")
      name_pm25_quant <- paste0(gam_date, 
                                "_", rast_source, 
                                "_", "Quant_PM2.5")
      name_pmcrit_m0p5m <- paste0(gam_date, 
                                  "_", rast_source, 
                                  "_", "PM2.5_Crit_m0p5m")
      name_pmcrit_m1p0m <- paste0(gam_date, 
                                  "_", rast_source, 
                                  "_", "PM2.5_Crit_m1p0m")
      name_pm25_smoke_m0p5m <- paste0(gam_date, 
                                      "_", rast_source, 
                                      "_", "smoke_PM2.5_m0p5m")
      name_pm25_smoke_m1p0m <- paste0(gam_date, 
                                      "_", rast_source, 
                                      "_", "smoke_PM2.5_m1p0m")
      
      if(rast_file %in% processed_data_list) {
        rast_proc <- readRDS(file.path("src/data", rast_file))
        
        if(gam_date %in% names(rast_proc)) {
          lyr_ras[[name_pm25]] <- rast_proc[[gam_date]]$pm25
          lyr_ras[[name_pm25_quant]] <- rast_proc[[gam_date]]$pm25_quant
          lyr_ras[[name_pmcrit_m0p5m]] <- rast_proc[[gam_date]]$pm25_crit_m0p5m
          lyr_ras[[name_pmcrit_m1p0m]] <- rast_proc[[gam_date]]$pm25_crit_m1p0m
          lyr_ras[[name_pm25_smoke_m0p5m]] <- rast_proc[[gam_date]]$pm25_smoke_m0p5m
          lyr_ras[[name_pm25_smoke_m1p0m]] <- rast_proc[[gam_date]]$pm25_smoke_m1p0m
        }
        rm(rast_proc)
      }
      
    } else {
      
      name_o3_obs <- paste0(gam_date, 
                            "_", rast_source, 
                            "_", "MDA8O3")
      name_o3_pred <- paste0(gam_date, 
                             "_", rast_source, 
                             "_", "MDA8O3_pred")
      name_resids <- paste0(gam_date, 
                            "_", rast_source, 
                            "_", "MDA8O3_resids")
      name_smo <- paste0(gam_date, 
                         "_", rast_source, 
                         "_", "SMO")
      name_pm25 <- paste0(gam_date, 
                          "_", rast_source, 
                          "_", "PM2.5")
      name_pmcrit <- paste0(gam_date, 
                            "_", rast_source, 
                            "_", "PM2.5_Crit")
      
      if(rast_file %in% processed_data_list) {
        rast_proc <- readRDS(file.path("src/data", rast_file))
        
        if(gam_date %in% names(rast_proc)) {
          lyr_ras[[name_o3_obs]] <- rast_proc[[gam_date]]$o3
          lyr_ras[[name_o3_pred]] <- rast_proc[[gam_date]]$o3_pred
          lyr_ras[[name_resids]] <- rast_proc[[gam_date]]$resids
          lyr_ras[[name_smo]] <- rast_proc[[gam_date]]$smo
          lyr_ras[[name_pm25]] <- rast_proc[[gam_date]]$pm25
          lyr_ras[[name_pmcrit]] <- rast_proc[[gam_date]]$pm25_crit
        }
        rm(rast_proc)
      }
    }
  })
  
  ##### Obs MDA8 mapping ----
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_pts_o3_obs,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {    
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {  
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_pts_o3_obs") %>%
        leaflet::removeControl("lyr_pts_o3_obs_legend")
      
      if(!is.null(lyr_dat())) {
        func_layer_points(input = input,
                          reactive_id = lyr_pts,
                          map_id = "map_site_lyr",
                          z = "MDA8O3_obs_category",
                          input_group = "gam_previous_lyr_pts_o3_obs",
                          output_group = "lyr_pts_o3_obs",
                          input_data = lyr_dat(),
                          gam_previous_lyr_date = input$gam_previous_lyr_date,
                          gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                          legend_title = "Obs MDA8 (ppb)",
                          legend_color = "categorical_epa",
                          legend_category_name = category_name_o3,
                          legend_position = "bottomleft")
      }

    }
  }, ignoreInit = T)
  
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_ras_o3_obs,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {    
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_ras_o3_obs") %>%
        leaflet::removeControl("lyr_ras_o3_obs_legend")
      
      if(!is.null(lyr_dat())) {
        func_layer_raster(input = input,
                          reactive_id = lyr_ras,
                          map_id = "map_site_lyr",
                          z = "MDA8O3", 
                          zlim = c(0, 70),
                          input_group = "gam_previous_lyr_ras_o3_obs",
                          output_group = "lyr_ras_o3_obs",
                          input_data = lyr_dat(),
                          gam_previous_lyr_date = input$gam_previous_lyr_date,
                          gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                          legend_title = "Obs MDA8 (ppb)",
                          legend_color = "viridis",
                          legend_position = "bottomright")
      }

    }
  }, ignoreInit = T)
  
  ##### Pred MDA8 mapping ----
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_pts_o3_pred,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {    
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_pts_o3_pred") %>%
        leaflet::removeControl("lyr_pts_o3_pred_legend")
      
      if(!is.null(lyr_dat())) {
        func_layer_points(input = input,
                          reactive_id = lyr_pts,
                          map_id = "map_site_lyr",
                          z = "MDA8O3_pred_category",
                          input_group = "gam_previous_lyr_pts_o3_pred",
                          output_group = "lyr_pts_o3_pred",
                          input_data = lyr_dat(),
                          gam_previous_lyr_date = input$gam_previous_lyr_date,
                          gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                          legend_title = "Pred MDA8 (ppb)",
                          legend_color = "categorical_epa",
                          legend_category_name = category_name_o3,
                          legend_position = "bottomleft")
      }

    }
  }, ignoreInit = T)
  
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_ras_o3_pred,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {    
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {      
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_ras_o3_pred") %>%
        leaflet::removeControl("lyr_ras_o3_pred_legend")
      
      if(!is.null(lyr_dat())) {
        func_layer_raster(input = input,
                          reactive_id = lyr_ras,
                          map_id = "map_site_lyr",
                          z = "MDA8O3_pred", 
                          zlim = c(0, 70),
                          input_group = "gam_previous_lyr_ras_o3_pred",
                          output_group = "lyr_ras_o3_pred",
                          input_data = lyr_dat(),
                          gam_previous_lyr_date = input$gam_previous_lyr_date,
                          gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                          legend_title = "Pred MDA8 (ppb)",
                          legend_color = "viridis",
                          legend_position = "bottomright")
      }

    }
  }, ignoreInit = T)
  
  ##### Resids mapping ----
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_pts_res,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {    
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {      
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_pts_res") %>%
        leaflet::removeControl("lyr_pts_res_legend")
      
      if(!is.null(lyr_dat())) {
        func_layer_points(input = input,
                          reactive_id = lyr_pts,
                          map_id = "map_site_lyr",
                          z = "Resi_category",
                          input_group = "gam_previous_lyr_pts_res",
                          output_group = "lyr_pts_res",
                          input_data = lyr_dat(),
                          gam_previous_lyr_date = input$gam_previous_lyr_date,
                          gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                          legend_title = "Residual (ppb)",
                          legend_color = "categorical_epa",
                          legend_category_name = category_name_resi,
                          legend_position = "bottomleft")
      }

    }
  }, ignoreInit = T)
  
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_ras_res,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {    
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {      
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_ras_res") %>%
        leaflet::removeControl("lyr_ras_res_legend")
      
      if(!is.null(lyr_dat())) {
        func_layer_raster(input = input,
                          reactive_id = lyr_ras,
                          map_id = "map_site_lyr",
                          z = "MDA8O3_resids",
                          zlim = c(-10, 10),
                          input_group = "gam_previous_lyr_ras_res",
                          output_group = "lyr_ras_res",
                          input_data = lyr_dat(),
                          gam_previous_lyr_date = input$gam_previous_lyr_date,
                          gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                          legend_title = "Residual (ppb)",
                          legend_color = "jet",
                          legend_position = "bottomright")
      }

    }
  }, ignoreInit = T)
  
  ##### SMO mapping ----
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_pts_smo,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {    
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_pts_smo") %>%
        leaflet::removeControl("lyr_pts_smo_legend")
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_pts_smo_mark") %>%
        leaflet::removeControl("lyr_pts_smo_mark_legend")
      
      if(!is.null(lyr_dat())) {
        if(input$gam_previous_lyr_gam_ver == "epa_ember") {
          func_layer_points(input = input,
                            reactive_id = lyr_pts,
                            map_id = "map_site_lyr",
                            z = "SMO_category",
                            input_group = "gam_previous_lyr_pts_smo",
                            output_group = "lyr_pts_smo",
                            input_data = lyr_dat(),
                            gam_previous_lyr_date = input$gam_previous_lyr_date,
                            gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                            legend_title = "SMO (ppb)",
                            legend_color = "categorical_epa",
                            legend_category_name = category_name_smo_ember,
                            legend_position = "bottomleft")
          
        } else {
          if(input$gam_previous_lyr_gam_ver != "pm_cbsa") {
            func_layer_points(input = input,
                              reactive_id = lyr_pts,
                              map_id = "map_site_lyr",
                              z = "SMO_category",
                              input_group = "gam_previous_lyr_pts_smo",
                              output_group = "lyr_pts_smo",
                              input_data = lyr_dat()[smoke == 1],
                              gam_previous_lyr_date = input$gam_previous_lyr_date,
                              gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                              legend_title = "SMO (ppb)",
                              legend_color = "categorical_epa",
                              legend_category_name = category_name_smo_gam,
                              legend_position = "bottomleft")
            
            func_layer_points_mark(input = input,
                                   map_id = "map_site_lyr",
                                   input_group = "gam_previous_lyr_pts_smo",
                                   output_group = "lyr_pts_smo_mark",
                                   input_data = lyr_dat()[smoke == 0],
                                   gam_previous_lyr_date = input$gam_previous_lyr_date,
                                   gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver,
                                   fill_color = "white",
                                   border_lwd = 1.5,
                                   border_opacity = 0.75,
                                   method = "point",
                                   legend_title = "Non-smoke day", 
                                   legend_position = "bottomleft")
          }
        }
      }
      
    }
    
  }, ignoreInit = T)
  
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_ras_smo,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {    
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {      
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_ras_smo") %>%
        leaflet::removeControl("lyr_ras_smo_legend")
      
      if(!is.null(lyr_dat())) {
        if(input$gam_previous_lyr_gam_ver == "epa_ember") {
          z_lim <- c(0, 10)
          func_layer_raster(input = input,
                            reactive_id = lyr_ras,
                            map_id = "map_site_lyr",
                            z = "SMO",
                            zlim = z_lim,
                            input_group = "gam_previous_lyr_ras_smo",
                            output_group = "lyr_ras_smo",
                            input_data = lyr_dat(),
                            gam_previous_lyr_date = input$gam_previous_lyr_date,
                            gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                            raster_na_out = F, 
                            legend_title = "SMO (ppb)",
                            legend_color = "jet",
                            legend_position = "bottomright")
        } else {
          if(input$gam_previous_lyr_gam_ver != "pm_cbsa") {
            z_lim <- c(-10, 10)
            func_layer_raster(input = input,
                              reactive_id = lyr_ras,
                              map_id = "map_site_lyr",
                              z = "SMO",
                              zlim = z_lim,
                              input_group = "gam_previous_lyr_ras_smo",
                              output_group = "lyr_ras_smo",
                              input_data = lyr_dat()[smoke == 1],
                              gam_previous_lyr_date = input$gam_previous_lyr_date,
                              gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                              raster_na_out = T, 
                              legend_title = "SMO (ppb)",
                              legend_color = "jet",
                              legend_position = "bottomright")
          }
        }
      }

    }
  }, ignoreInit = T)
  
  ##### PM2.5 mapping ----
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_pts_pm_obs,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {       
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") { 
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_pts_pm_obs") %>%
        leaflet::removeControl("lyr_pts_pm_obs_legend")
      
      if(!is.null(lyr_dat())) {
        func_layer_points(input = input,
                          reactive_id = lyr_pts,
                          map_id = "map_site_lyr",
                          z = "PM2.5_category",
                          input_group = "gam_previous_lyr_pts_pm_obs",
                          output_group = "lyr_pts_pm_obs",
                          input_data = lyr_dat(),
                          gam_previous_lyr_date = input$gam_previous_lyr_date,
                          gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                          legend_title = "PM2.5 (ug m^-3)",
                          legend_color = "categorical_epa",
                          legend_category_name = category_name_pm,
                          legend_position = "bottomleft")
      }

    }
  }, ignoreInit = T)
  
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_ras_pm_obs,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {    
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_ras_pm_obs") %>%
        leaflet::removeControl("lyr_ras_pm_obs_legend")
      
      if(!is.null(lyr_dat())) {
        func_layer_raster(input = input,
                          reactive_id = lyr_ras,
                          map_id = "map_site_lyr",
                          z = "PM2.5", 
                          zlim = c(0, 35),
                          input_group = "gam_previous_lyr_ras_pm_obs",
                          output_group = "lyr_ras_pm_obs",
                          input_data = lyr_dat(),
                          gam_previous_lyr_date = input$gam_previous_lyr_date,
                          gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                          legend_title = "PM2.5 (ug m^-3)",
                          legend_color = "BrBG",
                          legend_position = "bottomright")
      }

    }
  }, ignoreInit = T)
  
  
  ##### Quant PM2.5 mapping (for PM CBSA) ----
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_pts_pm_quant,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {       
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") { 
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_pts_pm_quant") %>%
        leaflet::removeControl("lyr_pts_pm_quant_legend")
      
      if(!is.null(lyr_dat())) {
        if(input$gam_previous_lyr_gam_ver == "pm_cbsa") {
          func_layer_points(input = input,
                            reactive_id = lyr_pts,
                            map_id = "map_site_lyr",
                            z = "Quant_PM2.5_category",
                            input_group = "gam_previous_lyr_pts_pm_quant",
                            output_group = "lyr_pts_pm_quant",
                            input_data = lyr_dat(),
                            gam_previous_lyr_date = input$gam_previous_lyr_date,
                            gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                            legend_title = "PM2.5 quantile (%)",
                            legend_color = "categorical_epa",
                            legend_category_name = category_name_pm_quant,
                            legend_position = "bottomleft")
        }
      }

    }
  }, ignoreInit = T)
  
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_ras_pm_quant,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {       
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") { 
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_ras_pm_quant") %>%
        leaflet::removeControl("lyr_ras_pm_quant_legend")
      
      if(!is.null(lyr_dat())) {
        if(input$gam_previous_lyr_gam_ver == "pm_cbsa") {
          func_layer_raster(input = input,
                            reactive_id = lyr_ras,
                            map_id = "map_site_lyr",
                            z = "Quant_PM2.5", 
                            zlim = c(0, 100),
                            input_group = "gam_previous_lyr_ras_pm_quant",
                            output_group = "lyr_ras_pm_quant",
                            input_data = lyr_dat(),
                            gam_previous_lyr_date = input$gam_previous_lyr_date,
                            gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                            legend_title = "PM2.5 quantile (%)",
                            legend_position = "bottomright")
        }
      }

    }
  }, ignoreInit = T)
  
  
  ##### Smoke PM2.5 mapping (for PM CBSA) ----
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_pts_smoke_pm_m0p5m,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {       
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") { 
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_pts_smoke_pm_m0p5m") %>%
        leaflet::removeControl("lyr_pts_smoke_pm_m0p5m_legend")
      
      if(!is.null(lyr_dat())) {
        if(input$gam_previous_lyr_gam_ver == "pm_cbsa") {
          func_layer_points(input = input,
                            reactive_id = lyr_pts,
                            map_id = "map_site_lyr",
                            z = "smoke_PM2.5_m0p5m_category",
                            input_group = "gam_previous_lyr_pts_smoke_pm_m0p5m",
                            output_group = "lyr_pts_smoke_pm_m0p5m",
                            input_data = lyr_dat(),
                            gam_previous_lyr_date = input$gam_previous_lyr_date,
                            gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                            legend_title = "Smoke PM2.5 m0p5m (ug m^-3)",
                            legend_color = "categorical_epa",
                            legend_category_name = category_name_pm,
                            legend_position = "bottomleft")
        }
      }

    }
  }, ignoreInit = T)
  
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_ras_smoke_pm_m0p5m,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {    
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_ras_smoke_pm_m0p5m") %>%
        leaflet::removeControl("lyr_ras_smoke_pm_m0p5m_legend")
      
      if(!is.null(lyr_dat())) {
        if(input$gam_previous_lyr_gam_ver == "pm_cbsa") {
          func_layer_raster(input = input,
                            reactive_id = lyr_ras,
                            map_id = "map_site_lyr",
                            z = "smoke_PM2.5_m0p5m", 
                            zlim = c(0, 35),
                            input_group = "gam_previous_lyr_ras_smoke_pm_m0p5m",
                            output_group = "lyr_ras_smoke_pm_m0p5m",
                            input_data = lyr_dat(),
                            gam_previous_lyr_date = input$gam_previous_lyr_date,
                            gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                            legend_title = "Smoke PM2.5 m0p5m (ug m^-3)",
                            legend_color = "BrBG",
                            legend_position = "bottomright")
        }
      }

    }
  }, ignoreInit = T)
  
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_pts_smoke_pm_m1p0m,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {       
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") { 
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_pts_smoke_pm_m1p0m") %>%
        leaflet::removeControl("lyr_pts_smoke_pm_m1p0m_legend")
      
      if(!is.null(lyr_dat())) {
        if(input$gam_previous_lyr_gam_ver == "pm_cbsa") {
          func_layer_points(input = input,
                            reactive_id = lyr_pts,
                            map_id = "map_site_lyr",
                            z = "smoke_PM2.5_m1p0m_category",
                            input_group = "gam_previous_lyr_pts_smoke_pm_m1p0m",
                            output_group = "lyr_pts_smoke_pm_m1p0m",
                            input_data = lyr_dat(),
                            gam_previous_lyr_date = input$gam_previous_lyr_date,
                            gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                            legend_title = "Smoke PM2.5 m1p0m (ug m^-3)",
                            legend_color = "categorical_epa",
                            legend_category_name = category_name_pm,
                            legend_position = "bottomleft")
        }
      }

    }
  }, ignoreInit = T)
  
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_ras_smoke_pm_m1p0m,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {    
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_ras_smoke_pm_m1p0m") %>%
        leaflet::removeControl("lyr_ras_smoke_pm_m1p0m_legend")
      
      if(!is.null(lyr_dat())) {
        if(input$gam_previous_lyr_gam_ver == "pm_cbsa") {
          func_layer_raster(input = input,
                            reactive_id = lyr_ras,
                            map_id = "map_site_lyr",
                            z = "smoke_PM2.5_m1p0m", 
                            zlim = c(0, 35),
                            input_group = "gam_previous_lyr_ras_smoke_pm_m1p0m",
                            output_group = "lyr_ras_smoke_pm_m1p0m",
                            input_data = lyr_dat(),
                            gam_previous_lyr_date = input$gam_previous_lyr_date,
                            gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                            legend_title = "Smoke PM2.5 m1p0m (ug m^-3)",
                            legend_color = "BrBG",
                            legend_position = "bottomright")
        }
      }

    }
  }, ignoreInit = T)
  
  
  ##### PM2.5-crit mapping (for O3) ----
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_pts_pm_crit,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {        
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_pts_pm_crit") %>%
        leaflet::removeControl("lyr_pts_pm_crit_legend")
      
      if(!is.null(lyr_dat())) {
        if(input$gam_previous_lyr_gam_ver != "pm_cbsa") {
          func_layer_points(input = input,
                            reactive_id = lyr_pts,
                            map_id = "map_site_lyr",
                            z = "PM2.5_Crit_category",
                            input_group = "gam_previous_lyr_pts_pm_crit",
                            output_group = "lyr_pts_pm_crit",
                            input_data = lyr_dat(),
                            gam_previous_lyr_date = input$gam_previous_lyr_date,
                            gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                            legend_title = "PM2.5-crit (ug m^-3)",
                            legend_color = "categorical_epa",
                            legend_category_name = category_name_pmcrit,
                            legend_position = "bottomleft")
        }
      }

    }
  }, ignoreInit = T)
  
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_ras_pm_crit,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {    
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {    
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_ras_pm_crit") %>%
        leaflet::removeControl("lyr_ras_pm_crit_legend")
      
      if(!is.null(lyr_dat())) {
        if(input$gam_previous_lyr_gam_ver != "pm_cbsa") {
          func_layer_raster(input = input,
                            reactive_id = lyr_ras,
                            map_id = "map_site_lyr",
                            z = "PM2.5_Crit", 
                            zlim = c(5, 15),
                            input_group = "gam_previous_lyr_ras_pm_crit",
                            output_group = "lyr_ras_pm_crit",
                            input_data = lyr_dat(),
                            gam_previous_lyr_date = input$gam_previous_lyr_date,
                            gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                            legend_title = "PM2.5-crit (ug m^-3)",
                            legend_color = "BrBG",
                            legend_position = "bottomright")
        }
      }

    }
  }, ignoreInit = T)
  
  
  ##### PM2.5-crit mapping (for PM CBSA) ----
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_pts_pm_crit_m0p5m,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {        
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {      
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_pts_pm_crit_m0p5m") %>%
        leaflet::removeControl("lyr_pts_pm_crit_m0p5m_legend")
      
      if(!is.null(lyr_dat())) {
        if(input$gam_previous_lyr_gam_ver == "pm_cbsa") {
          func_layer_points(input = input,
                            reactive_id = lyr_pts,
                            map_id = "map_site_lyr",
                            z = "PM2.5_Crit_m0p5m_category",
                            input_group = "gam_previous_lyr_pts_pm_crit_m0p5m",
                            output_group = "lyr_pts_pm_crit_m0p5m",
                            input_data = lyr_dat(),
                            gam_previous_lyr_date = input$gam_previous_lyr_date,
                            gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                            legend_title = "PM2.5-crit (ug m^-3)",
                            legend_color = "categorical_epa",
                            legend_category_name = category_name_pmcrit,
                            legend_position = "bottomleft")
        }
      }

    }
  }, ignoreInit = T)
  
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_ras_pm_crit_m0p5m,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {    
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_ras_pm_crit_m0p5m") %>%
        leaflet::removeControl("lyr_ras_pm_crit_m0p5m_legend")
      
      if(!is.null(lyr_dat())) {
        if(input$gam_previous_lyr_gam_ver == "pm_cbsa") {
          func_layer_raster(input = input,
                            reactive_id = lyr_ras,
                            map_id = "map_site_lyr",
                            z = "PM2.5_Crit_m0p5m", 
                            zlim = c(5, 15),
                            input_group = "gam_previous_lyr_ras_pm_crit_m0p5m",
                            output_group = "lyr_ras_pm_crit_m0p5m",
                            input_data = lyr_dat(),
                            gam_previous_lyr_date = input$gam_previous_lyr_date,
                            gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                            legend_title = "PM2.5-crit (ug m^-3)",
                            legend_color = "BrBG",
                            legend_position = "bottomright")
        }
      }

    }
  }, ignoreInit = T)
  
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_pts_pm_crit_m1p0m,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {        
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {     
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_pts_pm_crit_m1p0m") %>%
        leaflet::removeControl("lyr_pts_pm_crit_m1p0m_legend")
      
      if(!is.null(lyr_dat())) {
        if(input$gam_previous_lyr_gam_ver == "pm_cbsa") {
          func_layer_points(input = input,
                            reactive_id = lyr_pts,
                            map_id = "map_site_lyr",
                            z = "PM2.5_Crit_m1p0m_category",
                            input_group = "gam_previous_lyr_pts_pm_crit_m1p0m",
                            output_group = "lyr_pts_pm_crit_m1p0m",
                            input_data = lyr_dat(),
                            gam_previous_lyr_date = input$gam_previous_lyr_date,
                            gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                            legend_title = "PM2.5-crit (ug m^-3)",
                            legend_color = "categorical_epa",
                            legend_category_name = category_name_pmcrit,
                            legend_position = "bottomleft")
        }
      }

    }
  }, ignoreInit = T)
  
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_ras_pm_crit_m1p0m,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {    
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_ras_pm_crit_m1p0m") %>%
        leaflet::removeControl("lyr_ras_pm_crit_m1p0m_legend")
      
      if(!is.null(lyr_dat())) {
        if(input$gam_previous_lyr_gam_ver == "pm_cbsa") {
          func_layer_raster(input = input,
                            reactive_id = lyr_ras,
                            map_id = "map_site_lyr",
                            z = "PM2.5_Crit_m1p0m", 
                            zlim = c(5, 15),
                            input_group = "gam_previous_lyr_ras_pm_crit_m1p0m",
                            output_group = "lyr_ras_pm_crit_m1p0m",
                            input_data = lyr_dat(),
                            gam_previous_lyr_date = input$gam_previous_lyr_date,
                            gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                            legend_title = "PM2.5-crit (ug m^-3)",
                            legend_color = "BrBG",
                            legend_position = "bottomright")
        }
      }

    }
  }, ignoreInit = T)
  
  
  ##### R2 mapping ----
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_pts_r2,
         input$gam_previous_lyr_gam_ver)
  }, {        
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_pts_r2") %>%
        leaflet::removeControl("lyr_pts_r2_legend")
      
      if(input$gam_previous_lyr_gam_ver != "pm_cbsa") {
        if(input$gam_previous_lyr_gam_ver == "gam_v1") {
          r2_dat <- data_stat_gam_v1
        }
        
        if(input$gam_previous_lyr_gam_ver == "gam_v3") {
          r2_dat <- data_stat_gam_v3
        }
        
        if(input$gam_previous_lyr_gam_ver == "gam_v3_edm") {
          r2_dat <- data_stat_gam_v3_edm
        }
        
        ## Hide/Show dataset (2025-02-27)
        # if(input$gam_previous_lyr_gam_ver == "gam_v2") {
        #   r2_dat <- data_stat_gam_v2
        # }
        # 
        # if(input$gam_previous_lyr_gam_ver == "gam_v2_edm") {
        #   r2_dat <- data_stat_gam_v2_edm
        # }
        
        if(input$gam_previous_lyr_gam_ver == "epa_ember") {
          r2_dat <- data_stat_epa_ember
        }
        
        tryCatch({
          r2_dat[, rsq_category := cut(
            adj_rsq,
            breaks = category_val_r2,
            labels = category_name_r2,
            right = F
          )]
        }, error = function(e) {
          r2_dat[, rsq_category := NA]
        })
        
        func_layer_points(input = input,
                          reactive_id = lyr_pts,
                          map_id = "map_site_lyr",
                          z = "rsq_category",
                          input_group = "gam_previous_lyr_pts_r2",
                          output_group = "lyr_pts_r2",
                          input_data = r2_dat,
                          gam_previous_lyr_date = "2000-01-01",
                          gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                          legend_title = "R2",
                          legend_color = "categorical_epa",
                          legend_category_name = category_name_r2,
                          legend_position = "bottomleft")
      }
    }
  }, ignoreInit = T)
  
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_ras_r2,
         input$gam_previous_lyr_gam_ver)
  }, {        
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_ras_r2") %>%
        leaflet::removeControl("lyr_ras_r2_legend")
      
      if(input$gam_previous_lyr_gam_ver != "pm_cbsa") {
        if(input$gam_previous_lyr_gam_ver == "gam_v1") {
          r2_dat <- data_stat_gam_v1
        }
        
        if(input$gam_previous_lyr_gam_ver == "gam_v3") {
          r2_dat <- data_stat_gam_v3
        }
        
        if(input$gam_previous_lyr_gam_ver == "gam_v3_edm") {
          r2_dat <- data_stat_gam_v3_edm
        }
        
        ## Hide/Show dataset (2025-02-27)
        # if(input$gam_previous_lyr_gam_ver == "gam_v2") {
        #   r2_dat <- data_stat_gam_v2
        # }
        # 
        # if(input$gam_previous_lyr_gam_ver == "gam_v2_edm") {
        #   r2_dat <- data_stat_gam_v2_edm
        # }
        
        if(input$gam_previous_lyr_gam_ver == "epa_ember") {
          r2_dat <- data_stat_epa_ember
        }
        
        func_layer_raster(input = input,
                          reactive_id = lyr_ras,
                          map_id = "map_site_lyr",
                          z = "adj_rsq",
                          zlim = c(0, 1),
                          input_group = "gam_previous_lyr_ras_r2",
                          output_group = "lyr_ras_r2",
                          input_data = r2_dat,
                          gam_previous_lyr_date = "2000-01-01",
                          gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                          legend_title = "R2",
                          legend_color = "jet",
                          legend_position = "bottomright")
      }
    }
  }, ignoreInit = T)
  
  ##### Term-rank mapping ----
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_pts_rank,
         input$gam_previous_lyr_gam_ver)
  }, {        
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("lyr_pts_rank") %>%
        leaflet::removeControl("lyr_pts_rank_legend")
      
      if(!input$gam_previous_lyr_gam_ver %in% c("epa_ember", "pm_cbsa")) {
        if(input$gam_previous_lyr_gam_ver == "gam_v1") {
          rank_dat <- data_term_gam_v1
        }
        
        if(input$gam_previous_lyr_gam_ver == "gam_v3") {
          rank_dat <- data_term_gam_v3
        }
        
        if(input$gam_previous_lyr_gam_ver == "gam_v3_edm") {
          rank_dat <- data_term_gam_v3_edm
        }
        
        ## Hide/Show dataset (2025-02-27)
        # if(input$gam_previous_lyr_gam_ver == "gam_v2") {
        #   rank_dat <- data_term_gam_v2
        # }
        # 
        # if(input$gam_previous_lyr_gam_ver == "gam_v2_edm") {
        #   rank_dat <- data_term_gam_v2_edm
        # }
        
        func_layer_points(input = input,
                          reactive_id = lyr_pts,
                          map_id = "map_site_lyr",
                          z = "term",
                          zlim = NULL,
                          input_group = "gam_previous_lyr_pts_rank",
                          output_group = "lyr_pts_rank",
                          input_data = rank_dat[rank == 1],
                          gam_previous_lyr_date = "2000-01-01",
                          gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver, 
                          legend_title = "1st rank term",
                          legend_color = "categorical_term",
                          legend_position = "bottomleft")
      }
    }
  }, ignoreInit = T)
  
  
  ##### HMS mapping ----
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_hms,
         input$gam_previous_lyr_date)
  }, {    
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("hms_layer")
      
      tryCatch({
        cat("observeEvent / GAM previous / Layer map / hms_map...\n")
        
        if(input$gam_previous_lyr_hms == F) {
          leaflet::leafletProxy("map_site_lyr") %>%
            leaflet::clearGroup("hms_layer")
        } else {
          
          progress <- shiny::Progress$new()
          progress$set(message = "Processing HMS data...", value = 0)
          on.exit(progress$close())
          
          smo_date <- as.character(input$gam_previous_lyr_date)
          
          hms_year <- lubridate::year(smo_date)
          hms_file <- paste0("Processed_hms_list_", hms_year, ".rds")
          
          if(hms_file %in% processed_data_list) {
            hms_file <- readRDS(file.path("src/data", hms_file))
            hms_date <- names(hms_file)
            if(smo_date %in% hms_date) {
              lyr_hms[[smo_date]] <- hms_file[[smo_date]]
            }
            rm(hms_year, hms_file, hms_date)
          }
          
          if(!smo_date %in% names(lyr_hms)) {
            lyr_hms[[smo_date]] <- func_hms_smoke(date = smo_date,
                                                  base_path = main_path)
          }
          
          if(!is.null(lyr_hms[[smo_date]]$shp_smoke_light)) {
            leaflet::leafletProxy("map_site_lyr") %>%
              leaflet::addPolygons(data = lyr_hms[[smo_date]]$shp_smoke_light,
                                   color = "black",
                                   weight = 0.5,
                                   opacity = 1,
                                   fillColor = "grey90",
                                   fillOpacity = 0.25,
                                   group = "hms_layer")
          }
          
          if(!is.null(lyr_hms[[smo_date]]$shp_smoke_medium)) {
            leaflet::leafletProxy("map_site_lyr") %>%
              leaflet::addPolygons(data = lyr_hms[[smo_date]]$shp_smoke_medium,
                                   color = "black",
                                   weight = 0.5,
                                   opacity = 1,
                                   fillColor = "grey50",
                                   fillOpacity = 0.25,
                                   group = "hms_layer")
          }
          
          if(!is.null(lyr_hms[[smo_date]]$shp_smoke_heavy)) {
            leaflet::leafletProxy("map_site_lyr") %>%
              leaflet::addPolygons(data = lyr_hms[[smo_date]]$shp_smoke_heavy,
                                   color = "black",
                                   weight = 0.5,
                                   opacity = 1,
                                   fillColor = "black",
                                   fillOpacity = 0.25,
                                   group = "hms_layer")
          }
          
        }
      }, error = function(e) {
        cat("Error - observe / GAM previous / Layer map / hms_map: ", e$message, "\n")
        NULL
      })
    }
  }, ignoreInit = T)
  
  ##### Fire mapping ----
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_fire,
         input$gam_previous_lyr_date)
  }, {    
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {
      
      leaflet::leafletProxy("map_site_lyr") %>%
        leaflet::clearGroup("fire_layer")
      
      tryCatch({
        cat("observeEvent / GAM previous / Layer map / fire_map...\n")
        
        if(input$gam_previous_lyr_fire == F) {
          leaflet::leafletProxy("map_site_lyr") %>%
            leaflet::clearGroup("fire_layer")
        } else {
          
          progress <- shiny::Progress$new()
          progress$set(message = "Processing Fire data...", value = 0)
          on.exit(progress$close())
          
          smo_date <- as.character(input$gam_previous_lyr_date)
          
          if(!smo_date %in% names(lyr_fire)) {
            lyr_fire[[smo_date]] <- func_hms_fire(date = smo_date,
                                                  base_path = main_path)
          }
          
          if(!is.null(lyr_fire[[smo_date]]$shp_fire)) {
            leaflet::leafletProxy("map_site_lyr") %>%
              leaflet::addCircleMarkers(data = lyr_fire[[smo_date]]$shp_fire,
                                        lng = ~lon,
                                        lat = ~lat,
                                        radius = 1,
                                        color = "red",
                                        stroke = F,
                                        opacity = 1,
                                        weight = 1,
                                        fillOpacity = 1,
                                        group = "fire_layer")
          }
        }
        
      }, error = function(e) {
        cat("Error - observe / GAM previous / Layer map / fire_map: ", e$message, "\n")
        NULL
      })
    }
  }, ignoreInit = T)
  
  
  ##### NS/SD mark mapping (for O3) ----
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_pts_smoke_mark,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {    
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {
      
      if(!is.null(lyr_dat())) {
        if(input$gam_previous_lyr_gam_ver %in% c("epa_ember", "pm_cbsa")) {
          leaflet::leafletProxy("map_site_lyr") %>%
            leaflet::clearGroup("lyr_pts_smoke_mark") %>%
            leaflet::removeControl("lyr_pts_smoke_mark_legend")
        } else {
          func_layer_points_mark(input = input,
                                 map_id = "map_site_lyr",
                                 input_group = "gam_previous_lyr_pts_smoke_mark",
                                 output_group = "lyr_pts_smoke_mark",
                                 input_data = lyr_dat()[smoke == 1],
                                 gam_previous_lyr_date = input$gam_previous_lyr_date,
                                 gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver,
                                 border_color = "red",
                                 border_opacity = 1,
                                 border_lwd = 2,
                                 fill_color = "white",
                                 fill_opacity = 1,
                                 method = "point",
                                 legend_title = "Smoke day", 
                                 legend_position = "bottomleft")
        }
      }

    }
  }, ignoreInit = T)
  
  ##### NS/SD mark mapping (for PM CBSA) ----
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_pts_smoke_mark_m0p5m,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {    
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {
      
      if(!is.null(lyr_dat())) {
        if(input$gam_previous_lyr_gam_ver != "pm_cbsa") {
          leaflet::leafletProxy("map_site_lyr") %>%
            leaflet::clearGroup("lyr_pts_smoke_mark_m0p5m") %>%
            leaflet::removeControl("lyr_pts_smoke_mark_m0p5m_legend")
        } else {
          func_layer_points_mark(input = input,
                                 map_id = "map_site_lyr",
                                 input_group = "gam_previous_lyr_pts_smoke_mark_m0p5m",
                                 output_group = "lyr_pts_smoke_mark_m0p5m",
                                 input_data = lyr_dat()[smoke_m0p5m == 1],
                                 gam_previous_lyr_date = input$gam_previous_lyr_date,
                                 gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver,
                                 border_color = "deepskyblue",
                                 border_opacity = 1,
                                 border_lwd = 2,
                                 fill_color = "white",
                                 fill_opacity = 1,
                                 method = "point",
                                 legend_title = "Smoke day m0p5m", 
                                 legend_position = "bottomleft")
        }
      }

    }
  }, ignoreInit = T)
  
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_pts_smoke_mark_m1p0m,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {    
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {
      
      if(!is.null(lyr_dat())) {
        if(input$gam_previous_lyr_gam_ver != "pm_cbsa") {
          leaflet::leafletProxy("map_site_lyr") %>%
            leaflet::clearGroup("lyr_pts_smoke_mark_m1p0m") %>%
            leaflet::removeControl("lyr_pts_smoke_mark_m1p0m_legend")
        } else {
          func_layer_points_mark(input = input,
                                 map_id = "map_site_lyr",
                                 input_group = "gam_previous_lyr_pts_smoke_mark_m1p0m",
                                 output_group = "lyr_pts_smoke_mark_m1p0m",
                                 input_data = lyr_dat()[smoke_m1p0m == 1],
                                 gam_previous_lyr_date = input$gam_previous_lyr_date,
                                 gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver,
                                 border_color = "red",
                                 border_opacity = 1,
                                 border_lwd = 2,
                                 fill_color = "white",
                                 fill_opacity = 1,
                                 method = "point",
                                 legend_title = "Smoke day m1p0m", 
                                 legend_position = "bottomleft")
        }
      }

    }
  }, ignoreInit = T)
  
  ##### SMO mark mapping ----
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_pts_smo_pM1p0MAD,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {    
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {
      
      if(!is.null(lyr_dat())) {
        if(input$gam_previous_lyr_gam_ver %in% c("epa_ember", "pm_cbsa")) {
          leaflet::leafletProxy("map_site_lyr") %>%
            leaflet::clearGroup("lyr_pts_smo_pM1p0MAD") %>%
            leaflet::removeControl("lyr_pts_smo_pM1p0MAD_legend")
        } else {
          lyr_dat_tmp <- lyr_dat()[smoke == 1]
          lyr_dat_tmp <- lyr_dat_tmp[SMO > pM1p0MAD]
          
          func_layer_points_mark(input = input,
                                 map_id = "map_site_lyr",
                                 input_group = "gam_previous_lyr_pts_smo_pM1p0MAD",
                                 output_group = "lyr_pts_smo_pM1p0MAD",
                                 input_data = lyr_dat_tmp,
                                 gam_previous_lyr_date = input$gam_previous_lyr_date,
                                 gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver,
                                 border_color = "deepskyblue",
                                 border_opacity = 1,
                                 border_lwd = 3,
                                 fill_color = "white",
                                 fill_opacity = 1,
                                 method = "point",
                                 legend_title = "SMO > 83th", 
                                 legend_position = "bottomleft")
        }
      }

    }
  }, ignoreInit = T)
  
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_pts_smo_pM2p0MAD,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {    
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {
      
      if(!is.null(lyr_dat())) {
        if(input$gam_previous_lyr_gam_ver %in% c("epa_ember", "pm_cbsa")) {
          leaflet::leafletProxy("map_site_lyr") %>%
            leaflet::clearGroup("lyr_pts_smo_pM2p0MAD") %>%
            leaflet::removeControl("lyr_pts_smo_pM2p0MAD_legend")
        } else {
          lyr_dat_tmp <- lyr_dat()[smoke == 1]
          lyr_dat_tmp <- lyr_dat_tmp[SMO > pM2p0MAD]
          
          func_layer_points_mark(input = input,
                                 map_id = "map_site_lyr",
                                 input_group = "gam_previous_lyr_pts_smo_pM2p0MAD",
                                 output_group = "lyr_pts_smo_pM2p0MAD",
                                 input_data = lyr_dat_tmp,
                                 gam_previous_lyr_date = input$gam_previous_lyr_date,
                                 gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver,
                                 border_color = "forestgreen",
                                 border_opacity = 1,
                                 border_lwd = 3,
                                 fill_color = "white",
                                 fill_opacity = 1,
                                 method = "point",
                                 legend_title = "SMO > 96th", 
                                 legend_position = "bottomleft")
        }
      }

    }
  }, ignoreInit = T)
  
  observeEvent({
    list(input$gam_previous_layer_map_tabs,
         input$gam_previous_lyr_pts_smo_p975,
         input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {    
    if(input$gam_previous_layer_map_tabs == "gam_previous_layer_map_main") {
      
      if(!is.null(lyr_dat())) {
        if(input$gam_previous_lyr_gam_ver %in% c("epa_ember", "pm_cbsa")) {
          leaflet::leafletProxy("map_site_lyr") %>%
            leaflet::clearGroup("lyr_pts_smo_p975") %>%
            leaflet::removeControl("lyr_pts_smo_p975_legend")
        } else {
          lyr_dat_tmp <- lyr_dat()[smoke == 1]
          lyr_dat_tmp <- lyr_dat_tmp[SMO > p975]
          
          func_layer_points_mark(input = input,
                                 map_id = "map_site_lyr",
                                 input_group = "gam_previous_lyr_pts_smo_p975",
                                 output_group = "lyr_pts_smo_p975",
                                 input_data = lyr_dat_tmp,
                                 gam_previous_lyr_date = input$gam_previous_lyr_date,
                                 gam_previous_lyr_gam_ver = input$gam_previous_lyr_gam_ver,
                                 border_color = "magenta",
                                 border_opacity = 1,
                                 border_lwd = 3,
                                 fill_color = "white",
                                 fill_opacity = 1,
                                 method = "point",
                                 legend_title = "SMO > 97.5th", 
                                 legend_position = "bottomleft")
        }
      }

    }
  }, ignoreInit = T)
  
  
  #### Layer map: Plotting ----
  observeEvent({
    list(input$gam_previous_lyr_gam_ver,
         input$gam_previous_lyr_date)
  }, {
    
    lyr_tmp <- lyr_dat()
    
    tryCatch({
      
      cat("observeEvent / layer_map / plotting... \n")
      
      if(!is.null(lyr_tmp) & nrow(lyr_tmp) != 0) {
        
        data_source <- unique(lyr_tmp$source)
        data_date <- unique(lyr_tmp$date)
        
        if(data_source == "pm_cbsa") {
          lyr_tmp <- lyr_tmp[!is.na(smoke_m0p5m)]
          lyr_tmp <- lyr_tmp[!is.na(smoke_m1p0m)]
        } else {
          lyr_tmp <- lyr_tmp[!is.na(smoke)]
        }
        
        lyr_pred_df <- lyr_tmp
        
        if(data_source == "pm_cbsa") {
          
          ##### PM2.5 plots ----
          lyr_pred_df[, PM2.5_smk_m0p5m := PM2.5]
          lyr_pred_df[, PM2.5_smk_m0p5m_mark := as.character(PM2.5_smk_m0p5m)]
          lyr_pred_df[smoke_m0p5m == 0, PM2.5_smk_m0p5m := NA]
          lyr_pred_df[smoke_m0p5m == 0, PM2.5_smk_m0p5m_mark := "N"]
          lyr_pred_df[smoke_m0p5m == 1, PM2.5_smk_m0p5m_mark := "Y"]
          
          lyr_pred_df[, PM2.5_smk_m1p0m := PM2.5]
          lyr_pred_df[, PM2.5_smk_m1p0m_mark := as.character(PM2.5_smk_m1p0m)]
          lyr_pred_df[smoke_m1p0m == 0, PM2.5_smk_m1p0m := NA]
          lyr_pred_df[smoke_m1p0m == 0, PM2.5_smk_m1p0m_mark := "N"]
          lyr_pred_df[smoke_m1p0m == 1, PM2.5_smk_m1p0m_mark := "Y"]
          
          lyr_pred_df[, date := as.Date(date)]
          lyr_pred_df <- lyr_pred_df[!is.na(smoke_m0p5m)]
          lyr_pred_df <- lyr_pred_df[!is.na(smoke_m1p0m)]
          
          lyr_pred_df[, exceedance := ifelse(PM2.5 > 9, T, F)]
          lyr_pred_df[, cause_m0p5m := NA_character_]
          lyr_pred_df[, cause_m1p0m := NA_character_]
          lyr_pred_df[exceedance == T & smoke_m0p5m == 0, cause_m0p5m := "not_caused_by_smoke"]
          lyr_pred_df[exceedance == T & smoke_m0p5m == 1, cause_m0p5m := "due_to_smoke"]
          lyr_pred_df[exceedance == T & smoke_m1p0m == 0, cause_m1p0m := "not_caused_by_smoke"]
          lyr_pred_df[exceedance == T & smoke_m1p0m == 1, cause_m1p0m := "due_to_smoke"]
          
          lyr_pred_df[, PM2.5 := round(PM2.5, 1)]
          lyr_pred_df[, PM2.5_smk_m0p5m := round(PM2.5_smk_m0p5m, 2)]
          lyr_pred_df[, PM2.5_smk_m1p0m := round(PM2.5_smk_m1p0m, 2)]
          
          ## Comparison by state
          lyr_state_df <- lyr_pred_df[, 
                                      .(
                                        No_of_sites = .N,
                                        PM2.5 = mean(PM2.5, na.rm = T),
                                        PM2.5_Crit_m0p5m = mean(PM2.5_Crit_m0p5m, na.rm = T),
                                        PM2.5_Crit_m1p0m = mean(PM2.5_Crit_m1p0m, na.rm = T),
                                        No_of_smoke_days_m0p5m = sum(smoke_m0p5m == 1, na.rm = T),
                                        No_of_smoke_days_m1p0m = sum(smoke_m1p0m == 1, na.rm = T)
                                      ), 
                                      by = state]
          lyr_state_df[, PM2.5 := round(PM2.5, 1)]
          lyr_state_df[, PM2.5_Crit_m0p5m := round(PM2.5_Crit_m0p5m, 2)]
          lyr_state_df[, PM2.5_Crit_m1p0m := round(PM2.5_Crit_m1p0m, 2)]
          
          output$lyr_plot_comp_by_state <- renderEcharts4r({
            e <- lyr_state_df %>%
              e_charts(state) %>%
              e_line(serie = PM2.5,
                     name = "Observed PM2.5 (ug m-3)",
                     lineStyle = list(width = 1, color = "green"),
                     symbol = "rect",
                     symbolSize = 8,
                     smooth = F) %>%
              e_line(serie = PM2.5_Crit_m0p5m,
                     name = "PM2.5-crit m0p5m (ug m-3)",
                     lineStyle = list(width = 1, color = "grey"),
                     symbol = "rect",
                     symbolSize = 8,
                     smooth = F) %>% 
              e_line(serie = PM2.5_Crit_m1p0m,
                     name = "PM2.5-crit m1p0m (ug m-3)",
                     lineStyle = list(width = 1, color = "black"),
                     symbol = "rect",
                     symbolSize = 8,
                     smooth = F) %>% 
              e_bar(serie = No_of_sites,
                    name = "No. of sites",
                    color = "orange") %>% 
              e_bar(serie = No_of_smoke_days_m0p5m,
                    name = "No. of smoke days m0p5m",
                    color = "deepskyblue") %>% 
              e_bar(serie = No_of_smoke_days_m1p0m,
                    name = "No. of smoke days m1p0m",
                    color = "red") %>% 
              e_mark_line(data = list(yAxis = 9), 
                          title = "9 ug m-3",
                          lineStyle = list(type = "dashed", color = "grey"),
                          symbol = c("none", "none"),
                          name = "9 ug m-3") %>%
              e_mark_line(data = list(yAxis = 35), 
                          title = "35 ug m-3",
                          lineStyle = list(type = "dashed", color = "grey"),
                          symbol = c("none", "none"),
                          name = "35 ug m-3") %>%
              e_title(text = paste0("Comparison by state ",
                                    "(date: ", unique(data_date), ", ",
                                    "source: ", data_source, ")"),
                      left = "left",
                      textStyle = list(fontSize = 14)) %>%
              e_x_axis(name = "", 
                       nameLocation = "middle",
                       nameGap = 40,
                       nameTextStyle = list(fontSize = 14, color = "black"),
                       axisLabel = list(rotate = 90, 
                                        textStyle = list(fontSize = 12, color = "black"))) %>%
              e_y_axis(name = "No. of sites, ppb, and ug m-3", 
                       nameLocation = "middle", 
                       nameGap = 40,
                       nameTextStyle = list(fontSize = 14, color = "black"),
                       axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
              e_tooltip(trigger = "axis",
                        axisPointer = list(
                          type = "cross"
                        )) %>%
              e_color(c("green",
                        "grey",
                        "black",
                        "orange",
                        "deepskyblue", 
                        "red")) %>% 
              e_legend(show = T, 
                       top = "5%") %>%
              e_datazoom(type = "inside") %>% 
              e_datazoom(type = "slider",
                         start = 0,
                         end = 100) %>%
              e_toolbox(feature = list(saveAsImage = list(name = paste0("plot_lyr_map_",
                                                                        data_source,
                                                                        "_",
                                                                        data_date)),
                                       dataZoom = list(),
                                       restore = list())) %>%
              e_grid(top = "17%",
                     left = "7%",
                     right = "5%",
                     bottom = "32%") %>%
              e_animation(show = F)
            e
          })
          
          ## Exceedance days by state
          lyr_exc_df_m0p5m <- lyr_pred_df[, .N, by = .(state, cause_m0p5m)]
          lyr_exc_df_m1p0m <- lyr_pred_df[, .N, by = .(state, cause_m1p0m)]
          lyr_exc_df_wide_m0p5m <- data.table::dcast(lyr_exc_df_m0p5m, state ~ cause_m0p5m, value.var = "N", fill = 0)
          lyr_exc_df_wide_m1p0m <- data.table::dcast(lyr_exc_df_m1p0m, state ~ cause_m1p0m, value.var = "N", fill = 0)
          
          for(col in c("due_to_smoke", "not_caused_by_smoke")) {
            if(!col %in% colnames(lyr_exc_df_wide_m0p5m)) {
              lyr_exc_df_wide_m0p5m[, (col) := 0]
            }
          }
          
          for(col in c("due_to_smoke", "not_caused_by_smoke")) {
            if(!col %in% colnames(lyr_exc_df_wide_m1p0m)) {
              lyr_exc_df_wide_m1p0m[, (col) := 0]
            }
          }
          
          lyr_exc_df_wide <- merge(lyr_exc_df_wide_m0p5m,
                                   lyr_exc_df_wide_m1p0m,
                                   all = T,
                                   by = "state")
          
          colnames(lyr_exc_df_wide)[colnames(lyr_exc_df_wide) == "due_to_smoke.x"] <- "due_to_smoke_m0p5m"
          colnames(lyr_exc_df_wide)[colnames(lyr_exc_df_wide) == "due_to_smoke.y"] <- "due_to_smoke_m1p0m"
          colnames(lyr_exc_df_wide)[colnames(lyr_exc_df_wide) == "not_caused_by_smoke.x"] <- "not_caused_by_smoke_m0p5m"
          colnames(lyr_exc_df_wide)[colnames(lyr_exc_df_wide) == "not_caused_by_smoke.y"] <- "not_caused_by_smoke_m1p0m"
          
          legend_1 <- "Days with smoke PM2.5=0 (m0p5m)"
          legend_2 <- "Days with smoke PM2.5>0 (m0p5m)"
          legend_3 <- "Days with smoke PM2.5=0 (m1p0m)"
          legend_4 <- "Days with smoke PM2.5>0 (m1p0m)"
          
          output$lyr_plot_exceedance_by_state <- renderEcharts4r({
            if(all(lyr_exc_df_wide$`due_to_smoke_m0p5m` == 0) && 
               all(lyr_exc_df_wide$`due_to_smoke_m1p0m` == 0) && 
               all(lyr_exc_df_wide$`not_caused_by_smoke_m0p5m` == 0) && 
               all(lyr_exc_df_wide$`not_caused_by_smoke_m1p0m` == 0)) {
              e <- e_charts() %>%
                e_title("No exceedance days can be found in this site", 
                        left = "center",
                        top = "middle") %>%
                e_text_style(color = "red", 
                             fontSize = 15)
            } else {
              e <- lyr_exc_df_wide %>%
                e_charts(state) %>%
                e_bar(`not_caused_by_smoke_m0p5m`, 
                      stack = "smoke_m0p5m",
                      name = legend_1, 
                      color = "grey") %>%
                e_bar(`due_to_smoke_m0p5m`, 
                      stack = "smoke_m0p5m", 
                      name = legend_2, 
                      color = "deepskyblue") %>%
                e_bar(`not_caused_by_smoke_m1p0m`, 
                      stack = "smoke_m1p0m",
                      name = legend_3, 
                      color = "black") %>%
                e_bar(`due_to_smoke_m1p0m`, 
                      stack = "smoke_m1p0m", 
                      name = legend_4, 
                      color = "red") %>%
                e_title(paste0("Days with > 9 ug m-3 by state ", 
                               "(date: ", unique(data_date), ", ",
                               "source: ", data_source, ")"), 
                        left = "left",
                        textStyle = list(fontSize = 14)) %>%
                e_x_axis(name = "", 
                         nameLocation = "middle", 
                         nameGap = 40,
                         nameTextStyle = list(fontSize = 14, color = "black"),
                         axisLabel = list(rotate = 90, 
                                          textStyle = list(fontSize = 12, color = "black"))) %>%
                e_y_axis(name = "No. of days", 
                         nameLocation = "middle", 
                         nameGap = 40,
                         nameTextStyle = list(fontSize = 14, color = "black"),
                         axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
                e_tooltip(trigger = "axis",
                          axisPointer = list(
                            type = "cross"
                          )) %>%
                e_legend(show = T, 
                         width = "60%",
                         top = "5%") %>%
                e_datazoom(type = "inside") %>% 
                e_datazoom(type = "slider",
                           start = 0,
                           end = 100) %>%
                e_toolbox(feature = list(saveAsImage = list(name = paste0("plot_exceedance_day_",
                                                                          unique(data_source), 
                                                                          "_date_",
                                                                          unique(data_date))), 
                                         dataZoom = list(),
                                         restore = list())) %>%
                e_grid(top = "17%",
                       left = "7%",
                       right = "5%",
                       bottom = "32%") %>%
                e_animation(show = F)
            } 
            e
          })
          
          ## Comparison by AQS
          output$lyr_plot_comp_by_aqs <- renderEcharts4r({
            e <- lyr_pred_df %>%
              e_charts(AQS_PM) %>%
              e_line(serie = PM2.5, 
                     name = "Observed PM2.5", 
                     lineStyle = list(width = 1, color = "green"), 
                     symbol = "rect",
                     smooth = F) %>%
              e_line(serie = PM2.5_Crit_m0p5m,
                     name = "PM2.5-crit m0p5m (ug m-3)",
                     lineStyle = list(width = 1, color = "grey"),
                     symbol = "rect",
                     symbolSize = 8,
                     smooth = F) %>% 
              e_line(serie = PM2.5_Crit_m1p0m,
                     name = "PM2.5-crit m1p0m (ug m-3)",
                     lineStyle = list(width = 1, color = "black"),
                     symbol = "rect",
                     symbolSize = 8,
                     smooth = F) %>% 
              e_scatter(serie = PM2.5_smk_m0p5m_mark,
                        symbolSize = 13,
                        legend = F,
                        name = "Smoke day m0p5m") %>%
              e_scatter(serie = PM2.5_smk_m0p5m,
                        symbolSize = 13,
                        legend = T,
                        name = "Smoke day m0p5m (Y/N)") %>%
              e_scatter(serie = PM2.5_smk_m1p0m_mark,
                        symbolSize = 10,
                        legend = F,
                        name = "Smoke day m1p0m") %>%
              e_scatter(serie = PM2.5_smk_m1p0m,
                        symbolSize = 10,
                        legend = T,
                        name = "Smoke day m1p0m (Y/N)") %>% 
              e_mark_line(data = list(yAxis = 9), 
                          title = "9 ug m-3",
                          lineStyle = list(type = "dashed", color = "grey"),
                          symbol = c("none", "none"),
                          name = "9 ug m-3") %>%
              e_mark_line(data = list(yAxis = 35), 
                          title = "35 ug m-3",
                          lineStyle = list(type = "dashed", color = "grey"),
                          symbol = c("none", "none"),
                          name = "35 ug m-3") %>%
              e_title(text = paste0("Comparison by AQS PM ",
                                    "(date: ", unique(data_date), ", ",
                                    "source: ", data_source, ")"),
                      left = "left",
                      textStyle = list(fontSize = 14)) %>%
              e_x_axis(name = "AQS code", 
                       nameLocation = "middle", 
                       nameGap = 80,
                       nameTextStyle = list(fontSize = 14, color = "black"),
                       axisLabel = list(rotate = 90, 
                                        textStyle = list(fontSize = 12, color = "black"))) %>%
              e_y_axis(name = "ppb and ug m-3", 
                       nameLocation = "middle",
                       nameGap = 40,
                       nameTextStyle = list(fontSize = 14, color = "black"),
                       axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
              e_tooltip(trigger = "axis",
                        formatter = htmlwidgets::JS("
                            function(params) {
                            let result = 'AQS: ' + params[0].value[0] + '<br/>';
                            params.forEach(function(item) {
                            if (item.seriesName !== 'Smoke day m0p5m (Y/N)' && item.seriesName !== 'Smoke day m1p0m (Y/N)') {
                            result += 
                            item.marker +
                            ' ' + 
                            item.seriesName +
                            ': ' + 
                            item.value[1] + '<br/>';
                            }}); return result;}"),
                        axisPointer = list(
                          type = "cross"
                        )) %>% 
              e_legend(show = T, 
                       top = "5%") %>%
              e_color(c("green",
                        "grey",
                        "black",
                        "deepskyblue",
                        "deepskyblue",
                        "red",
                        "red")) %>% 
              e_datazoom(type = "inside") %>% 
              e_datazoom(type = "slider",
                         start = 0,
                         end = 100) %>%
              e_toolbox(feature = list(saveAsImage = list(name = paste0("plot_lyr_map_",
                                                                        data_source,
                                                                        "_",
                                                                        data_date)),
                                       dataZoom = list(),
                                       restore = list())) %>%
              e_grid(top = "17%",
                     left = "7%",
                     right = "5%",
                     bottom = "32%") %>%
              e_animation(show = F)
            e
          })
          
          ## Comparison by AQS-scat
          output$lyr_plot_comp_by_aqs_scat <- 
            renderEcharts4r({
              e_charts() %>%
                e_title("This plot is not supported with this data.", left = "center", top = "middle") %>%
                e_text_style(color = "red", fontSize = 15)
            })
          
        } else {
          
          ##### MDA8 plots ----
          lyr_pred_df[, date := as.Date(date)]
          lyr_pred_df[, MDA8O3_pred := round(MDA8O3_pred, 1)]
          lyr_pred_df[, exceedance := ifelse(MDA8O3 > 70, T, F)]
          lyr_pred_df[, MDA8O3_smk := MDA8O3]
          lyr_pred_df[, MDA8O3_smk_mark := as.character(MDA8O3_smk)]
          lyr_pred_df[smoke == 0, MDA8O3_smk := NA]
          lyr_pred_df[smoke == 0, MDA8O3_smk_mark := "N"]
          lyr_pred_df[smoke == 1, MDA8O3_smk_mark := "Y"]
          
          if(data_source == "epa_ember") {
            lyr_pred_df[, cause := NA_character_]
            lyr_pred_df[exceedance == T & smoke == 0, cause := "not_caused_by_smoke"]
            lyr_pred_df[exceedance == T & smoke == 1, cause := "due_to_smoke"]
          } else {
            lyr_pred_df[smoke == 0, SMO := NA]
            lyr_pred_df[, cause := NA_character_]
            lyr_pred_df[exceedance == T, cause := "not_caused_by_smoke"]
            lyr_pred_df[exceedance == T & smoke == 1 & MDA8O3_resids > p975, cause := "due_to_smoke"]
          }
          
          lyr_pred_df$group <- factor(lyr_pred_df$smoke,
                                      levels = c("0", "1"),
                                      labels = c("Non-smoke day", "Smoke day"))
          
          if(data_source == "epa_ember") {
            lyr_pred_df$group <- "Data points"
          }
          
          
          ## Comparison by state
          lyr_state_df <- lyr_pred_df[, 
                                      .(
                                        No_of_sites = .N,
                                        MDA8O3 = mean(MDA8O3, na.rm = T),
                                        MDA8O3_pred = mean(MDA8O3_pred, na.rm = T),
                                        MDA8O3_resids = mean(MDA8O3_resids, na.rm = T),
                                        SMO = mean(SMO[smoke == 1], na.rm = T),
                                        PM2.5 = mean(PM2.5, na.rm = T),
                                        PM2.5_Crit = mean(PM2.5_Crit, na.rm = T),
                                        No_of_smoke_days = sum(smoke == 1, na.rm = T)
                                      ), 
                                      by = state]
          lyr_state_df[, MDA8O3 := round(MDA8O3, 1)]
          lyr_state_df[, MDA8O3_pred := round(MDA8O3_pred, 1)]
          lyr_state_df[, MDA8O3_resids := round(MDA8O3_resids, 1)]
          lyr_state_df[, SMO := round(SMO, 1)]
          lyr_state_df[, PM2.5 := round(PM2.5, 1)]
          lyr_state_df[, PM2.5_Crit := round(PM2.5_Crit, 2)]
          
          if(data_source == "epa_ember") {
            lyr_state_df[No_of_smoke_days == 0, SMO := 0]
          }
          
          if(data_source == "epa_ember") {
            label_bar <- "No. of days with SMO>0"
          } else {
            label_bar <- "No. of smoke days"
          }
          
          output$lyr_plot_comp_by_state <- renderEcharts4r({
            e <- lyr_state_df %>%
              e_charts(state) %>%
              e_line(serie = MDA8O3,
                     name = "Observed MDA8 (ppb)",
                     lineStyle = list(width = 1, color = "green"),
                     symbol = "triangle",
                     symbolSize = 8,
                     smooth = F) %>%
              e_line(serie = MDA8O3_pred,
                     name = "Predicted MDA8 (ppb)",
                     lineStyle = list(width = 1, color = "magenta"),
                     symbol = "triangle",
                     symbolSize = 8,
                     smooth = F) %>%
              e_line(serie = SMO,
                     name = "SMO (ppb)",
                     lineStyle = list(width = 1, color = "blue"),
                     symbolSize = 8,
                     smooth = F) %>%
              e_mark_line(data = list(yAxis = 70), 
                          title = "70 ppb",
                          lineStyle = list(type = "dashed", color = "grey"),
                          symbol = c("none", "none"),
                          name = "70 ppb") %>%
              e_title(text = paste0("Comparison by state ",
                                    "(date: ", unique(data_date), ", ",
                                    "source: ", data_source, ")"),
                      left = "left",
                      textStyle = list(fontSize = 14)) %>%
              e_x_axis(name = "", 
                       nameLocation = "middle",
                       nameGap = 40,
                       nameTextStyle = list(fontSize = 14, color = "black"),
                       axisLabel = list(rotate = 90, 
                                        textStyle = list(fontSize = 12, color = "black"))) %>%
              e_y_axis(name = "No. of sites, ppb, and ug m-3", 
                       nameLocation = "middle", 
                       nameGap = 40,
                       nameTextStyle = list(fontSize = 14, color = "black"),
                       axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
              e_tooltip(trigger = "axis",
                        axisPointer = list(
                          type = "cross"
                        )) %>%
              e_legend(show = T, 
                       top = "5%") %>%
              e_datazoom(type = "inside") %>% 
              e_datazoom(type = "slider",
                         start = 0,
                         end = 100) %>%
              e_toolbox(feature = list(saveAsImage = list(name = paste0("plot_lyr_map_",
                                                                        data_source,
                                                                        "_",
                                                                        data_date)),
                                       dataZoom = list(),
                                       restore = list())) %>%
              e_grid(top = "17%",
                     left = "7%",
                     right = "5%",
                     bottom = "32%") %>%
              e_animation(show = F)
            
            if(data_source == "epa_ember") {
              e <- e %>% 
                e_bar(serie = No_of_sites,
                      name = "No. of sites",
                      color = "orange") %>% 
                e_bar(serie = No_of_smoke_days,
                      name = label_bar,
                      color = "darkred") %>% 
                e_color(c("green",
                          "magenta", 
                          "blue", 
                          "orange", 
                          "darkred"))
            } else {
              e <- e %>% 
                e_data(lyr_state_df) %>%
                e_line(serie = PM2.5,
                       name = "Observed PM2.5 (ug m-3)",
                       lineStyle = list(width = 1, color = "black"),
                       symbol = "rect",
                       symbolSize = 8,
                       smooth = F) %>%
                e_line(serie = PM2.5_Crit,
                       name = "PM2.5-crit (ug m-3)",
                       lineStyle = list(width = 1, color = "grey"),
                       symbol = "rect",
                       symbolSize = 8,
                       smooth = F) %>% 
                e_bar(serie = No_of_sites,
                      name = "No. of sites",
                      color = "orange") %>% 
                e_bar(serie = No_of_smoke_days,
                      name = label_bar,
                      color = "darkred") %>% 
                e_color(c("green",
                          "magenta", 
                          "blue", 
                          "black",
                          "grey",
                          "orange", 
                          "darkred"))
            }
            
            e
          })
          
          ## Exceedance days by state
          lyr_exc_df <- lyr_pred_df[, .N, by = .(state, cause)]
          lyr_exc_df_wide <- dcast(lyr_exc_df, state ~ cause, value.var = "N", fill = 0)
          
          for(col in c("due_to_smoke", "not_caused_by_smoke")) {
            if(!col %in% colnames(lyr_exc_df_wide)) {
              lyr_exc_df_wide[, (col) := 0]
            }
          }
          
          if(data_source == "epa_ember") {
            legend_1 <- "Exceedance days with SMO=0"
            legend_2 <- "Exceedance days with SMO>0"
          } else {
            legend_1 <- "Not caused by smoke"
            legend_2 <- "Significant smoke contribution to MDA8"
          }
          
          output$lyr_plot_exceedance_by_state <- renderEcharts4r({
            if(all(lyr_exc_df_wide$`due_to_smoke` == 0) && 
               all(lyr_exc_df_wide$`not_caused_by_smoke` == 0)) {
              e <- e_charts() %>%
                e_title("No exceedance days can be found in this site", 
                        left = "center",
                        top = "middle") %>%
                e_text_style(color = "red", 
                             fontSize = 15)
            } else {
              e <- lyr_exc_df_wide %>%
                e_charts(state) %>%
                e_bar(`not_caused_by_smoke`, 
                      stack = "smoke", 
                      name = legend_1, 
                      color = "blue") %>%
                e_bar(`due_to_smoke`,
                      stack = "smoke",
                      name = legend_2,
                      color = "red") %>%
                e_title(paste0("Exceedance days (> 70 ppb) by state ", 
                               "(date: ", unique(data_date), ", ",
                               "source: ", data_source, ")"), 
                        left = "left",
                        textStyle = list(fontSize = 14)) %>%
                e_x_axis(name = "", 
                         nameLocation = "middle", 
                         nameGap = 40,
                         nameTextStyle = list(fontSize = 14, color = "black"),
                         axisLabel = list(rotate = 90, 
                                          textStyle = list(fontSize = 12, color = "black"))) %>%
                e_y_axis(name = "No. of exceedance days", 
                         nameLocation = "middle", 
                         nameGap = 40,
                         nameTextStyle = list(fontSize = 14, color = "black"),
                         axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
                e_tooltip(trigger = "axis",
                          axisPointer = list(
                            type = "cross"
                          )) %>%
                e_legend(show = T, 
                         top = "7%") %>%
                e_datazoom(type = "inside") %>% 
                e_datazoom(type = "slider",
                           start = 0,
                           end = 100) %>%
                e_toolbox(feature = list(saveAsImage = list(name = paste0("plot_exceedance_day_",
                                                                          unique(data_source), 
                                                                          "_date_",
                                                                          unique(data_date))), 
                                         dataZoom = list(),
                                         restore = list())) %>%
                e_grid(top = "17%",
                       left = "7%",
                       right = "5%",
                       bottom = "32%") %>%
                e_animation(show = F)
            } 
            e
          })
          
          ## Comparison by AQS
          if(data_source == "epa_ember") {
            label_scat <- "Day with SMO>0"
            label_scat_val <- "Day with SMO>0 (Y/N)"
          } else {
            label_scat <- "Smoke day"
            label_scat_val <- "Smoke day (Y/N)"
          }
          
          output$lyr_plot_comp_by_aqs <- renderEcharts4r({
            e <- lyr_pred_df %>%
              e_charts(AQS_O3) %>%
              e_line(serie = MDA8O3,
                     name = "Observed MDA8 (ppb)",
                     lineStyle = list(width = 1, color = "green"),
                     symbol = "triangle",
                     symbolSize = 8,
                     smooth = F) %>%
              e_line(serie = MDA8O3_pred,
                     name = "Predicted MDA8 (ppb)",
                     lineStyle = list(width = 1, color = "magenta"),
                     symbol = "triangle",
                     symbolSize = 8,
                     smooth = F) %>%
              e_line(serie = SMO,
                     name = "SMO (ppb)",
                     lineStyle = list(width = 1, color = "blue"),
                     symbolSize = 8,
                     smooth = F) %>%
              e_mark_line(data = list(yAxis = 70), 
                          title = "70 ppb",
                          lineStyle = list(type = "dashed", color = "grey"),
                          symbol = c("none", "none"),
                          name = "70 ppb") %>%
              e_title(text = paste0("Comparison by AQS O3 ",
                                    "(date: ", unique(data_date), ", ",
                                    "source: ", data_source, ")"),
                      left = "left",
                      textStyle = list(fontSize = 14)) %>%
              e_x_axis(name = "AQS code", 
                       nameLocation = "middle", 
                       nameGap = 80,
                       nameTextStyle = list(fontSize = 14, color = "black"),
                       axisLabel = list(rotate = 90, 
                                        textStyle = list(fontSize = 12, color = "black"))) %>%
              e_y_axis(name = "ppb and ug m-3", 
                       nameLocation = "middle",
                       nameGap = 40,
                       nameTextStyle = list(fontSize = 14, color = "black"),
                       axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
              e_tooltip(trigger = "axis",
                        formatter = htmlwidgets::JS("
                            function(params) {
                            let result = 'AQS: ' + params[0].value[0] + '<br/>';
                            params.forEach(function(item) {
                            if (item.seriesName !== 'Day with SMO>0 (Y/N)' && item.seriesName !== 'Smoke day (Y/N)') {
                            result += 
                            item.marker +
                            ' ' + 
                            item.seriesName +
                            ': ' + 
                            item.value[1] + '<br/>';
                            }}); return result;}"),
                        axisPointer = list(
                          type = "cross"
                        )) %>% 
              e_legend(show = T, 
                       top = "5%") %>%
              e_datazoom(type = "inside") %>% 
              e_datazoom(type = "slider",
                         start = 0,
                         end = 100) %>%
              e_toolbox(feature = list(saveAsImage = list(name = paste0("plot_lyr_map_",
                                                                        data_source,
                                                                        "_",
                                                                        data_date)),
                                       dataZoom = list(),
                                       restore = list())) %>%
              e_grid(top = "17%",
                     left = "7%",
                     right = "5%",
                     bottom = "32%") %>%
              e_animation(show = F)
            
            if(data_source == "epa_ember") {
              e <- e %>% 
                e_data(lyr_pred_df) %>%
                e_scatter(serie = MDA8O3_smk_mark,
                          name = label_scat,
                          legend = F,
                          symbolSize = 12) %>%
                e_scatter(serie = MDA8O3_smk, 
                          name = label_scat_val,
                          legend = T,
                          symbolSize = 12) %>% 
                e_color(c("green", 
                          "magenta", 
                          "blue",
                          "red",
                          "red"))
            } else {
              e <- e %>% 
                e_data(lyr_pred_df) %>%
                e_line(serie = PM2.5,
                       name = "PM2.5 (ug m-3)",
                       lineStyle = list(width = 1, color = "black"),
                       symbol = "rect",
                       symbolSize = 8,
                       smooth = F) %>%
                e_line(serie = PM2.5_Crit,
                       name = "PM2.5-crit (ug m-3)",
                       lineStyle = list(width = 1, color = "grey"),
                       symbol = "rect",
                       symbolSize = 8,
                       smooth = F) %>% 
                e_scatter(serie = MDA8O3_smk_mark,
                          name = label_scat,
                          legend = F,
                          symbolSize = 12) %>%
                e_scatter(serie = MDA8O3_smk, 
                          name = label_scat_val,
                          legend = T,
                          symbolSize = 12) %>% 
                e_color(c("green", 
                          "magenta", 
                          "blue",
                          "black",
                          "grey",
                          "red",
                          "red"))
            }
            
            e
          })
          
          ## Comparison by AQS-scat
          output$lyr_plot_comp_by_aqs_scat <- renderEcharts4r({
            e <- lyr_pred_df %>%
              dplyr::group_by(group) %>% 
              e_charts(MDA8O3_pred) %>%
              e_scatter(MDA8O3, AQS_O3, symbolSize = 8) %>% 
              e_line(MDA8O3_pred, 
                     name = "1:1 line",
                     symbol = "line") %>% 
              e_mark_line(data = list(yAxis = 70), 
                          title = "70 ppb",
                          lineStyle = list(type = "dashed", color = "grey"),
                          symbol = c("none", "none"),
                          name = "70 ppb") %>%
              e_mark_line(data = list(xAxis = 70), 
                          title = "",
                          lineStyle = list(type = "dashed", color = "grey"),
                          symbol = c("none", "none"),
                          name = "70 ppb") %>%
              e_tooltip(trigger = "item",
                        formatter = JS("function(params) {
                          return params.seriesName + '<br/>' +
                          'AQS: ' + params.value[2] + '<br/>' +
                          'Observed MDA8 (ppb): ' + params.value[1] + '<br/>' +
                          'Predicted MDA8 (ppb): ' + params.value[0];
                                         }"),
                        axisPointer = list(
                          type = "cross"
                        )) %>%
              e_title(text = paste0("Comparison by AQS O3: Obs. vs Pred. ", 
                                    "(date: ", unique(data_date), ", ",
                                    "source: ", data_date, ")"),
                      left = "left",
                      textStyle = list(fontSize = 14)) %>%
              e_x_axis(name = "Predicted MDA8 (ppb)",
                       scale = T,
                       nameLocation = "middle",
                       nameGap = 30,
                       nameTextStyle = list(fontSize = 14, color = "black"),
                       axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
              e_y_axis(name = "Observed MDA8 (ppb)",
                       scale = T, 
                       nameLocation = "middle",
                       nameGap = 40,
                       nameTextStyle = list(fontSize = 14, color = "black"),
                       axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
              e_legend(top = "5%") %>%
              e_color(c("black", "red", "blue")) %>% 
              e_datazoom(type = "inside") %>%
              e_toolbox(feature = list(saveAsImage = list(name = paste0("plot_obs_vs_pred_",
                                                                        unique(data_source), 
                                                                        "_date_",
                                                                        unique(data_date))), 
                                       dataZoom = list(),
                                       restore = list())) %>%
              e_animation(show = F)
            e
          })
          
        }
        
      } else {
        
        output$lyr_plot_comp_by_state <- renderEcharts4r({
          e_charts() %>%
            e_title("No data found for this date.", left = "center", top = "middle") %>%
            e_text_style(color = "red", fontSize = 15)
        })
        output$lyr_plot_exceedance_by_state <- renderEcharts4r({
          e_charts() %>%
            e_title("No data found for this date.", left = "center", top = "middle") %>%
            e_text_style(color = "red", fontSize = 15)
        })
        output$lyr_plot_comp_by_aqs <- renderEcharts4r({
          e_charts() %>%
            e_title("No data found for this date.", left = "center", top = "middle") %>%
            e_text_style(color = "red", fontSize = 15)
        })
        output$lyr_plot_comp_by_aqs_scat <- renderEcharts4r({
          e_charts() %>%
            e_title("No data found for this date.", left = "center", top = "middle") %>%
            e_text_style(color = "red", fontSize = 15)
        })
        
      }
    }, error = function(e) {
      
      cat("Error - observeEvent / layer_map / plotting: ", e$message, "\n")
      
      output$lyr_plot_comp_by_state <- renderEcharts4r({
        e_charts() %>%
          e_title("No data found for this date.", left = "center", top = "middle") %>%
          e_text_style(color = "red", fontSize = 15)
      })
      output$lyr_plot_exceedance_by_state <- renderEcharts4r({
        e_charts() %>%
          e_title("No data found for this date.", left = "center", top = "middle") %>%
          e_text_style(color = "red", fontSize = 15)
      })
      output$lyr_plot_comp_by_aqs <- renderEcharts4r({
        e_charts() %>%
          e_title("No data found for this date.", left = "center", top = "middle") %>%
          e_text_style(color = "red", fontSize = 15)
      })
      output$lyr_plot_comp_by_aqs_scat <- renderEcharts4r({
        e_charts() %>%
          e_title("No data found for this date.", left = "center", top = "middle") %>%
          e_text_style(color = "red", fontSize = 15)
      })
    })
    
  }, ignoreInit = F)
  
  
  #### Layer Map: Export ----
  observeEvent(input$gam_previous_lyr_export_map, {
    tryCatch({
      cat("observe / GAM previous / Layer map / downloading...\n")
      shinyscreenshot::screenshot(id = "map_site_lyr",
                                  filename = paste0("map_site_lyr_", input$gam_previous_lyr_date))
    }, error = function(e) {
      cat("Error - observe / GAM previous / Layer map / downloading: ", e$message, "\n")
      NULL
    })
  }, ignoreInit = T)
  
  output$gam_previous_lyr_export_data <- downloadHandler(
    filename = function() {
      paste0("spatial_data_",
             input$gam_previous_lyr_gam_ver, "_",
             input$gam_previous_lyr_date, ".csv")
    },
    content = function(file) {
      tryCatch({
        cat("observe / GAM previous / Layer map / downloading...\n")
        export_data <- data.table::copy(lyr_dat())
        data.table::fwrite(file = file, 
                           x = export_data,
                           row.names = F)
      }, error = function(e) {
        cat("Error - observe / GAM previous / Layer map / downloading: ", e$message, "\n")
      })
    }
  )
  
  ### Sub-tab: Data comparison ----
  ##### For advanced searching ----
  select_state_and_aqs <- reactive({
    search_inputs <- c(input$gam_previous_comp_dataset_1, 
                       input$gam_previous_comp_dataset_2)
    
    data_list <- lapply(search_inputs, function(source_value) {
      unique(data_gam_stat()[stri_detect_regex(source, source_value, case_insensitive = T), .(state, AQS)])
    })
    
    Reduce(function(x, y) merge(x, y, by = c("state", "AQS")), data_list)
  })
  
  observe({
    updateSelectizeInput(session,
                         inputId = "gam_previous_search_input_comp_state",
                         choices = select_state_and_aqs()$state,
                         selected = NULL,
                         server = F)
  })
  
  observe({
    selected_state <- input$gam_previous_search_input_comp_state
    filtered_aqs <- select_state_and_aqs()[select_state_and_aqs()$state == selected_state, "AQS"]
    updateSelectizeInput(session,
                         inputId = "gam_previous_search_input_comp_aqs",
                         choices = unique(filtered_aqs),
                         selected = NULL,
                         server = F)
  })
  
  observe({
    dataset_selected_1 <- input$gam_previous_comp_dataset_1
    updated_choices_2 <- datacomp_dataset[!datacomp_dataset %in% dataset_selected_1]
    
    updateSelectInput(
      session,
      inputId = "gam_previous_comp_dataset_2",
      choices = updated_choices_2,
      selected = if(input$gam_previous_comp_dataset_2 %in% updated_choices_2) {
        input$gam_previous_comp_dataset_2
      } else {
        updated_choices_2[1]
      }
    )
  })
  
  ##### Plotting ----
  plot_data_generator <- function(dataset_name, state, aqs) {
    plot_vars <- c("source", "date", "state", "AQS_O3", "smoke",
                   "MDA8O3", "MDA8O3_pred", "MDA8O3_resids", "SMO", "PM2.5")

    # dataset_name = "gam_v3"
    # aqs = "010499991"
    
    folder_name <- "data_by_aqs"
    file_name <- paste0("data_by_aqs_", aqs, ".rds")
    
    plot_dat_tmp <- tryCatch({
      readRDS(file.path("src/data", folder_name, file_name))
    }, error = function(e) {
      NULL
    })
    
    plot_dat_tmp <- plot_dat_tmp[[dataset_name]]
    
    if(!is.null(plot_dat_tmp)) {
      plot_dat_tmp <- plot_dat_tmp[state == state]
      plot_dat_tmp <- plot_dat_tmp[AQS_O3 == aqs]
      
      plot_dat_tmp[, MDA8O3_resids := round(MDA8O3_resids, 1)]
      plot_dat_tmp[, MDA8O3_smk := MDA8O3]
      plot_dat_tmp[, MDA8O3_pred := round(MDA8O3_pred, 1)]
      plot_dat_tmp[, PM2.5 := round(PM2.5, 2)]
      
      if(dataset_name == "epa_ember") {
        plot_dat_tmp[, pSMO := SMO]
        plot_dat_tmp[SMO < 0, pSMO := 0]
      } else {
        plot_dat_tmp[smoke == 0, MDA8O3_smk := NA]
        plot_dat_tmp[smoke == 0, SMO := NA]
        plot_dat_tmp[, pSMO := SMO]
        plot_dat_tmp[SMO < 0, pSMO := 0]
      }
    }
    
    plot_dat_tmp
  }
  
  plot_series <- function(e, 
                          plot_data, 
                          input_conditions,
                          line_colors,
                          series_names,
                          symbol = "emptyCircle") {
    
    if(input_conditions$obs) {
      e <- e %>%
        e_data(plot_data) %>%
        e_line(
          serie = MDA8O3,
          name = paste0(series_names$obs, input_conditions$dataset),
          lineStyle = list(width = 1, color = line_colors$obs),
          itemStyle = list(color = line_colors$obs),
          symbol = symbol,
          symbolSize = 8,
          smooth = F
        )
    }
    
    if(input_conditions$pred) {
      e <- e %>%
        e_data(plot_data) %>%
        e_line(
          serie = MDA8O3_pred,
          name = paste0(series_names$pred, input_conditions$dataset),
          lineStyle = list(width = 1, color = line_colors$pred),
          itemStyle = list(color = line_colors$pred),
          symbol = symbol,
          symbolSize = 8,
          smooth = F
        )
    }

    if(input_conditions$smo) {
      e <- e %>%
        e_data(plot_data) %>%
        e_line(
          serie = SMO,
          name = paste0(series_names$smo, input_conditions$dataset),
          lineStyle = list(width = 1, color = line_colors$smo),
          itemStyle = list(color = line_colors$smo),
          symbol = symbol,
          symbolSize = 8,
          smooth = F
        )
    }
    
    if(input_conditions$resids) {
      e <- e %>%
        e_data(plot_data) %>%
        e_bar(
          serie = MDA8O3_resids,
          name = paste0(series_names$resids, input_conditions$dataset),
          itemStyle = list(color = line_colors$resids,
                           borderColor = "black",
                           borderWidth = 1)
        )
    }
    
    if(input_conditions$pm25) {
      e <- e %>%
        e_data(plot_data) %>%
        e_line(
          serie = PM2.5,
          name = paste0(series_names$pm25, input_conditions$dataset),
          lineStyle = list(width = 1, color = line_colors$pm25),
          itemStyle = list(color = line_colors$pm25),
          symbol = symbol,
          symbolSize = 8,
          smooth = F
        )
    }
    
    return(e)
  }
  
  plot_dat_1 <- reactive({
    req(input$gam_previous_comp_dataset_1, 
        input$gam_previous_search_input_comp_aqs)
    plot_data_generator(input$gam_previous_comp_dataset_1, 
                        input$gam_previous_search_input_comp_state,
                        input$gam_previous_search_input_comp_aqs)
  })
  
  plot_dat_2 <- reactive({
    req(input$gam_previous_comp_dataset_2, 
        input$gam_previous_search_input_comp_aqs)
    plot_data_generator(input$gam_previous_comp_dataset_2, 
                        input$gam_previous_search_input_comp_state,
                        input$gam_previous_search_input_comp_aqs)
  })
  
  dat_comp_enable_ids_1 <- c(
    "gam_previous_comp_mda8_resids_1",
    "gam_previous_comp_pm25_1"
  )
  
  dat_comp_enable_ids_2 <- c(
    "gam_previous_comp_mda8_resids_2",
    "gam_previous_comp_pm25_2"
  )
  
  observe({
    if(input$gam_previous_comp_dataset_1 == "epa_ember") {
      lapply(dat_comp_enable_ids_1, function(id) {
        updateCheckboxInput(
          session,
          inputId = id,
          value = F
        )
        shinyjs::hide(id = id)
        shinyjs::runjs(sprintf("$('#%s').closest('.checkbox').find('label').addClass('disabled-label');", id))
      })
    } else {
      lapply(dat_comp_enable_ids_1, function(id) {
        shinyjs::show(id = id)
        shinyjs::runjs(sprintf("$('#%s').closest('.checkbox').find('label').removeClass('disabled-label');", id))
      })
    }
    
    if(input$gam_previous_comp_dataset_2 == "epa_ember") {
      lapply(dat_comp_enable_ids_2, function(id) {
        updateCheckboxInput(
          session,
          inputId = id,
          value = F
        )
        shinyjs::hide(id = id)
        shinyjs::runjs(sprintf("$('#%s').closest('.checkbox').find('label').addClass('disabled-label');", id))
      })
    } else {
      lapply(dat_comp_enable_ids_2, function(id) {
        shinyjs::show(id = id)
        shinyjs::runjs(sprintf("$('#%s').closest('.checkbox').find('label').removeClass('disabled-label');", id))
      })
    }
  })
  
  
  ##### Series plot ----
  observeEvent({
    list(input$gam_previous_tabs,
         input$gam_previous_search_input_comp_aqs,
         input$gam_previous_comp_dataset_1,
         input$gam_previous_comp_dataset_2,
         input$gam_previous_comp_mda8_obs_1,
         input$gam_previous_comp_mda8_obs_2,
         input$gam_previous_comp_mda8_pred_1,
         input$gam_previous_comp_mda8_pred_2,
         input$gam_previous_comp_mda8_resids_1,
         input$gam_previous_comp_mda8_resids_2,
         input$gam_previous_comp_mda8_smo_1,
         input$gam_previous_comp_mda8_smo_2,
         input$gam_previous_comp_pm25_1,
         input$gam_previous_comp_pm25_2)
  }, {
    
    if(input$gam_previous_tabs == "gam_previous_comp") {
      
      tryCatch({
        cat("observe / Data comparison / plotting... \n")
        
        name_aqs <- input$gam_previous_search_input_comp_aqs
        name_state <- input$gam_previous_search_input_comp_state
        data_source_1 <- input$gam_previous_comp_dataset_1
        data_source_2 <- input$gam_previous_comp_dataset_2
        
        output$gam_previous_comp_plot_series <- renderEcharts4r({
          e <- plot_dat_1() %>%
            e_charts(date) %>% 
            e_title(text = paste0("AQS O3: ",
                                  name_aqs, 
                                  ", ", 
                                  name_state),
                    left = "center",
                    textStyle = list(fontSize = 14)) %>%
            e_x_axis(name = "Date",
                     scale = T,
                     nameLocation = "middle",
                     nameGap = 40,
                     nameTextStyle = list(fontSize = 14, color = "black"),
                     axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
            e_y_axis(name = "ppb",
                     scale = T, 
                     nameLocation = "middle",
                     nameGap = 50,
                     nameTextStyle = list(fontSize = 14, color = "black"),
                     axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
            e_datazoom(type = "inside") %>%
            e_datazoom(type = "slider",
                       start = 0,
                       end = 100) %>%
            e_toolbox(feature = list(saveAsImage = list(name = paste0("plot_comp_series_",
                                                                      data_source_1, 
                                                                      "_",
                                                                      data_source_2)),
                                     dataZoom = list(),
                                     restore = list())) %>%
            e_tooltip(trigger = "axis",
                      axisPointer = list(
                        type = "cross"
                      )) %>%
            e_legend(show = T, 
                     top = "7%") %>%
            e_grid(top = "17%",
                   left = "7%",
                   right = "5%",
                   bottom = "27%") %>%
            e_animation(show = F)
          
          input_conditions_1 <- list(
            obs = input$gam_previous_comp_mda8_obs_1,
            pred = input$gam_previous_comp_mda8_pred_1,
            resids = input$gam_previous_comp_mda8_resids_1,
            smo = input$gam_previous_comp_mda8_smo_1,
            pm25 = input$gam_previous_comp_pm25_1,
            dataset = input$gam_previous_comp_dataset_1
          )
          
          input_conditions_2 <- list(
            obs = input$gam_previous_comp_mda8_obs_2,
            pred = input$gam_previous_comp_mda8_pred_2,
            resids = input$gam_previous_comp_mda8_resids_2,
            smo = input$gam_previous_comp_mda8_smo_2,
            pm25 = input$gam_previous_comp_pm25_2,
            dataset = input$gam_previous_comp_dataset_2
          )
          
          line_colors_1 <- list(
            obs = "#1f77b4", 
            pred = "#ff7f0e",
            resids = "#2ca02c",
            smo = "#d62728", 
            # smo_pos = "purple",
            pm25 = "black"
          )
          
          line_colors_2 <- list(
            obs = "#17becf", 
            pred = "#bcbd22", 
            resids = "orange",
            smo = "#e377c2",
            # smo_pos = "#9467bd",
            pm25 = "grey"
          )
          
          series_names_1 <- list(
            obs = "Observed MDA8 (ppb): ",
            pred = "Predicted MDA8 (ppb): ",
            resids = "Residual (ppb): ",
            smo = "SMO (ppb): ",
            # smo_pos = "positive SMO (ppb): ",
            pm25 = "PM2.5 (ug m-3): "
          )
          
          series_names_2 <- series_names_1 
          
          e <- plot_series(e, 
                           plot_dat_1(), 
                           input_conditions_1, 
                           line_colors_1, 
                           series_names_1,
                           symbol = "emptyCircle")
          e <- plot_series(e,
                           plot_dat_2(), 
                           input_conditions_2, 
                           line_colors_2, 
                           series_names_2, 
                           symbol = "emptyRect")
          e <- e %>% 
            e_mark_line(data = list(yAxis = 70), 
                        title = "70 ppb",
                        lineStyle = list(type = "dashed", color = "grey"),
                        symbol = c("none", "none"),
                        name = "70 ppb")
          e
        })
        
      }, error = function(e) {
        cat("Error - observe / Data comparison / plotting: ", e$message, "\n")
        output$lyr_plot_comp_by_state <- renderEcharts4r({
          e_charts() %>%
            e_title("No matching data on this site", left = "center", top = "middle") %>%
            e_text_style(color = "red", fontSize = 15)
        })
        NULL
      })
    } 
  }, ignoreInit = T)
  
  
  ##### Scatter plot ----
  observeEvent({
    list(input$gam_previous_tabs,
         input$gam_previous_search_input_comp_aqs,
         input$gam_previous_comp_dataset_1,
         input$gam_previous_comp_dataset_2)
  }, {
    
    if(input$gam_previous_tabs == "gam_previous_comp") {
      
      tryCatch({
        cat("observe / Data comparison / plotting... \n")
        
        data_source_1 <- input$gam_previous_comp_dataset_1
        data_source_2 <- input$gam_previous_comp_dataset_2
        
        plot_dat_3 <- data.table::merge.data.table(plot_dat_1(),
                                                   plot_dat_2(),
                                                   by = c("date", "AQS_O3", "state"))
        
        if("epa_ember" %in% c(data_source_1, data_source_2)) {
          plot_dat_3[, smoke := "Data points"]
        } else {
          plot_dat_3[, smoke := smoke.x + smoke.y]
          plot_dat_3[, smoke := fifelse(smoke == 2, "Smoke day", 
                                        fifelse(smoke == 1, "Unmatched", "Non-smoke day"))]
          plot_dat_3 <- plot_dat_3[!is.na(smoke), ]
        }      
        
        plot_dat_3[, group := "Data points"]
        
        
        x_axis_name_pred <- paste0("Predicted MDA8 (ppb) (", data_source_1, ")")
        y_axis_name_pred <- paste0("Predicted MDA8 (ppb) (", data_source_2, ")")
        
        x_axis_name_resi <- paste0("SMO (ppb) (", data_source_1, ")")
        y_axis_name_resi <- paste0("SMO (ppb) (", data_source_2, ")")
        
        data.table::setnames(plot_dat_3, "MDA8O3_pred.x", x_axis_name_pred)
        data.table::setnames(plot_dat_3, "MDA8O3_pred.y", y_axis_name_pred)
        
        data.table::setnames(plot_dat_3, "SMO.x", x_axis_name_resi)
        data.table::setnames(plot_dat_3, "SMO.y", y_axis_name_resi)
        
        output$gam_previous_comp_plot_scatter_1 <- tryCatch({
          renderEcharts4r({
            e <- plot_dat_3 %>% 
              dplyr::group_by(smoke) %>% 
              e_charts_(x_axis_name_pred) %>%
              e_scatter_(y_axis_name_pred, 
                         symbolSize = 8) %>% 
              e_line_(x_axis_name_pred, 
                      name = "1:1 line",
                      symbol = "line") %>% 
              e_mark_line(data = list(yAxis = 70), 
                          title = "70 ppb",
                          lineStyle = list(type = "dashed", color = "grey"),
                          symbol = c("none", "none"),
                          name = "70 ppb") %>%
              e_mark_line(data = list(xAxis = 70), 
                          title = "",
                          lineStyle = list(type = "dashed", color = "grey"),
                          symbol = c("none", "none"),
                          name = "70 ppb") %>%
              e_tooltip(trigger = "item",
                        formatter = JS(sprintf("function(params) {
                    return params.seriesName + '<br/>' +
                    '%s: ' + params.value[1] + '<br/>' +
                    '%s: ' + params.value[0];
                                           }",
                                               data_source_1,
                                               data_source_2)
                        ),
                        axisPointer = list(
                          type = "cross"
                        )) %>% 
              e_title(text = paste0("Predicted MDA8 (",
                                    data_source_1, 
                                    " & ",
                                    data_source_2,
                                    ")"),
                      left = "left",
                      textStyle = list(fontSize = 14)) %>%
              e_x_axis(name = x_axis_name_pred, 
                       scale = T, 
                       nameLocation = "middle", nameGap = 30,
                       nameTextStyle = list(fontSize = 14, color = "black"),
                       axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
              e_y_axis(name = y_axis_name_pred,
                       scale = T, 
                       nameLocation = "middle",
                       nameGap = 40,
                       nameTextStyle = list(fontSize = 14, color = "black"),
                       axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
              e_legend(top = "8%") %>%
              e_color(c("black", "red", "green", "blue")) %>% 
              e_datazoom(type = "inside") %>%
              e_toolbox(feature = list(saveAsImage = list(name = paste0("plot_comp_pred_",
                                                                        data_source_1, 
                                                                        "_",
                                                                        data_source_2)), 
                                       dataZoom = list(),
                                       restore = list())) %>%
              e_grid(top = "17%",
                     left = "15%",
                     right = "10%",
                     bottom = "15%") %>%
              e_animation(duration = 500)
            e
          })
        }, error = function(e) {
          renderEcharts4r({
            e_charts() %>%
              e_title("No matching data on this site", left = "center", top = "middle") %>%
              e_text_style(color = "red", fontSize = 15)
          })
        })
        
        output$gam_previous_comp_plot_scatter_2 <- tryCatch({
          renderEcharts4r({
            e <- plot_dat_3 %>% 
              dplyr::group_by(group) %>% 
              e_charts_(x_axis_name_resi) %>%
              e_scatter_(y_axis_name_resi, 
                         symbolSize = 8) %>% 
              e_line_(x_axis_name_resi, 
                      name = "1:1 line",
                      symbol = "line") %>% 
              e_tooltip(trigger = "item",
                        formatter = JS(sprintf("function(params) {
                    return params.seriesName + '<br/>' +
                    '%s: ' + params.value[1] + '<br/>' +
                    '%s: ' + params.value[0];
                                           }",
                                               data_source_1,
                                               data_source_2)
                        ),
                        axisPointer = list(
                          type = "cross"
                        )) %>% 
              e_title(text = paste0("SMO (",
                                    data_source_1, 
                                    " & ",
                                    data_source_2,
                                    ")"),
                      left = "left",
                      textStyle = list(fontSize = 14)) %>%
              e_x_axis(name = x_axis_name_resi, 
                       scale = T, 
                       nameLocation = "middle", nameGap = 30,
                       nameTextStyle = list(fontSize = 14, color = "black"),
                       axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
              e_y_axis(name = y_axis_name_resi,
                       scale = T, 
                       nameLocation = "middle",
                       nameGap = 40,
                       nameTextStyle = list(fontSize = 14, color = "black"),
                       axisLabel = list(textStyle = list(fontSize = 14, color = "black"))) %>%
              e_legend(top = "8%") %>%
              e_color(c("black", "red", "green", "blue")) %>% 
              e_datazoom(type = "inside") %>%
              e_toolbox(feature = list(saveAsImage = list(name = paste0("plot_comp_resi_",
                                                                        data_source_1, 
                                                                        "_",
                                                                        data_source_2)), 
                                       dataZoom = list(),
                                       restore = list())) %>%
              e_grid(top = "17%",
                     left = "15%",
                     right = "10%",
                     bottom = "15%") %>%
              e_animation(duration = 500)
            e
          })
        }, error = function(e) {
          output$gam_previous_comp_plot_scatter_2 <- renderEcharts4r({
            e_charts() %>%
              e_title("No matching data on this site", left = "center", top = "middle") %>%
              e_text_style(color = "red", fontSize = 15)
          })
        })
        
      }, error = function(e) {
        cat("Error - observe / Data comparison / plotting: ", e$message, "\n")
        NULL
      })
    }
  }, ignoreInit = T)
  
  ### Server loading page ----
  session$onFlushed(function() {
    shinyjs::runjs("document.getElementById('loading_content').style.display = 'none';")
  }, once = T)
}


# Run the app ----
shinyApp(ui = ui,
         server = server)
