library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyalert)
library(shinyWidgets)
library(shinycustomloader)
library(plotly)
library(highcharter)
library(echarts4r)
library(bs4Dash)
library(waiter)


# npm config set registry https://npm.pkg.github.com/kmaheshkulkarni

bs4DashPage(
  enable_preloader = TRUE,
  title = "HQS",
  sidebar_collapsed = TRUE,
  #loading_duration = 1,
  navbar = dashboardHeader(
    
    leftUi = img(src="s.png",width = 170,height=59),
    rightUi = 
    #   bs4DropdownMenu(
    #   show = FALSE,
    #   labelText = "!",
    #   status = "danger",
    #   src = img(src="img/General Logo.png"),
    # )
    # John Deere Logo in the upper right corner
    # tags$li(
    #   class = "dropdown",
    #   img(
    #     src='img/General Logo.png',
    #     # The margin-right:150px might look weird, but it leaves room for the
    #     # "Logged in as" thing on the server
    #     style= 'margin-left:1350px; margin-top:10px')
    # # ),
    # tags$li(class = "dropdown",style= 'margin-right:50px; margin-top:21px',
    #         uiOutput("user_info")
    # )
  ),
  
  
  sidebar = bs4DashSidebar(
    expand_on_hover = TRUE,
    skin = "light",
    status = "primary",
    elevation = 3,
    opacity = 0.8,
    bs4SidebarMenu(
      id = "menu",
      flat = FALSE,
      compact = FALSE,
      child_indent = TRUE,
      bs4SidebarMenuItem(text = "Live Monitoring",tabName = "dashboard",icon = "calendar"),
      bs4SidebarMenuItem(text = "SPC Dashboard",tabName = "performance",icon = "dashboard"),
      bs4SidebarMenuItem(text = "Override Performance",tabName = "over_per",icon = "briefcase"),
      bs4SidebarMenuItem(text = "Report Download",tabName="Report",icon = "expand")
    )
  ),
  
  
  body = bs4DashBody(
    useShinyjs(),
    useShinyalert(),
    setShadow(class = "box"),
    tags$head(tags$style(HTML("div.col-sm-9 {padding:1px}"))),
    tags$head(tags$style(HTML("div.col-sm-7 {padding:1px}"))),
    tags$head(tags$style(HTML("div.col-sm-5 {padding:1px}"))),
    tags$head(tags$style(HTML("div.col-sm-6 {padding:1px}"))),
    tags$head(tags$style(HTML("div.col-sm-8 {padding:1px}"))),
    tags$head(tags$style(HTML("div.col-sm-4 {padding:1px}"))),
    tags$head(tags$style(HTML("div.col-sm-3 {padding:1px}"))),
    tags$head(tags$style(HTML("div.col-sm-2 {padding:1px}"))),
    tags$style(".small-box.bg-yellow { background-color: #FFDE00 !important; color: #000000 !important; }"),
    tags$style(".small-box.bg-green { background-color: #367C2B !important; color: #000000 !important; }"),
    tags$style(HTML(".box.box-solid.box-info > .box-header{ background:#367C2B; background-color:#367C2B }")),
    tags$style(".fa-bezier-curve {color:#27251F}"),
    tags$style(".fa-tachometer-alt {color:#27251F}"),
    tags$style(".fa-arrow-circle-down {color:#27251F}"),
    tags$style(".fa-arrow-circle-up {color:#27251F}"),
    tags$style(".fa-angle-double-up {color:#27251F}"),
    tags$style(".fa-exclamation {color:#27251F}"),
    tags$style(".fa-window-close {color:#FFDE00}"),
    tags$style(".fa-search-plus {color:#27251F}"),
    tags$style(".fa-search-minus {color:#27251F}"),
    
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "css/jdify.css"
      )
    ),
    #######################  Today Dashboard TAB  #####################################         
    bs4TabItems(
      bs4TabItem(tabName = "dashboard",
              fluidRow(
                column(3,
                       bs4Card(title = "Today's OverRide",width = 12,solidHeader = TRUE,status="info", 
                               maximizable = TRUE, closable = FALSE,
                           withLoader(highchartOutput("oc_count_plot"),type = "html",loader = "loader3")  
                       )
                ),
                
                column(9,
                       bs4Card(title = " Top 10 Overrideded Parameter",width = 12,solidHeader = TRUE,status="info", 
                               maximizable = TRUE, closable = FALSE,
                           withLoader(highchartOutput("oc_parameter_plot"),type = "html",loader = "loader3") 
                       )
                )
              ),
              fluidRow(
                bs4Card(title = "Top 25 Over and Under Specfication ",width = 12,solidHeader = TRUE,status="info", 
                        maximizable = TRUE, closable = FALSE,
                    withLoader(highchartOutput("over_under_specification_plot"), type = "html",loader = "loader3") 
                )
              )
              
      ),
      ####################### Performance TAB  #####################################     
      
      ############ TOP UI #########################
      tabItem(tabName = "performance",
              fluidRow(
                bs4Card(title = "Control Panel",width = 12,solidHeader = TRUE,status="info", 
                        maximizable = TRUE, closable = FALSE,
                    column(2, uiOutput("hx_dept_list_creation")),
                    column(3, uiOutput("hx_station_list_creation")),
                    column(2, uiOutput("hx_parameter_group_creation")),
                    column(2, uiOutput("hx_parameter_creation")),
                    column(2,
                           align="center",
                           airDatepickerInput(
                             inputId = "selected_date_range",
                             label = "Time period",
                             range = TRUE, value = c(Sys.Date()-7, Sys.Date())
                           )
                    ),
                    column(1,
                           align="center",
                           HTML('<br/>'),
                           actionButton("top_get_data","Get Data")
                    )
                    
                )
              ),
              ################# Infobox TAB Panel ###################
              fluidRow(
                column(2, valueBoxOutput("valuebox1",width = 12)),
                column(2, valueBoxOutput("valuebox2",width = 12)),
                column(2, valueBoxOutput("valuebox3",width = 12)),
                column(2, valueBoxOutput("valuebox4",width = 12)),
                column(2, valueBoxOutput("valuebox5",width = 12)),
                column(2, valueBoxOutput("valuebox6",width = 12))
              ),
              ######################## PLots #############################
              
              fluidRow(
                bs4Card(title = "Control Chart",solidHeader = TRUE,width = '12',collapsible = TRUE,height = 'auto',
                        status="warning", maximizable = TRUE, closable = FALSE,
                    withLoader(plotlyOutput("tourque_performance",height = 700) ,type = "html",loader = "loader10") 
                    #withLoader(highchartOutput("tourque_performance",height = 700) ,type = "html",loader = "loader10") 
                )
              ),
              # fluidRow(
              #   bs4Card(title = "Performance",width = 12,solidHeader = TRUE,status="info", maximizable = TRUE,
              #           withLoader(highchartOutput("runner_chart") ,type = "html",loader = "loader3") 
              #   )
              # ),
              
              fluidRow(
                column(7,
                       bs4Card(title = "Normal Distribution",width = 12,solidHeader = TRUE,status="info", 
                               maximizable = TRUE, closable = FALSE,
                               fluidRow(
                                 column(12,
                                        withLoader(plotlyOutput("density_plot") ,type = "html",loader = "loader3") 
                                 )
                               )
                       )
                ),
                column(5,
                       bs4Card(title = "Process Capability Interpretation",width = 12,solidHeader = TRUE,status="info", 
                               maximizable = TRUE, closable = FALSE,
                               withLoader(highchartOutput("performance_index"),type = "html",loader = "loader3") 
                       )
                )
                
                
                
              )
      ),
      
      
      
      
      
      ####################### Report TAB #################
      
      tabItem(tabName = "Report",
                 fluidRow(
                   bs4Card(title = "Control Chart",solidHeader = TRUE,width = '12',collapsible = FALSE,height = 'auto',status="warning", 
                           maximizable = TRUE, closable = FALSE,
                           withLoader(plotlyOutput("tourque_performance",height = 700) ,type = "html",loader = "loader10") 
                   )
                 ),
                 
                 fluidRow(
                   column(7, bs4Card(title = "Normal Distribution",width = 12,solidHeader = TRUE,status="info", 
                                     maximizable = TRUE, closable = FALSE, collapsible = FALSE,
                                  withLoader(plotlyOutput("density_plot") ,type = "html",loader = "loader3"))
                          ),
                   column(5, bs4Card(title = "Process Capability Interpretation",width = 12,solidHeader = TRUE,status="info", 
                                     maximizable = TRUE, closable = FALSE, collapsible = FALSE,
                                     withLoader(highchartOutput("performance_index"),type = "html",loader = "loader3") 
                   )
                   )
                   ),
              fluidRow(
                downloadButton('FlexD', label = "HTML"),
                downloadButton('Word', label = "WORD")
              )
                 
       ),
      
      
      
      #######################  Over  ALL performance TAB #####################################         
      
      ###################### Top UI #################
      tabItem(tabName = "over_per",
              fluidRow(
                bs4Card(title = "Control Panel",solidHeader = TRUE,width = '12',collapsible = TRUE,height = 'auto',status="info", 
                        maximizable = TRUE, closable = FALSE,
                    fluidRow(
                      column(2,
                             align="center",
                             selectInput("selected_year","Year",choices = c(2019:2050),
                                         selected = as.numeric(format(Sys.Date(),'%Y')),width = "85%")
                      ),
                      column(2,
                             align="center",
                             HTML('<br/>'),
                             materialSwitch("over_per_month_option","By Month",status="info",inline=FALSE)
                      ),
                      column(2,
                             align="center",
                             hidden(div(id="over_per_month_id",selectInput("selected_month","Month",choices = c("January"="01",
                                                                                                                "February"="02",
                                                                                                                "March"="03",
                                                                                                                "April"="04",
                                                                                                                "May"="05",
                                                                                                                "June"="06",
                                                                                                                "July"="07",
                                                                                                                "August"="08",
                                                                                                                "September"="09",
                                                                                                                "October"="10",
                                                                                                                "November"="11",
                                                                                                                "December"="12"),
                                                                           selected = format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%m"))))
                      ),
                      column(2,
                             align="center",
                             HTML('<br/>'),
                             hidden(div(id="over_per_week_option_material_id",materialSwitch("over_per_week_option","By Week",status="info",inline=FALSE)))
                             
                      ),
                      column(2,
                             align="center",
                             hidden(div(id="over_per_week_id",
                                        airDatepickerInput(
                                          inputId = "selected_date",
                                          label = "Weeks with Dates",
                                          range = TRUE, value = c(Sys.Date()-7, Sys.Date())
                                        )
                             ))
                      ),
                      column(2,
                             align="center",
                             HTML('<br/>'),
                             actionButton("get_or_data","Generate Report")
                      )
                    )
                )
                
              ),
              ############  All charts #############          
              fluidRow(
                column(6,
                       bs4Card(title = "Top 10 Parameter in OverRide",solidHeader = TRUE,width = '12',collapsible = TRUE,height = 'auto',
                               status="success", maximizable = TRUE, closable = FALSE,
                           withLoader(highchartOutput("parameter_issue"),type = "html",loader = "dnaspin")  
                       )
                ),
                column(6,
                       bs4Card(title = "Top 10 Station in OverRide ",solidHeader = TRUE,width = '12',collapsible = TRUE,height = 'auto',
                               status="success", maximizable = TRUE, closable = FALSE,
                           withLoader( highchartOutput("overrides_by_station"),type = "html",loader = "dnaspin")  
                       )
                )
                
              ),
              fluidRow(
                column(6,
                       bs4Card(title = "Top 10 Department in OverRide",solidHeader = TRUE,width = '12',collapsible = TRUE,height = 'auto',
                               status="success", maximizable = TRUE, closable = FALSE,
                           withLoader( highchartOutput("overrides_by_department"),type = "html",loader = "dnaspin")  
                       )
                ),
                column(6,
                       bs4Card(title = "Top 10 OverRide Code in OverRide",solidHeader = TRUE,width = '12',collapsible = TRUE,height = 'auto',
                               status="success", maximizable = TRUE, closable = FALSE,
                           withLoader(highchartOutput("overrides_by_person"),type = "html",loader = "dnaspin") 
                       )
                )
                
              ),
              fluidRow(
                column(6,
                       bs4Card(title = "Top 10 OverRide Code in OverRide",solidHeader = TRUE,width = '12',collapsible = TRUE,height = 'auto',
                               status="success", maximizable = TRUE, closable = FALSE,
                           withLoader( highchartOutput("overrides_by_oc_code"),type = "html",loader = "dnaspin")  
                       )
                ),
                column(6,
                       bs4Card(title = "Top 10 Comment in OverRide",solidHeader = TRUE,width = '12',collapsible = TRUE,height = 'auto',
                               status="success", maximizable = TRUE, closable = FALSE,
                           withLoader(highchartOutput("overrides_by_comment"),type = "html",loader = "dnaspin") 
                       )
                )
                
              )
              
      )
      
      
      ################## All End ########################        
    )
  )
  
)


