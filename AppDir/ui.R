dashboardPage(skin = "black",
  dashboardHeader(title =tags$img(width='200px',src='http://www.kowarik.net/logoS.png')), 
                    #"Jobs / Skills Dashboard"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Europe", tabName = "map"),
      selectInput("Country", choices = as.list(c("Czech Republic", "Germany","Ireland","United Kingdom","---")),label="",selected="---"),
      menuItem("Skills - Jobs", tabName = "globalview"),
      menuItem("Jobgroup view", tabName = "groupview"),
      menuItem("Skill network", tabName = "skillnet")
    )
  ),
  dashboardBody(
    tags$style(HTML("
.box.box-solid.box-info>.box-header{
background:#666666;
border-bottom-color:#666666;
border-left-color:#666666;
border-right-color:#666666;
border-top-color:#666666;
}
.box.box-solid.box-info {
border:#666666;
}
                    ")),
    tabItems(
       tabItem("map",
               leafletOutput('myMap', width = "80%", height = 500)
              ),
      tabItem("globalview",
              fluidRow(
                valueBoxOutput("totaljobs"),
                valueBoxOutput("totalseeker")
                
              ),
              fluidRow(
                box(
                  width = 13, status = "info", solidHeader = TRUE,
                  title = "Job advertisments per category",
                  plotlyOutput("distPlot")
                )
              )
      ),
      tabItem("groupview",
              fluidRow(
                selectInput("jobgp", choices = as.list(jg),label="",selected="Science and engineering professionals"),
                valueBoxOutput("groupjobs"),
                valueBoxOutput("groupseeker")
              ),
              fluidRow(
                box(
                  width = 12, status = "info", solidHeader = TRUE,
                  title = "Top 10 needed Skills",
                  plotOutput("plotgroup")
                )
              )
      ),
      tabItem("skillnet",
              fluidRow(
                selectInput("skillg", choices = as.list(skilllev1),label="ESCO Skill level 1",selected="Computing")
              ),
              fluidRow(
                box(
                  width = 12, status = "info", solidHeader = TRUE,
                  title = "Connected Skills",
                  plotOutput("plotnet")
                )
              )
      )
    )
  )
)
