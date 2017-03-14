dashboardPage(skin = "black",
  dashboardHeader(title = "Jobs / Skills Dashboard"),
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
                  plotOutput("distPlot")
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
                  width = 13, status = "info", solidHeader = TRUE,
                  title = "Top 5 needed Skills",
                  plotOutput("plotgroup")
                )
              )
      ),
      tabItem("skillnet",
              fluidRow(
                selectInput("skillg", choices = as.list(skilllev1),label="",selected="Computing")
              ),
              fluidRow(
                box(
                  width = 13, status = "info", solidHeader = TRUE,
                  title = "Connected Skills",
                  plotOutput("plotnet")
                )
              )
      )
    )
  )
)
