dashboardPage(
  dashboardHeader(title = "Jobs / Skills Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Skills - Jobs", tabName = "globalview"),
      menuItem("Jobgroup view", tabName = "groupview")
    )
  ),
  dashboardBody(
    tabItems(
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
      )
    )
  )
)
