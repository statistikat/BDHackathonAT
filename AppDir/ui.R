dashboardPage(
  dashboardHeader(title = "Austria - BD Hackathon"),
  dashboardSidebar(
    selectInput("jobgp", label = h3("Job group"), 
                choices = as.list(jg)),
    sidebarMenu(
      menuItem("Skills - Jobs", tabName = "globalview"),
      menuItem("Jobgroup view", tabName = "groupview")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("globalview",
              fluidRow(
                valueBoxOutput("rate"),
                valueBoxOutput("count"),
                valueBoxOutput("users")
              ),
              fluidRow(
                box(
                  width = 8, status = "info", solidHeader = TRUE,
                  title = "Popularity by package (last 5 min)",
                  bubblesOutput("packagePlot", width = "100%", height = 600)
                ),
                box(
                  width = 4, status = "info",
                  title = "Top packages (last 5 min)",
                  tableOutput("packageTable")
                )
              )
      ),
      tabItem("groupview",
              fluidRow(
                valueBoxOutput("rate2"),
                valueBoxOutput("count2"),
                valueBoxOutput("users2")
              )
      )
    )
  )
)
