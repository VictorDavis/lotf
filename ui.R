shinyUI(pageWithSidebar(
  
  headerPanel("Library of the Future"),
  
  sidebarPanel(
    uiOutput("choose_author"),
    uiOutput("choose_title")
  ),
  
  mainPanel(
    htmlOutput("text")
  )
))