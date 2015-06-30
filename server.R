
source("lotf.R")

shinyServer(function(input, output) {
  
  # Drop-down selection box for Author
  output$choose_author <- renderUI({
    selectInput("author", "Author", as.list(getAuthors()))
  })
  
  # Drop-down selection box for Title
  output$choose_title <- renderUI({
    
    # Get the Titles by that Author
    # author <- get(input$author)
    titles <- getTitles(input$author)
    
    # Create the checkboxes and select them all by default
    selectInput("title", "Choose Title", titles)
  })
  
  
  # Output the data
  output$text <- renderUI({
    text <- getText(title = input$title, author = input$author)
    #HTML(paste0(text, collapse = "<br/>"))
    if (length(text) > 0) {
      html <- htmlify(text)
      HTML(html)
    } else { "" }
  })
})