new_panel2 <- function(){
  tagList(
  tags$h1(" "),
  tags$h1(" "),
  tags$h2("About the data"),
  tags$p("This data explore all reported losses from pharmacies in Canada from 2018-2024. The data is from an FOI conducted by CBC data journalist Tara Carman. You can read her github explanation", tags$a("here.", href = "https://github.com/taracarman/druglosses/issues"), "Note not every lost tablet is diverted. Everything from clerical errors, miscounts and  can explain why drugs sometimes go missing."),
  tags$h3("How I suggest looking at this data"),
  tags$p("Given that it is impossible to determine which of these reported losses are genuine errors and which are due to potential diversion the threshold that I suggest using the Grouped Category to limit losses to greater than 100. While it's possible that some small losses are illegitimate, and some losses greater than 100 are valid mistakes, losses greater than 100 serves as sort of a rough estimate for demonstrating a trend of missing drugs."),
  tags$h3("Exploring the data yourself"),
  tags$p("If you would like to explore the data yourself, you can either use the dataset provided by Carman, or you can access my code", tags$a("here", href = "https://github.com/ADrugResearcher/pharmacy_data"))
  )
}
