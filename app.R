
`%notin%` <- Negate(`%in%`)
library(readxl)
library(tidyverse)
full_theme <- function(){
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 1, hjust = 1),
        axis.title = element_text(size = 14),
        plot.caption = element_text(size = 12),
        legend.title=element_text(size=14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 15)
  )
}
load("pharm_new.RData")
#Function for making pretty y-axis breaks
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}


oxyhydro <- c("Hydromorphone", "Oxycodone")

abc_oh <- pharm_new |> 
 filter(generic_match %in% oxyhydro) |> 
  select(id, province, the_year, generic_match, quant,
         unit, substance_data, subs_quant, 
         facility, incident_type, incident_subtype, grouped_cat) |>
  rename(incident_id = id) |> 
  mutate(prov_drug = paste(province, generic_match, sep = "\n"))

# Creating variable to sort provinces so that they are in the same order
# Also helps for organizing them on the selectInput
prov_pick <- sort(unique(abc_oh$province))
op_lev <- data.frame(Prov = rep(prov_pick, length(oxyhydro)),
                     Drug = rep(oxyhydro, length(prov_pick))) |> 
  unite("lev", Prov:Drug, sep = "\n", remove = F) |> 
  arrange(Drug, Prov)
op_lev <- crossing(prov_pick, oxyhydro) |> 
  unite("lev", prov_pick:oxyhydro, sep = "\n", remove = F) |> 
  arrange(oxyhydro, prov_pick)

abc_oh$prov_drug <- factor(abc_oh$prov_drug,
                           levels = op_lev$lev, ordered = T)
the_years <- unique(abc_oh$the_year)
abc_oh$incident_type[abc_oh$incident_type == "Loss"] <- "Lost"

# Set colours so they stay the same by grouped_cat
coul <- rev(scales::viridis_pal()(6))
my_colours <- coul[as.numeric(as.factor(unique(pharm_new$grouped_cat)))]
names(my_colours) <- unique(pharm_new$grouped_cat)
rm(coul)

library(shiny)
library(shinythemes)
library(bslib)

group_options <- c("100-499","500-999","1000-1499", "1500-1999","2000+")
light <- bslib::bs_theme(version = 5, bootswatch = "flatly")
dark <- bslib::bs_theme(version = 5, bootswatch = "darkly")

loss_subtype <- sort(unique(
 abc_oh$incident_subtype[abc_oh$incident_type == "Lost"]
 ))

theft_subtype <- sort(unique(
 abc_oh$incident_subtype[abc_oh$incident_type == "Theft"]
))

source("explanation.R")

####--------------------------------UI-------------------------------------####

ui <- navbarPage("Opioids Lost/Stolen at Pharmacies 2018-2024",
####---------------------------------Panel 1--------------------------------####
                 tabPanel("Oxycodone & Hydromorphone Lost Events",
                          # Sidebar with a slider input for number of bins
                          sidebarLayout(
                            sidebarPanel(width = 2,
                                         checkboxGroupInput("provs", "Select by Province",
                                                            choices = unique(abc_oh$province),
                                                            selected = c("AB", "BC", "ON"),
                                                            width = "100%", inline = TRUE),
                                         checkboxGroupInput("grouped", "Select Grouped Categories",
                                                            choices = sort(unique(abc_oh$grouped_cat)),
                                                            selected = group_options),
                                         radioButtons("incidents", "Select Occurrence Types",
                                                      choices = sort(unique(abc_oh$incident_type)),
                                                      selected = c("Lost")),
                                         radioButtons("percap", "Per 100 000 People?",
                                                      choices = c("Yes", "No"),
                                                      selected = "No")
                            ),
                            # Show a plot of the generated distribution
                            mainPanel(
                            tabsetPanel(
                            tabPanel("Number of Occurrences",
                                     fluidRow(
                                       plotOutput("mplot",
                                           click = "plot_click2",
                                           height = 700)),
                                fluidRow(new_panel2())),
                          tabPanel("Occurrences by Subtype",
                                     plotOutput(
                                        "breakdown",
                                        height = 700
                                      )
                                   )
                                      ))
                 )), 
                 tabPanel("Total Opioid Tablets Lost/Stolen",
                          sidebarPanel(width =2,
                                       checkboxGroupInput("prov2",
                                                          "Select by Province",
                                                          choices = unique(abc_oh$province),
                                                          selected = c("AB", "BC"),
                                                          inline = TRUE),
                                       radioButtons(
                                         "include", 
                                         "Include Losses less than 100",
                                         choices = c("Yes", "No"),
                                         selected = "No"),
                                       radioButtons(
                                         "mpills",
                                         "By Mass or Number of Pills",
                                         choices = c("Mass", "Number of Pills"),
                                         selected = "Number of Pills"),
                                       radioButtons(
                                         "percap2", 
                                         "Per 100 000 People?",
                                          choices = c("Yes", "No"),
                                                   selected = "No")
                                       
                                       
                          ),
                          mainPanel(plotOutput("mplot2", height = 700), 
                                    width = 10)
                 )#,
#                 tabPanel("Event by Subtype",
#                          sidebarPanel(width = 2,
#                                       checkboxGroupInput("prov3", "Select Provinces",
#                                                          choices = unique(abc_oh$province),
#                                                          selected = c("AB"),
#                                                          inline = TRUE),
#                                       radioButtons("t_event", "Select Event Types",
#                                                    choices = sort(unique(abc_oh$incident_type)),
#                                                    selected = c("Theft")),
#                                       checkboxGroupInput("grouped2", "Select Grouped Categories",
#                                                          choices = sort(unique(abc_oh$grouped_cat)),
#                                                          selected = unique(abc_oh$grouped_cat)),
#                                       radioButtons("percap3", "Per 100 000?",
#                                                    choices = c("Yes", "No"),
#                                                    selected = "No")
#                          ),
#                          mainPanel(plotOutput("subplot", height = 700), width = 10)
                 ####----------------------Look at all the Data-------------------------####
#                 tabPanel("Broad Look at the Data",
#                          sidebarPanel(width = 2,
#                                       checkboxGroupInput("prov3", "Select Provinces",
#                                                          choices = unique(abc_oh$province),
#                                                          selected = c("AB", "BC"),
#                                                          inline = TRUE),
#                                       radioButtons("inc3", "Select Incident Types",
#                                                    choices = sort(unique(abc_oh$incident_type)),
#                                                    selected = c("Lost")),
#                                       selectInput("inc_type", "Select Subtype",
#                                                   choices = c(
#                                                     loss_subtype,
#                                                   selected = "Loss Unexplained")),
#                                       
#                                       radioButtons("percap3", "Per 100 000?",
#                                                    choices = c("Yes", "No"),
#                                                    selected = "No")
#                          ),
#                          mainPanel(plotOutput("mplot3", height = 800), width = 9)
#                          
#                 )
)

####-----------------------------Server------------------------------------####
# Define server logic 
server <- function(input, output, session) {
  
  #Sidebar variables
  t_incidents <- reactive(input$incidents)
  p_cap <- reactive(input$percap)
  provs <- reactive(input$provs)
  tgroups <- reactive(input$grouped)
  
  # Incidents reactive
  f_var <- reactive({
    abc_oh |>
      filter(province %in% provs() &
             grouped_cat %in% tgroups() &
             incident_type %in% t_incidents()) 
  })
  observeEvent(input$incidents, {
    updateCheckboxGroupInput(session, "grouped", 
                             choices = sort(unique(abc_oh$grouped_cat)),
                             selected = unique(abc_oh$grouped_cat))
    }, ignoreInit = TRUE, once = TRUE
  )

  ####----------------------Opioid Tablets Incidents----------------------####
  output$mplot <- renderPlot({
    inc_plot <- f_var() |> 
      group_by(province, generic_match, 
               the_year, grouped_cat) |> 
      count(name = "the_count") |> 
      ungroup() |> 
      mutate(grouped_cat = fct_rev(grouped_cat)) |> 
      left_join(pop)
    validate(
      need(nrow(inc_plot) >0.9, "Please select some outputs") 
    )

    max_plot = function(){
      inc_plot |> 
      group_by(province, the_year, generic_match) |> 
      summarise(max_count = sum(the_count)) |> 
      pull(max_count) |> 
      max()
    }

    tgroup_length = length(tgroups())
    
    if(input$percap == "Yes"){
      inc_plot <- inc_plot |> 
        mutate(the_count = the_count/population * 100000)
      the_title = paste("Oxycodone & Hydromorphone Tablets ", 
                        t_incidents(), " per 100 000 People", sep = "")
      y_ax = "Occurrences per 100 000 People"
      max_y = round(max_plot(), 2) + 0.5
      custom_y = function(){
        scale_y_continuous(
                           expand = c(0,0), 
                           limits = c(0, max_y))
      }
    } else {
      the_title = paste("Oxycodone & Hydromorphone Tablets ", 
                        t_incidents(), " by Year", sep = "")
      y_ax = "Number of Occurrences"
      max_y = round(max_plot(), -2) + 100
      custom_y <- function(){
        scale_y_continuous(
                           expand = c(0,0), 
                           limits = c(0, max_y))
      }
    }


    the_sub <- paste("Grouped by", tgroups()[1], 
                     "to", tgroups()[tgroup_length],
                     sep = " ")
    ggplot(inc_plot, aes(x = the_year, y = the_count,
                         fill = grouped_cat)) +
      geom_col() +
      scale_x_date(date_labels = "%Y", breaks = the_years) +
      custom_y() +
      scale_fill_manual(values = my_colours) +
      labs(x = "Year", y = y_ax, fill = "Quantity Reported",
           title = the_title, 
           subtitle = the_sub,
           caption = "Limited to (All) Long Term Care, Licensed Dealers/Producers, Hospitals & Pharmacies
                 Individual incidents may have multiple different units stolen in one event. 
           E.g., 100 hydromorphone 8mg & 4mg may be reported missing simultaneously") +
      theme_bw() +
      facet_grid(province ~generic_match) +
      full_theme()
  })
   
  breakbar <- reactive({
      f_var() |>
        select(the_year, incident_id, province, 
               incident_subtype) |> 
        arrange(the_year, incident_id, province, 
                incident_subtype) |> 
        group_by(province, the_year, incident_subtype) |> 
        summarise(event = n_distinct(incident_id)) |>
        ungroup() |> 
        left_join(pop)
  })
  output$breakdown <- renderPlot({
    inc_plot2 <- breakbar()
    validate(
      need(nrow(inc_plot2) >0.9, "Please select some outputs")
    )
    if(p_cap() == "Yes"){
      inc_plot2 <- inc_plot2 |> 
        mutate(event = event/population * 100000)
      the_title = paste("Oxycodone & Hydromorphone Tablets ", 
                        t_incidents(), " per 100 000 People", sep = "")
      y_ax = "Occurrences per 100 000 People"
    } else {
      the_title = paste("Oxycodone & Hydromorphone Tablets ", 
                        t_incidents(), " by Year", sep = "")
      y_ax = "Number of Occurrences"
    }
    
    tgroup_length = length(tgroups())
    
    the_sub <- paste("Grouped by", tgroups()[1], 
                     "to", tgroups()[tgroup_length],
                     sep = " ")
    ggplot(inc_plot2, aes(x = the_year, y = event, fill = incident_subtype)) +
      geom_bar(stat = "identity", 
               position = position_dodge2(preserve = "single")) +
      scale_x_date(date_labels = "%Y", breaks = the_years) +
      scale_y_continuous(
                         expand = c(0,0),
                         breaks = integer_breaks(n = 6)) +
      labs(x = "Year",
           y = y_ax, fill = paste0("Type of ", t_incidents()),
           title = the_title,
           subtitle = the_sub,
           caption = "Limited to (All) Long Term Care, Licensed Dealers/Producers, Hospitals & Pharmacies") +
      theme_bw() +
      facet_wrap(~province) +
      full_theme() +
      theme(legend.position = "bottom")
    
  })
  ####----------------------Total Oxycodone & HDM ----------------------####

  p_weight <- reactive({req(input$mpills)})
  to_include <- reactive({req(input$include)})
  include_all <- reactive({
    if(to_include() == "Yes"){
      df = abc_oh
    } else {
      df = abc_oh |> 
        filter(!(incident_type == "Lost" & grouped_cat == "1-99"))
    }
    return(df)
  })
  pill_filter <- reactive({
    
    df = include_all() |>
      filter(province %in% input$prov2) 
    if(p_weight() == "Number of Pills" | is.null(p_weight())){
      df = df |> 
        group_by(province, prov_drug, the_year,
                 incident_type, substance_data) |> 
        summarise(total = sum(quant)) |> 
        ungroup()
    } else {
      df = df |>
        group_by(province, prov_drug, the_year, 
                 incident_type, substance_data)  |> 
        summarise(total = sum(subs_quant)) |> 
        ungroup()
    }
    df = df |> 
      left_join(pop)
    return(df)
  })
  
  
  
  output$mplot2 <- renderPlot({
    mwplot <- pill_filter()
    validate(
      need(nrow(mwplot) >0.9, "Please select some outputs")
    )
    if(input$percap2 == "Yes"){
      mwplot <- mwplot |>
        mutate(total = total/population * 100000)
      y_ax1 <- "per 100 000 people"
    } else {
      y_ax1 <- ""
    }
    if(p_weight() == "Number of Pills"){
      the_title <- paste0("Number of Tablets Lost/Stolen from Medical Centres ",
                          y_ax1)
      y_ax <- paste0("Number of Tablets ", y_ax1)
      
    } else {
      the_title <- paste0("Total MG Lost/Stolen ", y_ax1)
      y_ax <- paste0("Total Mass (mg) ", y_ax1)
    }
    max_y2 <- mwplot |> 
      group_by(prov_drug, incident_type, the_year) |> 
      summarise(max_count = sum(total)) |> 
      pull(max_count) |> 
      max()
    max_y2 = max_y2 + (max_y2 * 0.1)
    p <- ggplot(mwplot, aes(x = the_year, y = total,
                       fill = substance_data)) +
      geom_col() +
      scale_x_date(date_labels = "%Y", breaks = the_years) +
      scale_y_continuous(labels = scales::comma,
                         expand = c(0,0),
                         limits = c(0, max_y2),
                         breaks = integer_breaks(n = 6)) +
      theme_bw() +
      facet_grid(incident_type ~ prov_drug, scales = "free_x") +
      full_theme() +
      theme(legend.position = "bottom")
    if(to_include() == "No"){
      p + labs(x = "Year", y = y_ax, fill = "Quantity Reported",
               title = the_title,
               caption = "Limited to (All) Long Term Care, Licensed Dealers/Producers, Hospitals & Pharmacies
               Not including items lost less than 100")
    }
    else {
      p + labs(x = "Year", y = y_ax, fill = "Quantity Reported",
               title = the_title,
               caption = "Limited to (All) Long Term Care, Licensed Dealers/Producers, Hospitals & Pharmacies")
    }
    
  })
  ####----------------------Subtype----------------------####
#  the_events <- reactive(input$t_events)
#  p_cap <- reactive(input$percap3)
#  provs <- reactive(input$prov3)
#  tgroups <- reactive(input$grouped2)
#  sub_var <- reactive({
#    main = abc_oh |>
#      filter(province %in% prov3(),
#             grouped_cat %in% grouped(),
#             incident_type == the_events())
#    event_stat = main |> 
#      select(the_year, incident_id, province, 
#             incident_subtype) |> 
#      arrange(the_year, incident_id, province, 
#              incident_subtype) |> 
#      group_by(province, the_year, incident_subtype) |> 
#      summarise(the_count = sum(quant), 
#                event = n_distinct(incident_id)) |>
#      ungroup() |> 
#      left_join(pop)
#    sum_stats = main |> 
#      select(the_year, incident_id, province, 
#             incident_subtype) |> 
#      arrange(the_year, incident_id, province, 
#              incident_subtype) |> 
#      group_by(province, the_year, incident_subtype, incident_id) |> 
#      summarise(the_count = sum(quant)) |>
#      group_by()
#      
#  })
#  the_counter <- reactive(0)
#  observeEvent(input$incidents, {
#    updateCheckboxGroupInput(session, "grouped2", 
#                             choices = sort(unique(abc_oh$grouped_cat)),
#                             selected = selected = group_options)
#  }, ignoreInit = TRUE, once = TRUE
#  )
  
  ####------------ Broad Exploration of the data ------------####
  # To be completed
 # output$mplot3 <- renderPlot({
    
    
    
    
    
#  })
}

# Run the application 
shinyApp(ui = ui, server = server)
