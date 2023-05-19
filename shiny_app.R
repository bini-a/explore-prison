library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(maps)
library(stringr)
library(shinydashboard)
library(RColorBrewer)


required_packages <- c("shiny", "shinythemes", "ggplot2", "dplyr", "maps", "stringr", "shinydashboard", "RColorBrewer")

# Check if required packages are installed and install if necessary
for (package in required_packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(maps)
library(stringr)
library(shinydashboard)
library(RColorBrewer)

# Load data
df <-
  read.csv("~/project-team25/data/prison_data.csv", stringsAsFactors = FALSE)
map_data <- map_data("state")
order_of_bars <-
  c("At the time of Arrest",
    "Past 30 days",
    "Past 12 months",
    "More than a year ago",
    "Never used")
order_edu <- c("Less Than High School", "High School Graduate", "Some College",
               "College Degree or More")
explore_cols <-
  c( "Age","Sex","Race","State","Highest_Year_Education_Before_Prison",
     "Age_group","Crime_type")

df <- df %>%
  filter(!State %in% c("-1", "-2", NA, "DC", "PR")) %>%
  filter(Race != "Other") %>%
  filter(Age_group != "<18")


df_selected <- df[explore_cols]

df <-
  df %>% mutate(used_any_ever = as.integer(if_any(
    c(Alcohol,
      Alcohol_12months,
      Alcohol_ever,
      Marijuana,
      Marijuana_30days,
      Marijuana_12months,
      Marijuana_ever,
      Heroin,
      Heroin_30days,
      Heroin_12months,
      Heroin_ever,
      Cocaine,
      Cocaine_30days,
      Cocaine_12months,
      Cocaine_ever
    ),
    ~ . == 1
  )),
  used_any_12months = as.integer(if_any(
    c(
      Alcohol_12months,
      Marijuana_12months,
      Heroin_12months,
      Cocaine_12months
    ),
    ~ . == 1
  )))


ui <- dashboardPage(
  dashboardHeader(title = "Prisoners and Drug Use"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Introduction",
      tabName = "intro",
      icon = icon("users")
    ),
    menuItem(
      "Explore Prison Population",
      tabName = "explore",
      icon = icon("users")
    ),
    menuItem("Drug Use", tabName = "drug_tab", icon = icon("map"))
  )),
  dashboardBody(tabItems(
    tabItem(tabName = "intro",
            fluidRow(
              box(width = 10,
                  h2("Introduction"),
                  br(), 
                  br(),
                  p("This is a shiny app that explores prison survey data from year 2016.The Bureau of Justice Statistics (BJS) conducted the Survey of Prison Inmates (SPI), a national, wide-ranging
survey of prisoners age 18 or older who were incarcerated in state or federal correctional facilities within the United States during 2016. We build this exploration tool to allow users to easily interact and visualize using filtered columns of data"),
                  br(),
                  p("Note: In the race column AA/IN refers to American Indian/Alaska Native and ANPI refers to Asian/Native Hawaiian/Other Pacific Islander")
              ))),
    
    tabItem(tabName = "explore",
            fluidPage(
              sidebarLayout(
                sidebarPanel( width =2,
                              radioButtons(
                                "explore_col",
                                "Select one:",
                                choices = c("Age Group", "Sex", "Race", "Education", 
                                            "Crime Type"),
                                selected = "Age Group"
                              ),
                              radioButtons(
                                "chart_type_0",
                                "Select Chart Type:",
                                choices = c("Bar", "Pie"),
                                selected = "Bar"
                              )
                              
                ),
                mainPanel( width=8,
                           tags$div(
                             style = "text-align: center; font-size: 24px; font-weight: bold; margin-top: 20px;",
                             "Demographic Distribution of Prison inmates",
                             plotOutput("plot0",  height = 500),
                             
                           )
                )
                
              )
            )),
    
    tabItem(tabName = "drug_tab",
            fluidPage(
              sidebarLayout(
                sidebarPanel( width = 2,
                              selectInput("age_group", "Age Group",
                                          sort(c(
                                            "All", unique(df$Age_group)
                                          )),
                                          selected = "All"),
                              selectInput("sex", "Sex",
                                          c("All", "Male", "Female"),
                                          selected = "All"),
                              selectInput("race", "Race",
                                          sort(c(
                                            "All", unique(df$Race)
                                          )),
                                          selected = "All"),
                              selectInput("education", "Education",
                                          sort(c(
                                            "All", unique(
                                              as.character(
                                                df$Highest_Year_Education_Before_Prison)
                                            ))), selected = "All"
                              ),
                              selectInput("crime", "Crime Type",
                                          sort(c("All", unique(df$Crime_type))), 
                                          selected = "All")
                              ,
                              radioButtons(
                                "chart_type",
                                "Select Chart Type:",
                                choices = c("Bar", "Pie"),
                                selected = "Bar"
                              )
                ),
                mainPanel(width = 8,
                          fluidRow(
                            tabBox(
                              width = 200, height = 600, 
                              tabPanel(
                                plotOutput('num_drugs_plot', height = 500),
                                title = "Drugs used before arrest"
                              ),
                              
                              tabPanel(
                                status = "primary",
                                plotOutput('last_use_plot',  height = 500),
                                title = "Last time of drug use",
                              ),
                              
                              tabPanel(
                                title = "Distribution across states",
                                fluidRow(
                                  column(
                                    width = 12,
                                    plotOutput('state_map_plot',  height = 500)
                                  ),
                                  
                                  column(
                                    width = 12,
                                    align = "center",
                                    div(
                                      class = "input-group",
                                      selectInput(
                                        "drug_type",
                                        "Drug Type",
                                        c("All", "Alcohol", "Marijuana", "Cocaine", 
                                          "Heroin"),
                                        selected = "All"
                                      )
                                    ),
                                    div(
                                      class = "input-group",
                                      selectInput(
                                        "time_period",
                                        "Time Period",
                                        c("Last 12 Months", "Lifetime"),
                                        selected = "Last 12 Months"
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                )
              ))
    ))
  ),
  tags$head(tags$style(HTML('* {font-family: "Helvetica"};')))
)

# Define server
server <- function(input, output, session) {
  
  df_filtered <- reactive({
    df %>%
      filter(if (input$age_group == "All")
        TRUE
        else
          Age_group == input$age_group) %>%
      filter(if (input$sex == "All")
        TRUE
        else
          Sex == input$sex) %>%
      filter(if (input$race == "All")
        TRUE
        else
          Race == input$race) %>%
      filter(if (input$education == "All")
        TRUE
        else
          Highest_Year_Education_Before_Prison == input$education) %>%
      filter(if (input$crime == "All") TRUE else Crime_type == input$crime)
  })
  
  
  output$plot0 <- renderPlot({
    selected_col = input$explore_col
    selected_col = ifelse(
      selected_col == "Age Group", "Age_group", selected_col)
    selected_col = ifelse(
      selected_col == "Education",
      "Highest_Year_Education_Before_Prison",
      selected_col
    )
    selected_col = ifelse(
      selected_col == "Crime Type", "Crime_type", selected_col)
    
    if (input$chart_type_0 == "Bar") {
      ggplot(df_selected, aes_string(x = selected_col)) +
        geom_bar(fill="#00d2ff") +
        stat_count(geom = "text",
                   aes(label = ..count..),
                   vjust = -0.5) +
        theme_bw(base_size=20) +
        {if (selected_col == "Race")
          scale_x_discrete(labels = c("2+ Races", "AI/AN", "ANPI",
                                      "Black", "Hispanic", "White"))}+
        labs(x = ifelse(selected_col == "Highest_Year_Education_Before_Prison",
                        "Highest educational attainment",
                        gsub("_", " ", selected_col)),
             y = "Count") +
        guides(fill = "none")
    }
    else{
      counts <- table(df_selected[[selected_col]])
      pct <- round(prop.table(counts) * 100, 1)
      ggplot(data = data.frame(counts),
             aes(x = "", y = Freq, fill = Var1)) +
        geom_bar(stat = "identity", color = "white") +
        coord_polar("y", start = 0) +
        labs(fill = selected_col) +
        geom_text(aes(label = paste0(round(
          Freq / sum(Freq) * 100
        ), "%")), position = position_stack(vjust = 0.5)) +
        theme_void(base_size = 20)
    }
  })
  
  output$last_use_plot <- renderPlot({
    if (input$chart_type == "Bar") {
      ggplot(df_filtered(), aes(
        x = time_of_use
      )) +
        geom_bar(fill="#00d2ff") +
        stat_count(geom = "text",
                   aes(label = ..count..),
                   vjust = -0.5) +
        theme_bw(base_size=20) +
        labs(x = "Time of Use", y = "Count") +
        guides(fill = "none")
    }
    else{
      counts <- table(df_filtered()$time_of_use)
      pct <- round(prop.table(counts) * 100, 1)
      ggplot(data = data.frame(counts),
             aes(x = "", y = Freq, fill = Var1)) +
        geom_bar(stat = "identity", color = "white") +
        coord_polar("y", start = 0) +
        labs(fill = "Time of Use", y = "Count") +
        geom_text(aes(label = paste0(round(
          Freq / sum(Freq) * 100
        ), "%")), position = position_stack(vjust = 0.5)) +
        theme_void(base_size=20)
    }
  })
  
  
  output$num_drugs_plot <- renderPlot({
    if (input$chart_type == "Bar") {
      ggplot(df_filtered(), aes(x = num_drugs)) +
        geom_bar(fill="#00d2ff") +
        stat_count(geom = "text",
                   aes(label = ..count..),
                   vjust = -0.5) +
        theme_bw(base_size=20) +
        labs(x = "Drug(s) Used", y = "Count") +
        guides(fill = "none")
      
    } else{
      counts <- table(df_filtered()$num_drugs)
      pct <- round(prop.table(counts) * 100, 1)
      ggplot(data = data.frame(counts),
             aes(x = "", y = Freq, fill = Var1)) +
        geom_bar(stat = "identity", color = "white") +
        coord_polar("y", start = 0) +
        labs(fill = "Drug(s) Used") +
        geom_text(aes(label = paste0(round(
          Freq / sum(Freq) * 100
        ), "%")), position = position_stack(vjust = 0.5)) +
        theme_void(base_size=20)
    }
  })
  
  output$state_map_plot <- renderPlot({
    if (input$drug_type == "All") {
      drug_col <-
        ifelse(input$time_period == "Lifetime",
               "used_any_ever",
               "used_any_12months")
      
    } else{
      drug_col = paste0(
        input$drug_type,
        ifelse(input$time_period == "Lifetime", "_ever", "_12months")
      )
    }
    
    map_df = df_filtered() %>%
      select(State, drug_col) %>%
      group_by(State) %>%
      summarize(percent_use = 100 * mean(get(drug_col) == 1))
    
    map_df$State = str_to_lower(state.name[match(map_df$State, state.abb)])
    
    map_df = left_join(map_data, map_df, by = c("region" = "State"))
    
    ggplot(map_df, aes(
      x = long,
      y = lat,
      group = group,
      fill = percent_use
    )) +
      geom_polygon() +
      theme_void() +
      scale_fill_distiller(palette = "RdBu",
                           direction = 1,
                           name = "% Use") +
      coord_fixed(1.3)
  })
}

# Run app
shinyApp(ui = ui, server = server)