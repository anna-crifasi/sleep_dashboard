# ---------------------- 1. Load Libraries ----------------------
library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(MASS)
library(ggplot2)

# ---------------------- 2. Import & Clean Data ----------------------
sleep_data <- read.csv("Sleep_health_and_lifestyle_dataset.csv")

# Clean inconsistent BMI category labels
sleep_data <- sleep_data %>%
  mutate(BMI.Category = ifelse(BMI.Category %in% c("Normal", "Normal Weight"),
                               "Normal", BMI.Category))

# ---------------------- 3. UI Layout ----------------------
ui <- fluidPage(
  titlePanel("Sleep Health and Lifestyle"),
  
  sidebarLayout(
    # Sidebar filters
    sidebarPanel(
      selectInput("occupation", "Occupation:", 
                  choices = c("All", unique(sleep_data$Occupation))),
      radioButtons("gender", "Gender:", choices = c("All", "Female", "Male")),
      width = 3
    ),
    
    # Main panel with tabs
    mainPanel(
      tabsetPanel(
        # ---- TAB 1: Sleep Measures ----
        tabPanel("Sleep Measures",
                 fluidRow(
                   column(6, plotlyOutput("sleep_duration", height = "400px")), 
                   column(6, plotlyOutput("sleep_quality", height = "400px")),
                   style = "margin-top: 20px; margin-bottom: 60px;"
                 ),
                 fluidRow(
                   column(12, plotlyOutput("sleep_disorder", height = "400px")),
                   style = "margin-bottom: 60px;"
                 ),
                 fluidRow(
                   column(6, plotlyOutput("sleep_duration_violin", height = "400px")),
                   column(6, plotlyOutput("duration_vs_quality", height = "400px")),
                   style = "margin-bottom: 60px;"
                 )
        ),
        
        # ---- TAB 2: Demographics ----
        tabPanel("Demographics",
                 fluidRow(
                   column(6,
                          selectInput("age_outcome", "Sleep health measure",
                                      choices = c("Sleep Duration" = "Sleep.Duration",
                                                  "Sleep Quality" = "Quality.of.Sleep")),
                          style = "margin-top: 20px; margin-bottom: 60px;"
                   )
                 ),
                 fluidRow(
                   column(6, plotlyOutput("age_sleep")),
                   column(6, plotlyOutput("agegroup_sleep")),
                   style = "margin-bottom: 60px;"
                 ),
                 fluidRow(
                   column(12, plotlyOutput("occupation_sleep", height = "600px")),
                   style = "margin-bottom: 60px;"
                 )
        ),
        
        # ---- TAB 3: Physical Health Indicators ----
        tabPanel("Physical Health Indicators",
                 fluidRow(
                   column(6, plotlyOutput("activity_sleep")),
                   column(6, plotlyOutput("steps_sleep")),
                   style = "margin-top: 20px; margin-bottom: 60px;"
                 ),
                 fluidRow(
                   column(6, plotlyOutput("bmi_sleep")),
                   style = "margin-bottom: 60px;"
                 )
        ),
        
        # ---- TAB 4: Stress Indicators ----
        tabPanel("Stress Indicators",
                 fluidRow(
                   column(6,
                          selectInput("stress_outcome", "Sleep health measure:",
                                      choices = c("Sleep Duration" = "Sleep.Duration",
                                                  "Sleep Quality" = "Quality.of.Sleep")),
                          plotlyOutput("stress_sleep", height = "400px")
                   ),
                   column(6, plotlyOutput("heart_sleep", height = "400px")),
                   style = "margin-top: 20px; margin-bottom: 60px;"
                 ),
                 fluidRow(
                   column(12, plotlyOutput("bp_sleep", height = "400px")),
                   style = "margin-bottom: 60px;"
                 )
        )
      )
    )
  )
)

# ---------------------- 4. Server Logic ----------------------
server <- function(input, output) {
  
  # ---- Reactive Data Filter ----
  filtered_data <- reactive({
    data <- sleep_data
    if (input$occupation != "All") data <- data %>% filter(Occupation == input$occupation)
    if (input$gender != "All") data <- data %>% filter(Gender == input$gender)
    data
  })
  
  # ==========================================================
  # TAB 1: Sleep Measures
  # ==========================================================
  
  # Histogram: Sleep Duration
  output$sleep_duration <- renderPlotly({
    data <- filtered_data()
    plot_ly(
      data, x = ~Sleep.Duration, type = "histogram",
      marker = list(color = "darkblue"),
      hovertemplate = "Hours: %{x}<br>Count: %{y}<extra></extra>"
    ) %>%
      layout(
        title = "Sleep Duration",
        xaxis = list(title = "Hours of Sleep", tick0 = 5.5, dtick = 0.5),
        yaxis = list(title = "Count")
      )
  })
  
  # Bar: Sleep Quality
  output$sleep_quality <- renderPlotly({
    data <- filtered_data() %>%
      count(Quality.of.Sleep) %>%
      mutate(percent = n / sum(n) * 100)
    
    plot_ly(
      data, x = ~Quality.of.Sleep, y = ~n, type = "bar",
      marker = list(color = "cornflowerblue"),
      hovertemplate = paste(
        "Quality: %{x}<br>Count: %{y}<br>Percent: ", round(data$percent, 1), "%<extra></extra>"
      )
    ) %>%
      layout(
        title = "Sleep Quality",
        xaxis = list(title = "Quality of Sleep", dtick = 1),
        yaxis = list(title = "Count")
      )
  })
  
  # Pie: Sleep Disorder
  output$sleep_disorder <- renderPlotly({
    data <- filtered_data() %>%
      count(Sleep.Disorder) %>%
      mutate(percent = n / sum(n) * 100)
    
    plot_ly(
      data,
      labels = ~Sleep.Disorder, values = ~n, type = "pie",
      textinfo = "label+percent",
      marker = list(colors = c("darkcyan", "lightgray", "powderblue"))
    ) %>%
      layout(title = "Sleep Disorders")
  })
  
  # Violin: Sleep Duration by Disorder
  output$sleep_duration_violin <- renderPlotly({
    data <- filtered_data()
    plot_ly(
      data, x = ~Sleep.Disorder, y = ~Sleep.Duration,
      type = "violin", points = "all", jitter = 0.2, pointpos = 0,
      marker = list(color = "darkcyan", size = 4),
      hovertemplate = "Disorder: %{x}<br>Hours: %{y}<extra></extra>"
    ) %>%
      layout(title = "Sleep Duration by Sleep Disorder")
  })
  
  # Scatter: Duration vs Quality
  output$duration_vs_quality <- renderPlotly({
    data <- filtered_data()
    plot_ly(
      data, x = ~Sleep.Duration, y = ~Quality.of.Sleep,
      type = "scatter", mode = "markers",
      marker = list(size = 8, color = "darkblue", opacity = 0.7),
      hovertemplate = "Hours: %{x}<br>Quality: %{y}<extra></extra>"
    ) %>%
      layout(title = "Sleep Duration vs Sleep Quality")
  })
  
  # ==========================================================
  # TAB 2: Demographics
  # ==========================================================
  
  # Scatter: Age vs Selected Outcome
  output$age_sleep <- renderPlotly({
    req(input$age_outcome)
    data <- filtered_data()
    outcome_label <- gsub("\\.", " ", input$age_outcome)
    
    plot_ly(
      data,
      x = ~Age,
      y = data[[input$age_outcome]],
      type = "scatter", mode = "markers",
      color = ~Gender,
      colors = c("Female" = "orchid1", "Male" = "blue3"),
      text = ~paste("Age:", Age, "<br>Gender:", Gender,
                    "<br>", outcome_label, ":", data[[input$age_outcome]]),
      hoverinfo = "text"
    ) %>%
      layout(title = paste(outcome_label, "by Age"),
             xaxis = list(title = "Age"),
             yaxis = list(title = outcome_label))
  })
  
  # Stacked Bar: Sleep Disorder by Age Group
  output$agegroup_sleep <- renderPlotly({
    data <- filtered_data() %>%
      mutate(Age.Group = case_when(
        Age >= 27 & Age <= 34 ~ "27-34",
        Age >= 35 & Age <= 41 ~ "35-41",
        Age >= 42 & Age <= 48 ~ "42-48",
        Age >= 49 & Age <= 59 ~ "49-59"
      )) %>%
      count(Age.Group, Sleep.Disorder)
    
    plot_ly(
      data, x = ~Age.Group, y = ~n, type = "bar",
      color = ~Sleep.Disorder,
      colors = c("darkcyan", "lightgray", "powderblue")
    ) %>%
      layout(
        title = "Sleep Disorder by Age Group",
        barmode = "stack",
        xaxis = list(title = "Age Group"),
        yaxis = list(title = "Count")
      )
  })
  
  # Horizontal Stacked Bar: Sleep Quality per Occupation
  output$occupation_sleep <- renderPlotly({
    data <- filtered_data() %>%
      count(Occupation, Quality.of.Sleep) %>%
      group_by(Occupation) %>%
      mutate(prop = n / sum(n)) %>%
      ungroup()
    
    plot_ly(
      data,
      x = ~prop,
      y = ~Occupation,
      color = ~as.factor(Quality.of.Sleep),
      type = "bar",
      orientation = "h",
      text = ~paste0(
        "Occupation: ", Occupation, "<br>",
        "Sleep Quality: ", Quality.of.Sleep, "<br>",
        "Count: ", n, "<br>",
        "Percent: ", scales::percent(prop, accuracy = 0.1)
      ),
      hoverinfo = "text",
      marker = list(line = list(color = "white", width = 0.3)),
      colors = c("navyblue", "mediumblue", "dodgerblue", 
                 "lightskyblue", "lightblue1", "lightcyan1")
    ) %>%
      layout(
        barmode = "stack",
        bargap = 0.5,
        title = "Sleep Quality per Occupation",
        xaxis = list(title = "", tickformat = ".0%"),
        yaxis = list(title = ""),
        legend = list(title = list(text = "Sleep Quality"))
      )
  })
  
  # ==========================================================
  # TAB 3: Physical Health Indicators
  # ==========================================================
  
  # Regression: Sleep Duration vs Activity
  output$activity_sleep <- renderPlotly({
    data <- filtered_data() %>%
      filter(!is.na(Physical.Activity.Level), !is.na(Sleep.Duration))
    if (nrow(data) == 0) return(NULL)
    
    fit <- rlm(Sleep.Duration ~ Physical.Activity.Level, data = data)
    pred_df <- data.frame(
      Physical.Activity.Level = seq(min(data$Physical.Activity.Level),
                                    max(data$Physical.Activity.Level), length.out = 100)
    )
    pred_df$Sleep.Duration <- predict(fit, newdata = pred_df)
    
    plot_ly(data = data, showlegend = FALSE) %>%
      add_markers(x = ~Physical.Activity.Level, y = ~Sleep.Duration,
                  marker = list(size = 8, color = "darkseagreen3")) %>%
      add_lines(data = pred_df, x = ~Physical.Activity.Level, y = ~Sleep.Duration,
                line = list(color = "navyblue", width = 3)) %>%
      layout(
        title = "Physical Activity and Sleep Duration",
        xaxis = list(title = "Physical Activity (Minutes/Day)", dtick = 10),
        yaxis = list(title = "Sleep Duration (hours)")
      )
  })
  
  # Boxplot: Sleep Quality vs Steps
  output$steps_sleep <- renderPlotly({
    data <- filtered_data() %>%
      mutate(Steps.Bin = cut(Daily.Steps,
                             breaks = c(3000, 5000, 6000, 7000, 10000),
                             labels = c("3000-5000", "5001-6000", "6001-7000", "7001-10000"),
                             include.lowest = TRUE))
    
    plot_ly(
      data, x = ~Steps.Bin, y = ~Quality.of.Sleep,
      type = "box", boxpoints = "all",
      marker = list(color = "lightblue")
    ) %>%
      layout(title = "Sleep Quality vs Daily Steps",
             xaxis = list(title = "Daily Steps"),
             yaxis = list(title = "Sleep Quality"))
  })
  
  # Bar: Average Sleep by BMI Category
  output$bmi_sleep <- renderPlotly({
    data_summary <- filtered_data() %>%
      group_by(BMI.Category) %>%
      summarize(avg_sleep = mean(Sleep.Duration, na.rm = TRUE), count = n(), .groups = "drop") %>%
      mutate(BMI.Category = factor(BMI.Category, levels = c("Normal", "Overweight", "Obese")))
    
    plot_ly(data_summary, x = ~BMI.Category, y = ~avg_sleep,
            type = "bar", text = ~round(avg_sleep, 1),
            textposition = "auto",
            marker = list(color = "darkslateblue")) %>%
      layout(title = "Average Sleep Duration by BMI Category",
             yaxis = list(title = "Sleep Duration (hours)"))
  })
  
  # ==========================================================
  # TAB 4: Stress Indicators
  # ==========================================================
  
  # Scatter: Stress vs Selected Sleep Outcome
  output$stress_sleep <- renderPlotly({
    req(input$stress_outcome)
    data <- filtered_data()
    outcome_label <- gsub("\\.", " ", input$stress_outcome)
    
    plot_ly(
      data, x = ~Stress.Level, y = data[[input$stress_outcome]],
      type = "scatter", mode = "markers",
      color = ~Gender, colors = c("Female" = "orchid1", "Male" = "blue3"),
      marker = list(size = 8, opacity = 0.7)
    ) %>%
      layout(title = paste(outcome_label, "vs Stress Level"),
             xaxis = list(title = "Stress Level"),
             yaxis = list(title = outcome_label))
  })
  
  # Scatter: Heart Rate vs Sleep Duration
  output$heart_sleep <- renderPlotly({
    data <- filtered_data()
    plot_ly(
      data, x = ~Heart.Rate, y = ~Sleep.Duration,
      type = "scatter", mode = "markers",
      marker = list(size = 8, color = "darkcyan")
    ) %>%
      layout(title = "Sleep Duration vs Heart Rate",
             xaxis = list(title = "Heart Rate (BPM)"),
             yaxis = list(title = "Sleep Duration (hours)"))
  })
  
  # Line Plot: Blood Pressure vs Sleep Quality
  output$bp_sleep <- renderPlotly({
    data <- filtered_data() %>%
      separate(Blood.Pressure, into = c("SBP", "DBP"), sep = "/", convert = TRUE) %>%
      group_by(Quality.of.Sleep) %>%
      summarize(mean_SBP = mean(SBP, na.rm = TRUE),
                mean_DBP = mean(DBP, na.rm = TRUE)) %>%
      pivot_longer(cols = c(mean_SBP, mean_DBP),
                   names_to = "Measure", values_to = "Value") %>%
      mutate(Measure = recode(Measure, "mean_SBP" = "SBP", "mean_DBP" = "DBP"))
    
    plot_ly(
      data, x = ~Quality.of.Sleep, y = ~Value,
      color = ~Measure, type = "scatter", mode = "lines+markers",
      colors = c("darkblue", "lightblue"),
      line = list(width = 4), marker = list(size = 8)
    ) %>%
      layout(title = "Average Systolic and Diastolic BP by Sleep Quality",
             xaxis = list(title = "Sleep Quality"),
             yaxis = list(title = "Blood Pressure (mmHg)"),
             legend = list(title = list(text = "Measure")))
  })
}

# ---------------------- 5. Run App ----------------------
shinyApp(ui, server)