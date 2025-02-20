library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(here)
library(stringr)

# Load the dataset
data <- read.csv(here("data", "clean_data.csv"))

# Recode demographic variables
data <- data %>%
  mutate(
    Gender = recode(Gender, `0` = "Boy", `1` = "Girl", `2` = "Transgender", `3` = "Agender"),
    Race2 = recode(Race2, `1` = "Non-Hispanic White", `2` = "All Other Race/Ethnicity"),
    FRPL = recode(FRPL, `0` = "Yes", `1` = "No", `2` = "I don't know")
  )

# Calculate valid ranges for sliders
slider_ranges <- list(
  mv = list(
    max = round(max(data$MV_avg, na.rm = TRUE)),
    value = round(mean(data$MV_avg, na.rm = TRUE))
  ),
  lp = list(
    max = round(max(data$LP_avg, na.rm = TRUE)),
    value = round(mean(data$LP_avg, na.rm = TRUE))
  ),
  sb = list(
    max = round(max(data$SB_avg, na.rm = TRUE)),
    value = round(mean(data$SB_avg, na.rm = TRUE))
  ),
  st = list(
    max = round(max(data$avST, na.rm = TRUE)),
    value = round(mean(data$avST, na.rm = TRUE))
  )
)

# Define a mapping for variable names
variable_labels <- list(
  "MV_avg" = "Average Daily MVPA (minutes)",
  "LP_avg" = "Average Daily Light-Intensity PA (minutes)",
  "SB_avg" = "Average Daily Sedentary Time (minutes)",
  "avST" = "Average Daily Phone Screen Time (minutes)",
  "ND_avg" = "Average Daily Nature Dose (minutes)",
  "avperiod" = "Average Nightly Sleep Duration (minutes)",
  "Race2" = "Two-Category Race & Ethnicity",
  "Age_year" = "Age (Years)",
  "FRPL" = "Free or Reduced-Price Lunch Eligibility"
)

# Define UO brand colors
uo_colors <- list(
  green = "#154733",      
  yellow = "#FEE123",     
  legacy_green = "#004F27", 
  grass_green = "#518241",  
  lime_green = "#7BAE28",   
  chartreuse = "#9FD430",   
  dark_blue = "#004D6C",    
  light_blue = "#3F8EA8"
) 

# Custom CSS for branding
custom_css <- tags$style(HTML("
  .skin-blue .main-header .navbar { background-color: #154733; }  /* UO Green */
  .skin-blue .main-sidebar { background-color: #004F27; }  /* Legacy Green */
  .skin-blue .main-header .logo { background-color: #518241; color: white; }  /* Grass Green */
  .box { border-top: 3px solid #FEE123; }  /* UO Yellow */
  .content-wrapper, .right-side { background-color: #f8f9fa; } /* Light background */
  .footer { text-align: right; padding: 10px; background: #f8f9fa; }
  .bold { font-weight: bold; }
"))

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Activity-Nature-Sleep"),
  dashboardSidebar(
    div(
      class = "sidebar-menu",
      style = "display: flex; flex-direction: column; height: 95vh;",
      div(
        style = "flex: 1; overflow-y: auto;",
        sidebarMenu(
          menuItem("Dashboard Information", tabName = "dashboard_info", icon = icon("info-circle")),
          menuItem("Study Information", tabName = "study_info", icon = icon("info-circle")),
          menuItem("Demographics", tabName = "demographics", icon = icon("users")),
          menuItem("Distributions", tabName = "distributions", icon = icon("chart-bar")),
          menuItem("Relationships", tabName = "relationships", icon = icon("project-diagram")),
          menuItem("Predict Active Adolescent's Sleep", tabName = "predict_sleep", icon = icon("bed"))
        )
      ),
      div(
        style = "flex-shrink: 0; padding: 8px; margin-bottom: 10px; background-color: #004F27;",
        tags$img(src = "logo.jpg", style = "width: 100%; height: auto;")
      )
    )
  ),
  dashboardBody(
    custom_css,
    tabItems(
      # Dashboard Information Tab
      tabItem(tabName = "dashboard_info",
              fluidRow(
                box(
                  width = 12,
                  h4("Dashboard Information"),
                  p(tags$span(class = "bold", "Goals of this Specific Dashboard:"), 
                    "This dashboard is intended to provide a way to disseminate the results of my dissertation project and allow for interested viewers to be able to understand the importance of balancing activity behaviors and environmental exposures to promote sleep health among physically active adolescents."),
                  p(tags$span(class = "bold", "Rationale/Impact:"), 
                    "Adolescents in the United States have been declared as an at-risk population due to an ongoing insufficient sleep endemic. In other words, although adolescents are recommended to achieve between 8-10 hours of sleep per night to support their health and development during a critical period of development, a majority do not meet the minimum of 8 hours per night of sleep. This places them at increased risk for poor academic performance, behavioral problems, poor development during puberty, and, over a longer term, a greater risk for developing chronic health conditions."),
                  p(tags$span(class = "bold", "Caution in Interpretation and Generalizability:"), 
                    "The data and findings presented in this dashboard should be interpreted with caution, as they may not be generalizable to the broader adolescent population. Participants were recruited from the greater Eugene-Springfield, Oregon area and represent a sample of 326 adolescents (ages 12-17 years) who were abnormally physically active. Given the specific geographic location, recruitment methods, and sample characteristics, these findings may not reflect the behaviors, exposures, or health outcomes of all adolescents. Viewers should consider these factors when interpreting the data and applying insights beyond this study population.")
                )
              )
      ),
      
      # Study Information Tab
      tabItem(tabName = "study_info",
              fluidRow(
                box(
                  width = 12,
                  h4("Study Information"),
                  p("Co-PIs: Dr. Elizabeth Budd & Dr. Nichole Kelly", tags$br(), 
                    "Research Coordinators: Zach Farley & Esmeralda Castro"),
                  p("Study Description:
The NatureDose Teen Study is a prospective study among adolescents conducted in the greater Eugene-Springfield, Oregon area. Data were collected between May and October of 2022 and a second round of data were collected for additional participants between May and October of 2023.  Participants were recruited through postcards, flyers, and word of mouth (snowball sampling). In total, 326 adolescents (ages 12-17 years) were recruited and enrolled in the study; the data seen in this dashboard represents that of these 326 adolescents. 
Study Period: Participants in the study were enrolled for a 7-day data collection period. On day zero – the day participants first met with research staff – participants and their parent/guardian were informed of the study details, and assented and consented to participate, respectively. Also, during this visit, participants completed a one-time baseline survey that covered demographics, and some psychosocial constructs. Additionally, participants were asked to download the NatureDose Phone Application on their personal phones – to continuously batch how much time they spent exposed to natural elements shown to be beneficial to health and well-being – and were fitted with an accelerometer on their non-dominant wrist (worn like a watch). Accelerometers are validated to collect movement-related information continuously throughout a given study period, yielding insight into how much time a given participant is engaging in varying intensities of physical activity, sedentary behavior (or the time spent in a sitting or reclining position), and can estimate sleep periods. Participants had the NatureDose phone application on their phone and wore the accelerometer continuously throughout the following 7-day study period. Additionally, each night of the study period (Day 1 – Day 7) participants were sent a Qualtrics survey link to report about daily stressors, body appreciation, and to use their phones internal monitoring tools to report their phone-based screen time for each study period day. Thus, data shown in this dashboard are representative of 7-days of data among the 326 adolescent participants enrolled in the study.")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  h4("Variable Descriptions"),
                  dataTableOutput("variable_table")
                )
              )
      ),
      
      # Demographics Tab
      tabItem(tabName = "demographics",
              fluidRow(
                column(width = 12, align = "center",
                       box(width = NULL,
                           selectInput("demo_var", "Select Demographic Variable:", 
                                       choices = c("Two-Category Race & Ethnicity" = "Race2", 
                                                   "Gender" = "Gender", 
                                                   "Age (Years)" = "Age_year", 
                                                   "Free or Reduced-Price Lunch Eligibility" = "FRPL"))
                       )
                ),
                column(width = 12, align = "center",
                       box(width = NULL, height = "600px",
                           plotlyOutput("demo_plot", height = "550px")
                       )
                ))
      ),
      
      # Distributions Tab
      tabItem(tabName = "distributions",
              fluidRow(
                column(width = 12, align = "center",
                       box(width = NULL,
                           selectInput("dist_var", "Select Variable:", 
                                       choices = c("Average Daily MVPA (minutes)" = "MV_avg", 
                                                   "Average Daily Light-Intensity PA (minutes)" = "LP_avg", 
                                                   "Average Daily Sedentary Time (minutes)" = "SB_avg", 
                                                   "Average Daily Phone Screen Time (minutes)" = "avST", 
                                                   "Average Daily Nature Dose (minutes)" = "ND_avg", 
                                                   "Average Daily Nightly Sleep Duration (minutes)" = "avperiod"))
                       )
                ),
                column(width = 12, align = "center",
                       box(width = NULL, height = "600px",
                           plotlyOutput("dist_plot", height = "550px")
                       )
                ),
                fluidRow(
                  box(
                    width = 12,
                    textOutput("dist_description")
                  )
                ))
      ),
      
      # Relationships Tab
      tabItem(tabName = "relationships",
              fluidRow(
                column(width = 12, align = "center",
                       box(width = NULL,
                           div(style = "display: flex; justify-content: center; gap: 20px;",
                               selectInput("x_var", "Select X Variable:", 
                                           choices = c("Average Daily MVPA (minutes)" = "MV_avg", 
                                                       "Average Daily Light-Intensity PA (minutes)" = "LP_avg", 
                                                       "Average Daily Sedentary Time (minutes)" = "SB_avg", 
                                                       "Average Daily Phone Screen Time (minutes)" = "avST", 
                                                       "Average Daily Nature Dose (minutes)" = "ND_avg")),
                               selectInput("y_var", "Select Y Variable:", 
                                           choices = c("Average Daily MVPA (minutes)" = "MV_avg", 
                                                       "Average Daily Light-Intensity PA (minutes)" = "LP_avg", 
                                                       "Average Daily Sedentary Time (minutes)" = "SB_avg", 
                                                       "Average Daily Phone Screen Time (minutes)" = "avST", 
                                                       "Average Daily Nature Dose (minutes)" = "ND_avg", 
                                                       "Average Nightly Sleep Duration (minutes)" = "avperiod"))
                           )
                       )
                ),
                column(width = 12, align = "center",
                       box(width = NULL, height = "600px",
                           plotlyOutput("scatter_plot", height = "550px")
                       )
                ),
                fluidRow(
                  box(
                    width = 12,
                    textOutput("rel_description")
                  )
                ))
      ),
      
      # Predict Your Sleep Tab
      tabItem(tabName = "predict_sleep",
              fluidRow(
                box(
                  width = 12,
                  h4("Predict Your Sleep Duration"),
                  p("Use the sliders below to input your daily activity levels and see the predicted sleep duration based on our regression model.")
                )
              ),
              fluidRow(
                box(
                  width = 6,
                  sliderInput("pred_mv", "Daily MVPA (minutes):",
                              min = 0, 
                              max = slider_ranges$mv$max,
                              value = slider_ranges$mv$value),
                  sliderInput("pred_lp", "Daily Light-Intensity PA (minutes):",
                              min = 0, 
                              max = slider_ranges$lp$max,
                              value = slider_ranges$lp$value),
                  sliderInput("pred_sb", "Daily Sedentary Time (minutes):",
                              min = 0, 
                              max = slider_ranges$sb$max,
                              value = slider_ranges$sb$value),
                  sliderInput("pred_st", "Daily Screen Time (minutes):",
                              min = 0, 
                              max = slider_ranges$st$max,
                              value = slider_ranges$st$value)
                ),
                box(
                  width = 6,
                  plotlyOutput("prediction_plot"),
                  textOutput("prediction_text"),
                  tags$br(),
                  textOutput("model_interpretation")
                )
              )
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Variable Descriptions Table
  output$variable_table <- renderDataTable({
    variable_data <- data.frame(
      "Variable Name" = c("MVPA", "LPA", "SB", "Avperiod", "ND", "ST", "Age", "Gender", "Race", "FRPL"),
      "Description" = c(
        "AKA: Moderate-to-Vigorous Intensity Physical Activity Minutes. MVPA is achieved through activities like participating in team sports, going on a moderate-difficult hike or run, vigorous dancing, and related activities.",
        "AKA: Light-intensity physical activity. LPA includes activities like going on a light walk, doing light housework, or playing games like pool.",
        "AKA: Sedentary Behavior. SB includes activities that involve sitting, reclining, or lying down with very low energy expenditure.",
        "AKA: Nighttime sleep period. These were specified as any sleep period starting between the hours of 7pm and 4am.",
        "AKA: NatureDose. ND minutes are a count of the amount of time one spent exposed to natural elements that are shown to be beneficial to health and wellbeing (e.g., parks with green spaces, natural water sources, tree canopy, etc.).",
        "AKA: Phone-based screen time minutes. The total amount of time one spent on their phone.",
        "The age of the participant(s), ranging from 12-17 years of age.",
        "The gender identity of participants (Boy, Girl, Transgender, Agender)",
        "Race/Ethnicity of the participants (Non-Hispanic White only and All Others)",
        "AKA: Free-or-reduced Priced Lunch Status (Yes, No, I Don't Know)."
      ),
      "Measured By" = c("Accelerometer", "Accelerometer", "Accelerometer", "Accelerometer", "NatureDose Phone Application", "Phone internal monitoring tools reported via survey.", "Baseline Survey Question", "Baseline Survey Question", "Baseline Survey Question", "Baseline Survey Question")
    )
    datatable(variable_data, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # Distributions Plot
  output$dist_plot <- renderPlotly({
    req(input$dist_var)
    
    var_label <- variable_labels[[input$dist_var]]
    
    # Calculate normal distribution parameters
    mean_val <- mean(data[[input$dist_var]], na.rm = TRUE)
    sd_val <- sd(data[[input$dist_var]], na.rm = TRUE)
    
    # Calculate the histogram first to get the scaling right
    hist_data <- hist(data[[input$dist_var]], plot = FALSE, breaks = seq(
      0,
      max(data[[input$dist_var]], na.rm = TRUE),
      length.out = 30
    ))
    max_count <- max(hist_data$counts)
    
    # Create data for theoretical normal distribution
    x_range <- seq(0, mean_val + 4*sd_val, length.out = 100)
    normal_data <- data.frame(
      x = x_range,
      y = dnorm(x_range, mean = mean_val, sd = sd_val) * 
        max_count / dnorm(mean_val, mean = mean_val, sd = sd_val)
    )
    
    p <- ggplot() +
      geom_histogram(data = data, 
                     aes_string(x = input$dist_var, y = "..count.."),
                     binwidth = 10, fill = uo_colors$green, 
                     color = "black", alpha = 0.7) +
      geom_line(data = normal_data, 
                aes(x = x, y = y, 
                    text = "This curve shows a theoretical normal distribution"),
                color = uo_colors$yellow, size = 1.2) +
      scale_x_continuous(limits = c(0, NA)) +
      labs(
        title = stringr::str_wrap(paste("Distribution of", var_label), width = 40), 
        x = stringr::str_wrap(var_label, width = 40), 
        y = "Count"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
        axis.title.x = element_text(size = 10, margin = margin(t = 10)),
        axis.title.y = element_text(size = 10, margin = margin(r = 10)),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
      )
    
    ggplotly(p) %>% 
      layout(showlegend = FALSE)
  })
  
  # Distribution Description
  output$dist_description <- renderText({
    req(input$dist_var)
    
    var_label <- variable_labels[[input$dist_var]]
    mean_value <- round(mean(data[[input$dist_var]], na.rm = TRUE), 2)
    min_value <- round(min(data[[input$dist_var]], na.rm = TRUE), 2)
    max_value <- round(max(data[[input$dist_var]], na.rm = TRUE), 2)
    
    paste("In this sample of 326 physically active adolescents, the average daily", var_label, "was", mean_value, 
          "minutes, with the lowest daily average being", min_value, "minutes and the highest daily average being", max_value, "minutes.")
  })
  
  # Demographics Plot
  output$demo_plot <- renderPlotly({
    req(input$demo_var)
    
    demo_label <- variable_labels[[input$demo_var]]
    
    p <- data %>%
      count(!!sym(input$demo_var)) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      ggplot(aes_string(x = input$demo_var, y = "n")) +
      geom_bar(stat = "identity", fill = uo_colors$legacy_green, color = "black", alpha = 0.7) +
      geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                position = position_stack(vjust = 0.5),
                color = "white", size = 4) +
      labs(
        title = stringr::str_wrap(paste("Distribution of", demo_label), width = 40),
        x = stringr::str_wrap(demo_label, width = 40),
        y = "Count"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
        axis.title.x = element_text(size = 10, margin = margin(t = 10)),
        axis.title.y = element_text(size = 10, margin = margin(r = 10)),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p) %>% 
      layout(showlegend = FALSE)
  })
  
  # Scatter Plot
  output$scatter_plot <- renderPlotly({
    req(input$x_var, input$y_var)
    
    x_label <- variable_labels[[input$x_var]]
    y_label <- variable_labels[[input$y_var]]
    
    p <- ggplot(data, aes_string(x = input$x_var, y = input$y_var)) +
      geom_point(color = uo_colors$yellow, alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, color = uo_colors$lime_green, alpha = 0.5) +
      labs(
        title = stringr::str_wrap(paste("Relationship between", y_label, "and", x_label), width = 40),
        x = stringr::str_wrap(x_label, width = 40),
        y = stringr::str_wrap(y_label, width = 40)
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
        axis.title.x = element_text(size = 10, margin = margin(t = 10)),
        axis.title.y = element_text(size = 10, margin = margin(r = 10)),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
      )
    
    ggplotly(p)
  })
  
  # Relationship Description
  output$rel_description <- renderText({
    req(input$x_var, input$y_var)
    
    x_label <- variable_labels[[input$x_var]]
    y_label <- variable_labels[[input$y_var]]
    
    correlation <- cor(data[[input$x_var]], data[[input$y_var]], use = "complete.obs")
    correlation <- round(correlation, 2)
    n <- sum(!is.na(data[[input$x_var]]) & !is.na(data[[input$y_var]]))
    t_value <- abs(correlation) * sqrt((n - 2) / (1 - correlation^2))
    p_value <- 2 * pt(-t_value, df = n - 2)
    significant <- ifelse(p_value <= 0.05, "significant", "not significant")
    magnitude <- ifelse(abs(correlation) < 0.3, "weak", 
                        ifelse(abs(correlation) < 0.7, "moderate", "strong"))
    direction <- ifelse(correlation > 0, "positive", "negative")
    
    paste("This scatterplot shows the relationship between", y_label, "and", x_label, 
          "for the sample of 326 physically active adolescents. The association between these variables has a", 
          magnitude, direction, "association (r =", correlation, "). This association is", significant, 
          "at the p < 0.05 level.")
  })
  
  # Sleep Prediction Model
  sleep_model <- reactive({
    lm(avperiod ~ MV_avg + LP_avg + SB_avg + avST, data = data)
  })
  
  # Generate prediction plot
  output$prediction_plot <- renderPlotly({
    req(input$pred_mv, input$pred_lp, input$pred_sb, input$pred_st)
    
    model <- sleep_model()
    
    # Create prediction data
    new_data <- data.frame(
      MV_avg = input$pred_mv,
      LP_avg = input$pred_lp,
      SB_avg = input$pred_sb,
      avST = input$pred_st
    )
    
    # Get prediction and confidence interval
    pred <- predict(model, newdata = new_data, interval = "confidence")
    
    # Create bar plot
    p <- plot_ly() %>%
      add_bars(x = "Predicted Sleep", y = pred[1],
               marker = list(color = uo_colors$legacy_green),
               error_y = list(
                 type = "data",
                 array = pred[3] - pred[1],
                 arrayminus = pred[1] - pred[2],
                 color = "#000000"
               )) %>%
      layout(
        yaxis = list(title = "Minutes of Sleep",
                     range = c(0, max(data$avperiod))),
        showlegend = FALSE
      )
    
    p
  })
  
  # Generate prediction text
  output$prediction_text <- renderText({
    req(input$pred_mv, input$pred_lp, input$pred_sb, input$pred_st)
    
    model <- sleep_model()
    
    # Create prediction data
    new_data <- data.frame(
      MV_avg = input$pred_mv,
      LP_avg = input$pred_lp,
      SB_avg = input$pred_sb,
      avST = input$pred_st
    )
    
    # Get prediction and confidence interval
    pred <- predict(model, newdata = new_data, interval = "confidence")
    
    paste0("Predicted sleep duration: ", round(pred[1]), " minutes (",
           round(pred[1]/60, 1), " hours)\n",
           "95% Confidence Interval: ", round(pred[2]), "-", round(pred[3]),
           " minutes")
  })
  
  # Generate model interpretation
  output$model_interpretation <- renderText({
    model <- sleep_model()
    coef <- coef(model)
    r2 <- summary(model)$r.squared
    
    interpretation <- paste0(
      "Model Interpretation:\n",
      "This prediction is based on a multiple regression model (R² = ", round(r2, 3), "). ",
      "For every additional minute of:\n",
      "- MVPA: ", round(coef["MV_avg"], 2), " minute change in sleep\n",
      "- Light PA: ", round(coef["LP_avg"], 2), " minute change in sleep\n",
      "- Sedentary time: ", round(coef["SB_avg"], 2), " minute change in sleep\n",
      "- Screen time: ", round(coef["avST"], 2), " minute change in sleep"
    )
    
    interpretation
  })
}

# Run the application 
shinyApp(ui = ui, server = server)