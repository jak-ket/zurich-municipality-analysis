library(shiny)
library(RColorBrewer)
library(shinydashboard)
library(shinyjs)

department_plots <- list(
  "Allgemeine Verwaltung" = "images/Allgemeine_Verwaltung.png",
  "Öff. Ordnung & Sicherheit" = "images/offentliche_ordnung.png",
  "Kultur, Sport und Freizeit" = "images/kultur_sport_frei.png",
  "Gesundheit" = "images/gesundheit.png",
  "Soziale Sicherheit" = "images/soziale_sicherheit.png",
  "Verkehr und Nachrichtenübermittlung" = "images/verkehr_nachrichten.png",
  "Umweltschutz und Raumordnung" = "images/umwelt_raumordnung.png",
  "Finanzen und Steuern" = "images/finanzen_steuern.png"
)

corrected_department_names <- list(
  "Allgemeine Verwaltung" = "Allgemeine_Verwaltung",
  "Öff. Ordnung & Sicherheit" = "Ordnung_Sicherheit",
  "Kultur, Sport und Freizeit" = "KultSportFrei",
  "Gesundheit" = "Gesundheit",
  "Soziale Sicherheit" = "Soziale_Sicherheit",
  "Verkehr und Nachrichtenübermittlung" = "VerkehrNachrichten",
  "Umweltschutz und Raumordnung" = "Umwelt_Raumordnung",
  "Finanzen und Steuern" = "FinanzenSteuern"
)

file_names <- c("Allgemeine_Verwaltung.csv", "FinanzenSteuern.csv", "Gesundheit.csv",
                "KultSportFrei.csv", "Ordnung_Sicherheit.csv", "Soziale Sicherheit.csv",
                "Umwelt_Raumordnung.csv", "VerkehrNachrichten.csv", "Volkswirtschaft.csv")

file_names_base <- gsub(".csv", "", file_names)
file_names_base_comparison <- setdiff(file_names_base, "Volkswirtschaft")

directory <- "./csv_files2/"

data_frames <- list()


# READ FTE AND STAFF COSTS DATA
for (file_name in file_names_base) {
  file_path <- paste0(directory, file_name, ".csv")
  file_path_FTE <- paste0(directory, file_name, "_FTE.csv")
  
  df <- read.csv(file_path, sep = ",", header = TRUE)
  df2 <- read.csv(file_path_FTE, sep =",", header = TRUE)
  
  df$LOG.STAFF.COSTS <- exp(df$LOG.STAFF.COSTS)
  df$pred_LOG.STAFF.COSTS <- exp(df$pred_LOG.STAFF.COSTS)
  
  # Exclude the "STAFF_COSTS" column
  df2 <- df2[, c("GEMEINDE", "pred_fte", "YEAR")]
  colnames(df2) <- c("Gemeinde", "Erwartete FTE's", "YEAR")
  
  df$Efficiency <- (df$LOG.STAFF.COSTS - df$pred_LOG.STAFF.COSTS) / df$LOG.STAFF.COSTS
  
  df <- df[, !colnames(df) %in% c("X"), drop = FALSE]
  df[, "Department"] <- file_name
  df[, "Department"] <- gsub(".csv", "", df[, "Department"])
  
  colnames(df) <- c("YEAR", "Gemeinde", "Personalaufwand (CHF)", "Erwarteter Personalaufwand (CHF)", "Efficiency", "Department")
  
  total_df <- merge(df, df2, by = c("Gemeinde", "YEAR"), all = TRUE)
  
  data_frames[[file_name]] <- total_df
}
combined_data <- do.call(rbind, data_frames)

# READ FTE COMPARISON DATA 
comparison_data_frames <- list()

for (file_name in file_names_base_comparison) {
  file_path <- paste0(directory, file_name, "_Comparison.csv")
  df <- read.csv(file_path, sep = ",", header = TRUE)
  
  df$Relative_Abweichung <- (df$ftes - df$pred_fte) / df$ftes
  
  df <- df[, c("municipality", "year", "pred_fte", "ftes", "Relative_Abweichung")]
  colnames(df) <- c("Gemeinde", "YEAR", "Erwartete FTE's", "Tatsächliche FTE's", "Relative_Abweichung")
  
  df$Department <- gsub("_Comparison.csv", "", file_name)  # Correct department name for comparison
  comparison_data_frames[[file_name]] <- df
}
comparison_data <- do.call(rbind, comparison_data_frames)

# USE THIS FOR DEBUGGING PURPOSES
#print(head(subset(comparison_data, Department == "Allgemeine_Verwaltung")))

# Define UI 
#------------------------------------------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),  # use this for the password protection
  hidden(
    div(id = "main_content",
        titlePanel("Zürcher Gemeinden - Geschätze Personalaufwände"),
        fluidRow(
          column(8,
                 tags$p("Diese interaktive Webanwendung ermöglicht es Benutzern, geschätzte Personalaufwände und FTE-Prognosen für verschiedene Abteilungen und Gemeinden zu visualisieren und zu analysieren. 
                        Sie können verschiedene Metriken auswählen, um detaillierte Einblicke zu erhalten und die Effizienz der Abteilungen zu bewerten."),
                 tags$p("Erstellt am 23.04.2024", style="font-style:italic;")
          ),
          column(4,
                 img(src = "federas.png", align = "right", style="height: 50px")  
          )
        ),
        tabsetPanel(
          tabPanel("Gemeindedaten", sidebarLayout(
            sidebarPanel(
              selectInput("selectedYear", "Wählen Sie ein Jahr:", choices = unique(combined_data$YEAR)),
              selectInput("selectedGemeinde", "Wählen Sie eine Gemeinde:", choices = unique(combined_data$Gemeinde)),
              selectInput("selectedMetric", "Wählen Sie eine Metrik:",
                          choices = c("Personalaufwand (CHF)", "Erwarteter Personalaufwand (CHF)", "Erwartete FTE's"))
            ),
            mainPanel(
              tableOutput("gemeindeData"),
              plotOutput("gemeindeBarPlot")
            )
          )),
          tabPanel("Departmentsdaten", sidebarLayout(
            sidebarPanel(
              selectInput("selectedDept", "Wählen Sie eine Abteilung:", choices = names(department_plots))
            ),
            mainPanel(
              uiOutput("deptPlot")
            )
          )),
          tabPanel("FTE Vergleich", sidebarLayout(
            sidebarPanel(
              selectInput("fteSelectedYear", "Wählen Sie ein Jahr:", choices = unique(comparison_data$YEAR)),
              selectInput("fteSelectedDept", "Wählen Sie eine Abteilung:", choices = names(department_plots))
            ),
            mainPanel(
              tableOutput("fteComparisonData"),
              plotOutput("fteComparisonPlot")
            )
          ))
        )
    )
  )
)

# Define server logic
#------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  department_plots <- list(
    "Allgemeine Verwaltung" = "Allgemeine_Verwaltung.png",
    "Öff. Ordnung & Sicherheit" = "offentliche_ordnung.png",
    "Kultur, Sport und Freizeit" = "kultur_sport_frei.png",
    "Gesundheit" = "gesundheit.png",
    "Soziale Sicherheit" = "soziale_sicherheit.png",
    "Verkehr und Nachrichtenübermittlung" = "verkehr_nachrichten.png",
    "Umweltschutz und Raumordnung" = "umwelt_raumordnung.png",
    "Finanzen und Steuern" = "finanzen_steuern.png"
  )
  
  # Password Authentication, change password here
  observe({
    showModal(modalDialog(
      title = "Passwort erforderlich",
      textInput("password", "Geben Sie das Passwort ein:", ""),
      footer = tagList(
        actionButton("auth", "Einloggen")
      ),
      easyClose = FALSE
    ))
  })
  
  observeEvent(input$auth, {
    if (input$password == "ETH_Federas_2024") #change password here 
      {
      removeModal()
      show("main_content")
    } else {
      showModal(modalDialog(
        title = "Falsches Passwort",
        "Das eingegebene Passwort ist nicht korrekt. Bitte versuchen Sie es erneut.",
        easyClose = TRUE
      ))
    }
  })
  
  output$gemeindeData <- renderTable({
    req(input$selectedYear, input$selectedGemeinde)
    selected_data <- subset(combined_data, Gemeinde == input$selectedGemeinde & YEAR == input$selectedYear)
    if (nrow(selected_data) == 0) {
      return(data.frame(Notice = "No data available for the selected Gemeinde and Year"))
    }
    selected_data[, c("Department", "Personalaufwand (CHF)", "Erwarteter Personalaufwand (CHF)", "Erwartete FTE's", "Efficiency")]
  })
  
  output$gemeindeBarPlot <- renderPlot({
    req(input$selectedYear, input$selectedGemeinde, input$selectedMetric)
    selected_metric <- input$selectedMetric
    
    gemeinde_data <- subset(combined_data, Gemeinde == input$selectedGemeinde & YEAR == input$selectedYear)
    
    if (nrow(gemeinde_data) == 0) return(NULL)
    
    if (selected_metric %in% c("Personalaufwand (CHF)", "Erwarteter Personalaufwand (CHF)")) {
      metric_values <- log(gemeinde_data[[selected_metric]])
      y_label <- paste0("Log of ", selected_metric)
    } else {
      metric_values <- gemeinde_data[[selected_metric]]
      y_label <- selected_metric
    }
    
    department_names <- gemeinde_data$Department
    efficiency_scores <- gemeinde_data$Efficiency
    
    color_palette <- ifelse(efficiency_scores < 0, "#60d394", "#ff9b85")
    
    barplot(metric_values, names.arg = department_names, las = 2,
            cex.names = 0.7, main = paste(selected_metric, "für", input$selectedGemeinde, "im Jahr", input$selectedYear),
            ylab = y_label, col = color_palette, border = "white")
  })
  
  output$deptPlot <- renderUI({
    req(input$selectedDept)
    department <- input$selectedDept
    img(src = department_plots[[corrected_department_names[[department]]]], style = "width:50%;")
  })
  
  output$fteComparisonData <- renderTable({
    req(input$fteSelectedYear, input$fteSelectedDept)
    corrected_dept <- corrected_department_names[[input$fteSelectedDept]]
    selected_data <- subset(comparison_data, YEAR == input$fteSelectedYear & Department == corrected_dept)
    if (nrow(selected_data) == 0) {
      return(data.frame(Notice = "No data available for the selected Department and Year"))
    }
    #print(selected_data)  # use this for debugging purposes
    selected_data[, c("Gemeinde", "Erwartete FTE's", "Tatsächliche FTE's", "Relative_Abweichung")]
  })
  
  output$fteComparisonPlot <- renderPlot({
    req(input$fteSelectedYear, input$fteSelectedDept)
    corrected_dept <- corrected_department_names[[input$fteSelectedDept]]
    selected_data <- subset(comparison_data, YEAR == input$fteSelectedYear & Department == corrected_dept)
    
    if (nrow(selected_data) == 0) return(NULL)
    
    barplot_height <- cbind(selected_data$`Erwartete FTE's`, selected_data$`Tatsächliche FTE's`)
    barplot_labels <- selected_data$Gemeinde
    
    barplot(t(barplot_height), beside = TRUE, names.arg = barplot_labels, las = 2,
            cex.names = 0.7, col = c("blue", "red"), 
            legend.text = c("Erwartete FTE's", "Tatsächliche FTE's"),
            main = paste("FTE Vergleich für", input$fteSelectedDept, "im Jahr", input$fteSelectedYear),
            ylab = "FTEs")
  })
  
  observe({
    updateSelectInput(session, "selectedGemeinde", choices = unique(combined_data$Gemeinde))
  })
}

shinyApp(ui, server)
