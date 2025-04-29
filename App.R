library(bs4Dash)
library(meta)
library(metafor)
library(dplyr)
library(httr)
library(jsonlite)
library(dmetar)  # For additional diagnostics

# ---------------------------
# Helper Functions
# ---------------------------
generate_content <- function(prompt, api_key) {
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent?key=", api_key)
  body <- list(contents = list(list(parts = list(list(text = prompt)))))
  res <- POST(url,
              body = toJSON(body, auto_unbox = TRUE),
              encode = "json",
              add_headers(`Content-Type` = "application/json"))
  parsed <- fromJSON(content(res, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
  generated <- ""
  if (!is.null(parsed$candidates) && length(parsed$candidates) > 0) {
    candidate <- parsed$candidates[[1]]
    if (!is.null(candidate$content) &&
        !is.null(candidate$content$parts) &&
        length(candidate$content$parts) > 0 &&
        !is.null(candidate$content$parts[[1]]$text)) {
      generated <- candidate$content$parts[[1]]$text
    }
  }
  list(generated = generated)
}

clean_markdown <- function(text) {
  text <- gsub("\\*\\*", "", text)
  text <- gsub("\\*", "", text)
  text <- gsub("#", "", text)
  text
}

# ---------------------------
# UI
# ---------------------------
ui <- bs4DashPage(
  title = "786MIII Meta-Analysis Text and LLM Integration",
  header = bs4DashNavbar(title = "786MIII Meta-Analysis Text & LLM Integration"),
  sidebar = bs4DashSidebar(
    sidebarMenu(
      menuItem("Inputs", tabName = "inputs", icon = icon("upload")),
      menuItem("Meta-Analysis Text", tabName = "meta_text", icon = icon("file-alt"))
    )
  ),
  body = bs4DashBody(
    tabItems(
      # Inputs Tab
      tabItem(
        tabName = "inputs",
        box(
          title = "Data Input & Options",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          p("Upload a CSV file with columns: 
            meanintervention, sdintervention, totalintervention, 
            meancontrol, sdcontrol, totalcontrol, author, 
            Reg, Reg2, Reg3, subgroup."),
          fileInput("fileInput", "Upload CSV File", accept = c("text/csv", ".csv")),
          numericInput("defaultInterventionN", "Default Intervention Sample Size (if not provided in CSV)", value = 100, min = 1),
          radioButtons("effectMeasure", "Effect Measure:", choices = c("SMD" = "SMD", "MD" = "MD"), selected = "MD"),
          selectInput("method.smd", "Combination Method:", 
                      choices = c("Hedges" = "Hedges", "Glass" = "Glass", "Cohen" = "Cohen"), selected = "Hedges"),
          selectInput("method.tau", "Heterogeneity Method:", 
                      choices = c("DL" = "DL", "REML" = "REML", "PM" = "PM", "EB" = "EB", "SJ" = "SJ"), selected = "PM"),
          selectInput("effectModel", "Effect Model:", 
                      choices = c("Random" = "RE", "Fixed" = "FE"), selected = "RE")
        ),
        box(
          title = "Download Sample CSV File",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          p("Download a sample CSV file containing all required columns: 
            meanintervention, sdintervention, totalintervention, 
            meancontrol, sdcontrol, totalcontrol, author, 
            Reg, Reg2, Reg3, subgroup."),
          fluidRow(
            column(12, downloadButton("download_sample_all", "Download Sample CSV"))
          )
        )
      ),
      # Meta-Analysis Text Tab
      tabItem(
        tabName = "meta_text",
        fluidRow(
          box(
            title = "Expanded Meta-Analysis Text Results",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            tabBox(
              width = 12,
              tabPanel("Heterogeneity", htmlOutput("heterogeneityTextOutput")),
              tabPanel("Leave-One-Out", htmlOutput("leaveOneOutTextOutput")),
              tabPanel("Publication Bias", htmlOutput("publicationBiasTextOutput")),
              tabPanel("Meta-Regression", htmlOutput("metaRegressionTextOutput")),
              tabPanel("Subgroup Analysis", htmlOutput("subgroupAnalysisTextOutput")),
              tabPanel("Cumulative Meta-Analysis", htmlOutput("cumulativeMetaTextOutput")),
              tabPanel("Bayesian Meta-Analysis", htmlOutput("bayesianMetaTextOutput")),
              tabPanel("Trim and Fill", htmlOutput("trimFillTextOutput")),
              tabPanel("Outlier Detection", htmlOutput("outliersTextOutput")),
              tabPanel("P-Curve Analysis", htmlOutput("pcurveTextOutput")),
              tabPanel("Overall Summary", htmlOutput("overallSummaryTextOutput")),
              tabPanel("Study Characteristics", htmlOutput("studyCharTextOutput")),
              tabPanel("Study Weights", htmlOutput("studyWeightsTextOutput")),
              tabPanel("Fixed vs Random Comparison", htmlOutput("fixedRandomTextOutput")),
              tabPanel("Aggregated Raw", htmlOutput("rawMetaText")),
              tabPanel("LLM Cochrane", htmlOutput("llmCochraneText")),
              tabPanel("LLM NEJM", htmlOutput("llmNEJMText")),
              tabPanel("LLM Lancet", htmlOutput("llmLancetText")),
              tabPanel("LLM Plain", htmlOutput("llmPlainText"))
            )
          )
        )
      )
    )
  ),
  footer = bs4DashFooter("Copyright © 2025")
)

# ---------------------------
# Server
# ---------------------------
server <- function(input, output, session) {
  
  # Read CSV file
  dataInput <- reactive({
    req(input$fileInput)
    dat <- read.csv(input$fileInput$datapath)
    # If the CSV does not include the intervention sample sizes, add a default column
    if (!"totalintervention" %in% names(dat)) {
      dat$totalintervention <- input$defaultInterventionN
    }
    dat
  })
  
  metaResult <- reactive({
    req(dataInput())
    dat <- dataInput()
    metacont(n.e = dat$totalintervention,
             mean.e = dat$meanintervention,
             sd.e = dat$sdintervention,
             n.c = dat$totalcontrol,
             mean.c = dat$meancontrol,
             sd.c = dat$sdcontrol,
             studlab = dat$author,
             data = dat,
             sm = input$effectMeasure,
             method.smd = input$method.smd,
             method.tau = input$method.tau,
             common = (input$effectModel == "FE"),
             random = (input$effectModel == "RE"))
  })
  
  # ---------------------------
  # Download Handler for Sample CSV File
  # ---------------------------
  sample_basic <- data.frame(
    meanintervention = c(10, 12, 15, 8, 11),
    sdintervention   = c(2, 2.5, 3, 2, 2.2),
    totalintervention = c(50, 60, 55, 48, 52),
    meancontrol      = c(9, 11, 14, 7, 10),
    sdcontrol        = c(2, 2.5, 3, 2, 2.2),
    totalcontrol     = c(50, 60, 55, 48, 52),
    author           = c("Study1", "Study2", "Study3", "Study4", "Study5")
  )
  
  sample_all <- sample_basic %>%
    mutate(Reg = c(1, 2, 3, 4, 5),
           Reg2 = c(5, 4, 3, 2, 1),
           Reg3 = c(2, 3, 2, 3, 2),
           subgroup = c("A", "B", "A", "B", "A"))
  
  output$download_sample_all <- downloadHandler(
    filename = function() {
      paste("sample_all_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(sample_all, file, row.names = FALSE)
    }
  )
  
  # ---------------------------
  # Expanded Meta-Analysis Text Outputs
  # ---------------------------
  heterogeneityText <- reactive({
    req(metaResult())
    m <- metaResult()
    if (is.null(m$Q) || !is.finite(m$Q) || is.null(m$I2)) {
      return("Heterogeneity information is not available.")
    }
    k <- m$k
    I2_val <- round(as.numeric(m$I2), 1)
    Q_val <- round(as.numeric(m$Q), 2)
    p_Q <- round(as.numeric(m$pval.Q), 4)
    level <- if(I2_val < 30) "low" else if(I2_val < 60) "moderate" else if(I2_val < 75) "substantial" else "considerable"
    paste0("Heterogeneity Summary\n\n",
           "Number of studies: ", k, "\n",
           "I²: ", I2_val, "%\n",
           "Q-statistic: ", Q_val, " (p = ", p_Q, ")\n",
           "This indicates ", level, " heterogeneity overall.")
  })
  
  leaveOneOutText <- reactive({
    req(metaResult())
    m <- metaResult()
    paste(capture.output(print(metainf(m))), collapse = "\n")
  })
  
  publicationBiasText <- reactive({
    req(metaResult())
    m <- metaResult()
    pb <- metabias(m, method.bias = "linreg")
    paste(capture.output(print(summary(pb))), collapse = "\n")
  })
  
  metaRegressionText <- reactive({
    req(dataInput())
    dat <- dataInput()
    if(all(c("Reg", "Reg2", "Reg3") %in% names(dat))){
      dat2 <- escalc(measure = input$effectMeasure,
                     m1i = dat$meanintervention,
                     sd1i = dat$sdintervention,
                     n1i = dat$totalintervention,
                     m2i = dat$meancontrol,
                     sd2i = dat$sdcontrol,
                     n2i = dat$totalcontrol,
                     data = dat,
                     slab = dat$author)
      m_reg <- rma(yi = dat2$yi, vi = dat2$vi, mods = ~ Reg + Reg2 + Reg3, data = dat2, method = "REML")
      paste(capture.output(print(summary(m_reg))), collapse = "\n")
    } else {
      "Meta-regression data not available."
    }
  })
  
  subgroupAnalysisText <- reactive({
    req(dataInput())
    dat <- dataInput()
    if("subgroup" %in% names(dat)){
      m_sub <- metacont(n.e = dat$totalintervention,
                        mean.e = dat$meanintervention,
                        sd.e = dat$sdintervention,
                        n.c = dat$totalcontrol,
                        mean.c = dat$meancontrol,
                        sd.c = dat$sdcontrol,
                        studlab = dat$author,
                        data = dat,
                        sm = input$effectMeasure,
                        method.smd = input$method.smd,
                        method.tau = input$method.tau,
                        common = (input$effectModel == "FE"),
                        random = (input$effectModel == "RE"),
                        subgroup = dat$subgroup)
      paste(capture.output(print(summary(m_sub))), collapse = "\n")
    } else {
      "Subgroup analysis data not available."
    }
  })
  
  cumulativeMetaText <- reactive({
    req(metaResult())
    m <- metaResult()
    paste(capture.output(print(metacum(m))), collapse = "\n")
  })
  
  bayesianMetaText <- reactive({
    req(dataInput())
    dat <- dataInput()
    if(nrow(dat) < 2) return("At least 2 studies are required for Bayesian meta-analysis.")
    if(!requireNamespace("bayesmeta", quietly = TRUE)){
      return("Bayesian meta-analysis function not available.")
    }
    dat2 <- escalc(measure = input$effectMeasure,
                   m1i = dat$meanintervention,
                   sd1i = dat$sdintervention,
                   n1i = dat$totalintervention,
                   m2i = dat$meancontrol,
                   sd2i = dat$sdcontrol,
                   n2i = dat$totalcontrol,
                   data = dat,
                   slab = dat$author)
    sigma <- sqrt(dat2$vi)
    bmeta <- bayesmeta::bayesmeta(y = dat2$yi, sigma = sigma)
    paste(capture.output(print(summary(bmeta))), collapse = "\n")
  })
  
  trimFillText <- reactive({
    req(metaResult())
    m <- metaResult()
    paste(capture.output(print(summary(trimfill(m)))), collapse = "\n")
  })
  
  outliersText <- reactive({
    req(metaResult())
    m <- metaResult()
    out <- tryCatch(dmetar::find.outliers(m), error = function(e) "Outlier detection not available.")
    if(is.character(out)) return(out)
    paste("Outlier Studies:", paste(out, collapse = ", "))
  })
  
  pcurveText <- reactive({
    req(metaResult())
    m <- metaResult()
    pc <- tryCatch(dmetar::pcurve(m), error = function(e) "P-curve analysis not available.")
    paste(capture.output(print(pc)), collapse = "\n")
  })
  
  overallSummaryText <- reactive({
    req(metaResult())
    paste(capture.output(print(summary(metaResult()))), collapse = "\n")
  })
  
  studyCharText <- reactive({
    req(dataInput())
    dat <- dataInput()
    paste("Study Characteristics:\n", paste(capture.output(print(dat)), collapse = "\n"))
  })
  
  studyWeightsText <- reactive({
    req(metaResult())
    m <- metaResult()
    if (!is.null(m$w.random)) {
      paste("Study Weights (Random):", paste(names(m$w.random), round(m$w.random, 2), collapse = "\n"))
    } else {
      "Study weights not available."
    }
  })
  
  fixedRandomText <- reactive({
    req(dataInput())
    dat <- dataInput()
    fixed_model <- metacont(n.e = dat$totalintervention,
                            mean.e = dat$meanintervention,
                            sd.e = dat$sdintervention,
                            n.c = dat$totalcontrol,
                            mean.c = dat$meancontrol,
                            sd.c = dat$sdcontrol,
                            studlab = dat$author,
                            data = dat,
                            sm = input$effectMeasure,
                            method.smd = input$method.smd,
                            method.tau = input$method.tau,
                            common = TRUE,
                            random = FALSE)
    random_model <- metaResult()
    txt_fixed <- paste("Fixed Effect Estimate:", round(as.numeric(fixed_model$TE.fixed), 2),
                       "(95% CI:", round(as.numeric(fixed_model$lower.fixed), 2), "to", round(as.numeric(fixed_model$upper.fixed), 2), ")")
    txt_random <- paste("Random Effect Estimate:", round(as.numeric(random_model$TE.random), 2),
                        "(95% CI:", round(as.numeric(random_model$lower.random), 2), "to", round(as.numeric(random_model$upper.random), 2), ")")
    paste("Fixed vs Random Effects Comparison:\n\n", txt_fixed, "\n", txt_random)
  })
  
  allText <- reactive({
    paste(
      heterogeneityText(),
      "\nLeave-One-Out Analysis:\n", leaveOneOutText(),
      "\nPublication Bias:\n", publicationBiasText(),
      "\nMeta-Regression:\n", metaRegressionText(),
      "\nSubgroup Analysis:\n", subgroupAnalysisText(),
      "\nCumulative Meta-Analysis:\n", cumulativeMetaText(),
      "\nBayesian Meta-Analysis:\n", bayesianMetaText(),
      "\nTrim and Fill Analysis:\n", trimFillText(),
      "\nOutlier Detection:\n", outliersText(),
      "\nP-Curve Analysis:\n", pcurveText(),
      "\nOverall Meta-Analysis Summary:\n", overallSummaryText(),
      "\nStudy Characteristics:\n", studyCharText(),
      "\nStudy Weights:\n", studyWeightsText(),
      "\nFixed vs Random Effects Comparison:\n", fixedRandomText(),
      sep = "\n\n"
    )
  })
  
  # ---------------------------
  # Render UI Outputs
  # ---------------------------
  output$heterogeneityTextOutput <- renderUI({ tags$div(style = "text-align: justify; white-space: pre-wrap;", heterogeneityText()) })
  output$leaveOneOutTextOutput <- renderUI({ tags$div(style = "text-align: justify; white-space: pre-wrap;", leaveOneOutText()) })
  output$publicationBiasTextOutput <- renderUI({ tags$div(style = "text-align: justify; white-space: pre-wrap;", publicationBiasText()) })
  output$metaRegressionTextOutput <- renderUI({ tags$div(style = "text-align: justify; white-space: pre-wrap;", metaRegressionText()) })
  output$subgroupAnalysisTextOutput <- renderUI({ tags$div(style = "text-align: justify; white-space: pre-wrap;", subgroupAnalysisText()) })
  output$cumulativeMetaTextOutput <- renderUI({ tags$div(style = "text-align: justify; white-space: pre-wrap;", cumulativeMetaText()) })
  output$bayesianMetaTextOutput <- renderUI({ tags$div(style = "text-align: justify; white-space: pre-wrap;", bayesianMetaText()) })
  output$trimFillTextOutput <- renderUI({ tags$div(style = "text-align: justify; white-space: pre-wrap;", trimFillText()) })
  output$outliersTextOutput <- renderUI({ tags$div(style = "text-align: justify; white-space: pre-wrap;", outliersText()) })
  output$pcurveTextOutput <- renderUI({ tags$div(style = "text-align: justify; white-space: pre-wrap;", pcurveText()) })
  output$overallSummaryTextOutput <- renderUI({ tags$div(style = "text-align: justify; white-space: pre-wrap;", overallSummaryText()) })
  output$studyCharTextOutput <- renderUI({ tags$div(style = "text-align: justify; white-space: pre-wrap;", studyCharText()) })
  output$studyWeightsTextOutput <- renderUI({ tags$div(style = "text-align: justify; white-space: pre-wrap;", studyWeightsText()) })
  output$fixedRandomTextOutput <- renderUI({ tags$div(style = "text-align: justify; white-space: pre-wrap;", fixedRandomText()) })
  output$rawMetaText <- renderUI({ tags$div(style = "text-align: justify; white-space: pre-wrap;", allText()) })
  
  # ---------------------------
  # LLM Integration for Converted Text (4 styles)
  # ---------------------------
  llmResults <- reactiveValues(cochrane = "", nejm = "", lancet = "", plain = "")
  
  observe({
    req(allText())
    originalText <- allText()
    api_keys <- c("AIzaSyDo6kED06Epfl4zZ-jrMGK0VRTlWc1AGxg",
                  "AIzaSyCwKB0hETywXjhB9aXNPw0dBSjO8kWPgU0",
                  "AIzaSyCmeHvCXwls2Nixhg8g2ALGlJ6ce80SOXw",
                  "AIzaSyAydG28jErd8P2JJ76nXtDVWk-OXcFXR6c",
                  "AIzaSyDC6Y4ZyvuesG4K0Qmht7GhAZ5RLx-d5V4",
                  "AIzaSyAdv7S9Mmc960IsPfsbWvy6UPB11Zf43_o")
    
    prompt_cochrane <- paste(
      "Rewrite the following meta-analysis results into a detailed, well-structured Cochrane-style results section. ",
      "Include clear headings: Introduction, Methods, Results, Discussion, and Conclusion with proper line breaks. ",
      "The output should be at least 350 words long with a detailed explanation of methodology, statistical results, and clinical implications. ",
      "Output only the structured results section without extra metadata.\n\nOriginal Meta-Analysis Text:\n", 
      originalText
    )
    key_cochrane <- sample(api_keys, 1)
    res_cochrane <- generate_content(prompt_cochrane, key_cochrane)
    llmResults$cochrane <- if(nzchar(res_cochrane$generated)){
      clean_markdown(res_cochrane$generated)
    } else "No LLM-generated content received for Cochrane style."
    
    prompt_nejm <- paste(
      "Rewrite the following meta-analysis results into a detailed, formal NEJM-style description. ",
      "Include a succinct summary, detailed statistical results, and clinical implications. ",
      "The output should be at least 350 words long. Output only the structured results section without extra metadata.\n\nOriginal Meta-Analysis Text:\n", 
      originalText
    )
    key_nejm <- sample(api_keys, 1)
    res_nejm <- generate_content(prompt_nejm, key_nejm)
    llmResults$nejm <- if(nzchar(res_nejm$generated)){
      clean_markdown(res_nejm$generated)
    } else "No LLM-generated content received for NEJM style."
    
    prompt_lancet <- paste(
      "Rewrite the following meta-analysis results into a detailed, well-structured Lancet-style results section. ",
      "Include clear headings, detailed analysis, and clinical implications. ",
      "The output should be at least 350 words long. Output only the structured results section without extra metadata.\n\nOriginal Meta-Analysis Text:\n", 
      originalText
    )
    key_lancet <- sample(api_keys, 1)
    res_lancet <- generate_content(prompt_lancet, key_lancet)
    llmResults$lancet <- if(nzchar(res_lancet$generated)){
      clean_markdown(res_lancet$generated)
    } else "No LLM-generated content received for Lancet style."
    
    prompt_plain <- paste(
      "Rewrite the following meta-analysis results into a detailed plain language summary. ",
      "The output must be easy to understand, avoid jargon, and be at least 350 words long, covering methodology, statistical details, and clinical implications. ",
      "Output only the plain language summary without extra metadata.\n\nOriginal Meta-Analysis Text:\n", 
      originalText
    )
    key_plain <- sample(api_keys, 1)
    res_plain <- generate_content(prompt_plain, key_plain)
    llmResults$plain <- if(nzchar(res_plain$generated)){
      clean_markdown(res_plain$generated)
    } else "No LLM-generated content received for Plain language style."
  })
  
  output$llmCochraneText <- renderUI({ tags$div(style = "text-align: justify; white-space: pre-wrap;", llmResults$cochrane) })
  output$llmNEJMText <- renderUI({ tags$div(style = "text-align: justify; white-space: pre-wrap;", llmResults$nejm) })
  output$llmLancetText <- renderUI({ tags$div(style = "text-align: justify; white-space: pre-wrap;", llmResults$lancet) })
  output$llmPlainText <- renderUI({ tags$div(style = "text-align: justify; white-space: pre-wrap;", llmResults$plain) })
  
}

shinyApp(ui, server)
