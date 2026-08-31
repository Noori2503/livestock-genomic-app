# =========================================================
# mod_reports.R  —  Phase 1 (Herd Summary) + Phase 2 (CSV/Excel)
#                 + Phase 3a (Animal Herd Location)
# =========================================================
# Requires the `openxlsx` package for Excel export
# (install.packages("openxlsx") if not already installed).
# Fully standalone Shiny module — does not modify mod_analytics.R,
# global.R, app.R, or any other existing module. Runs its own
# queries against milk_yield_records / phenotypes / breeds / herds /
# farms / herd_type so it doesn't depend on any other module's
# internals.

# ---------------------------------------------------------
# UI
# ---------------------------------------------------------
mod_reports_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h2("Reports"),

    fluidRow(
      column(
        width = 4,
        selectInput(
          inputId = ns("report_type"),
          label   = "Report Type:",
          choices = c("Herd Summary" = "herd",
                      "Individual Animal" = "individual",
                      "Animal Herd Location" = "location")
        )
      ),
      column(
        width = 4,
        # Only shown when "Animal Herd Location" is selected
        conditionalPanel(
          condition = "input.report_type == 'location'",
          ns = ns,
          uiOutput(ns("animal_selector"))
        )
      ),
      column(
        width = 4,
        br(),
        actionButton(ns("generate_report"), "Generate Report",
                     class = "btn-primary")
      )
    ),

    uiOutput(ns("report_output"))
  )
}

# ---------------------------------------------------------
# SERVER
# ---------------------------------------------------------
# `records_data` is passed in as a reactive function from app.R.
mod_reports_server <- function(id, records_data) {
  moduleServer(id, function(input, output, session) {

    # ------------------------------------------------------
    # Animal dropdown for the Location report
    # ------------------------------------------------------
    output$animal_selector <- renderUI({
      req(records_data())
      selectInput(
        inputId = NS(id, "selected_animal_code"),
        label   = "Select Animal:",
        choices = records_data()$Animal_Code
      )
    })

    report_data <- eventReactive(input$generate_report, {

      if (input$report_type == "herd") {

        animals_df <- records_data()

        # --- Milk yield: same source as mod_analytics.R's Milk Yield chart ---
        milk_df <- tryCatch({
          dbGetQuery(db_pool, "
            SELECT
                a.animal_code AS \"Animal_Code\",
                (COALESCE(m.morning_yield, 0) + COALESCE(m.evening_yield, 0)) AS milk_yield
            FROM milk_yield_records m
            INNER JOIN lactation l ON m.lactation_id = l.lactation_id
            INNER JOIN animals a ON l.animal_id = a.animal_id
            WHERE m.status IS DISTINCT FROM 'Rejected'
          ")
        }, error = function(e) {
          message("Reports milk yield query error: ", conditionMessage(e))
          data.frame(Animal_Code = character(), milk_yield = numeric())
        })

        # --- Fat %: same source as mod_analytics.R's Fat % chart ---
        fat_df <- tryCatch({
          dbGetQuery(db_pool, "
            SELECT
                a.animal_code AS \"Animal_Code\",
                p.value AS fat_value
            FROM phenotypes p
            INNER JOIN traits t ON p.trait_id = t.trait_id
            INNER JOIN animals a ON p.animal_id = a.animal_id
            WHERE t.trait_id = 2
              AND p.status IS DISTINCT FROM 'Rejected'
          ")
        }, error = function(e) {
          message("Reports fat % query error: ", conditionMessage(e))
          data.frame(Animal_Code = character(), fat_value = numeric())
        })

        list(
          type = "herd",
          total_animals      = nrow(animals_df),
          total_milk_records = sum(!is.na(milk_df$milk_yield)),
          total_fat_records  = sum(!is.na(fat_df$fat_value)),
          avg_milk_yield     = round(mean(milk_df$milk_yield, na.rm = TRUE), 2),
          avg_fat_pct        = round(mean(fat_df$fat_value, na.rm = TRUE), 2),
          breed_summary      = table(animals_df$Breed),
          gender_summary     = table(animals_df$Gender),
          status_summary     = table(animals_df$Status)
        )

      } else if (input$report_type == "location") {

        req(input$selected_animal_code)

        # --- Herd/farm location for one animal ---
        # animals.breed_id  -> breeds.breed_id      (Breed name)
        # animals.herd_id   -> herds.herd_id        (Herd name, herd_type_id, herd status)
        # herds.farm_id     -> farms.farm_id        (Farm name)
        # herds.herd_type_id -> herd_type."herd_type_id " (Herd type name)
        # NOTE: herd_type's columns were created with trailing spaces in their
        # names ("herd_type_id ", "type_name ") — the quoting below is required,
        # not a typo. breeds/herds/farms/animals all use normal unquoted names.
        loc_df <- tryCatch({
          dbGetQuery(db_pool, "
            SELECT
                a.animal_code   AS \"Animal_Code\",
                a.animal_name   AS \"Animal_Name\",
                a.gender        AS \"Gender\",
                b.breed_name    AS \"Breed\",
                a.status        AS \"Status\",
                f.farm_name     AS \"Farm\",
                h.herd_name     AS \"Herd\",
                ht.\"type_name \" AS \"Herd_Type\"
            FROM animals a
            LEFT JOIN breeds b     ON a.breed_id = b.breed_id
            LEFT JOIN herds h      ON a.herd_id = h.herd_id
            LEFT JOIN farms f      ON h.farm_id = f.farm_id
            LEFT JOIN herd_type ht ON h.herd_type_id = ht.\"herd_type_id \"
            WHERE a.animal_code = $1
          ", params = list(input$selected_animal_code))
        }, error = function(e) {
          message("Reports location query error: ", conditionMessage(e))
          data.frame()
        })

        list(
          type = "location",
          animal_code = input$selected_animal_code,
          info = loc_df
        )

      } else {
        # Individual Animal report — later phase
        list(type = "individual")
      }
    })

    output$report_output <- renderUI({
      req(report_data())
      rd <- report_data()

      if (rd$type == "herd") {

        tagList(
          h4("Herd Summary"),

          tags$table(
            class = "table table-bordered",
            tags$tr(tags$td("Total Animals"),      tags$td(rd$total_animals)),
            tags$tr(tags$td("Total Milk Records"), tags$td(rd$total_milk_records)),
            tags$tr(tags$td("Total Fat Records"),  tags$td(rd$total_fat_records)),
            tags$tr(tags$td("Average Milk Yield"), tags$td(paste(rd$avg_milk_yield, "L"))),
            tags$tr(tags$td("Average Fat %"),      tags$td(paste(rd$avg_fat_pct, "%")))
          ),

          h4("Breed Summary"),
          tableOutput(NS(id, "breed_table")),

          h4("Gender Summary"),
          tableOutput(NS(id, "gender_table")),

          h4("Status Summary"),
          tableOutput(NS(id, "status_table")),

          br(),
          fluidRow(
            column(
              width = 3,
              downloadButton(NS(id, "download_csv"), "Download CSV",
                              class = "btn-primary", style = "width:100%;")
            ),
            column(
              width = 3,
              downloadButton(NS(id, "download_excel"), "Download Excel",
                              class = "btn-primary", style = "width:100%;")
            )
          )
        )

      } else if (rd$type == "location") {

        if (nrow(rd$info) == 0) {
          return(tagList(p(paste("No record found for animal code", rd$animal_code))))
        }

        row <- rd$info[1, ]

        tagList(
          h4(paste("Herd Location —", row$Animal_Code)),
          tags$table(
            class = "table table-bordered",
            tags$tr(tags$td("Animal Code"),  tags$td(row$Animal_Code)),
            tags$tr(tags$td("Animal Name"),  tags$td(ifelse(is.na(row$Animal_Name), "—", row$Animal_Name))),
            tags$tr(tags$td("Farm"),         tags$td(ifelse(is.na(row$Farm), "—", row$Farm))),
            tags$tr(tags$td("Herd"),         tags$td(ifelse(is.na(row$Herd), "—", row$Herd))),
            tags$tr(tags$td("Herd Type"),    tags$td(ifelse(is.na(row$Herd_Type), "—", row$Herd_Type))),
            tags$tr(tags$td("Gender"),       tags$td(ifelse(is.na(row$Gender), "—", row$Gender))),
            tags$tr(tags$td("Breed"),        tags$td(ifelse(is.na(row$Breed), "—", row$Breed))),
            tags$tr(tags$td("Status"),       tags$td(ifelse(is.na(row$Status), "—", row$Status)))
          )
        )

      } else {
        tagList(p("Individual Animal report coming in a later phase."))
      }
    })

    output$breed_table  <- renderTable(as.data.frame(report_data()$breed_summary))
    output$gender_table <- renderTable(as.data.frame(report_data()$gender_summary))
    output$status_table <- renderTable(as.data.frame(report_data()$status_summary))

    # ---------------------------------------------------------
    # Helper: turn the herd summary list into a single flat
    # data frame — used by the CSV export.
    # ---------------------------------------------------------
    build_summary_df <- function(rd) {
      data.frame(
        Metric = c("Total Animals", "Total Milk Records", "Total Fat Records",
                   "Average Milk Yield", "Average Fat %"),
        Value  = c(rd$total_animals, rd$total_milk_records, rd$total_fat_records,
                   rd$avg_milk_yield, rd$avg_fat_pct)
      )
    }

    # ---------------------------------------------------------
    # Download CSV — herd summary stats + breed/gender/status
    # breakdowns stacked in one file.
    # ---------------------------------------------------------
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("herd_summary_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        rd <- report_data()

        summary_df <- build_summary_df(rd)
        breed_df   <- as.data.frame(rd$breed_summary)
        gender_df  <- as.data.frame(rd$gender_summary)
        status_df  <- as.data.frame(rd$status_summary)

        con <- file(file, open = "w")
        writeLines("Herd Summary", con)
        write.csv(summary_df, con, row.names = FALSE)
        writeLines("", con)
        writeLines("Breed Summary", con)
        write.csv(breed_df, con, row.names = FALSE)
        writeLines("", con)
        writeLines("Gender Summary", con)
        write.csv(gender_df, con, row.names = FALSE)
        writeLines("", con)
        writeLines("Status Summary", con)
        write.csv(status_df, con, row.names = FALSE)
        close(con)
      }
    )

    # ---------------------------------------------------------
    # Download Excel — one workbook, each table on its own sheet.
    # Needs: install.packages("openxlsx")
    # ---------------------------------------------------------
    output$download_excel <- downloadHandler(
      filename = function() {
        paste0("herd_summary_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        rd <- report_data()

        wb <- openxlsx::createWorkbook()

        openxlsx::addWorksheet(wb, "Herd Summary")
        openxlsx::writeData(wb, "Herd Summary", build_summary_df(rd))

        openxlsx::addWorksheet(wb, "Breed Summary")
        openxlsx::writeData(wb, "Breed Summary", as.data.frame(rd$breed_summary))

        openxlsx::addWorksheet(wb, "Gender Summary")
        openxlsx::writeData(wb, "Gender Summary", as.data.frame(rd$gender_summary))

        openxlsx::addWorksheet(wb, "Status Summary")
        openxlsx::writeData(wb, "Status Summary", as.data.frame(rd$status_summary))

        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )

  })
}

# =========================================================
# app.R wiring (unchanged — no changes needed there):
#
#   nav_panel(title = "Reports", value = "reports_tab",
#             mod_reports_ui("reports_1"))
#
#   mod_reports_server("reports_1", records_data = records_data)
# =========================================================