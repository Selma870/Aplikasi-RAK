# ===============================
library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(tidyr)
library(car)
library(agricolae)
library(reshape2)

# ===============================
#  UI App
# ===============================
ui <- dashboardPage(
  dashboardHeader(title = "Aplikasi RAK"),
  
  # Sidebar  
  dashboardSidebar(
    sidebarMenu(
      menuItem("1. Tentang RAK", tabName = "tentang", icon = icon("info-circle")),
      menuItem("2. Input & Validasi Data", tabName = "validasi", icon = icon("check-circle")),
      menuItem("3. Uji Hipotesis", tabName = "uji", icon = icon("flask")),
      menuItem("4. Uji Lanjut", tabName = "lanjut", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto&display=swap"),
      tags$style(HTML("
        body, h1, h2, h3, h4, h5, p, label {
          font-family: 'Roboto', sans-serif;
        }
      "))
    ),
    
    tabItems(
      # Tab Penjelasan RAK di Menu
      tabItem(tabName = "tentang",
              fluidRow(
                column(width = 6,
                       box(title = "Penjelasan Teori RAK", width = 12, status = "primary", solidHeader = TRUE,
                           h3("Rancangan Acak Kelompok (RAK)"),
                           p("RAK digunakan ketika unit percobaan bersifat heterogen dengan unit percobaan berasal dari satu sumber keragaman untuk mengurangi 
                           variasi kesalahan eksperimen dengan mengelompokkan unit eksperimental yang serupa ke dalam blok atau kelompok. Dengan menggunakan 
                           rancangan ini maka peneliti dapat mengontrol variabilitas di antara unit eksperimen yang dapat memengaruhi data."),
                           tags$ul(
                             tags$li(strong("Pengelompokan (Blocking):"), " Unit dikelompokkan agar lebih homogen di dalam satu kelompok."),
                             tags$li(strong("Pengacakan Terbatas:"), " Perlakuan diacak hanya di dalam tiap kelompok, bukan ke seluruh unit percobaan."),
                             tags$li(strong("Dua Sumber Keragaman Terkontrol:"), " Efek perlakuan dan efek kelompok dipisahkan dari galat."),
                             tags$li(strong("Lebih Presisi dari RAL:"), " Jika unit tidak homogen (heterogen), RAK menurunkan galat percobaan dibanding RAL.")
                           ),
                           h4("Tujuan RAK"),
                           tags$ul(
                             tags$li("Melihat pengaruh perlakuan setelah mengendalikan keragaman akibat kelompok."),
                             tags$li("Membandingkan antar perlakuan secara lebih akurat."),
                             tags$li("Menarik kesimpulan valid meskipun unit percobaan tidak seragam.")
                           ),
                           h4("Syarat Penting"),
                           tags$ul(
                             tags$li("Setiap perlakuan muncul tepat satu kali pada setiap kelompok"),
                             tags$li("Unit di dalam satu kelompok relatif homogen"),
                             tags$li("Tidak ada interaksi antara perlakuan dan kelompok"),
                             tags$li("Galat menyebar normal, homogen, dan saling independen")
                           ),
                           p("Jika data homogen, gunakan RAL")
                       )
                ),
                column(width = 6,
                       box(title = "Struktur Data yang Diperlukan", width = 12, tableOutput("contoh_tabel_struktur")),
                       box(title = "Contoh Tabel Kontingensi", width = 12, tableOutput("contoh_tabel"))
                )
              ),
              fluidRow(
                box(title = "Penjelasan Uji Lanjut", width = 12, status = "info", solidHeader = TRUE,
                    h4("Uji Lanjut (Post Hoc) Setelah ANOVA"),
                    p("Jika pengaruh perlakuan pada ANOVA signifikan, gunakan uji lanjut untuk mengetahui perlakuan mana yang berbeda, setelah efek kelompok diperhitungkan dalam model."),
                    tags$ul(
                      tags$li(strong("BNT (LSD):"), " sensitif, cocok untuk perlakuan sedikit."),
                      tags$li(strong("BNJ (HSD Tukey):"), " konservatif, cocok banyak perlakuan."),
                      tags$li(strong("Duncan:"), " membentuk grup berbeda bertingkat.")
                    )
                )
              ),
      ),
      # Tab Validasi dataset
      tabItem(tabName = "validasi",
              fluidRow(
                box(title = "Upload & Input", width = 4, status = "info",
                    fileInput("datafile", "Upload file CSV atau Excel", accept = c(".csv", ".xlsx")),
                    numericInput("alpha", "Nilai alpha:", 0.05, 0.001, 0.1, 0.001),
                    uiOutput("pilih_kolom"),
                    actionButton("validasi_btn", "Validasi Data")
                ),
                box(title = "Hasil Validasi", width = 8,
                    verbatimTextOutput("validasi_output"),
                    verbatimTextOutput("ringkasan_output"),
                    tableOutput("tabel_kontingensi"),
                    plotOutput("qqplot_data"),
                    plotOutput("boxplot_data"),
                    plotOutput("boxplot_kelompok")
                )
              )
      ),
      
      # Tab Uji ANOVA dataset
      tabItem(tabName = "uji",
              fluidRow(
                box(title = "Uji ANOVA", width = 6, status = "primary",
                    uiOutput("ukuran_input"),
                    verbatimTextOutput("hipotesis"),
                    verbatimTextOutput("anova_output"),
                    verbatimTextOutput("keputusan_output")
                ),
                box(title = "Lanjut ke Uji Lanjut?", width = 6,
                    uiOutput("lanjut_uji")
                )
              ),
              fluidRow(
                box(title = "Interpretasi Hasil ANOVA", width = 12, status = "success", solidHeader = TRUE,
                    htmlOutput("interpretasi_anova")
                )
              )
      ),  
      
      # Tab Uji Lanjut
      tabItem(tabName = "lanjut",
              fluidRow(
                box(title = "Hasil Uji Lanjut", width = 12,
                    uiOutput("lanjut_isi")
                )
              )
      )
    )
  )
)
# VALIDASI DATA
  observeEvent(input$validasi_btn, {
    req(rv$data, input$kol_perlakuan, input$kol_kelompok, input$kol_respon)
    
    kolom_sama <- (input$kol_perlakuan == input$kol_respon) ||
      (input$kol_kelompok == input$kol_respon) ||
      (input$kol_perlakuan == input$kol_kelompok)
    
    if (kolom_sama || !is.numeric(rv$data[[input$kol_respon]])) {
      output$validasi_output <- renderText("kolom Perlakuan, Kelompok, dan Respon harus berbeda, dan Respon harus numerik.")
      output$ringkasan_output <- renderPrint({})
      output$tabel_kontingensi <- renderTable({})
      output$boxplot_data <- renderPlot({})
      output$boxplot_kelompok <- renderPlot({})
      output$qqplot_data <- renderPlot({})
      rv$valid <- FALSE
      return()
    }
    
    if (any(is.na(rv$data[[input$kol_perlakuan]]) | is.na(rv$data[[input$kol_kelompok]]) | is.na(rv$data[[input$kol_respon]]))) {
      output$validasi_output <- renderText(
        paste0(
          "Terdapat data kosong (NA) pada kolom perlakuan, kelompok, atau respon.\n\n",
          "Silakan periksa dan perbaiki data Anda.\n\n",
          "Beberapa solusi yang dapat dilakukan:\n",
          "- Buka file Excel/CSV Anda.\n",
          "- Lengkapi nilai yang kosong sesuai pengamatan.\n",
          "- Jika data benar-benar hilang, pertimbangkan:\n",
          "  - Menghapus baris tersebut jika jumlahnya sedikit.\n",
          "  - Mengisi dengan rerata (mean) kelompok atau perlakuan jika relevan secara ilmiah.\n",
          "  - Melakukan eksperimen ulang untuk melengkapi data.\n\n",
          "Setelah diperbaiki, silakan upload ulang file Anda melalui tombol Upload."
        )
      )
      output$ringkasan_output <- renderPrint({})
      output$tabel_kontingensi <- renderTable({})
      output$boxplot_data <- renderPlot({})
      output$boxplot_kelompok <- renderPlot({})
      output$qqplot_data <- renderPlot({})
      rv$valid <- FALSE
      return()
    }
    
    df <- data.frame(
      perlakuan = as.factor(rv$data[[input$kol_perlakuan]]),
      kelompok  = as.factor(rv$data[[input$kol_kelompok]]),
      respon    = as.numeric(as.character(rv$data[[input$kol_respon]]))
    )
    
    # Cek desain lengkap: setiap perlakuan harus muncul TEPAT 1 kali di setiap kelompok
    tab_silang <- xtabs(~ perlakuan + kelompok, data=df)
    desain_lengkap <- all(tab_silang == 1)
    
    if (!desain_lengkap) {
      output$validasi_output <- renderText(
        paste0(
          "Desain RAK tidak lengkap.\n\n",
          "Pada RAK, setiap perlakuan wajib muncul tepat satu kali pada setiap kelompok ",
          "(tidak boleh ada kombinasi Perlakuan x Kelompok yang kosong ataupun berulang).\n\n",
          "Silakan periksa kembali kombinasi Perlakuan x Kelompok pada tabel silang di bawah."
        )
      )
      output$ringkasan_output <- renderPrint({
        cat("Tabel silang Perlakuan x Kelompok (seharusnya semua bernilai 1):\n")
        print(tab_silang)
      })
      output$tabel_kontingensi <- renderTable({})
      output$boxplot_data <- renderPlot({})
      output$boxplot_kelompok <- renderPlot({})
      output$qqplot_data <- renderPlot({})
      rv$valid <- FALSE
      return()
    }
    
    rv$data <- df
    alpha <- input$alpha
    
    # Normalitas diuji pada RESIDUAL model RAK (perlakuan + kelompok), bukan per kelompok perlakuan
    model_cek <- lm(respon ~ perlakuan + kelompok, data = df)
    res_model <- residuals(model_cek)
    
    if (length(res_model) < 3 || length(unique(round(res_model, 8))) == 1) {
      norm_txt <- "Galat konstan / jumlah data terlalu sedikit -> Tidak bisa diuji normalitas"
    } else {
      p_norm <- shapiro.test(res_model)$p.value
      norm_txt <- if (p_norm > alpha) {
        paste0("p = ", round(p_norm, 4), " > ", alpha, " -> Normal")
      } else {
        paste0("p = ", round(p_norm, 4), " <= ", alpha, " -> Tidak Normal")
      }
    }
    
    levene <- leveneTest(respon ~ perlakuan, data = df)
    
    output$ringkasan_output <- renderPrint({
      cat("Jumlah Perlakuan :", length(unique(df$perlakuan)), "\n")
      cat("Jumlah Kelompok  :", length(unique(df$kelompok)), "\n\n")
      cat("Uji Normalitas Galat (Shapiro-Wilk pada residual model RAK):\n")
      cat(norm_txt, "\n\n")
      cat("Levene Test (homogenitas ragam antar perlakuan): p =", round(levene[["Pr(>F)"]][1], 4))
    })
    
    output$tabel_kontingensi <- renderTable({
      dcast(df, kelompok ~ perlakuan, value.var = "respon")
    })
    
    output$qqplot_data <- renderPlot({
      qqnorm(res_model, main = "Q-Q Plot Residuals (Model RAK)", pch = 19, col = "darkblue")
      qqline(res_model, col = "red", lwd = 2)
    })
    
    output$boxplot_data <- renderPlot({
      boxplot(respon ~ perlakuan, data = df, col = "lightblue", main = "Boxplot Respon per Perlakuan",
              xlab = "Perlakuan", ylab = "Respon")
    })
    
    output$boxplot_kelompok <- renderPlot({
      boxplot(respon ~ kelompok, data = df, col = "lightgreen", main = "Boxplot Respon per Kelompok",
              xlab = "Kelompok", ylab = "Respon")
    })
    
    rv$valid <- TRUE
    output$validasi_output <- renderText("Data valid (desain RAK lengkap). Lanjut ke tab Uji Hipotesis.")
  })
  
  output$ukuran_input <- renderUI({
    req(rv$valid)
    jumlah_perlakuan <- length(unique(rv$data$perlakuan))
    jumlah_kelompok  <- length(unique(rv$data$kelompok))
    helpText(paste0("Ukuran desain: ", jumlah_perlakuan, " perlakuan x ", jumlah_kelompok,
                    " kelompok (Total ", jumlah_perlakuan * jumlah_kelompok, " data)"))
  })
  
  output$hipotesis <- renderText({
    paste0(
      "Faktor Perlakuan\n",
      "H0: Tidak ada pengaruh perlakuan\n",
      "H1: Ada pengaruh perlakuan\n\n",
      "Faktor Kelompok\n",
      "H0: Tidak ada pengaruh kelompok\n",
      "H1: Ada pengaruh kelompok"
    )
  })
