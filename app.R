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

# ===============================
# === SERVER
# ===============================
server <- function(input, output, session) {
  rv <- reactiveValues(data=NULL, hasil_anova=NULL, anova_p=NULL, anova_p_kelompok=NULL, valid=FALSE)
  
  # DATA UPLOAD DAN PILIH KOLOM  
  observeEvent(input$datafile, {
    ext <- tools::file_ext(input$datafile$name)
    df <- if (ext == "csv") read.csv(input$datafile$datapath) else read_excel(input$datafile$datapath)
    rv$data <- df
    output$pilih_kolom <- renderUI({
      tagList(
        selectInput("kol_perlakuan", "Kolom Perlakuan:", choices = names(rv$data)),
        selectInput("kol_kelompok", "Kolom Kelompok/Blok:", choices = names(rv$data)),
        selectInput("kol_respon", "Kolom Respon:", choices = names(rv$data))
      )
    })
  })

