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
  
  # CONTOH TABEL
  output$contoh_tabel_struktur <- renderTable({
    data.frame(
      Kelompok = rep(c("Kelompok 1", "Kelompok 2", "Kelompok 3"), each = 3),
      Perlakuan = rep(c("Pupuk A", "Pupuk B", "Pupuk C"), times = 3),
      Tinggi_Tanaman = c(15.2, 17.1, 12.5,
                         14.8, 16.9, 12.8,
                         15.5, 17.5, 12.0)
    )
  })
  
  output$contoh_tabel <- renderTable({
    data.frame(
      "Kelompok ke-" = 1:3,
      "Pupuk A" = c(15.2, 14.8, 15.5),
      "Pupuk B" = c(17.1, 16.9, 17.5),
      "Pupuk C" = c(12.5, 12.8, 12.0)
    )
  })
  
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
  
  #UJI ANOVA & INTERPRETASI
  output$anova_output <- renderPrint({
    hasil <- aov(respon ~ perlakuan + kelompok, data = rv$data)
    rv$hasil_anova <- hasil
    
    anova_table <- summary(hasil)[[1]]
    rv$anova_p <- anova_table[["Pr(>F)"]][1]            # p-value Perlakuan
    rv$anova_f <- anova_table[["F value"]][1]
    rv$anova_p_kelompok <- anova_table[["Pr(>F)"]][2]   # p-value Kelompok
    rv$anova_f_kelompok <- anova_table[["F value"]][2]
    
    cat("Tabel ANOVA (RAK):\n")
    print(anova_table)
    
    cat("\nNilai Statistik Perlakuan:\n")
    cat("F-hitung :", round(rv$anova_f, 4), "\n")
    cat("p-value  :", format(rv$anova_p, 5), "\n")
    
    cat("\nNilai Statistik Kelompok:\n")
    cat("F-hitung :", round(rv$anova_f_kelompok, 4), "\n")
    cat("p-value  :", format(rv$anova_p_kelompok, 5), "\n")
  })
  
  
  output$keputusan_output <- renderPrint({
    req(rv$anova_p, rv$anova_p_kelompok)
    
    cat("Keputusan Faktor Perlakuan:\n")
    cat("p-value =", format(rv$anova_p, 5), "| alpha =", input$alpha, "\n")
    if (rv$anova_p <= input$alpha) {
      cat("Tolak H0: Ada pengaruh perlakuan terhadap respon.\n\n")
    } else {
      cat("Gagal tolak H0: Tidak ada pengaruh perlakuan terhadap respon.\n\n")
    }
    
    cat("Keputusan Faktor Kelompok:\n")
    cat("p-value =", format(rv$anova_p_kelompok, 5), "| alpha =", input$alpha, "\n")
    if (rv$anova_p_kelompok <= input$alpha) {
      cat("Tolak H0: Ada pengaruh kelompok terhadap respon.")
    } else {
      cat("Gagal tolak H0: Tidak ada pengaruh kelompok terhadap respon.")
    }
  })
  
  output$interpretasi_anova <- renderUI({
    req(rv$anova_p, rv$anova_p_kelompok)
    
    # --- Blok kesimpulan Faktor PERLAKUAN ---
    blok_perlakuan <- if (rv$anova_p <= input$alpha) {
      paste0("
      <div style='padding:12px; background:#e3fcec; border-left:5px solid #2ecc71; margin-bottom:14px;'>
        <h4><b>Kesimpulan - Faktor Perlakuan:</b></h4>
        <p>Terdapat <b>perbedaan yang signifikan</b> antara perlakuan (<i>p-value = ", format(rv$anova_p, 5), " < ", input$alpha, "</i>).</p>
        <p>Karena H0 ditolak, maka dengan menggunakan tingkat signifikansi ", input$alpha * 100, "% dapat disimpulkan bahwa setidaknya ada satu perlakuan yang berbeda pengaruhnya terhadap respon.</p>

        <h4><b>Interpretasi:</b></h4>
        <p>Perlakuan yang diberikan <b>memiliki pengaruh nyata</b> terhadap nilai respon, setelah efek kelompok diperhitungkan dalam model.</p>

        <h4><b>Saran:</b></h4>
        <p>Lanjutkan ke <b>Uji Lanjut</b> untuk mengetahui pasangan perlakuan mana yang berbeda secara signifikan.</p>
      </div>
      ")
    } else {
      paste0("
      <div style='padding:12px; background:#fdecea; border-left:5px solid #e74c3c; margin-bottom:14px;'>
        <h4><b>Kesimpulan - Faktor Perlakuan:</b></h4>
        <p>Tidak ditemukan perbedaan yang signifikan antar perlakuan (<i>p-value = ", format(rv$anova_p, 5), " >= ", input$alpha, "</i>).</p>

        <h4><b>Interpretasi:</b></h4>
        <p>Perlakuan yang diberikan <b>tidak terbukti mempengaruhi</b> nilai respon secara statistik.</p>

        <h4><b>Saran:</b></h4>
        <p>Tinjau kembali desain eksperimen atau coba uji pada variabel respon yang berbeda.</p>
      </div>
      ")
    }
    
    # --- Blok kesimpulan Faktor KELOMPOK ---
    blok_kelompok <- if (rv$anova_p_kelompok <= input$alpha) {
      paste0("
      <div style='padding:12px; background:#eaf4fc; border-left:5px solid #3498db;'>
        <h4><b>Kesimpulan - Faktor Kelompok:</b></h4>
        <p>Terdapat <b>perbedaan yang signifikan</b> antar kelompok (<i>p-value = ", format(rv$anova_p_kelompok, 5), " < ", input$alpha, "</i>).</p>
        <p>Karena H0 ditolak, maka dengan menggunakan tingkat signifikansi ", input$alpha * 100, "% dapat disimpulkan bahwa setidaknya ada satu kelompok yang memberikan efek berbeda terhadap respon.</p>

        <h4><b>Interpretasi:</b></h4>
        <p>Pengelompokan (blocking) yang dilakukan <b>efektif</b> dalam menjelaskan keragaman data, sehingga penggunaan RAK pada percobaan ini sudah tepat.</p>
      </div>
      ")
    } else {
      paste0("
      <div style='padding:12px; background:#f4f4f4; border-left:5px solid #95a5a6;'>
        <h4><b>Kesimpulan - Faktor Kelompok:</b></h4>
        <p>Tidak ditemukan perbedaan yang signifikan antar kelompok (<i>p-value = ", format(rv$anova_p_kelompok, 5), " >= ", input$alpha, "</i>).</p>

        <h4><b>Interpretasi:</b></h4>
        <p>Faktor kelompok <b>tidak terbukti berpengaruh nyata</b> terhadap respon. Pengelompokan mungkin kurang diperlukan pada data ini, namun kesimpulan untuk faktor perlakuan di atas tetap valid secara statistik.</p>
      </div>
      ")
    }
    
    HTML(paste0(blok_perlakuan, blok_kelompok))
  })
  
  output$lanjut_uji <- renderUI({
    req(rv$anova_p <= input$alpha)
    radioButtons("uji_lanjut", "Lakukan uji lanjut?", c("Tidak", "Ya"))
  })
  
  output$lanjut_isi <- renderUI({
    req(rv$hasil_anova, rv$anova_p <= input$alpha)
    if (input$uji_lanjut == "Ya") {
      tagList(
        selectInput("jenis_uji", "Jenis Uji Lanjut:", choices = c("BNT", "BNJ", "Duncan")),
        verbatimTextOutput("uji_lanjut_output"),
        htmlOutput("interpretasi_lanjut")
      )
    } else {
      h4("Anda tidak memilih untuk uji lanjut.")
    }
  })
  
  #UJI LANJUT & INTERPRETASI
    observeEvent(input$jenis_uji, {
      req(rv$hasil_anova, input$jenis_uji)
      hasil <- switch(input$jenis_uji,
                      "BNT" = LSD.test(rv$hasil_anova, "perlakuan", p.adj = "none"),
                      "BNJ" = HSD.test(rv$hasil_anova, "perlakuan"),
                      "Duncan" = duncan.test(rv$hasil_anova, "perlakuan"))
      
      output$uji_lanjut_output <- renderPrint({ hasil })
      
      output$interpretasi_lanjut <- renderUI({
        req(hasil$groups)
        
        # Ambil nilai rerata
        means <- hasil$means$respon
        if (is.null(names(means))) {
          names(means) <- rownames(hasil$means)
        }
        
        if (length(means) < 2) {
          return(HTML("<div style='color:red;'>Jumlah perlakuan terlalu sedikit untuk dibandingkan.</div>"))
        }
        
        perlakuan <- names(means)
        
        # Interpretasi khusus Duncan
        if (input$jenis_uji == "Duncan") {
          means_duncan <- hasil$means
          q_duncan <- hasil$duncan
          
          if (!is.null(q_duncan)) {
            pasangan_tbl <- data.frame(Pasangan = character(), Selisih = numeric(), 
                                       CriticalRange = numeric(), BedaNyata = character(), stringsAsFactors = FALSE)
            
            perlakuan_names <- rownames(means_duncan)
            mean_vals <- means_duncan$respon
            
            urut <- order(mean_vals, decreasing = TRUE)
            perlakuan_urut <- perlakuan_names[urut]
            mean_urut <- mean_vals[urut]
            
            for (i in 1:(length(perlakuan_urut) - 1)) {
              for (j in (i + 1):length(perlakuan_urut)) {
                p1 <- perlakuan_urut[i]
                p2 <- perlakuan_urut[j]
                m1 <- mean_urut[i]
                m2 <- mean_urut[j]
                selisih <- abs(m1 - m2)
                step <- j - i + 1
                if (step > nrow(q_duncan)) step <- nrow(q_duncan)
                
                kritis <- q_duncan$CriticalRange[step - 1]
                beda <- ifelse(selisih > kritis, "Ya", "Tidak")
                
                pasangan_tbl <- rbind(pasangan_tbl, data.frame(
                  Pasangan = paste(p1, "vs", p2),
                  Selisih = round(selisih, 3),
                  CriticalRange = round(kritis, 3),
                  BedaNyata = beda
                ))
              }
            }
            
            html_duncan <- paste0(
              "<h4><b>Tabel Perbandingan 1 vs 1 (Duncan)</b></h4>",
              "<div style='overflow-x:auto;'>",
              "<table style='border-collapse: separate; border-spacing: 10px 6px; width: 100%;'>",
              "<thead style='background:#f2f2f2;'>",
              "<tr><th>Pasangan</th><th>Selisih</th><th>Critical Range</th><th>Berbeda Nyata?</th></tr>",
              "</thead><tbody>"
            )
            for (k in 1:nrow(pasangan_tbl)) {
              html_duncan <- paste0(html_duncan,
                                    "<tr>",
                                    "<td>", pasangan_tbl$Pasangan[k], "</td>",
                                    "<td align='center'>", pasangan_tbl$Selisih[k], "</td>",
                                    "<td align='center'>", pasangan_tbl$CriticalRange[k], "</td>",
                                    "<td align='center'>", pasangan_tbl$BedaNyata[k], "</td>",
                                    "</tr>"
              )
            }
            html_duncan <- paste0(html_duncan, "</tbody></table></div><br>")
            
            grup <- hasil$groups
            grup <- grup[order(grup$groups), , drop = FALSE]
            
            teks_grup <- paste0(
              "<h4><b>Interpretasi Hasil Uji Duncan</b></h4>",
              "<p>Perlakuan dikelompokkan berdasarkan huruf yang <b>berbeda</b>. Jika dua perlakuan memiliki huruf berbeda, maka <b>berbeda signifikan</b>.</p>",
              "<ul>"
            )
            for (i in 1:nrow(grup)) {
              teks_grup <- paste0(teks_grup, "<li><b>", rownames(grup)[i], "</b> (rata-rata = ", round(grup$respon[i], 3),
                                  ") -> grup <b>", grup$groups[i], "</b></li>")
            }
            teks_grup <- paste0(teks_grup, "</ul>")
            
            terbaik <- rownames(grup)[which.max(grup$respon)]
            saran <- paste0(
              "<h4><b>Saran:</b></h4>",
              "<p>Perlakuan <b>", terbaik, "</b> memiliki rata-rata tertinggi dan termasuk dalam grup signifikan tertinggi. Ini bisa dipilih sebagai perlakuan terbaik <i>(jika berbeda nyata dari yang lain).</i></p>"
            )
            
            return(HTML(paste0(
              "<div style='padding:10px; background:#f0f8ff; border-left:5px solid #3498db'>",
              html_duncan,
              teks_grup,
              saran,
              "</div>"
            )))
          }
        }
        
      # Interpretasi umum untuk uji lain (BNT, BNJ)
      nilai_kritis <- NA
      stat <- hasil$statistics
      if (!is.null(stat)) {
        if (!is.null(stat$LSD)) {
          nilai_kritis <- stat$LSD
        } else if (!is.null(stat$HSD)) {
          nilai_kritis <- stat$HSD
        } else if (!is.null(stat$MSD)) {
          nilai_kritis <- stat$MSD
        }
      }
      
      if (is.na(nilai_kritis)) {
        return(HTML("<div style='color:red;'>Nilai kritis tidak ditemukan. Tidak bisa lanjut interpretasi.</div>"))
      }
      
      tbl <- data.frame(Pasangan = character(), Selisih = numeric(), Kritis = numeric(), BedaNyata = character(), stringsAsFactors = FALSE)
      
      for (i in 1:(length(perlakuan) - 1)) {
        for (j in (i + 1):length(perlakuan)) {
          nama1 <- perlakuan[i]
          nama2 <- perlakuan[j]
          
          cek1 <- nama1 %in% names(means)
          cek2 <- nama2 %in% names(means)
          if (is.na(cek1) || is.na(cek2) || !cek1 || !cek2) next
          
          m1 <- means[nama1]
          m2 <- means[nama2]
          if (is.na(m1) || is.na(m2)) next
          
          selisih <- abs(m1 - m2)
          beda <- ifelse(selisih > nilai_kritis, "Ya", "Tidak")
          
          tbl <- rbind(tbl, data.frame(
            Pasangan = paste(nama1, "vs", nama2),
            Selisih = round(selisih, 3),
            Kritis = round(nilai_kritis, 3),
            BedaNyata = beda,
            stringsAsFactors = FALSE
          ))
        }
      }
      
      table_html <- paste0(
        "<h4><b>Tabel Perbandingan Antar Perlakuan</b></h4>",
        "<div style='overflow-x:auto;'>",
        "<table style='border-collapse: separate; border-spacing: 10px 6px; width: 100%;'>",
        "<thead style='background:#f2f2f2;'>",
        "<tr><th>Pasangan</th><th>Selisih</th><th>Nilai Kritis</th><th>Berbeda Nyata?</th></tr>",
        "</thead><tbody>"
      )
      for (k in 1:nrow(tbl)) {
        table_html <- paste0(table_html,
                             "<tr>",
                             "<td>", tbl$Pasangan[k], "</td>",
                             "<td align='center'>", tbl$Selisih[k], "</td>",
                             "<td align='center'>", tbl$Kritis[k], "</td>",
                             "<td align='center'>", tbl$BedaNyata[k], "</td>",
                             "</tr>")
      }
      table_html <- paste0(table_html, "</table><br>")
      
      grup <- hasil$groups
      grup <- grup[order(grup$groups), , drop = FALSE]
      
      teks_grup <- paste0(
        "<h4><b>Interpretasi Hasil Uji ", input$jenis_uji, "</b></h4>",
        "<p>Perlakuan dikelompokkan berdasarkan huruf yang <b>berbeda</b>. Jika dua perlakuan memiliki huruf berbeda, maka <b>berbeda signifikan</b>.</p>",
        "<ul>"
      )
      for (i in 1:nrow(grup)) {
        teks_grup <- paste0(teks_grup, "<li><b>", rownames(grup)[i], "</b> (rata-rata = ", round(grup$respon[i], 3),
                            ") -> grup <b>", grup$groups[i], "</b></li>")
      }
      teks_grup <- paste0(teks_grup, "</ul>")
      
      terbaik <- rownames(grup)[which.max(grup$respon)]
      saran <- paste0(
        "<h4><b>Saran:</b></h4>",
        "<p>Perlakuan <b>", terbaik, "</b> memiliki rata-rata tertinggi dan termasuk dalam grup signifikan tertinggi. Ini bisa dipilih sebagai perlakuan terbaik <i>(jika berbeda nyata dari yang lain).</i></p>"
      )
      
      HTML(paste0(
        "<div style='padding:10px; background:#f0f8ff; border-left:5px solid #3498db'>",
        table_html,
        teks_grup,
        saran,
        "</div>"
      ))
    })
  })
}
shinyApp(ui, server)
