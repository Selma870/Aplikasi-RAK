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
    
    
    df <- data.frame(
      perlakuan = as.factor(rv$data[[input$kol_perlakuan]]),
      kelompok  = as.factor(rv$data[[input$kol_kelompok]]),
      respon    = as.numeric(as.character(rv$data[[input$kol_respon]]))
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
    
