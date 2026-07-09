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
    
