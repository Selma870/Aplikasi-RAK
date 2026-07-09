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
    
