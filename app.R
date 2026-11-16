# SHINY DASHBOARD
# ANALISIS DATA TANAMAN PADI SUMATERA

# 1. INSTALL PACKAGE
# 2. LOAD PACKAGE
library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(tidyr)
library(GGally)
library(corrplot)
library(car)
library(lmtest)
library(broom)
library(scales)
library(nortest)

# 3. WARNA DASHBOARD
biru <- "#168BCE"
biru_tua <- "#145DA0"
biru_muda <- "#D9F0FF"

# 4. NAMA VARIABEL
var_num <- c("Produksi", "Luas_Panen", "Curah_hujan", "Kelembapan", "Suhu_rata_rata")
label_num <- c("Produksi" = "Produksi", "Luas_Panen" = "Luas_Panen", "Curah_hujan" = "Curah_hujan", 
               "Kelembapan" = "Kelembapan", "Suhu_rata_rata" = "Suhu_rata_rata")

# 5. USER INTERFACE
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "Rice Data Analytics",
    titleWidth = 260
  ),
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      menuItem("Beranda & Data", tabName = "beranda",
               icon = icon("home")),
      menuItem("Analisis Univariat", tabName = "univariat",
               icon = icon("chart-bar")),
      menuItem("Analisis Bivariat", tabName = "bivariat",
               icon = icon("chart-line")),
      menuItem("Analisis Multivariat", tabName = "multivariat",
               icon = icon("project-diagram")),
      menuItem("Regresi Linear", tabName = "regresi",
               icon = icon("calculator"))
    ),
    br(),
    fileInput(
      "file",
      "Upload Data Excel",
      accept = ".xlsx"
    ),
    helpText("Data: Data Tanaman Padi Sumatera.xlsx")
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #F3F9FD;
        }
        .main-header .logo {
          font-weight: bold;
          background-color: #145DA0 !important;
        }
        .main-header .navbar {
          background-color: #168BCE !important;
        }
        .skin-blue .main-sidebar {
          background-color: #123B5D;
        }
        .box {
          border-radius: 10px;
        }
        .small-box {
          border-radius: 12px;
        }
        h3, h4 {
          color: #145DA0;
        }
      "))
    ),
    tabItems(
      # TAB 1: BERANDA
      tabItem(
        tabName = "beranda",
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "🌾 Analisis Data Tanaman Padi Sumatera",
            h4("Latar Belakang"),
            p("Tanaman padi merupakan salah satu komoditas pangan
              penting di Indonesia. Produksi padi dipengaruhi oleh
              berbagai faktor, baik faktor luas lahan maupun kondisi
              lingkungan seperti curah hujan, kelembapan, dan suhu."),
            p("Perbedaan karakteristik wilayah dan kondisi iklim
              antardaerah dapat menyebabkan variasi produksi padi.
              Oleh karena itu, eksplorasi data diperlukan untuk
              memahami karakteristik setiap variabel serta pola
              hubungan antarvariabel pada sejumlah provinsi di
              Sumatera."),
            p("Dashboard ini menyajikan analisis statistik deskriptif,
              eksplorasi bivariat dan multivariat, serta pemodelan
              regresi linear berganda untuk mengkaji hubungan antara
              produksi padi dan beberapa faktor yang diduga berkaitan
              dengannya.")
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "info",
            solidHeader = TRUE,
            title = "Tujuan Analisis",
            tags$ol(
              tags$li("Mengeksplorasi hubungan antara variabel
                      numerik dan kategorik."),
              tags$li("Mengidentifikasi pola hubungan beberapa
                      variabel secara simultan."),
              tags$li("Membangun model regresi linear berganda
                      untuk menganalisis hubungan faktor-faktor
                      prediktor dengan produksi padi."),
              tags$li("Melakukan pengujian signifikansi parameter
                      dan pemeriksaan asumsi model regresi.")
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Sumber Data",
            p("Dataset diperoleh dari Kaggle dengan judul
              Dataset Tanaman Padi Sumatera, Indonesia."),
            tags$a(
              href = "https://www.kaggle.com/datasets/ardikasatria/datasettanamanpadisumatera",
              "Buka Sumber Dataset di Kaggle",
              target = "_blank"
            ),
            p("Dataset diunduh sebagai file dengan format xlxs.")
          )
        ),
        fluidRow(
          valueBoxOutput("jumlah_obs", width = 4),
          valueBoxOutput("jumlah_var", width = 4),
          valueBoxOutput("jumlah_prov", width = 4)
        ),
        fluidRow(
          box(
            title = "Informasi Variabel",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("info_variabel")
          )
        ),
        fluidRow(
          box(
            title = "Pratinjau Data",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("tabel_data")
          )
        )
      ),
      # TAB 2: ANALISIS UNIVARIAT
      tabItem(
        tabName = "univariat",
        fluidRow(
          box(
            title = "Univariat Variabel Numerik",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            selectInput(
              "variabel_uni",
              "Pilih Variabel Numerik",
              choices = NULL
            ),
            selectInput(
              "jenis_plot_uni",
              "Pilih Visualisasi",
              choices = c(
                "Histogram",
                "Density Plot",
                "Boxplot",
                "Stem-and-Leaf"
              )
            )
          ),
          box(
            title = "Statistik Deskriptif",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            DTOutput("stat_uni")
          )
        ),
        fluidRow(
          box(
            title = "Visualisasi Variabel Numerik",
            status = "info",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput("plot_uni", height = "400px"),
            verbatimTextOutput("stem_leaf")
          ),
          box(
            title = "Deteksi Outlier Metode IQR",
            status = "warning",
            solidHeader = TRUE,
            width = 4,
            verbatimTextOutput("hasil_outlier")
          )
        ),
        fluidRow(
          box(
            title = "Univariat Variabel Kategorik: Provinsi",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            selectInput(
              "plot_prov",
              "Jenis Grafik",
              choices = c(
                "Bar Chart",
                "Pie Chart",
                "Pareto Chart"
              )
            ),
            plotlyOutput("grafik_prov", height = "400px")
          )
        )
      ),
      # TAB 3: ANALISIS BIVARIAT
      tabItem(
        tabName = "bivariat",
        # NUMERIK VS NUMERIK
        fluidRow(
          box(
            title = "A. Hubungan Numerik dengan Numerik",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            p("Menganalisis hubungan antara dua variabel
              numerik menggunakan scatter plot, garis regresi,
              dan korelasi Pearson."),
            column(
              4,
              selectInput(
                "x_nn",
                "Variabel X",
                choices = NULL
              )
            ),
            column(
              4,
              selectInput(
                "y_nn",
                "Variabel Y",
                choices = NULL
              )
            ),
            column(
              4,
              selectInput(
                "plot_nn",
                "Jenis Visualisasi",
                choices = c(
                  "Scatter Plot",
                  "Scatter Plot dengan Garis Regresi",
                  "Line Plot",
                  "Hexbin Plot"
                )
              )
            ),
            plotlyOutput("grafik_nn", height = "420px"),
            h4("Hasil Korelasi Pearson"),
            verbatimTextOutput("hasil_nn")
          )
        ),
        # KATEGORIK VS NUMERIK
        fluidRow(
          box(
            title = "B. Hubungan Kategorik dengan Numerik",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            p("Menganalisis perbedaan distribusi variabel numerik
              berdasarkan kelompok kategorik."),
            column(
              4,
              selectInput(
                "x_kn",
                "Variabel Kategorik",
                choices = c("Provinsi")
              )
            ),
            column(
              4,
              selectInput(
                "y_kn",
                "Variabel Numerik",
                choices = NULL
              )
            ),
            column(
              4,
              selectInput(
                "plot_kn",
                "Jenis Visualisasi",
                choices = c("Bar Chart Rata-rata", "Boxplot", "Violin Plot", "Boxplot + Violin Plot",
                            "Density Plot", "Strip Plot", "Cleveland Dot Plot", "Mean dan Standard Error"
                )
              )
            ),
            plotlyOutput("grafik_kn", height = "450px"),
            h4("Ringkasan Statistik Kelompok"),
            DTOutput("ringkasan_kn")
          )
        )
      ),
      # TAB 4: ANALISIS MULTIVARIAT
      tabItem(
        tabName = "multivariat",
        fluidRow(
          box(
            title = "Matriks Korelasi",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            p("Matriks korelasi digunakan untuk melihat arah
              dan kekuatan hubungan linear antarvariabel numerik."),
            plotOutput("heatmap", height = "550px")
          )
        ),
        fluidRow(
          box(
            title = "Scatterplot Matrix",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            p("Scatterplot matrix menyajikan hubungan berpasangan
              dari beberapa variabel numerik secara bersamaan."),
            plotOutput("pairs_plot", height = "650px")
          )
        ),
        fluidRow(
          box(
            title = "Eksplorasi Tiga Variabel Sekaligus",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            p("Pilih tiga variabel numerik untuk melihat pola
              hubungan secara simultan melalui visualisasi 3D."),
            column(
              4,
              selectInput(
                "x_3d",
                "Sumbu X",
                choices = NULL,
                selected = "Luas_Panen"
              )
            ),
            column(
              4,
              selectInput(
                "y_3d",
                "Sumbu Y",
                choices = NULL,
                selected = "Curah_hujan"
              )
            ),
            column(
              4,
              selectInput(
                "z_3d",
                "Sumbu Z",
                choices = NULL,
                selected = "Produksi"
              )
            ),
            plotlyOutput("plot_3d", height = "550px"),
            h4("Korelasi Antar Tiga Variabel"),
            DTOutput("korelasi_3d"),
            h4("Interpretasi Hubungan"),
            verbatimTextOutput("interpretasi_3d")
          )
        )
      ),
      # TAB 5: REGRESI LINEAR BERGANDA
      tabItem(
        tabName = "regresi",
        fluidRow(
          box(
            title = "Spesifikasi Model Regresi",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            p("Variabel dependen (Y): Produksi"),
            tags$ul(
              tags$li("X1 = Luas_Panen"),
              tags$li("X2 = Curah_hujan"),
              tags$li("X3 = Kelembapan"),
              tags$li("X4 = Suhu_rata_rata")
            ),
            p(strong(
              "Y = β0 + β1X1 + β2X2 + β3X3 + β4X4 + ε"
            )),
            selectInput(
              "metode_estimasi",
              "Metode Estimasi Parameter",
              choices = c(
                "Ordinary Least Squares (OLS)",
                "Maximum Likelihood Estimation (MLE)",
                "Method of Moments (MOM)"
              )
            ),
            p("Untuk model linear dengan galat normal,
              ketiga pendekatan tersebut menghasilkan estimasi
              koefisien yang sama dalam kondisi standar.")
          )
        ),
        fluidRow(
          valueBoxOutput("r_squared", width = 4),
          valueBoxOutput("adj_r_squared", width = 4),
          valueBoxOutput("rmse", width = 4)
        ),
        # KORELASI PEARSON
        fluidRow(
          box(
            title = "A. Korelasi Pearson Variabel Independen dan Dependen",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            p("Mengukur hubungan linear antara setiap variabel
              independen dengan Produksi."),
            DTOutput("korelasi_regresi"),
            plotOutput("scatter_regresi", height = "450px")
          )
        ),
        # ESTIMASI PARAMETER
        fluidRow(
          box(
            title = "B. Estimasi Parameter Model",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("metode_hasil"),
            DTOutput("koef_regresi")
          ),
          box(
            title = "Plot Selang Interval Estimasi Parameter",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("plot_interval", height = "450px")
          )
        ),
        # UJI SIGNIFIKANSI
        fluidRow(
          box(
            title = "C. Uji Signifikansi Parsial (Uji t)",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            DTOutput("uji_parsial")
          ),
          box(
            title = "D. Uji Signifikansi Simultan (Uji F)",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("uji_simultan")
          )
        ),
        # UJI MULTIKOLINEARITAS
        fluidRow(
          box(
            title = "E. Uji Multikolinearitas (VIF)",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            p("Uji multikolinearitas dilakukan untuk mengetahui
              apakah terdapat hubungan linear yang kuat
              antarvariabel independen menggunakan Variance
              Inflation Factor (VIF)."),
            DTOutput("uji_multikolinearitas")
          )
        ),
        # ASUMSI
        fluidRow(
          box(
            title = "E. Uji Normalitas Kolmogorov-Smirnov",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("uji_normalitas"),
            plotOutput("plot_normalitas", height = "350px")
          ),
          box(
            title = "F. Uji Independensi Durbin-Watson",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("uji_independen"),
            plotOutput("plot_acf", height = "350px")
          )
        ),
        fluidRow(
          box(
            title = "G. Uji Identik Glejser",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("uji_identik"),
            plotOutput("plot_identik", height = "350px")
          ),
          box(
            title = "H. Plot Observasi dan Residual",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotOutput("plot_observasi_residual", height = "350px")
          )
        ),
        # AKTUAL PREDIKSI DAN DIAGNOSTIK
        fluidRow(
          box(
            title = "Aktual vs Prediksi",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("aktual_prediksi", height = "400px")
          ),
          box(
            title = "Plot Diagnostik Residual",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotOutput("diagnostik", height = "400px")
          )
        )
      )
    )
  )
)

# 6. SERVER
server <- function(input, output, session) {
  # MEMBACA DATA EXCEL
  data_asli <- reactive({
    req(input$file)
    df <- read_excel(
      input$file$datapath,
      sheet = 1
    )
    kolom_wajib <- c("Provinsi", "Tahun", "Produksi", "Luas_Panen", "Curah_hujan",  "Kelembapan", "Suhu_rata_rata"
    )
    kolom_hilang <- setdiff(kolom_wajib, names(df))
    validate(
      need(
        length(kolom_hilang) == 0,
        paste(
          "Kolom tidak ditemukan:",
          paste(kolom_hilang, collapse = ", ")
        )
      )
    )
    df <- df %>%
      select(all_of(kolom_wajib)) %>%
      mutate(
        Provinsi = as.factor(Provinsi),
        Tahun = suppressWarnings(as.numeric(Tahun)),
        across(
          all_of(var_num),
          ~ suppressWarnings(as.numeric(.x))
        )
      )
    df
  })
  # Data lengkap untuk analisis numerik
  data_lengkap <- reactive({
    req(data_asli())
    data_asli() %>%
      filter(
        if_all(
          all_of(var_num),
          ~ !is.na(.x) & is.finite(.x)
        )
      )
  })
  # Pilihan variabel numerik
  observeEvent(data_asli(), {
    pilihan <- label_num
    updateSelectInput(
      session, "variabel_uni",
      choices = pilihan,
      selected = "Produksi"
    )
    updateSelectInput(
      session, "x_nn",
      choices = pilihan,
      selected = "Luas_Panen"
    )
    updateSelectInput(
      session, "y_nn",
      choices = pilihan,
      selected = "Produksi"
    )
    updateSelectInput(
      session, "y_kn",
      choices = pilihan,
      selected = "Produksi"
    )
    updateSelectInput(
      session, "x_3d",
      choices = pilihan,
      selected = "Luas_Panen"
    )
    updateSelectInput(
      session, "y_3d",
      choices = pilihan,
      selected = "Curah_hujan"
    )
    updateSelectInput(
      session, "z_3d",
      choices = pilihan,
      selected = "Produksi"
    )
  })
  # Data regresi
  data_reg <- reactive({
    req(data_asli())
    data_asli() %>%
      filter(
        if_all(
          all_of(c("Produksi", setdiff(var_num, "Produksi"))),
          ~ !is.na(.x) & is.finite(.x)
        )
      )
  })
  # Model regresi OLS
  model_regresi <- reactive({
    req(data_reg())
    lm(
      Produksi ~ Luas_Panen + Curah_hujan +
        Kelembapan + Suhu_rata_rata,
      data = data_reg()
    )
  })
  # BERANDA
  output$jumlah_obs <- renderValueBox({
    req(data_asli())
    valueBox(
      nrow(data_asli()),
      "Jumlah Observasi",
      icon = icon("database"),
      color = "blue"
    )
  })
  output$jumlah_var <- renderValueBox({
    req(data_asli())
    valueBox(
      ncol(data_asli()),
      "Jumlah Variabel",
      icon = icon("list"),
      color = "aqua"
    )
  })
  output$jumlah_prov <- renderValueBox({
    req(data_asli())
    valueBox(
      n_distinct(data_asli()$Provinsi),
      "Jumlah Provinsi",
      icon = icon("map"),
      color = "green"
    )
  })
  output$info_variabel <- renderDT({
    req(data_asli())
    info <- data.frame(
      Variabel = c("Provinsi", "Tahun", "Produksi", "Luas_Panen", "Curah_hujan", "Kelembapan", "Suhu_rata_rata"
      ),
      Tipe = c("Kategorik", "Numerik (waktu)", rep("Numerik", 5)
      ),
      Keterangan = c("Nama provinsi pengamatan", "Tahun pengamatan", "Jumlah produksi padi",
                     "Luas lahan yang dipanen", "Curah hujan", "Tingkat kelembapan udara", "Suhu udara rata-rata"
      )
    )
    datatable(
      info,
      options = list(dom = "t"),
      rownames = FALSE
    )
  })
  output$tabel_data <- renderDT({
    req(data_asli())
    datatable(
      data_asli(),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      filter = "top"
    )
  })
  # UNIVARIAT NUMERIK
  output$stat_uni <- renderDT({
    req(data_asli(), input$variabel_uni)
    x <- data_asli()[[input$variabel_uni]]
    x <- x[is.finite(x)]
    hasil <- data.frame(
      Statistik = c("Jumlah Data", "Minimum", "Maksimum", "Rata-rata", "Median", "Modus",
                    "Standar Deviasi", "Varians", "Kuartil 1", "Kuartil 3"
      ),
      Nilai = c(length(x), min(x), max(x), mean(x), median(x),
                as.numeric(names(sort(table(x), decreasing = TRUE)[1])),
                sd(x), var(x), quantile(x, 0.25), quantile(x, 0.75)
      )
    )
    hasil$Nilai <- round(as.numeric(hasil$Nilai), 3)
    datatable(
      hasil,
      options = list(dom = "t"),
      rownames = FALSE
    )
  })
  output$plot_uni <- renderPlotly({
    req(data_asli(), input$variabel_uni)
    df <- data_asli()
    x <- input$variabel_uni
    validate(
      need(any(is.finite(df[[x]])),
           "Tidak ada data numerik yang dapat ditampilkan.")
    )
    if (input$jenis_plot_uni == "Histogram") {
      p <- ggplot(df, aes(x = .data[[x]])) +
        geom_histogram(
          fill = biru,
          color = "white",
          bins = 20,
          na.rm = TRUE
        ) +
        labs(
          title = "Histogram",
          x = x,
          y = "Frekuensi"
        )
    } else if (input$jenis_plot_uni == "Density Plot") {
      p <- ggplot(df, aes(x = .data[[x]])) +
        geom_density(
          fill = biru,
          alpha = 0.5,
          na.rm = TRUE
        ) +
        labs(
          title = "Density Plot",
          x = x,
          y = "Kepadatan"
        )
    } else {
      p <- ggplot(df, aes(y = .data[[x]])) +
        geom_boxplot(
          fill = biru,
          alpha = 0.7,
          na.rm = TRUE
        ) +
        labs(
          title = "Boxplot",
          y = x
        )
    }
    ggplotly(p)
  })
  output$stem_leaf <- renderPrint({
    req(data_asli(), input$variabel_uni)
    if (input$jenis_plot_uni == "Stem-and-Leaf") {
      x <- data_asli()[[input$variabel_uni]]
      x <- x[is.finite(x)]
      cat("Stem-and-Leaf Plot\n\n")
      print(stem(x))
    } else {
      cat("Pilih Stem-and-Leaf pada pilihan visualisasi.")
    }
  })
  output$hasil_outlier <- renderPrint({
    req(data_asli(), input$variabel_uni)
    x <- data_asli()[[input$variabel_uni]]
    x <- x[is.finite(x)]
    q1 <- quantile(x, 0.25)
    q3 <- quantile(x, 0.75)
    iqr <- IQR(x)
    batas_bawah <- q1 - 1.5 * iqr
    batas_atas <- q3 + 1.5 * iqr
    outlier <- x[
      x < batas_bawah | x > batas_atas
    ]
    cat("Batas bawah:", round(batas_bawah, 3), "\n")
    cat("Batas atas:", round(batas_atas, 3), "\n")
    cat("Jumlah outlier:", length(outlier), "\n\n")
    if (length(outlier) > 0) {
      cat("Nilai outlier:\n")
      print(outlier)
    } else {
      cat("Tidak ditemukan outlier berdasarkan metode IQR.")
    }
  })
  # UNIVARIAT PROVINSI
  output$grafik_prov <- renderPlotly({
    req(data_asli(), input$plot_prov)
    df <- data_asli() %>%
      count(Provinsi, name = "Frekuensi") %>%
      arrange(desc(Frekuensi)) %>%
      mutate(
        Kumulatif = cumsum(Frekuensi),
        Persen_Kumulatif = Kumulatif / sum(Frekuensi) * 100
      )
    if (input$plot_prov == "Bar Chart") {
      p <- ggplot(
        df,
        aes(x = reorder(Provinsi, Frekuensi),
            y = Frekuensi)
      ) +
        geom_col(fill = biru) +
        coord_flip() +
        labs(x = "Provinsi", y = "Frekuensi")
    } else if (input$plot_prov == "Pie Chart") {
      # Pie chart dibuat langsung dengan Plotly agar tidak perlu
      # mengonversi coord_polar() dari ggplot ke Plotly.
      return(
        plotly::plot_ly(
          data = df,
          labels = ~Provinsi,
          values = ~Frekuensi,
          type = "pie",
          textinfo = "label+percent",
          hovertemplate = paste(
            "Provinsi: %{label}<br>",
            "Frekuensi: %{value}<br>",
            "Persentase: %{percent}<extra></extra>"
          ),
          sort = FALSE
        ) %>%
          plotly::layout(
            showlegend = TRUE,
            margin = list(l = 20, r = 20, b = 20, t = 20)
          )
      )
    } else {
      p <- ggplot(df, aes(
        x = reorder(Provinsi, -Frekuensi),
        y = Frekuensi
      )) +
        geom_col(fill = biru) +
        geom_line(
          aes(y = Persen_Kumulatif *
                max(Frekuensi) / 100,
              group = 1),
          color = "red",
          linewidth = 1
        ) +
        geom_point(
          aes(y = Persen_Kumulatif *
                max(Frekuensi) / 100),
          color = "red"
        ) +
        labs(x = "Provinsi", y = "Frekuensi") +
        theme_minimal()
    }
    ggplotly(p)
  })
  # BIVARIAT: NUMERIK VS NUMERIK
  output$grafik_nn <- renderPlotly({
    req(data_lengkap(), input$x_nn, input$y_nn)
    df <- data_lengkap()
    validate(
      need(input$x_nn != input$y_nn,
           "Pilih dua variabel yang berbeda.")
    )
    if (input$plot_nn == "Line Plot") {
      p <- ggplot(
        df,
        aes(
          x = .data[[input$x_nn]],
          y = .data[[input$y_nn]],
          color = Provinsi
        )
      ) +
        geom_line(alpha = 0.7) +
        geom_point() +
        labs(x = input$x_nn, y = input$y_nn)
    } else if (input$plot_nn == "Hexbin Plot") {
      p <- ggplot(
        df,
        aes(
          x = .data[[input$x_nn]],
          y = .data[[input$y_nn]]
        )
      ) +
        geom_bin2d() +
        labs(x = input$x_nn, y = input$y_nn)
    } else {
      p <- ggplot(
        df,
        aes(
          x = .data[[input$x_nn]],
          y = .data[[input$y_nn]],
          color = Provinsi
        )
      ) +
        geom_point(alpha = 0.7, size = 2) +
        labs(x = input$x_nn, y = input$y_nn)
      if (input$plot_nn ==
          "Scatter Plot dengan Garis Regresi") {
        p <- p +
          geom_smooth(
            aes(group = 1),
            method = "lm",
            se = TRUE,
            color = "black"
          )
      }
    }
    p <- p + theme_minimal()
    ggplotly(p)
  })
  output$hasil_nn <- renderPrint({
    req(data_lengkap(), input$x_nn, input$y_nn)
    validate(
      need(input$x_nn != input$y_nn,
           "Pilih dua variabel berbeda.")
    )
    x <- data_lengkap()[[input$x_nn]]
    y <- data_lengkap()[[input$y_nn]]
    hasil <- cor.test(x, y, method = "pearson")
    cat("Koefisien korelasi:", round(unname(hasil$estimate), 4), "\n")
    cat("P-value:", format.pval(hasil$p.value), "\n")
    cat("\nInterpretasi:\n")
    r <- unname(hasil$estimate)
    kekuatan <- if (abs(r) < 0.2) {
      "sangat lemah"
    } else if (abs(r) < 0.4) {
      "lemah"
    } else if (abs(r) < 0.6) {
      "sedang"
    } else if (abs(r) < 0.8) {
      "kuat"
    } else {
      "sangat kuat"
    }
    arah <- if (r > 0) "positif" else "negatif"
    cat("Hubungan", kekuatan, "dengan arah", arah, ".\n")
    if (hasil$p.value < 0.05) {
      cat("Hubungan signifikan pada taraf 5%.")
    } else {
      cat("Hubungan tidak signifikan pada taraf 5%.")
    }
  })
  # BIVARIAT: KATEGORIK VS NUMERIK
  data_kn <- reactive({
    req(data_asli(), input$y_kn)
    data_asli() %>%
      mutate(Kelompok = factor(Provinsi)) %>%
      filter(
        !is.na(Kelompok),
        is.finite(.data[[input$y_kn]])
      )
  })
  output$grafik_kn <- renderPlotly({
    req(data_kn(), input$plot_kn)
    df <- data_kn()
    y <- input$y_kn
    if (input$plot_kn == "Bar Chart Rata-rata") {
      p <- ggplot(
        df,
        aes(x = Kelompok, y = .data[[y]])
      ) +
        stat_summary(
          fun = mean,
          geom = "col",
          fill = biru
        ) +
        labs(x = input$x_kn, y = paste("Rata-rata", y))
    } else if (input$plot_kn == "Boxplot") {
      p <- ggplot(
        df,
        aes(x = Kelompok, y = .data[[y]], fill = Kelompok)
      ) +
        geom_boxplot(show.legend = FALSE) +
        labs(x = input$x_kn, y = y)
    } else if (input$plot_kn == "Violin Plot") {
      p <- ggplot(
        df,
        aes(x = Kelompok, y = .data[[y]], fill = Kelompok)
      ) +
        geom_violin(show.legend = FALSE, alpha = 0.7) +
        labs(x = input$x_kn, y = y)
    } else if (input$plot_kn == "Boxplot + Violin Plot") {
      p <- ggplot(
        df,
        aes(x = Kelompok, y = .data[[y]], fill = Kelompok)
      ) +
        geom_violin(show.legend = FALSE, alpha = 0.35) +
        geom_boxplot(
          width = 0.15,
          outlier.shape = NA,
          show.legend = FALSE
        ) +
        labs(x = input$x_kn, y = y)
    } else if (input$plot_kn == "Density Plot") {
      p <- ggplot(
        df,
        aes(
          x = .data[[y]],
          color = Kelompok,
          fill = Kelompok
        )
      ) +
        geom_density(alpha = 0.15, na.rm = TRUE) +
        labs(x = y, y = "Kepadatan")
    } else if (input$plot_kn == "Strip Plot") {
      p <- ggplot(
        df,
        aes(x = Kelompok, y = .data[[y]], color = Kelompok)
      ) +
        geom_jitter(width = 0.2, alpha = 0.65) +
        labs(x = input$x_kn, y = y)
    } else if (input$plot_kn == "Cleveland Dot Plot") {
      ringkas <- df %>%
        group_by(Kelompok) %>%
        summarise(
          Nilai = mean(.data[[y]], na.rm = TRUE),
          .groups = "drop"
        )
      p <- ggplot(
        ringkas,
        aes(x = Nilai, y = reorder(Kelompok, Nilai))
      ) +
        geom_point(size = 3, color = biru) +
        labs(x = paste("Rata-rata", y), y = input$x_kn)
    } else {
      ringkas <- df %>%
        group_by(Kelompok) %>%
        summarise(
          Rata_rata = mean(.data[[y]], na.rm = TRUE),
          SE = sd(.data[[y]], na.rm = TRUE) /
            sqrt(sum(!is.na(.data[[y]]))),
          .groups = "drop"
        )
      p <- ggplot(
        ringkas,
        aes(
          x = Kelompok,
          y = Rata_rata
        )
      ) +
        geom_col(fill = biru, alpha = 0.8) +
        geom_errorbar(
          aes(
            ymin = Rata_rata - SE,
            ymax = Rata_rata + SE
          ),
          width = 0.2
        ) +
        labs(
          x = input$x_kn,
          y = paste("Rata-rata", y, "± SE")
        )
    }
    p <- p + theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  output$ringkasan_kn <- renderDT({
    req(data_kn(), input$y_kn)
    df <- data_kn()
    hasil <- df %>%
      group_by(Kelompok) %>%
      summarise(
        Jumlah = n(),
        Rata_rata = mean(.data[[input$y_kn]], na.rm = TRUE),
        Median = median(.data[[input$y_kn]], na.rm = TRUE),
        Minimum = min(.data[[input$y_kn]], na.rm = TRUE),
        Maksimum = max(.data[[input$y_kn]], na.rm = TRUE),
        SD = sd(.data[[input$y_kn]], na.rm = TRUE),
        .groups = "drop"
      )
    datatable(
      hasil,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  # MULTIVARIAT
  data_multi <- reactive({
    req(data_lengkap())
    data_lengkap() %>%
      select(all_of(var_num))
  })
  output$heatmap <- renderPlot({
    req(data_multi())
    validate(
      need(all(sapply(data_multi(), sd) > 0),
           "Terdapat variabel konstan.")
    )
    corrplot(
      cor(data_multi()),
      method = "color",
      type = "lower",
      addCoef.col = "black",
      tl.col = "black",
      tl.srt = 45,
      number.cex = 0.8
    )
  })
  output$pairs_plot <- renderPlot({
    req(data_multi())
    GGally::ggpairs(
      data_multi(),
      title = "Scatterplot Matrix Antarvariabel"
    )
  })
  output$plot_3d <- renderPlotly({
    req(data_lengkap(), input$x_3d, input$y_3d, input$z_3d)
    pilihan <- c(input$x_3d, input$y_3d, input$z_3d)
    validate(
      need(length(unique(pilihan)) == 3,
           "Pilih tiga variabel yang berbeda.")
    )
    df <- data_lengkap()
    plot_ly(
      df,
      x = ~.data[[input$x_3d]],
      y = ~.data[[input$y_3d]],
      z = ~.data[[input$z_3d]],
      color = ~Provinsi,
      type = "scatter3d",
      mode = "markers",
      marker = list(size = 4, opacity = 0.7)
    ) %>%
      layout(
        title = "Visualisasi Hubungan Tiga Variabel",
        scene = list(
          xaxis = list(title = input$x_3d),
          yaxis = list(title = input$y_3d),
          zaxis = list(title = input$z_3d)
        )
      )
  })
  output$korelasi_3d <- renderDT({
    req(data_lengkap(), input$x_3d, input$y_3d, input$z_3d)
    pilihan <- c(input$x_3d, input$y_3d, input$z_3d)
    validate(
      need(length(unique(pilihan)) == 3,
           "Pilih tiga variabel yang berbeda.")
    )
    df <- data_lengkap() %>%
      select(all_of(pilihan))
    hasil <- cor(df)
    datatable(
      round(hasil, 4),
      options = list(dom = "t")
    )
  })
  output$interpretasi_3d <- renderPrint({
    req(data_lengkap(), input$x_3d, input$y_3d, input$z_3d)
    pilihan <- c(input$x_3d, input$y_3d, input$z_3d)
    validate(
      need(length(unique(pilihan)) == 3,
           "Pilih tiga variabel yang berbeda.")
    )
    df <- data_lengkap() %>%
      select(all_of(pilihan))
    hasil <- cor(df)
    cat("Interpretasi korelasi berpasangan:\n\n")
    for (i in 1:2) {
      for (j in (i + 1):3) {
        r <- hasil[i, j]
        kekuatan <- if (abs(r) < 0.2) {
          "sangat lemah"
        } else if (abs(r) < 0.4) {
          "lemah"
        } else if (abs(r) < 0.6) {
          "sedang"
        } else if (abs(r) < 0.8) {
          "kuat"
        } else {
          "sangat kuat"
        }
        arah <- if (r >= 0) "positif" else "negatif"
        cat(
          rownames(hasil)[i], "dengan",
          colnames(hasil)[j], ":",
          round(r, 4), "- hubungan",
          kekuatan, arah, "\n"
        )
      }
    }
    cat("\nCatatan: korelasi menunjukkan hubungan berpasangan,
        bukan hubungan sebab-akibat.")
  })
  # REGRESI: KORELASI PEARSON
  output$korelasi_regresi <- renderDT({
    req(data_reg())
    hasil <- lapply(
      setdiff(var_num, "Produksi"),
      function(x) {
        ct <- cor.test(
          data_reg()[[x]],
          data_reg()$Produksi,
          method = "pearson"
        )
        data.frame(
          Variabel = x,
          Korelasi = unname(ct$estimate),
          P_Value = ct$p.value
        )
      }
    ) %>%
      bind_rows()
    hasil <- hasil %>%
      mutate(
        Interpretasi = ifelse(
          abs(Korelasi) < 0.2, "Sangat lemah",
          ifelse(
            abs(Korelasi) < 0.4, "Lemah",
            ifelse(
              abs(Korelasi) < 0.6, "Sedang",
              ifelse(abs(Korelasi) < 0.8, "Kuat",
                     "Sangat kuat")
            )
          )
        )
      )
    datatable(
      hasil,
      options = list(dom = "t"),
      rownames = FALSE
    )
  })
  output$scatter_regresi <- renderPlot({
    req(data_reg())
    df <- data_reg() %>%
      select(
        Produksi,
        Luas_Panen,
        Curah_hujan,
        Kelembapan,
        Suhu_rata_rata
      )
    GGally::ggpairs(
      df,
      columns = 1:5,
      title = "Scatterplot Hubungan Variabel Regresi"
    )
  })
  # ESTIMASI PARAMETER
  output$metode_hasil <- renderPrint({
    req(model_regresi(), input$metode_estimasi)
    cat("Metode estimasi:", input$metode_estimasi, "\n\n")
    if (input$metode_estimasi ==
        "Ordinary Least Squares (OLS)") {
      cat("Estimasi dilakukan dengan meminimumkan jumlah kuadrat residual.")
    } else if (input$metode_estimasi ==
               "Maximum Likelihood Estimation (MLE)") {
      cat("Dengan asumsi galat berdistribusi normal dan varians konstan,
          estimasi MLE untuk koefisien regresi sama dengan estimasi OLS.")
    } else {
      cat("Metode momen menggunakan persamaan normal untuk memperoleh
          estimasi koefisien. Pada model linear standar, hasilnya sama
          dengan estimasi OLS.")
    }
    cat("\n\nKoefisien model:\n")
    print(coef(model_regresi()))
  })
  output$koef_regresi <- renderDT({
    req(model_regresi())
    hasil <- broom::tidy(model_regresi()) %>%
      mutate(
        across(
          where(is.numeric),
          ~ round(.x, 5)
        )
      )
    datatable(
      hasil,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  output$plot_interval <- renderPlot({
    req(model_regresi())
    ci <- as.data.frame(confint(model_regresi()))
    ci$Parameter <- rownames(ci)
    names(ci)[1:2] <- c("Bawah", "Atas")
    ci$Estimasi <- coef(model_regresi())
    ggplot(
      ci,
      aes(
        x = reorder(Parameter, Estimasi),
        y = Estimasi,
        ymin = Bawah,
        ymax = Atas
      )
    ) +
      geom_pointrange(
        color = biru,
        linewidth = 0.8
      ) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "red"
      ) +
      coord_flip() +
      labs(
        title = "Interval Kepercayaan 95% Parameter",
        x = "Parameter",
        y = "Estimasi Koefisien"
      ) +
      theme_minimal()
  })
  # UJI SIGNIFIKANSI PARSIAL
  output$uji_parsial <- renderDT({
    req(model_regresi())
    hasil <- broom::tidy(model_regresi()) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        Keputusan = ifelse(
          p.value < 0.05,
          "Signifikan",
          "Tidak signifikan"
        )
      ) %>%
      mutate(
        across(
          where(is.numeric),
          ~ round(.x, 5)
        )
      )
    datatable(
      hasil,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  # UJI SIGNIFIKANSI SIMULTAN
  output$uji_simultan <- renderPrint({
    req(model_regresi())
    hasil <- summary(model_regresi())$fstatistic
    f_hitung <- unname(hasil[1])
    df1 <- unname(hasil[2])
    df2 <- unname(hasil[3])
    p_value <- pf(
      f_hitung,
      df1,
      df2,
      lower.tail = FALSE
    )
    cat("F hitung:", round(f_hitung, 5), "\n")
    cat("Derajat bebas pembilang:", df1, "\n")
    cat("Derajat bebas penyebut:", df2, "\n")
    cat("P-value:", format.pval(p_value), "\n\n")
    if (p_value < 0.05) {
      cat("Kesimpulan: variabel independen secara simultan
          berpengaruh signifikan terhadap Produksi.")
    } else {
      cat("Kesimpulan: variabel independen secara simultan
          tidak signifikan terhadap Produksi.")
    }
  })
  # UJI MULTIKOLINEARITAS VIF
  output$uji_multikolinearitas <- renderDT({
    req(model_regresi())
    nilai_vif <- car::vif(model_regresi())
    hasil <- data.frame(
      Variabel = names(nilai_vif),
      VIF = as.numeric(nilai_vif)
    ) %>%
      mutate(
        Keterangan = ifelse(
          VIF < 5,
          "Tidak terdapat indikasi multikolinearitas kuat",
          "Terdapat indikasi multikolinearitas"
        ),
        VIF = round(VIF, 4)
      )
    datatable(
      hasil,
      options = list(
        pageLength = 5,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  # UJI NORMALITAS KOLMOGOROV-SMIRNOV
  output$uji_normalitas <- renderPrint({
    req(model_regresi())
    residu <- residuals(model_regresi())
    cat("Uji Kolmogorov-Smirnov dengan koreksi Lilliefors\n")
    cat("Koreksi digunakan karena parameter distribusi normal
        diestimasi dari data residual.\n\n")
    hasil <- nortest::lillie.test(residu)
    print(hasil)
    if (hasil$p.value > 0.05) {
      cat("\nKesimpulan: tidak terdapat bukti yang cukup
          untuk menolak normalitas residual.")
    } else {
      cat("\nKesimpulan: residual menunjukkan penyimpangan
          dari distribusi normal.")
    }
  })
  output$plot_normalitas <- renderPlot({
    req(model_regresi())
    qqnorm(
      residuals(model_regresi()),
      main = "Normal Probability Plot"
    )
    qqline(
      residuals(model_regresi()),
      col = "red",
      lwd = 2
    )
  })
  # UJI INDEPENDENSI DURBIN-WATSON
  output$uji_independen <- renderPrint({
    req(model_regresi())
    hasil <- lmtest::dwtest(model_regresi())
    print(hasil)
    if (hasil$p.value > 0.05) {
      cat("\nTidak terdapat bukti autokorelasi orde pertama.")
    } else {
      cat("\nTerdapat indikasi autokorelasi orde pertama.")
    }
    cat("\nPerhatikan bahwa data ini memiliki pengamatan
        berulang berdasarkan provinsi dan tahun.")
  })
  output$plot_acf <- renderPlot({
    req(model_regresi())
    df <- data_reg() %>%
      mutate(
        Residual = residuals(model_regresi())
      ) %>%
      arrange(Provinsi, Tahun)
    acf(
      df$Residual,
      main = "ACF Residual"
    )
  })
  # UJI IDENTIK GLEJSER
  model_glejser <- reactive({
    req(model_regresi(), data_reg())
    df <- data_reg() %>%
      mutate(
        Abs_Residual = abs(residuals(model_regresi()))
      )
    lm(
      Abs_Residual ~ Luas_Panen + Curah_hujan +
        Kelembapan + Suhu_rata_rata,
      data = df
    )
  })
  output$uji_identik <- renderPrint({
    req(model_glejser())
    cat("Uji Glejser: regresi nilai absolut residual
        terhadap variabel independen.\n\n")
    print(summary(model_glejser()))
    cat("\nUji simultan Glejser:\n")
    print(anova(model_glejser()))
    p <- pf(
      summary(model_glejser())$fstatistic[1],
      summary(model_glejser())$fstatistic[2],
      summary(model_glejser())$fstatistic[3],
      lower.tail = FALSE
    )
    cat("\nP-value uji simultan:", format.pval(p), "\n")
    if (p > 0.05) {
      cat("Tidak terdapat indikasi heteroskedastisitas
          berdasarkan uji Glejser simultan.")
    } else {
      cat("Terdapat indikasi heteroskedastisitas
          berdasarkan uji Glejser simultan.")
    }
  })
  output$plot_identik <- renderPlot({
    req(model_regresi())
    df <- data.frame(
      Observasi = seq_along(residuals(model_regresi())),
      Residual = residuals(model_regresi())
    )
    ggplot(
      df,
      aes(x = Observasi, y = Residual)
    ) +
      geom_point(color = biru, alpha = 0.7) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "red"
      ) +
      geom_smooth(
        method = "loess",
        se = FALSE,
        color = "black"
      ) +
      labs(
        title = "Plot Observasi dan Residual",
        x = "Urutan Observasi",
        y = "Residual"
      ) +
      theme_minimal()
  })
  # PLOT TIME SERIES RESIDUAL PER PROVINSI
  output$plot_observasi_residual <- renderPlot({
    req(model_regresi())
    df <- data_reg() %>%
      mutate(
        Residual = residuals(model_regresi())
      ) %>%
      arrange(Provinsi, Tahun)
    ggplot(
      df,
      aes(
        x = Tahun,
        y = Residual,
        color = Provinsi,
        group = Provinsi
      )
    ) +
      geom_line() +
      geom_point(size = 1.5) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "black"
      ) +
      labs(
        title = "Residual Berdasarkan Tahun dan Provinsi",
        x = "Tahun",
        y = "Residual",
        color = "Provinsi"
      ) +
      theme_minimal()
  })
  # NILAI R-SQUARED, ADJUSTED R-SQUARED, DAN RMSE
  output$r_squared <- renderValueBox({
    req(model_regresi())
    valueBox(
      paste0(
        round(summary(model_regresi())$r.squared * 100, 2),
        "%"
      ),
      "R-Squared",
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  output$adj_r_squared <- renderValueBox({
    req(model_regresi())
    valueBox(
      paste0(
        round(summary(model_regresi())$adj.r.squared * 100, 2),
        "%"
      ),
      "Adjusted R-Squared",
      icon = icon("chart-area"),
      color = "aqua"
    )
  })
  output$rmse <- renderValueBox({
    req(model_regresi())
    nilai <- sqrt(
      mean(residuals(model_regresi())^2)
    )
    valueBox(
      round(nilai, 3),
      "RMSE",
      icon = icon("calculator"),
      color = "green"
    )
  })
  # AKTUAL VS PREDIKSI
  output$aktual_prediksi <- renderPlotly({
    req(model_regresi())
    df <- data.frame(
      Aktual = model_regresi()$model$Produksi,
      Prediksi = fitted(model_regresi())
    )
    p <- ggplot(
      df,
      aes(x = Aktual, y = Prediksi)
    ) +
      geom_point(
        color = biru,
        size = 2.5,
        alpha = 0.7
      ) +
      geom_abline(
        intercept = 0,
        slope = 1,
        linetype = "dashed",
        color = "red"
      ) +
      labs(
        title = "Nilai Aktual vs Prediksi",
        x = "Produksi Aktual",
        y = "Produksi Prediksi"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  # EMPAT PLOT DIAGNOSTIK
  output$diagnostik <- renderPlot({
    req(model_regresi())
    par(mfrow = c(2, 2))
    plot(model_regresi(), which = 1)
    plot(model_regresi(), which = 2)
    plot(model_regresi(), which = 3)
    plot(model_regresi(), which = 5)
    par(mfrow = c(1, 1))
  })
}

# 7. JALANKAN DASHBOARD
shinyApp(ui = ui, server = server)

