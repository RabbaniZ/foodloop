library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)

# CSV file path for seller data
seller_file <- "seller_data.csv"

# Create empty seller file if it doesn't exist
if (!file.exists(seller_file)) {
  write.csv(data.frame(
    wilayah = character(),
    restoran = character(),
    makanan_tersedia = numeric(),
    harga = numeric(),
    stringsAsFactors = FALSE
  ), seller_file, row.names = FALSE)
}

# Built-in restaurant data
resto_data <- data.frame(
  wilayah = c("Depok", "Umbulharjo", "Kraton"),
  restoran = c("Warung SS", "Gudeg Yu Djum", "Soto Mbah Katro"),
  makanan_tersedia = c(10, 5, 7),
  harga = c(15000, 20000, 12000),
  stringsAsFactors = FALSE
)

# Load seller data
loadSellerData <- function() {
  if (file.exists(seller_file)) {
    read.csv(seller_file, stringsAsFactors = FALSE)
  } else {
    data.frame()
  }
}

# UI
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      tags$img(src = "logo_reverse.png", height = "90px", style = "margin-right:10px; margin-top:-23px;"),
    )
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML("
      body, .content-wrapper, .right-side {
        background-color: #ffffff !important;
      }
    
      .skin-blue .main-header .navbar {
        background-color: #f39c12; /* Yellow-orange */
      }
    
      .skin-blue .main-header .logo {
        background-color: #e67e22; /* Darker orange */
      }
    
     .login-box {
        box-shadow: 0 12px 25px rgba(0, 0, 0, 0.1);
        background: white;
        border-radius: 16px;
        padding: 40px;
        max-width: 400px;
        width: 90%;
        text-align: center;
        transition: transform 0.2s ease;
      }
      .login-box:hover {
        transform: translateY(-5px);
        box-shadow: 0 20px 40px rgba(0, 0, 0, 0.2);
      }
      
      .btn-login {
        background-color: #f39c12 !important;
        color: white !important;
        border: 2px solid #f39c12 !important;
        border-radius: 8px !important;
        width: 100%;
        font-weight: bold;
        margin-top: 10px;
      }
    
      .btn-login:hover {
        background-color: white !important;
        color: #f39c12 !important;
        border-color: #f39c12 !important;
      }
    
      .btn-outline-yellow {
        background-color: #ffffff !important;
        color: #f39c12 !important;
        border: 2px solid #f39c12 !important;
        font-weight: bold;
        border-radius: 8px !important;
      }
    
      .btn-outline-yellow:hover {
        background-color: #f39c12 !important;
        color: white !important;
      }
      
            body {
        background-color: white !important;
      }
      
      input[type=text], input[type=number], .form-control {
        border: 2px solid #f39c12 !important;
        border-radius: 8px !important;
        padding: 8px;
      }
      
      .btn-orange {
        background-color: #f39c12 !important;
        color: white !important;
        border: none;
      }
      
      .btn-orange:hover {
        background-color: white !important;
        color: #f39c12 !important;
        border: 2px solid #f39c12 !important;
      }
      
      .dataTable thead th {
        background-color: #fff3e0;
        color: #e65100;
        font-weight: bold;
        text-align: center;
      }
      .dataTable tbody tr:nth-child(odd) {
        background-color: #fdf6ed;
      }
      .dataTable tbody tr:hover {
        background-color: #ffecb3;
        cursor: pointer;
      }
      .btn-logout {
        background-color: white;
        color: #f39c12;
        border: 2px solid #f39c12;
        padding: 6px 15px;
        border-radius: 8px;
        font-weight: bold;
        transition: 0.3s ease;
      }
      
      .btn-logout:hover {
        background-color: #f39c12;
        color: white;
        border-color: #f39c12;
        box-shadow: 0 0 6px rgba(243, 156, 18, 0.4);
      }
      .login-wrapper {
        background: linear-gradient(to bottom right, #fff7e6, #ffe0b3);
        height: 100vh;
        display: flex;
        justify-content: center;
        align-items: center;
      }
       input[type='text'],
       input[type='password'],
        input[type='number'] {
        width: 100% !important;
        box-sizing: border-box;
      }

    "))),
    uiOutput("main_ui")
  )
)

# Server
server <- function(input, output, session) {
  user <- reactiveValues(
    authenticated = FALSE,
    role = NULL,
    wilayah = NULL
  )
  
  # UI screen manager
  output$main_ui <- renderUI({
    if (!user$authenticated && is.null(user$role)) {
      tags$div(style = "
            height: 100vh;
            display: flex;
            align-items: center;
            justify-content: center;
            background-color: #ffffff;",
               tags$div(class = "login-box", style = "text-align: center;",
                        tags$img(src = "logo.png", width = "240px", style = "margin-bottom: 10px;"),
                        h3("Login as..."),
                        div(style = "display: flex; gap: 20px; justify-content: center; margin-top: 20px;",
                            actionButton("btn_buyer", "Login as Buyer", class = "btn btn-outline-yellow"),
                            actionButton("btn_seller", "Login as Seller", class = "btn btn-outline-yellow")
                        )
               )
      )
    }
    # Login form screen (username/password)
    else if (!user$authenticated) {
      tags$div(style = "
            height: 100vh;
            display: flex;
            align-items: center;
            justify-content: center;
            background-color: #ffffff;",
               tags$div(class = "login-box", style = "text-align: center;",
                        tags$img(src = "logo.png", width = "200px", style = "margin-bottom: 20px;"),
                        h3(paste("Login", toupper(user$role))),
                        textInput("username", label = NULL, placeholder = "Username", width = "100%"),
                        passwordInput("password", label = NULL, placeholder = "Password", width = "100%"),
                        actionButton("login", "Login", class = "btn btn-login")
               )
      )
    } else if (!user$authenticated) {
      div(class = "login-box",
          h3(paste("Login", toupper(user$role))),
          textInput("username", "Username"),
          passwordInput("password", "Password"),
          actionButton("login", "Login", class = "btn btn-primary"))
    } else if (user$role == "buyer") {
      tagList(
        div(
          style = "display: flex; justify-content: flex-end; max-width: 1000px; margin: 10px auto;",
          actionButton("logout", "Logout", class = "btn btn-logout")
        ),
        
        # Card: Wilayah Selector
        div(
          style = "max-width: 700px; margin: 40px auto; background: #fff; padding: 25px; border-radius: 12px; box-shadow: 0 4px 10px rgba(0,0,0,0.1);",
          h3(style = "text-align: center; font-weight: bold; margin-bottom: 25px;", "Pilih Wilayah"),
          div(style = "display: flex; align-items: center; gap: 12px;",
              div(style = "flex: 1;",
                  selectInput("wilayah", label = "Pilih Wilayah", choices = unique(c(resto_data$wilayah, loadSellerData()$wilayah)))
              ),
              actionButton("lihat_data", "Cari Makanan",
                           class = "btn btn-orange",
                           style = "height: 39px; padding: 6px 20px; font-weight: bold; border-radius: 8px;")
          )
        ),
        
        # Card: Result - either show table or default friendly text
        div(
          style = "max-width: 800px; margin: 20px auto; background: #fff; padding: 25px; border-radius: 12px; box-shadow: 0 4px 10px rgba(0,0,0,0.1);",
          conditionalPanel(
            condition = "input.lihat_data == 0",
            div(
              style = "text-align: center; font-size: 20px; font-weight: bold; color: #555;",
              "🤔 Mau makan apa hari ini? Pilih wilayah dulu yuk~"
            )
          ),
          conditionalPanel(
            condition = "input.lihat_data > 0",
            tagList(
              DTOutput("tabel_buyer"),
              uiOutput("form_pesan"),
              verbatimTextOutput("status_pesanan")
            )
          )
        )
      )
    } else if (user$role == "seller") {
      tagList(
        # 🔸 Logout Button (top-right aligned)
        div(
          style = "display: flex; justify-content: flex-end; max-width: 1000px; margin: 10px auto;",
          actionButton("logout", "Logout", class = "btn btn-logout")
        ),
        
        # 🔸 Main Seller UI Content
        div(class = "data-box",
            
            # Logo & Title
            h3(style = "text-align:center; margin-bottom: 30px;", "Tambah Makanan untuk Dijual"),
            
            # Card for the form
            div(style = "background: #fff; padding: 30px; border-radius: 12px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); max-width: 500px; margin: 0 auto;",
                textInput("wilayah_input", "Wilayah", placeholder = "Contoh: Depok", width = "10000%"),
                textInput("resto_input", "Nama Restoran", placeholder = "Contoh: Warung Bu Tini", width = "1000%"),
                numericInput("stok_input", "Jumlah Porsi", value = 1, min = 1, width = "100%"),
                numericInput("harga_input", "Harga per Porsi", value = 10000, min = 1000, width = "100%"),
                actionButton("submit_makanan", "Tambah ke Daftar", class = "btn btn-orange", style = "margin-top: 15px; width: 100%;")
            ),
            
            br(), br(),
            
            # Table section
            h3("📋 Daftar Makanan yang Anda Jual", style = "font-weight: bold; margin-top: 30px; margin-bottom: 20px;"),
            div(class = "box", 
                style = "padding: 20px; border-radius: 10px; background: #fff; box-shadow: 0 0 10px rgba(0,0,0,0.05);",
                DTOutput("tabel_seller")
            )
        )
      )
    }
  })
  
  # Button to pick role
  observeEvent(input$btn_buyer, {
    user$role <- "buyer"
  })
  observeEvent(input$btn_seller, {
    user$role <- "seller"
  })
  
  observeEvent(input$logout, {
    user$authenticated <- FALSE
    user$role <- NULL
  })
  
  # Login credentials
  observeEvent(input$login, {
    valid <- (user$role == "buyer" && input$username == "buyer" && input$password == "buyer") ||
      (user$role == "seller" && input$username == "seller" && input$password == "seller")
    if (valid) {
      user$authenticated <- TRUE
    } else {
      showModal(modalDialog("Login gagal. Username/password salah."))
    }
  })
  
  # Buyer Table
  buyer_data <- eventReactive(input$lihat_data, {
    df <- rbind(resto_data, loadSellerData())
    subset(df, wilayah == input$wilayah)
  })
  
  output$tabel_buyer <- renderDT({
    req(buyer_data())
    datatable(
      buyer_data(),
      selection = "single",
      options = list(
        pageLength = 5,
        columnDefs = list(
          list(className = 'dt-center', targets = c(2, 3))
        )
      ),
      rownames = FALSE,
      colnames = c("Wilayah", "Restoran", "Makanan Tersedia", "Harga")
    ) %>% 
      formatCurrency("harga", "Rp ", digits = 0, mark = ".")
  })
  
  # Buyer Order Form
  output$form_pesan <- renderUI({
    if (length(input$tabel_buyer_rows_selected) == 0) return(NULL)
    resto <- buyer_data()[input$tabel_buyer_rows_selected, ]
    div(style = "max-width: 400px; margin: 30px auto; padding: 20px; background: #fff; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);",
        h4(paste("Pemesanan dari:", resto$restoran), style = "margin-bottom: 20px; text-align: center;"),
        numericInput("jumlah_beli", "Jumlah Porsi:", 1, min = 1, max = resto$makanan_tersedia, width = "100%"),
        actionButton("pesan", "Pesan Sekarang", class = "btn btn-orange", style = "width: 100%; margin-top: 10px;")
    )
  })

  # Buyer Order Logic
  output$status_pesanan <- renderText({
    req(input$pesan)
    isolate({
      if (length(input$tabel_buyer_rows_selected) == 0) {
        return("❌ Silakan pilih makanan dulu.")
      }
      
      # Load the seller data (with the latest updates)
      data <- read.csv(seller_file, stringsAsFactors = FALSE)
      
      # Get the selected restaurant from the displayed data
      selected_index <- input$tabel_buyer_rows_selected
      resto <- buyer_data()[selected_index, ]
      
      # Find the matching row in seller data
      seller_row_index <- which(data$restoran == resto$restoran & data$wilayah == resto$wilayah)
      
      # Ensure there's a valid 'makanan_tersedia' value
      if (is.na(data$makanan_tersedia[seller_row_index]) || data$makanan_tersedia[seller_row_index] < 0) {
        return("❌ Stok tidak tersedia.")
      }
      
      jumlah <- input$jumlah_beli
      if (jumlah <= 0) {
        return("❌ Jumlah pesanan harus lebih besar dari 0.")
      }
      
      if (jumlah > data$makanan_tersedia[seller_row_index]) {
        return("❌ Stok tidak cukup.")
      }
      
      # Deduct the stock after a successful order
      data$makanan_tersedia[seller_row_index] <- data$makanan_tersedia[seller_row_index] - jumlah
      
      # Save the updated data back to the CSV
      write.csv(data, seller_file, row.names = FALSE)
      
      # Return confirmation of the order
      total <- jumlah * resto$harga
      paste("✅ Pesanan berhasil!\n",
            "Ambil di:", resto$restoran,
            "\nJumlah:", jumlah,
            "\nTotal bayar: Rp", format(total, big.mark = ".", scientific = FALSE))
    })
  })

  
  # Seller input
  observeEvent(input$submit_makanan, {
    new_row <- data.frame(
      wilayah = input$wilayah_input,
      restoran = input$resto_input,
      makanan_tersedia = input$stok_input,
      harga = input$harga_input,
      stringsAsFactors = FALSE
    )
    seller_data <- loadSellerData()
    seller_data <- rbind(seller_data, new_row)
    write.csv(seller_data, seller_file, row.names = FALSE)
    showModal(modalDialog("✅ Data makanan berhasil ditambahkan!"))
  })
  
  output$tabel_seller <- renderDT({
    datatable(
      loadSellerData(),
      options = list(
        pageLength = 5,
        columnDefs = list(
          list(className = 'dt-center', targets = c(2, 3))
        )
      ),
      rownames = FALSE,
      colnames = c("Wilayah", "Restoran", "Makanan Tersedia", "Harga")
    ) %>%
      formatCurrency("harga", currency = "Rp ", digits = 0, mark = ".")
  })
}

shinyApp(ui, server)