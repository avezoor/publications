# Memuat library yang diperlukan
library(readxl)  # Untuk membaca file Excel
library(dplyr)   # Untuk manipulasi data 

# Menentukan path file Excel yang akan dibaca
file_path <- ""  # Ganti dengan path file Excel Anda
data <- read_excel(file_path, sheet = "Dataset")  # Membaca sheet bernama "Dataset"

# Melihat nama-nama kolom dalam dataset
colnames(data)

# Daftar kolom yang akan dianalisis
columnsToAnalyze <- c("Kolom_satu", "Kolom_2")

# Fungsi untuk menghitung modus (nilai yang paling sering muncul)
mode_function <- function(x) {
  uniq_x <- unique(x)  # Menemukan nilai unik
  uniq_x[which.max(tabulate(match(x, uniq_x)))]  # Nilai dengan frekuensi tertinggi
}

# Melakukan iterasi untuk setiap kolom dalam daftar kolom yang akan dianalisis
for (column_name in columnsToAnalyze) {
  # Mengecek apakah kolom ada dalam dataset
  if (!column_name %in% colnames(data)) {
    cat("Kolom", column_name, "tidak ditemukan dalam dataset.\n")
    next  # Lompat ke kolom berikutnya jika kolom tidak ditemukan
  }
  
  # Mengambil data kolom
  z <- data[[column_name]]
  
  # Mengecek apakah data dalam kolom bersifat numerik
  if (!is.numeric(z)) {
    cat("Kolom", column_name, "bukan numerik.\n")
    next  # Lompat ke kolom berikutnya jika bukan numerik
  }
  
  # Menampilkan statistik deskriptif untuk kolom
  cat("Statistik untuk kolom", column_name, ":\n")
  cat("  Rata-rata (Mean):", mean(z, na.rm = TRUE), "\n")
  cat("  Median:", median(z, na.rm = TRUE), "\n")
  cat("  Modus:", mode_function(z), "\n")
  cat("  Standar Deviasi (SD):", sd(z, na.rm = TRUE), "\n")
  cat("  Varians:", var(z, na.rm = TRUE), "\n")
  cat("  Nilai Minimum:", min(z, na.rm = TRUE), "\n")
  cat("  Nilai Maksimum:", max(z, na.rm = TRUE), "\n")
  cat("  Rentang (Range):", max(z, na.rm = TRUE) - min(z, na.rm = TRUE), "\n")
  cat("  Kuartil 1 (25%):", quantile(z, probs = 0.25, na.rm = TRUE), "\n")
  cat("  Kuartil 3 (75%):", quantile(z, probs = 0.75, na.rm = TRUE), "\n")
  cat("  Disentil (10%):", quantile(z, probs = 0.10, na.rm = TRUE), "\n")
  cat("  Persentil (1%):", quantile(z, probs = 0.01, na.rm = TRUE), "\n")
  
  # Membuat histogram untuk kolom
  hist(z, 
       main = paste("Histogram dari", column_name),  # Judul histogram
       xlab = paste("Nilai", column_name),          # Label sumbu x
       col = "lightblue",                           # Warna batang histogram
       border = "black")                            # Warna garis tepi
  
  # Menunggu input dari pengguna untuk melanjutkan ke kolom berikutnya
  readline(prompt = "Tekan [Enter] untuk melanjutkan ke kolom berikutnya...")
}
