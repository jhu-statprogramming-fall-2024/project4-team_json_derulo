install.packages("s3")
library(s3)

create_clinic <- function(name, address, zip, open_hours = NULL, max_GAw, max_GAd = NULL, phone) {
  structure(
    list(
      name = name,
      address = address,
      zip = zip,
      open_hours = open_hours,
      max_GAw = max_GAw,
      max_GAd = max_GAd, 
      phone = phone
    ),
    class = "clinic"
  )
}

print.clinic <- function(clinic) {
  paste("Clinic:", clinic$name)
}

dupont_clinic <- create_clinic(name = "DuPont Clinic", address = "1120 19th St NW", zip = 20036, open_hours = "M-F 0900 - 1700, Sa 1000 - 1400", max_GAw = 32, max_GAd = 6, phone = "202-844-2004")
carafem <- create_clinic(name = "carafem Health Center", address = "5530 Wisconsin Ave, Suite 1200", zip = 20815, max_GAw = 13, max_GAd = 0, phone = "855-SAY-CARA" )
CARE <- create_clinic(name = "Clinics for Abortion & Reproductive Excellence", address = "10401 Old Georgetown Rd, Suite 104", zip = 20814, max_GAw = 35, max_GAd = 0, open_hours = "M 0900 - 1700, T-F 0830 - 1700, Sa 0900 - 1530", phone = "301-517-6810")