install.packages("s3")
library(s3)

create_clinic <- function(name, address, zip, open_hours = NULL, max_GAw, max_GAd = NULL, phone, lat, long) {
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

dupont_clinic <- create_clinic(name = "DuPont Clinic", 
                               address = "1120 19th St NW", 
                               zip = 20036, 
                               open_hours = "M-F 0900 - 1700, Sa 1000 - 1400",
                               max_GAw = 32,
                               max_GAd = 6, 
                               phone = "202-844-2004",
                               lat = 38.90478876534545,
                               long = -77.04373883435558)
carafem <- create_clinic(name = "carafem Health Center", 
                         address = "5530 Wisconsin Ave, Suite 1200", 
                         zip = 20815, 
                         max_GAw = 13, 
                         max_GAd = 0, 
                         phone = "855-SAY-CARA",
                         lat = 38.96496013009437,
                         long = -77.08803977909659)
CARE <- create_clinic(name = "Clinics for Abortion & Reproductive Excellence",
                      address = "10401 Old Georgetown Rd, Suite 104",
                      zip = 20814,
                      max_GAw = 35,
                      max_GAd = 0,
                      open_hours = "M 0900 - 1700, T-F 0830 - 1700, Sa 0900 - 1530",
                      phone = "301-517-6810",
                      lat = 39.02660039064823,
                      long = -77.12531972883426)
partners <- create_clinic(name = "Partners in Abortion Care",
                          address = "7305 Baltimore Ave, Suite 107",
                          zip = 20740,
                          max_GAw = 34,
                          max_GAd = 0,
                          phone = "301-932-1222",
                          lat = 38.979095445474755,
                          long = -76.9368688521127)
whole_womans_baltimore <- create_clinic(name = "Whole Woman's Health of Baltimore",
                              address = "7648 Belair Rd, Floor 1",
                              zip = 21236,
                              max_GAw = 22,
                              max_GAd = 0,
                              phone = "877-835-1090",
                              lat = 39.367552304718366,
                              long = -76.52029503491501)
womens_hc_md <- create_clinic(name = "Women's Health Center of Maryland",
                              address = "17204 McMullen Hwy SW",
                              zip = 21557,
                              max_GAw = 16,
                              max_GAd = 0,
                              phone = "301-709-5101",
                              lat = 39.562494789850064,
                              long = -78.85909350884934, 
                              open_hours = "M-Th 0800 - 1700, F 0800-1300")
wc_raleigh <- create_clinic(name = "A Woman's Choice of Raleigh",
                            address = "3305 Drake Cir",
                            zip = 27607,
                            max_GAw = 12,
                            max_GAd = 0,
                            phone = "919-781-6811",
                            lat = 35.814774106462025,
                            long = -78.6940318539617,
                            open_hours = "M-Th 0800 - 1700, F 0800 - 1600, Sa 0800 - 1200")
wc_greensboro <- create_clinic(name = "A Woman's Choice of Greensboro",
                               address = "2425 Randleman Rd",
                               zip = 27406,
                               max_GAw = 12,
                               max_GAd = 0,
                               phone = "336-273-9465",
                               lat = 36.03672864287146, 
                               long = -79.79903671534046,
                               open_hours = "M-Th 0800 - 1700, F 0800 - 1600, Sa 0800 - 1200")
wc_charlotte <- create_clinic(name = "A Woman's Choice of Charlotte",
                              address = "421 N Wendover Rd",
                              zip = 28211,
                              phone = "704-367-2255",
                              open_hours = "M-Th 0800 - 1700, F 0800 - 1600, Sa 0800 - 1200",
                              max_GAw = 12,
                              max_GAd = 0,
                              lat = 35.19012178939107,
                              long = -80.80493254840565)

a_capital_whc <- create_clinic(name = "A Capital Women's Health Clinic",
                               address = "1511 Starling Dr",
                               zip = 23229,
                               phone = "804-754-1928",
                               open_hours = "M-F 0800 - 1700, Sa 0900 - 1200",
                               max_GAw = 16,
                               max_GAd = 0,
                               lat = 37.6056619856187,
                               long = -77.56699732432077)
wc_danville <- create_clinic(name = "A Woman's Choice of Danville",
                             address = "159 Executive Dr, Suite E",
                             zip = 24541,
                             phone = "434-218-1032",
                             open_hours = "M-Th 0800 - 1700, F 0800 - 1600, Sa 0800 - 1200",
                             max_GAw = 15,
                             max_GAd = 0,
                             lat = 36.59342415106964,
                             long = -79.43234447301381)
bristol_wh <- create_clinic(name = "Bristol Women's Health",
                            address = "2603 Osborne St",
                            zip = 24201,
                            phone = "276-494-0501",
                            open_hours = "M-F 0800 - 1700, Sa = 0800 - 1200",
                            max_GAw = 16,
                            max_GAd = 0,
                            lat = 36.60156709693608,
                            long = -82.21738143120973)
falls_church_hc <- create_clinic(name = "Falls Church Healthcare Center",
                                 address = "900 S Washington St, Suite 300",
                                 zip = 22046,
                                 max_GAw = 15,
                                 max_GAd = 6,
                                 phone = "703-532-2500",
                                 lat = 38.87943123725555,
                                 long = -77.18239460563538)
meadow <- create_clinic(name = "Meadow Reproductive Health & Wellness",
                        address = "1749 Old Meadow Rd, #600",
                        zip = 22102,
                        phone = "703-783-3300",
                        max_GAw = 12,
                        max_GAd = 0,
                        open_hours = "M-F 0930 - 1700",
                        lat = 38.922101211643124,
                        long = -77.21019365166195)

whole_womans_alexandria <- create_clinic(name = "Whole Woman's Health of Alexandria",
                                         address = "2839 Duke St",
                                         zip = 22314,
                                         phone = "703-667-4064",
                                         open_hours = "M-F 0900 - 1700, Sa 0830 - 1500",
                                         max_GAw = 18,
                                         max_GAd = 0,
                                         lat = 38.80787625143519,
                                         long = -77.07899630133653)
whole_womans_charlottesville <- create_clinic(name = "Whole Woman's Health of Charlottesville",
                                              address = "2321 Commonwealth Dr",
                                              zip = 22901,
                                              phone = "434-973-4888",
                                              open_hours = "M-F 0800 - 1700",
                                              max_GAw = 18,
                                              max_GAd = 0,
                                              lat = 38.07390158589749,
                                              long = -78.48611373072481)















