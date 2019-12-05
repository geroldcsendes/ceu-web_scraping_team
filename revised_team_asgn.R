library(xml2)
library(rvest)
library(comprehenr)
library(data.table)

# This is my and Oscar Leal's code for scraping ultimatespecs
# Before the `carspecs` function we experimented with the website and set concluded the logic for the function
# If you don't want to deal with this then proceed to `carspecs`

# Let's take a look at Porsche models
url <- "https://www.ultimatespecs.com/car-specs/Porsche"
t <- read_html(url)
write_html(t, "cars/porsche.html")

porsche_urls <- t %>% html_nodes('.someOtherRow a') %>% 
  html_attr('href')

porsche_urls <- to_vec(for(x in porsche_urls) if (endsWith(x,'.html')) x)

length(porsche_urls)
porsche_urls

porsche_make_year <- t %>% html_nodes('.yearText') %>% 
  html_text()
porsche_make_year

length(porsche_make_year)

porsche_urls[1]

root_url <- "https://www.ultimatespecs.com"
type_url <- paste0(root_url, '/car-specs/Porsche/109463/Porsche-Panamera-Sport-Turismo-4S-.html')


car_name <- read_html(type_url) %>% 
  html_nodes(".page_ficha_title_text span") %>% 
  html_text()
car_name

#subvers_specs_info$carname <- car_name # Assign to df

production_years <- read_html(type_url) %>% 
  html_nodes("strong") %>% 
  html_text()

car_id <- paste0(car_name, production_years)
car_id

specs_name <- read_html(type_url)
specs_name <- specs_name %>% html_nodes('.tabletd')  %>% 
  html_text()

specs_value <- read_html(type_url)
specs_value <- specs_value %>% html_nodes('.tabletd_right')  %>% 
  html_text()

length(specs_name)
length(specs_value)

specs_name <- head(specs_name, -1)
length(specs_name)
length(specs_value)

myx <- data.frame(specs_value)
myx
View(myx)

myx$specs_name <- specs_name
myx$car_id <- car_id
View(myx)

x_spread <- reshape(myx, idvar = "car_id", timevar = "specs_name", direction = "wide")
View(x_spread)
colnames(x_spread)
colnames(x_spread) <- c('Car', specs_name)
View(x_spread)


get_type_specs <- function(url) {
  
  root_url <- "https://www.ultimatespecs.com"
  type_url <- paste0(root_url, url)
  print(type_url)
  
  car_name <- read_html(type_url) %>% 
    html_nodes(".page_ficha_title_text span") %>% 
    html_text()
  print(car_name)
  # Trim whitespaces
  car_name <- trimws(car_name)
  
  #subvers_specs_info$carname <- car_name # Assign to df
  
  production_years <- read_html(type_url) %>% 
    html_nodes("strong") %>% 
    html_text()
  # Trim
  production_years <- trimws(production_years)
  
  car_id <- paste0(car_name, production_years)
  print(car_id)
  #car_id
  
  specs_name <- read_html(type_url)
  specs_name <- specs_name %>% html_nodes('.tabletd')  %>% 
    html_text()
  # Clean
  specs_name <- trimws(gsub('\n', ' ', specs_name))
  specs_name <- gsub(':', '', specs_name)
  specs_name <- head(specs_name, -1)
  
  specs_value <- read_html(type_url)
  specs_value <- specs_value %>% html_nodes('.tabletd_right')  %>% 
    html_text()
  
  print(paste0("specs length:", length(specs_name)))
  print(paste0("values length", length(specs_value)))
  
  # Make df
  
  if (length(specs_name) == length(specs_value)){
    tmp_df <- data.frame(specs_value)
    
    tmp_df$specs_name <- specs_name
    tmp_df$car_id <- car_id
    #tmp_df$url <- type_url
    
    tmp_df <- reshape(tmp_df, idvar = "car_id", timevar = "specs_name", direction = "wide")
    colnames(tmp_df) <- c('Car', specs_name)
    
    return(tmp_df)
  }
  else {
  }
  
}

mytry <- '/car-specs/Porsche/109463/Porsche-Panamera-Sport-Turismo-4S-.html'
mytry_res <- get_type_specs(mytry)
View(mytry_res)

mytry2 <- '/car-specs/Porsche/112464/Porsche-Cayenne-III-.html'
mytry_res2 <- get_type_specs(mytry2)
View(mytry_res2)

tryset <- porsche_urls[c(1,31,100)]
tryset <- '/car-specs/Porsche/109463/Porsche-Panamera-Sport-Turismo-4S-.html'

myres <- lapply(tryset, get_type_specs)

view_res <- rbindlist(myres, fill = T)
View(view_res)

a <- 'dsfjsadkgé : '
a <- gsub(':', '', a)
a

get_specs <- function(make) {
  
  # Given make with a capital starting letter like: Porsche
  make_url <- paste0("https://www.ultimatespecs.com/car-specs/", make)
  
  # Get types in a make
  t <- read_html(make_url)
  
  type_urls <- t %>% html_nodes('.someOtherRow a') %>% 
    html_attr('href')
 
  type_urls <- to_vec(for(x in type_urls) if (endsWith(x,'.html')) x) 
  type_urls <- type_urls[1:100]
  res <- lapply(type_urls, get_type_specs)
  
  res <- rbindlist(res, fill = T)
  
  return (res)
}


porsche <- get_specs('Porsche')


saveRDS(porsche, "porsche_full_specs.RDS")
