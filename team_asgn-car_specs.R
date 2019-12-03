library(xml2)
library(rvest)
library(comprehenr)

# This is my and Oscar Leal's code for scraping ultimatespecs
# Before the `carspecs` function we experimented with the website and set concluded the logic for the function
# If you don't want to deal with this then proceed to `carspecs`

# Let's take a look at Porsche models
url <- "https://www.ultimatespecs.com/car-specs/Porsche-models"
t <- read_html(url)
write_html(t, "cars/porsche.html")

porsche_urls <- t %>% html_nodes('.col-4') %>% 
  html_attr('href')
porsche_urls

# Upon close inspection, one can see that the relative URLs takes the 'root' URL 
# (www.ultimatespecs.com/) as the base and not the current URL
root_url <- "https://www.ultimatespecs.com"
boxster_url <- paste0(root_url, porsche_urls[1])
boxster_url

# Get html for Porsche Boxer
t_boxster <- read_html(boxster_url)
write_html(t_boxster, "cars/boxster.html")

# Get the types of the Boxster like 718, 982 etc..
boxter_types <- t_boxster %>% html_nodes('.col-4') %>% 
  html_attr('href')

boxter_types

# Get versions of a Boxster type, like 718 Boxster GTS/GTS PDK
boxster_version_url <- paste0(root_url, boxter_types[1])
t_boxster_version <- read_html(boxster_version_url)
boxster_version <- t_boxster_version %>% html_nodes('#petrol_engines a') %>% 
  html_attr("href")

boxster_version

# Get the specs
boxer_version_specs_url <- paste0(root_url, boxster_version[1])
t_boxster_version_specs <- read_html(boxer_version_specs_url)
write_html(t_boxster_version_specs, 'version_spec.html')

# Get specification in text
# Unfortunately no html table available
boxster_version_specs <- t_boxster_version_specs %>% html_nodes('.tabletd_right, .tabletd')  %>% 
  html_text()

boxster_version_specs

# Define odd ad even numbers until the length of the caharacter vector
even_numbers <- seq(2, by=2, length(boxster_version_specs))
odd_numbers <- seq(1, by=2, length(boxster_version_specs))

# Even numbers are the values
myvalues <- boxster_version_specs[even_numbers]
myvalues_cleaned <- trimws(gsub('\n', ' ', myvalues))

# Odd numbers are column names
mycolumns <- boxster_version_specs[odd_numbers]
mycolumns_cleaned <- trimws(gsub('\n', ' ', mycolumns))
mycolumns_cleaned <- head(mycolumns_cleaned, -1)

# Lets put this together into a function
carspecs <- function(make) {
  
  # Given make with a capital starting letter like: Porsche
  make_url <- paste0("https://www.ultimatespecs.com/car-specs/", make,"-models")

  # Get types in a make
  t <- read_html(make_url)
  
  type_urls <- t %>% html_nodes('.col-4') %>% 
    html_attr('href')

  root_url <- "https://www.ultimatespecs.com"
  
  type_counter <- 0
  for (type in type_urls) {
    # Get the version in a make
    version_url <- paste0(root_url, type)  
    t_version <- read_html(version_url)
    
    version_urls <- t_version %>% html_nodes('.col-4') %>% 
      html_attr("href")
    #print(version_urls)
    
    version_counter <- 0
    for (subversion in version_urls) {
      # Get subeversions
      t_subversion <- read_html(paste0(root_url,subversion))
      subversion_urls <- t_subversion %>% html_nodes('td:nth-child(1) a') %>% 
        html_attr("href")
    
      subversion_urls <- to_vec(for(x in subversion_urls) if (endsWith(x,'.html')) x)
      print(subversion_urls)
      counter <- 0
      for (subversion_specs in subversion_urls) {
        
        # Get the subversion specs
        subversion_specs_url <- paste0(root_url, subversion_specs)
        
        t_subversion_specs <- read_html(subversion_specs_url)
        
        subversion_specs <- t_subversion_specs %>% html_nodes('.tabletd_right, .tabletd')  %>% 
          html_text()

        print(length(subversion_specs))
        
        even_numbers <- seq(2, by=2, length(subversion_specs))
        odd_numbers <- seq(1, by=2, length(subversion_specs))
        
        # Clean values
        myvalues <- subversion_specs[even_numbers]
        myvalues_cleaned <- trimws(gsub('\n', ' ', myvalues))
        myvalues_cleaned <- trimws(gsub(':', '', myvalues_cleaned))
        
        # Clean column names
        mycolumns <- subversion_specs[odd_numbers]
        mycolumns_cleaned <- trimws(gsub('\n', ' ', mycolumns))
        mycolumns_cleaned <- trimws(gsub(':', '', mycolumns_cleaned))
        mycolumns_cleaned <- head(mycolumns_cleaned, -1)
        
        # Try because sometimes specs data is not well filled and would result in an unequal 
        # length for column names and values. Let's just ignore these cases

        try( subvers_specs_info <- data.frame('specs'= mycolumns_cleaned, 'values'=myvalues_cleaned))
        
        car_name <- read_html(subversion_specs_url) %>% 
          html_nodes(".page_ficha_title_text span") %>% 
          html_text()
        subvers_specs_info$carname <- car_name # Assign to df
        
        production_years <- read_html(subversion_specs_url) %>% 
          html_nodes("strong") %>% 
          html_text()
        subvers_specs_info$year <- production_years # Assign to df
        
        # If first, then define a base dataframe
        if(counter==0) {
          base_df <- subvers_specs_info
        }
        # If not first, append to the previous one(s)
        else {
          base_df <- rbind(base_df, subvers_specs_info)
        }
        counter <- counter + 1
      }
      
      if(version_counter==0) {
        base_version_df <- base_df
      }
      else {
        base_version_df <- rbind(base_version_df, base_df)
      }
      version_counter <- version_counter +1
      # break 
      # for debugging
    }
    if(type_counter==0) {
      base_type_df <- base_version_df
    }
    else {
      base_type_df <- rbind(base_type_df, base_version_df)
    }
    type_counter <- type_counter +1
    # break 
    # for debugging
  }
  return(base_type_df)
}

res <- carspecs("Porsche")

View(res)

# Save specs for all Porsche types on the website
saveRDS(res, "porsche_full_specs.RDS")

# The final dataframe is not a tidy table. It is hard to make it tidy in general because the number
# of features scraped for types is varying. You can check it yourself: 

res %>% group_by(carname) %>% 
  summarise('cnt' = n()) %>% 
  arrange(desc(cnt))

# So it is smarter to first filter the dataframe to attributes we are interested in, such as power_
res_hp <- res %>% 
  filter(specs == 'Maximum power - Output - Horsepower')
View(res_hp)
# Filter distinct row (due to error )
res_hp <- distinct(res_hp)
# Define id colum
res_hp <- res_hp  %>% unite('carname_year',carname:year)
# Reshape
reshaped <- reshape(res_hp, idvar = "carname_year", timevar = "specs", direction = "wide")
saveRDS(res_hp, "porsche_subset_tidy.RDS")
