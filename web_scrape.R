# WEBSCRAPING ----

# 1.0 LIBRARIES ----

library(tidyverse) # Main Package - Loads dplyr, purrr
library(rvest)     # HTML Hacking & Web Scraping
library(furrr)     # Parallel Processing using purrr (iteration)
library(fs)        # Working with File System
library(xopen)     # Quickly opening URLs
library(jsonlite)


# Examine Product Family Page ----
url <- "https://www.specialized.com/us/en/shop/Bikes/c/bikes"
#xopen(url)

# Get features from front page found in nested JSON 
get_features <- function(url) {
    
    # Read HTML from URL 
    html <- read_html(url)
    
    # Convert JSON container to R object
    json <- html %>%
        html_nodes(".product-list__item-wrapper") %>%
        html_attr("data-product-ic") 
    
    # Convert to tibble
    bike_features <- stream_in(textConnection(gsub("\\n", "", json))) %>% as_tibble()
    
    # Get price
    bike_price <- html %>%
        html_nodes(".js-plp-price") %>%
        html_text() %>%
        parse_number() %>%  # Use `readr` to remove keep only integer 
        enframe(name = NULL, value = "price") 
    
    # Get urls
    bike_url <- html %>%
        html_nodes("a.product-list__image") %>%
        html_attr("href") %>%
        enframe(name = NULL, value = "url") %>%
        mutate(url = str_glue("https://www.specialized.com{url}"))
    
    # Bind price & urls to features
    bike_features_tbl <- bike_features %>% 
        select(-p_item_id, -price, -position, -p_size, 
               -p_subCategory5:-p_subCategory15, -p_collection) %>%
        bind_cols(bike_price, bike_url) 
    
    return(bike_features_tbl)
}

# Get features from front page 
bike_features_tbl <- get_features("https://www.specialized.com/us/en/shop/bikes/c/bikes?q=%3Aprice-desc%3Aarchived%3Afalse&show=All")

# bike_features_tbl %>%
#     write_csv("specialized_bikes_2019.csv")

#-------------------------------------------------------------------------------

url <- "https://www.specialized.com/us/en/s-works-roubaix--sram-red-etap-axs/p/171042?color=263085-171042"

# Get features from each bike page
get_specs <- function(url) {
    
    html <- read_html(url)
    
    # Bike Specs Key
    bike_spec_type <- html %>%
        html_nodes(".product__specs-table-key") %>%
        html_text() %>%
        str_to_title() %>%
        str_replace_all("\n","") %>%
        enframe(name = NULL, value = "key") 
    
    # Bike Specs Value
    bike_spec_value <- html %>%
        html_nodes(".product__specs-table-value") %>%
        html_text() %>%
        str_replace_all("\n","") %>%
        enframe(name = NULL, value = "value") 
    
    # Bike Name to Join On
    bike_name <- html %>%
        html_nodes(".product__name") %>%
        html_text() %>%
        enframe(name = NULL, value = "name")
    
    # Bike Geometries Table
    bike_geometries <- html %>%
        html_nodes(".table") %>%
        html_table() %>% 
        as.data.frame() %>%
        rename(Geometry = Var.1) 
    
    # Remove `X` from column names
    colnames(bike_geometries) <- colnames(bike_geometries) %>%
        str_replace_all("X", "")
    
    # Bike Specs Table
    ret <- bike_spec_type %>%
        bind_cols(bike_spec_value) 
    
    return(ret)
}



plan("multiprocess")
bike_specs_tbl <- bike_features_tbl %>%
    mutate(bike_features = future_map(url, get_specs)) %>% unnest()


bike_specs_tbl <- bike_specs_tbl %>% 
    filter(brand != "Kids") %>% 
    spread(key = key, value = value) %>%
    select(-c(" Bottom Bracket":" Wiring Harness"))


colnames(bike_specs_tbl) <- colnames(bike_specs_tbl) %>%
    str_replace_all("p_price", "price_range") %>%
    str_replace_all("p_", "")

bike_specs_tbl


create_bike_specs <- function(data) {
    
    plan("multiprocess")
    
    bike_specs_tbl <- bike_features_tbl %>%
        mutate(bike_features = future_map(url, get_specs)) %>% 
        unnest() %>% 
        filter(brand != "Kids") %>% 
        spread(key = key, value = value) %>%
        select(-c(" Bottom Bracket":" Wiring Harness"))
    
    colnames(bike_specs_tbl) <- colnames(bike_specs_tbl) %>%
        str_replace_all("p_price", "price_range") %>%
        str_replace_all("p_", "")
    
    return(bike_specs_tbl)
    
}
    

