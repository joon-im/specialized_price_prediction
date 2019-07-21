preprocess_data <-
function(data) {
    
    # Remove columns
    bikes_tbl <- data %>%
        select(-url, -p_scene7_id, -p_subCategory1) 
    
    # Rename columns
    colnames(bikes_tbl) <- colnames(bikes_tbl) %>%
        str_replace_all("p_price", "price_range") %>%
        str_replace_all("p_", "")
    
    # Convert character to factor columns
    # bikes_factor_tbl <- as.data.frame(unclass(bikes_tbl)) 
    
    # Drop unnecessary columns
    train_tbl <- bikes_tbl %>%
        select(-c(name, id, color, primaryCategory, subCategory3, subCategory4,
                  price_range, group, subCategory2, wheelSize))
    
    return(train_tbl)
}
