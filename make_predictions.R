generate_new_bike <-
function(brand, brakeType, experience,
                              suspension, productFamily, gender, 
                              driveTrain, material, .ml_model) {
    
    new_bike_tbl <- tibble(
        brand           = brand,
        productFamily   = productFamily,
        experience      = experience,
        brakeType       = brakeType,
        suspension      = suspension,
        gender          = gender,
        driveTrain      = driveTrain,
        material        = material
    ) 
    
    predict(.ml_model, new_data = new_bike_tbl) %>%
        bind_cols(new_bike_tbl) %>%
        rename(price = .pred)
    
}
format_table <-
function(new_bike_tbl) {
    
    new_bike_tbl %>%
        select(experience, brand, productFamily, driveTrain,
               material, gender, suspension, brakeType, price) %>%
        mutate(price = scales::dollar(price, accuracy = 1)) %>%
        rename(Experience = experience, Brand = brand, 
               Product = productFamily, `Drive Train` = driveTrain,
               Material = material, Gender = gender, Suspension = suspension,
               `Brake Type` = brakeType, Price = price) %>%
        gather(key = "New Model Attribute", value = "Value", factor_key = T) 
}
bind_bike_predictions <-
function(bikes_tbl, new_bike_tbl) {
    
    bikes_tbl %>%
        mutate(estimate = "Actual") %>%
        bind_rows(
            new_bike_tbl %>% mutate(estimate = "Prediction")
        ) %>%
        select(estimate, brand, productFamily, experience, brakeType, suspension,
               gender, driveTrain, material, price) 
    
}



theme_specialized <- function (base_size = 11, base_family = "DINNextW01-Regular") 
{
    dark_gray <- "#414141"
    green <- "#18BC9C"
    white <- "#FFFFFF"
    grey <- "grey80"
    red <- "#ed1b2e"
    
    theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(line = element_line(colour = dark_gray, size = 0.5, linetype = 1, lineend = "butt"), 
          rect = element_rect(fill = white, colour = dark_gray, size = 0.5, linetype = 1), 
          text = element_text(family = base_family, 
                              face = "plain", 
                              colour = dark_gray, 
                              size = base_size, 
                              lineheight = 0.9, 
                              hjust = 0.5, 
                              vjust = 0.5, 
                              angle = 0, 
                              margin = margin(), 
                              debug = FALSE), 
          axis.line = element_blank(), 
          axis.text = element_text(size = 11), 
          axis.ticks = element_line(color = grey, size = rel(1/3)), 
          axis.title = element_text(size = 10), 
          panel.background = element_rect(fill = white, color = NA), 
          panel.border = element_rect(fill = NA, size = rel(1/2), color = dark_gray), 
          panel.grid.major = element_line(color = grey, size = rel(1/3)), 
          panel.grid.minor = element_line(color = grey, size = rel(1/3)), 
          panel.grid.minor.x = element_blank(), 
          panel.spacing = unit(0.75, "cm"), 
          legend.key = element_rect(fill = white, color = NA), 
          legend.position = "bottom", 
          legend.title = element_blank(), 
          strip.background = element_rect(fill = dark_gray, color = dark_gray), 
          strip.text = element_text(color = white, size = 11), 
          strip.text.x = element_text(margin = margin(3, 3, 3, 3)),
          plot.title = element_text(size = 12, 
                                    hjust = 0, 
                                    margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")), 
          plot.subtitle = element_text(size = rel(0.9), 
                                       hjust = 0, 
                                       margin = margin(t = 3, r = 3, b = 3, l = 3, unit = "pt")), 
          complete = TRUE)
}


plot_bike_predictions <-
function(data, interactive = TRUE) {
    
    g <- data %>% 
        drop_na(material) %>%
        
        rename(Estimate = estimate) %>%
        
        mutate(brand = coalesce(brand, "Unknown"),
               brand = fct_reorder(brand, price),
               label_text = str_glue("<b>Experience:</b> {experience}
                                     <b>Brand:</b> {brand}
                                     <b>Product:</b> {productFamily}
                                     <b>Drive Train:</b> {driveTrain}
                                     <b>Material:</b> {material}
                                     <b>Gender:</b> {gender}
                                     <b>Suspension:</b> {suspension}
                                     <b>Brake:</b> {brakeType}
                                     <b>Price:</b> {scales::dollar(price, accuracy=1)}")) %>%
        
        ggplot(aes(brand, price, color=Estimate)) +
        geom_violin() + 
        geom_jitter(aes(text = label_text), width = 0.1, alpha = 0.5) + 
        facet_wrap(~ material) +
        
        coord_flip() +
        
        theme_specialized() + 
        scale_colour_manual(values = c("#414141", "#ed1b2e")) +
        scale_y_log10(labels = scales::dollar_format(accuracy=1)) + 
        labs(x="",y="",title="")
        
    if (interactive) {
        return(ggplotly(g, tooltip="text"))
        
    } else {
        return(g)
    }
    
}




