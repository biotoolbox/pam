create_regression_data_table_eilers_peeters <- function(files, use_etr_I) {
    library(data.table)
    result <- data.table()

    for (file in files) {
        title <- basename(file)
        data <- read_pam_data(file)
        reg_data <- generate_regression_eilers_peeters(data, use_etr_I)
        reg_data[["etr_regression_data"]] <- NULL
        reg_data[["file"]] <- title
        result <- rbind(result, reg_data)
        print(reg_data)
    }

    return(result)
}

plot_combo <- function(title, data, use_etr_I) {
    library(ggplot2)
    reg_data_eilers_peeters <- generate_regression_eilers_peeters(data, use_etr_I)
    reg_data_platt <- generate_regression_platt(data, use_etr_I)
    reg_data_walsby <- generate_regression_walsby(data, use_etr_I)

    etr_to_use <- ""
    if (use_etr_I) {
        etr_to_use <- etr_I_col_name
        data <- data[Action != "Fm-Det."]
    } else {
        etr_to_use <- etr_II_col_name
        data <- data[Action != "Pm.-Det."]
    }

    etr_regression_data_eilers_peeters <- reg_data_eilers_peeters[["etr_regression_data"]]
    etr_regression_data_platt <- reg_data_platt[["etr_regression_data"]]
    etr_regression_data_walsby <- reg_data_walsby[["etr_regression_data"]]

    plot <- ggplot(data, aes(x = PAR, y = get(etr_to_use))) +
        geom_point() +
        geom_line(data = etr_regression_data_eilers_peeters, aes(x = PAR, y = prediction), alpha = 0.4, color = "#f700ff") +
        geom_line(data = etr_regression_data_walsby, aes(x = PAR, y = prediction), alpha = 0.4, color = "#6f00ff") +
        geom_line(data = etr_regression_data_platt, aes(x = PAR, y = prediction), alpha = 0.4, color = "#00ff15") +
        labs(x = par_label, y = etr_label, title = eval(title)) +
        theme_minimal()

    return(plot)
}

# Calculate best model for data bsp: 70% eilers peeters, 20% platt, 10% walsby
# wrapper pdf
# Tabelle unter plots etrmax, ik, alpha etc
# paramterdata validation
# vollweider model in R erstellen
# alle Berechnungen aus original Quellen prüfen
# startwerte a,b,c als parameter übergeben
# legende für kombidiagramm das mit den vielen linien
