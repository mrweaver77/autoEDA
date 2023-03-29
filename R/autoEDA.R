
#' Removes outliers based on quartiles or IQR
#' @export
#' @param x numeric variable
#' @param high clean upper bound
#' @param low clean lower bound
#' @param lower_q lower quartile
#' @param upper_q upper quartile
#' @param method method of cleaning either 'quartile' or 'iqr'
clean_ints <- function(x,
                       high = TRUE,
                       low = TRUE,
                       lower_q = .01,
                       upper_q = .99,
                       method = "quartile") {


  if (method == "quartile") {
    upper <- quantile(x, .99, na.rm = T)
    lower <- quantile(x, .01, na.rm = T)

  } else if (method == "iqr") {
    upper_b <- quantile(x, .75, na.rm = T)
    lower_b <- quantile(x, .25, na.rm = T)
    iqr <- upper_b - lower_b
    lower <- lower_b - iqr
    upper <- upper_b + iqr

  } else {
    stop("Not a valid method. Choose 'quartile' or 'iqr'.")
  }

  if (high == FALSE) {
    y <- ifelse(x < lower, NA, x)
  } else if (low == FALSE) {
    y <- ifelse(x > upper, NA, x)
  } else {
    y <- ifelse(x < lower | x > upper, NA, x)
  }

  return(y)
}


#' Visualize numeric features
#' @export
#' @param data dataframe
#' @param var feature name in string format
#' @param outcome outcome name in string format
#' @param outcome_type either 'regression' or 'classification'
#' @param positive_class if doing binary classification then specify the positive class
#' @param remove_outliers remove outliers via clean_ints
#' @param smooth_adjust # degree of smoothing of density plot
viz_int <- function(data,
                    var,
                    outcome,
                    outcome_type,
                    positive_class = NA,
                    remove_outliers = TRUE,,
                    smooth_adjust = 1) {

  options(ggplot2.discrete.color = c("grey64", "dodgerblue4"))

  data[,var] <- as.numeric(as.character(data[,var]))

  if (remove_outliers == TRUE) {
    data[,var] <- clean_ints(data[,var])
  }

  if (outcome_type == "classification") {
    data[,outcome] <- as.factor(as.character(data[,outcome]))

    if (!is.na(positive_class)) {

      data[,"outcome_int"] <- ifelse(data[,outcome] == positive_class, 1, 0)

      c <- cor.test(data[,var],data[,"outcome_int"])

      d_data <- data[,c(outcome,var,"outcome_int")] %>% drop_na()

      neg_max <- max(density(d_data[,var][d_data[,"outcome_int"] == 0])$y)*1.05
      pos_max <- max(density(d_data[,var][d_data[,outcome] == positive_class])$y)*1.05

      label_pos_cor <- max(neg_max,pos_max)

      p1 <-data %>%
        ggplot(aes_string(x = var, fill=outcome)) +
        geom_density(alpha=.6, adjust = smooth_adjust) +
        geom_text(aes(x=mean(data[,var], na.rm = T),
                      y=label_pos_cor,
                      label = paste0("r = ",round(as.numeric(c$estimate),2),
                                     ", p = ",round(as.numeric(c$p.value),2))))

      label_pos_anova <- max(data[,var],na.rm = T)*1.1

      p2 <- data %>%
        ggplot(aes_string(x = outcome, y = var, fill = outcome)) +
        geom_boxplot() +
        stat_compare_means(method = "anova", label.y = label_pos_anova) +
        stat_compare_means(label = "p.signif", method = "t.test",
                           ref.group = ".all.")

      p <- ggarrange(p1, p2, common.legend = TRUE)

      annotate_figure(p, top = text_grob(paste0(var, " and ", outcome),
                                         face = "bold", size = 14))
    } else {
      p1 <-data %>%
        ggplot(aes_string(x = var, fill=outcome)) +
        geom_density(alpha=.6, adjust = smooth_adjust)

      label_pos <- max(data[,var],na.rm = T)*1.1

      p2 <- data %>%
        ggplot(aes_string(x = outcome, y = var, fill = outcome)) +
        geom_boxplot() +
        stat_compare_means(method = "anova", label.y = label_pos) +
        stat_compare_means(label = "p.signif", method = "t.test",
                           ref.group = ".all.")

      p <- ggarrange(p1, p2, common.legend = TRUE)

      annotate_figure(p, top = text_grob(paste0(var, " and ", outcome),
                                         face = "bold", size = 14))
    }

  } else if (outcome_type == "regression") {
    data[,outcome] <- as.numeric(as.character(data[,outcome]))
    data %>%
      ggplot(aes_string(x = var, y = outcome)) +
      geom_point(stat = "identity") +
      geom_smooth(method = "lm") +
      labs(title = paste0(var," and ",outcome)) +
      stat_cor(method = "pearson")
  } else {
    stop("Not a valid outcome type. Either choose 'classification' or 'regression'.")
  }
}


#' Visualize categorical features
#' @export
#' @param data dataframe
#' @param  outcome outcome name in string format
#' @param  outcome_type either 'classification' or 'regression'
#' @param positive_class if doing binary classification specify the positive class
#' @param var feature name in string format
#' @param na_remove specify the removal of outliers
#' @param lump group all levels underneath a certain threshold as 'other'
#' @param prop if using lump specify the proportion
#' @param string_miss replace levels like 'unknown' or 'other' with NA
viz_factor <- function(data,
                       outcome,
                       outcome_type,
                       positive_class = NA,
                       var,
                       na_remove = T,
                       lump = T,
                       prop = .01,
                       string_miss = T) {

  colors <- brewer.pal(12,"Paired")

  df <- data[,c(var,outcome)]

  if (is.numeric(df[,1])) {
    df[,1] <- as.factor(as.character(df[,1]))
  }

  if (string_miss == TRUE) {
    df[,var][tolower(df[,var]) %in% c("unknown","other")] <- NA
  }

  if (lump == TRUE) {
    df[,var] <- fct_lump_prop(df[,var], prop = prop)
    lump_prop <- tabyl(df[,var][-4]) %>%
      data.frame()
    other_prop <- lump_prop[lump_prop[,1]=="Other",]
  }

  if (na_remove == TRUE) {
    df <- df[complete.cases(df),]
  }

  if (outcome_type == "classification") {
    df[,outcome] <- as.factor(as.character(df[,outcome]))

    if (length(unique(df[,outcome])) > 2) {

      if (!is.na(positive_class)) {
        stop("More than 2 levels in outcome. Use 'positive_class' for binary classification problems.")
      } else {
        df %>%
          group_by_at(c(1,2)) %>%
          summarise(n = n(),
                    .groups = "drop") %>%
          ungroup() %>%
          group_by_at(1) %>%
          mutate(prop = prop.table(n)) %>%
          ggplot(aes_string(x=outcome,y="prop", fill = outcome)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          facet_wrap(as.formula(paste0("~",var)),
                     scales = "free",
                     ncol = 2)

      }
    } else {

      if (!is.na(positive_class)) {

        cs <- chisq.test(df[,var], df[,outcome], correct = F)

        p_data <- df %>%
          mutate(out_m = ifelse(.[[2]] == positive_class, 1, 0)) %>%
          group_by_at(1) %>%
          summarise(outcome = sum(out_m)/length(out_m))

        cs_pos <- max(p_data$outcome, na.rm = T)*1.1

        p1 <- df %>%
          mutate(out_m = ifelse(.[[2]] == positive_class, 1, 0)) %>%
          group_by_at(1) %>%
          summarise(outcome = sum(out_m)/length(out_m)) %>%
          ggplot(aes_string(var,"outcome", fill = var)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          labs(x=NULL,
               y="Prevelance") +
          geom_text(aes(x = length(unique(df[,var])),
                        y = cs_pos/2,
                        label = paste0("X2",
                                       "(",cs$parameter,", N = ",nrow(df),")",
                                       " = ", round(cs$statistic,2),
                                       ", p = ",round(cs$p.value,2))))
        dfs <- df %>%
          group_by_at(var) %>%
          summarise(n = n()/nrow(df))

        df2 <- dfs %>%
          mutate(csum = rev(cumsum(rev(n))),
                 pos = n/2 + lead(csum,1),
                 pos = if_else(is.na(pos), n/2, pos))

        df2[,"group"] <- df2[,1]

        p2 <- ggplot(df2, aes(x="",y=n,fill=fct_inorder(group))) +
          geom_col(width = 1, color = 1) +
          coord_polar(theta = "y") +
          geom_label_repel(data = df2,
                           aes(y = pos, label = paste0(round(n*100,2),"%")),
                           size = 4.5, nudge_x = 1, show.legend = FALSE) +
          guides(fill = guide_legend(title = var)) +
          theme_void()

        p <- ggarrange(p1, p2, common.legend = T)

        annotate_figure(p, top = text_grob(paste0(var, " and ", outcome),
                                           face = "bold", size = 14))

      } else {
        p1 <- prop.table(table(df)) %>%
          data.frame() %>%
          ggplot(aes_string(x=var, y="Freq", fill = outcome)) +
          geom_bar(stat = "identity", position = "dodge") +
          coord_flip()# +
        #scale_fill_manual(values = colors)

        p2 <- df %>%
          group_by_at(c(1,2)) %>%
          summarise(n = n(),
                    .groups = "drop") %>%
          ungroup() %>%
          group_by_at(1) %>%
          mutate(prop = prop.table(n)) %>%
          ggplot(aes_string(x=outcome,y="prop", fill = outcome)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          facet_wrap(as.formula(paste0("~",var)),
                     scales = "free",
                     ncol = 2)# +
        #scale_fill_manual(values = colors)

        p <- ggarrange(p1, p2, common.legend = T)

        annotate_figure(p, top = text_grob(paste0(var, " and ", outcome),
                                           face = "bold", size = 14))
      }
    }
  } else if (outcome_type == "regression") {

    label_pos <- max(df[,outcome],na.rm = T)*1.1

    p1 <- df %>%
      ggplot(aes_string(x = outcome, fill = var)) +
      geom_density(alpha = .6)

    p2 <- df %>%
      ggplot(aes_string(x=var, y=outcome, fill = var)) +
      geom_boxplot() +
      stat_compare_means(method = "anova", label.y = label_pos) +
      stat_compare_means(label = "p.signif", method = "t.test",
                         ref.group = ".all.") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))


    p <- ggarrange(p1, p2, common.legend = T)

    annotate_figure(p, top = text_grob(paste0(var, " and ", outcome),
                                       face = "bold", size = 14))

  } else {
    stop("Not a valid outcome type. Either choose 'classification' or 'regression'.")
  }
}



#' Visualize all features in dataset
#' @export
#' @param data dataframe
#' @param outcome outcome name in string format
#' @param outcome_type either 'classification' or 'regression'
#' @param positive_class if doing binary classification specify the positive class
#' @param smooth_adjust
viz_data <- function(data,
                     outcome,
                     outcome_type,
                     positive_class = NA,
                     smooth_adjust = 1) {
  pb <- txtProgressBar(min = 0, max = ncol(data), style = 3)
  df <- data %>%
    mutate_if(is.factor, as.character) %>%
    mutate_if(is.integer, as.numeric)

  plots <- list()

  for (i in 1:ncol(df)) {
    print(colnames(df)[i])
    if (colnames(df)[i] == outcome) {
      print("Skipping outcome variable...")
      next
    } else if (length(unique(df[,i][!is.na(df[,i])])) <= 1) {
      print("<= 1 unique values...")
      next
    } else {
      if (is.numeric(df[,i])) {

        if (length(unique(df[,i])) < 5) {
          p <- viz_factor(df,
                          outcome = outcome,
                          outcome_type = outcome_type,
                          positive_class = positive_class,
                          var = colnames(df)[i],
                          lump = FALSE,
                          string_miss = TRUE,
                          na_remove = TRUE)
        } else if (between(length(unique(df[,i])),5,30)) {
          p <- viz_int(df,
                       var = colnames(df)[i],
                       outcome = outcome,
                       outcome_type = outcome_type,
                       remove_outliers = FALSE,
                       smooth_adjust = smooth_adjust)
        } else {

          if (length(unique(df[,outcome])) == 2) {
            p <- viz_int(df,
                         var = colnames(df)[i],
                         outcome = outcome,
                         outcome_type = outcome_type,
                         positive_class = positive_class,
                         remove_outliers = TRUE,
                         smooth_adjust = smooth_adjust)
          } else {
            p <- viz_int(df,
                         var = colnames(df)[i],
                         outcome = outcome,
                         outcome_type = outcome_type,
                         remove_outliers = TRUE,
                         smooth_adjust = smooth_adjust)
          }
        }
      } else if (is.character(df[,i])) {
        if (length(unique(df[,i])) > 100) {
          print("Large amount of unique values. Consider changing to numeric?")
          next
        } else if (!is.na(positive_class)) {
          p <- viz_factor(df,
                          outcome = outcome,
                          outcome_type = outcome_type,
                          positive_class = positive_class,
                          var = colnames(df)[i],
                          lump = TRUE,
                          string_miss = TRUE,
                          na_remove = TRUE)
        } else {
          p <- viz_factor(df,
                          outcome = outcome,
                          outcome_type = outcome_type,
                          var = colnames(df)[i],
                          lump = TRUE,
                          string_miss = TRUE,
                          na_remove = TRUE)
        }
      }
    }

    plots[[i]] <- p
    setTxtProgressBar(pb, i)
  }
  final_plots <- plots[lengths(plots) != 0]
  return(final_plots)
}


#' Export visualizations to powerpoint or pdf
#' @export
create_eda_report <- function(plots, path, format = "ppt") {

  pb <- txtProgressBar(min = 0, max = length(plots), style = 3)

  if (format == "pdf") {
    destination <- path
    pdf(file=destination)

    for (i in 1:length(plots)) {
      plot(plots[[i]])
      setTxtProgressBar(pb, i)
    }
    dev.off()
  } else {
    file.opened <- function(path) {
      suppressWarnings(
        "try-error" %in% class(
          try(file(path,
                   open = "w"),
              silent = TRUE
          )
        )
      )
    }

    if (file.opened(path) == TRUE) {
      stop("Close existing file first...")
    }

    doc <- read_pptx()

    for (i in 1:length(plots)) {
      doc <- add_slide(doc)
      doc <- ph_with(doc, value = plots[[i]], ph_location_fullsize())
      setTxtProgressBar(pb, i)
    }

    print(doc, target = path)
  }
}

