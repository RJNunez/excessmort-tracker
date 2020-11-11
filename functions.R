##
get_excess_deaths <- function(dat, start, end)
{
  # -- Dates to exclude when computing expected mortality
  flu_season    <- seq(make_date(2017, 12, 16), make_date(2018, 1, 16), by = "day")
  exclude_dates <- c(flu_season, seq(make_date(2020, 1, 1), today(), by = "day"))
  
  # -- Number of observations
  knots <- dat %>%
    filter(!jurisdiction %in% c("Connecticut", "North Carolina", "Italy")) %>%
    filter(date >= start, date <= end) %>%
    # filter(date >= input$range_edeaths[1], date <= input$range_edeaths[2]) %>%
    group_by(jurisdiction) %>%
    summarize(n = n()) %>%
    arrange(n) %>%
    slice(1) %>%
    pull(n)
  
  # -- Getting rid of bad data
  dat <- filter(dat, !jurisdiction %in% c("Connecticut", "North Carolina", "Italy"))
  
  # -- Jurisdictions 
  jurs <- unique(dat$jurisdiction)
  
  # -- Choosing the number of knots
  if(knots >= 25) {
    nknots <- 12
  } else {
    nknots <- ceiling(knots/2)
  }
  
  # -- Computing excess deaths
  eds <- map_df(jurs, function(x){
    
    fit <- dat %>%
      filter(jurisdiction == x) %>%
      arrange(date) %>%
      excess_model(.,
                   start          = start,
                   end            = end,
                   exclude        = exclude_dates,
                   knots.per.year = nknots, 
                   aic            = FALSE, 
                   order.max      = 7,
                   weekday.effect = FALSE,
                   verbose        = FALSE)
    
    excess_cumulative(fit, start = start, end = end) %>%
      mutate(jurisdiction = x) %>%
      mutate(lwr = fitted - 1.96 * se, 
             upr = fitted + 1.96 * se)
  }) %>%
    as_tibble() %>%
    left_join(select(dat, date, jurisdiction, covid19, population), by = c("date", "jurisdiction"))

  # -- To be returned
  return(eds)
}

##
percent_change_plot <- function(dat, jurisdictions, start, end, ci_ind)
{
  # -- For the gray lines in the background
  background_dat <- filter(dat, date >= start, date <= end)
  
  # -- Jurisdiction specific data
  jurisdiction_dat <- dat %>%
    filter(jurisdiction %in% jurisdictions) %>%
    filter(date >= start, date <= end) %>%
    mutate(jurisdiction = factor(jurisdiction, levels = jurisdictions))
  
  # -- Used for labeling
  last_dp <- jurisdiction_dat %>%
    group_by(jurisdiction) %>%
    filter(date >= max(date)-8) %>%
    ungroup() %>%
    mutate(label = paste0(" ", jurisdiction))
  
  # -- Used to determine y-axis
  y_limits <- range(jurisdiction_dat$fitted)
  edays    <- weeks(2)
  
  # -- Making Viz
  p <- jurisdiction_dat %>%
    ggplot(aes(date, fitted, color=jurisdiction)) +
    geom_hline(yintercept = 0, color="#525252", lty=2) +
    geom_line(aes(date, fitted, group=jurisdiction), color="#969696", size=0.50, alpha=0.20, data = background_dat) +
    geom_line(size=1, show.legend = FALSE, data = jurisdiction_dat) +
    geom_dl(aes(label=label), method=list("last.points", fontfamily="Helvetica", fontface="bold", cex=1), data = last_dp) +
    ylab("Percent change from average mortality") +
    xlab("Date") +
    coord_cartesian(ylim = c(y_limits[1], y_limits[2])) +
    scale_y_continuous(label = scales::percent) + 
    scale_x_date(date_labels = "%b %Y",
                 limits = c(start, end + edays)) +
    scale_color_manual(name = "",
                       values = my_palette) +
    theme_sandstone()

  if(ci_ind == "Yes")
  {
    p <- p + 
      geom_ribbon(aes(ymin = lwr, ymax = upr, fill=jurisdiction), color=NA, alpha=0.50, show.legend = FALSE, data= jurisdiction_dat) +
      scale_fill_manual(name = "",
                        values = my_palette)
  }
  return(p)
}

##
excess_deaths_plot <- function(dat, jurisdictions, start, end, ci_ind, pop_ind)
{
  # -- For the gray lines in the background
  background_dat <- filter(dat, date >= start, date <= end) %>%
    mutate(fitted100 = 100000 * fitted / population,
           se100     = 100000 / population * se,
           lwr100    = fitted100 - se100 * 1.96,
           upr100    = fitted100 + se100 * 1.96)
    
  # -- Covid19 mortality data
  covid_dat <- dat %>%
    filter(jurisdiction %in% jurisdictions, 
           date >= start, date <= end) %>%
    mutate(jurisdiction = factor(jurisdiction, levels = jurisdictions))
  
  # -- State specific data
  jurisdiction_dat <- dat %>%
    filter(jurisdiction %in% jurisdictions, 
           date >= start, date <= end) %>%
    mutate(jurisdiction = factor(jurisdiction, levels = jurisdictions)) %>%
    mutate(fitted100 = 100000 * fitted / population,
           se100     = 100000 / population * se,
           lwr100    = fitted100 - se100 * 1.96,
           upr100    = fitted100 + se100 * 1.96)
  
  # -- Used for labeling
  last_dp <- jurisdiction_dat %>%
    group_by(jurisdiction) %>%
    filter(date >= max(date)-8) %>%
    ungroup() %>%
    mutate(label = paste0(" ", jurisdiction))
  
  if(pop_ind == "No"){
    
    # -- Used to determine y-axis
    y_limits <- range(jurisdiction_dat$fitted)
    edays    <- weeks(2)
    
    # -- Making Viz
    p <- jurisdiction_dat %>%
      ggplot(aes(date, fitted, label=jurisdiction, color=jurisdiction)) +
      geom_line(aes(date, fitted, group=jurisdiction), color="#969696", size=0.50, alpha=0.20, data = background_dat) +
      # geom_line(aes(date, covid19, color=jurisdiction), lty=2, size=0.50, show.legend = FALSE, data = covid_dat) +
      geom_line(size=1, show.legend = FALSE, data = jurisdiction_dat) +
      geom_dl(method=list("last.points", fontfamily="Helvetica", fontface="bold", cex=1), data = last_dp) +
      ylab("Cumulative excess deaths") +
      xlab("Date") +
      coord_cartesian(ylim = c(y_limits[1], y_limits[2])) +
      scale_y_continuous(label = scales::comma) + 
      scale_x_date(date_labels = "%b %Y",
                   limits = c(start, end + edays)) +
      scale_color_manual(name = "",
                         values = my_palette) +
      scale_fill_manual(name = "",
                        values = my_palette) +
      theme_sandstone()
    
    if(ci_ind == "Yes")
    {
      p <- p + 
        geom_ribbon(aes(ymin = lwr, ymax = upr, fill=jurisdiction), color=NA, alpha=0.50, show.legend = FALSE, data= jurisdiction_dat) +
        scale_fill_manual(name = "",
                          values = my_palette)
    }
  } else {
    # -- Used to determine y-axis
    y_limits <- range(jurisdiction_dat$fitted100)
    edays    <- weeks(2)
  
    # -- Making Viz
    p <- jurisdiction_dat %>%
      ggplot(aes(date, fitted100, color=jurisdiction)) +
      geom_line(aes(date, fitted100, group=jurisdiction), color="#969696", size=0.50, alpha=0.20, data = background_dat) +
      # geom_line(aes(date, covid19, color=jurisdiction), lty=2, size=0.50, show.legend = FALSE, data = covid_dat) +
      geom_line(size=1, show.legend = FALSE, data = jurisdiction_dat) +
      geom_dl(aes(label=label), method=list("last.points", fontfamily="Helvetica", fontface="bold", cex=1), data = last_dp) +
      ylab("Cumulative excess deaths per 100,000") +
      xlab("Date") +
      coord_cartesian(ylim = c(y_limits[1], y_limits[2])) +
      scale_y_continuous(label = scales::comma) + 
      scale_x_date(date_labels = "%b %Y",
                   limits = c(start, end + edays)) +
      scale_color_manual(name = "",
                         values = my_palette) +
      scale_fill_manual(name = "",
                        values = my_palette) +
      theme_sandstone()
    
    if(ci_ind == "Yes")
    {
      p <- p + 
        geom_ribbon(aes(ymin = lwr100, ymax = upr100, fill=jurisdiction), color=NA, alpha=0.50, show.legend = FALSE, data= jurisdiction_dat) +
        scale_fill_manual(name = "",
                          values = my_palette)
    }
  }
  return(p)
}