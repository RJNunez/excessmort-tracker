##
get_excess_deaths <- function(dat, jurisdictions, start, end)
{
  # -- Dates to exclude when computing expected mortality
  flu_season    <- seq(make_date(2017, 12, 16), make_date(2018, 1, 16), by = "day")
  exclude_dates <- c(flu_season, seq(make_date(2020, 1, 1), today(), by = "day"))

  # -- Number of observations
  num_obs <- dat %>%
    filter(jurisdiction %in% jurisdictions) %>%
    filter(date >= start, date <= end) %>%
    group_by(jurisdiction) %>%
    summarize(n = n()) %>%
    arrange(n) %>%
    slice(1) %>%
    pull(n)
  
  # -- Getting rid of bad data
  dat <- filter(dat, jurisdiction %in% jurisdictions)
  
  # -- Jurisdictions 
  jurs <- unique(dat$jurisdiction)
  
  # -- Choosing the number of knots
  if(num_obs >= 13) {
    nknots <- 12
  } else {
    nknots <- ceiling(num_obs/2)
  }
  
  # -- Computing excess deaths
  eds <- map_df(jurs, function(x){
    
    if(x == "Puerto Rico") {
      fit <- dat %>%
        filter(jurisdiction == x) %>%
        arrange(date) %>%
        excess_model(.,
                     start          = start,
                     end            = end,
                     exclude        = unique(sort(c(exclude_dates, seq(make_date(2017, 9, 20), make_date(2018, 3, 31), by = "day")))),
                     knots.per.year = nknots, 
                     aic            = FALSE, 
                     order.max      = 7,
                     weekday.effect = FALSE,
                     verbose        = FALSE)
    } else {
      
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
    }
    
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
plot_percent_change <- function(dat, jurisdictions, start, end, ci_ind)
{
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
  y_limits <- range(jurisdiction_dat$lwr, jurisdiction_dat$upr)
  edays    <- weeks(2)
  
  # -- Making Viz
  p <- jurisdiction_dat %>%
    ggplot(aes(date, fitted, color=jurisdiction)) +
    geom_hline(yintercept = 0, color="#525252", lty=2) +
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

  if(ci_ind)
  {
    p <- p + 
      geom_ribbon(aes(ymin = lwr, ymax = upr, fill=jurisdiction), color=NA, alpha=0.50, show.legend = FALSE, data= jurisdiction_dat) +
      scale_fill_manual(name = "",
                        values = my_palette)
  }
  return(p)
}

# dat <- filter(percent_change, type == "weighted")
# dat <- percent_change_countries
# start <- make_date(2020,03,01)
# end <- max(cdc_counts$date)
# end <- make_date(2020, 07, 01)

# start <- make_date(2020,03,01)
# end <- make_date(2020,08,30)
# jurisdictions <- c("Puerto Rico", "Florida")
# ed <- get_excess_deaths(dat = cdc_counts, jurisdictions, start, end)
# pc <- filter(percent_change, type == "weighted")

##
make_table <- function(pc, ed, jurisdictions, start, end)
{
  #-------------------------------------------------------------#
  ### pc: percent change data table
  ### ed: excess deaths data table (data from get_excess_deaths)
  #-------------------------------------------------------------#
  
  jurisdiction_pc <- filter(pc, jurisdiction %in% jurisdictions)
  jurisdiction_ed <- filter(ed, jurisdiction %in% jurisdictions)
  
  tab1 <- jurisdiction_pc %>%
    filter(date >= start, date <= end) %>%
    mutate(pc = paste0(format(round(100*fitted, 1), nsmall = 1), "%", "(",
                       format(round(100*lwr, 1), nsmall = 1), "%", " to ",
                       format(round(100*upr, 1), nsmall = 1), "%", ")")) %>%
    select(date, jurisdiction, pc)
  
  tab2 <- jurisdiction_ed %>%
    filter(date >= start, date <= end) %>%
    mutate(fitted100 = 100000 * fitted / population,
           se100     = 100000 / population * se,
           lwr100    = fitted100 - se100 * 1.96,
           upr100    = fitted100 + se100 * 1.96,
           fitted    = prettyNum(round(fitted), big.mark = ","),
           lwr       = prettyNum(round(lwr), big.mark = ","),
           upr       = prettyNum(round(upr), big.mark = ","),
           ed        = paste0(format(fitted), "(",
                              format(lwr), " to ",
                              format(upr), ")"),
           ed100     = paste0(format(round(fitted100, 1), nsmall = 1), "(",
                              format(round(lwr100, 1), nsmall = 1), " to ",
                              format(round(upr100, 1), nsmall = 1), ")")) %>%
    select(date, jurisdiction, ed, ed100)
    

  tab <- left_join(tab1, tab2, by = c("date", "jurisdiction")) %>%
    setNames(c("Date", "Jurisdiction", "PC (CI)", "CEM (CI)", "CEM per 100,000 (CI)")) %>%
    mutate(Date = format(Date, "%B %d, %Y"))
  
  DT::datatable(tab, 
                rownames = FALSE,
                options = list(dom = 't',
                               pageLength = -1))
}

##
plot_worse_percent_change <- function(dat, start, end)
{
  # -- Wrangling data
  jurisdiction_dat <- dat %>%
    group_by(jurisdiction) %>%
    mutate(label = c(NA, diff(fitted))) %>%
    ungroup()
  
  # -- Finding closest date to end date
  end <- unique(dat$date)[which(abs(unique(dat$date) - end) == min(abs(unique(dat$date) - end)))]
  
  # -- Worse jurisdictions
  top_worse <- jurisdiction_dat %>%
    filter(date == end) %>%
    group_by(jurisdiction) %>%
    mutate(last_fitted = last(fitted)) %>%
    ungroup() %>%
    filter(date == max(date)) %>%
    arrange(desc(last_fitted)) %>%
    slice(1:6) %>%
    pull(jurisdiction)
  
  # -- Viz
  jurisdiction_dat %>%
    filter(jurisdiction %in% top_worse, date >= start, date <= end) %>%
    mutate(jurisdiction = factor(jurisdiction, levels = top_worse)) %>%
    group_by(jurisdiction) %>%
    mutate(last_fitted = last(fitted),
           change      = ifelse(last(label) > 0, "Upward", "Downward")) %>%
    ungroup() %>%
    ggplot(aes(date, fitted, color=change, fill=change)) +
    geom_hline(yintercept = 0, color="#525252", lty=2) +
    geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.20, color=NA, show.legend = FALSE) +
    geom_line(show.legend = FALSE) +
    geom_dl(aes(color=change, label=paste0("",round(100*last_fitted,1),"%")), method=list("last.points", fontface="bold", cex=0.90)) +
    scale_color_manual(values = c("Upward" = "#cb181d", "Downward" = "#2171b5")) +
    scale_fill_manual(values = c("Upward" = "#cb181d", "Downward" = "#2171b5")) +
    scale_x_date(date_labels = "%b %Y", limits = c(start, end+weeks(2))) +
    scale_y_continuous(labels = scales::percent) +
    ylab("Percent change from average mortality") +
    xlab("Date") +
    facet_wrap(~jurisdiction, nrow=2, ncol=3) +
    theme_sandstone()
}
# plot_worse_percent_change(dat, start, end)

##
plot_excess_deaths <- function(dat, jurisdictions, start, end, ci_ind, pop_ind)
{
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
  
  if(pop_ind == "Totals"){
    
    # -- Used to determine y-axis
    y_limits <- range(jurisdiction_dat$lwr, jurisdiction_dat$upr)
    edays    <- weeks(2)
    
    # -- Making Viz
    p <- jurisdiction_dat %>%
      ggplot(aes(date, fitted, label=jurisdiction, color=jurisdiction)) +
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
    
    if(ci_ind)
    {
      p <- p + 
        geom_ribbon(aes(ymin = lwr, ymax = upr, fill=jurisdiction), color=NA, alpha=0.50, show.legend = FALSE, data= jurisdiction_dat) +
        scale_fill_manual(name = "",
                          values = my_palette)
    }
  } else {
    # -- Used to determine y-axis
    y_limits <- range(jurisdiction_dat$lwr100, jurisdiction_dat$upr100)
    edays    <- weeks(2)
  
    # -- Making Viz
    p <- jurisdiction_dat %>%
      ggplot(aes(date, fitted100, color=jurisdiction)) +
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
    
    if(ci_ind)
    {
      p <- p + 
        geom_ribbon(aes(ymin = lwr100, ymax = upr100, fill=jurisdiction), color=NA, alpha=0.50, show.legend = FALSE, data= jurisdiction_dat) +
        scale_fill_manual(name = "",
                          values = my_palette)
    }
  }
  return(p)
}