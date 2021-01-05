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
    left_join(select(dat, date, jurisdiction, covid19, population), by = c("date", "jurisdiction")) %>%
    group_by(jurisdiction) %>%
    mutate(covid19 = ifelse(is.na(covid19), 0, covid19)) %>%
    mutate(covid19 = c(0, diff(covid19))) %>%
    mutate(covid19 = ifelse(covid19 < 0, 0, covid19)) %>%
    mutate(covid19 = cumsum(covid19)) %>%
    ungroup()
  
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
    # ggtitle("Percent Change from Average Mortality") +
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
    select(date, jurisdiction, pc, fitted) %>%
    rename(pcORDER = fitted)
  
  tab2 <- jurisdiction_ed %>%
    filter(date >= start, date <= end) %>%
    mutate(fitted100 = 100000 * fitted / population,
           se100     = 100000 / population * se,
           lwr100    = fitted100 - se100 * 1.96,
           upr100    = fitted100 + se100 * 1.96,
           covid100  = 100000 * covid19 / population,
           d         = fitted - covid19,
           d100      = fitted100 - covid100,
           lwrd      = d - 1.96 * se, 
           uprd      = d + 1.96 * se, 
           lwrd100   = d100 - 1.96 * se100, 
           uprd100   = d100 + 1.96 * se100, 
           fitted    = prettyNum(round(fitted), big.mark = ","),
           lwr       = prettyNum(round(lwr), big.mark = ","),
           upr       = prettyNum(round(upr), big.mark = ","),
           c19       = prettyNum(round(covid19), big.mark = ","),
           c19100    = format(round(covid100, 1), nsmall = 1),
           dif       = prettyNum(round(d), big.mark = ","),
           lwrd      = prettyNum(round(lwrd), big.mark = ","),
           uprd      = prettyNum(round(uprd), big.mark = ","),
           ed        = paste0(format(fitted), "(",
                              format(lwr), " to ",
                              format(upr), ")"),
           ed100     = paste0(format(round(fitted100, 1), nsmall = 1), "(",
                              format(round(lwr100, 1), nsmall = 1), " to ",
                              format(round(upr100, 1), nsmall = 1), ")"),
           dif       = paste0(format(dif), "(",
                              format(lwrd), " to ",
                              format(uprd), ")"),
           dif100    = paste0(format(round(d100, 1), nsmall = 1), "(",
                              format(round(lwrd100, 1), nsmall = 1), " to ",
                              format(round(uprd100, 1), nsmall = 1), ")")) %>%
    select(date, jurisdiction, c19, c19100, ed, ed100, dif, dif100, fitted, fitted100, covid19, covid100, d, d100) %>%
    rename(edORDER     = fitted, 
           ed100ORDER  = fitted100,
           c19ORDER    = covid19,
           c19100ORDER = covid100,
           dORDER      = d,
           d100ORDER   = d100)
  
  tab <- left_join(tab1, tab2, by = c("date", "jurisdiction")) %>%
    setNames(c("dateORDER", "Jurisdiction", "PC (CI)", "pcORDER", "C19", "C19 per 100,000", 
               "CEM (CI)", "CEM per 100,000 (CI)", "CEM - C19 (CI)", "CEM - C19 per 100,000 (CI)",
               "edORDER", "ed100ORDER", "c19ORDER", "c19100ORDER", "dORDER", "d100ORDER")) %>%
    mutate(Date = format(dateORDER, "%B %d, %Y")) %>%
    select(Date, `Jurisdiction`, `PC (CI)`, 
           C19, `C19 per 100,000`, `CEM (CI)`, 
           `CEM per 100,000 (CI)`, `CEM - C19 (CI)`, `CEM - C19 per 100,000 (CI)`,
           dateORDER, pcORDER, edORDER, ed100ORDER, c19ORDER, c19100ORDER, dORDER, d100ORDER) %>%
    arrange(Jurisdiction, desc(dateORDER))
  
  tab <- DT::datatable(tab, rownames = FALSE,
                # caption = paste("•PC  = Percent change \n•CEM = Cumulative excess mortality \n•C19 = Covid-19 reported death toll \n•CI  = 95% confidence interval"),
                options = list(dom = 't', pageLength = -1,
                columnDefs = list(
                  list(targets = 0, orderData = 9),
                  list(targets = 2, orderData = 10),
                  list(targets = 3, orderData = 13),
                  list(targets = 4, orderData = 14),
                  list(targets = 5, orderData = 11),
                  list(targets = 6, orderData = 12),
                  list(targets = 7, orderData = 15),
                  list(targets = 8, orderData = 16),
                  list(targets = 9, visible = FALSE),
                  list(targets = 10, visible = FALSE),
                  list(targets = 11, visible = FALSE),
                  list(targets = 12, visible = FALSE),
                  list(targets = 13, visible = FALSE),
                  list(targets = 14, visible = FALSE),
                  list(targets = 15, visible = FALSE),
                  list(targets = 16, visible = FALSE),
                  list(className = 'dt-right', targets = 0:8)))) %>%
    DT::formatStyle(1:2,"white-space"="nowrap")
  
  return(tab)
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

##
plot_worse_excess_deaths <- function(dat, pc, start, end)
{
  # -- Wrangling data
  jurisdiction_dat <- pc %>%
    group_by(jurisdiction) %>%
    mutate(label = c(NA, diff(fitted))) %>%
    ungroup()
  
  # -- Finding closest date to end date
  end <- unique(pc$date)[which(abs(unique(pc$date) - end) == min(abs(unique(pc$date) - end)))]
  
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
  
  # -- To be used below
  labels <- jurisdiction_dat %>%
    filter(jurisdiction %in% top_worse, date >= start, date <= end) %>%
    mutate(jurisdiction = factor(jurisdiction, levels = top_worse)) %>%
    group_by(jurisdiction) %>%
    mutate(change = ifelse(last(label) > 0, "Upward", "Downward")) %>%
    ungroup() %>%
    select(jurisdiction, change) %>%
    unique()
  
  # -- Getting excess deaths data
  ed <- get_excess_deaths(dat = dat, jurisdictions = top_worse, start = start, end = end)

  # -- Viz
  ed %>%
    mutate(fitted100  = 100000 * fitted / population,
           covid19100 = 100000 * covid19/ population,
           se100      = 100000 / population * se,
           lwr100     = fitted100 - se100 * 1.96,
           upr100     = fitted100 + se100 * 1.96) %>%
    group_by(jurisdiction) %>%
    mutate(last_fitted = last(fitted100)) %>%
    ungroup() %>%
    mutate(jurisdiction = factor(jurisdiction, levels = top_worse)) %>%
    left_join(labels, by = "jurisdiction") %>%
    ggplot(aes(date, fitted100, color=change, fill=change)) +
    geom_line(aes(date, covid19100), lty=2, show.legend = FALSE) +
    geom_ribbon(aes(ymin=lwr100, ymax=upr100), alpha=0.20, color=NA, show.legend = FALSE) +
    geom_line(show.legend = FALSE) +
    geom_dl(aes(color=change, label=paste0("",round(last_fitted,1))), method=list("last.points", fontface="bold", cex=0.90)) +
    scale_color_manual(values = c("Upward" = "#cb181d", "Downward" = "#2171b5")) +
    scale_fill_manual(values = c("Upward" = "#cb181d", "Downward" = "#2171b5")) +
    scale_x_date(date_labels = "%b %Y", limits = c(start, end+weeks(2))) +
    scale_y_continuous(labels = scales::comma) +
    ylab("Cumulative excess mortality per 100,000") +
    xlab("Date") +
    facet_wrap(~jurisdiction, nrow=2, ncol=3) +
    theme_sandstone()
}

##
plot_excess_deaths <- function(dat, jurisdictions, start, end, ci_ind, pop_ind, c19_ind)
{
  # -- State specific data
  jurisdiction_dat <- dat %>%
    filter(jurisdiction %in% jurisdictions, 
           date >= start, date <= end) %>%
    mutate(jurisdiction = factor(jurisdiction, levels = jurisdictions)) %>%
    mutate(fitted100  = 100000 * fitted / population,
           covid19100 = 100000 * covid19/ population,
           se100      = 100000 / population * se,
           lwr100     = fitted100 - se100 * 1.96,
           upr100     = fitted100 + se100 * 1.96)
  
  # -- Used for labeling
  last_dp <- jurisdiction_dat %>%
    group_by(jurisdiction) %>%
    filter(date >= max(date)-8) %>%
    ungroup() %>%
    mutate(label = paste0(" ", jurisdiction))
  
  if(c19_ind){
    if(pop_ind == "Totals"){
      
      # -- Used to determine y-axis
      y_limits <- range(jurisdiction_dat$lwr, jurisdiction_dat$upr, jurisdiction_dat$covid19)
      edays    <- weeks(2)
      
      # -- Making Viz
      p <- jurisdiction_dat %>%
        ggplot(aes(date, fitted, label=jurisdiction, color=jurisdiction)) +
        geom_line(size=1, show.legend = FALSE, data = jurisdiction_dat) +
        geom_line(aes(date, covid19), lty=2, show.legend = FALSE, data = jurisdiction_dat) +
        geom_dl(method=list("last.points", fontfamily="Helvetica", fontface="bold", cex=1), data = last_dp) +
        ylab("Cumulative excess deaths") +
        xlab("Date") +
        # ggtitle("Cumulative Excess Deaths", subtitle = "The dashed-curves correspond to the COVID-19 death toll") +
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
      y_limits <- range(jurisdiction_dat$lwr100, jurisdiction_dat$upr100, jurisdiction_dat$covid19100)
      edays    <- weeks(2)
      
      # -- Making Viz
      p <- jurisdiction_dat %>%
        ggplot(aes(date, fitted100, color=jurisdiction)) +
        geom_line(size=1, show.legend = FALSE, data = jurisdiction_dat) +
        geom_line(aes(date, covid19100), lty=2, show.legend = FALSE, data = jurisdiction_dat) +
        geom_dl(aes(label=label), method=list("last.points", fontfamily="Helvetica", fontface="bold", cex=1), data = last_dp) +
        ylab("Cumulative excess deaths per 100,000") +
        xlab("Date") +
        # ggtitle("Cumulative Excess Deaths per 100,000", subtitle = "The dashed-curves correspond to the COVID-19 death toll") +
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
    
  } else{
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
        # ggtitle("Cumulative Excess Deaths") +
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
        # ggtitle("Cumulative Excess Deaths per 100,000") +
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
  }
  return(p)
}

##
plot_excess_v_covid <- function(dat, pc)
{
  # -- Getting jurisdictions
  jurisdictions <- unique(dat$jurisdiction)

  # -- Getting excess deaths data
  ed <- get_excess_deaths(dat, jurisdictions, start = make_date(2020,03,01), end = today())
  
  # -- Getting trend
  labels <- pc %>%
    group_by(jurisdiction) %>%
    mutate(label  = c(NA, diff(fitted)),
           change = ifelse(last(label) > 0, "Upward", "Downward")) %>%
    ungroup() %>%
    select(jurisdiction, change) %>%
    unique()
  
  # -- Viz
  ed %>%
    group_by(jurisdiction) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    mutate(difference = 100000 * (fitted - covid19) / population,
           diff_se = 100000 * se / population,
           diff_lwr = difference - 1.96 * diff_se,
           diff_upr = difference + 1.96 * diff_se) %>%
    left_join(labels, by = "jurisdiction") %>%
    ggplot(aes(reorder(jurisdiction, difference), difference, color = change)) +
    geom_hline(yintercept = 0, lty=2, color="gray") +
    geom_errorbar(aes(ymin = diff_lwr, ymax = diff_upr), width = 0.10, show.legend = FALSE) +
    geom_point(size=2, show.legend = FALSE) +
    geom_point(size=2, color = "black", pch = 1) +
    coord_flip() +
    scale_y_continuous(breaks = seq(-100, 200, by=25)) +
    scale_color_manual(values = c("Upward" = "#cb181d", "Downward" = "#2171b5")) +
    ylab("Excess deaths minus reported Covid-19 deaths per 100,000") +
    xlab("") +
    theme_sandstone()
}

##
plot_us_map <- function(percent_change, us_map, temp_date)
{
  # -- Fixing date bug
  temp_date <- ymd(parse_date_time(temp_date, orders = c("bdy", "bY")))
  
  # - Viz
  percent_change %>%
    filter(type == "weighted") %>%
    select(date, jurisdiction, fitted) %>%
    filter(!grepl("New York", jurisdiction)) %>%
    bind_rows(ny_dat) %>%
    filter(date == temp_date) %>%
    mutate(fitted_cat = cut(fitted, breaks = c(-Inf, 0, 0.20, 0.40, 0.60, 0.80, 1, Inf), include.lowest = TRUE, right=FALSE),
           fitted_con = ifelse(fitted < 0, 0, fitted),
           fitted_con = ifelse(fitted > 1, 1, fitted_con)) %>%
    {merge(us_map, .,by.x = "NAME", by.y = "jurisdiction", all.y = T)} %>%
    ggplot() +
    geom_sf(fill= "#636363", size = 0.15, data = us_map) +
    geom_sf(aes(fill = fitted_con), size = 0.15) +
    geom_text(aes(X, Y, label = STUSPS),
              data     = us_map,
              size     = 2.0,
              color    = "black",
              fontface = "bold") +
    ggtitle(paste0(" Week ending on ", format(temp_date, "%b %d, %Y"))) +
    scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"),
                         name = "Percent \nChange",
                         limits= c(0, 1),
                         labels = scales::percent) +
    theme_void() +
    theme(legend.position = "right",
          legend.text     = element_text(face = "bold", color = "#525252"),
          legend.title    = element_text(face = "bold", color = "#525252"),
          plot.title      = element_text(face = "bold", color = "#525252"),
          plot.background = element_rect(fill = "#e6dccd"))
}

##
plot_us_map_ed <- function(usa_ed_dat, us_map, temp_date)
{
  # -- Fixing date bug
  temp_date <- ymd(parse_date_time(temp_date, orders = c("bdy", "bY")))
  
  # - Viz
  usa_ed_dat %>%
    filter(!grepl("New York", jurisdiction)) %>%
    select(date, jurisdiction, fitted, population) %>%
    bind_rows(ny_ed_dat) %>%
    mutate(fitted = 100000 * fitted / population,
           fitted = ifelse(fitted < 0, 0, fitted),
           fitted = ifelse(fitted > 200, 200, fitted)) %>%
    filter(date == temp_date) %>%
    {merge(us_map, .,by.x = "NAME", by.y = "jurisdiction", all.y = T)} %>%
    ggplot() +
    geom_sf(fill= "#636363", size = 0.15, data = us_map) +
    geom_sf(aes(fill = fitted), size = 0.15) +
    geom_text(aes(X, Y, label = STUSPS),
              data     = us_map,
              size     = 2.0,
              color    = "black",
              fontface = "bold") +
    ggtitle(paste0(" Week ending on ", format(temp_date, "%b %d, %Y"))) +
    scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"),
                         name = "Cumulative excess \ndeaths per 100,000 \npeople",
                         limits= c(0, 200)) +
    theme_void() +
    theme(legend.position = "right",
          legend.text     = element_text(face = "bold", color = "#525252"),
          legend.title    = element_text(face = "bold", color = "#525252"),
          plot.title      = element_text(face = "bold", color = "#525252"),
          plot.background = element_rect(fill = "#e6dccd"))
}

##
plot_world_map <- function(pc, map, temp_date)
{
  # -- Fixing date bug
  temp_date <- ymd(parse_date_time(temp_date, orders = c("bdy", "bY")))

  # -- Viz
  percent_change_countries %>%
    select(date, jurisdiction, fitted) %>%
    filter(date == temp_date) %>%
    mutate(fitted_con = ifelse(fitted < 0, 0, fitted),
           fitted_con = ifelse(fitted > 1, 1, fitted_con)) %>%
    {merge(world, .,by.x = "name", by.y = "jurisdiction", all.y = T)} %>%
    ggplot() +
    geom_sf(fill= "#636363", size = 0.15, data = world) +
    geom_sf(aes(fill = fitted_con), size = 0.15) +
    coord_sf(ylim = c(-60, 85), expand = FALSE) +
    ggtitle(paste0(" Week ending on ", format(temp_date, "%b %d, %Y"))) +
    scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"),
                         name = "Percent \nChange",
                         limits= c(0, 1),
                         labels = scales::percent) +
    theme_void() +
    theme(legend.position = "right",
          legend.text     = element_text(face = "bold", color = "#525252"),
          legend.title    = element_text(face = "bold", color = "#525252"),
          plot.title      = element_text(face = "bold", color = "#525252"),
          plot.background = element_rect(fill = "#e6dccd"),
          panel.background = element_rect(fill = "#e6dccd", color = "#e6dccd"))
}

##
plot_world_map_ed <- function(world_ed_dat, map, temp_date)
{
  # -- Fixing date bug
  temp_date <- ymd(parse_date_time(temp_date, orders = c("bdy", "bY")))
  
  # - Viz
  world_ed_dat %>%
    select(date, jurisdiction, fitted, population) %>%
    mutate(fitted = 100000 * fitted / population,
           fitted = ifelse(fitted < 0, 0, fitted),
           fitted = ifelse(fitted > 200, 200, fitted)) %>%
    filter(date == temp_date) %>%
    {merge(world, .,by.x = "name", by.y = "jurisdiction", all.y = T)} %>%
    ggplot() +
    geom_sf(fill= "#636363", size = 0.15, data = world) +
    geom_sf(aes(fill = fitted), size = 0.15) +
    ggtitle(paste0(" Week ending on ", format(temp_date, "%b %d, %Y"))) +
    scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"),
                         name = "Cumulative excess \ndeaths per 100,000 \npeople",
                         limits= c(0, 200)) +
    theme_void() +
    theme(legend.position = "right",
          legend.text     = element_text(face = "bold", color = "#525252"),
          legend.title    = element_text(face = "bold", color = "#525252"),
          plot.title      = element_text(face = "bold", color = "#525252"),
          plot.background = element_rect(fill = "#e6dccd"))
}
