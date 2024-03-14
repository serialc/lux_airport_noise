# Data sources:
# https://data.public.lu/fr/datasets/archives-bruit-des-avions/
# https://data.public.lu/fr/datasets/statistiques-bruit-des-avions/

#install.packages('pdftools')
library(pdftools)
library(dplyr) 

# retrieve the pdf_data
all_pdf <- pdf_data('../source_pdfs/2020-byhour.pdf')

processed_pages <- lapply(all_pdf, function(pdf_page) {
  # convert to data.frame - and we only are doing the first page for now
  #pdf_page <- as.data.frame(all_pdf[[1]])
  #pdf_page <- as.data.frame(all_pdf[[4]])
  pdf_page <- as.data.frame(pdf_page)
  
  # separate data into rows (same y coordinate)
  page_rows <- split(pdf_page, pdf_page$y)
  
  # reorder and filter out graph data
  pr <- lapply(page_rows, function(x) {
    #x <- pagerows[[10]]
    # list inner dataframes by x coordinates
    ox <- x[order(x$x),]
    # remove any data in the 'bottom-right' graphs
    ox[(ox$x < 460 | ox$y < 159) & ox$y > 140 & ox$y < 460,]
    })
  
  # only keep the valid pdf rows (not empty)
  vpr <- pr[sapply(pr, nrow) > 0]
  
  # insight
  #sapply(vpr, nrow)
  
  # it's still messy - some lines have text across two rows due to slight vertical offset
  # go through, look if there's another set with similar y coordinate number
  vpr_names <- as.integer(names(vpr))
  
  # run the below until no longer needed
  need_to_run <- TRUE
  while ( need_to_run ) {
    need_to_run <- FALSE
    for( rnum in names(vpr) ) {
      # rnum <- "149"
      rnum_int <- as.integer(rnum)
      
      if ((rnum_int + 1) %in% vpr_names) {
        next_rnum <- as.character(rnum_int + 1)
        
        # create the combined dataframe
        comb_lines <- rbind( vpr[rnum][[1]], vpr[next_rnum][[1]])
        
        # add the rows from the next data frame to the current data frame, sorted
        vpr[rnum][[1]] <- comb_lines[order(comb_lines$x),]
        
        if ( nrow(vpr[[next_rnum]]) ) {
          need_to_run <- TRUE
        }
        # replace the next dataframe with no rows (but don't delete!)
        vpr[next_rnum][[1]] <- vpr[[next_rnum]][FALSE,]
      }
    }
  }
  
  # explore automation of clustering y values
  #plot(sapply(5:25,  function(i) { mean(kmeans(pdf_page$y, i)$withinss) }))
  
  # now remove empty rows  - get the pure rows
  pure <- vpr[sapply(vpr, nrow) > 0]
  
  location <- paste(collapse = " ", vpr[[1]]$text[2:length(vpr[[1]])])
  
  # extract the desired data
  roughrows <- sapply( names(pure), function(rowname) {
    row_name_int <- as.integer(rowname)
    # skip table headings
    if ( row_name_int < 170) { return() }
    
    # the x-coordinates are fuzzy - don't use
    # use the ORDER of the values
    return(pure[[rowname]]$text[1:6])
  })
  
  # remove the NULL
  rrfilter <- roughrows[!sapply(roughrows, function(x) {is.null(x)})]
  
  # now transform to a clean dataframe
  rr <- data.frame(do.call("rbind", rrfilter))
  
  # replace empty with NA
  rr[rr == '-'] <- NA
  colnames(rr) <- c('date', 'hour', 'aircraft_events', 'total_LDEN_dB', 'aircraft_LDEN_dB', 'bg_LDEN_dB')
  
  # add location
  rr$location <- location
  print(paste("Finished", rr$date[1]))
  return(rr)
  })

# clean up
pp <- do.call('rbind', processed_pages)

# number of rows and frequency
# 28  68  72  76  80  86  89  92  95  96 
# 1   1   5   3   1   1   1   1   5 348

# dates with an unusual normal of rows of data
# 01/01/20 01/01/21 - These are fine
# 05/03/20 06/03/20 07/03/20 08/03/20 09/03/20 10/03/20 11/03/20
# 12/01/20 12/02/20 13/03/20 20/08/20 20/09/20 20/12/20 21/12/20
# 25/10/20 26/06/20 29/03/20

# The first two are normal as LUX provides data 7am to 7am next day rather than midnight
# There are a couple other kinds of problems
# 1 - Some missing data
# 2 - Some hours are not rounded - but messy like 6:14 rather than 6:00
# 3 - Daylight savings (e.g., 29/03/20)

write.table(pp, file = '../export_csv/2020_airport_noise_data.csv', row.names = FALSE)

#### Visualize data for 2020 ####
read.table(file = '../export_csv/2020_airport_noise_data.csv')

pps <- split(pp, pp$location)

sapply(pps, function(sites) {
  #site <- pps[[1]]
  # only keep those with the magic number of rows - 24
  sapply(site$aircraft_events, nrow)
  do.call('rbind', split(site$aircraft_events, site$date))
})
sapply(split(pp$aircraft_events, pp$date), nrow)
      