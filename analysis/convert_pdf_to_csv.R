# Data sources (Administration de la navigation aérienne):
# https://data.public.lu/fr/datasets/archives-bruit-des-avions/
# https://data.public.lu/fr/datasets/statistiques-bruit-des-avions/

#install.packages('pdftools')
library(pdftools)

extractPanel <- function(df, ytlim, xrlim, yblim, xllim) {
  # return all the items that fall within the bounds

  # if passed a tibble, convert to vanilla df
  if ( class(df)[1] == "tbl_df" ) {
    df <- as.data.frame(df)
  }
  
  # if it's something else, like a list, die
  if ( class(df)[1] != "data.frame" ) {
    stop(paste("extractPanel needs a data.frame not a", class(df)))
  }
  
  # spatially subset
  sub_df <- df[  df$x > xllim &
                 (df$x + df$width) < xrlim &
                 df$y > ytlim &
                 (df$y + df$height) < yblim,]
  
  # order based on
  ord_sub_df <- sub_df[order(sub_df$y, sub_df$x), ]
  
  # return it
  return(ord_sub_df)
}
fixRows <- function(udf, method="nudge", classes_number=NA) {
  # unprocessed data frame passed
  
  if ( method == "classify" ) {
    library(classInt)
    
    if ( is.na(classes_number) ) {
      stop("Method classify requires a number of rows (classes_number) be provided! Otherwise used method 'cluster'.")
    }
    
    # classify
    ci <- classify_intervals(udf$y, classes_number, style="jenks")
    
    # overwrite the y values within each group
    fdf <- do.call('rbind', lapply(split(udf, ci), function(g) {
      g$y <- median(g$y)
      return(g)
    }))
  }
  
  # pushes data into next row if a neighbouring row exists (+/-1 in y value)
  if ( method == "nudge" ) {
    need_to_run <- TRUE

    # split day's data by y coordinate value
    pr <- split(udf, udf$y)
    
    # get the list of list names as int
    row_ycoords <- as.integer(names(pr))
    
    # repeat 'nudging' process as needed
    while ( need_to_run ) {
      need_to_run <- FALSE
      
      # for each row in the data set
      for( rnum_name in names(pr) ) {
        # rnum_name <- "175"
        row_y <- as.integer(rnum_name)
        
        if ((row_y + 1) %in% row_ycoords) {
          rnum_next_name <- as.character(row_y + 1)
          
          # create the combined dataframe
          comb_lines <- rbind( pr[rnum_name][[1]], pr[rnum_next_name][[1]])
          
          # add the rows from the next data frame to the current data frame, sorted
          pr[rnum_name][[1]] <- comb_lines[order(comb_lines$x),]
          
          # look at what we've copied to the row above
          # if there's data, then we need to rerun the process
          if ( nrow(pr[[rnum_next_name]]) > 0 ) {
            need_to_run <- TRUE
          }
          
          # replace the next data frame with no rows (but don't delete!)
          pr[rnum_next_name][[1]] <- pr[[rnum_next_name]][FALSE,]
        }
      }
    }
    
    # now remove empty rows  - get the 'good' rows
    fdf <- pr[sapply(pr, nrow) > 0]
  }
  
  # pass fixed data frame
  return(fdf)
}

# process each PDF file
for ( pdf_fn in list.files("../source_pdfs/") ) {
  #pdf_fn <- list.files("../source_pdfs/")[9]
  pdf_fp = paste0("../source_pdfs/", pdf_fn)
  csv_fp = paste0("../export_csv/", strsplit(pdf_fn, "-")[[1]][1], "_airport_noise.csv")
  
  # don't process if the file already exists
  if (file.exists(csv_fp)) {
    print(paste("File", csv_fp, "already exists. Skipping processing source file", pdf_fp, "."))
  } else {
    # retrieve the data from the pdf
    all_pdf <- pdf_data(pdf_fp)
    
    # clean each page and row bind results
    cln_pdf <- do.call('rbind', lapply(all_pdf, function(apdf) {
      #apdf <- all_pdf[[49]]
      apdf <- as.data.frame(apdf)
      
      # get the table
      ndata <- extractPanel(apdf, ytlim=172, xrlim=436, yblim=443, xllim=33)
      
      # figure out the number of rows in the table by counting dates
      # they are particular in that they have a trailing space (usually)
      trows = sum(ndata$space)
      
      # sometime the spaces are missing - page 1378 of 2017 file (30/10/17 NMT#4)
      if ( trows == 0 ) {
        # something changed, but can count number of xx/xx/xx formatted strings
        trows <- sum(sapply(strsplit(ndata$text, '/'), length) == 3)
      }
      # cluster the rows, provide the table row count
      fndata <- fixRows(ndata, method = "classify", classes_number = trows)
      
      # get the location
      nloc <- extractPanel(apdf, ytlim=140, yblim=160, xllim=500, xrlim=800)
      
      # add it to the table
      locstr <- paste(nloc$text[2:nrow(nloc)], collapse=" ")
      
      # only want the date-time and aircraft count, env. noise, aircraft noise
      fndf <- data.frame(t(sapply(split(fndata, fndata$y), function(drow) {
        #drow <- split(fndata, fndata$y)[[1]]
        drow$text
      }))[,1:6])
      colnames(fndf) <- c('date', 'hour', 'aircraft_events', 'total_LDEN_dB', 'aircraft_LDEN_dB', 'bg_LDEN_dB')
      fndf$site <- locstr
      
      # replace empty with NA
      fndf[fndf == '-'] <- NA
      
      return(fndf)
    }))
    
    # save to csv file
    write.table(cln_pdf, file = csv_fp, sep=",", quote = FALSE, row.names = FALSE)
    print(paste("File", csv_fp, "written."))
  }
}
