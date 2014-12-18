#' Update an R package description file after performing a git commit
#' 
#' The function updates the version number by increasing the patch number by one. The date is set to the current date, and the SHA entry is set to the first 8 characters of the last commit hash. Changes are then ammended to the last commit. This function should be executed always after performing a commit (when not amended to previous one!!).

update.description <- function()
{
  # Read description file
    description <- scan(file='DESCRIPTION',what=character(),sep='\n')
    
  # Get last commit hash
    hash <- substr(scan(file='.git/refs/heads/master',what=character()),1,8)
  
  # Update date
    nr <- which(grepl('Date',description))
    description[nr] <- paste('Date:',Sys.Date())
  
  # Update version number
    nr <- which(grepl('Version',description))
    version <- strsplit(description[nr],' ')[[1]][2]
    version <- strsplit(version,'[.]')[[1]]
    description[nr] <- paste0('Version: ',version[1],'.',version[2],'.',as.numeric(version[3])+1)
  
  # Update SHA (or insert if not yet available)
    if(any(grepl('SHA',description)))
    {
      nr <- which(grepl('SHA',description))
    } else {
      nr <- length(description)+1
    }
    description[nr] <- paste('SHA:',hash)
    
  # Write updated description file
    fileConn<-file("DESCRIPTION")
    writeLines(description, fileConn)
    close(fileConn)
    
  # this part interferes with RStudio apparently
  # do manual commit with amend for now
#     system('git add DESCRIPTION')
#     system('git commit --amend')
}


