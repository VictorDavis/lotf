
## string utilities
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
escape <- function(x) gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", x)
left <- function(str, x) substring(str, 1, x)
right <- function(str, x) left(paste0(rev(strsplit(str,split = "")[[1]]), collapse=""), x)

## returns file contents without unzipping data directory
readLOTF <- function(filename) {
  
  con <- unz("data.zip", filename)
  contents <- readLines(con)
  close(con)
  contents
}

## reads section of lf3.etx, loading entire thing into RAM takes too long
readETX <- function(beg, end) {
  fname <- "data/lf3.etx"
  if (!file.exists(fname)) unzip("data.zip", "lf3.etx", exdir = "data")
  scan(file = fname, what = "character", skip = beg, nlines = end - beg, encoding = "latin1")
}

## reads section of lf3.txt, loading entire thing into RAM takes too long
readLF3 <- function(beg, end) {
  fname <- "data/lf3.txt"
  scan(file = fname, what = "character", skip = beg, nlines = end - beg)
}

## merge aut/ftf for list of titles by author, with ids
getTitleAuthor <- function() {
  author <- readLOTF("lf3.aut") ## author file, alphabetical
  titles <- readLOTF("lf3.ftf") ## title file
  id <- readLOTF("lf3.id") ## metadata file
  
  ## authornbr <- sapply(titles, function(x) as.numeric(substr(x,1,3))) ## extract author number
  authornbr <- as.numeric(substr(titles,1,3))+1
  titles <- as.data.frame(cbind(substr(titles,5,100), authornbr, author[authornbr], id))
  names(titles) <- c("title", "authornbr", "author", "meta")
  
  titles$title <- trim(titles$title)
  titles$author <- trim(titles$author)
  titles
}

## just get authors
getAuthors <- function() {
  ta <- getTitleAuthor()
  unique(ta$author)
}

## get titles by author
getTitles <- function(author = null) {
  ta <- getTitleAuthor()
  if (is.null(author)) {
    ta
  } else {
    ta[ta$author == author,"title"]
  }
}

## get text by author/title
getText <- function(n = NA, title = NA, author = NA) {
  if (is.na(n)) {
    ta <- getTitleAuthor()
    title_matches <- (ta$title == title)
    if (sum(title_matches) == 1) {
      n <- which(title_matches)
    } else if (sum(title_matches > 1)) {
      n <- which((ta$author == author) & title_matches)
    }
  }
  
  if (!is.na(n)) {
    print(n)
    lim <- getLimits()
    
    beg <- lim[n]
    end <- lim[n+1]
    if (file.exists("lf3.txt")) {
      text <- readLF3(beg, end)
    } else {
      text <- readETX(beg, end)
      text <- decode(text)
    }
  } else {
    text <- list()
  }
  
  text
}

## decodes text based on super-secret encryption key
decode <- function(text) {
  ## loop through lines
  for (i in 1:length(text)) {
    line <- text[i]
    line <- gsub("\037.$", "", line) # warning: deleting information after the lf
    line <- gsub("\037.","\037", line) # begin paragraph
    plain <- if (line == "") {
      ""
    } else {
      sapply(strsplit(line, split="")[[1]], decodeChar)
    }
    line <- paste0(plain, collapse = "")
    # line <- gsub("^\\{.*\\}$", "", line) # delete lines of the form {chapter, verse, etc}
    # wild[grep("^\\{(([A-Z0-9_])* )?\\^([a-z])* ([0-9])*\\}$",wild)]
    line <- gsub("\035", "<i>", line) #italics
    line <- gsub("\036", "</i>", line)
    line <- gsub("^\037","<br>", line) # begin paragraph: don't use <p> as cipher contains no close tag symbol
    line <- gsub("\037","&nbsp;&nbsp;&nbsp;&nbsp;", line) # tabspace?
    line <- gsub("^-","<br>", line) # blank line
    text[i] <- line
  }
  
  text
}

## extracts {bracketed} TOC information
getTOC <- function(text) {
  skeleton <- "^<br>\\{([^a-z\\^]*)\\^([a-z]*) ([0-9]*)\\}$"
  bMatch <- grepl(skeleton, text)
  nMatch <- grep(skeleton, text)
  cMatch <- text[bMatch]
  iMatch <- regexec(skeleton, cMatch)
  rMatch <- regmatches(cMatch, iMatch)
  nav <- data.frame(t(sapply(rMatch, c)), stringsAsFactors = F)
  if (length(nav) > 0) {
    names(nav) <- c("expression", "location", "block", "cursor")
    nav$location <- trim(nav$location)
    block <- nav[1,"block"]
    nav <- data.frame(nav, n = sapply(1:nrow(nav), function(x) min(which(text == nav[x,"expression"]))))
    
    toc <- unique(nav[,c("location")])
    if (length(toc) > 1) {
      toc <- data.frame(toc, n = sapply(1:length(toc), function(x) {
        expr <- escape(toc[x])
        expr <- paste0("(<br>)?", expr)
        min(grep(expr, text))
      }))
      toc$expression <- paste0(gsub("\\|", ",",gsub("\\_"," ",toc$toc)), ": ", gsub("<br>","",text[toc$n+1]))
      df <- data.frame(expression = toc$expression, location = toc$toc, block = rep(block, nrow(toc)), cursor = rep(0, nrow(toc)), n = toc$n)
      nav <- rbind(nav, df)
      nav <- nav[order(nav$n),]
    }
    nav$n2 <- nav$n - 1:nrow(nav) + 1
  } else { nav <- list() }
  
  nav
}

## deletes all location markers
htmlify <- function(text, createTOC = T) {
  nav <- getTOC(text)
  toc <- NA
  
  if (length(nav) > 0) {
    html <- text[-nav$n]
    if (sum(nav$cursor == 0) > 0) {
      toc <- nav[nav$cursor == 0, c("expression", "location","n2")]
      tocHTML <- paste0("<a href=\"#",toc$location,"\">",toc$expression,"</a>")
      # chapter markers
      html[toc$n2] <- gsub("<br>","",html[toc$n2])
      html[toc$n2] <- paste0("<br><a name = \"", toc$location, "\" class=\"chapter\">", html[toc$n2], "</a>")
      
      # append TOC
      if (createTOC) {
        html <- c(tocHTML, html)
      }
    }
  } else {
    html <- text
  }
  
  html <- html[!grepl("\u0085[iI]", html)]
  html <- html[!grepl("\\{", html)]
  html <- html[!grepl("\\}", html)]
  html <- html[grepl("[a-z]", html) | !grepl("[\\_]", html)]
  
  html <- gsub("\u0085\u0080(.*)\\.(cif|CIF)\u0085\u0080", "<p><img src=\"jpg/\\1\\.jpg\"/></p>", html)
  
  # convert <br> to <p>
  html <- paste0(html, collapse = " ")
  html <- gsub("<br>", "</p><break><p>", html)
  html <- strsplit(html, split = "<break>")[[1]]
  closed <- grepl("^<p>(.*)</p>$", html)
  leading <- grepl("^<p>(.*)$", html)
  trailing <- grepl("^(.*)</p>$", html)
  html[leading & !closed] <- paste0(html[leading & !closed], "</p>", collapse = "")
  html[trailing & !closed] <- paste0("<p>", html[trailing & !closed], collapse = "")
  html <- gsub(" </p>", "</p>", html)
  
  html <- gsub("<p>", "<div>", html)
  html <- gsub("</p>", "</div>", html)
  
  html
}

## super-secret encryption key
decodeChar <- function(x) {
  intX <- utf8ToInt(x) # utf8ToInt(iconv(x, "latin1", "utf-8")) # 
  if (intX > 122) {
    intX <- intX - 121
    intToUtf8(intX)
  } else if (intX < 32) {
    intToUtf8(intX)
  } else {
    paste0(" ", intToUtf8(intX), collapse = "")
  }
}

## returns title breaks, limits
getLimits <- function() {
  lim <- readLOTF("lf3.lim") ## limit file
  s <- sapply(lim, function(x) as.numeric(substr(x,1,10)))
  names(s) <- NULL
  s <- s / 78 ## why?
  s
}

## write book to html file
writeHTML <- function(n, ta) {
  # fetch, decode & htmlify text
  text <- getText(n)
  html <- htmlify(text, createTOC = F)
  
  # format file name
  title <- gsub("( )+","_",gsub("[[:punct:]]", "", ta[n,"title"]))
  author<- gsub("( )+","_",gsub("[[:punct:]]", "", ta[n,"author"]))
  
#   # if work contains chapters, write to single file
#   if (sum(grepl("class=\"chapter\">", html)) > 0) {
#     fname <- sprintf("html/%s-%s.html", author, title)
#   } else {
#     # combine all chapter-less works into single "collected works" file
#     first_line <- sprintf("<br><a name=\"%s\" class=\"chapter\">%s</a>", title, ta[n,"title"])
#     html <- c(first_line, html)
#     fname <- sprintf("html/%s-Collected_Works.html", author)
#   }
  
  fname <- sprintf("html/%s-%s.html", author, title)
  
  # write lines
  print(fname)
  f = file(fname, "a")
  writeLines(html, f)
  close(f)
}

## write all books to html
writeAll <- function() {
  ta <- getTitleAuthor()
  for (i in 1:nrow(ta)) {
    writeHTML(i, ta)
  }
  
  # now convert them all to ebooks
  flist <- list.files("html", "*.html", full.names = T)
  iMatch <- regexec("^html/(.*)\\-(.*)\\.html$", flist)
  rMatch <- regmatches(flist, iMatch)
  df <- data.frame(t(sapply(rMatch, c)), stringsAsFactors = F)
  names(df) <- c("htmlname","author","title")
  df$epubname <- gsub("html","epub",df$htmlname)
  df$title <- gsub("_"," ",df$title)
  df$author <- sapply(df$author, function(x) {
    ttl <- strsplit(x, "_")[[1]]
    if (length(ttl) > 1) {
      ttl <- c(ttl[2:length(ttl)], ttl[1])
      ttl <- paste(ttl, collapse=" ")
    }
    ttl
  })
  
  # write all html to epub
  logname <- "write.log"
  if (file.exists(logname)) {
    file.remove(logname)
  }
  for (i in 1:nrow(df)) {
    dx <- df[i,]
    cmd <- sprintf("ebook-convert %s %s --chapter \"//*[@class=\\\"chapter\\\"]\" --authors \"%s\" --title \"%s\" --language en --remove-paragraph-spacing >> %s", dx$htmlname, dx$epubname, dx$author, dx$title, logname)
    system(cmd)
  }
}