
## string utilities
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
escape <- function(x) gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", x)

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
  if (!file.exists(fname)) unzip("data.zip", fname, exdir = "data")
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

## builds key from Yeats' poem
yeats <- function(data) {
    ## hardcoded 12 lines of yeats' text
    ciph <- data[matrix(3581134 + outer(1:4,5*0:2,'+'), ncol=1)[,1]]
    ciph <- sapply(1:12, function(x) regmatches(ciph[x], regexec("\037,(.*)\037[0-9;]", ciph[x]))[[1]][2])
    ciph <- lapply(ciph, function(x) strsplit(x, split="")[[1]])
    
    plain <- readLines("1770.txt")
    plain <- lapply(plain, function(x) strsplit(x, split="")[[1]])
    
    key <- lapply(1:12, function(x) cbind(plain[[x]], ciph[[x]]))
    d <- as.data.frame(key[[1]], stringsAsFactors = FALSE)
    for (i in 2:12) d <- rbind(d, as.data.frame(key[[i]], stringsAsFactors = FALSE))
    names(d) <- c("plain", "cipher")
    d <- unique(d[order(d[,2],d[,1]),])
    d <- d[d$plain != d$cipher,]
    
    d
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
    line <- gsub("^\037","<p>", line) # begin paragraph
    line <- gsub("\037","&nbsp;&nbsp;&nbsp;&nbsp;", line) # begin paragraph
    line <- gsub("^-","<br><br>", line) # begin paragraph
    text[i] <- line
  }
  
  text
}

## extracts {bracketed} TOC information
getTOC <- function(text) {
  skeleton <- "^<p>\\{([^a-z\\^]*)\\^([a-z]*) ([0-9]*)\\}$"
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
      toc <- data.frame(toc, n = sapply(1:length(toc), function(x) min(grep(paste0("(<p>)?",toc[x]), text))))
      toc$expression <- paste0(gsub("\\|", ",",gsub("\\_"," ",toc$toc)), ": ", gsub("<p>","",text[toc$n+1]))
      df <- data.frame(expression = toc$expression, location = toc$toc, block = rep(block, nrow(toc)), cursor = rep(0, nrow(toc)), n = toc$n)
      nav <- rbind(nav, df)
      nav <- nav[order(nav$n),]
    }
    nav$n2 <- nav$n - 1:nrow(nav) + 1
  } else { nav <- list() }
  
  nav
}

## deletes all location markers
htmlify <- function(text) {
  nav <- getTOC(text)
  toc <- NA
  
  if (length(nav) > 0) {
    html <- text[-nav$n]
    if (sum(nav$cursor == 0) > 0) {
      toc <- nav[nav$cursor == 0, c("expression", "location","n2")]
      tocHTML <- paste0("<a href=\"#",toc$location,"\">",toc$expression,"</a>")
      # chapter markers
      html[toc$n2] <- paste0("<a name = \"", toc$location, "\" class=\"chapter\">", html[toc$n2], "</a>")
      
      # append TOC
      html <- c(tocHTML, html)
    }
  } else {
    html <- text
  }
  
  html <- html[!grepl("\u0085[iI]", html)]
  html <- html[!grepl("\\{", html)]
  html <- html[!grepl("\\}", html)]
  html <- html[!grepl("\\_", html)]
  
  html <- gsub("\u0085\u0080(.*)\\.(cif|CIF)\u0085\u0080", "<p><img src=\"jpg/\\1\\.jpg\"/></p>", html)
  html
}

## super-secret encryption key
decodeChar <- function(x) {
  intX <- utf8ToInt(x) #iconv(x, "latin1", "utf-8"))
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