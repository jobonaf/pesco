
# function: searches for matches to 'pattern'
xgrep <- function(pattern,string,where=FALSE) {
  dum <- gregexpr(pattern,string,fixed=TRUE)[[1]][1]
  if (where) {
    out <- dum
  } else {
    out <- dum > 0
  }
  return(out)
}

# function: extracts a word from a string
subwrd <- function(string,pos) {
  dum <- strsplit(string," ")[[1]]
  dum <- dum[!dum==""]
  out <- dum[pos]
  return(out)
}

# function: capital to small letters
small <- function(string) {
  if (is.character(string)) {
    dum <- ""
    for (i in 1:nchar(string)) {
      char <- substr(string,i,i)
      if (char %in% LETTERS) {char <- letters[match(char,LETTERS)]}
      dum <- paste(dum,char,sep="")
    }
    string <- dum
    return(string)
  } else {
    string <- as.character(string)
    return(string)
  }
}

# function: small to capital letters
capital <- function(strings) {
  if (is.character(strings)) {
    idx <- 0
    for(string in strings) {
      idx <- idx+1
      dum <- ""
      for (i in 1:nchar(string)) {
        char <- substr(string,i,i)
        if (char %in% letters) {char <- LETTERS[match(char,letters)]}
        dum <- paste(dum,char,sep="")
      }
      strings[idx] <- dum
    }
  } else {
    strings <- as.character(strings)
  }
  return(strings)
}

# function: beginning letters capital
Capital <- function(strings) {
  if (is.character(strings)) {
    idx <- 0
    for(string in strings) {
      idx <- idx+1
      dum <- ""
      for (i in 1:nchar(string)) {
        char <- substr(string,i,i)
        if (char %in% letters &
              ((i==1) |
                 (i>1 & substr(string,i-1,i-1) %in% c(" ",".","-","'")))) {
          char <- LETTERS[match(char,letters)]
        }
        dum <- paste(dum,char,sep="")
      }
      strings[idx] <- dum
    }
  } else {
    strings <- as.character(strings)
  }
  return(strings)
}

# function: trim
trim <- function(string) {
  for (i in 1:length(string)) {
    dum  <- ""
    char <- " "
    j <- nchar(string[i])+1
    while (char==" ") {
      j <- j-1
      char <- substr(string[i],j,j)
      if (! (char == " ")) {dum <- substr(string[i],1,j)}
    }
    string[i] <- dum
  }
  return(string)
}


