
test.f1 <- function(){
  # TEST TRUE
  test.data <- list(c(id='0', en1='160 employees', rel='be has by', en2='CourtLink', y='0', sentence='CourtLink, which has 160 employees, developed a filing system that enables judges, lawyers and court clerks to process pleadings, motions and other documents electronically over a secure connection.'))
  checkEquals(f1(test.data)[[1]], 0)
  # TEST FALSE
  test.data <- list(c(id='0', en1='160 employees', rel='be has by', en2='CourtLink', y='0', sentence='CourtLink, which has 160 employees'))
  checkEquals(f1(test.data)[[1]], 1)
}

test.f_prep <- function(){
  # TEST FALSE
  test.data <- list(read.table(text = '  pos chunk      words
    1  NN  B-NP        aim
    2  TO  B-VP         to
    3  VB  I-VP       form
    4  DT  B-NP          a
    5  JJ  I-NP   national
    6  NN  I-NP      unity
    7  NN  I-NP government
    8  IN  B-PP       with'
  ))
  checkEquals(f_prep(test.data, 'in')[[1]], 0)
  # TEST TRUE
  test.data <- list(read.table(text = '  pos chunk      words
    1  NN  B-NP        aim
    2  TO  B-VP         to
    3  VB  I-VP       form
    4  DT  B-NP          a
    5  JJ  I-NP   national
    6  NN  I-NP      unity
    7  NN  I-NP government
    8  IN  B-PP         in'
  ))
  checkEquals(f_prep(test.data, 'in')[[1]], 1)
}

test.f9 <- function(){
  # TEST FALSE
  test.data <- list(c(id='0', en1='160 employees', rel='be has by', en2='CourtLink', y='0', sentence='CourtLink, which has 160 employees, developed a filing system that enables judges, lawyers and court clerks to process pleadings, motions and other documents electronically over a secure connection.'))
  checkEquals(f9(test.data)[[1]], 0)
  # TEST TRUE
  test.data <- list(c(id='0', en1='160 employees', rel='be has by', en2='CourtLink', y='0', sentence='CourtLink, which has 160 employees be has by'))
  checkEquals(f9(test.data)[[1]], 1)
}

test.f10 <- function(){
  # TEST FALSE
  test.data <- list(c(id='0', en1='160 employees', rel='be has by', en2='CourtLink', y='0', sentence='CourtLink, which has 160 employees, developed a filing system that enables judges, lawyers and court clerks to process pleadings, motions and other documents electronically over a secure connection.'))
  checkEquals(f10(test.data)[[1]], 0)
  # TEST TRUE
  test.data <- list(c(id='0', en1='160 employees', rel='be has by', en2='CourtLink', y='0', sentence='CourtLink, which has 160 employees be has by CourtLink'))
  checkEquals(f10(test.data)[[1]], 1)
}

test.f11 <- function(){
  # TEST FALSE
  test.data <- list(c(id='0', en1='160 employees', rel='be has by', en2='CourtLink', y='0', sentence='CourtLink, which has 160 employees, developed a filing system that enables judges, lawyers and court clerks to process pleadings, motions and other documents electronically over a secure connection.'))
  checkEquals(f11(test.data)[[1]], 0)
  # TEST TRUE
  test.data <- list(c(id='0', en1='160 employees', rel='be has by', en2='CourtLink', y='0', sentence='CourtLink, which has 160 employees be has by CourtLink'))
  checkEquals(f11(test.data)[[1]], 1)
}

test.f12 <- function(){
  # TEST TRUE
  test.data <- list(tag(as.String("fuck fuck fuck fuck fuck")))
  checkEquals(f12(test.data), 1)
  
  # TEST FALSE
  test.data <- list(tag(as.String("fuck fuck fuck fuck fuck
                 fuck fuck fuck fuck fuck
                 fuck fuck fuck fuck fuck
                 fuck fuck fuck fuck fuck
                 fuck fuck fuck fuck fuck
                 fuck fuck fuck fuck fuck")))
  checkEquals(f12(test.data), 0)
}
