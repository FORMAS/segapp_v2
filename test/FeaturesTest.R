
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
  # TEST FALSE
  test.data <- list(read.table(text = 'type start end features void
                               2 word     1   9 POS=NNP, chunk_tag=B-NP
                               3 word    10  10 POS=,, chunk_tag=O
                               4 word    12  16 POS=WDT, chunk_tag=B-NP
                               5 word    18  20 POS=VBZ, chunk_tag=B-VP
                               6 word    22  24 POS=CD, chunk_tag=B-NP
                               7 word    26  34 POS=NNS, chunk_tag=I-NP
                               8 word    35  35 POS=,, chunk_tag=O
                               9 word    37  45 POS=VBD, chunk_tag=B-VP
                               10 word    47  47 POS=DT, chunk_tag=B-NP
                               11 word    49  54 POS=NN, chunk_tag=I-NP
                               12 word    56  61 POS=NN, chunk_tag=I-NP
                               13 word    63  66 POS=WDT, chunk_tag=B-NP
                               14 word    68  74 POS=VBZ, chunk_tag=B-VP
                               15 word    76  81 POS=NNS, chunk_tag=B-NP
                               16 word    82  82 POS=,, chunk_tag=O
                               17 word    84  90 POS=NNS, chunk_tag=B-NP
                               18 word    92  94 POS=CC, chunk_tag=O
                               19 word    96 100 POS=NN, chunk_tag=B-NP
                               20 word   102 107 POS=NNS, chunk_tag=I-NP
                               21 word   109 110 POS=TO, chunk_tag=B-VP
                               22 word   112 118 POS=VB, chunk_tag=I-VP
                               23 word   120 128 POS=NNS, chunk_tag=B-NP
                               24 word   129 129 POS=,, chunk_tag=O
                               25 word   131 137 POS=NNS, chunk_tag=B-NP
                               26 word   139 141 POS=CC, chunk_tag=O
                               27 word   143 147 POS=JJ, chunk_tag=B-NP
                               28 word   149 157 POS=NNS, chunk_tag=I-NP
                               29 word   159 172 POS=RB, chunk_tag=B-ADVP
                               30 word   174 177 POS=IN, chunk_tag=B-PP
                               31 word   179 179 POS=DT, chunk_tag=B-NP
                               32 word   181 186 POS=JJ, chunk_tag=I-NP
                               33 word   188 197 POS=NN, chunk_tag=I-NP
                               34 word   198 198 POS=., chunk_tag=O'))
  checkEquals(f12(test.data), 0)
  
  # TEST FALSE
  test.data <- list(read.table(text = 'type start end features void
                               2 word     1   9 POS=NNP, chunk_tag=B-NP
                               3 word    10  10 POS=,, chunk_tag=O
                               4 word    12  16 POS=WDT, chunk_tag=B-NP
                               5 word    18  20 POS=VBZ, chunk_tag=B-VP
                               6 word    22  24 POS=CD, chunk_tag=B-NP
                               7 word    26  34 POS=NNS, chunk_tag=I-NP
                               8 word    35  35 POS=,, chunk_tag=O
                               9 word    37  45 POS=VBD, chunk_tag=B-VP
                               10 word    47  47 POS=DT, chunk_tag=B-NP
                               11 word    49  54 POS=NN, chunk_tag=I-NP
                               12 word    56  61 POS=NN, chunk_tag=I-NP
                               13 word    63  66 POS=WDT, chunk_tag=B-NP
                               14 word    68  74 POS=VBZ, chunk_tag=B-VP
                               15 word    76  81 POS=NNS, chunk_tag=B-NP
                               16 word    82  82 POS=,, chunk_tag=O
                               17 word    84  90 POS=NNS, chunk_tag=B-NP
                               18 word    92  94 POS=CC, chunk_tag=O
                               19 word    96 100 POS=NN, chunk_tag=B-NP
                               20 word   102 107 POS=NNS, chunk_tag=I-NP
                               21 word   109 110 POS=TO, chunk_tag=B-VP
                               22 word   112 118 POS=VB, chunk_tag=I-VP
                               23 word   120 128 POS=NNS, chunk_tag=B-NP
                               24 word   129 129 POS=,, chunk_tag=O
                               25 word   131 137 POS=NNS, chunk_tag=B-NP'))
  checkEquals(f12(test.data), 1)
}
