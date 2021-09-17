## aut survey indices

aut.SI1 <- 
  mfdb_sample_count(mdb, c('length'), c(list(
    data_source = 'iceland-ldist',
    sampling_type = 'AUT',
    length = mfdb_interval("len", c(5,15),open_ended = 'lower')),
    defaults))


aut.SI2 <- 
  mfdb_sample_count(mdb, c('length'), c(list(
    data_source = 'iceland-ldist',
    sampling_type = 'AUT',
    length = mfdb_interval("len", c(15,25),open_ended = 'lower')),
    defaults))


aut.SI3 <- 
  mfdb_sample_count(mdb, c('length'), c(list(
    data_source = 'iceland-ldist',
    sampling_type = 'AUT',
    length = mfdb_interval("len", c(25,35),open_ended = 'lower')),
    defaults))


aut.SI4 <- 
  mfdb_sample_count(mdb, c('length'), c(list(
    data_source = 'iceland-ldist',
    sampling_type = 'AUT',
    length = mfdb_interval("len", c(35,45),open_ended = 'lower')),
    defaults))


aut.SI5 <- 
  mfdb_sample_count(mdb, c('length'), c(list(
    data_source = 'iceland-ldist',
    sampling_type = 'AUT',
    length = mfdb_interval("len", c(45,55),open_ended = 'lower')),
    defaults))


aut.SI6 <- 
  mfdb_sample_count(mdb, c('length'), c(list(
    data_source = 'iceland-ldist',
    sampling_type = 'AUT',
    length = mfdb_interval("len", c(55,65),open_ended = 'lower')),
    defaults))



aut.SI7 <- 
  mfdb_sample_count(mdb, c('length'), c(list(
    data_source = 'iceland-ldist',
    sampling_type = 'AUT',
    length = mfdb_interval("len", c(65,130),open_ended = 'upper')),
    defaults))


