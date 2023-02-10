# Copyright 2016 SEARCH-The National Consortium for Justice Information and Statistics
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' @importFrom DBI dbClearResult dbSendQuery
truncateArresteeSegments <- function(conn) {
  dbClearResult(dbSendQuery(conn, "truncate ArresteeSegment"))
  dbClearResult(dbSendQuery(conn, "truncate ArresteeSegmentWasArmedWith"))
}

#' @importFrom DBI dbClearResult dbSendQuery
truncateArrestReportSegments <- function(conn) {
  dbClearResult(dbSendQuery(conn, "truncate ArrestReportSegment"))
  dbClearResult(dbSendQuery(conn, "truncate ArrestReportSegmentWasArmedWith"))
}

#' @import dplyr
#' @import tidyr
#' @import tibble
#' @importFrom stringr str_sub
#' @importFrom DBI dbWriteTable
writeRawArresteeSegmentTables <- function(conn, inputDfList, tableList) {

  dfName <- load(inputDfList[7])
  arresteeSegmentDf <- get(dfName) %>%  mutate_if(is.factor, as.character)
  rm(list=dfName)

  if ('ORI' %in% colnames(arresteeSegmentDf)) {
    arresteeSegmentDf <- arresteeSegmentDf %>% rename(V6003=ORI, V6004=INCNUM)
  }

  arresteeSegmentDf <- arresteeSegmentDf  %>%
    inner_join(tableList$Agency %>% select(AgencyORI), by=c('V6003'='AgencyORI')) %>%
    inner_join(tableList$AdministrativeSegment %>% select(ORI, IncidentNumber, AdministrativeSegmentID), by=c('V6003'='ORI', 'V6004'='IncidentNumber')) %>%
    mutate(ArresteeSegmentID=row_number(), SegmentActionTypeTypeID=99998L)

  ArresteeSegment <- arresteeSegmentDf %>%
    mutate(ArresteeSequenceNumber=as.integer(V6006),
           ArrestTransactionNumber=V6007,
           ArrestDate=as_date(ifelse(is.na(V6008), NA, ymd(V6008))),
           ArrestDateID=createKeyFromDate(ArrestDate),
           AgeOfArresteeMin=as.integer(V6014),
           AgeOfArresteeMax=AgeOfArresteeMin) %>%
    left_join(tableList$MultipleArresteeSegmentsIndicatorType %>% select(MultipleArresteeSegmentsIndicatorTypeID, StateCode),
              by=c('V6010'='StateCode')) %>%
    left_join(tableList$RaceOfPersonType %>% select(RaceOfPersonTypeID, StateCode), by=c('V6016'='StateCode')) %>%
    left_join(tableList$SexOfPersonType %>% select(SexOfPersonTypeID, StateCode), by=c('V6015'='StateCode')) %>%
    left_join(tableList$EthnicityOfPersonType %>% select(EthnicityOfPersonTypeID, StateCode), by=c('V6017'='StateCode')) %>%
    left_join(tableList$ResidentStatusOfPersonType %>% select(ResidentStatusOfPersonTypeID, StateCode), by=c('V6018'='StateCode')) %>%
    left_join(tableList$DispositionOfArresteeUnder18Type %>% select(DispositionOfArresteeUnder18TypeID, StateCode),
              by=c('V6019'='StateCode')) %>%
    left_join(tableList$UcrOffenseCodeType %>% select(UCROffenseCodeTypeID, StateCode), by=c('V6011'='StateCode')) %>%
    left_join(tableList$TypeOfArrestType %>% select(TypeOfArrestTypeID, StateCode), by=c('V6009'='StateCode')) %>%
    mutate(RaceOfPersonTypeID=ifelse(is.na(RaceOfPersonTypeID), 99998L, RaceOfPersonTypeID),
           UCROffenseCodeTypeID=ifelse(is.na(UCROffenseCodeTypeID), 99998L, UCROffenseCodeTypeID),
           EthnicityOfPersonTypeID=ifelse(is.na(EthnicityOfPersonTypeID), 99998L, EthnicityOfPersonTypeID),
           TypeOfArrestTypeID=ifelse(is.na(TypeOfArrestTypeID), 99998L, TypeOfArrestTypeID),
           MultipleArresteeSegmentsIndicatorTypeID=ifelse(is.na(MultipleArresteeSegmentsIndicatorTypeID), 99998L, MultipleArresteeSegmentsIndicatorTypeID),
           ResidentStatusOfPersonTypeID=ifelse(is.na(ResidentStatusOfPersonTypeID), 99998L, ResidentStatusOfPersonTypeID),
           DispositionOfArresteeUnder18TypeID=ifelse(is.na(DispositionOfArresteeUnder18TypeID), 99998L, DispositionOfArresteeUnder18TypeID),
           SexOfPersonTypeID=ifelse(is.na(SexOfPersonTypeID), 99998L, SexOfPersonTypeID)) %>%
    mutate(ArresteeSegmentID=row_number()) %>% as_tibble()

  ArresteeSegmentWasArmedWith <- ArresteeSegment %>%
    select(ArresteeSegmentID, V6012, V6013) %>%
    gather(key='index', value='StateCode', V6012, V6013) %>%
    select(-index) %>%
    filter(trimws(StateCode) != '') %>%
    filter(! is.na(StateCode)) %>%
    mutate(AutomaticWeaponIndicator=str_sub(StateCode, 3, 3), StateCode=str_sub(StateCode, 1, 2)) %>%
    left_join(tableList$ArresteeWasArmedWithType %>% select(ArresteeWasArmedWithTypeID, StateCode), by='StateCode') %>%
    mutate(ArresteeSegmentWasArmedWithID=row_number()) %>%
    select(ArresteeSegmentWasArmedWithID, ArresteeSegmentID, ArresteeWasArmedWithTypeID, AutomaticWeaponIndicator)

  ArresteeSegment <- ArresteeSegment %>%
    select(ArresteeSegmentID, SegmentActionTypeTypeID, AdministrativeSegmentID, ArresteeSequenceNumber, AgeOfArresteeMin, AgeOfArresteeMax,
           SexOfPersonTypeID, RaceOfPersonTypeID, EthnicityOfPersonTypeID, ResidentStatusOfPersonTypeID, TypeOfArrestTypeID,
           DispositionOfArresteeUnder18TypeID, UCROffenseCodeTypeID, ArrestDate, ArrestDateID, ArrestTransactionNumber,
           MultipleArresteeSegmentsIndicatorTypeID)

  rm(arresteeSegmentDf)

  writeLines(paste0("Writing ", nrow(ArresteeSegment), " arrestee segments to database"))
  writeDataFrameToDatabase(conn, ArresteeSegment, 'ArresteeSegment', viaBulk=TRUE, localBulk=FALSE, append=FALSE)

  attr(ArresteeSegment, 'type') <- 'FT'

  writeLines(paste0("Writing ", nrow(ArresteeSegmentWasArmedWith), " ArresteeSegmentWasArmedWith association rows to database"))
  writeDataFrameToDatabase(conn, ArresteeSegmentWasArmedWith, 'ArresteeSegmentWasArmedWith', viaBulk=TRUE, localBulk=FALSE, append=FALSE)

  attr(ArresteeSegmentWasArmedWith, 'type') <- 'AT'

  tableList$ArresteeSegment <- ArresteeSegment
  tableList$ArresteeSegmentWasArmedWith <- ArresteeSegmentWasArmedWith

  tableList

}

#' @import dplyr
#' @import tidyr
#' @import tibble
#' @importFrom stringr str_sub
#' @importFrom DBI dbWriteTable
writeRawArrestReportSegmentTables <- function(conn, inputDfList, state, tableList, records=-1) {

  currentMonth <- formatC(month(Sys.Date()), width=2, flag="0")
  currentYear <- year(Sys.Date()) %>% as.integer()

  dfName <- load(inputDfList[8])
  arresteeSegmentDf <- get(dfName) %>%  mutate_if(is.factor, as.character)
  rm(list=dfName)

  arresteeSegmentDf <- arresteeSegmentDf %>%
    inner_join(tableList$Agency %>% select(AgencyORI), by=c('V7003'='AgencyORI')) %>%
    mutate(ArrestReportSegmentID=row_number(), SegmentActionTypeTypeID=99998L)

  if (records != -1) {
    arresteeSegmentDf <- arresteeSegmentDf %>% head(records)
  }

  ArrestReportSegment <- arresteeSegmentDf %>%
    mutate(ArresteeSequenceNumber=as.integer(V7006),
           ArrestTransactionNumber=V7004,
           ORI=V7003,
           MonthOfTape=currentMonth, YearOfTape=currentYear,
           CityIndicator=NA_character_,
           ArrestDate=as_date(ifelse(is.na(V7005), NA, ymd(V7005))),
           ArrestDateID=createKeyFromDate(ArrestDate),
           AgeOfArresteeMin=as.integer(V7012),
           AgeOfArresteeMax=AgeOfArresteeMin) %>%
    left_join(tableList$Agency %>% select(AgencyID, ORI=AgencyORI), by='ORI') %>%
    left_join(tableList$RaceOfPersonType %>% select(RaceOfPersonTypeID, StateCode), by=c('V7014'='StateCode')) %>%
    left_join(tableList$SexOfPersonType %>% select(SexOfPersonTypeID, StateCode), by=c('V7013'='StateCode')) %>%
    left_join(tableList$EthnicityOfPersonType %>% select(EthnicityOfPersonTypeID, StateCode), by=c('V7015'='StateCode')) %>%
    left_join(tableList$ResidentStatusOfPersonType %>% select(ResidentStatusOfPersonTypeID, StateCode), by=c('V7016'='StateCode')) %>%
    left_join(tableList$DispositionOfArresteeUnder18Type %>% select(DispositionOfArresteeUnder18TypeID, StateCode),
              by=c('V7017'='StateCode')) %>%
    left_join(tableList$UcrOffenseCodeType %>% select(UCROffenseCodeTypeID, StateCode), by=c('V7009'='StateCode')) %>%
    left_join(tableList$TypeOfArrestType %>% select(TypeOfArrestTypeID, StateCode), by=c('V7008'='StateCode')) %>%
    mutate(RaceOfPersonTypeID=ifelse(is.na(RaceOfPersonTypeID), 99998L, RaceOfPersonTypeID),
           UCROffenseCodeTypeID=ifelse(is.na(UCROffenseCodeTypeID), 99998L, UCROffenseCodeTypeID),
           EthnicityOfPersonTypeID=ifelse(is.na(EthnicityOfPersonTypeID), 99998L, EthnicityOfPersonTypeID),
           TypeOfArrestTypeID=ifelse(is.na(TypeOfArrestTypeID), 99998L, TypeOfArrestTypeID),
           ResidentStatusOfPersonTypeID=ifelse(is.na(ResidentStatusOfPersonTypeID), 99998L, ResidentStatusOfPersonTypeID),
           DispositionOfArresteeUnder18TypeID=ifelse(is.na(DispositionOfArresteeUnder18TypeID), 99998L, DispositionOfArresteeUnder18TypeID),
           SexOfPersonTypeID=ifelse(is.na(SexOfPersonTypeID), 99998L, SexOfPersonTypeID)) %>%
    mutate(StateCode=state) %>%
    as_tibble()

  ArrestReportSegmentWasArmedWith <- ArrestReportSegment %>%
    select(ArrestReportSegmentID, V7010, V7011) %>%
    gather(key='index', value='StateCode', V7010, V7011) %>%
    select(-index) %>%
    filter(trimws(StateCode) != '') %>%
    filter(! is.na(StateCode)) %>%
    mutate(AutomaticWeaponIndicator=str_sub(StateCode, 3, 3), StateCode=str_sub(StateCode, 1, 2)) %>%
    left_join(tableList$ArresteeWasArmedWithType %>% select(ArresteeWasArmedWithTypeID, StateCode), by='StateCode') %>%
    mutate(ArrestReportSegmentWasArmedWithID=row_number()) %>%
    select(ArrestReportSegmentWasArmedWithID, ArrestReportSegmentID, ArresteeWasArmedWithTypeID, AutomaticWeaponIndicator)

  ArrestReportSegment <- ArrestReportSegment %>%
    select(ArrestReportSegmentID, SegmentActionTypeTypeID, ORI, CityIndicator, ArresteeSequenceNumber, AgeOfArresteeMin, AgeOfArresteeMax,
           SexOfPersonTypeID, RaceOfPersonTypeID, EthnicityOfPersonTypeID, ResidentStatusOfPersonTypeID, TypeOfArrestTypeID,
           DispositionOfArresteeUnder18TypeID, UCROffenseCodeTypeID, ArrestDate, ArrestDateID, ArrestTransactionNumber,
           MonthOfTape, YearOfTape, AgencyID)

  rm(arresteeSegmentDf)

  writeLines(paste0("Writing ", nrow(ArrestReportSegment), " arrest report segments to database"))
  writeDataFrameToDatabase(conn, ArrestReportSegment, 'ArrestReportSegment', viaBulk=TRUE, localBulk=FALSE, append=FALSE)
  attr(ArrestReportSegment, 'type') <- 'FT'

  writeLines(paste0("Writing ", nrow(ArrestReportSegmentWasArmedWith), " ArrestReportSegmentWasArmedWith association rows to database"))
  writeDataFrameToDatabase(conn, ArrestReportSegmentWasArmedWith, 'ArrestReportSegmentWasArmedWith', viaBulk=TRUE, localBulk=FALSE, append=FALSE)
  attr(ArrestReportSegmentWasArmedWith, 'type') <- 'AT'

  tableList$ArrestReportSegment <- ArrestReportSegment
  tableList$ArrestReportSegmentWasArmedWith <- ArrestReportSegmentWasArmedWith

  tableList

}
