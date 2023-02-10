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
truncateOffender <- function(conn) {
  dbClearResult(dbSendQuery(conn, 'truncate OffenderSegment'))
}

#' @import dplyr
#' @import tidyr
#' @import tibble
#' @importFrom DBI dbWriteTable
writeRawOffenderSegmentTables <- function(conn, inputDfList, tableList) {

  dfName <- load(inputDfList[6])
  offenderSegmentDf <- get(dfName) %>%  mutate_if(is.factor, as.character)
  rm(list=dfName)

  if ('ORI' %in% colnames(offenderSegmentDf)) {
    offenderSegmentDf <- offenderSegmentDf %>% rename(V5003=ORI, V5004=INCNUM)
  }

  offenderSegmentDf <- offenderSegmentDf %>%
    inner_join(tableList$Agency %>% select(AgencyORI), by=c('V5003'='AgencyORI')) %>%
    inner_join(tableList$AdministrativeSegment %>% select(ORI, IncidentNumber, AdministrativeSegmentID), by=c('V5003'='ORI', 'V5004'='IncidentNumber')) %>%
    mutate(OffenderSegmentID=row_number(), SegmentActionTypeTypeID=99998L)

  OffenderSegment <- offenderSegmentDf %>%
    mutate(OffenderSequenceNumber=as.integer(V5006),
           AgeOfOffenderMin=as.integer(V5007),
           AgeOfOffenderMax=AgeOfOffenderMin) %>%
    left_join(tableList$RaceOfPersonType %>% select(RaceOfPersonTypeID, StateCode), by=c('V5009'='StateCode')) %>%
    left_join(tableList$SexOfPersonType %>% select(SexOfPersonTypeID, StateCode), by=c('V5008'='StateCode')) %>%
    left_join(tableList$EthnicityOfPersonType %>% select(EthnicityOfPersonTypeID, StateCode), by=c('V5011'='StateCode')) %>%
    mutate(RaceOfPersonTypeID=ifelse(is.na(RaceOfPersonTypeID), 99998L, RaceOfPersonTypeID),
           EthnicityOfPersonTypeID=ifelse(is.na(EthnicityOfPersonTypeID), 99998L, EthnicityOfPersonTypeID),
           SexOfPersonTypeID=ifelse(is.na(SexOfPersonTypeID), 99998L, SexOfPersonTypeID)) %>%
    select(OffenderSegmentID, SegmentActionTypeTypeID, AdministrativeSegmentID, OffenderSequenceNumber, AgeOfOffenderMin, AgeOfOffenderMax,
           SexOfPersonTypeID, RaceOfPersonTypeID, EthnicityOfPersonTypeID) %>% as_tibble()

  rm(offenderSegmentDf)

  writeLines(paste0('Writing ', nrow(OffenderSegment), ' offender segments to database'))
  writeDataFrameToDatabase(conn, OffenderSegment, 'OffenderSegment', viaBulk=TRUE, localBulk=FALSE, append=FALSE)

  attr(OffenderSegment, 'type') <- 'FT'

  tableList$OffenderSegment <- OffenderSegment

  tableList

}
