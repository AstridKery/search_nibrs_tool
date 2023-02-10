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
truncateVictim <- function(conn) {
  dbClearResult(dbSendQuery(conn, "truncate VictimSegment"))
  dbClearResult(dbSendQuery(conn, "truncate VictimOffenderAssociation"))
  dbClearResult(dbSendQuery(conn, "truncate victimOffenseAssociation"))
  dbClearResult(dbSendQuery(conn, "truncate aggravatedAssaultHomicideCircumstances"))
  dbClearResult(dbSendQuery(conn, "truncate typeInjury"))
}

#' @import dplyr
#' @import tidyr
#' @import tibble
#' @importFrom DBI dbWriteTable
writeRawVictimSegmentTables <- function(conn, inputDfList, tableList) {

  dfName <- load(inputDfList[5])
  victimSegmentDf <- get(dfName) %>%  mutate_if(is.factor, as.character)
  rm(list=dfName)

  if ('ORI' %in% colnames(victimSegmentDf)) {
    victimSegmentDf <- victimSegmentDf %>% rename(V4003=ORI, V4004=INCNUM)
  }


  victimSegmentDf <- victimSegmentDf %>%
    inner_join(tableList$Agency %>% select(AgencyORI), by=c('V4003'='AgencyORI')) %>%
    inner_join(tableList$AdministrativeSegment %>% select(ORI, IncidentNumber, AdministrativeSegmentID), by=c('V4003'='ORI', 'V4004'='IncidentNumber')) %>%
    mutate(VictimSegmentID=row_number(), SegmentActionTypeTypeID=99998L)

  VictimSegment <- victimSegmentDf %>%
    mutate(VictimSequenceNumber=as.integer(V4006),
           V4017A=gsub(x=V4017A, pattern='\\(([0-9]+)\\).+', replacement='\\1'),
           V4023=gsub(x=V4023, pattern='\\(([0-9]+)\\).+', replacement='\\1'),
           V4024=gsub(x=V4024, pattern='\\(([0-9]+)\\).+', replacement='\\1'),
           V4017A=trimws(V4017A),
           V4018=trimws(V4018),
           AgeOfVictimMin=case_when(
             V4018 %in% c('NB','NN','BB') ~ '0',
             V4018=='' | is.na(V4018) ~ NA_character_,
             TRUE ~ V4018
           ),
           AgeOfVictimMin=as.integer(AgeOfVictimMin),
           AgeOfVictimMax=AgeOfVictimMin,
           AgeNeonateIndicator=ifelse(is.na(V4018) | V4018==' ', NA_integer_, V4018=='NN'),
           AgeFirstWeekIndicator=ifelse(is.na(V4018) | V4018==' ', NA_integer_, V4018=='NB'),
           AgeFirstYearIndicator=ifelse(is.na(V4018) | V4018==' ', NA_integer_, V4018=='BB')
    ) %>%
    left_join(tableList$TypeOfVictimType %>% select(TypeOfVictimTypeID, StateCode), by=c('V4017'='StateCode')) %>%
    left_join(tableList$OfficerActivityCircumstanceType %>% select(OfficerActivityCircumstanceTypeID, StateCode),
              by=c('V4017A'='StateCode')) %>%
    left_join(tableList$OfficerAssignmentTypeType %>% select(OfficerAssignmentTypeTypeID, StateCode), by=c('V4017B'='StateCode')) %>%
    left_join(tableList$SexOfPersonType %>% select(SexOfPersonTypeID, StateCode), by=c('V4019'='StateCode')) %>%
    left_join(tableList$RaceOfPersonType %>% select(RaceOfPersonTypeID, StateCode), by=c('V4020'='StateCode')) %>%
    left_join(tableList$EthnicityOfPersonType %>% select(EthnicityOfPersonTypeID, StateCode), by=c('V4021'='StateCode')) %>%
    left_join(tableList$ResidentStatusOfPersonType %>% select(ResidentStatusOfPersonTypeID, StateCode),
              by=c('V4022'='StateCode')) %>%
    left_join(tableList$AdditionalJustifiableHomicideCircumstancesType %>%
                select(AdditionalJustifiableHomicideCircumstancesTypeID, StateCode),
              by=c('V4025'='StateCode')) %>%
    mutate(VictimSegmentID=row_number(), OfficerActivityCircumstanceTypeID=case_when(is.na(OfficerActivityCircumstanceTypeID) ~ 99998L, TRUE ~ OfficerActivityCircumstanceTypeID)) %>% as_tibble()


  VictimOffenseAssociation <- VictimSegment %>%
    select(AdministrativeSegmentID, VictimSegmentID, V4007:V4016) %>%
    gather(key='index', value='UCROffenseCode', V4007:V4016) %>%
    select(-index) %>%
    filter(trimws(UCROffenseCode) != '') %>%
    filter(! is.na(UCROffenseCode)) %>%
    inner_join(tableList$OffenseSegment %>% select(AdministrativeSegmentID, OffenseSegmentID, UCROffenseCode), by=c('AdministrativeSegmentID', 'UCROffenseCode')) %>%
    mutate(VictimOffenseAssociationID=row_number()) %>%
    select(VictimOffenseAssociationID, VictimSegmentID, OffenseSegmentID)

  AggravatedAssaultHomicideCircumstances <- VictimSegment %>%
    select(VictimSegmentID, V4023, V4024) %>%
    gather(key='index', value='StateCode', V4023, V4024) %>%
    select(-index) %>%
    filter(trimws(StateCode) != '') %>%
    filter(! is.na(StateCode)) %>%
    inner_join(tableList$AggravatedAssaultHomicideCircumstancesType %>%
                 select(AggravatedAssaultHomicideCircumstancesTypeID, StateCode),
               by='StateCode') %>%
    select(VictimSegmentID, AggravatedAssaultHomicideCircumstancesTypeID) %>%
    mutate(AggravatedAssaultHomicideCircumstancesID=row_number())

  TypeInjury <- VictimSegment %>%
    select(VictimSegmentID, V4026:V4030) %>%
    gather(key='index', value='StateCode', V4026:V4030) %>%
    select(-index) %>%
    filter(! is.na(StateCode)) %>%
    filter(trimws(StateCode) != '') %>%
    inner_join(tableList$TypeInjuryType %>% select(TypeInjuryTypeID, StateCode), by='StateCode') %>%
    select(VictimSegmentID, TypeInjuryTypeID) %>%
    mutate(TypeInjuryID=row_number())

  VictimOffenderAssociation <- VictimSegment %>%
    select(AdministrativeSegmentID, VictimSegmentID, V4031:V4050) %>%
    gather(key='var', value='value', V4031:V4050) %>%
    filter(! is.na(value)) %>%
    filter(trimws(value) != '') %>%
    mutate(varnum=as.integer(str_sub(var, 2, 5))) %>%
    mutate(
      var=case_when(
        varnum %% 2 == 1 ~ 'OffenderSequenceNumber',
        TRUE ~ 'VictimOffenderRelationshipCode'
      ),
      seqnum=case_when(
        var=='VictimOffenderRelationshipCode' ~ varnum-1L,
        TRUE ~ varnum
      ),
      seqnum=seqnum-4030-(seqnum-4030-1)/2) %>%
    select(-varnum)

  VictimOffenderAssociation <- VictimOffenderAssociation %>% spread(key=var, value=value) %>%
    mutate(OffenderSequenceNumber=as.integer(OffenderSequenceNumber)) %>%
    inner_join(tableList$OffenderSegment %>% select(AdministrativeSegmentID, OffenderSegmentID, OffenderSequenceNumber),
               by=c('AdministrativeSegmentID', 'OffenderSequenceNumber')) %>%
    left_join(tableList$VictimOffenderRelationshipType %>% select(VictimOffenderRelationshipTypeID, StateCode),
              by=c('VictimOffenderRelationshipCode'='StateCode')) %>%
    select(VictimSegmentID, OffenderSegmentID, VictimOffenderRelationshipTypeID) %>%
    mutate(VictimOffenderAssociationID=row_number()) %>%
    mutate(VictimOffenderRelationshipTypeID=ifelse(is.na(VictimOffenderRelationshipTypeID), 99998L, VictimOffenderRelationshipTypeID))

  VictimSegment <- VictimSegment %>%
    select(VictimSegmentID, AdministrativeSegmentID, SegmentActionTypeTypeID, VictimSequenceNumber, TypeOfVictimTypeID,
           OfficerActivityCircumstanceTypeID, OfficerAssignmentTypeTypeID, AgeOfVictimMin, AgeOfVictimMax, AgeNeonateIndicator, AgeFirstWeekIndicator,
           AgeFirstYearIndicator, SexOfPersonTypeID, RaceOfPersonTypeID, EthnicityOfPersonTypeID, ResidentStatusOfPersonTypeID,
           AdditionalJustifiableHomicideCircumstancesTypeID)

  rm(victimSegmentDf)

  writeLines(paste0("Writing ", nrow(VictimSegment), " VictimSegment association rows to database"))
  writeDataFrameToDatabase(conn, VictimSegment, 'VictimSegment', viaBulk=TRUE, localBulk=FALSE, append=FALSE)

  attr(VictimSegment, 'type') <- 'FT'

  writeLines(paste0("Writing ", nrow(VictimOffenseAssociation), " VictimOffenseAssociation association rows to database"))
  writeDataFrameToDatabase(conn, VictimOffenseAssociation, 'victimOffenseAssociation', viaBulk=TRUE, localBulk=FALSE, append=FALSE)

  attr(VictimOffenseAssociation, 'type') <- 'AT'

  writeLines(paste0("Writing ", nrow(VictimOffenderAssociation), " VictimOffenderAssociation association rows to database"))
  writeDataFrameToDatabase(conn, VictimOffenderAssociation, 'VictimOffenderAssociation', viaBulk=TRUE, localBulk=FALSE, append=FALSE)

  attr(VictimOffenderAssociation, 'type') <- 'AT'

  writeLines(paste0("Writing ", nrow(AggravatedAssaultHomicideCircumstances), " AggravatedAssaultHomicideCircumstances association rows to database"))
  writeDataFrameToDatabase(conn, AggravatedAssaultHomicideCircumstances, 'aggravatedAssaultHomicideCircumstances', viaBulk=TRUE, localBulk=FALSE, append=FALSE)

  attr(AggravatedAssaultHomicideCircumstances, 'type') <- 'AT'

  writeLines(paste0("Writing ", nrow(TypeInjury), " TypeInjury association rows to database"))
  writeDataFrameToDatabase(conn, TypeInjury, 'typeInjury', viaBulk=TRUE, localBulk=FALSE, append=FALSE)

  attr(TypeInjury, 'type') <- 'AT'

  tableList$VictimSegment <- VictimSegment
  tableList$VictimOffenseAssociation <- VictimOffenseAssociation
  tableList$VictimOffenderAssociation <- VictimOffenderAssociation
  tableList$AggravatedAssaultHomicideCircumstances <- AggravatedAssaultHomicideCircumstances
  tableList$TypeInjury <- TypeInjury

  tableList

}
