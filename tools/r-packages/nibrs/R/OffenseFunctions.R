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
# functions related to Offense data manipulation

#' @importFrom DBI dbSendQuery dbClearResult
truncateOffenses <- function(conn) {
  dbClearResult(dbSendQuery(conn, "truncate OffenseSegment"))
  dbClearResult(dbSendQuery(conn, "truncate offenderSuspectedOfUsing"))
  dbClearResult(dbSendQuery(conn, "truncate typeCriminalActivity"))
  dbClearResult(dbSendQuery(conn, "truncate TypeOfWeaponForceInvolved"))
  dbClearResult(dbSendQuery(conn, "truncate biasMotivation"))
}

#' @import dplyr
#' @import tidyr
#' @import tibble
#' @importFrom stringr str_sub
#' @importFrom DBI dbWriteTable
writeRawOffenseSegmentTables <- function(conn, inputDfList, tableList) {

  dfName <- load(inputDfList[3])
  offenseSegmentDf <- get(dfName) %>%  mutate_if(is.factor, as.character) %>%
    inner_join(tableList$Agency %>% select(AgencyORI), by=c('V2003'='AgencyORI'))
  rm(list=dfName)

  offenseSegmentDf <- offenseSegmentDf %>%
    inner_join(tableList$AdministrativeSegment %>% select(ORI, IncidentNumber, AdministrativeSegmentID), by=c('V2003'='ORI', 'V2004'='IncidentNumber')) %>%
    mutate(OffenseSegmentID=row_number(), SegmentActionTypeTypeID=99998L) %>% as_tibble()


  OffenseSegment <- offenseSegmentDf %>%
    mutate(V2011=gsub(x=V2011, pattern='\\(([0-9]+)\\).+', replacement='\\1'),
           UCROffenseCode=V2006) %>%
    left_join(tableList$UcrOffenseCodeType %>% select(UCROffenseCodeTypeID, StateCode), by=c('V2006'='StateCode'))

  OffenseSegment <- OffenseSegment   %>%
    left_join(tableList$locationTypeType %>% select(locationTypeTypeID, StateCode), by=c('V2011'='StateCode')) %>%
    left_join(tableList$MethodOfEntryType %>% select(MethodOfEntryTypeID, StateCode), by=c('V2013'='StateCode')) %>%
    select(OffenseSegmentID, SegmentActionTypeTypeID, AdministrativeSegmentID, UCROffenseCodeTypeID, OffenseAttemptedCompleted=V2007,
           locationTypeTypeID, NumberOfPremisesEntered=V2012, MethodOfEntryTypeID, UCROffenseCode)

  OffenderSuspectedOfUsing <- offenseSegmentDf %>%
    select(AdministrativeSegmentID, OffenseSegmentID, V2008:V2010) %>%
    gather(key=index, value=StateCode, V2008:V2010) %>% filter(StateCode != ' ') %>%
    select(-index) %>%
    inner_join(tableList$OffenderSuspectedOfUsingType %>% select(OffenderSuspectedOfUsingTypeID, StateCode), by='StateCode') %>%
    mutate(OffenderSuspectedOfUsingID=row_number()) %>%
    select(OffenderSuspectedOfUsingID, OffenseSegmentID, OffenderSuspectedOfUsingTypeID)

  TypeCriminalActivity <- offenseSegmentDf %>%
    select(AdministrativeSegmentID, OffenseSegmentID, V2014:V2016) %>%
    gather(key=index, value=StateCode, V2014:V2016) %>% filter(StateCode != ' ') %>%
    select(-index) %>%
    inner_join(tableList$TypeOfCriminalActivityType %>% select(TypeOfCriminalActivityTypeID, StateCode), by='StateCode') %>%
    mutate(TypeCriminalActivityID=row_number()) %>%
    select(TypeCriminalActivityID, OffenseSegmentID, TypeOfCriminalActivityTypeID)

  BiasMotivation <- offenseSegmentDf %>%
    mutate_at(vars(starts_with('V2020')), gsub, pattern='\\(([0-9]+)\\).+', replacement='\\1') %>%
    select(AdministrativeSegmentID, OffenseSegmentID, starts_with('V2020')) %>%
    gather(key=index, value=StateCode, starts_with('V2020')) %>% filter(StateCode != ' ') %>%
    select(-index) %>%
    inner_join(tableList$BiasMotivationType %>% select(BiasMotivationTypeID, StateCode), by='StateCode') %>%
    mutate(BiasMotivationID=row_number()) %>%
    select(BiasMotivationID, OffenseSegmentID, BiasMotivationTypeID)

  TypeOfWeaponForceInvolved <- offenseSegmentDf %>%
    select(AdministrativeSegmentID, OffenseSegmentID, V2017:V2019) %>%
    gather(key=index, value=StateCode, V2017:V2019) %>% filter(StateCode != ' ') %>%
    select(-index) %>%
    mutate(AutomaticWeaponIndicator=ifelse(str_length(StateCode)==3, str_sub(StateCode, 3, 3), NA_character_)) %>%
    mutate(StateCode=str_sub(StateCode, 1, 2)) %>%
    inner_join(tableList$TypeOfWeaponForceInvolvedType %>% select(TypeOfWeaponForceInvolvedTypeID, StateCode), by='StateCode') %>%
    mutate(TypeOfWeaponForceInvolvedID=row_number()) %>%
    select(TypeOfWeaponForceInvolvedID, OffenseSegmentID, TypeOfWeaponForceInvolvedTypeID, AutomaticWeaponIndicator)

  rm(offenseSegmentDf)

  writeLines(paste0("Writing ", nrow(OffenseSegment), " offense segments to database"))
  writeDataFrameToDatabase(conn, select(OffenseSegment, -UCROffenseCode), 'OffenseSegment', viaBulk=TRUE, localBulk=FALSE, append=FALSE)

  attr(OffenseSegment, 'type') <- 'FT'

  writeLines(paste0("Writing ", nrow(OffenderSuspectedOfUsing), " OffenderSuspectedOfUsing records to database"))
  writeDataFrameToDatabase(conn, OffenderSuspectedOfUsing, 'offenderSuspectedOfUsing', viaBulk=TRUE, localBulk=FALSE, append=FALSE)

  attr(OffenderSuspectedOfUsing, 'type') <- 'AT'

  writeLines(paste0("Writing ", nrow(TypeCriminalActivity), " TypeCriminalActivity records to database"))
  writeDataFrameToDatabase(conn, TypeCriminalActivity, 'typeCriminalActivity', viaBulk=TRUE, localBulk=FALSE, append=FALSE)

  attr(TypeCriminalActivity, 'type') <- 'AT'

  writeLines(paste0("Writing ", nrow(BiasMotivation), " BiasMotivation records to database"))
  writeDataFrameToDatabase(conn, BiasMotivation, 'biasMotivation', viaBulk=TRUE, localBulk=FALSE, append=FALSE)

  attr(BiasMotivation, 'type') <- 'AT'

  writeLines(paste0("Writing ", nrow(TypeOfWeaponForceInvolved), " TypeOfWeaponForceInvolved records to database"))
  writeDataFrameToDatabase(conn, TypeOfWeaponForceInvolved, 'TypeOfWeaponForceInvolved', viaBulk=TRUE, localBulk=FALSE, append=FALSE)

  attr(TypeOfWeaponForceInvolved, 'type') <- 'AT'

  tableList$OffenseSegment <- OffenseSegment
  tableList$OffenderSuspectedOfUsing <- OffenderSuspectedOfUsing
  tableList$TypeCriminalActivity <- TypeCriminalActivity
  tableList$BiasMotivation <- BiasMotivation
  tableList$TypeOfWeaponForceInvolved <- TypeOfWeaponForceInvolved

  tableList

}
