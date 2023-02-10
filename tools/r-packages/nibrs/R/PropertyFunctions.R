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

#' @importFrom DBI dbSendQuery dbClearResult
truncateProperty <- function(conn) {
  dbClearResult(dbSendQuery(conn, "truncate PropertySegment"))
  dbClearResult(dbSendQuery(conn, "truncate PropertyType"))
  dbClearResult(dbSendQuery(conn, "truncate SuspectedDrugType"))
}

#' @import dplyr
#' @import tidyr
#' @import tibble
#' @importFrom DBI dbWriteTable
writeRawPropertySegmentTables <- function(conn, inputDfList, tableList) {

  dfName <- load(inputDfList[4])
  propertySegmentDf <- get(dfName) %>%  mutate_if(is.factor, as.character)
  rm(list=dfName)

  if ('ORI' %in% colnames(propertySegmentDf)) {
    propertySegmentDf <- propertySegmentDf %>% rename(V3003=ORI, V3004=INCNUM)
  }

  propertySegmentDf <- propertySegmentDf %>%
    inner_join(tableList$Agency %>% select(AgencyORI), by=c('V3003'='AgencyORI')) %>%
    inner_join(tableList$AdministrativeSegment %>% select(ORI, IncidentNumber, AdministrativeSegmentID), by=c('V3003'='ORI', 'V3004'='IncidentNumber')) %>%
    mutate(SegmentActionTypeTypeID=99998L)

  PropertyType <- propertySegmentDf %>%
    select(AdministrativeSegmentID, V3006:V3011, SegmentActionTypeTypeID, V3012:V3023) %>%
    mutate(
      V3006=gsub(x=V3006, pattern='\\(([0-9]+)\\).+', replacement='\\1'),
      V3007=gsub(x=V3007, pattern='\\(([0-9]+)\\).+', replacement='\\1'),
      ValueOfProperty=V3008,
      V3009=as.character(V3009),
      RecoveredDate=as_date(ifelse(is.na(V3009), NA, ymd(V3009))),
      RecoveredDateID=createKeyFromDate(RecoveredDate)
    ) %>%
    left_join(tableList$TypePropertyLossEtcType %>% select(TypePropertyLossEtcTypeID, StateCode), by=c('V3006'='StateCode')) %>%
    left_join(tableList$PropertyDescriptionType %>% select(PropertyDescriptionTypeID, StateCode), by=c('V3007'='StateCode')) %>%
    mutate(TypePropertyLossEtcTypeID=ifelse(is.na(TypePropertyLossEtcTypeID), 99998L, TypePropertyLossEtcTypeID),
           PropertyDescriptionTypeID=ifelse(is.na(PropertyDescriptionTypeID), 99998L, PropertyDescriptionTypeID),
           RecoveredDateID=ifelse(is.na(RecoveredDateID), BLANK_DATE_VALUE, RecoveredDateID)) %>%
    select(AdministrativeSegmentID, TypePropertyLossEtcTypeID, PropertyDescriptionTypeID, ValueOfProperty, RecoveredDate, RecoveredDateID,
           V3010, V3011, SegmentActionTypeTypeID, V3012:V3023) %>%
    filter(TypePropertyLossEtcTypeID != 99999L) %>%
    mutate(PropertyTypeID=row_number()) %>% as_tibble()

  PropertySegment <- PropertyType %>%
    select(AdministrativeSegmentID, TypePropertyLossEtcTypeID, SegmentActionTypeTypeID, NumberOfStolenMotorVehicles=V3010,
           NumberOfRecoveredMotorVehicles=V3011, V3012:V3023) %>%
    group_by(AdministrativeSegmentID, TypePropertyLossEtcTypeID) %>%
    filter(row_number()==1) %>%
    ungroup() %>%
    mutate(PropertySegmentID=row_number())

  SuspectedDrugType <- PropertySegment %>%
    gather(key=index, value=StateCode, V3012, V3016, V3020) %>%
    select(PropertySegmentID, StateCode) %>%
    bind_cols(
      PropertySegment %>%
        gather(key=index, value=TypeDrugMeasurementCode, V3015, V3019, V3023) %>%
        select(TypeDrugMeasurementCode),
      PropertySegment %>%
        gather(key=index, value=qqq, V3013, V3017, V3021) %>%
        select(qqq),
      PropertySegment %>%
        gather(key=index, value=qqqq, V3014, V3018, V3022) %>%
        select(qqqq)
    ) %>%
    filter(trimws(StateCode) != '') %>%
    filter(! is.na(StateCode)) %>%
    mutate(EstimatedDrugQuantity=qqq + (qqqq*.001)) %>%
    left_join(tableList$TypeDrugMeasurementType %>% select(TypeDrugMeasurementTypeID, TypeDrugMeasurementCode=StateCode), by='TypeDrugMeasurementCode') %>%
    left_join(tableList$SuspectedDrugTypeType %>% select(SuspectedDrugTypeTypeID, StateCode), by='StateCode') %>%
    mutate(TypeDrugMeasurementTypeID=ifelse(is.na(TypeDrugMeasurementTypeID), 99998L, TypeDrugMeasurementTypeID),
           SuspectedDrugTypeTypeID=ifelse(is.na(SuspectedDrugTypeTypeID), 99998L, SuspectedDrugTypeTypeID)) %>%
    select(PropertySegmentID, SuspectedDrugTypeTypeID, TypeDrugMeasurementTypeID, EstimatedDrugQuantity) %>%
    ungroup() %>%
    mutate(SuspectedDrugTypeID=row_number())

  PropertySegment <- PropertySegment %>% select(-(V3012:V3023))

  PropertyType <- PropertyType %>%
    inner_join(PropertySegment %>% select(PropertySegmentID, AdministrativeSegmentID, TypePropertyLossEtcTypeID),
               by=c('AdministrativeSegmentID', 'TypePropertyLossEtcTypeID')) %>%
    select(PropertyTypeID, PropertySegmentID, PropertyDescriptionTypeID, ValueOfProperty, RecoveredDate, RecoveredDateID)

  rm(propertySegmentDf)

  writeLines(paste0("Writing ", nrow(PropertySegment), " property segments to database"))
  writeDataFrameToDatabase(conn, PropertySegment, 'PropertySegment', viaBulk=TRUE, localBulk=FALSE, append=FALSE)

  attr(PropertySegment, 'type') <- 'FT'

  writeLines(paste0("Writing ", nrow(SuspectedDrugType), " SuspectedDrugType association rows to database"))
  writeDataFrameToDatabase(conn, SuspectedDrugType, 'SuspectedDrugType', viaBulk=TRUE, localBulk=FALSE, append=FALSE)

  attr(SuspectedDrugType, 'type') <- 'AT'

  writeLines(paste0("Writing ", nrow(PropertyType), " PropertyType association rows to database"))
  writeDataFrameToDatabase(conn, PropertyType, 'PropertyType', viaBulk=TRUE, localBulk=FALSE, append=FALSE)

  attr(PropertyType, 'type') <- 'AT'

  tableList$PropertySegment <- PropertySegment
  tableList$PropertyType <- PropertyType
  tableList$SuspectedDrugType <- SuspectedDrugType

  tableList

}
