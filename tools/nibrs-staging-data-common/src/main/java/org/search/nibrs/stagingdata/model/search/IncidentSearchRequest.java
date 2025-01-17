/*
 * Copyright 2016 SEARCH-The National Consortium for Justice Information and Statistics
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.search.nibrs.stagingdata.model.search;

import java.io.Serializable;
import java.time.LocalDate;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.hibernate.validator.constraints.Range;
import org.springframework.format.annotation.DateTimeFormat;

import com.fasterxml.jackson.annotation.JsonIgnore;

public class IncidentSearchRequest implements Serializable {
	private static final long serialVersionUID = 7916910066665545067L;
	private String incidentIdentifier; 
	private Integer ownerId; 
	private String stateCode; 
	private List<Integer> agencyIds; 
	private List<String> agenycyNames; 
	@DateTimeFormat(pattern = "MM/dd/yyyy")
	private LocalDate incidentDateRangeStartDate; 
	@DateTimeFormat(pattern = "MM/dd/yyyy")
	private LocalDate incidentDateRangeEndDate; 
	@DateTimeFormat(pattern = "MM/dd/yyyy")
	private LocalDate reportTimestampStartDate; 
	@DateTimeFormat(pattern = "MM/dd/yyyy")
	private LocalDate reportTimestampEndDate; 
	private Integer ucrOffenseCodeTypeId; 
	private String offenseCode; 

	@Range(min=1, max=12)
	private Integer submissionMonth;
	private Integer submissionYear; 
	@Range(min=1, max=12)
	private Integer submissionStartMonth;
	private Integer submissionStartYear; 
	@Range(min=1, max=12)
	private Integer submissionEndMonth;
	private Integer submissionEndYear; 
	private FbiSubmissionStatus fbiSubmissionStatus;
	
	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
	public String getIncidentIdentifier() {
		return incidentIdentifier;
	}
	public void setIncidentIdentifier(String incidentIdentifier) {
		this.incidentIdentifier = StringUtils.trimToNull(incidentIdentifier);
	}
	public Integer getUcrOffenseCodeTypeId() {
		return ucrOffenseCodeTypeId;
	}
	public void setUcrOffenseCodeTypeId(Integer ucrOffenseCodeTypeId) {
		this.ucrOffenseCodeTypeId = ucrOffenseCodeTypeId;
	}
	public Integer getSubmissionMonth() {
		return submissionMonth;
	}
	public void setSubmissionMonth(Integer submissionMonth) {
		this.submissionMonth = submissionMonth;
	}
	public Integer getSubmissionYear() {
		return submissionYear;
	}
	public void setSubmissionYear(Integer submissionYear) {
		this.submissionYear = submissionYear;
	}
	public String getOffenseCode() {
		return offenseCode;
	}
	public void setOffenseCode(String offenseCode) {
		this.offenseCode = offenseCode;
	} 

	@JsonIgnore
	public boolean isEmpty() {
		return StringUtils.isBlank(incidentIdentifier)
				&& (StringUtils.isBlank(stateCode))  
				&& (agencyIds == null || agencyIds.isEmpty())  
				&& incidentDateRangeStartDate == null
				&& incidentDateRangeEndDate == null
				&& reportTimestampStartDate == null
				&& reportTimestampEndDate == null
				&& (ucrOffenseCodeTypeId == null || ucrOffenseCodeTypeId == 0) 
				&& submissionMonth == null 
				&& submissionYear == null 
				&& submissionStartMonth == null 
				&& submissionStartYear == null 
				&& submissionEndMonth == null 
				&& submissionEndYear == null 
				;
	}
	public List<Integer> getAgencyIds() {
		return agencyIds;
	}
	public void setAgencyIds(List<Integer> agencyIds) {
		this.agencyIds = agencyIds;
	}
	public List<String> getAgenycyNames() {
		return agenycyNames;
	}
	public void setAgenycyNames(List<String> agenycyNames) {
		this.agenycyNames = agenycyNames;
	}
	public LocalDate getIncidentDateRangeStartDate() {
		return incidentDateRangeStartDate;
	}
	public void setIncidentDateRangeStartDate(LocalDate incidentDateRangeStartDate) {
		this.incidentDateRangeStartDate = incidentDateRangeStartDate;
	}
	public LocalDate getIncidentDateRangeEndDate() {
		return incidentDateRangeEndDate;
	}
	public void setIncidentDateRangeEndDate(LocalDate incidentDateRangeEndDate) {
		this.incidentDateRangeEndDate = incidentDateRangeEndDate;
	}
	public Integer getSubmissionStartMonth() {
		return submissionStartMonth;
	}
	public void setSubmissionStartMonth(Integer submissionStartMonth) {
		this.submissionStartMonth = submissionStartMonth;
	}
	public Integer getSubmissionStartYear() {
		return submissionStartYear;
	}
	public void setSubmissionStartYear(Integer submissionStartYear) {
		this.submissionStartYear = submissionStartYear;
	}
	public Integer getSubmissionEndMonth() {
		return submissionEndMonth;
	}
	public void setSubmissionEndMonth(Integer submissionEndMonth) {
		this.submissionEndMonth = submissionEndMonth;
	}
	public Integer getSubmissionEndYear() {
		return submissionEndYear;
	}
	public void setSubmissionEndYear(Integer submissionEndYear) {
		this.submissionEndYear = submissionEndYear;
	}
	
	public java.sql.Date getSubmissionStartDate() {
		java.sql.Date date = null;
		if (submissionStartYear != null && submissionStartYear > 0) {
			LocalDate localDate = LocalDate.of(submissionStartYear, 1, 1);
			
			if (submissionStartMonth != null) {
					localDate = LocalDate.of(submissionStartYear, submissionStartMonth, 1);
			}
			date = java.sql.Date.valueOf(localDate);
		}
		
		return date;
	}
	
	public java.sql.Date getSubmissionEndDate() {
		
		java.sql.Date date = null;
		if (submissionEndYear != null && submissionEndYear > 0) {
			LocalDate localDate = LocalDate.of(submissionEndYear, 12, 31);
			
			if ( submissionEndMonth != null) {
					if (submissionEndMonth < 12) {
						localDate = LocalDate.of(submissionEndYear, submissionEndMonth+1, 1).minusDays(1);
					}
					else {
						localDate = LocalDate.of(submissionEndYear, 12, 31);
					}
			}
			
			date = java.sql.Date.valueOf(localDate);
		}
		
		return date;
	}
	public FbiSubmissionStatus getFbiSubmissionStatus() {
		return fbiSubmissionStatus;
	}
	public void setFbiSubmissionStatus(FbiSubmissionStatus fbiSubmissionStatus) {
		this.fbiSubmissionStatus = fbiSubmissionStatus;
	}
	public Integer getOwnerId() {
		return ownerId;
	}
	public void setOwnerId(Integer ownerId) {
		this.ownerId = ownerId;
	}
	public String getStateCode() {
		return stateCode;
	}
	public void setStateCode(String stateCode) {
		this.stateCode = stateCode;
	}
	public LocalDate getReportTimestampStartDate() {
		return reportTimestampStartDate;
	}
	public void setReportTimestampStartDate(LocalDate reportTimestampStartDate) {
		this.reportTimestampStartDate = reportTimestampStartDate;
	}
	public LocalDate getReportTimestampEndDate() {
		return reportTimestampEndDate;
	}
	public void setReportTimestampEndDate(LocalDate reportTimestampEndDate) {
		this.reportTimestampEndDate = reportTimestampEndDate;
	}
	
}
