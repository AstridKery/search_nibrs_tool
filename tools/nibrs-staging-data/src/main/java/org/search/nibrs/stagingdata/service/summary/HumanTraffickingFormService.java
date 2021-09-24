
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

package org.search.nibrs.stagingdata.service.summary;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.search.nibrs.model.codes.OffenseCode;
import org.search.nibrs.model.reports.SummaryReportRequest;
import org.search.nibrs.model.reports.humantrafficking.HumanTraffickingForm;
import org.search.nibrs.model.reports.humantrafficking.HumanTraffickingRowName;
import org.search.nibrs.stagingdata.AppProperties;
import org.search.nibrs.stagingdata.model.Agency;
import org.search.nibrs.stagingdata.model.segment.AdministrativeSegment;
import org.search.nibrs.stagingdata.model.segment.OffenseSegment;
import org.search.nibrs.stagingdata.repository.AgencyRepository;
import org.search.nibrs.stagingdata.service.AdministrativeSegmentService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class HumanTraffickingFormService {

	private final Log log = LogFactory.getLog(this.getClass());
	@Autowired
	AdministrativeSegmentService administrativeSegmentService;
	@Autowired
	public AgencyRepository agencyRepository; 
	@Autowired
	public AppProperties appProperties; 
	
	public HumanTraffickingFormService() {
		super();
	}
	
	public HumanTraffickingForm createHumanTraffickingReportByRequest(SummaryReportRequest summaryReportRequest) {
		HumanTraffickingForm humanTraffickingForm = new HumanTraffickingForm(summaryReportRequest.getIncidentYear(), summaryReportRequest.getIncidentMonth()); 
		
		if (summaryReportRequest.getAgencyId() != null){
			Optional<Agency> agency = agencyRepository.findById(summaryReportRequest.getAgencyId()); 
			if (agency.isPresent()){
				humanTraffickingForm.setAgencyName(agency.get().getAgencyName());
				humanTraffickingForm.setOri(agency.get().getAgencyOri());
				humanTraffickingForm.setStateName(agency.get().getStateName());
				humanTraffickingForm.setStateCode(agency.get().getStateCode());
				humanTraffickingForm.setPopulation(agency.get().getPopulation());
			}
			else{
				return humanTraffickingForm; 
			}
		}
		else{
			Agency agency = agencyRepository.findFirstByStateCode(summaryReportRequest.getStateCode());
			humanTraffickingForm.setAgencyName("");
			humanTraffickingForm.setStateName(agency.getStateName());
			humanTraffickingForm.setStateCode(agency.getStateCode());
			humanTraffickingForm.setPopulation(null);
		}

		processReportedOffenses(summaryReportRequest, humanTraffickingForm);
		processOffenseClearances(summaryReportRequest, humanTraffickingForm);
		
		fillTheGrandTotalRow(humanTraffickingForm);

		log.info("humanTraffickingForm: " + humanTraffickingForm);
		return humanTraffickingForm;
	}

	
	private void processReportedOffenses(SummaryReportRequest summaryReportRequest,
			HumanTraffickingForm humanTraffickingForm) {
		List<AdministrativeSegment> administrativeSegments = 
				administrativeSegmentService.findHumanTraffickingIncidentByRequest(summaryReportRequest);
		countReportedOffenses(humanTraffickingForm, administrativeSegments);
	}
	
	private void processOffenseClearances(SummaryReportRequest summaryReportRequest,
			HumanTraffickingForm humanTraffickingForm) {
		List<AdministrativeSegment> administrativeSegments = administrativeSegmentService.findHumanTraffickingIncidentByRequestAndClearanceDate(summaryReportRequest);
		countOffenseClearances(humanTraffickingForm, administrativeSegments);
	}

	private void countOffenseClearances(HumanTraffickingForm humanTraffickingForm,
			List<AdministrativeSegment> administrativeSegments) {
		for (AdministrativeSegment administrativeSegment: administrativeSegments){
			if (administrativeSegment.getOffenseSegments().size() == 0) continue;
			
			boolean isClearanceInvolvingOnlyJuvenile = administrativeSegment.isClearanceInvolvingOnlyJuvenile();
			
			OffenseSegment offense = getHumanTraffickingOffense(administrativeSegment);
			HumanTraffickingRowName humanTraffickingRowName = null; 
			int offenseCount = 1; 
			switch (OffenseCode.forCode(offense.getUcrOffenseCodeType().getNibrsCode())){
			case _64A:
				humanTraffickingRowName = HumanTraffickingRowName.COMMERCIAL_SEX_ACTS;
				offenseCount = getOffenseCountByConnectedVictim(administrativeSegment, offense);
				break; 
			case _64B: 
				humanTraffickingRowName = HumanTraffickingRowName.INVOLUNTARY_SERVITUDE; 
				offenseCount = getOffenseCountByConnectedVictim(administrativeSegment, offense);
				break; 
			default: 
			}
			
			if (humanTraffickingRowName != null){
				humanTraffickingForm.getRows()[humanTraffickingRowName.ordinal()].increaseClearedOffenses(offenseCount);
				
				if (isClearanceInvolvingOnlyJuvenile){
					humanTraffickingForm.getRows()[humanTraffickingRowName.ordinal()].increaseClearanceInvolvingOnlyJuvenile(offenseCount);
				}
			}

		}
	}

	public OffenseSegment getHumanTraffickingOffense(AdministrativeSegment administrativeSegment) {

		OffenseSegment offense = administrativeSegment.getOffenseSegments()
				.stream()
				.filter(offenseSegment->
					Arrays.asList("A", "C").contains(offenseSegment.getOffenseAttemptedCompleted()) &&
					offenseSegment.getUcrOffenseCodeType().getNibrsCode().contentEquals("64A"))
				.findFirst().orElse(null);
		
		if (offense == null) {
			offense = administrativeSegment.getOffenseSegments()
				.stream()
				.filter(offenseSegment->
					Arrays.asList("A", "C").contains(offenseSegment.getOffenseAttemptedCompleted()) &&
					offenseSegment.getUcrOffenseCodeType().getNibrsCode().contentEquals("64B"))
				.findFirst().orElse(null);
		}
		return offense;
	}

	private void countReportedOffenses(HumanTraffickingForm humanTraffickingForm,
			List<AdministrativeSegment> administrativeSegments) {
		for (AdministrativeSegment administrativeSegment: administrativeSegments){
			if (administrativeSegment.getOffenseSegments().size() == 0) continue; 
			
			OffenseSegment offense = getHumanTraffickingOffense(administrativeSegment); 
			HumanTraffickingRowName humanTraffickingRowName = null; 
			int offenseCount = 1; 
			OffenseCode offenseCode = OffenseCode.forCode(offense.getUcrOffenseCodeType().getNibrsCode()); 
			switch (offenseCode){
			case _64A:
				humanTraffickingRowName = HumanTraffickingRowName.COMMERCIAL_SEX_ACTS;
				offenseCount = getOffenseCountByConnectedVictim(administrativeSegment, offense);
				break; 
			case _64B: 
				humanTraffickingRowName = HumanTraffickingRowName.INVOLUNTARY_SERVITUDE; 
				offenseCount = getOffenseCountByConnectedVictim(administrativeSegment, offense);
				break; 
			default: 
			}
			
			if (humanTraffickingRowName != null){
				humanTraffickingForm.getRows()[humanTraffickingRowName.ordinal()].increaseReportedOffenses(offenseCount);
			}
			
		}
	}

	private int getOffenseCountByConnectedVictim(AdministrativeSegment administrativeSegment, OffenseSegment offense) {
		long offenseCount = administrativeSegment.getVictimSegments()
				.stream()
				.filter(victim->victim.getOffenseSegments().contains(offense))
				.count();
		return Long.valueOf(offenseCount).intValue();
	}

	private void fillTheGrandTotalRow(HumanTraffickingForm humanTraffickingForm) {
		HumanTraffickingRowName totalRow = HumanTraffickingRowName.GRAND_TOTAL; 
		
		fillTheTotalRow(humanTraffickingForm, totalRow, HumanTraffickingRowName.COMMERCIAL_SEX_ACTS, 
				HumanTraffickingRowName.INVOLUNTARY_SERVITUDE);
		
	}

	private void fillTheTotalRow(HumanTraffickingForm humanTraffickingForm, HumanTraffickingRowName totalRow, HumanTraffickingRowName... rowsArray) {
		List<HumanTraffickingRowName> rows = Arrays.asList(rowsArray);
		int totalReportedOffense = 
				rows.stream()
					.mapToInt(row -> humanTraffickingForm.getRows()[row.ordinal()].getReportedOffenses())
					.sum(); 
		humanTraffickingForm.getRows()[totalRow.ordinal()].setReportedOffenses(totalReportedOffense);
		
		int totalUnfoundedOffense = 
				rows.stream()
				.mapToInt(row -> humanTraffickingForm.getRows()[row.ordinal()].getUnfoundedOffenses())
				.sum(); 
		humanTraffickingForm.getRows()[totalRow.ordinal()].setUnfoundedOffenses(totalUnfoundedOffense);
		
		int totalClearedOffense = 
				rows.stream()
				.mapToInt(row -> humanTraffickingForm.getRows()[row.ordinal()].getClearedOffenses())
				.sum(); 
		humanTraffickingForm.getRows()[totalRow.ordinal()].setClearedOffenses(totalClearedOffense);
		
		int totalClearanceInvolvingJuvenile = 
				rows.stream()
				.mapToInt(row -> humanTraffickingForm.getRows()[row.ordinal()].getClearanceInvolvingOnlyJuvenile())
				.sum(); 
		humanTraffickingForm.getRows()[totalRow.ordinal()].setClearanceInvolvingOnlyJuvenile(totalClearanceInvolvingJuvenile);
	}

}
