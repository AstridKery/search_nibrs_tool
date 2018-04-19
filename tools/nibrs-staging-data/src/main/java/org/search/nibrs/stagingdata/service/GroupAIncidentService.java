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
package org.search.nibrs.stagingdata.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

import javax.transaction.Transactional;

import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.search.nibrs.common.ParsedObject;
import org.search.nibrs.model.GroupAIncidentReport;
import org.search.nibrs.model.NIBRSAge;
import org.search.nibrs.stagingdata.controller.BadRequestException;
import org.search.nibrs.stagingdata.model.AdditionalJustifiableHomicideCircumstancesType;
import org.search.nibrs.stagingdata.model.Agency;
import org.search.nibrs.stagingdata.model.AggravatedAssaultHomicideCircumstancesType;
import org.search.nibrs.stagingdata.model.ArresteeSegmentWasArmedWith;
import org.search.nibrs.stagingdata.model.ArresteeWasArmedWithType;
import org.search.nibrs.stagingdata.model.BiasMotivationType;
import org.search.nibrs.stagingdata.model.CargoTheftIndicatorType;
import org.search.nibrs.stagingdata.model.ClearedExceptionallyType;
import org.search.nibrs.stagingdata.model.DispositionOfArresteeUnder18Type;
import org.search.nibrs.stagingdata.model.EthnicityOfPersonType;
import org.search.nibrs.stagingdata.model.LocationType;
import org.search.nibrs.stagingdata.model.MethodOfEntryType;
import org.search.nibrs.stagingdata.model.MultipleArresteeSegmentsIndicatorType;
import org.search.nibrs.stagingdata.model.OffenderSuspectedOfUsingType;
import org.search.nibrs.stagingdata.model.OfficerActivityCircumstanceType;
import org.search.nibrs.stagingdata.model.OfficerAssignmentTypeType;
import org.search.nibrs.stagingdata.model.PropertyDescriptionType;
import org.search.nibrs.stagingdata.model.PropertyType;
import org.search.nibrs.stagingdata.model.RaceOfPersonType;
import org.search.nibrs.stagingdata.model.ResidentStatusOfPersonType;
import org.search.nibrs.stagingdata.model.SexOfPersonType;
import org.search.nibrs.stagingdata.model.SuspectedDrugType;
import org.search.nibrs.stagingdata.model.SuspectedDrugTypeType;
import org.search.nibrs.stagingdata.model.TypeDrugMeasurementType;
import org.search.nibrs.stagingdata.model.TypeInjuryType;
import org.search.nibrs.stagingdata.model.TypeOfArrestType;
import org.search.nibrs.stagingdata.model.TypeOfCriminalActivityType;
import org.search.nibrs.stagingdata.model.TypeOfVictimType;
import org.search.nibrs.stagingdata.model.TypeOfWeaponForceInvolved;
import org.search.nibrs.stagingdata.model.TypeOfWeaponForceInvolvedType;
import org.search.nibrs.stagingdata.model.TypePropertyLossEtcType;
import org.search.nibrs.stagingdata.model.UcrOffenseCodeType;
import org.search.nibrs.stagingdata.model.VictimOffenderAssociation;
import org.search.nibrs.stagingdata.model.VictimOffenderRelationshipType;
import org.search.nibrs.stagingdata.model.segment.AdministrativeSegment;
import org.search.nibrs.stagingdata.model.segment.ArresteeSegment;
import org.search.nibrs.stagingdata.model.segment.OffenderSegment;
import org.search.nibrs.stagingdata.model.segment.OffenseSegment;
import org.search.nibrs.stagingdata.model.segment.PropertySegment;
import org.search.nibrs.stagingdata.model.segment.VictimSegment;
import org.search.nibrs.stagingdata.repository.AdditionalJustifiableHomicideCircumstancesTypeRepository;
import org.search.nibrs.stagingdata.repository.AgencyRepository;
import org.search.nibrs.stagingdata.repository.AggravatedAssaultHomicideCircumstancesTypeRepository;
import org.search.nibrs.stagingdata.repository.ArresteeWasArmedWithTypeRepository;
import org.search.nibrs.stagingdata.repository.BiasMotivationTypeRepository;
import org.search.nibrs.stagingdata.repository.CargoTheftIndicatorTypeRepository;
import org.search.nibrs.stagingdata.repository.ClearedExceptionallyTypeRepository;
import org.search.nibrs.stagingdata.repository.DispositionOfArresteeUnder18TypeRepository;
import org.search.nibrs.stagingdata.repository.EthnicityOfPersonTypeRepository;
import org.search.nibrs.stagingdata.repository.LocationTypeRepository;
import org.search.nibrs.stagingdata.repository.MethodOfEntryTypeRepository;
import org.search.nibrs.stagingdata.repository.MultipleArresteeSegmentsIndicatorTypeRepository;
import org.search.nibrs.stagingdata.repository.OffenderSuspectedOfUsingTypeRepository;
import org.search.nibrs.stagingdata.repository.OfficerActivityCircumstanceTypeRepository;
import org.search.nibrs.stagingdata.repository.OfficerAssignmentTypeTypeRepository;
import org.search.nibrs.stagingdata.repository.PropertyDescriptionTypeRepository;
import org.search.nibrs.stagingdata.repository.RaceOfPersonTypeRepository;
import org.search.nibrs.stagingdata.repository.ResidentStatusOfPersonTypeRepository;
import org.search.nibrs.stagingdata.repository.SegmentActionTypeRepository;
import org.search.nibrs.stagingdata.repository.SexOfPersonTypeRepository;
import org.search.nibrs.stagingdata.repository.SuspectedDrugTypeTypeRepository;
import org.search.nibrs.stagingdata.repository.TypeDrugMeasurementTypeRepository;
import org.search.nibrs.stagingdata.repository.TypeInjuryTypeRepository;
import org.search.nibrs.stagingdata.repository.TypeOfArrestTypeRepository;
import org.search.nibrs.stagingdata.repository.TypeOfCriminalActivityTypeRepository;
import org.search.nibrs.stagingdata.repository.TypeOfVictimTypeRepository;
import org.search.nibrs.stagingdata.repository.TypeOfWeaponForceInvolvedTypeRepository;
import org.search.nibrs.stagingdata.repository.TypePropertyLossEtcTypeRepository;
import org.search.nibrs.stagingdata.repository.UcrOffenseCodeTypeRepository;
import org.search.nibrs.stagingdata.repository.VictimOffenderRelationshipTypeRepository;
import org.search.nibrs.stagingdata.repository.segment.AdministrativeSegmentRepository;
import org.search.nibrs.stagingdata.repository.segment.OffenseSegmentRepository;
import org.search.nibrs.stagingdata.util.DateUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * Service to process Group B Arrest Report.  
 *
 */
@Service
public class GroupAIncidentService {
	private static final Log log = LogFactory.getLog(GroupAIncidentService.class);
	private static final String BAD_DELETE_REQUEST = "The incident number is required to delete an incident";
	@Autowired
	AdministrativeSegmentRepository administrativeSegmentRepository;
	@Autowired
	OffenseSegmentRepository offenseSegmentRepository;
	@Autowired
	public AgencyRepository agencyRepository; 
	@Autowired
	public SegmentActionTypeRepository segmentActionTypeRepository; 
	@Autowired
	public ClearedExceptionallyTypeRepository clearedExceptionallyTypeRepository; 
	@Autowired
	public UcrOffenseCodeTypeRepository ucrOffenseCodeTypeRepository; 
	@Autowired
	public LocationTypeRepository locationTypeRepository; 
	@Autowired
	public MethodOfEntryTypeRepository methodOfEntryTypeRepository; 
	@Autowired
	public BiasMotivationTypeRepository biasMotivationTypeRepository; 
	@Autowired
	public CargoTheftIndicatorTypeRepository cargoTheftIndicatorTypeRepository; 
	@Autowired
	public TypeOfWeaponForceInvolvedTypeRepository typeOfWeaponForceInvolvedTypeRepository; 
	@Autowired
	public OffenderSuspectedOfUsingTypeRepository offenderSuspectedOfUsingTypeRepository; 
	@Autowired
	public TypeOfCriminalActivityTypeRepository typeOfCriminalActivityTypeRepository; 
	@Autowired
	public TypePropertyLossEtcTypeRepository typePropertyLossEtcTypeRepository; 
	@Autowired
	public TypeDrugMeasurementTypeRepository typeDrugMeasurementTypeRepository; 
	@Autowired
	public PropertyDescriptionTypeRepository propertyDescriptionTypeRepository; 
	@Autowired
	public SuspectedDrugTypeTypeRepository suspectedDrugTypeTypeRepository; 
	@Autowired
	public DispositionOfArresteeUnder18TypeRepository dispositionOfArresteeUnder18TypeRepository; 
	@Autowired
	public EthnicityOfPersonTypeRepository ethnicityOfPersonTypeRepository; 
	@Autowired
	public RaceOfPersonTypeRepository raceOfPersonTypeRepository; 
	@Autowired
	public SexOfPersonTypeRepository sexOfPersonTypeRepository; 
	@Autowired
	public TypeOfArrestTypeRepository typeOfArrestTypeRepository; 
	@Autowired
	public ResidentStatusOfPersonTypeRepository residentStatusOfPersonTypeRepository; 
	@Autowired
	public MultipleArresteeSegmentsIndicatorTypeRepository multipleArresteeSegmentsIndicatorTypeRepository; 
	@Autowired
	public ArresteeWasArmedWithTypeRepository arresteeWasArmedWithTypeRepository; 
	@Autowired
	public TypeOfVictimTypeRepository typeOfVictimTypeRepository; 
	@Autowired
	public OfficerActivityCircumstanceTypeRepository officerActivityCircumstanceTypeRepository; 
	@Autowired
	public OfficerAssignmentTypeTypeRepository officerAssignmentTypeTypeRepository; 
	@Autowired
	public AdditionalJustifiableHomicideCircumstancesTypeRepository additionalJustifiableHomicideCircumstancesTypeRepository; 
	@Autowired
	public TypeInjuryTypeRepository typeInjuryTypeRepository; 
	@Autowired
	public AggravatedAssaultHomicideCircumstancesTypeRepository aggravatedAssaultHomicideCircumstancesTypeRepository; 
	@Autowired
	public VictimOffenderRelationshipTypeRepository victimOffenderRelationshipTypeRepository; 
	@Autowired
	public CodeTableService codeTableService; 
	
	@Transactional
	public AdministrativeSegment saveAdministrativeSegment(AdministrativeSegment administrativeSegment){
		return administrativeSegmentRepository.save(administrativeSegment);
	}
	
	@Transactional
	public long deleteAdministrativeSegment(String incidentNumber){
		return administrativeSegmentRepository.deleteByIncidentNumber(incidentNumber);
	}
	
	@Transactional
	public long deleteGroupAIncidentReport(String incidentNumber){
		if ( StringUtils.isBlank(incidentNumber)){
			log.error(BAD_DELETE_REQUEST); 
			throw new BadRequestException(BAD_DELETE_REQUEST);
		}

		return administrativeSegmentRepository.deleteByIncidentNumber(incidentNumber);
	}
	
	public AdministrativeSegment findAdministrativeSegment(Integer id){
		return administrativeSegmentRepository.findByAdministrativeSegmentId(id);
	}
	
	public List<AdministrativeSegment> findAllAdministrativeSegments(){
		List<AdministrativeSegment> administrativeSegments = new ArrayList<>();
		administrativeSegmentRepository.findAll().forEach(administrativeSegments::add);
		return administrativeSegments;
	}
	
	public OffenseSegment saveOffenseSegment(OffenseSegment offenseSegment){
		return offenseSegmentRepository.save(offenseSegment);
	}
	
	public Iterable<OffenseSegment> saveOffenseSegment(List<OffenseSegment> offenseSegments){
		return offenseSegmentRepository.save(offenseSegments);
	}
	
	public Iterable<AdministrativeSegment> saveGroupAIncidentReports(GroupAIncidentReport... groupAIncidentReports){
		List<AdministrativeSegment> administrativeSegments = new ArrayList<>();
		
		for (GroupAIncidentReport groupAIncidentReport: groupAIncidentReports){
			if (administrativeSegmentRepository.existsByIncidentNumber(groupAIncidentReport.getIncidentNumber())){
				administrativeSegmentRepository.deleteByIncidentNumber(groupAIncidentReport.getIncidentNumber());
			}
			
			AdministrativeSegment administrativeSegment = new AdministrativeSegment(); 
//			Optional<AdministrativeSegment> existingAdministrativeSegment = 
//					Optional.ofNullable(administrativeSegmentRepository.findFirstByIncidentNumber(groupAIncidentReport.getIncidentNumber()));
//				
//			AdministrativeSegment administrativeSegment = existingAdministrativeSegment.orElseGet(AdministrativeSegment::new); 

			administrativeSegment.setAgency(agencyRepository.findFirstByAgencyOri(groupAIncidentReport.getOri()));
			
			String reportActionType = String.valueOf(groupAIncidentReport.getReportActionType()).trim();
			administrativeSegment.setSegmentActionType(segmentActionTypeRepository.findFirstBySegmentActionTypeCode(reportActionType));
			
			Optional<Integer> monthOfTape = Optional.ofNullable(groupAIncidentReport.getMonthOfTape());
			monthOfTape.ifPresent( m-> {
				administrativeSegment.setMonthOfTape(StringUtils.leftPad(String.valueOf(m), 2, '0'));
			});
			
			if (groupAIncidentReport.getYearOfTape() != null){
				administrativeSegment.setYearOfTape(String.valueOf(groupAIncidentReport.getYearOfTape()));
			}
			
			administrativeSegment.setCityIndicator(groupAIncidentReport.getCityIndicator());
			administrativeSegment.setOri(groupAIncidentReport.getOri());
			administrativeSegment.setIncidentNumber(groupAIncidentReport.getIncidentNumber());
			administrativeSegment.setIncidentDate(DateUtils.asDate(groupAIncidentReport.getIncidentDate().getValue()));
			administrativeSegment.setIncidentDateType(codeTableService.getDateType(DateUtils.asDate(groupAIncidentReport.getIncidentDate().getValue())));
			administrativeSegment.setReportDateIndicator(groupAIncidentReport.getReportDateIndicator());
			administrativeSegment.setReportDateIndicator(groupAIncidentReport.getReportDateIndicator());
			administrativeSegment.setExceptionalClearanceDate(DateUtils.asDate(groupAIncidentReport.getExceptionalClearanceDate().getValue()));
			administrativeSegment.setExceptionalClearanceDateType(codeTableService.getDateType(DateUtils.asDate(groupAIncidentReport.getExceptionalClearanceDate().getValue())));
			
			Optional<Integer> incidentHour = Optional.ofNullable(groupAIncidentReport.getIncidentHour().getValue());
			administrativeSegment.setIncidentHour(incidentHour.map(String::valueOf).orElse(""));
			
			ClearedExceptionallyType clearedExceptionallyType = 
					codeTableService.getCodeTableType(groupAIncidentReport.getExceptionalClearanceCode(), 
							clearedExceptionallyTypeRepository::findFirstByClearedExceptionallyCode, 
							ClearedExceptionallyType::new); 
			administrativeSegment.setClearedExceptionallyType(clearedExceptionallyType);
			
			Agency agency = codeTableService.getCodeTableType(groupAIncidentReport.getOri(), agencyRepository::findFirstByAgencyOri, Agency::new); 
			administrativeSegment.setAgency(agency);
			
			CargoTheftIndicatorType cargoTheftIndicatorType = 
					codeTableService.getCodeTableType(groupAIncidentReport.getCargoTheftIndicator(), 
							cargoTheftIndicatorTypeRepository::findFirstByCargoTheftIndicatorCode, CargoTheftIndicatorType::new); 
			administrativeSegment.setCargoTheftIndicatorType(cargoTheftIndicatorType);
			
			processProperties(administrativeSegment, groupAIncidentReport);
			processOffenses(administrativeSegment, groupAIncidentReport);
			processOffenders(administrativeSegment, groupAIncidentReport);
			processArrestees(administrativeSegment, groupAIncidentReport);
			processVictims(administrativeSegment, groupAIncidentReport);
			administrativeSegments.add(administrativeSegment);
		}
		
		return administrativeSegmentRepository.save(administrativeSegments);
	}
	private void processProperties(AdministrativeSegment administrativeSegment,
			GroupAIncidentReport groupAIncidentReport) {
		if (groupAIncidentReport.getPropertyCount() > 0){
			Set<PropertySegment> propertySegments = Optional.ofNullable(administrativeSegment.getPropertySegments())
					.orElseGet(HashSet::new);
			propertySegments.clear();
			
			for (org.search.nibrs.model.PropertySegment property: groupAIncidentReport.getProperties()){
				PropertySegment propertySegment = new PropertySegment();
				propertySegment.setSegmentActionType(administrativeSegment.getSegmentActionType());
				propertySegment.setAdministrativeSegment(administrativeSegment);
				
				TypePropertyLossEtcType typePropertyLossEtcType = codeTableService.getCodeTableType(
						property.getTypeOfPropertyLoss(), typePropertyLossEtcTypeRepository::findFirstByTypePropertyLossEtcCode, TypePropertyLossEtcType::new);
				propertySegment.setTypePropertyLossEtcType(typePropertyLossEtcType );
				
				Integer numberOfRecoveredMotorVehicles = Optional.ofNullable(property.getNumberOfRecoveredMotorVehicles())
						.map(ParsedObject::getValue).orElse(null);
				propertySegment.setNumberOfRecoveredMotorVehicles(numberOfRecoveredMotorVehicles);
				
				Integer numberOfStolenMotorVehicles = Optional.ofNullable(property.getNumberOfStolenMotorVehicles())
						.map(ParsedObject::getValue).orElse(null);
				propertySegment.setNumberOfStolenMotorVehicles(numberOfStolenMotorVehicles);
//				3 PropertySegment Segments:
//				PropertySegment [typeOfPropertyLoss=7, propertyDescription=[20, null, null, null, null, null, null, null, null, null], valueOfProperty=[5000, null, null, null, null, null, null, null, null, null], dateRecovered=[null, null, null, null, null, null, null, null, null, null], numberOfStolenMotorVehicles=null, numberOfRecoveredMotorVehicles=null, suspectedDrugType=[null, null, null], estimatedDrugQuantity=[null, null, null], typeDrugMeasurement=[null, null, null], populatedPropertyDescriptionCount=1, populatedSuspectedDrugTypeCount=0]
//				PropertySegment [typeOfPropertyLoss=5, propertyDescription=[20, null, null, null, null, null, null, null, null, null], valueOfProperty=[5000, null, null, null, null, null, null, null, null, null], dateRecovered=[Thu Jan 08 00:00:00 CST 2015, null, null, null, null, null, null, null, null, null], numberOfStolenMotorVehicles=null, numberOfRecoveredMotorVehicles=null, suspectedDrugType=[null, null, null], estimatedDrugQuantity=[null, null, null], typeDrugMeasurement=[null, null, null], populatedPropertyDescriptionCount=1, populatedSuspectedDrugTypeCount=0]
//				PropertySegment [typeOfPropertyLoss=6, propertyDescription=[10, 11, null, null, null, null, null, null, null, null], valueOfProperty=[null, 100, null, null, null, null, null, null, null, null], dateRecovered=[null, null, null, null, null, null, null, null, null, null], numberOfStolenMotorVehicles=null, numberOfRecoveredMotorVehicles=null, suspectedDrugType=[E, E, X], estimatedDrugQuantity=[0.001, 0.001, null], typeDrugMeasurement=[LB, OZ, null], populatedPropertyDescriptionCount=2, populatedSuspectedDrugTypeCount=3]
				processPropertyType(propertySegment, property); 
				processSuspectedDrugType(propertySegment, property); 

				propertySegments.add(propertySegment);
			}
			
			administrativeSegment.setPropertySegments(propertySegments);
		}
		
	}

	private void processSuspectedDrugType(PropertySegment propertySegment,
			org.search.nibrs.model.PropertySegment property) {
		Set<SuspectedDrugType> suspectedDrugTypes = new HashSet<>(); 
		if (property.getPopulatedSuspectedDrugTypeCount() > 0){
			for (int i = 0; i < property.getPopulatedSuspectedDrugTypeCount(); i++){
				String suspectedDrugTypeString = StringUtils.trimToNull(property.getSuspectedDrugType(i)); 
				
				SuspectedDrugTypeType suspectedDrugTypeType = 
						codeTableService.getCodeTableType(suspectedDrugTypeString, suspectedDrugTypeTypeRepository::findFirstBySuspectedDrugTypeCode, null);
				
				if (suspectedDrugTypeType != null){
					SuspectedDrugType suspectedDrugType = new SuspectedDrugType(); 
					suspectedDrugType.setPropertySegment(propertySegment);
					suspectedDrugType.setSuspectedDrugTypeType(suspectedDrugTypeType);
					
					Double estimatedDrugQuantity = 
							Optional.ofNullable(property.getEstimatedDrugQuantity(i)).map(ParsedObject::getValue).orElse(null);
					suspectedDrugType.setEstimatedDrugQuantity(estimatedDrugQuantity);
					
					
					TypeDrugMeasurementType typeDrugMeasurementType = codeTableService.getCodeTableType(
							property.getTypeDrugMeasurement(i), typeDrugMeasurementTypeRepository::findFirstByTypeDrugMeasurementCode, TypeDrugMeasurementType::new);
					suspectedDrugType.setTypeDrugMeasurementType(typeDrugMeasurementType );
					
					suspectedDrugTypes.add(suspectedDrugType);
				}
			}
		}
		
		if (!suspectedDrugTypes.isEmpty()){
			propertySegment.setSuspectedDrugTypes(suspectedDrugTypes);
		}
		
	}

	private void processPropertyType(PropertySegment propertySegment, org.search.nibrs.model.PropertySegment property) {
		Set<PropertyType> propertyTypes = new HashSet<>(); 
		if (property.getPopulatedPropertyDescriptionCount() > 0){
			for (int i = 0; i < property.getPopulatedPropertyDescriptionCount(); i++){
				String propertyDescription = StringUtils.trimToNull(property.getPropertyDescription(i)); 
				
				PropertyDescriptionType propertyDescriptionType = 
						codeTableService.getCodeTableType(propertyDescription, propertyDescriptionTypeRepository::findFirstByPropertyDescriptionCode, null);
				
				if (propertyDescriptionType != null){
					PropertyType propertyType = new PropertyType(); 
					propertyType.setPropertySegment(propertySegment);
					propertyType.setPropertyDescriptionType(propertyDescriptionType);
					Double valueOfProperty = 
							Optional.ofNullable(property.getValueOfProperty(i))
								.map(ParsedObject::getValue)
								.map(Double::valueOf)
								.orElse(null);
					propertyType.setValueOfProperty(valueOfProperty);

					Date dateRecovered = DateUtils.asDate(property.getDateRecovered()[i].getValue());
					propertyType.setRecoveredDate(dateRecovered);
					propertyType.setRecoveredDateType(codeTableService.getDateType(dateRecovered));
					
					propertyTypes.add(propertyType);
				}
			}
		}
		
		if (!propertyTypes.isEmpty()){
			propertySegment.setPropertyTypes(propertyTypes);
		}
	}
	
//	1 VictimSegment Segments:
//		VictimSegment [age=20-22, sex=F, race=B, ethnicity=N, victimSequenceNumber=01, 
//	ucrOffenseCodeConnection=[13A, null, null, null, null, null, null, null, null, null], 
//	typeOfVictim=I, residentStatus=R, aggravatedAssaultHomicideCircumstances=[null, null], 
//	additionalJustifiableHomicideCircumstances=null, typeOfInjury=[N, null, null, null, null], 
//	offenderNumberRelated=[01, null, null, null, null, null, null, null, null, null], 
//	victimOffenderRelationship=[SE, null, null, null, null, null, null, null, null, null], 
//	typeOfOfficerActivityCircumstance=null, officerAssignmentType=null, officerOtherJurisdictionORI=null, 
//	populatedAggravatedAssaultHomicideCircumstancesCount=0, 
//	populatedTypeOfInjuryCount=1, populatedUcrOffenseCodeConnectionCount=1, populatedOffenderNumberRelatedCount=1]
//	,segmentType=6]
	
	private void processVictims(AdministrativeSegment administrativeSegment,
			GroupAIncidentReport groupAIncidentReport) {
		
		if (groupAIncidentReport.getVictimCount() > 0){
			Map<String, OffenseSegment> offenseCodeOffenseMap = new HashMap<>(); 
			Optional<Set<OffenseSegment>> offenseSegments= Optional.ofNullable(administrativeSegment.getOffenseSegments());
			offenseSegments.ifPresent(offenses -> 
			offenses.forEach(offense->offenseCodeOffenseMap.put(offense.getUcrOffenseCodeType().getUcrOffenseCode(), offense))
					);
			
			Map<Integer, OffenderSegment> offenderSequenceNumberOffenderMap = new HashMap<>();
			Optional<Set<OffenderSegment>> offenderSegments = Optional.ofNullable(administrativeSegment.getOffenderSegments()); 
			offenderSegments.ifPresent( offenders -> 
			offenders.forEach(offender->offenderSequenceNumberOffenderMap.put(offender.getOffenderSequenceNumber(), offender)));
			
			Set<VictimSegment> victimSegments = Optional.ofNullable(administrativeSegment.getVictimSegments())
					.orElseGet(HashSet::new);
			victimSegments.clear();
			for (org.search.nibrs.model.VictimSegment victim: groupAIncidentReport.getVictims()){
				VictimSegment victimSegment = new VictimSegment();
				victimSegment.setSegmentActionType(administrativeSegment.getSegmentActionType());
				victimSegment.setAdministrativeSegment(administrativeSegment);
				victimSegment.setVictimSequenceNumber(victim.getVictimSequenceNumber().getValue());

				TypeOfVictimType typeOfVictimType = 
						codeTableService.getCodeTableType(victim.getTypeOfVictim(), typeOfVictimTypeRepository::findFirstByTypeOfVictimCode, TypeOfVictimType::new);
				victimSegment.setTypeOfVictimType(typeOfVictimType);
				
				OfficerActivityCircumstanceType officerActivityCircumstanceType = 
						codeTableService.getCodeTableType(victim.getTypeOfOfficerActivityCircumstance(), 
								officerActivityCircumstanceTypeRepository::findFirstByOfficerActivityCircumstanceCode, 
								OfficerActivityCircumstanceType::new);
				victimSegment.setOfficerActivityCircumstanceType(officerActivityCircumstanceType);
				
				OfficerAssignmentTypeType officerAssignmentTypeType = 
						codeTableService.getCodeTableType(victim.getOfficerAssignmentType(), 
								officerAssignmentTypeTypeRepository::findFirstByOfficerAssignmentTypeCode, 
								OfficerAssignmentTypeType::new);
				victimSegment.setOfficerAssignmentTypeType(officerAssignmentTypeType);
				
				victimSegment.setOfficerOtherJurisdictionOri(victim.getOfficerOtherJurisdictionORI());
				
				Optional<NIBRSAge> victimAge = Optional.ofNullable(victim.getAge());
				victimSegment.setAgeOfVictimMax(victimAge.map(NIBRSAge::getAgeMax).orElse(null));
				victimSegment.setAgeOfVictimMin(victimAge.map(NIBRSAge::getAgeMin).orElse(null));
				victimSegment.setAgeNeonateIndicator(BooleanUtils.toIntegerObject(victimAge.map(NIBRSAge::isNeonate).orElse(false)));
				victimSegment.setAgeFirstWeekIndicator(BooleanUtils.toIntegerObject(victimAge.map(NIBRSAge::isNewborn).orElse(false)));
				victimSegment.setAgeFirstYearIndicator(BooleanUtils.toIntegerObject(victimAge.map(NIBRSAge::isBaby).orElse(false)));
				
				SexOfPersonType sexOfPersonType = codeTableService.getCodeTableType(
						victim.getSex(), sexOfPersonTypeRepository::findFirstBySexOfPersonCode, SexOfPersonType::new);
				victimSegment.setSexOfPersonType(sexOfPersonType);
				
				RaceOfPersonType raceOfPersonType = codeTableService.getCodeTableType(
						victim.getRace(), raceOfPersonTypeRepository::findFirstByRaceOfPersonCode, RaceOfPersonType::new);
				victimSegment.setRaceOfPersonType(raceOfPersonType);
				
				EthnicityOfPersonType ethnicityOfPersonType = codeTableService.getCodeTableType(
						victim.getEthnicity(), ethnicityOfPersonTypeRepository::findFirstByEthnicityOfPersonCode, EthnicityOfPersonType::new);
				victimSegment.setEthnicityOfPersonType(ethnicityOfPersonType);
				
				ResidentStatusOfPersonType residentStatusOfPersonType = codeTableService.getCodeTableType(
						victim.getResidentStatus(), 
						residentStatusOfPersonTypeRepository::findFirstByResidentStatusOfPersonCode, 
						ResidentStatusOfPersonType::new);
				victimSegment.setResidentStatusOfPersonType(residentStatusOfPersonType);
				
				AdditionalJustifiableHomicideCircumstancesType additionalJustifiableHomicideCircumstancesType = codeTableService.getCodeTableType(
						victim.getAdditionalJustifiableHomicideCircumstances(), 
						additionalJustifiableHomicideCircumstancesTypeRepository::findFirstByAdditionalJustifiableHomicideCircumstancesCode, 
						AdditionalJustifiableHomicideCircumstancesType::new);
				victimSegment.setAdditionalJustifiableHomicideCircumstancesType(additionalJustifiableHomicideCircumstancesType);
				
				processTypeInjuryTypes(victimSegment, victim);
				processAggravatedAssaultHomicideCircumstancesTypes(victimSegment, victim);
				
				if (victim.getPopulatedUcrOffenseCodeConnectionCount() > 0){
					Set<OffenseSegment> offenses = new HashSet<>();
					Arrays.stream(victim.getUcrOffenseCodeConnection())
							.filter(StringUtils::isNotBlank)
							.map(offenseCodeOffenseMap::get)
							.filter(Objects::nonNull)
							.forEach(offenses::add); 
					victimSegment.setOffenseSegments(offenses);
				}
				
				processVictimOffenderAssociations(victimSegment, victim, offenderSequenceNumberOffenderMap);
				victimSegments.add(victimSegment);
			}
			administrativeSegment.setVictimSegments(victimSegments);
		}
		
	}

	private void processVictimOffenderAssociations(VictimSegment victimSegment,
			org.search.nibrs.model.VictimSegment victim,
			Map<Integer, OffenderSegment> offenderSequenceNumberOffenderMap) {
		if (victim.getPopulatedOffenderNumberRelatedCount() > 0){
			Set<VictimOffenderAssociation> victimOffenderAssociations = new HashSet<>();
			for (int i = 0; i < victim.getPopulatedOffenderNumberRelatedCount(); i++){
				
				Integer offenderSequenceNumber = Optional.ofNullable(victim.getOffenderNumberRelated(i))
						.map(ParsedObject::getValue).orElse(null);
				OffenderSegment offenderSegment = 
						offenderSequenceNumberOffenderMap.get(offenderSequenceNumber); 
				if ( offenderSegment != null){
					VictimOffenderAssociation victimOffenderAssociation = new VictimOffenderAssociation(); 
					victimOffenderAssociation.setOffenderSegment(offenderSegment);
					victimOffenderAssociation.setVictimSegment(victimSegment);
					
					String victimOffenderRelationship = StringUtils.trimToNull(victim.getVictimOffenderRelationship(i)); 
					VictimOffenderRelationshipType victimOffenderRelationshipType = codeTableService
							.getCodeTableType(
									victimOffenderRelationship, 
									victimOffenderRelationshipTypeRepository::findFirstByVictimOffenderRelationshipCode, 
									VictimOffenderRelationshipType::new);
					victimOffenderAssociation.setVictimOffenderRelationshipType(victimOffenderRelationshipType);	
					victimOffenderAssociations.add(victimOffenderAssociation);
				}
			}
			victimSegment.setVictimOffenderAssociations(victimOffenderAssociations);
		}
	}

	private void processAggravatedAssaultHomicideCircumstancesTypes(VictimSegment victimSegment,
		org.search.nibrs.model.VictimSegment victim) {
		if (victim.getPopulatedAggravatedAssaultHomicideCircumstancesCount() > 0){
			Set<AggravatedAssaultHomicideCircumstancesType> aggravatedAssaultHomicideCircumstancesTypes = new HashSet<>();
			Arrays.stream(victim.getAggravatedAssaultHomicideCircumstances())
					.filter(StringUtils::isNotBlank)
					.map(item -> codeTableService.getCodeTableType(
								item, 
								aggravatedAssaultHomicideCircumstancesTypeRepository::findFirstByAggravatedAssaultHomicideCircumstancesCode, 
								null) )
					.filter(Objects::nonNull)
					.forEach(aggravatedAssaultHomicideCircumstancesTypes::add);
			victimSegment.setAggravatedAssaultHomicideCircumstancesTypes(aggravatedAssaultHomicideCircumstancesTypes);
		}
}

	private void processTypeInjuryTypes(VictimSegment victimSegment, org.search.nibrs.model.VictimSegment victim) {
		if (victim.getPopulatedTypeOfInjuryCount() > 0){
			Set<TypeInjuryType> typeInjuryTypes = new HashSet<>();
			Arrays.stream(victim.getTypeOfInjury())
					.filter(StringUtils::isNotBlank)
					.map(item -> codeTableService.getCodeTableType(item, typeInjuryTypeRepository::findFirstByTypeInjuryCode, null))
					.filter(Objects::nonNull)
					.forEach(typeInjuryTypes::add);
			victimSegment.setTypeInjuryTypes(typeInjuryTypes);
		}
	}

	private void processArrestReportSegmentArmedWiths(ArresteeSegment arresteeSegment, org.search.nibrs.model.ArresteeSegment arrestee) {
		Set<ArresteeSegmentWasArmedWith> armedWiths = new HashSet<>();  
		
		if (arrestee.containsArresteeArmedWith()){
			for (int i = 0; i < org.search.nibrs.model.ArresteeSegment.ARRESTEE_ARMED_WITH_COUNT; i++){
				String arresteeArmedWithCode = 
						StringUtils.trimToNull(arrestee.getArresteeArmedWith(i));
				String automaticWeaponIndicator = 
						StringUtils.trimToEmpty(arrestee.getAutomaticWeaponIndicator(i));
				
				if (StringUtils.isNotBlank(arresteeArmedWithCode)){
					Optional<ArresteeWasArmedWithType> arresteeWasArmedWithType = 
							Optional.ofNullable(codeTableService.getCodeTableType(arresteeArmedWithCode,
									arresteeWasArmedWithTypeRepository::findFirstByArresteeWasArmedWithCode, 
									null));
					arresteeWasArmedWithType.ifPresent( type ->
						armedWiths.add(new ArresteeSegmentWasArmedWith(
								null, arresteeSegment, type, automaticWeaponIndicator))
					);
				}
			}
		}
		
		if (!armedWiths.isEmpty()){
			arresteeSegment.setArresteeSegmentWasArmedWiths(armedWiths);
		}
	}
	
	private void processArrestees(AdministrativeSegment administrativeSegment,
			GroupAIncidentReport groupAIncidentReport) {
		if (groupAIncidentReport.getArresteeCount() > 0){
			Set<ArresteeSegment> arresteeSegments = Optional.ofNullable(administrativeSegment.getArresteeSegments())
					.orElseGet(HashSet::new);
			arresteeSegments.clear();
			for (org.search.nibrs.model.ArresteeSegment arrestee: groupAIncidentReport.getArrestees()){
				ArresteeSegment arresteeSegment = new ArresteeSegment();
				arresteeSegment.setSegmentActionType(administrativeSegment.getSegmentActionType());
				arresteeSegment.setAdministrativeSegment(administrativeSegment);
				arresteeSegment.setArresteeSequenceNumber(arrestee.getArresteeSequenceNumber().getValue());
				arresteeSegment.setArrestTransactionNumber(arrestee.getArrestTransactionNumber());
				arresteeSegment.setArrestDate(DateUtils.asDate(arrestee.getArrestDate().getValue()));
				arresteeSegment.setArrestDateType(codeTableService.getDateType(DateUtils.asDate(arrestee.getArrestDate().getValue())));
				
				TypeOfArrestType typeOfArrestType = codeTableService.getCodeTableType(
						arrestee.getTypeOfArrest(), typeOfArrestTypeRepository::findFirstByTypeOfArrestCode, TypeOfArrestType::new);
				arresteeSegment.setTypeOfArrestType(typeOfArrestType );
				
				MultipleArresteeSegmentsIndicatorType multipleArresteeSegmentsIndicatorType = 
						codeTableService.getCodeTableType(
							arrestee.getMultipleArresteeSegmentsIndicator(), 
							multipleArresteeSegmentsIndicatorTypeRepository::findFirstByMultipleArresteeSegmentsIndicatorCode, 
							MultipleArresteeSegmentsIndicatorType::new);
				arresteeSegment.setMultipleArresteeSegmentsIndicatorType(multipleArresteeSegmentsIndicatorType);
				
				arresteeSegment.setAgeOfArresteeMin(arrestee.getAge().getAgeMin());
				arresteeSegment.setAgeOfArresteeMax(arrestee.getAge().getAgeMax());

				SexOfPersonType sexOfPersonType = codeTableService.getCodeTableType(
						arrestee.getSex(), sexOfPersonTypeRepository::findFirstBySexOfPersonCode, SexOfPersonType::new);
				arresteeSegment.setSexOfPersonType(sexOfPersonType);
				
				RaceOfPersonType raceOfPersonType = codeTableService.getCodeTableType(
						arrestee.getRace(), raceOfPersonTypeRepository::findFirstByRaceOfPersonCode, RaceOfPersonType::new);
				arresteeSegment.setRaceOfPersonType(raceOfPersonType);
				
				EthnicityOfPersonType ethnicityOfPersonType = codeTableService.getCodeTableType(
						arrestee.getEthnicity(), ethnicityOfPersonTypeRepository::findFirstByEthnicityOfPersonCode, EthnicityOfPersonType::new);
				arresteeSegment.setEthnicityOfPersonType(ethnicityOfPersonType);
				
				ResidentStatusOfPersonType residentStatusOfPersonType = codeTableService.getCodeTableType(
						arrestee.getResidentStatus(), 
						residentStatusOfPersonTypeRepository::findFirstByResidentStatusOfPersonCode, 
						ResidentStatusOfPersonType::new);
				arresteeSegment.setResidentStatusOfPersonType(residentStatusOfPersonType);
				
				DispositionOfArresteeUnder18Type dispositionOfArresteeUnder18Type = codeTableService.getCodeTableType(
						arrestee.getDispositionOfArresteeUnder18(), 
						dispositionOfArresteeUnder18TypeRepository::findFirstByDispositionOfArresteeUnder18Code, 
						DispositionOfArresteeUnder18Type::new);
				arresteeSegment.setDispositionOfArresteeUnder18Type(dispositionOfArresteeUnder18Type );
				
				UcrOffenseCodeType ucrOffenseCodeType = codeTableService.getCodeTableType(
						arrestee.getUcrArrestOffenseCode(), 
						ucrOffenseCodeTypeRepository::findFirstByUcrOffenseCode, 
						UcrOffenseCodeType::new);;
				arresteeSegment.setUcrOffenseCodeType(ucrOffenseCodeType);
	
				processArrestReportSegmentArmedWiths(arresteeSegment, arrestee);
				arresteeSegments.add(arresteeSegment);
			}
			
			administrativeSegment.setArresteeSegments(arresteeSegments);
		}
	}

	private void processOffenders(AdministrativeSegment administrativeSegment,
			GroupAIncidentReport groupAIncidentReport) {
		if (groupAIncidentReport.getOffenderCount() > 0){
			Set<OffenderSegment> offenderSegments = Optional.ofNullable(administrativeSegment.getOffenderSegments())
					.orElseGet(HashSet::new);
			offenderSegments.clear();

			for (org.search.nibrs.model.OffenderSegment offender: groupAIncidentReport.getOffenders()){
				OffenderSegment offenderSegment = new OffenderSegment();
				offenderSegment.setSegmentActionType(administrativeSegment.getSegmentActionType());
				offenderSegment.setAdministrativeSegment(administrativeSegment);
				
				Optional<NIBRSAge> offenderAge = Optional.ofNullable(offender.getAge());
				offenderSegment.setAgeOfOffenderMax(offenderAge.map(NIBRSAge::getAgeMax).orElse(null));
				offenderSegment.setAgeOfOffenderMin(offenderAge.map(NIBRSAge::getAgeMin).orElse(null));
				offenderSegment.setOffenderSequenceNumber(offender.getOffenderSequenceNumber().getValue());
				
				SexOfPersonType sexOfPersonType = codeTableService.getCodeTableType(
						offender.getSex(), sexOfPersonTypeRepository::findFirstBySexOfPersonCode, SexOfPersonType::new);
				offenderSegment.setSexOfPersonType(sexOfPersonType);
				
				RaceOfPersonType raceOfPersonType = codeTableService.getCodeTableType(
						offender.getRace(), raceOfPersonTypeRepository::findFirstByRaceOfPersonCode, RaceOfPersonType::new);
				offenderSegment.setRaceOfPersonType(raceOfPersonType);
				
				EthnicityOfPersonType ethnicityOfPersonType = codeTableService.getCodeTableType(
						offender.getEthnicity(), ethnicityOfPersonTypeRepository::findFirstByEthnicityOfPersonCode, EthnicityOfPersonType::new);
				offenderSegment.setEthnicityOfPersonType(ethnicityOfPersonType);

				offenderSegments.add(offenderSegment);
			}
			
			administrativeSegment.setOffenderSegments(offenderSegments);
		}
	}

	private void processOffenses(AdministrativeSegment administrativeSegment, GroupAIncidentReport groupAIncidentReport) {
		if (groupAIncidentReport.getOffenseCount() > 0){

			Set<OffenseSegment> offenseSegments = Optional.ofNullable(administrativeSegment.getOffenseSegments())
					.orElseGet(HashSet::new);
			offenseSegments.clear();
			
			for(org.search.nibrs.model.OffenseSegment offense: groupAIncidentReport.getOffenses()){
				OffenseSegment offenseSegment = new OffenseSegment();
				offenseSegment.setSegmentActionType(administrativeSegment.getSegmentActionType());
				offenseSegment.setAdministrativeSegment(administrativeSegment);
				
				UcrOffenseCodeType ucrOffenseCodeType = 
						codeTableService.getCodeTableType(offense.getUcrOffenseCode(), 
								ucrOffenseCodeTypeRepository::findFirstByUcrOffenseCode, UcrOffenseCodeType::new);
				offenseSegment.setUcrOffenseCodeType(ucrOffenseCodeType);
				offenseSegment.setOffenseAttemptedCompleted(offense.getOffenseAttemptedCompleted());
				
				LocationType locationType = 
						codeTableService.getCodeTableType(offense.getLocationType(), 
								locationTypeRepository::findFirstByLocationTypeCode, LocationType::new);
				offenseSegment.setLocationType(locationType);
				
				offenseSegment.setNumberOfPremisesEntered(offense.getNumberOfPremisesEntered().getValue());
				
				MethodOfEntryType methodOfEntryType = 
						codeTableService.getCodeTableType(offense.getMethodOfEntry(), 
								methodOfEntryTypeRepository::findFirstByMethodOfEntryCode, MethodOfEntryType::new);
				offenseSegment.setMethodOfEntryType(methodOfEntryType);
				processTypeOfWeaponForceInvolved(offenseSegment, offense); 
				processTypeOfCriminalActivityCount(offenseSegment, offense); 
				processOffendersSuspectedOfUsing(offenseSegment, offense);
				
				Set<BiasMotivationType> biasMotivationTypes = new HashSet<>();
				Arrays.stream(offense.getBiasMotivation())
					.filter(StringUtils::isNotBlank)
					.map(code -> codeTableService.getCodeTableType(code, 
								biasMotivationTypeRepository::findFirstByBiasMotivationCode, null))
					.filter(Objects::nonNull)
					.forEach(biasMotivationTypes::add);
					
				offenseSegment.setBiasMotivationTypes(biasMotivationTypes);
				
				offenseSegments.add(offenseSegment);
			}
			administrativeSegment.setOffenseSegments(offenseSegments);
		}
		
	}

	private void processOffendersSuspectedOfUsing(OffenseSegment offenseSegment,
			org.search.nibrs.model.OffenseSegment offense) {
		if (offense.getPopulatedOffendersSuspectedOfUsingCount() > 0){
			Set<OffenderSuspectedOfUsingType> offenderSuspectedOfUsingTypes = new HashSet<>(); 
			
			for (int i = 0; i < offense.getPopulatedOffendersSuspectedOfUsingCount(); i++){
				String offenderSuspectedUsingCode = StringUtils.trimToNull(offense.getOffendersSuspectedOfUsing(i));
				OffenderSuspectedOfUsingType offenderSuspectedOfUsingType = 
						codeTableService.getCodeTableType(offenderSuspectedUsingCode, offenderSuspectedOfUsingTypeRepository::findFirstByOffenderSuspectedOfUsingCode, null);
				if (offenderSuspectedOfUsingType != null){
					offenderSuspectedOfUsingTypes.add(offenderSuspectedOfUsingType); 
				}
			}
			offenseSegment.setOffenderSuspectedOfUsingTypes(offenderSuspectedOfUsingTypes);
		}
	}

	private void processTypeOfCriminalActivityCount(OffenseSegment offenseSegment,
			org.search.nibrs.model.OffenseSegment offense) {
		if (offense.getPopulatedTypeOfCriminalActivityCount() > 0){
			Set<TypeOfCriminalActivityType> typeOfCriminalActivityTypes = new HashSet<>(); 
			
			for (int i = 0; i < offense.getPopulatedTypeOfCriminalActivityCount(); i++){
				String typeOfCriminalActivityCode = StringUtils.trimToNull(offense.getTypeOfCriminalActivity(i));
				TypeOfCriminalActivityType typeOfCriminalActivityType = 
						codeTableService.getCodeTableType(typeOfCriminalActivityCode, typeOfCriminalActivityTypeRepository::findFirstByTypeOfCriminalActivityCode, null);
				if (typeOfCriminalActivityType != null){
					typeOfCriminalActivityTypes.add(typeOfCriminalActivityType); 
				}
			}
			offenseSegment.setTypeOfCriminalActivityTypes(typeOfCriminalActivityTypes);
		}
	}

	private void processTypeOfWeaponForceInvolved(OffenseSegment offenseSegment,
			org.search.nibrs.model.OffenseSegment offense) {
		Set<TypeOfWeaponForceInvolved> typeOfWeaponForceInvolveds = new HashSet<>(); 
		if (offense.getPopulatedTypeOfWeaponForceInvolvedCount() > 0){
			for (int i = 0; i < offense.getPopulatedTypeOfWeaponForceInvolvedCount(); i++){
				String typeOfWeaponForceInvolvedCode = 
						StringUtils.trimToNull(offense.getTypeOfWeaponForceInvolved(i));
				String automaticWeaponIndicator = 
						StringUtils.trimToEmpty(offense.getAutomaticWeaponIndicator(i));
				
				if (StringUtils.isNotBlank(typeOfWeaponForceInvolvedCode)){
					Optional<TypeOfWeaponForceInvolvedType> typeOfWeaponForceInvolvedType = 
							Optional.ofNullable(codeTableService.getCodeTableType(typeOfWeaponForceInvolvedCode,
									typeOfWeaponForceInvolvedTypeRepository::findFirstByTypeOfWeaponForceInvolvedCode, 
									null));
					typeOfWeaponForceInvolvedType.ifPresent( type ->
						typeOfWeaponForceInvolveds.add(new TypeOfWeaponForceInvolved(
								offenseSegment, type, automaticWeaponIndicator))
					);
				}
			}
		}
		
		if (!typeOfWeaponForceInvolveds.isEmpty()){
			offenseSegment.setTypeOfWeaponForceInvolveds(typeOfWeaponForceInvolveds);
		}
	}

}
