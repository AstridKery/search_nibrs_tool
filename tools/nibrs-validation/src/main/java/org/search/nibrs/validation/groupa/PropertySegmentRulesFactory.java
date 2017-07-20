/*
 * Copyright 2016 Research Triangle Institute
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
package org.search.nibrs.validation.groupa;

import static org.search.nibrs.util.ArrayUtils.allNull;
import static org.search.nibrs.util.ArrayUtils.notAllNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.search.nibrs.common.NIBRSError;
import org.search.nibrs.common.ParsedObject;
import org.search.nibrs.common.ValidationTarget;
import org.search.nibrs.model.GroupAIncidentReport;
import org.search.nibrs.model.OffenseSegment;
import org.search.nibrs.model.PropertySegment;
import org.search.nibrs.model.codes.NIBRSErrorCode;
import org.search.nibrs.model.codes.OffenseAttemptedCompletedCode;
import org.search.nibrs.model.codes.OffenseCode;
import org.search.nibrs.model.codes.PropertyDescriptionCode;
import org.search.nibrs.model.codes.SuspectedDrugTypeCode;
import org.search.nibrs.model.codes.TypeOfDrugMeasurementCode;
import org.search.nibrs.model.codes.TypeOfPropertyLossCode;
import org.search.nibrs.validation.rules.DuplicateCodedValueRule;
import org.search.nibrs.validation.rules.NumericValueRule;
import org.search.nibrs.validation.rules.Rule;
import org.search.nibrs.validation.rules.ValidValueListRule;

public class PropertySegmentRulesFactory {
	
	private static abstract class StolenMotorVehiclesRule implements Rule<PropertySegment> {
		private boolean stolenMode = false;
		public StolenMotorVehiclesRule(boolean stolenMode) {
			this.stolenMode = stolenMode;
		}
		@Override
		public NIBRSError apply(PropertySegment subject) {
			NIBRSError ret = null;
			Integer smv = stolenMode ? subject.getNumberOfStolenMotorVehicles().getValue() : subject.getNumberOfRecoveredMotorVehicles().getValue();
			boolean nmvNull = (smv == null);
			boolean mvOffenseInvolved = false;
			boolean offenseAttempted = false;
			for (OffenseSegment os : ((GroupAIncidentReport) subject.getParentReport()).getOffenses()) {
				if (OffenseCode._240.code.equals(os.getUcrOffenseCode())) {
					mvOffenseInvolved = true;
					
					if (OffenseAttemptedCompletedCode.A.code.equals(os.getOffenseAttemptedCompleted())) {
						offenseAttempted = true;
					}
				}
			}
			String typeOfPropertyLoss = subject.getTypeOfPropertyLoss();
			ret = evaluateRule(nmvNull, mvOffenseInvolved, offenseAttempted, typeOfPropertyLoss, subject);
			return ret;
		}
		
		protected abstract NIBRSError evaluateRule(boolean nmvNull, boolean smvOffenseInvolved, boolean offenseAttempted, String typeOfPropertyLoss, PropertySegment subject);

	}
	
	private static abstract class Rule364 implements Rule<PropertySegment> {
		private String dataElementIdentifier;
		public Rule364(String dataElementIdentifier) {
			this.dataElementIdentifier = dataElementIdentifier;
		}
		@Override
		public NIBRSError apply(PropertySegment subject) {
			NIBRSError ret = null;
			GroupAIncidentReport parent = (GroupAIncidentReport) subject.getParentReport();
			boolean drugOffense = parent.isOffenseInvolved(OffenseCode._35A);
			
			String typePropertyLoss = subject.getTypeOfPropertyLoss();
			if (drugOffense && typePropertyLoss != null && TypeOfPropertyLossCode._6.code.equals(typePropertyLoss)) {
				if (evaluateProperty(subject)) {
					ret = subject.getErrorTemplate();
					ret.setNIBRSErrorCode(NIBRSErrorCode._364);
					ret.setValue(null);
					ret.setDataElementIdentifier(dataElementIdentifier);
				}
			}
			return ret;
		}
		private boolean evaluateProperty(PropertySegment segment) {
			boolean ret = false;
			for (int i=0;i < 10;i++) {
				if (PropertyDescriptionCode._10.code.equals(segment.getPropertyDescription(i)) && notAllNull(segment.getSuspectedDrugType())) {
					if (allNull(getDrugElementArray(segment))) {
						ret = true;
						break;
					}
				}
			}
			return ret;
		}
		protected abstract Object[] getDrugElementArray(PropertySegment segment);
	}

	@SuppressWarnings("unused")
	private static final Logger LOG = LogManager.getLogger(PropertySegmentRulesFactory.class);
	
	private List<Rule<PropertySegment>> rulesList = new ArrayList<>();
	private Set<String> zeroValuePropertyDescriptions = new HashSet<>();
	
	public PropertySegmentRulesFactory() {
		
		zeroValuePropertyDescriptions.add(PropertyDescriptionCode._09.code);
		zeroValuePropertyDescriptions.add(PropertyDescriptionCode._22.code);
		zeroValuePropertyDescriptions.add(PropertyDescriptionCode._48.code);
		zeroValuePropertyDescriptions.add(PropertyDescriptionCode._65.code);
		zeroValuePropertyDescriptions.add(PropertyDescriptionCode._66.code);
		
		rulesList.add(getRule301ForSuspectedDrugType());
		rulesList.add(getRule304ForTypePropertyLoss());
		rulesList.add(getRule304ForListBoundElement("propertyDescription", "15", PropertyDescriptionCode.codeSet()));
		rulesList.add(getRule304ForListBoundElement("suspectedDrugType", "20", SuspectedDrugTypeCode.codeSet()));
		rulesList.add(getRule304ForListBoundElement("typeDrugMeasurement", "22", TypeOfDrugMeasurementCode.codeSet()));
		rulesList.add(getRule304ForStolenMotorVehicleCount());
		rulesList.add(getRule304ForRecoveredMotorVehicleCount());
		rulesList.add(getRule304ForDrugQuantity());
		rulesList.add(getRule304ForPropertyValue());
		rulesList.add(getRule305());
		rulesList.add(getRule306());
		rulesList.add(getRule306ForPropertyDescriptions());
		rulesList.add(getRule342());
		rulesList.add(getRule351());
		rulesList.add(getRule352());
		rulesList.add(getRule353());
		rulesList.add(getRule354());
		rulesList.add(getRule355());
		rulesList.add(getRule357());
		rulesList.add(getRule358());
		rulesList.add(getRule359());
		rulesList.add(getRule360());
		rulesList.add(getRule361());
		rulesList.add(getRule362());
		rulesList.add(getRule363forQuantity());
		rulesList.add(getRule363forMeasurement());
		rulesList.add(getRule364forQuantity());
		rulesList.add(getRule364forMeasurement());
		rulesList.add(getRule365());
		rulesList.add(getRule367());
		rulesList.add(getRule387());
		rulesList.add(getRule388());
		rulesList.add(getRule390());
		rulesList.add(getRule391());
		rulesList.add(getRule392());
		
	}
	
	private Rule<PropertySegment> getRule301ForSuspectedDrugType() {
		return new Rule<PropertySegment>() {
			@Override
			public NIBRSError apply(PropertySegment subject) {
				NIBRSError ret = null;
				for (int i=0;i < 3;i++) {
					Double drugQuantity = subject.getEstimatedDrugQuantity(i);
					if (drugQuantity != null) {
						double d = drugQuantity.doubleValue();
						if (d < 0 || d > 100000000) {
							ret = subject.getErrorTemplate();
							ret.setDataElementIdentifier("20");
							ret.setNIBRSErrorCode(NIBRSErrorCode._301);
						}
					}
				}
				return ret;
			}
		};
	}

	Rule<PropertySegment> getRule367() {
		Set<String> allowedCodes = new HashSet<>();
		allowedCodes.add(SuspectedDrugTypeCode._E.code);
		allowedCodes.add(SuspectedDrugTypeCode._G.code);
		allowedCodes.add(SuspectedDrugTypeCode._K.code);
		return new Rule<PropertySegment>() {
			@Override
			public NIBRSError apply(PropertySegment subject) {
				NIBRSError ret = null;
				for (int i=0;i < 3;i++) {
					String drugType = subject.getSuspectedDrugType(i);
					String measurement = subject.getTypeDrugMeasurement(i);
					if (TypeOfDrugMeasurementCode._NP.code.equals(measurement) && !allowedCodes.contains(drugType)) {
						ret = subject.getErrorTemplate();
						ret.setNIBRSErrorCode(NIBRSErrorCode._367);
						ret.setValue(drugType);
						ret.setDataElementIdentifier("22");
						break;
					}
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule364forMeasurement() {
		return new Rule364("22") {
			@Override
			protected Object[] getDrugElementArray(PropertySegment segment) {
				return segment.getTypeDrugMeasurement();
			}

		};
	}
	
	Rule<PropertySegment> getRule364forQuantity() {
		return new Rule364("21") {
			@Override
			protected Object[] getDrugElementArray(PropertySegment segment) {
				return segment.getEstimatedDrugQuantity();
			}

		};
	}
	
	Rule<PropertySegment> getRule365() {
		return new Rule<PropertySegment>() {
			@Override
			public NIBRSError apply(PropertySegment subject) {
				NIBRSError ret = null;
				if ( notAllNull(subject.getSuspectedDrugType())) {
					GroupAIncidentReport parent = (GroupAIncidentReport) subject.getParentReport();
					boolean drugOffense = parent.getOffenses().stream()
							.anyMatch(offense->OffenseCode._35A.code.equals(offense.getUcrOffenseCode()));
							
					String typePropertyLoss = subject.getTypeOfPropertyLoss();
					if ( ! ( drugOffense && typePropertyLoss != null 
							&& (TypeOfPropertyLossCode._1.code.equals(typePropertyLoss)
								|| (TypeOfPropertyLossCode._6.code.equals(typePropertyLoss)
										&& subject.getPropertyDescriptionList().contains(PropertyDescriptionCode._10.code))))) {
						ret = subject.getErrorTemplate();
						ret.setDataElementIdentifier("20");
						
						Optional<String> value = Arrays.stream(subject.getSuspectedDrugType())
								.filter(Objects::nonNull)
								.reduce(String::concat);
						ret.setValue(value.get());
						ret.setNIBRSErrorCode(NIBRSErrorCode._365);
					}
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule363forMeasurement() {
		return new Rule<PropertySegment>() {
			@Override
			public NIBRSError apply(PropertySegment subject) {
				NIBRSError ret = null;
				for (int i=0;i < 3;i++) {
					if (SuspectedDrugTypeCode._X.code.equals(subject.getSuspectedDrugType(i)) && subject.getTypeDrugMeasurement(i) != null) {
						ret = subject.getErrorTemplate();
						ret.setNIBRSErrorCode(NIBRSErrorCode._363);
						ret.setValue(subject.getTypeDrugMeasurement(i));
						ret.setDataElementIdentifier("22");
						break;
					}
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule363forQuantity() {
		return new Rule<PropertySegment>() {
			@Override
			public NIBRSError apply(PropertySegment subject) {
				NIBRSError ret = null;
				for (int i=0;i < 3;i++) {
					if (SuspectedDrugTypeCode._X.code.equals(subject.getSuspectedDrugType(i)) && subject.getEstimatedDrugQuantity(i) != null) {
						ret = subject.getErrorTemplate();
						ret.setNIBRSErrorCode(NIBRSErrorCode._363);
						ret.setValue(subject.getEstimatedDrugQuantity(i));
						ret.setDataElementIdentifier("21");
						break;
					}
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule362() {
		return new Rule<PropertySegment>() {
			@Override
			public NIBRSError apply(PropertySegment subject) {
				NIBRSError ret = null;
				String[] suspectedDrugTypes = subject.getSuspectedDrugType();
				int notNullCount = 0;
				boolean xEntered = false;
				for (int i=0;i < suspectedDrugTypes.length;i++) {
					String suspectedDrugType = subject.getSuspectedDrugType(i);
					if (suspectedDrugType != null) {
						notNullCount++;
						if (SuspectedDrugTypeCode._X.code.equals(suspectedDrugType)) {
							xEntered = true;
						}
					}
				}
				if (xEntered && notNullCount != 3) {
					ret = subject.getErrorTemplate();
					ret.setNIBRSErrorCode(NIBRSErrorCode._362);
					ret.setValue(suspectedDrugTypes);
					ret.setDataElementIdentifier("20");
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule359() {
		Set<String> allowedPropertyDescriptions = new HashSet<>();
		allowedPropertyDescriptions.add(PropertyDescriptionCode._03.code);
		allowedPropertyDescriptions.add(PropertyDescriptionCode._05.code);
		allowedPropertyDescriptions.add(PropertyDescriptionCode._24.code);
		allowedPropertyDescriptions.add(PropertyDescriptionCode._28.code);
		allowedPropertyDescriptions.add(PropertyDescriptionCode._37.code);
		return new Rule<PropertySegment>() {
			@Override
			public NIBRSError apply(PropertySegment subject) {
				NIBRSError ret = null;
				Integer smv = subject.getNumberOfStolenMotorVehicles().getValue();
				Integer rmv = subject.getNumberOfRecoveredMotorVehicles().getValue();
				if ((smv != null && smv > 0) || (rmv != null && rmv > 0)) {
					boolean found = false;
					for (int i=0;i < 10 && !found;i++) {
						String pd = subject.getPropertyDescription(i);
						found = pd != null && allowedPropertyDescriptions.contains(pd);
					}
					if (!found) {
						ret = subject.getErrorTemplate();
						ret.setValue((smv != null && smv > 0)?smv:rmv);
						ret.setNIBRSErrorCode(NIBRSErrorCode._359);
						ret.setDataElementIdentifier("18");
					}
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule390() {
		return new Rule<PropertySegment>() {
			@Override
			public NIBRSError apply(PropertySegment subject) {
				NIBRSError ret = null;
				
				List<OffenseSegment> offenses = ((GroupAIncidentReport)subject.getParentReport()).getOffenses();
				
				List<String> offensesRequirePropertySegment = 
						offenses.stream()
							.map(item->item.getUcrOffenseCode())
							.filter(OffenseCode::isCrimeRequirePropertySegement)
							.collect(Collectors.toList()) ;
						
				if ( offensesRequirePropertySegment.size() == 0 ) 
					return ret; 
				
				boolean containsOffensesWithoutIllogicalPropertyDescriptions = 
						offensesRequirePropertySegment.stream()
						.filter(offenseCode->!OffenseCode.isOffenseHavingIllogicalPropertyDescriptions(offenseCode))
						.count() > 0 ; 
				if (containsOffensesWithoutIllogicalPropertyDescriptions || TypeOfPropertyLossCode.noneOrUnknownValueCodeSet().contains(subject.getTypeOfPropertyLoss())){
					return ret; 
				}

				List<String> existingDescriptions = 
						Arrays.stream(subject.getPropertyDescription())
						.filter(Objects::nonNull)
						.collect(Collectors.toList()); 
				boolean isLogicalPropertyDescription = false;
				for (String offenseCode: offensesRequirePropertySegment) {
					
					List<String> illogicalPropertyDescriptions = 
							PropertyDescriptionCode.getIllogicalPropertyDescriptions(offenseCode);
					
//					existingDescriptions.removeAll(illogicalPropertyDescriptions);
					
					if (!illogicalPropertyDescriptions.containsAll(existingDescriptions)) {
						isLogicalPropertyDescription = true; 
						break;
					}
						
				}
				
				if (!isLogicalPropertyDescription){
					ret = subject.getErrorTemplate();
					ret.setValue(null);
					ret.setNIBRSErrorCode(NIBRSErrorCode._390);
					ret.setDataElementIdentifier("15");
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule361() {
		return new StolenMotorVehiclesRule(false) {
			protected NIBRSError evaluateRule(boolean nmvNull, boolean mvOffenseInvolved, boolean offenseAttempted, String typeOfPropertyLoss, PropertySegment subject) {
				
				boolean completeMvOffenseInvolved = ((GroupAIncidentReport) subject.getParentReport()).getOffenses()
						.stream()
						.anyMatch(offense-> OffenseCode._240.code.equals(offense.getUcrOffenseCode()) 
								&& OffenseAttemptedCompletedCode.C.code.equals(offense.getOffenseAttemptedCompleted()));
						
				NIBRSError ret = null;
				if (nmvNull && ((typeOfPropertyLoss != null 
						&& TypeOfPropertyLossCode._5.code.equals(typeOfPropertyLoss)) 
						&& completeMvOffenseInvolved
						&& subject.containsVehiclePropertyCodes())) {
					ret = subject.getErrorTemplate();
					ret.setValue(subject.getNumberOfStolenMotorVehicles());
					ret.setNIBRSErrorCode(NIBRSErrorCode._361);
					ret.setDataElementIdentifier("19");
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule358() {
		return new StolenMotorVehiclesRule(true) {
			protected NIBRSError evaluateRule(boolean nmvNull, boolean mvOffenseInvolved, boolean offenseAttempted, String typeOfPropertyLoss, PropertySegment subject) {
				NIBRSError ret = null;
				if (nmvNull && ((typeOfPropertyLoss != null && TypeOfPropertyLossCode._7.code.equals(typeOfPropertyLoss)) && mvOffenseInvolved && !offenseAttempted)) {
					ret = subject.getErrorTemplate();
					ret.setValue(subject.getNumberOfStolenMotorVehicles());
					ret.setNIBRSErrorCode(NIBRSErrorCode._358);
					ret.setDataElementIdentifier("18");
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule360() {
		return new StolenMotorVehiclesRule(false) {
			protected NIBRSError evaluateRule(boolean nmvNull, boolean mvOffenseInvolved, boolean offenseAttempted, String typeOfPropertyLoss, PropertySegment subject) {
				NIBRSError ret = null;
				if (!nmvNull && ((typeOfPropertyLoss != null && !TypeOfPropertyLossCode._5.code.equals(typeOfPropertyLoss)) || !mvOffenseInvolved || offenseAttempted)) {
					ret = subject.getErrorTemplate();
					ret.setValue(subject.getNumberOfRecoveredMotorVehicles());
					ret.setNIBRSErrorCode(NIBRSErrorCode._360);
					ret.setDataElementIdentifier("19");
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule357() {
		return new StolenMotorVehiclesRule(true) {
			protected NIBRSError evaluateRule(boolean nmvNull, boolean mvOffenseInvolved, boolean offenseAttempted, String typeOfPropertyLoss, PropertySegment subject) {
				NIBRSError ret = null;
				if (!nmvNull && ((typeOfPropertyLoss != null && !TypeOfPropertyLossCode._7.code.equals(typeOfPropertyLoss)) || !mvOffenseInvolved || offenseAttempted)) {
					ret = subject.getErrorTemplate();
					ret.setValue(subject.getNumberOfStolenMotorVehicles());
					ret.setNIBRSErrorCode(NIBRSErrorCode._357);
					ret.setDataElementIdentifier("18");
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule355() {
		return new Rule<PropertySegment>() {
			@Override
			public NIBRSError apply(PropertySegment subject) {
				NIBRSError ret = null;
				String typeOfPropertyLoss = subject.getTypeOfPropertyLoss();
				if (typeOfPropertyLoss != null && !TypeOfPropertyLossCode._5.code.equals(typeOfPropertyLoss)) {
					for (int i=0;i < 10;i++) {
						ParsedObject<Date> dateRecoveredPO = subject.getDateRecovered(i);
						if (dateRecoveredPO.getValue() != null) {
							ret = subject.getErrorTemplate();
							ret.setDataElementIdentifier("17");
							ret.setValue(dateRecoveredPO.getValue());
							ret.setNIBRSErrorCode(NIBRSErrorCode._355);
						}
					}
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule354() {
		return new Rule<PropertySegment>() {
			@Override
			public NIBRSError apply(PropertySegment subject) {
				NIBRSError ret = null;
				for (int i=0;i < 10;i++) {
					if (subject.getValueOfProperty(i).getValue() != null && 
							subject.getPropertyDescription(i) == null) {
						ret = subject.getErrorTemplate();
						ret.setNIBRSErrorCode(NIBRSErrorCode._354);
						ret.setValue(subject.getValueOfProperty(i).getValue());
						ret.setDataElementIdentifier("15");
						break;
					}
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule353() {
		return new Rule<PropertySegment>() {
			@Override
			public NIBRSError apply(PropertySegment subject) {
				NIBRSError ret = null;
				for (int i=0;i < 10;i++) {
					ParsedObject<Integer> valueOfPropertyPO = subject.getValueOfProperty(i);
					String propertyDescription = subject.getPropertyDescription(i);
					if (propertyDescription != null && PropertyDescriptionCode._88.code.equals(propertyDescription) &&
							!(valueOfPropertyPO.isMissing() || valueOfPropertyPO.isInvalid()) && valueOfPropertyPO.getValue() != 1) {
						ret = subject.getErrorTemplate();
						ret.setNIBRSErrorCode(NIBRSErrorCode._353);
						ret.setValue(valueOfPropertyPO.getValue());
						ret.setDataElementIdentifier("16");
						break;
					}
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule352() {
		return new Rule<PropertySegment>() {
			@Override
			public NIBRSError apply(PropertySegment subject) {
				NIBRSError ret = null;
				String loss = subject.getTypeOfPropertyLoss();
				boolean drugOffense35AInvolved = ((GroupAIncidentReport) subject.getParentReport()).isOffenseInvolved(OffenseCode._35A);
				boolean otherOffenseRequirePropertySegment = ((GroupAIncidentReport) subject.getParentReport())
						.containsOtherCrimeRequirePropertySegment(OffenseCode._35A.code);
				boolean isRule392Exception2 = ((GroupAIncidentReport) subject.getParentReport()).isRule392Exception2();
				
				Object value = null;
				if ((TypeOfPropertyLossCode._8.code.equals(loss) || (TypeOfPropertyLossCode._1.code.equals(loss) && !drugOffense35AInvolved)) &&
						!(allNull(subject.getPropertyDescription()) &&
								allNull(subject.getValueOfProperty()) &&
								allNull(subject.getDateRecovered()) &&
								(subject.getNumberOfRecoveredMotorVehicles().getValue() == null) &&
								(subject.getNumberOfStolenMotorVehicles().getValue() == null) &&
								allNull(subject.getSuspectedDrugType()) &&
								allNull(subject.getEstimatedDrugQuantity()) &&
								allNull(subject.getTypeDrugMeasurement())
								)) {
					value = loss; // not the best, but will work for now
				} else if (TypeOfPropertyLossCode._1.code.equals(loss) && drugOffense35AInvolved 
						&& !otherOffenseRequirePropertySegment && !isRule392Exception2 
						&& !(allNull(subject.getPropertyDescription()) &&
							allNull(subject.getValueOfProperty()) &&
							allNull(subject.getDateRecovered()) &&
							(subject.getNumberOfRecoveredMotorVehicles().getValue() == null) &&
							(subject.getNumberOfStolenMotorVehicles().getValue() == null) &&
							notAllNull(subject.getSuspectedDrugType())
						)) {
					value = loss;
				}
				if (value != null) {
					ret = subject.getErrorTemplate();
					ret.setValue(value);
					ret.setNIBRSErrorCode(NIBRSErrorCode._352);
					ret.setDataElementIdentifier("14");
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule392() {
		return new Rule<PropertySegment>() {
			@Override
			public NIBRSError apply(PropertySegment subject) {
				NIBRSError ret = null;
				String loss = subject.getTypeOfPropertyLoss();
				boolean drugOffense35AInvolved = ((GroupAIncidentReport) subject.getParentReport()).isOffenseInvolved(OffenseCode._35A);
				boolean otherOffenseRequirePropertySegment = ((GroupAIncidentReport) subject.getParentReport())
						.containsOtherCrimeRequirePropertySegment(OffenseCode._35A.code);
				boolean isRule392Exception2 = ((GroupAIncidentReport) subject.getParentReport()).isRule392Exception2();
				
				if (TypeOfPropertyLossCode._1.code.equals(loss) 
						&& drugOffense35AInvolved 
						&& !otherOffenseRequirePropertySegment 
						&& !isRule392Exception2 
						&& allNull(subject.getSuspectedDrugType())){
					ret = subject.getErrorTemplate();
					ret.setNIBRSErrorCode(NIBRSErrorCode._392);
					ret.setDataElementIdentifier("20");
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule387() {
		return new Rule<PropertySegment>() {
			@Override
			public NIBRSError apply(PropertySegment subject) {
				NIBRSError ret = null;
				String loss = subject.getTypeOfPropertyLoss();
				boolean drugOffense35AInvolved = ((GroupAIncidentReport) subject.getParentReport()).isOffenseInvolved(OffenseCode._35A);
				boolean drugOffense35BInvolved = ((GroupAIncidentReport) subject.getParentReport()).isOffenseInvolved(OffenseCode._35B);
				boolean otherOffenseRequirePropertySegment = ((GroupAIncidentReport) subject.getParentReport())
						.containsOtherCrimeRequirePropertySegment(OffenseCode._35A.code, OffenseCode._35B.code);
				boolean containsPropertyDescription11 = subject.containsPropertyDescription(PropertyDescriptionCode._11.code);
				boolean containsPropertyDescription10 = subject.containsPropertyDescription(PropertyDescriptionCode._10.code);
				
				if (TypeOfPropertyLossCode._6.code.equals(loss) 
						&& drugOffense35AInvolved 
						&& !drugOffense35BInvolved
						&& !otherOffenseRequirePropertySegment 
						&& containsPropertyDescription11){
					ret = subject.getErrorTemplate();
					ret.setValue(PropertyDescriptionCode._11.code);
					ret.setNIBRSErrorCode(NIBRSErrorCode._387);
					ret.setDataElementIdentifier("15");
				}
				
				if (TypeOfPropertyLossCode._6.code.equals(loss) 
						&& !drugOffense35AInvolved 
						&& drugOffense35BInvolved
						&& !otherOffenseRequirePropertySegment 
						&& containsPropertyDescription10){
					ret = subject.getErrorTemplate();
					ret.setValue(PropertyDescriptionCode._10.code);
					ret.setNIBRSErrorCode(NIBRSErrorCode._387);
					ret.setDataElementIdentifier("15");
				}

				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule391() {
		return new Rule<PropertySegment>() {
			@Override
			public NIBRSError apply(PropertySegment subject) {
				NIBRSError ret = null;
				for (int i=0;i < 10;i++) {
					ParsedObject<Integer> valueOfPropertyPO = subject.getValueOfProperty(i);
					String propertyDescription = subject.getPropertyDescription(i);
					if (!(valueOfPropertyPO.isMissing() || valueOfPropertyPO.isInvalid()) && valueOfPropertyPO.getValue() != 0 && zeroValuePropertyDescriptions.contains(propertyDescription)) {
						ret = subject.getErrorTemplate();
						ret.setNIBRSErrorCode(NIBRSErrorCode._391);
						ret.setValue(valueOfPropertyPO.getValue());
						ret.setDataElementIdentifier("16");
					}
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule388() {
		return new Rule<PropertySegment>() {
			@Override
			public NIBRSError apply(PropertySegment subject) {
				NIBRSError ret = null;
				
				List<String> vehicleValues = Arrays.asList(
						PropertyDescriptionCode._03.code, 
						PropertyDescriptionCode._05.code, 
						PropertyDescriptionCode._24.code, 
						PropertyDescriptionCode._28.code, 
						PropertyDescriptionCode._37.code);
				long countOfVehicles = Arrays.stream(subject.getPropertyDescription())
						.filter(item->vehicleValues.contains(item))
						.count(); 
				if ( !subject.getNumberOfStolenMotorVehicles().isMissing()
						&& !subject.getNumberOfStolenMotorVehicles().isInvalid()
						&& subject.getNumberOfStolenMotorVehicles().getValue() != null 
						&& (subject.getNumberOfStolenMotorVehicles().getValue() != 0 )
						&& countOfVehicles > subject.getNumberOfStolenMotorVehicles().getValue()) {
					ret = subject.getErrorTemplate();
					ret.setNIBRSErrorCode(NIBRSErrorCode._388);
					ret.setValue(subject.getNumberOfStolenMotorVehicles());
					ret.setDataElementIdentifier("18");
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule351() {
		Set<String> allowedZeroValue = new HashSet<>();
		allowedZeroValue.addAll(zeroValuePropertyDescriptions);
		allowedZeroValue.add(PropertyDescriptionCode._77.code);
		allowedZeroValue.add(PropertyDescriptionCode._99.code);
		return new Rule<PropertySegment>() {
			@Override
			public NIBRSError apply(PropertySegment subject) {
				NIBRSError ret = null;
				for (int i=0;i < 10;i++) {
					ParsedObject<Integer> valueOfPropertyPO = subject.getValueOfProperty(i);
					String propertyDescription = subject.getPropertyDescription(i);
					if (!(valueOfPropertyPO.isMissing() || valueOfPropertyPO.isInvalid()) && valueOfPropertyPO.getValue() == 0 && !allowedZeroValue.contains(propertyDescription)) {
						ret = subject.getErrorTemplate();
						ret.setNIBRSErrorCode(NIBRSErrorCode._351);
						ret.setValue(valueOfPropertyPO.getValue());
						ret.setDataElementIdentifier("16");
					}
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule342() {
		return new Rule<PropertySegment>() {
			@Override
			public NIBRSError apply(PropertySegment subject) {
				NIBRSError ret = null;
				List<String> value = Arrays.stream(subject.getValueOfProperty())
						.filter(item-> item.getValue() != null && 1000000 <= item.getValue())
						.map(item -> String.valueOf(item.getValue()))
						.map(item -> StringUtils.leftPad(item, 9, '0'))
						.collect(Collectors.toList());
				if ( value != null && value.size() > 0) {
					ret = subject.getErrorTemplate();
					ret.setWarning(true);
					ret.setNIBRSErrorCode(NIBRSErrorCode._342);
					ret.setValue(value);
					ret.setDataElementIdentifier("16");
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule306() {
		return new Rule<PropertySegment>() {
			@Override
			public NIBRSError apply(PropertySegment subject) {
				NIBRSError ret = null;
				String[] suspectedDrugType = subject.getSuspectedDrugType();
				String[] typeDrugMeasurement = subject.getTypeDrugMeasurement();
				if (suspectedDrugType != null && typeDrugMeasurement != null && suspectedDrugType.length == typeDrugMeasurement.length) {
					Map<String, String> typeToMeasureMap = new HashMap<>();
					for (int i=0;i < suspectedDrugType.length;i++) {
						String t = suspectedDrugType[i];
						String m = typeDrugMeasurement[i];
						if (t != null && m != null) {
							if (typeToMeasureMap.keySet().contains(t) && (SuspectedDrugTypeCode._U.code.equals(t) || m.equals(typeToMeasureMap.get(t)))) {
								ret = subject.getErrorTemplate();
								ret.setDataElementIdentifier("20");
								ret.setValue(t);
								ret.setNIBRSErrorCode(NIBRSErrorCode._306);
								break;
							}
						}
						typeToMeasureMap.put(t, m);
					}
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule306ForPropertyDescriptions() {
		return new DuplicateCodedValueRule<PropertySegment>("propertyDescription", "15", PropertySegment.class, NIBRSErrorCode._306);
	}
	
	/**
	 * This rule is removed from spec v3-1. Removing it from the rule list. 
	 * @return
	 */
	Rule<PropertySegment> getRule320() {
		return new Rule<PropertySegment>() {
			@Override
			public NIBRSError apply(PropertySegment subject) {
				NIBRSError ret = null;
				GroupAIncidentReport parentIncident = (GroupAIncidentReport) subject.getParentReport();
				ParsedObject<Date> incidentDatePO = parentIncident.getIncidentDate();
				if (!(incidentDatePO.isMissing() || incidentDatePO.isInvalid())) {
					Date incidentDate = incidentDatePO.getValue();
					for (int i = 0; i < 10; i++) {
						ParsedObject<Date> recoveredDatePO = subject.getDateRecovered(i);
						if (recoveredDatePO.getValue() != null) {
							Date recoveredDate = recoveredDatePO.getValue();
							if (recoveredDate.before(incidentDate)) {
								ret = subject.getErrorTemplate();
								ret.setDataElementIdentifier("17");
								ret.setValue(recoveredDate);
								ret.setNIBRSErrorCode(NIBRSErrorCode._320);
							}
						}
					}
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule305() {
		return new Rule<PropertySegment>() {
			@Override
			public NIBRSError apply(PropertySegment subject) {
				NIBRSError ret = null;
				GroupAIncidentReport parentIncident = (GroupAIncidentReport) subject.getParentReport();
				ParsedObject<Date> incidentDatePO = parentIncident.getIncidentDate();
				Integer monthOfTape = parentIncident.getMonthOfTape();
				Integer yearOfTape = parentIncident.getYearOfTape();
				Calendar c = Calendar.getInstance();
				for (int i=0;i < 10;i++) {
					ParsedObject<Date> recoveredDatePO = subject.getDateRecovered(i);
					if (recoveredDatePO.getValue() != null) {
						Date recoveredDate = recoveredDatePO.getValue();
						if (!incidentDatePO.isMissing() && !incidentDatePO.isInvalid() && recoveredDate.before(incidentDatePO.getValue())) {
							ret = subject.getErrorTemplate();
							ret.setDataElementIdentifier("17");
							ret.setValue(recoveredDate);
							ret.setNIBRSErrorCode(NIBRSErrorCode._305);
						} else if (monthOfTape != null && yearOfTape != null) {
							int y = yearOfTape.intValue();
							int m = monthOfTape.intValue();
							if (m == 12) {
								y++;
								m = 1;
							} else {
								m++;
							}
							c.set(y, m-1, 1);
							c.add(Calendar.DAY_OF_MONTH, -1);
							Date submissionDate = c.getTime();
							if (recoveredDate.after(submissionDate)) {
								ret = subject.getErrorTemplate();
								ret.setDataElementIdentifier("17");
								ret.setValue(recoveredDate);
								ret.setNIBRSErrorCode(NIBRSErrorCode._305);
							}
						}
					}
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule304ForPropertyValue() {
		return new Rule<PropertySegment>() {
			@Override
			public NIBRSError apply(PropertySegment subject) {
				NIBRSError ret = null;
				for (int i=0;i < 10;i++) {
					ParsedObject<Integer> propertyValue = subject.getValueOfProperty(i);
					if (propertyValue.getValue() != null) {
						int v = propertyValue.getValue();
						if (v < 0 || v > 999999999) {
							ret = subject.getErrorTemplate();
							ret.setDataElementIdentifier("16");
							ret.setValue(v);
							ret.setNIBRSErrorCode(NIBRSErrorCode._304);
						}
					}
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule304ForDrugQuantity() {
		return new Rule<PropertySegment>() {
			@Override
			public NIBRSError apply(PropertySegment subject) {
				NIBRSError ret = null;
				for (int i=0;i < 3;i++) {
					Double drugQuantity = subject.getEstimatedDrugQuantity(i);
					if (drugQuantity != null) {
						double d = drugQuantity.doubleValue();
						if (d < 0 || d > 100000000) {
							ret = subject.getErrorTemplate();
							ret.setDataElementIdentifier("21");
							ret.setValue(drugQuantity);
							ret.setNIBRSErrorCode(NIBRSErrorCode._304);
						}
					}
				}
				return ret;
			}
		};
	}
	
	Rule<PropertySegment> getRule304ForRecoveredMotorVehicleCount() {
		return new NumericValueRule<>(
				subject -> {
					return subject.getNumberOfRecoveredMotorVehicles().getValue();
				},
				(value, target) -> {
					return getErrorForMotorVehicleCountValue(value, target, "19");
				});
	}

	Rule<PropertySegment> getRule304ForStolenMotorVehicleCount() {
		return new NumericValueRule<>(
				subject -> {
					return subject.getNumberOfStolenMotorVehicles().getValue();
				},
				(value, target) -> {
					return getErrorForMotorVehicleCountValue(value, target, "18");
				});
	}

	Rule<PropertySegment> getRule304ForTypePropertyLoss() {
		return new ValidValueListRule<PropertySegment>("typeOfPropertyLoss", "14", PropertySegment.class, NIBRSErrorCode._304, TypeOfPropertyLossCode.codeSet(), false);
	}
	
	Rule<PropertySegment> getRule304ForListBoundElement(String propertyName, String dataElementIdentifier, Set<String> allowedCodeSet) {
		return new ValidValueListRule<PropertySegment>(propertyName, dataElementIdentifier, PropertySegment.class, NIBRSErrorCode._304, allowedCodeSet);
	}
	
	/**
	 * Get the list of rules for the property segment.
	 * @return the list of rules
	 */
	public List<Rule<PropertySegment>> getRulesList() {
		return Collections.unmodifiableList(rulesList);
	}

	private static final NIBRSError getErrorForMotorVehicleCountValue(Number value, ValidationTarget target, String dataElementIdentifier) {
		NIBRSError ret = null;
		if (value != null && (value.intValue() < 0 || value.intValue() > 99)) {
			ret = target.getErrorTemplate();
			ret.setNIBRSErrorCode(NIBRSErrorCode._304);
			ret.setDataElementIdentifier(dataElementIdentifier);
			ret.setValue(value);
		}
		return ret;
	}
	
}
