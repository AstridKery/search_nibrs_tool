package org.search.nibrs.validation;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.search.nibrs.model.GroupAIncidentReport;
import org.search.nibrs.model.OffenseSegment;
import org.search.nibrs.model.PropertySegment;
import org.search.nibrs.model.codes.LocationTypeCode;
import org.search.nibrs.model.codes.OffenseCode;

/**
 * Class that manages a set of "edits" to baseline incidents. These edits create "exemplars" of NIBRS rules violations that can be used to unit test the validation logic in the precert tool.
 *
 */
public class RuleViolationExemplarFactory {

	private static final RuleViolationExemplarFactory INSTANCE = new RuleViolationExemplarFactory();

	@SuppressWarnings("unused")
	private static final Logger LOG = LogManager.getLogger(RuleViolationExemplarFactory.class);

	private Map<Integer, Function<GroupAIncidentReport, List<GroupAIncidentReport>>> groupATweakerMap;

	private RuleViolationExemplarFactory() {
		groupATweakerMap = new HashMap<Integer, Function<GroupAIncidentReport, List<GroupAIncidentReport>>>();
		populateGroupAExemplarMap();
	}

	/**
	 * Get an instance of the factory.
	 * 
	 * @return the instance
	 */
	public static final RuleViolationExemplarFactory getInstance() {
		return INSTANCE;
	}

	/**
	 * Get an Incident that violates the specified rule. For rules and their numbers, reference the NIBRS Technical Specification, Section 5.
	 * 
	 * @param ruleNumber
	 *            the rule number
	 * @return an incident that exemplifies violation of the rule
	 */
	public List<GroupAIncidentReport> getGroupAIncidentsThatViolateRule(Integer ruleNumber) {
		return groupATweakerMap.get(ruleNumber).apply(BaselineIncidentFactory.getBaselineIncident());
	}

	private void populateGroupAExemplarMap() {

		groupATweakerMap.put(59, incident -> {
			GroupAIncidentReport ret = new GroupAIncidentReport(incident);
			// First two positions must be the code of the state (e.g., SC, MD) in which the incident occurred.
			// non-federal participants, every record must have the same code.
			ret.setOri("ZZ123456789");
			return Collections.singletonList(ret);
		});

		groupATweakerMap.put(101, incident -> {
			// The referenced data element in a Group A Incident AbstractReport
			// Segment 1 is mandatory & must be present.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.setYearOfTape(null);
			incidents.add(copy);
			copy = new GroupAIncidentReport(incident);
			copy.setMonthOfTape(null);
			incidents.add(copy);
			copy = new GroupAIncidentReport(incident);
			copy.setOri(null);
			incidents.add(copy);
			copy = new GroupAIncidentReport(incident);
			copy.setIncidentNumber(null);
			incidents.add(copy);
			copy = new GroupAIncidentReport(incident);
			copy.setIncidentDate(null);
			incidents.add(copy);
			copy = new GroupAIncidentReport(incident);
			copy.setExceptionalClearanceCode(null);
			incidents.add(copy);
			copy = new GroupAIncidentReport(incident);
			copy.setExceptionalClearanceCode("K"); // not valid code
			incidents.add(copy);
			return incidents;

		});

		groupATweakerMap.put(104, incident -> {
			// The referenced data element in a Group A Incident AbstractReport
			// Segment 1 must be valid.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.setYearOfTape(1054);
			incidents.add(copy);
			copy = new GroupAIncidentReport(incident);
			copy.setMonthOfTape(14);
			incidents.add(copy);
			// ReportDateIndicator should be set to "R" if unknown.
			copy = new GroupAIncidentReport(incident);
			copy.setReportDateIndicator("S");
			incidents.add(copy);
			copy = new GroupAIncidentReport(incident);
			copy.setCargoTheftIndicator("X");
			copy.setIncludesCargoTheft(true);
			incidents.add(copy);
			copy = new GroupAIncidentReport(incident);
			copy.setCargoTheftIndicator(null);
			copy.setIncludesCargoTheft(true);
			incidents.add(copy);
			return incidents;

		});

		groupATweakerMap.put(105, incident -> {
			// The data element in error contains a date that is not entered correctly.
			// Each component of the date must be valid; that is, months must be 01 through 12,
			// days must be 01 through 31, and year must include the century (i.e., 19xx, 20xx).
			// In addition, days cannot exceed maximum for the month (e.g., June cannot have 31days).
			// Also, the date cannot exceed the current date.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.setYearOfTape(0120);
			copy.setMonthOfTape(5);
			GroupAIncidentReport copy2 = new GroupAIncidentReport(copy);
			copy.setYearOfTape(2016);
			copy.setMonthOfTape(13);
			copy.setIncidentDate(Date.from(LocalDateTime.of(2016, 6, 31, 30, 7, 46).atZone(ZoneId.systemDefault()).toInstant()));
			GroupAIncidentReport copy4 = new GroupAIncidentReport(copy);
			copy.setExceptionalClearanceDate(Date.from(LocalDateTime.of(3016, 13, 12, 30, 7, 46).atZone(ZoneId.systemDefault()).toInstant()));
			incidents.add(copy);
			incidents.add(copy2);
			incidents.add(copy4);
			return incidents;
		});

		groupATweakerMap.put(115, incident -> {
			GroupAIncidentReport ret = new GroupAIncidentReport(incident);
			// (Incident Number) Must be blank right-fill if under 12 characters in length.
			// Cannot have embedded blanks between the first and last characters entered.
			ret.setIncidentNumber("1234 5678");
			return Collections.singletonList(ret);
		});

		groupATweakerMap.put(116, incident -> {
			// (Incident Number) must be left-justified with blank right-fill.
			// Since the number is less than 12 characters, it must begin in position 1.
			GroupAIncidentReport ret = new GroupAIncidentReport(incident);
			ret.setIncidentNumber(" 12345678");
			return Collections.singletonList(ret);
		});

		groupATweakerMap.put(117, incident -> {

			// (Incident Number) can only have character combinations of A through Z, 0 through 9,
			// hyphens, and/or blanks. For example, 89-123-SC is valid, but 89+123*SC is invalid.
			GroupAIncidentReport ret = new GroupAIncidentReport(incident);
			ret.setIncidentNumber("89+123*SC   ");
			return Collections.singletonList(ret);
		});

		groupATweakerMap.put(119, incident -> {
			// Data Element 2A (Cargo Theft) must be populated with a valid data value when
			// Data Element 6 (UCR OffenseSegment Code) contains a Cargo Theft-related offense.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.setCargoTheftIndicator(null);
			copy.getOffenses().get(0).setUcrOffenseCode("120");
			incidents.add(copy);
			return incidents;
		});

		groupATweakerMap.put(151, incident -> {
			// This field must be blank if the incident date is known. If the incident date is unknown,
			// then the report date would be entered instead and must be indicated with an "R" in the AbstractReport
			// Indicator field within the Administrative Segment. The "R" in this case is invalid.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.setReportDateIndicator("R");
			incidents.add(copy);
			return incidents;
		});

		groupATweakerMap.put(152, incident -> {
			// If Hour is entered within Data Element 3 (Incident Date/Hour), it must be 00 through 23.
			// If 00=Midnight is entered, be careful that the Incident Date is entered as if the time was
			// 1 minute past midnight.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy = new GroupAIncidentReport(incident);
			copy.setIncidentHour(24);
			incidents.add(copy);
			return incidents;
		});

		groupATweakerMap.put(153, incident -> {
			// Data Element 4 ((Cleared Exceptionally) Cannot be N=Not Applicable if Data Element 5 (Exceptional Clearance Date) is entered.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.setExceptionalClearanceCode("N");
			copy.setExceptionalClearanceDate(Date.from(LocalDateTime.of(2016, 5, 12, 10, 7, 46).atZone(ZoneId.systemDefault()).toInstant()));
			incidents.add(copy);
			return incidents;
		});

		groupATweakerMap.put(155, incident -> {
			// Data Element 5 (Exceptional Clearance Date) is earlier than Data Element 3 (Incident Date/Hour).
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.setIncidentDate(Date.from(LocalDateTime.of(2016, 5, 12, 10, 7, 46).atZone(ZoneId.systemDefault()).toInstant()));
			copy.setExceptionalClearanceDate(Date.from(LocalDateTime.of(2015, 5, 12, 10, 7, 46).atZone(ZoneId.systemDefault()).toInstant()));
			incidents.add(copy);
			return incidents;
		});

		groupATweakerMap.put(156, incident -> {
			// Data Element 5 (Exceptional Clearance Date) must be present if the case was cleared exceptionally.
			// Data Element 4 (Cleared Exceptionally) has an entry of A through E; therefore, the date must also be entered.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.setExceptionalClearanceCode("A");
			copy.setExceptionalClearanceDate(null);
			incidents.add(copy);
			return incidents;
		});

		groupATweakerMap.put(170, incident -> {
			// Data Element 3 (Incident Date) The date cannot be later than the year and month the electronic submission represents.
			// For example, the May 1999 electronic submission cannot contain incidents happening after this date.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.setIncidentDate(Date.from(LocalDateTime.of(3016, 5, 12, 10, 7, 46).atZone(ZoneId.systemDefault()).toInstant()));
			incidents.add(copy);
			return incidents;
		});

		groupATweakerMap.put(171, incident -> {
			// A Group A Incident AbstractReport was submitted with a date entered into Data Element 3 (Incident Date/Hour)
			// that is earlier than January 1 of the previous year, using the Month of Tape and Year of Tape as a reference point,
			// e.g., if the Month of Tape and Year of Tape contain a value of 01/1999, but the incident date is 12/25/1997, the incident will be rejected.
			// Volume 2, section I, provides specifications concerning the FBI�s 2-year database.
			// For example, the May 1999 electronic submission cannot contain incidents happening after this date.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.setIncidentDate(Date.from(LocalDateTime.of(2000, 5, 12, 10, 7, 46).atZone(ZoneId.systemDefault()).toInstant()));
			incidents.add(copy);
			return incidents;
		});

		groupATweakerMap.put(172, incident -> {
			// (Incident Date) cannot be earlier than 01/01/1991. This edit will preclude dates that are obviously
			// incorrect since the FBI began accepting NIBRS data on this date.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.setIncidentDate(Date.from(LocalDateTime.of(1990, 5, 12, 10, 7, 46).atZone(ZoneId.systemDefault()).toInstant()));
			incidents.add(copy);
			return incidents;
		});

		groupATweakerMap.put(173, incident -> {
			// Data Element 5 (Exceptional Clearance Date) cannot contain a date earlier than the date the LEA began submitting data via the NIBRS.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.setIncidentDate(Date.from(LocalDateTime.of(1990, 5, 12, 10, 7, 46).atZone(ZoneId.systemDefault()).toInstant()));
			GroupAIncidentReport copy1 = new GroupAIncidentReport(incident);
			copy1.setExceptionalClearanceDate(Date.from(LocalDateTime.of(1016, 5, 12, 10, 7, 46).atZone(ZoneId.systemDefault()).toInstant()));
			incidents.add(copy);
			incidents.add(copy1);
			return incidents;
		});

		groupATweakerMap.put(201, incident -> {
			// The referenced data element in a Group A Incident AbstractReport
			// Segment 2 is mandatory & must be present.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("XXX");
			incidents.add(copy);
			copy = new GroupAIncidentReport(incident);
			//Data Element 6
			copy.getOffenses().get(0).setUcrOffenseCode(null);
			incidents.add(copy);
			//Data Element 7
			copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setOffenseAttemptedCompleted(null);
			incidents.add(copy);
			//Data Element 8
			copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, null);
			incidents.add(copy);
			//Data Element 8A
			copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setBiasMotivation(0, null);
			incidents.add(copy);
			//Data Element 9
			copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setLocationType(null);
			incidents.add(copy);
						
			copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode(null);
			copy.getOffenses().get(0).setOffenseAttemptedCompleted(null);
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, null);
			copy.getOffenses().get(0).setBiasMotivation(0, null);
			copy.getOffenses().get(0).setLocationType(null);
			incidents.add(copy);

			return incidents;
		});

		groupATweakerMap.put(204, incident -> {
			// The referenced data element in a Group A Incident AbstractReport Segment 2 must
			// be populated with a valid data value.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setBiasMotivation(0, "10");
			incidents.add(copy);
			copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setLocationType("99");
			incidents.add(copy);
			copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setMethodOfEntry("X");
			incidents.add(copy);
			copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setTypeOfCriminalActivity(0, "XX");
			incidents.add(copy);
			copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "10");
			incidents.add(copy);
			copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setAutomaticWeaponIndicator(0, "B");
			incidents.add(copy);
			copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setNumberOfPremisesEntered(100);
			incidents.add(copy);
			
			return incidents;
			
		});

		groupATweakerMap.put(206, incident -> {
			// The referenced data element in error is one that contains multiple
			// data values. When more than one code is entered, none can be duplicate codes.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			//Data Element 8			
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "A");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(1, "C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(2, "C");
			incidents.add(copy);
			//Data Element 8A
			copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setBiasMotivation(0, "15");
			copy.getOffenses().get(0).setBiasMotivation(1, "26");
			copy.getOffenses().get(0).setBiasMotivation(2, "26");
			incidents.add(copy);
			//Data Element 12
			copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setTypeOfCriminalActivity(1, "J");
			copy.getOffenses().get(0).setTypeOfCriminalActivity(2, "P");
			incidents.add(copy);
			//Data Element 13
			copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "11");
			copy.getOffenses().get(0).setTypeOfWeaponForceInvolved(1, "11");
			copy.getOffenses().get(0).setBiasMotivation(0, "15");
			copy.getOffenses().get(0).setBiasMotivation(1, "26");
			copy.getOffenses().get(0).setBiasMotivation(2, "26");
			incidents.add(copy);
			
			return incidents;

		});

		groupATweakerMap.put(207, incident -> {
			// The referenced data element in error is one that contains multiple
			// data values. However "N" is mutually exclusive with other codes.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			//Data Element 8
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "A");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(1, "N");
			incidents.add(copy);
			//Data Element 8A
			copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setBiasMotivation(0, "15");
			copy.getOffenses().get(0).setBiasMotivation(1, "88");
			incidents.add(copy);
			//Data Element 12
			copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setTypeOfCriminalActivity(0, "J");
			copy.getOffenses().get(0).setTypeOfCriminalActivity(1, "N");
			incidents.add(copy);
			//Data Element 13
			copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "11");
			copy.getOffenses().get(0).setTypeOfWeaponForceInvolved(1, "99");
			incidents.add(copy);
			
			return incidents;

		});
		
		groupATweakerMap.put(219, incident -> {
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("09B");
			copy.getOffenses().get(0).setTypeOfCriminalActivity(0, "B");
			incidents.add(copy);
			copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("09B");
			copy.getOffenses().get(0).setTypeOfCriminalActivity(0, "C");
			copy.getOffenses().get(0).setTypeOfCriminalActivity(1, "J");
			incidents.add(copy);
			copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("250");
			copy.getOffenses().get(0).setTypeOfCriminalActivity(0, "J");
			incidents.add(copy);
			copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("720");
			copy.getOffenses().get(0).setTypeOfCriminalActivity(0, "J");
			incidents.add(copy);
			
			return incidents;

		});

		groupATweakerMap.put(220, incident -> {
			// Data Element 12 (Type Criminal Activity/Gang Information) Must be populated with a valid data value and cannot be 
			//blank when Data Element 6 (UCR OffenseSegment Code) is:
			// 250=Counterfeiting/Forgery
			// 280=Stolen PropertySegment Offenses
			// 35A=Drug/Narcotic Violations
			// 35B=Drug Equipment Violations
			// 39C=Gambling Equipment Violations
			// 370=Pornography/Obscene Material
			// 520=Weapon Law Violations
			// 720=Animal Cruelty
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			//TypeOfCriminalActivity cannot be blank
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("250");
			copy.getOffenses().get(0).setTypeOfCriminalActivity(0, null);
			GroupAIncidentReport copy2 = new GroupAIncidentReport(incident);
			copy2.getOffenses().get(0).setUcrOffenseCode("280");
			copy2.getOffenses().get(0).setTypeOfCriminalActivity(0, null);
			GroupAIncidentReport copy3 = new GroupAIncidentReport(incident);
			copy3.getOffenses().get(0).setUcrOffenseCode("35A");
			copy3.getOffenses().get(0).setTypeOfCriminalActivity(0, null);
			GroupAIncidentReport copy4 = new GroupAIncidentReport(incident);
			copy4.getOffenses().get(0).setUcrOffenseCode("35B");
			copy4.getOffenses().get(0).setTypeOfCriminalActivity(0, null);
			GroupAIncidentReport copy5 = new GroupAIncidentReport(incident);
			copy5.getOffenses().get(0).setUcrOffenseCode("39C");
			copy5.getOffenses().get(0).setTypeOfCriminalActivity(0, null);
			GroupAIncidentReport copy6 = new GroupAIncidentReport(incident);
			copy6.getOffenses().get(0).setUcrOffenseCode("370");
			copy6.getOffenses().get(0).setTypeOfCriminalActivity(0, null);
			GroupAIncidentReport copy7 = new GroupAIncidentReport(incident);
			copy7.getOffenses().get(0).setUcrOffenseCode("520");
			copy7.getOffenses().get(0).setTypeOfCriminalActivity(0, null);
			GroupAIncidentReport copy8 = new GroupAIncidentReport(incident);
			copy8.getOffenses().get(0).setUcrOffenseCode("720");
			copy8.getOffenses().get(0).setTypeOfCriminalActivity(0, null);
						
			incidents.add(copy);
			incidents.add(copy2);
			incidents.add(copy3);
			incidents.add(copy4);
			incidents.add(copy5);
			incidents.add(copy6);
			incidents.add(copy7);
			incidents.add(copy8);
			
			return incidents;

		});

		groupATweakerMap.put(221, incident -> {
			// Data Element 13 (Type Weapon/Force Involved) Must be populated with a valid data value and cannot be blank
			//when Data Element 6 (UCR OffenseSegment Code) is:
			//09A=Murder and Non-negligent Manslaughter
			//09B=Negligent Manslaughter
			//09C=Justifiable Homicide
			//100=Kidnapping/Abduction
			//11A=Rape
			//11B=Sodomy
			//11C=Sexual Assault With An Object
			//11D=Fondling
			//120=Robbery
			//13A=Aggravated Assault
			//13B=Simple Assault
			//210=Extortion/Blackmail
			//520=Weapon Law Violations
			//64A=Human Trafficking, Commercial Sex Acts
			//64B=Human Trafficking, Involuntary Servitude
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			//TypeOfWeaponForceInvolved cannot be blank
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("9A");
			copy.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, null);
			GroupAIncidentReport copy2 = new GroupAIncidentReport(incident);
			copy2.getOffenses().get(0).setUcrOffenseCode("9B");
			copy2.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, null);
			GroupAIncidentReport copy3 = new GroupAIncidentReport(incident);
			copy3.getOffenses().get(0).setUcrOffenseCode("9C");
			copy3.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, null);
			GroupAIncidentReport copy4 = new GroupAIncidentReport(incident);
			copy4.getOffenses().get(0).setUcrOffenseCode("100");
			copy4.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, null);
			GroupAIncidentReport copy5 = new GroupAIncidentReport(incident);
			copy5.getOffenses().get(0).setUcrOffenseCode("11A");
			copy5.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, null);
			GroupAIncidentReport copy6 = new GroupAIncidentReport(incident);
			copy6.getOffenses().get(0).setUcrOffenseCode("11B");
			copy6.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, null);
			GroupAIncidentReport copy7 = new GroupAIncidentReport(incident);
			copy7.getOffenses().get(0).setUcrOffenseCode("11C");
			copy7.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, null);
			GroupAIncidentReport copy8 = new GroupAIncidentReport(incident);
			copy8.getOffenses().get(0).setUcrOffenseCode("11D");
			copy8.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, null);
			GroupAIncidentReport copy9 = new GroupAIncidentReport(incident);
			copy8.getOffenses().get(0).setUcrOffenseCode("120");
			copy8.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, null);
			GroupAIncidentReport copy10 = new GroupAIncidentReport(incident);
			copy8.getOffenses().get(0).setUcrOffenseCode("13A");
			copy8.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, null);
			GroupAIncidentReport copy11 = new GroupAIncidentReport(incident);
			copy8.getOffenses().get(0).setUcrOffenseCode("13B");
			copy8.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, null);
			GroupAIncidentReport copy12 = new GroupAIncidentReport(incident);
			copy8.getOffenses().get(0).setUcrOffenseCode("210");
			copy8.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, null);
			GroupAIncidentReport copy13 = new GroupAIncidentReport(incident);
			copy8.getOffenses().get(0).setUcrOffenseCode("520");
			copy8.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, null);
			GroupAIncidentReport copy14 = new GroupAIncidentReport(incident);
			copy8.getOffenses().get(0).setUcrOffenseCode("64A");
			copy8.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, null);
			GroupAIncidentReport copy15 = new GroupAIncidentReport(incident);
			copy8.getOffenses().get(0).setUcrOffenseCode("64B");
			copy8.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, null);
			//TypeofWeaponForceInvolved must be valid.
			GroupAIncidentReport copy16 = new GroupAIncidentReport(incident);
			copy16.getOffenses().get(0).setUcrOffenseCode("9A");
			copy16.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "Z");
			GroupAIncidentReport copy17 = new GroupAIncidentReport(incident);
			copy17.getOffenses().get(0).setUcrOffenseCode("9B");
			copy17.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "Z");
			GroupAIncidentReport copy18 = new GroupAIncidentReport(incident);
			copy18.getOffenses().get(0).setUcrOffenseCode("9C");
			copy18.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "Z");
			GroupAIncidentReport copy19 = new GroupAIncidentReport(incident);
			copy19.getOffenses().get(0).setUcrOffenseCode("100");
			copy19.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "Z");
			GroupAIncidentReport copy20 = new GroupAIncidentReport(incident);
			copy20.getOffenses().get(0).setUcrOffenseCode("11A");
			copy20.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "Z");
			GroupAIncidentReport copy21 = new GroupAIncidentReport(incident);
			copy21.getOffenses().get(0).setUcrOffenseCode("11B");
			copy21.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "Z");
			GroupAIncidentReport copy22 = new GroupAIncidentReport(incident);
			copy22.getOffenses().get(0).setUcrOffenseCode("11C");
			copy22.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "Z");
			GroupAIncidentReport copy23 = new GroupAIncidentReport(incident);
			copy23.getOffenses().get(0).setUcrOffenseCode("11D");
			copy23.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "Z");
			GroupAIncidentReport copy24 = new GroupAIncidentReport(incident);
			copy24.getOffenses().get(0).setUcrOffenseCode("120");
			copy24.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "Z");
			GroupAIncidentReport copy25 = new GroupAIncidentReport(incident);
			copy25.getOffenses().get(0).setUcrOffenseCode("13A");
			copy25.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "Z");
			GroupAIncidentReport copy26 = new GroupAIncidentReport(incident);
			copy26.getOffenses().get(0).setUcrOffenseCode("13B");
			copy26.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "Z");
			GroupAIncidentReport copy27 = new GroupAIncidentReport(incident);
			copy27.getOffenses().get(0).setUcrOffenseCode("210");
			copy27.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "Z");
			GroupAIncidentReport copy28 = new GroupAIncidentReport(incident);
			copy28.getOffenses().get(0).setUcrOffenseCode("520");
			copy28.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "Z");
			GroupAIncidentReport copy29 = new GroupAIncidentReport(incident);
			copy29.getOffenses().get(0).setUcrOffenseCode("64A");
			copy29.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "Z");
			GroupAIncidentReport copy30 = new GroupAIncidentReport(incident);
			copy30.getOffenses().get(0).setUcrOffenseCode("64B");
			copy30.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "Z");
			
							
			incidents.add(copy);
			incidents.add(copy2);
			incidents.add(copy3);
			incidents.add(copy4);
			incidents.add(copy5);
			incidents.add(copy6);
			incidents.add(copy7);
			incidents.add(copy8);
			incidents.add(copy9);
			incidents.add(copy10);
			incidents.add(copy11);
			incidents.add(copy12);
			incidents.add(copy13);
			incidents.add(copy14);
			incidents.add(copy15);
			incidents.add(copy16);
			incidents.add(copy17);
			incidents.add(copy18);
			incidents.add(copy19);
			incidents.add(copy20);
			incidents.add(copy21);
			incidents.add(copy22);
			incidents.add(copy23);
			incidents.add(copy24);
			incidents.add(copy25);
			incidents.add(copy26);
			incidents.add(copy27);
			incidents.add(copy28);
			incidents.add(copy29);
			incidents.add(copy30);
				
			return incidents;

		});

	
		groupATweakerMap.put(251, incident -> {
			// (OffenseSegment Attempted/Completed) Must be a valid code of A=Attempted or C=Completed.
			GroupAIncidentReport ret = new GroupAIncidentReport(incident);
			ret.getOffenses().get(0).setOffenseAttemptedCompleted("X");
			return Collections.singletonList(ret);
		});

		groupATweakerMap.put(252, incident -> {

			// When number of premises is entered location type must be 14 or 19
			// and UCR OffenseSegment Code must be Burglary.

			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();

			Set<OffenseCode> offenseCodeSet = EnumSet.allOf(OffenseCode.class);
			offenseCodeSet.remove(OffenseCode._220); // Burglary

			Set<LocationTypeCode> locationTypeCodeSet = LocationTypeCode.asSet();
			locationTypeCodeSet.remove(LocationTypeCode._14);
			locationTypeCodeSet.remove(LocationTypeCode._19);

			for (OffenseCode oc : offenseCodeSet) {
				for (LocationTypeCode ltc : locationTypeCodeSet) {
					GroupAIncidentReport copy = new GroupAIncidentReport(incident);
					OffenseSegment offense = new OffenseSegment();
					offense.setNumberOfPremisesEntered(2);
					offense.setLocationType(ltc.code);
					offense.setUcrOffenseCode(oc.code);
					incident.addOffense(offense);
					incidents.add(copy);
				}
				GroupAIncidentReport copy = new GroupAIncidentReport(incident);
				OffenseSegment offense = new OffenseSegment();
				offense.setNumberOfPremisesEntered(2);
				offense.setLocationType(LocationTypeCode._14.code);
				offense.setUcrOffenseCode(oc.code);
				incident.addOffense(offense);
				incidents.add(copy);
			}

			for (LocationTypeCode ltc : locationTypeCodeSet) {
				GroupAIncidentReport copy = new GroupAIncidentReport(incident);
				OffenseSegment offense = new OffenseSegment();
				offense.setNumberOfPremisesEntered(2);
				offense.setLocationType(ltc.code);
				offense.setUcrOffenseCode(OffenseCode._220.code);
				incident.addOffense(offense);
				incidents.add(copy);
			}

			return incidents;

		});

		
		groupATweakerMap.put(253, incident -> {
			// Data Element 11 (Method of Entry) Data Element was not entered; it must be entered
			// when UCR OffenseSegment Code of 220=Burglary has been entered.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("220");
			copy.getOffenses().get(0).setMethodOfEntry(null);
			
			incidents.add(copy);
			return incidents;

		});

		groupATweakerMap.put(254, incident -> {
			// (Method of Entry) Data Element only applies to UCR OffenseSegment Code of 220=Burglary.
			// Since a burglary offense was not entered, the Method of Entry should not have been entered.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setMethodOfEntry("F");
			
			incidents.add(copy);
			
			return incidents;

		});

		groupATweakerMap.put(255, incident -> {
			// Data Element 13(Automatic Weapon Indicator) Must be A=Automatic or blank=Not Automatic
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setAutomaticWeaponIndicator(0, "F");
			
			incidents.add(copy);
			
			return incidents;

		});

		groupATweakerMap.put(256, incident -> {
			// OffenseSegment Attempted/Completed, Data Element 7, must be a valid code of A=Attempted or C=Completed if UCR code is Homicide
			// Assault.
			GroupAIncidentReport ret = new GroupAIncidentReport(incident);
			ret.getOffenses().get(0).setUcrOffenseCode("09A");
			ret.getOffenses().get(0).setOffenseAttemptedCompleted("X");
			return Collections.singletonList(ret);
		});

		groupATweakerMap.put(257, incident -> {
			// (Number of Premises Entered) Must be entered if offense code is 220 (Burglary)
			// and if Data Element 9 (Location Type) contains 14=Hotel/Motel/Etc. or 19=Rental Storage Facility.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("220");
			copy.getOffenses().get(0).setLocationType("14");
			copy.getOffenses().get(0).setNumberOfPremisesEntered(null);
			GroupAIncidentReport copy2 = new GroupAIncidentReport(incident);
			copy2.getOffenses().get(0).setUcrOffenseCode("220");
			copy2.getOffenses().get(0).setLocationType("19");
			copy2.getOffenses().get(0).setNumberOfPremisesEntered(null);
			
					
			incidents.add(copy);
			incidents.add(copy2);
			
			return incidents;

		});

		groupATweakerMap.put(258, incident -> {
			// (Automatic Weapon Indicator) In Data Element 13 (Type of Weapon/Force Involved), A=Automatic 
			//is the third character of code. It is valid only with the following codes:
			// 11=Firearm (Type Not Stated)
			// 12=Handgun
			// 13=Rifle
			// 15=Other Firearm
			// A weapon code other than those mentioned was entered with the automatic indicator. An automatic weapon is, by definition, a firearm.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setAutomaticWeaponIndicator(0, "A");
			copy.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "20");

			incidents.add(copy);
			
			return incidents;

		});

		groupATweakerMap.put(262, incident -> {
			// When a Group A Incident Report is submitted, the individual segments
			// comprising the incident cannot contain duplicates.
			// In this case, two OffenseSegment Segments were submitted having the same
			// offense in Data Element 6 (UCR OffenseSegment Code).
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			OffenseSegment offense2 = new OffenseSegment();
			offense2.setUcrOffenseCode("13A");
			offense2.setTypeOfCriminalActivity(0, "J");
			offense2.setOffenseAttemptedCompleted("C");
			offense2.setTypeOfWeaponForceInvolved(0, "99");
			offense2.setOffendersSuspectedOfUsing(0, "N");
			offense2.setBiasMotivation(0, "15");
			offense2.setLocationType("15");
			offense2.setNumberOfPremisesEntered(null);
			offense2.setAutomaticWeaponIndicator(0, " ");
			
					
			copy.addOffense(offense2);
			incidents.add(copy);
			return incidents;
		});

		groupATweakerMap.put(263, incident -> {
			// Can be submitted only 10 times for each Group A Incident AbstractReport;
			// 10 offense codes are allowed for each incident.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			//Offense Segment 2
			OffenseSegment offense2 = new OffenseSegment();
			offense2.setUcrOffenseCode("13A");
			offense2.setTypeOfCriminalActivity(0, "J");
			offense2.setOffenseAttemptedCompleted("C");
			offense2.setTypeOfWeaponForceInvolved(0, "99");
			offense2.setOffendersSuspectedOfUsing(0, "N");
			offense2.setBiasMotivation(0, "15");
			offense2.setLocationType("15");
			offense2.setNumberOfPremisesEntered(null);
			offense2.setAutomaticWeaponIndicator(0, " ");
			//OffenseSegment 3
			OffenseSegment offense3 = new OffenseSegment();
			offense3.setUcrOffenseCode("13B");
			offense3.setTypeOfCriminalActivity(0, "J");
			offense3.setOffenseAttemptedCompleted("C");
			offense3.setTypeOfWeaponForceInvolved(0, "99");
			offense3.setOffendersSuspectedOfUsing(0, "N");
			offense3.setBiasMotivation(0, "15");
			offense3.setLocationType("15");
			offense3.setNumberOfPremisesEntered(null);
			offense3.setAutomaticWeaponIndicator(0, " ");
			//OffenseSegment 4
			OffenseSegment offense4 = new OffenseSegment();
			offense4.setUcrOffenseCode("13C");
			offense4.setTypeOfCriminalActivity(0, "J");
			offense4.setOffenseAttemptedCompleted("C");
			offense4.setTypeOfWeaponForceInvolved(0, "99");
			offense4.setOffendersSuspectedOfUsing(0, "N");
			offense4.setBiasMotivation(0, "15");
			offense4.setLocationType("15");
			offense4.setNumberOfPremisesEntered(null);
			offense4.setAutomaticWeaponIndicator(0, " ");
			//OffenseSegment 5
			OffenseSegment offense5 = new OffenseSegment();
			offense5.setUcrOffenseCode("510");
			offense5.setTypeOfCriminalActivity(0, "J");
			offense5.setOffenseAttemptedCompleted("C");
			offense5.setTypeOfWeaponForceInvolved(0, "99");
			offense5.setOffendersSuspectedOfUsing(0, "N");
			offense5.setBiasMotivation(0, "15");
			offense5.setLocationType("15");
			offense5.setNumberOfPremisesEntered(null);
			offense5.setAutomaticWeaponIndicator(0, " ");
			//OffenseSegment 6
			OffenseSegment offense6 = new OffenseSegment();
			offense6.setUcrOffenseCode("26A");
			offense6.setTypeOfCriminalActivity(0, "J");
			offense6.setOffenseAttemptedCompleted("C");
			offense6.setTypeOfWeaponForceInvolved(0, "99");
			offense6.setOffendersSuspectedOfUsing(0, "N");
			offense6.setBiasMotivation(0, "15");
			offense6.setLocationType("15");
			offense6.setNumberOfPremisesEntered(null);
			offense6.setAutomaticWeaponIndicator(0, " ");
			//Offense Segment 7
			OffenseSegment offense7 = new OffenseSegment();
			offense7.setUcrOffenseCode("26B");
			offense7.setTypeOfCriminalActivity(0, "J");
			offense7.setOffenseAttemptedCompleted("C");
			offense7.setTypeOfWeaponForceInvolved(0, "99");
			offense7.setOffendersSuspectedOfUsing(0, "N");
			offense7.setBiasMotivation(0, "15");
			offense7.setLocationType("15");
			offense7.setNumberOfPremisesEntered(null);
			offense7.setAutomaticWeaponIndicator(0, " ");
			//OffenseSegment 8
			OffenseSegment offense8 = new OffenseSegment();
			offense8.setUcrOffenseCode("26C");
			offense8.setTypeOfCriminalActivity(0, "J");
			offense8.setOffenseAttemptedCompleted("C");
			offense8.setTypeOfWeaponForceInvolved(0, "99");
			offense8.setOffendersSuspectedOfUsing(0, "N");
			offense8.setBiasMotivation(0, "15");
			offense8.setLocationType("15");
			offense8.setNumberOfPremisesEntered(null);
			offense8.setAutomaticWeaponIndicator(0, " ");
			//OffenseSegment 9
			OffenseSegment offense9 = new OffenseSegment();
			offense9.setUcrOffenseCode("26D");
			offense9.setTypeOfCriminalActivity(0, "J");
			offense9.setOffenseAttemptedCompleted("C");
			offense9.setTypeOfWeaponForceInvolved(0, "99");
			offense9.setOffendersSuspectedOfUsing(0, "N");
			offense9.setBiasMotivation(0, "15");
			offense9.setLocationType("15");
			offense9.setNumberOfPremisesEntered(null);
			offense9.setAutomaticWeaponIndicator(0, " ");
			//OffenseSegment 10
			OffenseSegment offense10 = new OffenseSegment();
			offense10.setUcrOffenseCode("26E");
			offense10.setTypeOfCriminalActivity(0, "J");
			offense10.setOffenseAttemptedCompleted("C");
			offense10.setTypeOfWeaponForceInvolved(0, "99");
			offense10.setOffendersSuspectedOfUsing(0, "N");
			offense10.setBiasMotivation(0, "15");
			offense10.setLocationType("15");
			offense10.setNumberOfPremisesEntered(null);
			offense10.setAutomaticWeaponIndicator(0, " ");
			//OffenseSegment 11
			OffenseSegment offense11 = new OffenseSegment();
			offense11.setUcrOffenseCode("39A");
			offense11.setTypeOfCriminalActivity(0, "J");
			offense11.setOffenseAttemptedCompleted("C");
			offense11.setTypeOfWeaponForceInvolved(0, "99");
			offense11.setOffendersSuspectedOfUsing(0, "N");
			offense11.setBiasMotivation(0, "15");
			offense11.setLocationType("15");
			offense11.setNumberOfPremisesEntered(null);
			offense11.setAutomaticWeaponIndicator(0, " ");
			
				
			copy.addOffense(offense2);
			copy.addOffense(offense3);		
			copy.addOffense(offense4);
			copy.addOffense(offense5);
			copy.addOffense(offense6);
			copy.addOffense(offense7);
			copy.addOffense(offense8);
			copy.addOffense(offense9);
			copy.addOffense(offense10);
			copy.addOffense(offense11);

			incidents.add(copy);
			return incidents;
		});

		groupATweakerMap.put(264, incident -> {
			// Group A OffenseSegment code cannot contain a Group B OffenseSegment
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("90A");
					
			incidents.add(copy);
			
			return incidents;
		});

		groupATweakerMap.put(265, incident -> {
			// (Type Weapon/Force Involved) If an OffenseSegment Segment (Level 2) was submitted for 13B=Simple Assault,
			// Data Element 13 (Type Weapon/Force Involved) can only have codes of 40=Personal Weapons,
			// 90=Other, 95=Unknown, and 99=None. All other codes are not valid because they do not relate to a simple assault.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("13B");
			copy.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "11");
			GroupAIncidentReport copy2 = new GroupAIncidentReport(incident);
			copy2.getOffenses().get(0).setUcrOffenseCode("13B");
			copy2.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "12");
			GroupAIncidentReport copy3 = new GroupAIncidentReport(incident);
			copy3.getOffenses().get(0).setUcrOffenseCode("13B");
			copy3.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "13");
			GroupAIncidentReport copy4 = new GroupAIncidentReport(incident);
			copy4.getOffenses().get(0).setUcrOffenseCode("13B");
			copy4.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "14");
			GroupAIncidentReport copy5 = new GroupAIncidentReport(incident);
			copy5.getOffenses().get(0).setUcrOffenseCode("13B");
			copy5.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "15");
			GroupAIncidentReport copy6 = new GroupAIncidentReport(incident);
			copy6.getOffenses().get(0).setUcrOffenseCode("13B");
			copy6.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "20");
			GroupAIncidentReport copy7 = new GroupAIncidentReport(incident);
			copy7.getOffenses().get(0).setUcrOffenseCode("13B");
			copy7.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "30");
			GroupAIncidentReport copy8 = new GroupAIncidentReport(incident);
			copy8.getOffenses().get(0).setUcrOffenseCode("13B");
			copy8.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "50");
			GroupAIncidentReport copy9 = new GroupAIncidentReport(incident);
			copy9.getOffenses().get(0).setUcrOffenseCode("13B");
			copy9.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "60");
			GroupAIncidentReport copy10 = new GroupAIncidentReport(incident);
			copy10.getOffenses().get(0).setUcrOffenseCode("13B");
			copy10.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "65");
			GroupAIncidentReport copy11 = new GroupAIncidentReport(incident);
			copy11.getOffenses().get(0).setUcrOffenseCode("13B");
			copy11.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "70");
			GroupAIncidentReport copy12 = new GroupAIncidentReport(incident);
			copy12.getOffenses().get(0).setUcrOffenseCode("13B");
			copy12.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "80");
			
			
			incidents.add(copy);
			incidents.add(copy2);
			incidents.add(copy3);
			incidents.add(copy4);
			incidents.add(copy5);
			incidents.add(copy6);
			incidents.add(copy7);
			incidents.add(copy8);
			incidents.add(copy9);
			incidents.add(copy10);
			incidents.add(copy11);
			incidents.add(copy12);
			
			return incidents;
		});

		groupATweakerMap.put(266, incident -> {
			// When a Justifiable Homicide is reported, no other offense may be
			// reported in the Group A Incident AbstractReport. These should be submitted on another
			// Group A Incident AbstractReport.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			OffenseSegment offense2 = new OffenseSegment();
			offense2.setUcrOffenseCode("09C");
			offense2.setTypeOfCriminalActivity(0, "J");
			offense2.setOffenseAttemptedCompleted("C");
			offense2.setTypeOfWeaponForceInvolved(0, "1199");
			offense2.setOffendersSuspectedOfUsing(0, "N");
			offense2.setBiasMotivation(0, "03");
			offense2.setLocationType("15");
			offense2.setNumberOfPremisesEntered(null);
			offense2.setAutomaticWeaponIndicator(0, " ");
			
			incidents.add(copy);
			
			return incidents;

		});

		groupATweakerMap.put(267, incident -> {
			// If a homicide offense is submitted, Data Element 13 (Type Weapon/Force Involved)
			// cannot have 99=None. Some type of weapon/force must be used in a homicide offense.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("09A");
			copy.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "99");
			
			incidents.add(copy);
			
			return incidents;

		});

		groupATweakerMap.put(269, incident -> {
			// (Type Weapon/Force Involved) If Data Element 6 (UCR OffenseSegment Code) is 13B=Simple Assault and the
			// weapon involved is 11=Firearm, 12=Handgun, 13=Rifle, 14=Shotgun, or 15=Other Firearm, then the offense
			// should instead be classified as 13A=Aggravated Assault.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("13B");
			copy.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "11");
			GroupAIncidentReport copy2 = new GroupAIncidentReport(incident);
			copy2.getOffenses().get(0).setUcrOffenseCode("13B");
			copy2.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "12");
			GroupAIncidentReport copy3 = new GroupAIncidentReport(incident);
			copy3.getOffenses().get(0).setUcrOffenseCode("13B");
			copy3.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "13");
			GroupAIncidentReport copy4 = new GroupAIncidentReport(incident);
			copy4.getOffenses().get(0).setUcrOffenseCode("13B");
			copy4.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "14");
			GroupAIncidentReport copy5 = new GroupAIncidentReport(incident);
			copy5.getOffenses().get(0).setUcrOffenseCode("13B");
			copy5.getOffenses().get(0).setTypeOfWeaponForceInvolved(0, "15");
			
			
			incidents.add(copy);
			incidents.add(copy2);
			incidents.add(copy3);
			incidents.add(copy4);
			incidents.add(copy5);
			
			return incidents;

		});

		groupATweakerMap.put(270, incident -> {
			// If a justifiable homicide offense is submitted, Data Element 8A (Bias motivation) must be 88.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("09C");
			copy.getOffenses().get(0).setBiasMotivation(0, "15");
						
			incidents.add(copy);
			
			return incidents;

		});

		groupATweakerMap.put(301, incident -> {
			// The referenced data element in a Group A Incident AbstractReport
			// Segment 3 is mandatory & must be present.
			//Header Elements cannot be blank
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			//Year of Tape
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.setYearOfTape(null);
			//Valid Offense Segment for an incident including Property Loss
			//Motor Vehicle Theft, Completed, OffenderSuspectedOfUsing N/A, No BiasMotivationCode, Residence Location, 1 Premise, No forced entry
			copy.getOffenses().get(0).setUcrOffenseCode("240");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			copy.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy.getOffenses().get(0).setMethodOfEntry("N");
			//Valid Property Segment: Stolen, Automobiles(1) Value $10,000, 
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("7");
			property.setPropertyDescription(0, "03");
			property.setValueOfProperty(0, 000010000);
			property.setNumberOfStolenMotorVehicles(1);
			//Month of Tape
			GroupAIncidentReport copy2 = new GroupAIncidentReport(copy);
			copy2.setMonthOfTape(null);
			//Valid Offense Segment for an incident including Property Loss
			//Motor Vehicle Theft, Completed, OffenderSuspectedOfUsing N/A, No BiasMotivationCode, Residence Location, 1 Premise, No forced entry
			copy2.getOffenses().get(0).setUcrOffenseCode("220");
			copy2.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy2.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy2.getOffenses().get(0).setBiasMotivation(0, "88");
			copy2.getOffenses().get(0).setLocationType("20");
			copy2.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy2.getOffenses().get(0).setMethodOfEntry("N");
			//Valid Property Segment: Stolen, Automobiles(1) Value $10,000, 
			PropertySegment property2 = new PropertySegment();
			property.setTypeOfPropertyLoss("7");
			property.setPropertyDescription(0, "03");
			property.setValueOfProperty(0, 000010000);
			property.setNumberOfStolenMotorVehicles(1);
			//City Indicator Blank
			GroupAIncidentReport copy3 = new GroupAIncidentReport(copy);
			copy3.setCityIndicator(null);
			//Valid Offense Segment for an incident including Property Loss
			//Motor Vehicle Theft, Completed, OffenderSuspectedOfUsing N/A, No BiasMotivationCode, Residence Location, 1 Premise, No forced entry
			copy3.getOffenses().get(0).setUcrOffenseCode("220");
			copy3.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy3.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy3.getOffenses().get(0).setBiasMotivation(0, "88");
			copy3.getOffenses().get(0).setLocationType("20");
			copy3.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy3.getOffenses().get(0).setMethodOfEntry("N");
			//Valid Property Segment: Stolen, Automobiles(1) Value $10,000, 
			PropertySegment property3 = new PropertySegment();
			property.setTypeOfPropertyLoss("7");
			property.setPropertyDescription(0, "03");
			property.setValueOfProperty(0, 000010000);
			property.setNumberOfStolenMotorVehicles(1);
			//ORI Blank
			GroupAIncidentReport copy4 = new GroupAIncidentReport(copy);
			copy4.setOri(null);
			//Valid Offense Segment for an incident including Property Loss
			//Motor Vehicle Theft, Completed, OffenderSuspectedOfUsing N/A, No BiasMotivationCode, Residence Location, 1 Premise, No forced entry
			copy4.getOffenses().get(0).setUcrOffenseCode("220");
			copy4.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy4.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy4.getOffenses().get(0).setBiasMotivation(0, "88");
			copy4.getOffenses().get(0).setLocationType("20");
			copy4.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy4.getOffenses().get(0).setMethodOfEntry("N");
			//Valid Property Segment: Stolen, Automobiles(1) Value $10,000, 
			PropertySegment property4 = new PropertySegment();
			property4.setTypeOfPropertyLoss("7");
			property4.setPropertyDescription(0, "03");
			property4.setValueOfProperty(0, 000010000);
			property4.setNumberOfStolenMotorVehicles(1);
			//Incident Number blank
			GroupAIncidentReport copy5 = new GroupAIncidentReport(copy);
			copy5.setIncidentNumber(null);
			//Valid Offense Segment for an incident including Property Loss
			//Motor Vehicle Theft, Completed, OffenderSuspectedOfUsing N/A, No BiasMotivationCode, Residence Location, 1 Premise, No forced entry
			copy5.getOffenses().get(0).setUcrOffenseCode("220");
			copy5.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy5.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy5.getOffenses().get(0).setBiasMotivation(0, "88");
			copy5.getOffenses().get(0).setLocationType("20");
			copy5.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy5.getOffenses().get(0).setMethodOfEntry("N");
			//Valid Property Segment: Stolen, Automobiles(1) Value $10,000, 
			PropertySegment property5 = new PropertySegment();
			property5.setTypeOfPropertyLoss("7");
			property5.setPropertyDescription(0, "03");
			property5.setValueOfProperty(0, 000010000);
			property5.setNumberOfStolenMotorVehicles(1);
			
			
			incidents.add(copy);
			incidents.add(copy2);
			incidents.add(copy3);
			incidents.add(copy4);
			incidents.add(copy5);
			copy.addProperty(property);
			copy2.addProperty(property2);
			copy3.addProperty(property3);
			copy4.addProperty(property4);
			copy5.addProperty(property5);
			
			
			return incidents;
		});

		groupATweakerMap.put(304, incident -> {
			// The referenced data element in a Group A Incident AbstractReport Segment 3 must
			// be populated with a valid data value.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			//Invalid ORI
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.setOri("1234567890123");
			//Valid Offense Segment for an incident including Property Loss
			//Motor Vehicle Theft, Completed, OffenderSuspectedOfUsing N/A, No BiasMotivationCode, Residence Location, 1 Premise, No forced entry
			copy.getOffenses().get(0).setUcrOffenseCode("240");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			copy.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy.getOffenses().get(0).setMethodOfEntry("N");
			//Valid Property Segment: Stolen, Automobiles(1) Value $10,000, 
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("7");
			property.setPropertyDescription(0, "03");
			property.setValueOfProperty(0, 000010000);
			property.setNumberOfStolenMotorVehicles(1);
			//Invalid ORI State Code
			GroupAIncidentReport copy2 = new GroupAIncidentReport(copy);
			copy2.setOri("ZZ123456789");
			//Valid Offense Segment for an incident including Property Loss
			//Motor Vehicle Theft, Completed, OffenderSuspectedOfUsing N/A, No BiasMotivationCode, Residence Location, 1 Premise, No forced entry
			copy2.getOffenses().get(0).setUcrOffenseCode("240");
			copy2.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy2.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy2.getOffenses().get(0).setBiasMotivation(0, "88");
			copy2.getOffenses().get(0).setLocationType("20");
			copy2.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy2.getOffenses().get(0).setMethodOfEntry("N");
			//Valid Property Segment: Stolen, Automobiles(1) Value $10,000, 
			PropertySegment property2 = new PropertySegment();
			property2.setTypeOfPropertyLoss("7");
			property2.setPropertyDescription(0, "03");
			property2.setValueOfProperty(0, 000010000);
			property2.setNumberOfStolenMotorVehicles(1);
			//Invalid City Indicator
			GroupAIncidentReport copy3 = new GroupAIncidentReport(copy);
			copy3.setCityIndicator("ZZ12");
			//Valid Offense Segment for an incident including Property Loss
			//Motor Vehicle Theft, Completed, OffenderSuspectedOfUsing N/A, No BiasMotivationCode, Residence Location, 1 Premise, No forced entry
			copy3.getOffenses().get(0).setUcrOffenseCode("240");
			copy3.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy3.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy3.getOffenses().get(0).setBiasMotivation(0, "88");
			copy3.getOffenses().get(0).setLocationType("20");
			copy3.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy3.getOffenses().get(0).setMethodOfEntry("N");
			//Valid Property Segment: Stolen, Automobiles(1) Value $10,000, 
			PropertySegment property3 = new PropertySegment();
			property3.setTypeOfPropertyLoss("7");
			property3.setPropertyDescription(0, "03");
			property3.setValueOfProperty(0, 000010000);
			property3.setNumberOfStolenMotorVehicles(1);
			//Invalid TypeOfPropertyLoss
			GroupAIncidentReport copy4 = new GroupAIncidentReport(copy);
			copy4.getOffenses().get(0).setUcrOffenseCode("240");
			copy4.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy4.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy4.getOffenses().get(0).setBiasMotivation(0, "88");
			copy4.getOffenses().get(0).setLocationType("20");
			copy4.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy4.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property4 = new PropertySegment();
			property4.setTypeOfPropertyLoss("9");
			property4.setPropertyDescription(0, "03");
			property4.setValueOfProperty(0, 000010000);
			property4.setNumberOfStolenMotorVehicles(1);
			//Null TypeOfPropertyLoss
			GroupAIncidentReport copy5 = new GroupAIncidentReport(copy);
			copy5.getOffenses().get(0).setUcrOffenseCode("240");
			copy5.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy5.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy5.getOffenses().get(0).setBiasMotivation(0, "88");
			copy5.getOffenses().get(0).setLocationType("20");
			copy5.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy5.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property5 = new PropertySegment();
			property5.setTypeOfPropertyLoss(null);
			property5.setPropertyDescription(0, "03");
			property5.setValueOfProperty(0, 000010000);
			property5.setNumberOfStolenMotorVehicles(1);
			//Invalid PropertyDescription
			GroupAIncidentReport copy6 = new GroupAIncidentReport(copy);
			copy6.getOffenses().get(0).setUcrOffenseCode("240");
			copy6.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy6.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy6.getOffenses().get(0).setBiasMotivation(0, "88");
			copy6.getOffenses().get(0).setLocationType("20");
			copy6.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy6.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property6 = new PropertySegment();
			property6.setTypeOfPropertyLoss("7");
			property6.setPropertyDescription(0, "00");
			property6.setValueOfProperty(0, 000010000);
			property6.setNumberOfStolenMotorVehicles(1);
			//Null PropertyDescription
			GroupAIncidentReport copy7 = new GroupAIncidentReport(copy);
			copy7.getOffenses().get(0).setUcrOffenseCode("240");
			copy7.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy7.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy7.getOffenses().get(0).setBiasMotivation(0, "88");
			copy7.getOffenses().get(0).setLocationType("20");
			copy7.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy7.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property7 = new PropertySegment();
			property7.setTypeOfPropertyLoss("7");
			property7.setPropertyDescription(0, null);
			property7.setValueOfProperty(0, 000010000);
			property7.setNumberOfStolenMotorVehicles(1);
			//Null ValueOfProperty
			GroupAIncidentReport copy8 = new GroupAIncidentReport(copy);
			copy8.getOffenses().get(0).setUcrOffenseCode("240");
			copy8.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy8.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy8.getOffenses().get(0).setBiasMotivation(0, "88");
			copy8.getOffenses().get(0).setLocationType("20");
			copy8.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy8.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property8 = new PropertySegment();
			property8.setTypeOfPropertyLoss("7");
			property8.setPropertyDescription(0, "03");
			property8.setValueOfProperty(0, null);
			property8.setNumberOfStolenMotorVehicles(1);
			//Invalid ValueOfProperty
			GroupAIncidentReport copy9 = new GroupAIncidentReport(copy);
			copy9.getOffenses().get(0).setUcrOffenseCode("240");
			copy9.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy9.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy9.getOffenses().get(0).setBiasMotivation(0, "88");
			copy9.getOffenses().get(0).setLocationType("20");
			copy9.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy9.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property9 = new PropertySegment();
			property9.setTypeOfPropertyLoss("7");
			property9.setPropertyDescription(0, "03");
			property9.setValueOfProperty(0, 0000010000);
			property9.setNumberOfStolenMotorVehicles(1);
			//Invalid NumberOfStolenMotorVehicles
			GroupAIncidentReport copy10 = new GroupAIncidentReport(copy);
			copy10.getOffenses().get(0).setUcrOffenseCode("240");
			copy10.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy10.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy10.getOffenses().get(0).setBiasMotivation(0, "88");
			copy10.getOffenses().get(0).setLocationType("20");
			copy10.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy10.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property10 = new PropertySegment();
			property10.setTypeOfPropertyLoss("7");
			property10.setPropertyDescription(0, "03");
			property10.setValueOfProperty(0, 000010000);
			property10.setNumberOfStolenMotorVehicles(0);
			property10.setDateRecovered(0, null);
			//Null Date Recovered
			GroupAIncidentReport copy11 = new GroupAIncidentReport(copy);
			copy11.getOffenses().get(0).setUcrOffenseCode("240");
			copy11.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy11.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy11.getOffenses().get(0).setBiasMotivation(0, "88");
			copy11.getOffenses().get(0).setLocationType("20");
			copy11.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy11.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property11 = new PropertySegment();
			property11.setTypeOfPropertyLoss("5");
			property11.setPropertyDescription(0, "03");
			property11.setValueOfProperty(0, 000010000);
			property11.setNumberOfRecoveredMotorVehicles(1);
			property11.setDateRecovered(0, null);
			//Invalid Date Recovered
			GroupAIncidentReport copy12 = new GroupAIncidentReport(copy);
			copy12.getOffenses().get(0).setUcrOffenseCode("240");
			copy12.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy12.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy12.getOffenses().get(0).setBiasMotivation(0, "88");
			copy12.getOffenses().get(0).setLocationType("20");
			copy12.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy12.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property12 = new PropertySegment();
			property12.setTypeOfPropertyLoss("5");
			property12.setPropertyDescription(0, "03");
			property12.setValueOfProperty(0, 000010000);
			property12.setNumberOfRecoveredMotorVehicles(1);
			property12.setDateRecovered(0, (Date.from(LocalDateTime.of(20160, 5, 12, 10, 7, 46).atZone(ZoneId.systemDefault()).toInstant())));
			//Invalid NumberOfRecoveredMotorVehicles
			GroupAIncidentReport copy13 = new GroupAIncidentReport(copy);
			copy13.getOffenses().get(0).setUcrOffenseCode("240");
			copy13.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy13.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy13.getOffenses().get(0).setBiasMotivation(0, "88");
			copy13.getOffenses().get(0).setLocationType("20");
			copy13.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy13.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property13 = new PropertySegment();
			property13.setTypeOfPropertyLoss("5");
			property13.setPropertyDescription(0, "03");
			property13.setValueOfProperty(0, 000010000);
			property13.setNumberOfRecoveredMotorVehicles(0);
			property13.setDateRecovered(0, (Date.from(LocalDateTime.of(2016, 5, 12, 10, 7, 46).atZone(ZoneId.systemDefault()).toInstant())));
			//Null NumberOfRecoveredMotorVehicles
			GroupAIncidentReport copy14 = new GroupAIncidentReport(copy);
			copy14.getOffenses().get(0).setUcrOffenseCode("240");
			copy14.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy14.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy14.getOffenses().get(0).setBiasMotivation(0, "88");
			copy14.getOffenses().get(0).setLocationType("20");
			copy14.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy14.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property14 = new PropertySegment();
			property14.setTypeOfPropertyLoss("5");
			property14.setPropertyDescription(0, "03");
			property14.setValueOfProperty(0, 000010000);
			property14.setNumberOfRecoveredMotorVehicles(null);
			property14.setDateRecovered(0, (Date.from(LocalDateTime.of(2016, 5, 12, 10, 7, 46).atZone(ZoneId.systemDefault()).toInstant())));
			//Suspected Drug Type cannot be blank
			GroupAIncidentReport copy15 = new GroupAIncidentReport(copy);
			copy15.getOffenses().get(0).setUcrOffenseCode("35A");
			copy15.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy15.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy15.getOffenses().get(0).setBiasMotivation(0, "88");
			copy15.getOffenses().get(0).setLocationType("20");
			copy15.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy15.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property15 = new PropertySegment();
			property15.setTypeOfPropertyLoss("6");
			property15.setPropertyDescription(0, "10");
			property15.setSuspectedDrugType(0, null);
			property15.setEstimatedDrugQuantity(0, 1.0);
			property15.setTypeDrugMeasurement(0, "OZ");
			property15.setValueOfProperty(0, null);
			//Suspected Drug type must be valid
			GroupAIncidentReport copy16 = new GroupAIncidentReport(copy);
			copy16.getOffenses().get(0).setUcrOffenseCode("35A");
			copy16.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy16.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy16.getOffenses().get(0).setBiasMotivation(0, "88");
			copy16.getOffenses().get(0).setLocationType("20");
			PropertySegment property16 = new PropertySegment();
			property16.setTypeOfPropertyLoss("6");
			property16.setPropertyDescription(0, "10");
			property16.setSuspectedDrugType(0, "Z");
			property16.setEstimatedDrugQuantity(0, 1.0);
			property16.setTypeDrugMeasurement(0, "OZ");
			property16.setValueOfProperty(0, null);
			//Estimated Drug Quantity cannot be blank
			GroupAIncidentReport copy17 = new GroupAIncidentReport(copy);
			copy17.getOffenses().get(0).setUcrOffenseCode("35A");
			copy17.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy17.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy17.getOffenses().get(0).setBiasMotivation(0, "88");
			copy17.getOffenses().get(0).setLocationType("20");
			PropertySegment property17 = new PropertySegment();
			property17.setTypeOfPropertyLoss("6");
			property17.setPropertyDescription(0, "10");
			property17.setSuspectedDrugType(0, "A");
			property17.setEstimatedDrugQuantity(0, null);
			property17.setTypeDrugMeasurement(0, "OZ");
			property17.setValueOfProperty(0, null);
			//Estimated Drug Quantity must be valid.
			GroupAIncidentReport copy18 = new GroupAIncidentReport(copy);
			copy18.getOffenses().get(0).setUcrOffenseCode("35A");
			copy18.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy18.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy18.getOffenses().get(0).setBiasMotivation(0, "88");
			copy18.getOffenses().get(0).setLocationType("20");
			PropertySegment property18 = new PropertySegment();
			property18.setTypeOfPropertyLoss("6");
			property18.setPropertyDescription(0, "10");
			property18.setSuspectedDrugType(0, "A");
			property18.setEstimatedDrugQuantity(0, 9999999999.0);
			property18.setTypeDrugMeasurement(0, "OZ");
			property18.setValueOfProperty(0, null);
			//Drug Measurement cannot be blank.
			GroupAIncidentReport copy19 = new GroupAIncidentReport(copy);
			copy19.getOffenses().get(0).setUcrOffenseCode("35A");
			copy19.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy19.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy19.getOffenses().get(0).setBiasMotivation(0, "88");
			copy19.getOffenses().get(0).setLocationType("20");
			PropertySegment property19 = new PropertySegment();
			property19.setTypeOfPropertyLoss("6");
			property19.setPropertyDescription(0, "10");
			property19.setSuspectedDrugType(0, "A");
			property19.setEstimatedDrugQuantity(0, 1.0);
			property19.setTypeDrugMeasurement(0, null);
			property19.setValueOfProperty(0, null);
			//Drug Measurement must be valid.
			GroupAIncidentReport copy20 = new GroupAIncidentReport(copy);
			copy20.getOffenses().get(0).setUcrOffenseCode("35A");
			copy20.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy20.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy20.getOffenses().get(0).setBiasMotivation(0, "88");
			copy20.getOffenses().get(0).setLocationType("20");
			PropertySegment property20 = new PropertySegment();
			property20.setTypeOfPropertyLoss("6");
			property20.setPropertyDescription(0, "10");
			property20.setSuspectedDrugType(0, "A");
			property20.setEstimatedDrugQuantity(0, 1.0);
			property20.setTypeDrugMeasurement(0, "ZZ");
			property20.setValueOfProperty(0, null);
			
			
			
			incidents.add(copy);
			incidents.add(copy2);
			incidents.add(copy3);
			incidents.add(copy4);
			incidents.add(copy5);
			incidents.add(copy6);
			incidents.add(copy7);
			incidents.add(copy8);
			incidents.add(copy9);
			incidents.add(copy10);
			incidents.add(copy11);
			incidents.add(copy12);
			incidents.add(copy13);
			incidents.add(copy14);
			incidents.add(copy15);
			incidents.add(copy16);
			incidents.add(copy17);
			incidents.add(copy18);
			incidents.add(copy19);
			incidents.add(copy20);
			copy.addProperty(property);
			copy2.addProperty(property2);
			copy3.addProperty(property3);
			copy4.addProperty(property4);
			copy5.addProperty(property5);
			copy6.addProperty(property6);
			copy7.addProperty(property7);
			copy8.addProperty(property8);
			copy9.addProperty(property9);
			copy10.addProperty(property10);
			copy11.addProperty(property11);
			copy12.addProperty(property12);
			copy13.addProperty(property13);
			copy14.addProperty(property14);
			copy15.addProperty(property15);
			copy16.addProperty(property16);
			copy17.addProperty(property17);
			copy18.addProperty(property18);
			copy19.addProperty(property19);
			copy20.addProperty(property20);
			
			return incidents;
			
		});

		groupATweakerMap.put(305, incident -> {
			//(Date Recovered Each component of the date must be valid; that is, months must be 01 through 12, 
			//days must be 01 through 31, and year must include the century (i.e., 19xx, 20xx). 
			//In addition, days cannot exceed maximum for the month (e.g., June cannot have 31 days). 
			//The date cannot be later than that entered within the Month of Electronic submission and 
			//Year of Electronic submission fields on the data record. For example, if Month of Electronic 
			//submission and Year of Electronic submission are 06/1999, the recovered date cannot contain 
			//any date 07/01/1999 or later. Cannot be earlier than Data Element 3 (Incident Date/Hour).
			
			//Date is later than tape submission
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("240");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			copy.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("5");
			property.setPropertyDescription(0, "03");
			property.setValueOfProperty(0, 000010000);
			property.setNumberOfRecoveredMotorVehicles(1);
			property.setDateRecovered(0, (Date.from(LocalDateTime.of(2017, 5, 12, 10, 7, 46).atZone(ZoneId.systemDefault()).toInstant())));
			//Date is earlier than incident
			GroupAIncidentReport copy2 = new GroupAIncidentReport(incident);
			copy2.getOffenses().get(0).setUcrOffenseCode("240");
			copy2.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy2.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy2.getOffenses().get(0).setBiasMotivation(0, "88");
			copy2.getOffenses().get(0).setLocationType("20");
			copy2.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy2.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property2 = new PropertySegment();
			property2.setTypeOfPropertyLoss("5");
			property2.setPropertyDescription(0, "03");
			property2.setValueOfProperty(0, 10000);
			property2.setNumberOfRecoveredMotorVehicles(1);
			property2.setDateRecovered(0, (Date.from(LocalDateTime.of(2015, 5, 12, 10, 7, 46).atZone(ZoneId.systemDefault()).toInstant())));
			//Invalid Year
			GroupAIncidentReport copy3 = new GroupAIncidentReport(incident);
			copy3.getOffenses().get(0).setUcrOffenseCode("240");
			copy3.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy3.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy3.getOffenses().get(0).setBiasMotivation(0, "88");
			copy3.getOffenses().get(0).setLocationType("20");
			copy3.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy3.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property3 = new PropertySegment();
			property3.setTypeOfPropertyLoss("5");
			property3.setPropertyDescription(0, "03");
			property3.setValueOfProperty(0, 10000);
			property3.setNumberOfRecoveredMotorVehicles(1);
			property3.setDateRecovered(0, (Date.from(LocalDateTime.of(16, 5, 12, 10, 7, 46).atZone(ZoneId.systemDefault()).toInstant())));
			//Invalid Month
			GroupAIncidentReport copy4 = new GroupAIncidentReport(incident);
			copy4.getOffenses().get(0).setUcrOffenseCode("240");
			copy4.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy4.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy4.getOffenses().get(0).setBiasMotivation(0, "88");
			copy4.getOffenses().get(0).setLocationType("20");
			copy4.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy4.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property4 = new PropertySegment();
			property4.setTypeOfPropertyLoss("5");
			property4.setPropertyDescription(0, "03");
			property4.setValueOfProperty(0, 10000);
			property4.setNumberOfRecoveredMotorVehicles(1);
			property4.setDateRecovered(0, (Date.from(LocalDateTime.of(2016, 13, 12, 10, 7, 46).atZone(ZoneId.systemDefault()).toInstant())));
			//Invalid days in month
			GroupAIncidentReport copy5 = new GroupAIncidentReport(incident);
			copy5.getOffenses().get(0).setUcrOffenseCode("240");
			copy5.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy5.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy5.getOffenses().get(0).setBiasMotivation(0, "88");
			copy5.getOffenses().get(0).setLocationType("20");
			copy5.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy5.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property5 = new PropertySegment();
			property5.setTypeOfPropertyLoss("5");
			property5.setPropertyDescription(0, "03");
			property5.setValueOfProperty(0, 100000);
			property5.setNumberOfRecoveredMotorVehicles(1);
			property5.setDateRecovered(0, (Date.from(LocalDateTime.of(2016, 5, 32, 10, 7, 46).atZone(ZoneId.systemDefault()).toInstant())));
			
			
			incidents.add(copy);
			incidents.add(copy2);
			incidents.add(copy3);
			incidents.add(copy4);
			incidents.add(copy5);
			copy.addProperty(property);
			copy2.addProperty(property2);
			copy3.addProperty(property3);
			copy4.addProperty(property4);
			copy5.addProperty(property5);
			
			
			return incidents;
		});
			
		groupATweakerMap.put(306, incident -> {	
			//(Suspected Drug Type) The referenced data element in error is one that contains multiple data values. 
			//When more than one code is entered, none can be duplicate codes.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("35A");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("6");
			property.setPropertyDescription(0, "10");
			property.setSuspectedDrugType(0, "A");
			property.setSuspectedDrugType(1, "B");
			property.setSuspectedDrugType(2, "B");
			property.setEstimatedDrugQuantity(0, 1.0);
			property.setEstimatedDrugQuantity(1, 1.0);
			property.setEstimatedDrugQuantity(2, 1.0);
			property.setTypeDrugMeasurement(0, "OZ");
			property.setTypeDrugMeasurement(1, "OZ");
			property.setTypeDrugMeasurement(2, "OZ");
			property.setValueOfProperty(0, null);
			property.setValueOfProperty(1, null);
			property.setValueOfProperty(2, null);
			//There are two exceptions to this rule:
			//When a data value is entered in both Drug Type 1 and Drug Type 2, but different measurement categories are 
			//entered in Data Element 22 (Type Drug Measurement); this is allowed. For example, when A=Crack Cocaine
			//is entered in Drug Type 1 and it is also entered in Drug Type 2, Data Element 22 
			//(Type Drug Measurement) must be two different measurement categories 
			//(i.e., grams and liters) and not grams and pounds (same weight category).
			GroupAIncidentReport copy2 = new GroupAIncidentReport(copy);
			copy2.getOffenses().get(0).setUcrOffenseCode("35A");
			copy2.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy2.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy2.getOffenses().get(0).setBiasMotivation(0, "88");
			copy2.getOffenses().get(0).setLocationType("20");
			PropertySegment property2 = new PropertySegment();
			property2.setTypeOfPropertyLoss("6");
			property2.setPropertyDescription(0, "10");
			property2.setSuspectedDrugType(0, "A");
			property2.setSuspectedDrugType(1, "A");
			property2.setEstimatedDrugQuantity(0, 1.0);
			property2.setEstimatedDrugQuantity(1, 2.0);
			property2.setTypeDrugMeasurement(0, "OZ");
			property2.setTypeDrugMeasurement(1, "OZ");
			property2.setValueOfProperty(0, null);
			property2.setValueOfProperty(1, null);
			//When the data value is U=Unknown; it can be entered only once.
			GroupAIncidentReport copy3 = new GroupAIncidentReport(copy);
			copy3.getOffenses().get(0).setUcrOffenseCode("35A");
			copy3.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy3.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy3.getOffenses().get(0).setBiasMotivation(0, "88");
			copy3.getOffenses().get(0).setLocationType("20");
			PropertySegment property3 = new PropertySegment();
			property3.setTypeOfPropertyLoss("6");
			property3.setPropertyDescription(0, "10");
			property3.setSuspectedDrugType(0, "U");
			property3.setSuspectedDrugType(1, "A");
			property3.setEstimatedDrugQuantity(0, 1.0);
			property3.setEstimatedDrugQuantity(1, 2.0);
			property3.setTypeDrugMeasurement(0, "OZ");
			property3.setTypeDrugMeasurement(1, "GM");
			property3.setValueOfProperty(0, null);
			property3.setValueOfProperty(1, null);
						
			incidents.add(copy);
			incidents.add(copy2);
			incidents.add(copy3);
			copy.addProperty(property);
			copy2.addProperty(property2);
			copy3.addProperty(property3);
			
			return incidents;
		});
			
		groupATweakerMap.put(320, incident -> {
			// (Date Recovered) cannot be earlier than the date entered in Data Element 3 (Incident Date)
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("240");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			copy.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("5");
			property.setPropertyDescription(0, "03");
			property.setValueOfProperty(0, 000000000);
			property.setNumberOfRecoveredMotorVehicles(1);
			property.setDateRecovered(0, (Date.from(LocalDateTime.of(2016, 4, 12, 10, 7, 46).atZone(ZoneId.systemDefault()).toInstant())));
			//**** CANNOT TEST FOR THE FOLLOWING CONDITION ****
			//The date property is recovered cannot be before the date it is stolen.
			//The exception to this rule is when recovered property is reported for a pre-NIBRS incident. 
			//In this case, Segment Level 3 (Property Segment) will contain A=Add, 
			//but the data value in Data Element 2 (Incident Number) will not match an incident already 
			//on file in the national UCR database. The segment will be processed, 
			//but used only for SRS purposes and will not be included in the agency�s NIBRS figures.
			
			incidents.add(copy);
			copy.addProperty(property);
			
			return incidents;
		});
		
		groupATweakerMap.put(342, incident -> {
			//(Value of PropertySegment) When referenced data element contains a value that exceeds an FBI-assigned 
			//threshold amount, a warning message will be created. The participant is asked to check to 
			//see if the value entered was a data entry error, or if it was intended to be entered. 
			//A warning message is always produced when the value is $1,000,000 or greater. 
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("240");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			copy.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("7");
			property.setPropertyDescription(0, "03");
			property.setValueOfProperty(0, 010000000);
			property.setNumberOfStolenMotorVehicles(1);
			
			incidents.add(copy);
			copy.addProperty(property);
			return incidents;
		});

		groupATweakerMap.put(351, incident -> {
			// Value of PropertySegment) cannot be zero unless Data Element 15 (PropertySegment Description) is:
			//Mandatory zero
			//09=Credit/Debit Cards
			//22=Non-negotiable Instruments
			//48=Documents Personal or Business
			//65=Identity Documents
			//66=Identity Intangible
			//Optional zero
			//77=Other
			//99=(blank)this data value is not currently used by the FBI
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("240");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			copy.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("7");
			property.setPropertyDescription(0, "03");
			property.setValueOfProperty(0, 000000000);
			property.setNumberOfStolenMotorVehicles(1);
			
			
			incidents.add(copy);
			copy.addProperty(property);
			
			return incidents;
		});

		groupATweakerMap.put(352, incident -> {
			//To-Do: Many possibly permutations of the 15-22 blank rule.
			//(Suspected Drug Type) When this error occurs, data were found in one or more of the 
			//referenced data elements. These data elements must be blank based on other data element 
			//values that prohibit data being entered in these data elements. 
			//For example, if Data Element 14 (Type property Loss/Etc.) is 8=Unknown, 
			//Data Elements 15 through 22 must be blank. 
			//If it is 1=None and offense is 35A, then Data Elements 15 through 19 and 
			//21 through 22 must be blank. If it is 1=None and offense is not 35A, 
			//then Data Elements 15 through 22 must be blank. 
			//The exception to this rule is when Data Element 6 (UCR Offense Code) is 
			//35A=Drug/ Narcotic Violations and Data Element 14 (Type Property Loss/Etc.) 
			//is 1=None; Data Element 20 (Suspected Drug Type) must be entered.
			//
			//Data Element 14 (Type property Loss/Etc.) is 8=Unknown, 
			//Data Elements 15 through 22 must be blank. 
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("220");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			copy.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("8");
			property.setPropertyDescription(0, null);
			property.setValueOfProperty(0, null);
			property.setDateRecovered(0, null);
			property.setNumberOfStolenMotorVehicles(null);
			property.setNumberOfRecoveredMotorVehicles(null);
			property.setSuspectedDrugType(0, null);
			property.setEstimatedDrugQuantity(0, 1.0);
			property.setTypeDrugMeasurement(0, null);
			
			//If it is 1=None and offense is 35A, then Data Elements 15 through 19 and 
			//21 through 22 must be blank.
			GroupAIncidentReport copy2 = new GroupAIncidentReport(incident);
			copy2.getOffenses().get(0).setUcrOffenseCode("35A");
			copy2.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy2.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy2.getOffenses().get(0).setBiasMotivation(0, "88");
			copy2.getOffenses().get(0).setLocationType("20");
			copy2.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy2.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property2 = new PropertySegment();
			property2.setTypeOfPropertyLoss("1");
			property2.setPropertyDescription(0, null);
			property2.setValueOfProperty(0, null);
			property2.setDateRecovered(0, null);
			property2.setNumberOfStolenMotorVehicles(null);
			property2.setNumberOfRecoveredMotorVehicles(null);
			property2.setSuspectedDrugType(1, "A");
			property2.setEstimatedDrugQuantity(0, 1.0);
			property2.setTypeDrugMeasurement(0, "OZ");
			
			//If it is 1=None and offense is not 35A, 
			//then Data Elements 15 through 22 must be blank. 
			GroupAIncidentReport copy3 = new GroupAIncidentReport(incident);
			copy3.getOffenses().get(0).setUcrOffenseCode("240");
			copy3.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy3.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy3.getOffenses().get(0).setBiasMotivation(0, "88");
			copy3.getOffenses().get(0).setLocationType("20");
			copy3.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy3.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property3 = new PropertySegment();
			property3.setTypeOfPropertyLoss("1");
			property3.setPropertyDescription(0, null);
			property3.setValueOfProperty(0, null);
			property3.setDateRecovered(0, null);
			property3.setNumberOfStolenMotorVehicles(null);
			property3.setNumberOfRecoveredMotorVehicles(null);
			property3.setSuspectedDrugType(1, "A");
			property3.setEstimatedDrugQuantity(0, 1.0);
			property3.setTypeDrugMeasurement(0, "OZ");
			
			//The exception to this rule is when Data Element 6 (UCR Offense Code) is 
			//35A=Drug/ Narcotic Violations and Data Element 14 (Type Property Loss/Etc.) 
			//is 1=None; Data Element 20 (Suspected Drug Type) must be entered.
			GroupAIncidentReport copy4 = new GroupAIncidentReport(incident);
			copy4.getOffenses().get(0).setUcrOffenseCode("35A");
			copy4.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy4.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy4.getOffenses().get(0).setBiasMotivation(0, "88");
			copy4.getOffenses().get(0).setLocationType("20");
			PropertySegment property4 = new PropertySegment();
			property4.setTypeOfPropertyLoss("1");
			property4.setPropertyDescription(0, null);
			property4.setValueOfProperty(0, null);
			property4.setDateRecovered(0, null);
			property4.setNumberOfStolenMotorVehicles(null);
			property4.setNumberOfRecoveredMotorVehicles(null);
			property4.setSuspectedDrugType(1, null);
			property4.setEstimatedDrugQuantity(0, 1.0);
			property4.setTypeDrugMeasurement(0, "OZ");
			
			
			incidents.add(copy);
			incidents.add(copy2);
			incidents.add(copy3);
			incidents.add(copy4);
			copy.addProperty(property);
			copy.addProperty(property2);
			copy.addProperty(property3);
			copy.addProperty(property4);
			
			return incidents;
		});
		
		
		groupATweakerMap.put(353, incident -> {
			//(Value of Property) is 88=Pending Inventory, but Data Element 16 
			//(Value of Property) is not $1. 
			//Determine which of the data elements was entered incorrectly.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("240");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			copy.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("7");
			property.setPropertyDescription(0, "88");
			property.setValueOfProperty(0, 10000);
			property.setNumberOfStolenMotorVehicles(1);
			
			
			incidents.add(copy);
			copy.addProperty(property);
			
			return incidents;
		});

			
		groupATweakerMap.put(354, incident -> {
			// Data Element 16 (Value of Property) contains a value,
			// but Data Element 15 (Property Description) was not entered.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("240");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			copy.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("7");
			property.setValueOfProperty(0, 10000);
			property.setNumberOfStolenMotorVehicles(1);
			
			incidents.add(copy);
			copy.addProperty(property);
			
			return incidents;
		});

		groupATweakerMap.put(355, incident -> {
			// (Type Of Property Loss) must be 5=Recovered for Data Element 17 (Date Recovered) to be entered.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("240");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			copy.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("7");
			property.setPropertyDescription(0, "03");
			property.setValueOfProperty(0, 000000000);
			property.setNumberOfStolenMotorVehicles(1);
			property.setDateRecovered(0, (Date.from(LocalDateTime.of(2016, 5, 12, 10, 7, 46).atZone(ZoneId.systemDefault()).toInstant())));
			
			
			incidents.add(copy);
			copy.addProperty(property);
			
			return incidents;
		});
		
		//groupATweakerMap.put(356, incident -> {
			//to-do: Rule is not clear.
			// (PropertySegment Description) was entered, but Data Elements 15 (PropertySegment Description)
			// and/or 16 (PropertySegment Value) were not entered.
			//List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			//GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			//PropertySegment valueOfProperty = new PropertySegment();
			//valueOfProperty.setValueOfProperty(0, null);
			//PropertySegment propertyDescription = new PropertySegment();
			//propertyDescription.setPropertyDescription(0, "01");
			//GroupAIncidentReport copy2 = new GroupAIncidentReport(incident);
			//PropertySegment valueOfProperty2 = new PropertySegment();
			//.setValueOfProperty(0, 000000500);
			//PropertySegment propertyDescription2 = new PropertySegment();
			//propertyDescription2.setPropertyDescription(0, null);

			//incidents.add(copy);
			//incidents.add(copy2);
			
			//return incidents;
		//});

		groupATweakerMap.put(357, incident -> {
			//(Number of Stolen Motor Vehicles) was entered. However, Data Element 14 (Type PropertySegment Loss/Etc.) 
			//7=Stolen/Etc. was not entered, and/or Data Element 6 (UCR OffenseSegment Code) of 240=Motor Vehicle Theft was not entered, 
			//and/or Data Element 7 (OffenseSegment Attempted/Completed) was A=Attempted.
			//
			//Type of Property Loss not 'Stolen'
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("240");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			copy.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("2");
			property.setPropertyDescription(0, "03");
			property.setValueOfProperty(0, 10000);
			property.setNumberOfStolenMotorVehicles(1);
			//UCR Code not 240
			GroupAIncidentReport copy2 = new GroupAIncidentReport(incident);
			copy2.getOffenses().get(0).setUcrOffenseCode("13A");
			copy2.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy2.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy2.getOffenses().get(0).setBiasMotivation(0, "88");
			copy2.getOffenses().get(0).setLocationType("20");
			copy2.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy2.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property2 = new PropertySegment();
			property2.setTypeOfPropertyLoss("7");
			property2.setPropertyDescription(0, "03");
			property2.setValueOfProperty(0, 10000);
			property2.setNumberOfStolenMotorVehicles(1);
			//OffenseAttempted/Completed is Attempted
			GroupAIncidentReport copy3 = new GroupAIncidentReport(incident);
			copy3.getOffenses().get(0).setUcrOffenseCode("240");
			copy3.getOffenses().get(0).setOffenseAttemptedCompleted("A");
			copy3.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy3.getOffenses().get(0).setBiasMotivation(0, "88");
			copy3.getOffenses().get(0).setLocationType("20");
			copy3.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy3.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property3 = new PropertySegment();
			property3.setTypeOfPropertyLoss("7");
			property3.setPropertyDescription(0, "03");
			property3.setValueOfProperty(0, 10000);
			property3.setNumberOfStolenMotorVehicles(1);
						
			incidents.add(copy);
			incidents.add(copy2);
			incidents.add(copy3);
			copy.addProperty(property);
			copy2.addProperty(property2);
			copy3.addProperty(property3);
			
			return incidents;
		});
			
		
		groupATweakerMap.put(358, incident -> {
			//(Number of Stolen Motor Vehicles) Entry must be made for Data Element 18 
			//(Number of Stolen Motor Vehicles) when Data Element 6 (UCR OffenseSegment Code) 
			//is 240=Motor Vehicle Theft, Data Element 7 (OffenseSegment Attempted/Completed) is C=Completed, and 
			//Data Element 14 (Type PropertySegment Loss/Etc.) is 7=Stolen/Etc.
			
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("240");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			copy.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("7");
			property.setPropertyDescription(0, "03");
			property.setValueOfProperty(0, 10000);
			
			incidents.add(copy);
			copy.addProperty(property);
		
			return incidents;
			
		});
		
		groupATweakerMap.put(359, incident -> {
			// (Property Description) Must be one of the following
			//03=Automobiles
			//05=Buses
			//24=Other Motor Vehicles
			//28=Recreational Vehicles
			//37=Trucks
			//when Data Element 18 (Number of Stolen Motor Vehicles) or Data Element 19 (Number of Recovered Motor Vehicles) 
			//contain a data value other than 00=Unknown:
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("240");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			copy.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("7");
			property.setPropertyDescription(0, "01");
			property.setNumberOfStolenMotorVehicles(1);
			property.setValueOfProperty(0, 10000);
			
			incidents.add(copy);
			copy.addProperty(property);
		
			return incidents;
			
		});
		
		groupATweakerMap.put(360, incident -> {
			//(Number of Recovered Motor Vehicles was entered. However, Data Element 14 
			//(Type PropertySegment Loss/Etc.) 5=Recovered was not entered, and/or Data Element 6 
			//(UCR OffenseSegment Code) of 240=Motor Vehicle Theft was not entered, and/or Data Element 7 
			//(OffenseSegment Attempted/Completed) was A=Attempted.
			//
			//Property Loss Type not Recovered
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("240");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			copy.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("2");
			property.setPropertyDescription(0, "03");
			property.setValueOfProperty(0, 10000);
			property.setNumberOfRecoveredMotorVehicles(1);
			//UCR OffenseSegment Code not = 240
			GroupAIncidentReport copy2 = new GroupAIncidentReport(incident);
			copy2.getOffenses().get(0).setUcrOffenseCode("220");
			copy2.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy2.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy2.getOffenses().get(0).setBiasMotivation(0, "88");
			copy2.getOffenses().get(0).setLocationType("20");
			copy2.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy2.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property2 = new PropertySegment();
			property2.setTypeOfPropertyLoss("5");
			property2.setPropertyDescription(0, "03");
			property2.setValueOfProperty(0, 10000);
			property2.setNumberOfRecoveredMotorVehicles(1);
			//Offense Attempted/Completed not completed
			GroupAIncidentReport copy3 = new GroupAIncidentReport(incident);
			copy3.getOffenses().get(0).setUcrOffenseCode("240");
			copy3.getOffenses().get(0).setOffenseAttemptedCompleted("A");
			copy3.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy3.getOffenses().get(0).setBiasMotivation(0, "88");
			copy3.getOffenses().get(0).setLocationType("20");
			copy3.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy3.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property3 = new PropertySegment();
			property3.setTypeOfPropertyLoss("5");
			property3.setPropertyDescription(0, "03");
			property3.setValueOfProperty(0, 10000);
			property3.setNumberOfRecoveredMotorVehicles(1);
			
			
			incidents.add(copy);
			incidents.add(copy2);
			incidents.add(copy3);
			copy.addProperty(property);
			copy2.addProperty(property2);
			copy3.addProperty(property3);
		
			return incidents;
			
		});
			
		
		groupATweakerMap.put(361, incident -> {
			//((Number of Recovered Motor Vehicles) Entry must be made when Data Element 6 
			//(UCR OffenseSegment Code) is 240=Motor Vehicle Theft, Data Element 14 
			//(Type PropertySegment Loss/Etc.) is 5=Recovered, and Data Element 15 
			//(PropertySegment Description) contains a vehicle code.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("240");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			copy.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("5");
			property.setPropertyDescription(0, "03");
			property.setValueOfProperty(0, 10000);
						
			incidents.add(copy);
			copy.addProperty(property);
			
			
			return incidents;
			
		});
		
		
		groupATweakerMap.put(363 , incident -> {
			//(Estimated Drug Quantity) Since Data Element 20 (Suspected Drug Type) 
			//contains X=Over 3 Drug Types, Data Element 21 (Estimated Quantity) and 22 
			//(Type Measurement) must be blank
			//
			//EstimatedDrugQuantity not blank
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("35A");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("6");
			property.setPropertyDescription(0, "10");
			property.setValueOfProperty(0, 10000);
			property.setSuspectedDrugType(0, "X");
			property.setEstimatedDrugQuantity(0, 1.0);
			property.setTypeDrugMeasurement(0, null);
			//TypeDrugMeasurement not blank
			GroupAIncidentReport copy2 = new GroupAIncidentReport(incident);
			copy2.getOffenses().get(0).setUcrOffenseCode("35A");
			copy2.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy2.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy2.getOffenses().get(0).setBiasMotivation(0, "88");
			copy2.getOffenses().get(0).setLocationType("20");
			PropertySegment property2 = new PropertySegment();
			property2.setTypeOfPropertyLoss("6");
			property2.setPropertyDescription(0, "10");
			property2.setValueOfProperty(0, 10000);
			property2.setSuspectedDrugType(0, "X");
			property2.setEstimatedDrugQuantity(0, null);
			property2.setTypeDrugMeasurement(0, "OZ");
			
			incidents.add(copy);
			incidents.add(copy2);
			copy.addProperty(property);
			copy2.addProperty(property2);
			
			return incidents;
			
		});
			
		groupATweakerMap.put(364 , incident -> {
			//(Estimated Drug Quantity) When Data Element 6 (UCR Offense Code) is 
			//35A=Drug/Narcotic Violations, 14 (Type Property Loss/Etc.) is 6=Seized, 
			//15 (Type Property Description) is 10=Drugs, and Data Element 20 
			//(Suspected Drug Type) is entered, both Data Element 21 (Estimated Quantity) and 22 
			//(Type Measurement) must also be entered.
			//Estimated Quantity blank
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("35A");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("6");
			property.setPropertyDescription(0, "10");
			property.setValueOfProperty(0, 10000);
			property.setSuspectedDrugType(0, "B");
			property.setEstimatedDrugQuantity(0, null);
			property.setTypeDrugMeasurement(0, "OZ");
			//TypeDrugMeasurement blank
			GroupAIncidentReport copy2 = new GroupAIncidentReport(incident);
			copy2.getOffenses().get(0).setUcrOffenseCode("35A");
			copy2.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy2.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy2.getOffenses().get(0).setBiasMotivation(0, "88");
			copy2.getOffenses().get(0).setLocationType("20");
			PropertySegment property2 = new PropertySegment();
			property2.setTypeOfPropertyLoss("6");
			property2.setPropertyDescription(0, "10");
			property2.setValueOfProperty(0, 10000);
			property2.setSuspectedDrugType(0, "B");
			property2.setEstimatedDrugQuantity(0, 1.0);
			property2.setTypeDrugMeasurement(0, null);
			
			
			incidents.add(copy);
			incidents.add(copy2);
			copy.addProperty(property);
			copy2.addProperty(property2);
			
			return incidents;
			
		});
			
		groupATweakerMap.put(367, incident -> {
			//(Type Measurement) Data Element 22 (Type Measurement) was entered with 
			//NP in combination with an illogical drug type. Based upon the various ways a 
			//drug can be measured, very few edits can be done to check for illogical 
			//combinations of drug type and measurement. The only restriction will be to 
			//limit NP=Number of Plants to the following drugs:
			//	DRUG MEASUREMENT
			//	E=Marijuana NP
			//	G=Opium NP
			//	K=Other Hallucinogens NP
			//	All other Data Element 22 (Type Measurement) codes are applicable to any Data Element 20 
			//(Suspected Drug Type) code.			
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("35A");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("6");
			property.setPropertyDescription(0, "10");
			property.setSuspectedDrugType(0, "A");
			property.setEstimatedDrugQuantity(0, 1.0);
			property.setTypeDrugMeasurement(0, "NP");
			property.setValueOfProperty(0, 10000);
			GroupAIncidentReport copy2 = new GroupAIncidentReport(incident);
			copy2.getOffenses().get(0).setUcrOffenseCode("35A");
			copy2.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy2.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy2.getOffenses().get(0).setBiasMotivation(0, "88");
			copy2.getOffenses().get(0).setLocationType("20");
			PropertySegment property2 = new PropertySegment();
			property2.setTypeOfPropertyLoss("6");
			property2.setPropertyDescription(0, "10");
			property2.setSuspectedDrugType(0, "B");
			property2.setEstimatedDrugQuantity(0, 1.0);
			property2.setTypeDrugMeasurement(0, "NP");
			property2.setValueOfProperty(0, 10000);
			GroupAIncidentReport copy3 = new GroupAIncidentReport(incident);
			copy3.getOffenses().get(0).setUcrOffenseCode("35A");
			copy3.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy3.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy3.getOffenses().get(0).setBiasMotivation(0, "88");
			copy3.getOffenses().get(0).setLocationType("20");
			PropertySegment property3 = new PropertySegment();
			property3.setTypeOfPropertyLoss("6");
			property3.setPropertyDescription(0, "10");
			property3.setSuspectedDrugType(0, "C");
			property3.setEstimatedDrugQuantity(0, 1.0);
			property3.setTypeDrugMeasurement(0, "NP");
			property3.setValueOfProperty(0, 10000);
			GroupAIncidentReport copy4 = new GroupAIncidentReport(incident);
			copy4.getOffenses().get(0).setUcrOffenseCode("35A");
			copy4.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy4.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy4.getOffenses().get(0).setBiasMotivation(0, "88");
			copy4.getOffenses().get(0).setLocationType("20");
			PropertySegment property4 = new PropertySegment();
			property4.setTypeOfPropertyLoss("6");
			property4.setPropertyDescription(0, "10");
			property4.setSuspectedDrugType(0, "D");
			property4.setEstimatedDrugQuantity(0, 1.0);
			property4.setTypeDrugMeasurement(0, "NP");
			property4.setValueOfProperty(0, 10000);
			GroupAIncidentReport copy5 = new GroupAIncidentReport(incident);
			copy5.getOffenses().get(0).setUcrOffenseCode("35A");
			copy5.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy5.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy5.getOffenses().get(0).setBiasMotivation(0, "88");
			copy5.getOffenses().get(0).setLocationType("20");
			PropertySegment property5 = new PropertySegment();
			property5.setTypeOfPropertyLoss("6");
			property5.setPropertyDescription(0, "10");
			property5.setSuspectedDrugType(0, "F");
			property5.setEstimatedDrugQuantity(0, 1.0);
			property5.setTypeDrugMeasurement(0, "NP");
			property5.setValueOfProperty(0, 10000);
			GroupAIncidentReport copy6 = new GroupAIncidentReport(incident);
			copy6.getOffenses().get(0).setUcrOffenseCode("35A");
			copy6.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy6.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy6.getOffenses().get(0).setBiasMotivation(0, "88");
			copy6.getOffenses().get(0).setLocationType("20");
			PropertySegment property6 = new PropertySegment();
			property6.setTypeOfPropertyLoss("6");
			property6.setPropertyDescription(0, "10");
			property6.setSuspectedDrugType(0, "H");
			property6.setEstimatedDrugQuantity(0, 1.0);
			property6.setTypeDrugMeasurement(0, "NP");
			property6.setValueOfProperty(0, 10000);
			GroupAIncidentReport copy7 = new GroupAIncidentReport(incident);
			copy7.getOffenses().get(0).setUcrOffenseCode("35A");
			copy7.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy7.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy7.getOffenses().get(0).setBiasMotivation(0, "88");
			copy7.getOffenses().get(0).setLocationType("20");
			PropertySegment property7 = new PropertySegment();
			property7.setTypeOfPropertyLoss("6");
			property7.setPropertyDescription(0, "10");
			property7.setSuspectedDrugType(0, "I");
			property7.setEstimatedDrugQuantity(0, 1.0);
			property7.setTypeDrugMeasurement(0, "NP");
			property7.setValueOfProperty(0, 10000);
			GroupAIncidentReport copy8 = new GroupAIncidentReport(incident);
			copy8.getOffenses().get(0).setUcrOffenseCode("35A");
			copy8.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy8.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy8.getOffenses().get(0).setBiasMotivation(0, "88");
			copy8.getOffenses().get(0).setLocationType("20");
			PropertySegment property8 = new PropertySegment();
			property8.setTypeOfPropertyLoss("6");
			property8.setPropertyDescription(0, "10");
			property8.setSuspectedDrugType(0, "J");
			property8.setEstimatedDrugQuantity(0, 1.0);
			property8.setTypeDrugMeasurement(0, "NP");
			property8.setValueOfProperty(0, 10000);
			GroupAIncidentReport copy9 = new GroupAIncidentReport(incident);
			copy9.getOffenses().get(0).setUcrOffenseCode("35A");
			copy9.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy9.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy9.getOffenses().get(0).setBiasMotivation(0, "88");
			copy9.getOffenses().get(0).setLocationType("20");
			PropertySegment property9 = new PropertySegment();
			property9.setTypeOfPropertyLoss("6");
			property9.setPropertyDescription(0, "10");
			property9.setSuspectedDrugType(0, "L");
			property9.setEstimatedDrugQuantity(0, 1.0);
			property9.setTypeDrugMeasurement(0, "NP");
			property9.setValueOfProperty(0, 10000);
			GroupAIncidentReport copy10 = new GroupAIncidentReport(incident);
			copy10.getOffenses().get(0).setUcrOffenseCode("35A");
			copy10.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy10.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy10.getOffenses().get(0).setBiasMotivation(0, "88");
			copy10.getOffenses().get(0).setLocationType("20");
			PropertySegment property10 = new PropertySegment();
			property10.setTypeOfPropertyLoss("6");
			property10.setPropertyDescription(0, "10");
			property10.setSuspectedDrugType(0, "M");
			property10.setEstimatedDrugQuantity(0, 1.0);
			property10.setTypeDrugMeasurement(0, "NP");
			property10.setValueOfProperty(0, 10000);
			GroupAIncidentReport copy11 = new GroupAIncidentReport(incident);
			copy11.getOffenses().get(0).setUcrOffenseCode("35A");
			copy11.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy11.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy11.getOffenses().get(0).setBiasMotivation(0, "88");
			copy11.getOffenses().get(0).setLocationType("20");
			PropertySegment property11 = new PropertySegment();
			property11.setTypeOfPropertyLoss("6");
			property11.setPropertyDescription(0, "10");
			property11.setSuspectedDrugType(0, "N");
			property11.setEstimatedDrugQuantity(0, 1.0);
			property11.setTypeDrugMeasurement(0, "NP");
			property11.setValueOfProperty(0, 10000);
			GroupAIncidentReport copy12 = new GroupAIncidentReport(incident);
			copy12.getOffenses().get(0).setUcrOffenseCode("35A");
			copy12.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy12.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy12.getOffenses().get(0).setBiasMotivation(0, "88");
			copy12.getOffenses().get(0).setLocationType("20");
			PropertySegment property12 = new PropertySegment();
			property12.setTypeOfPropertyLoss("6");
			property12.setPropertyDescription(0, "10");
			property12.setSuspectedDrugType(0, "O");
			property12.setEstimatedDrugQuantity(0, 1.0);
			property12.setTypeDrugMeasurement(0, "NP");
			property12.setValueOfProperty(0, 10000);
			GroupAIncidentReport copy13 = new GroupAIncidentReport(incident);
			copy13.getOffenses().get(0).setUcrOffenseCode("35A");
			copy13.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy13.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy13.getOffenses().get(0).setBiasMotivation(0, "88");
			copy13.getOffenses().get(0).setLocationType("20");
			PropertySegment property13 = new PropertySegment();
			property13.setTypeOfPropertyLoss("6");
			property13.setPropertyDescription(0, "10");
			property13.setSuspectedDrugType(0, "P");
			property13.setEstimatedDrugQuantity(0, 1.0);
			property13.setTypeDrugMeasurement(0, "NP");
			property13.setValueOfProperty(0, 10000);
			GroupAIncidentReport copy14 = new GroupAIncidentReport(incident);
			copy14.getOffenses().get(0).setUcrOffenseCode("35A");
			copy14.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy14.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy14.getOffenses().get(0).setBiasMotivation(0, "88");
			copy14.getOffenses().get(0).setLocationType("20");
			PropertySegment property14 = new PropertySegment();
			property14.setTypeOfPropertyLoss("6");
			property14.setPropertyDescription(0, "10");
			property14.setSuspectedDrugType(0, "U");
			property14.setEstimatedDrugQuantity(0, 1.0);
			property14.setTypeDrugMeasurement(0, "NP");
			property14.setValueOfProperty(0, 10000);
			
			incidents.add(copy);
			incidents.add(copy2);
			incidents.add(copy3);
			incidents.add(copy4);
			incidents.add(copy5);
			incidents.add(copy6);
			incidents.add(copy7);
			incidents.add(copy8);
			incidents.add(copy9);
			incidents.add(copy10);
			incidents.add(copy11);
			incidents.add(copy12);
			incidents.add(copy13);
			incidents.add(copy14);
			
			copy.addProperty(property);
			copy.addProperty(property2);
			copy.addProperty(property3);
			copy.addProperty(property4);
			copy.addProperty(property5);
			copy.addProperty(property6);
			copy.addProperty(property7);
			copy.addProperty(property8);
			copy.addProperty(property9);
			copy.addProperty(property10);
			copy.addProperty(property11);
			copy.addProperty(property12);
			copy.addProperty(property13);
			copy.addProperty(property14);
			
			return incidents;
		});	
		groupATweakerMap.put(372, incident -> {
			//(Type of PropertySegment Loss) is 
			//2=Burned
			//3=Counterfeited/Forged
			//4=Destroyed/Damaged/Vandalized
			//5=Recovered
			//6=Seized
			//7=Stolen/Etc.
			//Data Elements 15 through 22 must have applicable entries in the segment.
			//
			//2=Burned
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("240");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			copy.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("2");
			//3=Counterfeited/Forged
			GroupAIncidentReport copy2 = new GroupAIncidentReport(incident);
			copy2.getOffenses().get(0).setUcrOffenseCode("240");
			copy2.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy2.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy2.getOffenses().get(0).setBiasMotivation(0, "88");
			copy2.getOffenses().get(0).setLocationType("20");
			copy2.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy2.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property2 = new PropertySegment();
			property2.setTypeOfPropertyLoss("3");
			//4=Destroyed/Damaged/Vandalized
			GroupAIncidentReport copy3 = new GroupAIncidentReport(incident);
			copy3.getOffenses().get(0).setUcrOffenseCode("240");
			copy3.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy3.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy3.getOffenses().get(0).setBiasMotivation(0, "88");
			copy3.getOffenses().get(0).setLocationType("20");
			copy3.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy3.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property3 = new PropertySegment();
			property3.setTypeOfPropertyLoss("4");
			//5=Recovered
			GroupAIncidentReport copy4 = new GroupAIncidentReport(incident);
			copy4.getOffenses().get(0).setUcrOffenseCode("240");
			copy4.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy4.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy4.getOffenses().get(0).setBiasMotivation(0, "88");
			copy4.getOffenses().get(0).setLocationType("20");
			copy4.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy4.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property4 = new PropertySegment();
			property4.setTypeOfPropertyLoss("5");
			//6=Seized
			GroupAIncidentReport copy5 = new GroupAIncidentReport(incident);
			copy5.getOffenses().get(0).setUcrOffenseCode("240");
			copy5.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy5.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy5.getOffenses().get(0).setBiasMotivation(0, "88");
			copy5.getOffenses().get(0).setLocationType("20");
			copy5.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy5.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property5 = new PropertySegment();
			property5.setTypeOfPropertyLoss("6");
			//7=Stolen
			GroupAIncidentReport copy6 = new GroupAIncidentReport(incident);
			copy6.getOffenses().get(0).setUcrOffenseCode("240");
			copy6.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy6.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy6.getOffenses().get(0).setBiasMotivation(0, "88");
			copy6.getOffenses().get(0).setLocationType("20");
			copy6.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy6.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property6 = new PropertySegment();
			property.setTypeOfPropertyLoss("7");
			
				
			incidents.add(copy);
			incidents.add(copy2);
			incidents.add(copy3);
			incidents.add(copy4);
			incidents.add(copy5);
			incidents.add(copy6);
			copy.addProperty(property);
			copy2.addProperty(property2);
			copy3.addProperty(property3);
			copy4.addProperty(property4);
			copy5.addProperty(property5);
			copy6.addProperty(property6);
								
			return incidents;
		});
		
		groupATweakerMap.put(375, incident -> {
			// to - do what's the difference between 372 and 375?
			//(Property Description) At least one Data Element 15 (Property Description) code 
			//must be entered when Data Element 14 (Type Property Loss/Etc.) contains Property Segment(s) for:
			//2=Burned
			//3=Counterfeited/Forged
			//4=Destroyed/Damaged/Vandalized
			//5=Recovered
			//6=Seized
			//7=Stolen/Etc.
			//
			//2=Burned
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("240");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			copy.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("2");
			//3=Counterfeited/Forged
			GroupAIncidentReport copy2 = new GroupAIncidentReport(incident);
			copy2.getOffenses().get(0).setUcrOffenseCode("240");
			copy2.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy2.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy2.getOffenses().get(0).setBiasMotivation(0, "88");
			copy2.getOffenses().get(0).setLocationType("20");
			copy2.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy2.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property2 = new PropertySegment();
			property2.setTypeOfPropertyLoss("3");
			//4=Destroyed/Damaged/Vandalized
			GroupAIncidentReport copy3 = new GroupAIncidentReport(incident);
			copy3.getOffenses().get(0).setUcrOffenseCode("240");
			copy3.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy3.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy3.getOffenses().get(0).setBiasMotivation(0, "88");
			copy3.getOffenses().get(0).setLocationType("20");
			copy3.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy3.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property3 = new PropertySegment();
			property3.setTypeOfPropertyLoss("4");
			//5=Recovered
			GroupAIncidentReport copy4 = new GroupAIncidentReport(incident);
			copy4.getOffenses().get(0).setUcrOffenseCode("240");
			copy4.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy4.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy4.getOffenses().get(0).setBiasMotivation(0, "88");
			copy4.getOffenses().get(0).setLocationType("20");
			copy4.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy4.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property4 = new PropertySegment();
			property4.setTypeOfPropertyLoss("5");
			//6=Seized
			GroupAIncidentReport copy5 = new GroupAIncidentReport(incident);
			copy5.getOffenses().get(0).setUcrOffenseCode("240");
			copy5.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy5.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy5.getOffenses().get(0).setBiasMotivation(0, "88");
			copy5.getOffenses().get(0).setLocationType("20");
			copy5.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy5.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property5 = new PropertySegment();
			property5.setTypeOfPropertyLoss("6");
			//7=Stolen
			GroupAIncidentReport copy6 = new GroupAIncidentReport(incident);
			copy6.getOffenses().get(0).setUcrOffenseCode("240");
			copy6.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy6.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy6.getOffenses().get(0).setBiasMotivation(0, "88");
			copy6.getOffenses().get(0).setLocationType("20");
			copy6.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy6.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property6 = new PropertySegment();
			property.setTypeOfPropertyLoss("7");
			
				
			incidents.add(copy);
			incidents.add(copy2);
			incidents.add(copy3);
			incidents.add(copy4);
			incidents.add(copy5);
			incidents.add(copy6);
			copy.addProperty(property);
			copy2.addProperty(property2);
			copy3.addProperty(property3);
			copy4.addProperty(property4);
			copy5.addProperty(property5);
			copy6.addProperty(property6);
								
			return incidents;
		});
		
		
		
	
		groupATweakerMap.put(383, incident -> {
			//(Value of PropertySegment) has a value other than zero entered. 
			//Since Data Element 15 (PropertySegment Description) code is 10=Drugs/Narcotics and the only Crime Against PropertySegment 
			//offense submitted is a 35A=Drug/Narcotic Violations, Data Element 16 (Value of PropertySegment) must be blank.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("35A");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			copy.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("6");
			property.setPropertyDescription(0, "10");
			property.setSuspectedDrugType(1, "A");
			property.setEstimatedDrugQuantity(0, 1.0);
			property.setTypeDrugMeasurement(0, "OZ");
			property.setValueOfProperty(0, 10000);
			
			
			incidents.add(copy);
			copy.addProperty(property);
						
			return incidents;
		});
		
		groupATweakerMap.put(384 , incident -> {
			//Data Element 21 (Estimated Drug Quantity) must be 000000001000=None (i.e., 1) 
			//when Data Element 22 (Type Drug Measurement) is XX=Not Reported 
			//indicating the drugs were sent to a laboratory for analysis. 
			//When the drug analysis is received by the LEA, Data Element 21 and 
			//Data Element 22 should be updated with the correct data values.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("35A");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			copy.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("6");
			property.setPropertyDescription(0, "10");
			property.setValueOfProperty(0, 10000);
			property.setSuspectedDrugType(0, "B");
			property.setEstimatedDrugQuantity(0, 1.0);
			property.setTypeDrugMeasurement(0, "XX");
			
			incidents.add(copy);
			copy.addProperty(property);
			return incidents;
			
		});
		
			groupATweakerMap.put(387, incident -> {
			//(PropertySegment Description) To ensure that 35A-35B Drug/Narcotic Offenses-Drug Equipment 
			//Violations are properly reported, Data Element 15 (PropertySegment Description) of 11=Drug/Narcotic Equipment 
			//is not allowed with only a 35A Drug/Narcotic Violation. Similarly, 10=Drugs/Narcotics is not 
			//allowed with only a 35B Drug Equipment Violation. And Data Element 14 (Type PropertySegment Loss/Etc.) is 6=Seized.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("35A");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			copy.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("6");
			property.setPropertyDescription(0, "11");
			property.setSuspectedDrugType(1, "A");
			property.setEstimatedDrugQuantity(0, 1.0);
			property.setTypeDrugMeasurement(0, "OZ");
			property.setValueOfProperty(0, null);
			
			//35B with Property Description Drugs/Narcotic
			GroupAIncidentReport copy2 = new GroupAIncidentReport(incident);
			copy2.getOffenses().get(0).setUcrOffenseCode("35B");
			copy2.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy2.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy2.getOffenses().get(0).setBiasMotivation(0, "88");
			copy2.getOffenses().get(0).setLocationType("20");
			copy2.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy2.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property2 = new PropertySegment();
			property2.setTypeOfPropertyLoss("6");
			property2.setPropertyDescription(0, "10");
			property2.setSuspectedDrugType(1, "A");
			property2.setEstimatedDrugQuantity(0, 1.0);
			property2.setTypeDrugMeasurement(0, "OZ");
			property2.setValueOfProperty(0, null);
					
					
			incidents.add(copy);
			incidents.add(copy2);
			copy.addProperty(property);
			copy2.addProperty(property2);
			
			return incidents;
		});
		
			groupATweakerMap.put(388, incident -> {
				//(Number of Stolen Motor Vehicles) More than one vehicle code was entered in 
				//Data Element 15 (PropertySegment Description), but the number stolen in Data Element 18 
				//(Number of Stolen Motor Vehicles) is less than this number. 
				//For example, if vehicle codes of 03=Automobiles and 05=Buses were entered as being stolen, 
				//then the number stolen must be at least 2, unless the number stolen was unknown (00).
				//The exception to this rule is when 00=Unknown is entered in Data Element 18.
				
				List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
				GroupAIncidentReport copy = new GroupAIncidentReport(incident);
				copy.getOffenses().get(0).setUcrOffenseCode("240");
				copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
				copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
				copy.getOffenses().get(0).setBiasMotivation(0, "88");
				copy.getOffenses().get(0).setLocationType("20");
				copy.getOffenses().get(0).setNumberOfPremisesEntered(1);
				copy.getOffenses().get(0).setMethodOfEntry("N");
				PropertySegment property = new PropertySegment();
				property.setTypeOfPropertyLoss("7");
				property.setPropertyDescription(0, "03");
				property.setValueOfProperty(0, 000010000);
				property.setPropertyDescription(1, "05");
				property.setValueOfProperty(1, 000020000);
				property.setNumberOfStolenMotorVehicles(1);
				
						
				incidents.add(copy);
				copy.addProperty(property);
			
				return incidents;
				
			});
			
			
			groupATweakerMap.put(389, incident -> {
				//(Number of Recovered Motor Vehicles) More than one vehicle code was entered in 
				//Data Element 15 (PropertySegment Description), but the number recovered in Data Element 18 
				//(Number of Recovered Motor Vehicles) is less than this number. 
				//For example, if vehicle codes of 03=Automobiles and 05=Buses were entered as being recovered, 
				//then the number recovered must be at least 2, unless the number recovered was unknown (00).
				//The exception to this rule is when 00=Unknown is entered in Data Element 18.
				
				List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
				GroupAIncidentReport copy = new GroupAIncidentReport(incident);
				copy.getOffenses().get(0).setUcrOffenseCode("240");
				copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
				copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
				copy.getOffenses().get(0).setBiasMotivation(0, "88");
				copy.getOffenses().get(0).setLocationType("20");
				copy.getOffenses().get(0).setNumberOfPremisesEntered(1);
				copy.getOffenses().get(0).setMethodOfEntry("N");
				PropertySegment property = new PropertySegment();
				property.setTypeOfPropertyLoss("5");
				property.setPropertyDescription(0, "03");
				property.setValueOfProperty(0, 000010000);
				property.setPropertyDescription(1, "05");
				property.setValueOfProperty(1, 000020000);
				property.setNumberOfRecoveredMotorVehicles(1);
				property.setDateRecovered(0, (Date.from(LocalDateTime.of(2016, 5, 12, 10, 7, 46).atZone(ZoneId.systemDefault()).toInstant())));
						
				incidents.add(copy);
				copy.addProperty(property);
				return incidents;
				
			});

		groupATweakerMap.put(390, incident -> {
			// (PropertySegment Description) must contain a data value that is logical for one or more of the offenses 
			//entered in Data Element 6 (UCR OffenseSegment Code).
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			//PropertySegment descriptions for structures are illogical with 220=Burglary/Breaking 
			//& Entering or 240=Motor Vehicle Theft
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("220");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			copy.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("7");
			property.setPropertyDescription(0, "29");
			property.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy2 = new GroupAIncidentReport(incident);			
			copy2.getOffenses().get(0).setUcrOffenseCode("240");
			copy2.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy2.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy2.getOffenses().get(0).setBiasMotivation(0, "88");
			copy2.getOffenses().get(0).setLocationType("20");
			copy2.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy2.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property2= new PropertySegment();
			property2.setTypeOfPropertyLoss("7");
			property2.setPropertyDescription(0, "29");
			property2.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy3 = new GroupAIncidentReport(incident);
			copy3.getOffenses().get(0).setUcrOffenseCode("220");
			copy3.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy3.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy3.getOffenses().get(0).setBiasMotivation(0, "88");
			copy3.getOffenses().get(0).setLocationType("20");
			copy3.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy3.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property3 = new PropertySegment();
			property3.setTypeOfPropertyLoss("7");
			property3.setPropertyDescription(0, "30");
			property3.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy4 = new GroupAIncidentReport(incident);
			copy4.getOffenses().get(0).setUcrOffenseCode("240");
			copy4.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy4.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy4.getOffenses().get(0).setBiasMotivation(0, "88");
			copy4.getOffenses().get(0).setLocationType("20");
			copy4.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy4.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property4= new PropertySegment();
			property4.setTypeOfPropertyLoss("7");
			property4.setPropertyDescription(0, "30");
			property4.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy5 = new GroupAIncidentReport(incident);
			copy5.getOffenses().get(0).setUcrOffenseCode("220");
			copy5.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy5.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy5.getOffenses().get(0).setBiasMotivation(0, "88");
			copy5.getOffenses().get(0).setLocationType("20");
			copy5.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy5.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property5 = new PropertySegment();
			property5.setTypeOfPropertyLoss("7");
			property5.setPropertyDescription(0, "31");
			property5.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy6 = new GroupAIncidentReport(incident);			
			copy6.getOffenses().get(0).setUcrOffenseCode("240");
			copy6.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy6.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy6.getOffenses().get(0).setBiasMotivation(0, "88");
			copy6.getOffenses().get(0).setLocationType("20");
			copy6.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy6.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property6= new PropertySegment();
			property6.setTypeOfPropertyLoss("7");
			property6.setPropertyDescription(0, "31");
			property6.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy7 = new GroupAIncidentReport(incident);
			copy7.getOffenses().get(0).setUcrOffenseCode("220");
			copy7.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy7.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy7.getOffenses().get(0).setBiasMotivation(0, "88");
			copy7.getOffenses().get(0).setLocationType("20");
			copy7.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy7.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property7 = new PropertySegment();
			property7.setTypeOfPropertyLoss("7");
			property7.setPropertyDescription(0, "32");
			property7.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy8 = new GroupAIncidentReport(incident);			
			copy8.getOffenses().get(0).setUcrOffenseCode("240");
			copy8.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy8.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy8.getOffenses().get(0).setBiasMotivation(0, "88");
			copy8.getOffenses().get(0).setLocationType("20");
			copy8.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy8.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property8= new PropertySegment();
			property8.setTypeOfPropertyLoss("7");
			property8.setPropertyDescription(0, "32");
			property8.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy9 = new GroupAIncidentReport(incident);
			copy9.getOffenses().get(0).setUcrOffenseCode("220");
			copy9.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy9.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy9.getOffenses().get(0).setBiasMotivation(0, "88");
			copy9.getOffenses().get(0).setLocationType("20");
			copy9.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy9.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property9 = new PropertySegment();
			property9.setTypeOfPropertyLoss("7");
			property9.setPropertyDescription(0, "33");
			property9.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy10 = new GroupAIncidentReport(incident);			
			copy10.getOffenses().get(0).setUcrOffenseCode("240");
			copy10.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy10.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy10.getOffenses().get(0).setBiasMotivation(0, "88");
			copy10.getOffenses().get(0).setLocationType("20");
			copy10.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy10.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property10= new PropertySegment();
			property10.setTypeOfPropertyLoss("7");
			property10.setPropertyDescription(0, "33");
			property10.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy11 = new GroupAIncidentReport(incident);
			copy11.getOffenses().get(0).setUcrOffenseCode("220");
			copy11.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy11.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy11.getOffenses().get(0).setBiasMotivation(0, "88");
			copy11.getOffenses().get(0).setLocationType("20");
			copy11.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy11.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property11 = new PropertySegment();
			property11.setTypeOfPropertyLoss("7");
			property11.setPropertyDescription(0, "34");
			property11.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy12 = new GroupAIncidentReport(incident);			
			copy12.getOffenses().get(0).setUcrOffenseCode("240");
			copy12.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy12.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy12.getOffenses().get(0).setBiasMotivation(0, "88");
			copy12.getOffenses().get(0).setLocationType("20");
			copy12.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy12.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property12= new PropertySegment();
			property12.setTypeOfPropertyLoss("7");
			property12.setPropertyDescription(0, "34");
			property12.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy13 = new GroupAIncidentReport(incident);
			copy13.getOffenses().get(0).setUcrOffenseCode("220");
			copy13.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy13.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy13.getOffenses().get(0).setBiasMotivation(0, "88");
			copy13.getOffenses().get(0).setLocationType("20");
			copy13.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy13.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property13 = new PropertySegment();
			property13.setTypeOfPropertyLoss("7");
			property13.setPropertyDescription(0, "35");
			property13.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy14 = new GroupAIncidentReport(incident);			
			copy14.getOffenses().get(0).setUcrOffenseCode("240");
			copy14.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy14.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy14.getOffenses().get(0).setBiasMotivation(0, "88");
			copy14.getOffenses().get(0).setLocationType("20");
			copy14.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy14.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property14= new PropertySegment();
			property14.setTypeOfPropertyLoss("7");
			property14.setPropertyDescription(0, "35");
			property14.setValueOfProperty(0, 000010000);
			//PropertySegment descriptions for items that would not fit in a purse or pocket (aircraft, vehicles, structures, 
			//a person's identity, watercraft, etc.) are illogical with 23A=Pocket-picking or 23B=Purse-snatching
			//Aircraft
			GroupAIncidentReport copy15 = new GroupAIncidentReport(incident);
			copy15.getOffenses().get(0).setUcrOffenseCode("23A");
			copy15.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy15.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy15.getOffenses().get(0).setBiasMotivation(0, "88");
			copy15.getOffenses().get(0).setLocationType("20");
			copy15.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy15.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property15 = new PropertySegment();
			property15.setTypeOfPropertyLoss("7");
			property15.setPropertyDescription(0, "01");
			property15.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy16 = new GroupAIncidentReport(incident);			
			copy16.getOffenses().get(0).setUcrOffenseCode("23B");
			copy16.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy16.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy16.getOffenses().get(0).setBiasMotivation(0, "88");
			copy16.getOffenses().get(0).setLocationType("20");
			copy16.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy16.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property16= new PropertySegment();
			property16.setTypeOfPropertyLoss("7");
			property16.setPropertyDescription(0, "01");
			property16.setValueOfProperty(0, 000010000);
			//Automobiles
			GroupAIncidentReport copy17 = new GroupAIncidentReport(incident);
			copy17.getOffenses().get(0).setUcrOffenseCode("23A");
			copy17.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy17.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy17.getOffenses().get(0).setBiasMotivation(0, "88");
			copy17.getOffenses().get(0).setLocationType("20");
			copy17.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy17.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property17 = new PropertySegment();
			property17.setTypeOfPropertyLoss("7");
			property17.setPropertyDescription(0, "03");
			property17.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy18 = new GroupAIncidentReport(incident);			
			copy18.getOffenses().get(0).setUcrOffenseCode("23B");
			copy18.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy18.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy18.getOffenses().get(0).setBiasMotivation(0, "88");
			copy18.getOffenses().get(0).setLocationType("20");
			copy18.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy18.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property18= new PropertySegment();
			property18.setTypeOfPropertyLoss("7");
			property18.setPropertyDescription(0, "03");
			property18.setValueOfProperty(0, 000010000);
			//Bicycles
			GroupAIncidentReport copy19 = new GroupAIncidentReport(incident);
			copy19.getOffenses().get(0).setUcrOffenseCode("23A");
			copy19.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy19.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy19.getOffenses().get(0).setBiasMotivation(0, "88");
			copy19.getOffenses().get(0).setLocationType("20");
			copy19.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy19.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property19 = new PropertySegment();
			property19.setTypeOfPropertyLoss("7");
			property19.setPropertyDescription(0, "04");
			property19.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy22 = new GroupAIncidentReport(incident);			
			copy22.getOffenses().get(0).setUcrOffenseCode("23B");
			copy22.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy22.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy22.getOffenses().get(0).setBiasMotivation(0, "88");
			copy22.getOffenses().get(0).setLocationType("20");
			copy22.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy22.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property22= new PropertySegment();
			property22.setTypeOfPropertyLoss("7");
			property22.setPropertyDescription(0, "04");
			property22.setValueOfProperty(0, 000010000);
			//Buses
			GroupAIncidentReport copy23 = new GroupAIncidentReport(incident);
			copy23.getOffenses().get(0).setUcrOffenseCode("23A");
			copy23.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy23.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy23.getOffenses().get(0).setBiasMotivation(0, "88");
			copy23.getOffenses().get(0).setLocationType("20");
			copy23.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy23.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property23 = new PropertySegment();
			property23.setTypeOfPropertyLoss("7");
			property23.setPropertyDescription(0, "05");
			property23.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy24 = new GroupAIncidentReport(incident);			
			copy24.getOffenses().get(0).setUcrOffenseCode("23B");
			copy24.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy24.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy24.getOffenses().get(0).setBiasMotivation(0, "88");
			copy24.getOffenses().get(0).setLocationType("20");
			copy24.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy24.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property24= new PropertySegment();
			property24.setTypeOfPropertyLoss("7");
			property24.setPropertyDescription(0, "05");
			property24.setValueOfProperty(0, 000010000);
			//Farm Equipment
			GroupAIncidentReport copy25 = new GroupAIncidentReport(incident);
			copy25.getOffenses().get(0).setUcrOffenseCode("23A");
			copy25.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy25.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy25.getOffenses().get(0).setBiasMotivation(0, "88");
			copy25.getOffenses().get(0).setLocationType("20");
			copy25.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy25.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property25 = new PropertySegment();
			property25.setTypeOfPropertyLoss("7");
			property25.setPropertyDescription(0, "12");
			property25.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy26 = new GroupAIncidentReport(incident);			
			copy26.getOffenses().get(0).setUcrOffenseCode("23B");
			copy26.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy26.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy26.getOffenses().get(0).setBiasMotivation(0, "88");
			copy26.getOffenses().get(0).setLocationType("20");
			copy26.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy26.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property26= new PropertySegment();
			property26.setTypeOfPropertyLoss("7");
			property26.setPropertyDescription(0, "12");
			property26.setValueOfProperty(0, 000010000);
			//Heavy Construction/Industrial Equipment
			GroupAIncidentReport copy27 = new GroupAIncidentReport(incident);
			copy27.getOffenses().get(0).setUcrOffenseCode("23A");
			copy27.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy27.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy27.getOffenses().get(0).setBiasMotivation(0, "88");
			copy27.getOffenses().get(0).setLocationType("20");
			copy27.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy27.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property27 = new PropertySegment();
			property27.setTypeOfPropertyLoss("7");
			property27.setPropertyDescription(0, "15");
			property27.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy28 = new GroupAIncidentReport(incident);			
			copy28.getOffenses().get(0).setUcrOffenseCode("23B");
			copy28.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy28.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy28.getOffenses().get(0).setBiasMotivation(0, "88");
			copy28.getOffenses().get(0).setLocationType("20");
			copy28.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy28.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property28= new PropertySegment();
			property28.setTypeOfPropertyLoss("7");
			property28.setPropertyDescription(0, "15");
			property28.setValueOfProperty(0, 000010000);
			//Livestock
			GroupAIncidentReport copy29 = new GroupAIncidentReport(incident);
			copy29.getOffenses().get(0).setUcrOffenseCode("23A");
			copy29.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy29.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy29.getOffenses().get(0).setBiasMotivation(0, "88");
			copy29.getOffenses().get(0).setLocationType("20");
			copy29.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy29.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property29 = new PropertySegment();
			property29.setTypeOfPropertyLoss("7");
			property29.setPropertyDescription(0, "18");
			property29.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy30 = new GroupAIncidentReport(incident);			
			copy30.getOffenses().get(0).setUcrOffenseCode("23B");
			copy30.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy30.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy30.getOffenses().get(0).setBiasMotivation(0, "88");
			copy30.getOffenses().get(0).setLocationType("20");
			copy30.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy30.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property30= new PropertySegment();
			property30.setTypeOfPropertyLoss("7");
			property30.setPropertyDescription(0, "18");
			property30.setValueOfProperty(0, 000010000);
			//Other Motor Vehicles
			GroupAIncidentReport copy31 = new GroupAIncidentReport(incident);
			copy31.getOffenses().get(0).setUcrOffenseCode("23A");
			copy31.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy31.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy31.getOffenses().get(0).setBiasMotivation(0, "88");
			copy31.getOffenses().get(0).setLocationType("20");
			copy31.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy31.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property31 = new PropertySegment();
			property31.setTypeOfPropertyLoss("7");
			property31.setPropertyDescription(0, "24");
			property31.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy32 = new GroupAIncidentReport(incident);			
			copy32.getOffenses().get(0).setUcrOffenseCode("23B");
			copy32.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy32.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy32.getOffenses().get(0).setBiasMotivation(0, "88");
			copy32.getOffenses().get(0).setLocationType("20");
			copy32.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy32.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property32= new PropertySegment();
			property32.setTypeOfPropertyLoss("7");
			property32.setPropertyDescription(0, "24");
			property32.setValueOfProperty(0, 000010000);
			//Recreational Vehicles
			GroupAIncidentReport copy33 = new GroupAIncidentReport(incident);
			copy33.getOffenses().get(0).setUcrOffenseCode("23A");
			copy33.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy33.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy33.getOffenses().get(0).setBiasMotivation(0, "88");
			copy33.getOffenses().get(0).setLocationType("20");
			copy33.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy33.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property33 = new PropertySegment();
			property33.setTypeOfPropertyLoss("7");
			property33.setPropertyDescription(0, "28");
			property33.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy34 = new GroupAIncidentReport(incident);			
			copy34.getOffenses().get(0).setUcrOffenseCode("23B");
			copy34.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy34.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy34.getOffenses().get(0).setBiasMotivation(0, "88");
			copy34.getOffenses().get(0).setLocationType("20");
			copy34.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy34.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property34= new PropertySegment();
			property34.setTypeOfPropertyLoss("7");
			property34.setPropertyDescription(0, "28");
			property34.setValueOfProperty(0, 000010000);
			//Structures - Single Occupancy Dwelling
			GroupAIncidentReport copy35 = new GroupAIncidentReport(incident);
			copy35.getOffenses().get(0).setUcrOffenseCode("23A");
			copy35.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy35.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy35.getOffenses().get(0).setBiasMotivation(0, "88");
			copy35.getOffenses().get(0).setLocationType("20");
			copy35.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy35.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property35 = new PropertySegment();
			property35.setTypeOfPropertyLoss("7");
			property35.setPropertyDescription(0, "29");
			property35.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy36 = new GroupAIncidentReport(incident);			
			copy36.getOffenses().get(0).setUcrOffenseCode("23B");
			copy36.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy36.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy36.getOffenses().get(0).setBiasMotivation(0, "88");
			copy36.getOffenses().get(0).setLocationType("20");
			copy36.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy36.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property36= new PropertySegment();
			property36.setTypeOfPropertyLoss("7");
			property36.setPropertyDescription(0, "29");
			property36.setValueOfProperty(0, 000010000);
			//Structures - Other Dwellings
			GroupAIncidentReport copy37 = new GroupAIncidentReport(incident);
			copy37.getOffenses().get(0).setUcrOffenseCode("23A");
			copy37.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy37.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy37.getOffenses().get(0).setBiasMotivation(0, "88");
			copy37.getOffenses().get(0).setLocationType("20");
			copy37.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy37.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property37 = new PropertySegment();
			property37.setTypeOfPropertyLoss("7");
			property37.setPropertyDescription(0, "30");
			property37.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy38 = new GroupAIncidentReport(incident);			
			copy38.getOffenses().get(0).setUcrOffenseCode("23B");
			copy38.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy38.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy38.getOffenses().get(0).setBiasMotivation(0, "88");
			copy38.getOffenses().get(0).setLocationType("20");
			copy38.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy38.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property38= new PropertySegment();
			property38.setTypeOfPropertyLoss("7");
			property38.setPropertyDescription(0, "30");
			property38.setValueOfProperty(0, 000010000);
			//Structures - Commercial Business
			GroupAIncidentReport copy39 = new GroupAIncidentReport(incident);
			copy39.getOffenses().get(0).setUcrOffenseCode("23A");
			copy39.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy39.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy39.getOffenses().get(0).setBiasMotivation(0, "88");
			copy39.getOffenses().get(0).setLocationType("20");
			copy39.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy39.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property39 = new PropertySegment();
			property39.setTypeOfPropertyLoss("7");
			property39.setPropertyDescription(0, "31");
			property39.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy40 = new GroupAIncidentReport(incident);			
			copy40.getOffenses().get(0).setUcrOffenseCode("23B");
			copy40.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy40.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy40.getOffenses().get(0).setBiasMotivation(0, "88");
			copy40.getOffenses().get(0).setLocationType("20");
			copy40.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy40.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property40= new PropertySegment();
			property40.setTypeOfPropertyLoss("7");
			property40.setPropertyDescription(0, "31");
			property40.setValueOfProperty(0, 000010000);
			//Structures - Industrial/Manufacturing
			GroupAIncidentReport copy41 = new GroupAIncidentReport(incident);
			copy41.getOffenses().get(0).setUcrOffenseCode("23A");
			copy41.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy41.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy41.getOffenses().get(0).setBiasMotivation(0, "88");
			copy41.getOffenses().get(0).setLocationType("20");
			copy41.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy41.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property41 = new PropertySegment();
			property41.setTypeOfPropertyLoss("7");
			property41.setPropertyDescription(0, "32");
			property41.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy42 = new GroupAIncidentReport(incident);			
			copy42.getOffenses().get(0).setUcrOffenseCode("23B");
			copy42.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy42.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy42.getOffenses().get(0).setBiasMotivation(0, "88");
			copy42.getOffenses().get(0).setLocationType("20");
			copy42.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy42.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property42= new PropertySegment();
			property42.setTypeOfPropertyLoss("7");
			property42.setPropertyDescription(0, "32");
			property42.setValueOfProperty(0, 000010000);
			//Structures - Public/Community
			GroupAIncidentReport copy43 = new GroupAIncidentReport(incident);
			copy43.getOffenses().get(0).setUcrOffenseCode("23A");
			copy43.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy43.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy43.getOffenses().get(0).setBiasMotivation(0, "88");
			copy43.getOffenses().get(0).setLocationType("20");
			copy43.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy43.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property43 = new PropertySegment();
			property43.setTypeOfPropertyLoss("7");
			property43.setPropertyDescription(0, "33");
			property43.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy44 = new GroupAIncidentReport(incident);			
			copy44.getOffenses().get(0).setUcrOffenseCode("23B");
			copy44.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy44.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy44.getOffenses().get(0).setBiasMotivation(0, "88");
			copy44.getOffenses().get(0).setLocationType("20");
			copy44.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy44.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property44= new PropertySegment();
			property44.setTypeOfPropertyLoss("7");
			property44.setPropertyDescription(0, "33");
			property44.setValueOfProperty(0, 000010000);
			//Structures - Storage
			GroupAIncidentReport copy45 = new GroupAIncidentReport(incident);
			copy45.getOffenses().get(0).setUcrOffenseCode("23A");
			copy45.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy45.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy45.getOffenses().get(0).setBiasMotivation(0, "88");
			copy45.getOffenses().get(0).setLocationType("20");
			copy45.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy45.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property45 = new PropertySegment();
			property45.setTypeOfPropertyLoss("7");
			property45.setPropertyDescription(0, "34");
			property45.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy46 = new GroupAIncidentReport(incident);			
			copy46.getOffenses().get(0).setUcrOffenseCode("23B");
			copy46.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy46.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy46.getOffenses().get(0).setBiasMotivation(0, "88");
			copy46.getOffenses().get(0).setLocationType("20");
			copy46.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy46.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property46= new PropertySegment();
			property46.setTypeOfPropertyLoss("7");
			property46.setPropertyDescription(0, "34");
			property46.setValueOfProperty(0, 000010000);
			//Structures - Other
			GroupAIncidentReport copy47 = new GroupAIncidentReport(incident);
			copy47.getOffenses().get(0).setUcrOffenseCode("23A");
			copy47.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy47.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy47.getOffenses().get(0).setBiasMotivation(0, "88");
			copy47.getOffenses().get(0).setLocationType("20");
			copy47.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy47.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property47 = new PropertySegment();
			property47.setTypeOfPropertyLoss("7");
			property47.setPropertyDescription(0, "35");
			property47.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy48 = new GroupAIncidentReport(incident);			
			copy48.getOffenses().get(0).setUcrOffenseCode("23B");
			copy48.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy48.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy48.getOffenses().get(0).setBiasMotivation(0, "88");
			copy48.getOffenses().get(0).setLocationType("20");
			copy48.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy48.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property48= new PropertySegment();
			property48.setTypeOfPropertyLoss("7");
			property48.setPropertyDescription(0, "35");
			property48.setValueOfProperty(0, 000010000);
			//Trucks
			GroupAIncidentReport copy49 = new GroupAIncidentReport(incident);
			copy49.getOffenses().get(0).setUcrOffenseCode("23A");
			copy49.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy49.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy49.getOffenses().get(0).setBiasMotivation(0, "88");
			copy49.getOffenses().get(0).setLocationType("20");
			copy49.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy49.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property49 = new PropertySegment();
			property49.setTypeOfPropertyLoss("7");
			property49.setPropertyDescription(0, "37");
			property49.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy50 = new GroupAIncidentReport(incident);			
			copy50.getOffenses().get(0).setUcrOffenseCode("23B");
			copy50.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy50.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy50.getOffenses().get(0).setBiasMotivation(0, "88");
			copy50.getOffenses().get(0).setLocationType("20");
			copy50.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy50.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property50= new PropertySegment();
			property50.setTypeOfPropertyLoss("7");
			property50.setPropertyDescription(0, "37");
			property50.setValueOfProperty(0, 000010000);
			//Watercraft
			GroupAIncidentReport copy51 = new GroupAIncidentReport(incident);
			copy51.getOffenses().get(0).setUcrOffenseCode("23A");
			copy51.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy51.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy51.getOffenses().get(0).setBiasMotivation(0, "88");
			copy51.getOffenses().get(0).setLocationType("20");
			copy51.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy51.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property51 = new PropertySegment();
			property51.setTypeOfPropertyLoss("7");
			property51.setPropertyDescription(0, "39");
			property51.setValueOfProperty(0, 000010000);
			GroupAIncidentReport copy52 = new GroupAIncidentReport(incident);			
			copy52.getOffenses().get(0).setUcrOffenseCode("23B");
			copy52.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy52.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy52.getOffenses().get(0).setBiasMotivation(0, "88");
			copy52.getOffenses().get(0).setLocationType("20");
			copy52.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy52.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property52= new PropertySegment();
			property52.setTypeOfPropertyLoss("7");
			property52.setPropertyDescription(0, "39");
			property52.setValueOfProperty(0, 000010000);
			//PropertySegment descriptions that cannot be shoplifted due to other UCR definitions 
			//(aircraft, vehicles, structures, a persons identity, watercraft, etc.) are 
			//illogical with 23C=Shoplifting
			//
			//Aircraft
			GroupAIncidentReport copy53 = new GroupAIncidentReport(incident);
			copy53.getOffenses().get(0).setUcrOffenseCode("23C");
			copy53.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy53.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy53.getOffenses().get(0).setBiasMotivation(0, "88");
			copy53.getOffenses().get(0).setLocationType("20");
			copy53.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy53.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property53 = new PropertySegment();
			property53.setTypeOfPropertyLoss("7");
			property53.setPropertyDescription(0, "01");
			property53.setValueOfProperty(0, 000010000);
			//Automobiles
			GroupAIncidentReport copy54 = new GroupAIncidentReport(incident);
			copy54.getOffenses().get(0).setUcrOffenseCode("23C");
			copy54.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy54.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy54.getOffenses().get(0).setBiasMotivation(0, "88");
			copy54.getOffenses().get(0).setLocationType("20");
			copy54.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy54.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property54 = new PropertySegment();
			property54.setTypeOfPropertyLoss("7");
			property54.setPropertyDescription(0, "03");
			property54.setValueOfProperty(0, 000010000);
			//Buses
			GroupAIncidentReport copy55 = new GroupAIncidentReport(incident);
			copy55.getOffenses().get(0).setUcrOffenseCode("23C");
			copy55.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy55.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy55.getOffenses().get(0).setBiasMotivation(0, "88");
			copy55.getOffenses().get(0).setLocationType("20");
			copy55.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy55.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property55 = new PropertySegment();
			property55.setTypeOfPropertyLoss("7");
			property55.setPropertyDescription(0, "05");
			property55.setValueOfProperty(0, 000010000);
			//Farm Equipment
			GroupAIncidentReport copy56 = new GroupAIncidentReport(incident);
			copy56.getOffenses().get(0).setUcrOffenseCode("23C");
			copy56.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy56.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy56.getOffenses().get(0).setBiasMotivation(0, "88");
			copy56.getOffenses().get(0).setLocationType("20");
			copy56.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy56.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property56 = new PropertySegment();
			property56.setTypeOfPropertyLoss("7");
			property56.setPropertyDescription(0, "12");
			property56.setValueOfProperty(0, 000010000);
			//Heavy Contruction/Industrial Equipment
			GroupAIncidentReport copy57 = new GroupAIncidentReport(incident);
			copy57.getOffenses().get(0).setUcrOffenseCode("23C");
			copy57.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy57.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy57.getOffenses().get(0).setBiasMotivation(0, "88");
			copy57.getOffenses().get(0).setLocationType("20");
			copy57.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy57.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property57 = new PropertySegment();
			property57.setTypeOfPropertyLoss("7");
			property57.setPropertyDescription(0, "15");
			property57.setValueOfProperty(0, 000010000);
			//Livestock
			GroupAIncidentReport copy58 = new GroupAIncidentReport(incident);
			copy58.getOffenses().get(0).setUcrOffenseCode("23C");
			copy58.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy58.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy58.getOffenses().get(0).setBiasMotivation(0, "88");
			copy58.getOffenses().get(0).setLocationType("20");
			copy58.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy58.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property58 = new PropertySegment();
			property58.setTypeOfPropertyLoss("7");
			property58.setPropertyDescription(0, "18");
			property58.setValueOfProperty(0, 000010000);
			//Other Motor Vehicles
			GroupAIncidentReport copy59 = new GroupAIncidentReport(incident);
			copy59.getOffenses().get(0).setUcrOffenseCode("23C");
			copy59.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy59.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy59.getOffenses().get(0).setBiasMotivation(0, "88");
			copy59.getOffenses().get(0).setLocationType("20");
			copy59.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy59.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property59 = new PropertySegment();
			property59.setTypeOfPropertyLoss("7");
			property59.setPropertyDescription(0, "24");
			property59.setValueOfProperty(0, 000010000);
			//Recreational Vehicles
			GroupAIncidentReport copy60 = new GroupAIncidentReport(incident);
			copy60.getOffenses().get(0).setUcrOffenseCode("23C");
			copy60.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy60.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy60.getOffenses().get(0).setBiasMotivation(0, "88");
			copy60.getOffenses().get(0).setLocationType("20");
			copy60.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy60.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property60 = new PropertySegment();
			property60.setTypeOfPropertyLoss("7");
			property60.setPropertyDescription(0, "28");
			property60.setValueOfProperty(0, 000010000);
			//Structures - Single Occupancy Dwellings
			GroupAIncidentReport copy61 = new GroupAIncidentReport(incident);
			copy61.getOffenses().get(0).setUcrOffenseCode("23C");
			copy61.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy61.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy61.getOffenses().get(0).setBiasMotivation(0, "88");
			copy61.getOffenses().get(0).setLocationType("20");
			copy61.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy61.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property61 = new PropertySegment();
			property61.setTypeOfPropertyLoss("7");
			property61.setPropertyDescription(0, "29");
			property61.setValueOfProperty(0, 000010000);
			//Structures - Other Dwellings
			GroupAIncidentReport copy62 = new GroupAIncidentReport(incident);
			copy62.getOffenses().get(0).setUcrOffenseCode("23C");
			copy62.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy62.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy62.getOffenses().get(0).setBiasMotivation(0, "88");
			copy62.getOffenses().get(0).setLocationType("20");
			copy62.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy62.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property62 = new PropertySegment();
			property62.setTypeOfPropertyLoss("7");
			property62.setPropertyDescription(0, "30");
			property62.setValueOfProperty(0, 000010000);
			//Structures - Commercial/Business
			GroupAIncidentReport copy63 = new GroupAIncidentReport(incident);
			copy63.getOffenses().get(0).setUcrOffenseCode("23C");
			copy63.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy63.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy63.getOffenses().get(0).setBiasMotivation(0, "88");
			copy63.getOffenses().get(0).setLocationType("20");
			copy63.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy63.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property63 = new PropertySegment();
			property63.setTypeOfPropertyLoss("7");
			property63.setPropertyDescription(0, "31");
			property63.setValueOfProperty(0, 000010000);
			//Structures - Industrial/Manufacturing
			GroupAIncidentReport copy64 = new GroupAIncidentReport(incident);
			copy64.getOffenses().get(0).setUcrOffenseCode("23C");
			copy64.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy64.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy64.getOffenses().get(0).setBiasMotivation(0, "88");
			copy64.getOffenses().get(0).setLocationType("20");
			copy64.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy64.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property64 = new PropertySegment();
			property64.setTypeOfPropertyLoss("7");
			property64.setPropertyDescription(0, "32");
			property64.setValueOfProperty(0, 000010000);
			//Structures - Public/Community
			GroupAIncidentReport copy65 = new GroupAIncidentReport(incident);
			copy65.getOffenses().get(0).setUcrOffenseCode("23C");
			copy65.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy65.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy65.getOffenses().get(0).setBiasMotivation(0, "88");
			copy65.getOffenses().get(0).setLocationType("20");
			copy65.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy65.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property65 = new PropertySegment();
			property65.setTypeOfPropertyLoss("7");
			property65.setPropertyDescription(0, "33");
			property65.setValueOfProperty(0, 000010000);
			//Structures - Storage
			GroupAIncidentReport copy66 = new GroupAIncidentReport(incident);
			copy66.getOffenses().get(0).setUcrOffenseCode("23C");
			copy66.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy66.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy66.getOffenses().get(0).setBiasMotivation(0, "88");
			copy66.getOffenses().get(0).setLocationType("20");
			copy66.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy66.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property66 = new PropertySegment();
			property66.setTypeOfPropertyLoss("7");
			property66.setPropertyDescription(0, "34");
			property66.setValueOfProperty(0, 000010000);
			//Structures - Other
			GroupAIncidentReport copy67 = new GroupAIncidentReport(incident);
			copy67.getOffenses().get(0).setUcrOffenseCode("23C");
			copy67.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy67.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy67.getOffenses().get(0).setBiasMotivation(0, "88");
			copy67.getOffenses().get(0).setLocationType("20");
			copy67.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy67.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property67 = new PropertySegment();
			property67.setTypeOfPropertyLoss("7");
			property67.setPropertyDescription(0, "35");
			property67.setValueOfProperty(0, 000010000);
			//Trucks
			GroupAIncidentReport copy68 = new GroupAIncidentReport(incident);
			copy68.getOffenses().get(0).setUcrOffenseCode("23C");
			copy68.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy68.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy68.getOffenses().get(0).setBiasMotivation(0, "88");
			copy68.getOffenses().get(0).setLocationType("20");
			copy68.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy68.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property68 = new PropertySegment();
			property68.setTypeOfPropertyLoss("7");
			property68.setPropertyDescription(0, "37");
			property68.setValueOfProperty(0, 000010000);
			//Watercraft
			GroupAIncidentReport copy69 = new GroupAIncidentReport(incident);
			copy69.getOffenses().get(0).setUcrOffenseCode("23C");
			copy69.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy69.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy69.getOffenses().get(0).setBiasMotivation(0, "88");
			copy69.getOffenses().get(0).setLocationType("20");
			copy69.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy69.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property69 = new PropertySegment();
			property69.setTypeOfPropertyLoss("7");
			property69.setPropertyDescription(0, "39");
			property69.setValueOfProperty(0, 000010000);
			//PropertySegment descriptions for vehicles and structures are illogical with 23D=Theft from Building, 
			//23E=Theft from Coin-Operated Machine or Device, 23F=Theft from Motor Vehicle, and 
			//23G=Theft of Motor Vehicle Parts or Accessories
			//
			//Aircraft
			GroupAIncidentReport copy70 = new GroupAIncidentReport(incident);
			copy70.getOffenses().get(0).setUcrOffenseCode("23D");
			copy70.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy70.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy70.getOffenses().get(0).setBiasMotivation(0, "88");
			copy70.getOffenses().get(0).setLocationType("20");
			copy70.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy70.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property70 = new PropertySegment();
			property70.setTypeOfPropertyLoss("7");
			property70.setPropertyDescription(0, "01");
			property70.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy71 = new GroupAIncidentReport(incident);
			copy71.getOffenses().get(0).setUcrOffenseCode("23E");
			copy71.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy71.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy71.getOffenses().get(0).setBiasMotivation(0, "88");
			copy71.getOffenses().get(0).setLocationType("20");
			copy71.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy71.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property71 = new PropertySegment();
			property71.setTypeOfPropertyLoss("7");
			property71.setPropertyDescription(0, "01");
			property71.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy72 = new GroupAIncidentReport(incident);
			copy72.getOffenses().get(0).setUcrOffenseCode("23F");
			copy72.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy72.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy72.getOffenses().get(0).setBiasMotivation(0, "88");
			copy72.getOffenses().get(0).setLocationType("20");
			copy72.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy72.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property72 = new PropertySegment();
			property72.setTypeOfPropertyLoss("7");
			property72.setPropertyDescription(0, "01");
			property72.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy73 = new GroupAIncidentReport(incident);
			copy73.getOffenses().get(0).setUcrOffenseCode("23G");
			copy73.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy73.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy73.getOffenses().get(0).setBiasMotivation(0, "88");
			copy73.getOffenses().get(0).setLocationType("20");
			copy73.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy73.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property73 = new PropertySegment();
			property73.setTypeOfPropertyLoss("7");
			property73.setPropertyDescription(0, "01");
			property73.setValueOfProperty(0, 000010000);
			//Automobiles
			GroupAIncidentReport copy74 = new GroupAIncidentReport(incident);
			copy74.getOffenses().get(0).setUcrOffenseCode("23D");
			copy74.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy74.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy74.getOffenses().get(0).setBiasMotivation(0, "88");
			copy74.getOffenses().get(0).setLocationType("20");
			copy74.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy74.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property74 = new PropertySegment();
			property74.setTypeOfPropertyLoss("7");
			property74.setPropertyDescription(0, "03");
			property74.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy75 = new GroupAIncidentReport(incident);
			copy75.getOffenses().get(0).setUcrOffenseCode("23E");
			copy75.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy75.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy75.getOffenses().get(0).setBiasMotivation(0, "88");
			copy75.getOffenses().get(0).setLocationType("20");
			copy75.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy75.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property75 = new PropertySegment();
			property75.setTypeOfPropertyLoss("7");
			property75.setPropertyDescription(0, "03");
			property75.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy76 = new GroupAIncidentReport(incident);
			copy76.getOffenses().get(0).setUcrOffenseCode("23F");
			copy76.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy76.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy76.getOffenses().get(0).setBiasMotivation(0, "88");
			copy76.getOffenses().get(0).setLocationType("20");
			copy76.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy76.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property76 = new PropertySegment();
			property76.setTypeOfPropertyLoss("7");
			property76.setPropertyDescription(0, "03");
			property76.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy77 = new GroupAIncidentReport(incident);
			copy77.getOffenses().get(0).setUcrOffenseCode("23G");
			copy77.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy77.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy77.getOffenses().get(0).setBiasMotivation(0, "88");
			copy77.getOffenses().get(0).setLocationType("20");
			copy77.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy77.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property77 = new PropertySegment();
			property77.setTypeOfPropertyLoss("7");
			property77.setPropertyDescription(0, "03");
			property77.setValueOfProperty(0, 000010000);
			//Buses
			GroupAIncidentReport copy78 = new GroupAIncidentReport(incident);
			copy78.getOffenses().get(0).setUcrOffenseCode("23D");
			copy78.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy78.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy78.getOffenses().get(0).setBiasMotivation(0, "88");
			copy78.getOffenses().get(0).setLocationType("20");
			copy78.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy78.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property78 = new PropertySegment();
			property78.setTypeOfPropertyLoss("7");
			property78.setPropertyDescription(0, "05");
			property78.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy79 = new GroupAIncidentReport(incident);
			copy79.getOffenses().get(0).setUcrOffenseCode("23E");
			copy79.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy79.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy79.getOffenses().get(0).setBiasMotivation(0, "88");
			copy79.getOffenses().get(0).setLocationType("20");
			copy79.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy79.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property79 = new PropertySegment();
			property79.setTypeOfPropertyLoss("7");
			property79.setPropertyDescription(0, "05");
			property79.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy80 = new GroupAIncidentReport(incident);
			copy80.getOffenses().get(0).setUcrOffenseCode("23F");
			copy80.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy80.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy80.getOffenses().get(0).setBiasMotivation(0, "88");
			copy80.getOffenses().get(0).setLocationType("20");
			copy80.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy80.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property80 = new PropertySegment();
			property80.setTypeOfPropertyLoss("7");
			property80.setPropertyDescription(0, "05");
			property80.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy81 = new GroupAIncidentReport(incident);
			copy81.getOffenses().get(0).setUcrOffenseCode("23G");
			copy81.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy81.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy81.getOffenses().get(0).setBiasMotivation(0, "88");
			copy81.getOffenses().get(0).setLocationType("20");
			copy81.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy81.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property81 = new PropertySegment();
			property81.setTypeOfPropertyLoss("7");
			property81.setPropertyDescription(0, "05");
			property81.setValueOfProperty(0, 000010000);
			//Other Motor Vehicles
			GroupAIncidentReport copy82 = new GroupAIncidentReport(incident);
			copy82.getOffenses().get(0).setUcrOffenseCode("23D");
			copy82.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy82.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy82.getOffenses().get(0).setBiasMotivation(0, "88");
			copy82.getOffenses().get(0).setLocationType("20");
			copy82.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy82.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property82 = new PropertySegment();
			property82.setTypeOfPropertyLoss("7");
			property82.setPropertyDescription(0, "24");
			property82.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy83 = new GroupAIncidentReport(incident);
			copy83.getOffenses().get(0).setUcrOffenseCode("23E");
			copy83.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy83.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy83.getOffenses().get(0).setBiasMotivation(0, "88");
			copy83.getOffenses().get(0).setLocationType("20");
			copy83.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy83.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property83 = new PropertySegment();
			property83.setTypeOfPropertyLoss("7");
			property83.setPropertyDescription(0, "24");
			property83.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy84 = new GroupAIncidentReport(incident);
			copy84.getOffenses().get(0).setUcrOffenseCode("23F");
			copy84.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy84.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy84.getOffenses().get(0).setBiasMotivation(0, "88");
			copy84.getOffenses().get(0).setLocationType("20");
			copy84.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy84.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property84 = new PropertySegment();
			property84.setTypeOfPropertyLoss("7");
			property84.setPropertyDescription(0, "24");
			property84.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy85 = new GroupAIncidentReport(incident);
			copy85.getOffenses().get(0).setUcrOffenseCode("23G");
			copy85.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy85.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy85.getOffenses().get(0).setBiasMotivation(0, "88");
			copy85.getOffenses().get(0).setLocationType("20");
			copy85.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy85.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property85 = new PropertySegment();
			property85.setTypeOfPropertyLoss("7");
			property85.setPropertyDescription(0, "24");
			property85.setValueOfProperty(0, 000010000);
			//Recreational Vehicles
			GroupAIncidentReport copy86 = new GroupAIncidentReport(incident);
			copy86.getOffenses().get(0).setUcrOffenseCode("23D");
			copy86.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy86.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy86.getOffenses().get(0).setBiasMotivation(0, "88");
			copy86.getOffenses().get(0).setLocationType("20");
			copy86.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy86.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property86 = new PropertySegment();
			property86.setTypeOfPropertyLoss("7");
			property86.setPropertyDescription(0, "28");
			property86.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy87 = new GroupAIncidentReport(incident);
			copy87.getOffenses().get(0).setUcrOffenseCode("23E");
			copy87.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy87.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy87.getOffenses().get(0).setBiasMotivation(0, "88");
			copy87.getOffenses().get(0).setLocationType("20");
			copy87.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy87.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property87 = new PropertySegment();
			property87.setTypeOfPropertyLoss("7");
			property87.setPropertyDescription(0, "28");
			property87.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy88 = new GroupAIncidentReport(incident);
			copy88.getOffenses().get(0).setUcrOffenseCode("23F");
			copy88.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy88.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy88.getOffenses().get(0).setBiasMotivation(0, "88");
			copy88.getOffenses().get(0).setLocationType("20");
			copy88.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy88.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property88 = new PropertySegment();
			property88.setTypeOfPropertyLoss("7");
			property88.setPropertyDescription(0, "28");
			property88.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy89 = new GroupAIncidentReport(incident);
			copy89.getOffenses().get(0).setUcrOffenseCode("23G");
			copy89.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy89.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy89.getOffenses().get(0).setBiasMotivation(0, "88");
			copy89.getOffenses().get(0).setLocationType("20");
			copy89.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy89.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property89 = new PropertySegment();
			property89.setTypeOfPropertyLoss("7");
			property89.setPropertyDescription(0, "28");
			property89.setValueOfProperty(0, 000010000);
			//Structures - Single Occupancy Dwellings
			GroupAIncidentReport copy90 = new GroupAIncidentReport(incident);
			copy90.getOffenses().get(0).setUcrOffenseCode("23D");
			copy90.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy90.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy90.getOffenses().get(0).setBiasMotivation(0, "88");
			copy90.getOffenses().get(0).setLocationType("20");
			copy90.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy90.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property90 = new PropertySegment();
			property90.setTypeOfPropertyLoss("7");
			property90.setPropertyDescription(0, "29");
			property90.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy91 = new GroupAIncidentReport(incident);
			copy91.getOffenses().get(0).setUcrOffenseCode("23E");
			copy91.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy91.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy91.getOffenses().get(0).setBiasMotivation(0, "88");
			copy91.getOffenses().get(0).setLocationType("20");
			copy91.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy91.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property91 = new PropertySegment();
			property91.setTypeOfPropertyLoss("7");
			property91.setPropertyDescription(0, "29");
			property91.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy92 = new GroupAIncidentReport(incident);
			copy92.getOffenses().get(0).setUcrOffenseCode("23F");
			copy92.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy92.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy92.getOffenses().get(0).setBiasMotivation(0, "88");
			copy92.getOffenses().get(0).setLocationType("20");
			copy92.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy92.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property92 = new PropertySegment();
			property92.setTypeOfPropertyLoss("7");
			property92.setPropertyDescription(0, "29");
			property92.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy93 = new GroupAIncidentReport(incident);
			copy93.getOffenses().get(0).setUcrOffenseCode("23G");
			copy93.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy93.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy93.getOffenses().get(0).setBiasMotivation(0, "88");
			copy93.getOffenses().get(0).setLocationType("20");
			copy93.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy93.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property93 = new PropertySegment();
			property93.setTypeOfPropertyLoss("7");
			property93.setPropertyDescription(0, "29");
			property93.setValueOfProperty(0, 000010000);
			//Structures - Other Dwellings
			GroupAIncidentReport copy94 = new GroupAIncidentReport(incident);
			copy94.getOffenses().get(0).setUcrOffenseCode("23D");
			copy94.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy94.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy94.getOffenses().get(0).setBiasMotivation(0, "88");
			copy94.getOffenses().get(0).setLocationType("20");
			copy94.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy94.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property94 = new PropertySegment();
			property94.setTypeOfPropertyLoss("7");
			property94.setPropertyDescription(0, "30");
			property94.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy95 = new GroupAIncidentReport(incident);
			copy95.getOffenses().get(0).setUcrOffenseCode("23E");
			copy95.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy95.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy95.getOffenses().get(0).setBiasMotivation(0, "88");
			copy95.getOffenses().get(0).setLocationType("20");
			copy95.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy95.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property95 = new PropertySegment();
			property95.setTypeOfPropertyLoss("7");
			property95.setPropertyDescription(0, "30");
			property95.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy96 = new GroupAIncidentReport(incident);
			copy96.getOffenses().get(0).setUcrOffenseCode("23F");
			copy96.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy96.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy96.getOffenses().get(0).setBiasMotivation(0, "88");
			copy96.getOffenses().get(0).setLocationType("20");
			copy96.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy96.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property96 = new PropertySegment();
			property96.setTypeOfPropertyLoss("7");
			property96.setPropertyDescription(0, "30");
			property96.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy97 = new GroupAIncidentReport(incident);
			copy97.getOffenses().get(0).setUcrOffenseCode("23G");
			copy97.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy97.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy97.getOffenses().get(0).setBiasMotivation(0, "88");
			copy97.getOffenses().get(0).setLocationType("20");
			copy97.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy97.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property97 = new PropertySegment();
			property97.setTypeOfPropertyLoss("7");
			property97.setPropertyDescription(0, "30");
			property97.setValueOfProperty(0, 000010000);
			//Structures - Commercial/Business
			GroupAIncidentReport copy98 = new GroupAIncidentReport(incident);
			copy98.getOffenses().get(0).setUcrOffenseCode("23D");
			copy98.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy98.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy98.getOffenses().get(0).setBiasMotivation(0, "88");
			copy98.getOffenses().get(0).setLocationType("20");
			copy98.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy98.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property98 = new PropertySegment();
			property98.setTypeOfPropertyLoss("7");
			property98.setPropertyDescription(0, "31");
			property98.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy99 = new GroupAIncidentReport(incident);
			copy99.getOffenses().get(0).setUcrOffenseCode("23E");
			copy99.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy99.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy99.getOffenses().get(0).setBiasMotivation(0, "88");
			copy99.getOffenses().get(0).setLocationType("20");
			copy99.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy99.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property99 = new PropertySegment();
			property99.setTypeOfPropertyLoss("7");
			property99.setPropertyDescription(0, "31");
			property99.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy100 = new GroupAIncidentReport(incident);
			copy100.getOffenses().get(0).setUcrOffenseCode("23F");
			copy100.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy100.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy100.getOffenses().get(0).setBiasMotivation(0, "88");
			copy100.getOffenses().get(0).setLocationType("20");
			copy100.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy100.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property100 = new PropertySegment();
			property100.setTypeOfPropertyLoss("7");
			property100.setPropertyDescription(0, "31");
			property100.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy101 = new GroupAIncidentReport(incident);
			copy101.getOffenses().get(0).setUcrOffenseCode("23G");
			copy101.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy101.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy101.getOffenses().get(0).setBiasMotivation(0, "88");
			copy101.getOffenses().get(0).setLocationType("20");
			copy101.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy101.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property101 = new PropertySegment();
			property101.setTypeOfPropertyLoss("7");
			property101.setPropertyDescription(0, "31");
			property101.setValueOfProperty(0, 000010000);
			//Structures - Industrial/Manufacturing
			GroupAIncidentReport copy102 = new GroupAIncidentReport(incident);
			copy102.getOffenses().get(0).setUcrOffenseCode("23D");
			copy102.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy102.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy102.getOffenses().get(0).setBiasMotivation(0, "88");
			copy102.getOffenses().get(0).setLocationType("20");
			copy102.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy102.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property102 = new PropertySegment();
			property102.setTypeOfPropertyLoss("7");
			property102.setPropertyDescription(0, "32");
			property102.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy103 = new GroupAIncidentReport(incident);
			copy103.getOffenses().get(0).setUcrOffenseCode("23E");
			copy103.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy103.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy103.getOffenses().get(0).setBiasMotivation(0, "88");
			copy103.getOffenses().get(0).setLocationType("20");
			copy103.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy103.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property103 = new PropertySegment();
			property103.setTypeOfPropertyLoss("7");
			property103.setPropertyDescription(0, "32");
			property103.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy104 = new GroupAIncidentReport(incident);
			copy104.getOffenses().get(0).setUcrOffenseCode("23F");
			copy104.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy104.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy104.getOffenses().get(0).setBiasMotivation(0, "88");
			copy104.getOffenses().get(0).setLocationType("20");
			copy104.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy104.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property104 = new PropertySegment();
			property104.setTypeOfPropertyLoss("7");
			property104.setPropertyDescription(0, "32");
			property104.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy105 = new GroupAIncidentReport(incident);
			copy105.getOffenses().get(0).setUcrOffenseCode("23G");
			copy105.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy105.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy105.getOffenses().get(0).setBiasMotivation(0, "88");
			copy105.getOffenses().get(0).setLocationType("20");
			copy105.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy105.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property105 = new PropertySegment();
			property105.setTypeOfPropertyLoss("7");
			property105.setPropertyDescription(0, "32");
			property105.setValueOfProperty(0, 000010000);
			//Structures - Public/Community
			GroupAIncidentReport copy106 = new GroupAIncidentReport(incident);
			copy106.getOffenses().get(0).setUcrOffenseCode("23D");
			copy106.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy106.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy106.getOffenses().get(0).setBiasMotivation(0, "88");
			copy106.getOffenses().get(0).setLocationType("20");
			copy106.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy106.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property106 = new PropertySegment();
			property106.setTypeOfPropertyLoss("7");
			property106.setPropertyDescription(0, "33");
			property106.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy107 = new GroupAIncidentReport(incident);
			copy107.getOffenses().get(0).setUcrOffenseCode("23E");
			copy107.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy107.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy107.getOffenses().get(0).setBiasMotivation(0, "88");
			copy107.getOffenses().get(0).setLocationType("20");
			copy107.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy107.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property107 = new PropertySegment();
			property107.setTypeOfPropertyLoss("7");
			property107.setPropertyDescription(0, "33");
			property107.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy108 = new GroupAIncidentReport(incident);
			copy108.getOffenses().get(0).setUcrOffenseCode("23F");
			copy108.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy108.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy108.getOffenses().get(0).setBiasMotivation(0, "88");
			copy108.getOffenses().get(0).setLocationType("20");
			copy108.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy108.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property108 = new PropertySegment();
			property108.setTypeOfPropertyLoss("7");
			property108.setPropertyDescription(0, "33");
			property108.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy109 = new GroupAIncidentReport(incident);
			copy109.getOffenses().get(0).setUcrOffenseCode("23G");
			copy109.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy109.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy109.getOffenses().get(0).setBiasMotivation(0, "88");
			copy109.getOffenses().get(0).setLocationType("20");
			copy109.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy109.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property109 = new PropertySegment();
			property109.setTypeOfPropertyLoss("7");
			property109.setPropertyDescription(0, "33");
			property109.setValueOfProperty(0, 000010000);
			//Structures - Storage
			GroupAIncidentReport copy110 = new GroupAIncidentReport(incident);
			copy110.getOffenses().get(0).setUcrOffenseCode("23D");
			copy110.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy110.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy110.getOffenses().get(0).setBiasMotivation(0, "88");
			copy110.getOffenses().get(0).setLocationType("20");
			copy110.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy110.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property110 = new PropertySegment();
			property110.setTypeOfPropertyLoss("7");
			property110.setPropertyDescription(0, "34");
			property110.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy111 = new GroupAIncidentReport(incident);
			copy111.getOffenses().get(0).setUcrOffenseCode("23E");
			copy111.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy111.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy111.getOffenses().get(0).setBiasMotivation(0, "88");
			copy111.getOffenses().get(0).setLocationType("20");
			copy111.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy111.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property111 = new PropertySegment();
			property111.setTypeOfPropertyLoss("7");
			property111.setPropertyDescription(0, "34");
			property111.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy112 = new GroupAIncidentReport(incident);
			copy112.getOffenses().get(0).setUcrOffenseCode("23F");
			copy112.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy112.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy112.getOffenses().get(0).setBiasMotivation(0, "88");
			copy112.getOffenses().get(0).setLocationType("20");
			copy112.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy112.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property112 = new PropertySegment();
			property112.setTypeOfPropertyLoss("7");
			property112.setPropertyDescription(0, "34");
			property112.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy113 = new GroupAIncidentReport(incident);
			copy113.getOffenses().get(0).setUcrOffenseCode("23G");
			copy113.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy113.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy113.getOffenses().get(0).setBiasMotivation(0, "88");
			copy113.getOffenses().get(0).setLocationType("20");
			copy113.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy113.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property113 = new PropertySegment();
			property113.setTypeOfPropertyLoss("7");
			property113.setPropertyDescription(0, "34");
			property113.setValueOfProperty(0, 000010000);
			//Structures - Other
			GroupAIncidentReport copy114 = new GroupAIncidentReport(incident);
			copy114.getOffenses().get(0).setUcrOffenseCode("23D");
			copy114.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy114.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy114.getOffenses().get(0).setBiasMotivation(0, "88");
			copy114.getOffenses().get(0).setLocationType("20");
			copy114.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy114.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property114 = new PropertySegment();
			property114.setTypeOfPropertyLoss("7");
			property114.setPropertyDescription(0, "35");
			property114.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy115 = new GroupAIncidentReport(incident);
			copy115.getOffenses().get(0).setUcrOffenseCode("23E");
			copy115.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy115.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy115.getOffenses().get(0).setBiasMotivation(0, "88");
			copy115.getOffenses().get(0).setLocationType("20");
			copy115.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy115.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property115 = new PropertySegment();
			property115.setTypeOfPropertyLoss("7");
			property115.setPropertyDescription(0, "35");
			property115.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy116 = new GroupAIncidentReport(incident);
			copy116.getOffenses().get(0).setUcrOffenseCode("23F");
			copy116.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy116.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy116.getOffenses().get(0).setBiasMotivation(0, "88");
			copy116.getOffenses().get(0).setLocationType("20");
			copy116.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy116.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property116 = new PropertySegment();
			property116.setTypeOfPropertyLoss("7");
			property116.setPropertyDescription(0, "35");
			property116.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy117 = new GroupAIncidentReport(incident);
			copy117.getOffenses().get(0).setUcrOffenseCode("23G");
			copy117.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy117.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy117.getOffenses().get(0).setBiasMotivation(0, "88");
			copy117.getOffenses().get(0).setLocationType("20");
			copy117.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy117.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property117 = new PropertySegment();
			property117.setTypeOfPropertyLoss("7");
			property117.setPropertyDescription(0, "35");
			property117.setValueOfProperty(0, 000010000);
			//Trucks
			GroupAIncidentReport copy118 = new GroupAIncidentReport(incident);
			copy118.getOffenses().get(0).setUcrOffenseCode("23D");
			copy118.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy118.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy118.getOffenses().get(0).setBiasMotivation(0, "88");
			copy118.getOffenses().get(0).setLocationType("20");
			copy118.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy118.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property118 = new PropertySegment();
			property118.setTypeOfPropertyLoss("7");
			property118.setPropertyDescription(0, "37");
			property118.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy119 = new GroupAIncidentReport(incident);
			copy119.getOffenses().get(0).setUcrOffenseCode("23E");
			copy119.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy119.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy119.getOffenses().get(0).setBiasMotivation(0, "88");
			copy119.getOffenses().get(0).setLocationType("20");
			copy119.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy119.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property119 = new PropertySegment();
			property119.setTypeOfPropertyLoss("7");
			property119.setPropertyDescription(0, "37");
			property119.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy120 = new GroupAIncidentReport(incident);
			copy120.getOffenses().get(0).setUcrOffenseCode("23F");
			copy120.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy120.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy120.getOffenses().get(0).setBiasMotivation(0, "88");
			copy120.getOffenses().get(0).setLocationType("20");
			copy120.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy120.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property120 = new PropertySegment();
			property120.setTypeOfPropertyLoss("7");
			property120.setPropertyDescription(0, "37");
			property120.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy121 = new GroupAIncidentReport(incident);
			copy121.getOffenses().get(0).setUcrOffenseCode("23G");
			copy121.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy121.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy121.getOffenses().get(0).setBiasMotivation(0, "88");
			copy121.getOffenses().get(0).setLocationType("20");
			copy121.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy121.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property121 = new PropertySegment();
			property121.setTypeOfPropertyLoss("7");
			property121.setPropertyDescription(0, "37");
			property121.setValueOfProperty(0, 000010000);
			//PropertySegment descriptions for vehicles are illogical with 23H=All Other Larceny
			//
			//Aircraft
			GroupAIncidentReport copy20 = new GroupAIncidentReport(incident);
			copy20.getOffenses().get(0).setUcrOffenseCode("23H");
			copy20.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy20.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy20.getOffenses().get(0).setBiasMotivation(0, "88");
			copy20.getOffenses().get(0).setLocationType("20");
			copy20.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy20.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property20 = new PropertySegment();
			property20.setTypeOfPropertyLoss("7");
			property20.setPropertyDescription(0, "01");
			property20.setValueOfProperty(0, 000010000);
			//Automobiles
			GroupAIncidentReport copy21 = new GroupAIncidentReport(incident);
			copy21.getOffenses().get(0).setUcrOffenseCode("23H");
			copy21.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy21.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy21.getOffenses().get(0).setBiasMotivation(0, "88");
			copy21.getOffenses().get(0).setLocationType("20");
			copy21.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy21.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property21 = new PropertySegment();
			property21.setTypeOfPropertyLoss("7");
			property21.setPropertyDescription(0, "03");
			property21.setValueOfProperty(0, 000010000);
			//Buses
			GroupAIncidentReport copy122 = new GroupAIncidentReport(incident);
			copy122.getOffenses().get(0).setUcrOffenseCode("23H");
			copy122.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy122.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy122.getOffenses().get(0).setBiasMotivation(0, "88");
			copy122.getOffenses().get(0).setLocationType("20");
			copy122.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy122.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property122 = new PropertySegment();
			property122.setTypeOfPropertyLoss("7");
			property122.setPropertyDescription(0, "05");
			property122.setValueOfProperty(0, 000010000);
			//Other Motor Vehicles
			GroupAIncidentReport copy123 = new GroupAIncidentReport(incident);
			copy123.getOffenses().get(0).setUcrOffenseCode("23H");
			copy123.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy123.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy123.getOffenses().get(0).setBiasMotivation(0, "88");
			copy123.getOffenses().get(0).setLocationType("20");
			copy123.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy123.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property123 = new PropertySegment();
			property123.setTypeOfPropertyLoss("7");
			property123.setPropertyDescription(0, "24");
			property123.setValueOfProperty(0, 000010000);
			//Recreational Vehicles
			GroupAIncidentReport copy124 = new GroupAIncidentReport(incident);
			copy124.getOffenses().get(0).setUcrOffenseCode("23H");
			copy124.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy124.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy124.getOffenses().get(0).setBiasMotivation(0, "88");
			copy124.getOffenses().get(0).setLocationType("20");
			copy124.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy124.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property124 = new PropertySegment();
			property124.setTypeOfPropertyLoss("7");
			property124.setPropertyDescription(0, "28");
			property124.setValueOfProperty(0, 000010000);
			//Trucks
			GroupAIncidentReport copy125 = new GroupAIncidentReport(incident);
			copy125.getOffenses().get(0).setUcrOffenseCode("23H");
			copy125.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy125.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy125.getOffenses().get(0).setBiasMotivation(0, "88");
			copy125.getOffenses().get(0).setLocationType("20");
			copy125.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy125.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property125 = new PropertySegment();
			property125.setTypeOfPropertyLoss("7");
			property125.setPropertyDescription(0, "37");
			property125.setValueOfProperty(0, 000010000);
			
			
			
			incidents.add(copy);
			incidents.add(copy2);
			incidents.add(copy3);
			incidents.add(copy4);
			incidents.add(copy5);
			incidents.add(copy6);
			incidents.add(copy7);
			incidents.add(copy8);
			incidents.add(copy9);
			incidents.add(copy10);
			incidents.add(copy11);
			incidents.add(copy12);
			incidents.add(copy13);
			incidents.add(copy14);
			incidents.add(copy15);
			incidents.add(copy16);
			incidents.add(copy17);
			incidents.add(copy18);
			incidents.add(copy19);
			incidents.add(copy20);
			incidents.add(copy21);
			incidents.add(copy22);
			incidents.add(copy23);
			incidents.add(copy24);
			incidents.add(copy25);
			incidents.add(copy26);
			incidents.add(copy27);
			incidents.add(copy28);
			incidents.add(copy29);
			incidents.add(copy30);
			incidents.add(copy31);
			incidents.add(copy32);
			incidents.add(copy33);
			incidents.add(copy34);
			incidents.add(copy35);
			incidents.add(copy36);
			incidents.add(copy37);
			incidents.add(copy38);
			incidents.add(copy39);
			incidents.add(copy40);
			incidents.add(copy41);
			incidents.add(copy42);
			incidents.add(copy43);
			incidents.add(copy44);
			incidents.add(copy45);
			incidents.add(copy46);
			incidents.add(copy47);
			incidents.add(copy48);
			incidents.add(copy49);
			incidents.add(copy50);
			incidents.add(copy51);
			incidents.add(copy52);
			incidents.add(copy53);
			incidents.add(copy54);
			incidents.add(copy55);
			incidents.add(copy56);
			incidents.add(copy57);
			incidents.add(copy58);
			incidents.add(copy59);
			incidents.add(copy60);
			incidents.add(copy61);
			incidents.add(copy62);
			incidents.add(copy63);
			incidents.add(copy64);
			incidents.add(copy65);
			incidents.add(copy66);
			incidents.add(copy67);
			incidents.add(copy68);
			incidents.add(copy69);
			incidents.add(copy70);
			incidents.add(copy71);
			incidents.add(copy72);
			incidents.add(copy73);
			incidents.add(copy74);
			incidents.add(copy75);
			incidents.add(copy76);
			incidents.add(copy77);
			incidents.add(copy78);
			incidents.add(copy79);
			incidents.add(copy80);
			incidents.add(copy81);
			incidents.add(copy82);
			incidents.add(copy83);
			incidents.add(copy84);
			incidents.add(copy85);
			incidents.add(copy86);
			incidents.add(copy87);
			incidents.add(copy88);
			incidents.add(copy89);
			incidents.add(copy90);
			incidents.add(copy91);
			incidents.add(copy92);
			incidents.add(copy93);
			incidents.add(copy94);
			incidents.add(copy95);
			incidents.add(copy96);
			incidents.add(copy97);
			incidents.add(copy98);
			incidents.add(copy99);
			incidents.add(copy100);
			incidents.add(copy101);
			incidents.add(copy102);
			incidents.add(copy103);
			incidents.add(copy104);
			incidents.add(copy105);
			incidents.add(copy106);
			incidents.add(copy107);
			incidents.add(copy108);
			incidents.add(copy109);
			incidents.add(copy110);
			incidents.add(copy111);
			incidents.add(copy112);
			incidents.add(copy113);
			incidents.add(copy114);
			incidents.add(copy115);
			incidents.add(copy116);
			incidents.add(copy117);
			incidents.add(copy118);
			incidents.add(copy119);
			incidents.add(copy120);
			incidents.add(copy121);
			incidents.add(copy122);
			incidents.add(copy123);
			incidents.add(copy124);
			incidents.add(copy125);
			copy.addProperty(property);
			copy2.addProperty(property2);
			copy3.addProperty(property3);
			copy4.addProperty(property4);
			copy5.addProperty(property5);
			copy6.addProperty(property6);
			copy7.addProperty(property7);
			copy8.addProperty(property8);
			copy9.addProperty(property9);
			copy10.addProperty(property10);
			copy11.addProperty(property11);
			copy12.addProperty(property12);
			copy13.addProperty(property13);
			copy14.addProperty(property14);
			copy15.addProperty(property15);
			copy16.addProperty(property16);
			copy17.addProperty(property17);
			copy18.addProperty(property18);
			copy19.addProperty(property19);
			copy20.addProperty(property20);
			copy21.addProperty(property21);
			copy22.addProperty(property22);
			copy23.addProperty(property23);
			copy24.addProperty(property24);
			copy25.addProperty(property25);
			copy26.addProperty(property26);
			copy27.addProperty(property27);
			copy28.addProperty(property28);
			copy29.addProperty(property29);
			copy30.addProperty(property30);
			copy31.addProperty(property31);
			copy32.addProperty(property32);
			copy33.addProperty(property33);
			copy34.addProperty(property34);
			copy35.addProperty(property35);
			copy36.addProperty(property36);
			copy37.addProperty(property37);
			copy38.addProperty(property38);
			copy39.addProperty(property39);
			copy40.addProperty(property40);
			copy41.addProperty(property41);
			copy42.addProperty(property42);
			copy43.addProperty(property43);
			copy44.addProperty(property44);
			copy45.addProperty(property45);
			copy46.addProperty(property46);
			copy47.addProperty(property47);
			copy48.addProperty(property48);
			copy49.addProperty(property49);
			copy50.addProperty(property50);
			copy51.addProperty(property51);
			copy52.addProperty(property52);
			copy53.addProperty(property53);
			copy54.addProperty(property54);
			copy55.addProperty(property55);
			copy56.addProperty(property56);
			copy57.addProperty(property57);
			copy58.addProperty(property58);
			copy59.addProperty(property59);
			copy60.addProperty(property60);
			copy61.addProperty(property61);
			copy62.addProperty(property62);
			copy63.addProperty(property63);
			copy64.addProperty(property64);
			copy65.addProperty(property65);
			copy66.addProperty(property66);
			copy67.addProperty(property67);
			copy68.addProperty(property68);
			copy69.addProperty(property69);
			copy70.addProperty(property70);
			copy71.addProperty(property71);
			copy72.addProperty(property72);
			copy73.addProperty(property73);
			copy74.addProperty(property74);
			copy75.addProperty(property75);
			copy76.addProperty(property76);
			copy77.addProperty(property77);
			copy78.addProperty(property78);
			copy79.addProperty(property79);
			copy80.addProperty(property80);
			copy81.addProperty(property81);
			copy82.addProperty(property82);
			copy83.addProperty(property83);
			copy84.addProperty(property84);
			copy85.addProperty(property85);
			copy86.addProperty(property86);
			copy87.addProperty(property87);
			copy88.addProperty(property88);
			copy89.addProperty(property89);
			copy90.addProperty(property90);
			copy91.addProperty(property91);
			copy92.addProperty(property92);
			copy93.addProperty(property93);
			copy94.addProperty(property94);
			copy95.addProperty(property95);
			copy96.addProperty(property96);
			copy97.addProperty(property97);
			copy98.addProperty(property98);
			copy99.addProperty(property99);
			copy100.addProperty(property100);
			copy101.addProperty(property101);
			copy102.addProperty(property102);
			copy103.addProperty(property103);
			copy104.addProperty(property104);
			copy105.addProperty(property105);
			copy106.addProperty(property106);
			copy107.addProperty(property107);
			copy108.addProperty(property108);
			copy109.addProperty(property109);
			copy110.addProperty(property110);
			copy111.addProperty(property111);
			copy112.addProperty(property112);
			copy113.addProperty(property113);
			copy114.addProperty(property114);
			copy115.addProperty(property115);
			copy116.addProperty(property116);
			copy117.addProperty(property117);
			copy118.addProperty(property118);
			copy119.addProperty(property119);
			copy120.addProperty(property120);
			copy121.addProperty(property121);
			copy122.addProperty(property122);
			copy123.addProperty(property123);
			copy124.addProperty(property124);
			copy125.addProperty(property125);
			
			
						
			return incidents;
		});
		
		groupATweakerMap.put(391, incident -> {
			//(Value of PropertySegment) has a code that requires a zero value in Data Element 16 (Value of PropertySegment). 
			//Either the wrong property description code was entered or the property value was not entered.
			//(This error was formerly error number 340, a warning message.) Data Element 16 
			//(Value of PropertySegment) must be zero when Data Element 15 (PropertySegment Description) is:
			//09=Credit/Debit Cards
			//22=Non-negotiable Instruments
			//48=Documents�Personal or Business
			//65=Identity Documents
			//66=Identity�Intangible
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("220");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			copy.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("7");
			property.setPropertyDescription(0, "09");
			property.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy2 = new GroupAIncidentReport(incident);
			copy2.getOffenses().get(0).setUcrOffenseCode("220");
			copy2.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy2.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy2.getOffenses().get(0).setBiasMotivation(0, "88");
			copy2.getOffenses().get(0).setLocationType("20");
			copy2.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy2.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property2 = new PropertySegment();
			property2.setTypeOfPropertyLoss("7");
			property2.setPropertyDescription(0, "22");
			property2.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy3 = new GroupAIncidentReport(incident);
			copy3.getOffenses().get(0).setUcrOffenseCode("220");
			copy3.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy3.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy3.getOffenses().get(0).setBiasMotivation(0, "88");
			copy3.getOffenses().get(0).setLocationType("20");
			copy3.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy3.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property3 = new PropertySegment();
			property3.setTypeOfPropertyLoss("7");
			property3.setPropertyDescription(0, "48");
			property3.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy4 = new GroupAIncidentReport(incident);
			copy4.getOffenses().get(0).setUcrOffenseCode("220");
			copy4.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy4.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy4.getOffenses().get(0).setBiasMotivation(0, "88");
			copy4.getOffenses().get(0).setLocationType("20");
			copy4.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy4.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property4 = new PropertySegment();
			property4.setTypeOfPropertyLoss("7");
			property4.setPropertyDescription(0, "65");
			property4.setValueOfProperty(0, 000010000);
			//
			GroupAIncidentReport copy5 = new GroupAIncidentReport(incident);
			copy5.getOffenses().get(0).setUcrOffenseCode("220");
			copy5.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy5.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy5.getOffenses().get(0).setBiasMotivation(0, "88");
			copy5.getOffenses().get(0).setLocationType("20");
			copy5.getOffenses().get(0).setNumberOfPremisesEntered(1);
			copy5.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property5 = new PropertySegment();
			property5.setTypeOfPropertyLoss("7");
			property5.setPropertyDescription(0, "65");
			property5.setValueOfProperty(0, 000010000);
						
			incidents.add(copy);
			incidents.add(copy2);
			incidents.add(copy3);
			incidents.add(copy4);
			incidents.add(copy5);
			copy.addProperty(property);
			copy2.addProperty(property2);
			copy3.addProperty(property3);
			copy4.addProperty(property4);
			copy5.addProperty(property5);
						
			return incidents;
		});
		
		groupATweakerMap.put(392, incident -> {
			//(Suspected Drug Type) An offense of 35A Drug/Narcotic Violations and Data Element 14 
			//(Type Property Loss/Etc.) with 1=None were entered but Data Element 20 
			//(Suspected Drug Type) was not submitted. Since a drug seizure did not occur, 
			//the suspected drug type must also be entered. (This error was formerly error number 341, a warning message.)
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("35A");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			copy.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("1");
			property.setPropertyDescription(0, "10");
			property.setSuspectedDrugType(0, null);
			property.setEstimatedDrugQuantity(0, 1.0);
			property.setTypeDrugMeasurement(0, "OZ");
			property.setValueOfProperty(0, 10000);
			
			incidents.add(copy);
			copy.addProperty(property);
						
			return incidents;
		});	
		
		groupATweakerMap.put(392, incident -> {
			//(Suspected Drug Type) An offense of 35A Drug/Narcotic Violations and Data Element 14 
			//(Type Property Loss/Etc.) with 1=None were entered but Data Element 20 
			//(Suspected Drug Type) was not submitted. Since a drug seizure did not occur, 
			//the suspected drug type must also be entered. (This error was formerly error number 341, a warning message.)
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("35A");
			copy.getOffenses().get(0).setOffenseAttemptedCompleted("C");
			copy.getOffenses().get(0).setOffendersSuspectedOfUsing(0, "N");
			copy.getOffenses().get(0).setBiasMotivation(0, "88");
			copy.getOffenses().get(0).setLocationType("20");
			copy.getOffenses().get(0).setMethodOfEntry("N");
			PropertySegment property = new PropertySegment();
			property.setTypeOfPropertyLoss("1");
			property.setPropertyDescription(0, "10");
			property.setSuspectedDrugType(0, null);
			property.setEstimatedDrugQuantity(0, 1.0);
			property.setTypeDrugMeasurement(0, "OZ");
			property.setValueOfProperty(0, 10000);
			
			
			incidents.add(copy);
			copy.addProperty(property);
						
			return incidents;
		});	
		
		groupATweakerMap.put(401, incident -> {
			// The referenced data element in a Group A Incident AbstractReport
			// Segment 4 is mandatory & must be present.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.setYearOfTape(null);
			GroupAIncidentReport copy2 = new GroupAIncidentReport(copy);
			copy2.setMonthOfTape(null);
			GroupAIncidentReport copy3 = new GroupAIncidentReport(copy);
			copy3.setOri(null);
			GroupAIncidentReport copy4 = new GroupAIncidentReport(copy);
			copy4.setIncidentNumber(null);
			GroupAIncidentReport copy5 = new GroupAIncidentReport(copy);
			copy5.getVictims().get(0).setVictimSequenceNumber(null);
			GroupAIncidentReport copy6 = new GroupAIncidentReport(copy);
			copy6.getVictims().get(0).setUcrOffenseCodeConnection(0, null);
			GroupAIncidentReport copy7 = new GroupAIncidentReport(copy);
			copy7.getVictims().get(0).setTypeOfVictim(null);
			GroupAIncidentReport copy8 = new GroupAIncidentReport(copy);
			copy8.getVictims().get(0).setVictimSequenceNumber(000);
			GroupAIncidentReport copy9 = new GroupAIncidentReport(copy);
			copy9.getVictims().get(0).setUcrOffenseCodeConnection(0, "999");
			GroupAIncidentReport copy10 = new GroupAIncidentReport(copy);
			copy10.getVictims().get(0).setTypeOfVictim("Z");
			
			
			incidents.add(copy);
			incidents.add(copy2);
			incidents.add(copy3);
			incidents.add(copy4);
			incidents.add(copy5);
			incidents.add(copy6);
			incidents.add(copy7);
			incidents.add(copy8);
			incidents.add(copy9);
			incidents.add(copy10);
			
				
			return incidents;
		});

		groupATweakerMap.put(404, incident -> {
			//To-do, waiting on response from Becki
			//The referenced data element in a Group A Incident Report 
			//must be populated with a valid data value and cannot be blank.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getVictims().get(0).setAgeString(null);
			
			incidents.add(copy);
			
			return incidents;
			
		});
		
		groupATweakerMap.put(409, incident -> {
			//(Age of Victim) contains more than two characters indicating a possible 
			//age-range was being attempted. If so, the field must contain numeric entry of four digits.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getVictims().get(0).setAgeString("253");
				
			incidents.add(copy);
				
			return incidents;
				
			});
		
		groupATweakerMap.put(410, incident -> {
			//(Age of Victim) was entered as an age-range. Accordingly, the first age 
			//component must be less than the second age.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getVictims().get(0).setAgeString("3025");
				
			incidents.add(copy);
				
			return incidents;
				
			});
		
		
		
		groupATweakerMap.put(406, incident -> {
			//(Victim Connected to UCR Offense Code) The referenced data element in 
			//error is one that contains multiple data values. When more than one code is 
			//entered, none can be duplicate codes.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			OffenseSegment offense = new OffenseSegment();
			offense.setUcrOffenseCode("120");
			offense.setOffenseAttemptedCompleted("C");
			offense.setBiasMotivation(0,"88");
			offense.setLocationType("20");
			offense.setNumberOfPremisesEntered(null);
			offense.setMethodOfEntry("N");
			offense.setTypeOfWeaponForceInvolved(0, "99");
			offense.setOffendersSuspectedOfUsing(0, "N");
			copy.getVictims().get(0).setUcrOffenseCodeConnection(1, "13A");
						
			incidents.add(copy);
			
			return incidents;
			
			
		});
		
		
		groupATweakerMap.put(454, incident -> {
			//(Type of Officer Activity/Circumstance), Data Element 25B (Officer Assignment Type), 
			//Data Element 26 (Age of Victim), Data Element 27 (Sex of Victim), and 
			//Data Element 28 (Race of Victim) must be entered when 
			//Data Element 25 (Type of Victim) is L=Law Enforcement Officer.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			//Officer Assignment Type is null
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getVictims().get(0).setTypeOfOfficerActivityCircumstance(null);
			copy.getVictims().get(0).setOfficerAssignmentType("K");
			copy.getVictims().get(0).setTypeOfVictim("L");
			//Officer Assignment Type is null
			GroupAIncidentReport copy2 = new GroupAIncidentReport(incident);
			copy2.getVictims().get(0).setTypeOfOfficerActivityCircumstance("01");
			copy2.getVictims().get(0).setOfficerAssignmentType(null);
			copy2.getVictims().get(0).setTypeOfVictim("L");
			//Age is null
			GroupAIncidentReport copy3 = new GroupAIncidentReport(incident);
			copy3.getVictims().get(0).setTypeOfOfficerActivityCircumstance("01");
			copy3.getVictims().get(0).setOfficerAssignmentType("K");
			copy3.getVictims().get(0).setAgeString(null);
			copy3.getVictims().get(0).setTypeOfVictim("L");
			//Sex is null
			GroupAIncidentReport copy4 = new GroupAIncidentReport(incident);
			copy4.getVictims().get(0).setTypeOfOfficerActivityCircumstance("01");
			copy4.getVictims().get(0).setOfficerAssignmentType("K");
			copy4.getVictims().get(0).setSex(null);
			copy4.getVictims().get(0).setTypeOfVictim("L");
			//Race is null
			GroupAIncidentReport copy5 = new GroupAIncidentReport(incident);
			copy5.getVictims().get(0).setTypeOfOfficerActivityCircumstance("01");
			copy5.getVictims().get(0).setOfficerAssignmentType("K");
			copy5.getVictims().get(0).setRace(null);
			copy5.getVictims().get(0).setTypeOfVictim("L");
			
			
					
			incidents.add(copy);
			incidents.add(copy2);
			incidents.add(copy3);
			incidents.add(copy4);
			incidents.add(copy5);
			
			return incidents;
			
			
		});
		
		
		groupATweakerMap.put(461, incident -> {
			//(Type of Victim) cannot have a value of S=Society/Public when the 
			//offense is 220=Burglary/Breaking and Entering.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("220");
			copy.getVictims().get(0).setTypeOfVictim("S");
			
			incidents.add(copy);
			
			return incidents;
					
		});
		
		groupATweakerMap.put(464, incident -> {
			//UCR Code contains a Crime Against Person, but Data Element 25 
			//(Type of Victim) is not I=Individual or L=Law Enforcement Officer when Data Element 24 
			//(Victim Connected to UCR Offense Code) contains a Crime Against Person.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getVictims().get(0).setTypeOfVictim("B");
			
			incidents.add(copy);
			
			return incidents;
					
		});
		
		groupATweakerMap.put(465, incident -> {
			//UCR Code contains a Crime Against Society, but Data Element 25 
			//(Type of Victim) is not S=Society.
			
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("720");
			copy.getVictims().get(0).setUcrOffenseCodeConnection(0, "720");
			copy.getVictims().get(0).setTypeOfVictim("B");
			
			incidents.add(copy);
			
			return incidents;
					
		});
		
		groupATweakerMap.put(467, incident -> {
			//UCR code contains a Crime Against Property, but Data Element 25 
			//(Type of Victim) is S=Society. This is not an allowable code for Crime Against Property offenses.
			
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("200");
			copy.getVictims().get(0).setUcrOffenseCodeConnection(0, "200");
			copy.getVictims().get(0).setTypeOfVictim("S");
			
			incidents.add(copy);
			
			return incidents;
					
		});
		
		groupATweakerMap.put(482, incident -> {
			//(Type of Victim) cannot be L=Law Enforcement Officer unless Data Element 24 
			//(Victim Connected to UCR Offense Code) is one of the following:
			//      09A=Murder & Non-negligent Manslaughter
			//		13A=Aggravated Assault
			//		13B=Simple Assault
			//		13C=Intimidation
			
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.getOffenses().get(0).setUcrOffenseCode("200");
			copy.getVictims().get(0).setUcrOffenseCodeConnection(0, "200");
			copy.getVictims().get(0).setTypeOfVictim("L");
			
			incidents.add(copy);
			
			return incidents;
					
		});
	
		
		
		groupATweakerMap.put(501, incident -> {
			// The referenced data element in a Group A Incident AbstractReport
			// Segment 5 is mandatory & must be present.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.setYearOfTape(null);
			GroupAIncidentReport copy2 = new GroupAIncidentReport(copy);
			copy2.setMonthOfTape(null);
			GroupAIncidentReport copy3 = new GroupAIncidentReport(copy);
			copy3.setOri(null);
			GroupAIncidentReport copy4 = new GroupAIncidentReport(copy);
			copy4.setIncidentNumber(null);
			
			incidents.add(copy);
			incidents.add(copy2);
			incidents.add(copy3);
			incidents.add(copy4);
			
			return incidents;
		});

		groupATweakerMap.put(601, incident -> {
			// The referenced data element in a Group A Incident AbstractReport
			// Segment 6 is mandatory & must be present.
			List<GroupAIncidentReport> incidents = new ArrayList<GroupAIncidentReport>();
			GroupAIncidentReport copy = new GroupAIncidentReport(incident);
			copy.setYearOfTape(null);
			GroupAIncidentReport copy2 = new GroupAIncidentReport(copy);
			copy2.setMonthOfTape(null);
			GroupAIncidentReport copy3 = new GroupAIncidentReport(copy);
			copy3.setOri(null);
			GroupAIncidentReport copy4 = new GroupAIncidentReport(copy);
			copy4.setIncidentNumber(null);
			
			incidents.add(copy);
			incidents.add(copy2);
			incidents.add(copy3);
			incidents.add(copy4);
			
			
			return incidents;
		});
		
	}}
		


	


