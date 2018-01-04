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
package org.search.nibrs.model.reports;

import java.io.Serializable;

public class ReturnAFormRow implements Serializable{
	private static final long serialVersionUID = -8439219265321448834L;
	private int reportedOffenses; 
	private int unfoundedOffenses; 
	private int actualOffenses; 
	private int clearedOffenses;
	private int clearanceInvolvingJuvenile;
	
	public ReturnAFormRow() {
		super();
	}
	public int getReportedOffenses() {
		return reportedOffenses;
	}
	public void setReportedOffenses(int reportedOffenses) {
		this.reportedOffenses = reportedOffenses;
	}
	public void increaseReportedOffenses(int increment){
		reportedOffenses += increment;
	}
	
	public int getUnfoundedOffenses() {
		return unfoundedOffenses;
	}
	public void setUnfoundedOffenses(int unfoundedOffenses) {
		this.unfoundedOffenses = unfoundedOffenses;
	}
	public void increaseUnfoundedOffenses(int increment){
		unfoundedOffenses += increment;
	}
	
	public int getActualOffenses() {
		return reportedOffenses - unfoundedOffenses;
	}
	
	public int getClearedOffenses() {
		return clearedOffenses;
	}
	public void setClearedOffenses(int clearedOffenses) {
		this.clearedOffenses = clearedOffenses;
	}
	public void increaseClearedOffenses(int increment){
		clearedOffenses += increment;
	}
	public int getClearanceInvolvingJuvenile() {
		return clearanceInvolvingJuvenile;
	}
	public void setClearanceInvolvingJuvenile(int clearanceInvolvingJuvenile) {
		this.clearanceInvolvingJuvenile = clearanceInvolvingJuvenile;
	}
	public void increaseClearanceInvolvingJuvenile(int increment){
		clearanceInvolvingJuvenile += increment;
	}
	@Override
	public String toString() {
		return "Summary [reportedOffenses=" + reportedOffenses + ", unfoundedOffenses=" + unfoundedOffenses
				+ ", actualOffenses=" + actualOffenses + ", clearedOffense=" + clearedOffenses
				+ ", clearanceInvolvingJuvenile=" + clearanceInvolvingJuvenile + "]";
	}
}
