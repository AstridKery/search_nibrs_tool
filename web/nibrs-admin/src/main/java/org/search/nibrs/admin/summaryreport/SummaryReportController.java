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
package org.search.nibrs.admin.summaryreport;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Map;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.search.nibrs.admin.services.rest.RestService;
import org.search.nibrs.model.reports.ReturnAForm;
import org.search.nibrs.model.reports.arson.ArsonReport;
import org.search.nibrs.model.reports.asr.AsrReports;
import org.search.nibrs.model.reports.humantrafficking.HumanTraffickingForm;
import org.search.nibrs.model.reports.supplementaryhomicide.SupplementaryHomicideReport;
import org.search.nibrs.report.service.ArsonExcelExporter;
import org.search.nibrs.report.service.AsrExcelExporter;
import org.search.nibrs.report.service.HumanTraffickingExporter;
import org.search.nibrs.report.service.ReturnAFormExporter;
import org.search.nibrs.report.service.StagingDataRestClient;
import org.search.nibrs.report.service.SupplementaryHomicideReportExporter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.SessionAttributes;

@Controller
@SessionAttributes({"summaryReportRequest", "oriMapping"})
public class SummaryReportController {
	private static final Log log = LogFactory.getLog(SummaryReportController.class);

	@Resource
	RestService restService;
	
	@Autowired 
	public StagingDataRestClient restClient; 
	@Autowired 
	public ReturnAFormExporter returnAFormExporter;
	@Autowired 
	public AsrExcelExporter asrExcelExporter;
	@Autowired 
	public ArsonExcelExporter arsonExcelExporter;
	@Autowired 
	public HumanTraffickingExporter humanTraffickingExporter;
	@Autowired 
	public SupplementaryHomicideReportExporter supplementaryHomicideReportExporter;
	
    @ModelAttribute
    public void addModelAttributes(Model model) {
    	
    	log.info("Add ModelAtrributes");
		
		if (!model.containsAttribute("oriMapping")) {
			model.addAttribute("oriMapping", restService.getOris());
		}
    	log.debug("Model: " + model);
    }

	@GetMapping("/summaryReports/searchForm")
	public String getSummaryReportSearchForm(Map<String, Object> model) throws IOException{
		SummaryReportRequest summaryReportRequest = (SummaryReportRequest) model.get("summaryReportRequest");
		
		if (summaryReportRequest == null) {
			summaryReportRequest = new SummaryReportRequest();
		}
		
		model.put("summaryReportRequest", summaryReportRequest);
	    return "/summaryReports/searchForm::summaryReportForm";
	}
	
	@GetMapping("/summaryReports/searchForm/reset")
	public String resetSearchForm(Map<String, Object> model) throws IOException {
		SummaryReportRequest summaryReportRequest = new SummaryReportRequest();;
		model.put("summaryReportRequest", summaryReportRequest);
	    return "/summaryReports/searchForm::summaryReportForm";
	}
	
	@GetMapping("/returnAForm/{ori}/{year}/{month}")
	public void getReturnAForm(@PathVariable String ori, @PathVariable String year, @PathVariable String month, 
			HttpServletResponse response) throws IOException{
		ReturnAForm returnAForm = restClient.getReturnAForm(ori, year, month);
		XSSFWorkbook workbook = returnAFormExporter.createReturnAWorkbook(returnAForm);
		String fileName = "ReturnA-" + returnAForm.getOri() + "-" + returnAForm.getYear() + "-" + StringUtils.leftPad(String.valueOf(returnAForm.getMonth()), 2, '0') + ".xlsx";
		downloadReport(response, workbook, fileName);
	}

	private void downloadReport(HttpServletResponse response, XSSFWorkbook workbook, String fileName) throws IOException {
		String mimeType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";
		// set content attributes for the response
		response.setContentType(mimeType);
		
//		response.setContentLength();
		
		// set headers for the response
		String headerKey = "Content-Disposition";
		String headerValue = String.format("attachment; filename=\"%s\"", fileName);
		response.setHeader(headerKey, headerValue);
		
		// get output stream of the response
		OutputStream outStream = response.getOutputStream();
		workbook.write(outStream);
		outStream.close();
	}
	
	@PostMapping("/summaryReports/returnAForm")
	public void getReturnAFormByRequest(@ModelAttribute SummaryReportRequest summaryReportRequest,
			HttpServletResponse response) throws IOException{
		ReturnAForm returnAForm = restClient.getReturnAForm(
				summaryReportRequest.getOri(), summaryReportRequest.getIncidentYearString(), summaryReportRequest.getIncidentMonthString());
		XSSFWorkbook workbook = returnAFormExporter.createReturnAWorkbook(returnAForm);
		String fileName = "ReturnA-" + returnAForm.getOri() + "-" + returnAForm.getYear() + "-" + StringUtils.leftPad(String.valueOf(returnAForm.getMonth()), 2, '0') + ".xlsx";

		downloadReport(response, workbook, fileName);
	}
	
	@PostMapping("/summaryReports/arsonReport")
	public void getArsonReportByRequest(@ModelAttribute SummaryReportRequest summaryReportRequest,
			HttpServletResponse response) throws IOException{
		log.info("get arson report");
		ArsonReport arsonReport = restClient.getArsonReport(
				summaryReportRequest.getOri(), summaryReportRequest.getIncidentYearString(), summaryReportRequest.getIncidentMonthString());
		XSSFWorkbook workbook = arsonExcelExporter.createWorkBook(arsonReport);
		String fileName = "ARSON-Report-" + arsonReport.getOri() + "-" + arsonReport.getYear() + "-" + StringUtils.leftPad(String.valueOf(arsonReport.getMonth()), 2, '0') + ".xlsx";
		downloadReport(response, workbook, fileName);
	}
	
	@PostMapping("/summaryReports/humanTraffickingReport")
	public void getHumanTraffickingReportByRequest(@ModelAttribute SummaryReportRequest summaryReportRequest,
			HttpServletResponse response) throws IOException{
		log.info("get arson report");
		HumanTraffickingForm humanTraffickingForm = restClient.getHumanTraffickingForm(
				summaryReportRequest.getOri(), summaryReportRequest.getIncidentYearString(), summaryReportRequest.getIncidentMonthString());
		XSSFWorkbook workbook = humanTraffickingExporter.createWorkbook(humanTraffickingForm);
		String fileName = "HumanTrafficking-" + humanTraffickingForm.getOri() + "-" + humanTraffickingForm.getYear() + 
				"-" + StringUtils.leftPad(String.valueOf(humanTraffickingForm.getMonth()), 2, '0') + ".xlsx";
		downloadReport(response, workbook, fileName);
	}
	
	@PostMapping("/summaryReports/asrReports")
	public void getAsrReportsByRequest(@ModelAttribute SummaryReportRequest summaryReportRequest,
			HttpServletResponse response) throws IOException{
		log.info("get arson report");
		AsrReports asrReports = restClient.getAsrReports(
				summaryReportRequest.getOri(), summaryReportRequest.getIncidentYearString(), summaryReportRequest.getIncidentMonthString());
		XSSFWorkbook workbook = asrExcelExporter.createWorkbook(asrReports);
		String fileName = "ASR-REPORTS-" + asrReports.getOri() + "-" + asrReports.getYear() + "-" + StringUtils.leftPad(String.valueOf(asrReports.getMonth()), 2, '0') + ".xlsx";
		downloadReport(response, workbook, fileName);
	}
	
	@PostMapping("/summaryReports/shrReports")
	public void getSupplementaryHomicideReportsByRequest(@ModelAttribute SummaryReportRequest summaryReportRequest,
			HttpServletResponse response) throws IOException{
		log.info("get arson report");
		SupplementaryHomicideReport supplementaryHomicideReport = restClient.getSupplementaryHomicideReport(
				summaryReportRequest.getOri(), summaryReportRequest.getIncidentYearString(), summaryReportRequest.getIncidentMonthString());
		XSSFWorkbook workbook = supplementaryHomicideReportExporter.createWorkbook(supplementaryHomicideReport);
		String fileName = "SupplementaryHomicideReport-" + supplementaryHomicideReport.getOri() + "-" + supplementaryHomicideReport.getYear() + 
				"-" + StringUtils.leftPad(String.valueOf(supplementaryHomicideReport.getMonth()), 2, '0') + ".xlsx";
		downloadReport(response, workbook, fileName);
	}
	
	@RequestMapping("/arsonReport/{ori}/{year}/{month}")
	public void getArsonReport(@PathVariable String ori, @PathVariable Integer year, @PathVariable Integer month){
		throw new NotImplementedException();
	}
	@RequestMapping("/humanTraffickingReport/{ori}/{year}/{month}")
	public void getHumanTraffickingReport(@PathVariable String ori, @PathVariable Integer year, @PathVariable Integer month){
		throw new NotImplementedException();
	}
	
	@RequestMapping("/asrReports/{ori}/{arrestYear}/{arrestMonth}")
	public void getAsrReports(@PathVariable String ori, @PathVariable Integer arrestYear, @PathVariable Integer arrestMonth){
		throw new NotImplementedException();
	}
	
	@RequestMapping("/shrReports/{ori}/{arrestYear}/{arrestMonth}")
	public void getSupplementaryHomicideReports(@PathVariable String ori, @PathVariable Integer arrestYear, @PathVariable Integer arrestMonth){
		throw new NotImplementedException();
	}
	
}