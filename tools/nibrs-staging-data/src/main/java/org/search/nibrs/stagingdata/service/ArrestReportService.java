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

import org.search.nibrs.stagingdata.model.segment.ArrestReportSegment;
import org.search.nibrs.stagingdata.repository.segment.ArrestReportSegmentRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * Service to process Group B Arrest Report.  
 *
 */
@Service
public class ArrestReportService {
	@Autowired
	ArrestReportSegmentRepository arrestReportSegmentRepository;
	
	public ArrestReportSegment saveArrestReportSegment(ArrestReportSegment arrestReportSegment){
		return arrestReportSegmentRepository.save(arrestReportSegment);
	}
	
	public ArrestReportSegment findArrestReportSegment(Integer id){
		return arrestReportSegmentRepository.findOne(id);
	}
}
