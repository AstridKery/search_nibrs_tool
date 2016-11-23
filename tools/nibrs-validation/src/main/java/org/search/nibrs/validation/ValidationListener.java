/*******************************************************************************
 * Copyright 2016 Research Triangle Institute
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *******************************************************************************/
package org.search.nibrs.validation;

import java.util.List;

import org.search.nibrs.common.NIBRSError;

/**
 * Should be implemented by classes interested in handling validation events
 */
public abstract class ValidationListener {
	
	/**
	 * Notifies implementer to the availability of NEW validation error(s) 
	 * 
	 * @param nibrsErrorList
	 * 		Contains NEW validation errors.  Does not contain errors that were 
	 * 		found in previous validationAvailable executions 
	 */
	public abstract void validationAvailable(List<NIBRSError> nibrsErrorList);

}
