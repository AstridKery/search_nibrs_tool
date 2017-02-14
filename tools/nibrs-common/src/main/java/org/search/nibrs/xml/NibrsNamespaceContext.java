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
package org.search.nibrs.xml;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import javax.xml.namespace.NamespaceContext;

import org.search.nibrs.util.MapNamespaceContext;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class NibrsNamespaceContext implements NamespaceContext {
	
	public static enum Namespace {
		nibrs("nibrs", "http://fbi.gov/cjis/nibrs/4.0"),
		cjis("cjis", "http://fbi.gov/cjis/1.0"),
		cjiscodes("cjiscodes", "http://fbi.gov/cjis/cjis-codes/1.0"),
		i("i", "http://release.niem.gov/niem/appinfo/3.0/"),
		ucr("ucr", "http://release.niem.gov/niem/codes/fbi_ucr/3.0/"),
		j("j", "http://release.niem.gov/niem/domains/jxdm/5.1/"),
		term("term", "http://release.niem.gov/niem/localTerminology/3.0/"),
		nc("nc", "http://release.niem.gov/niem/niem-core/3.0/"),
		niem_xsd("niem-xsd", "http://release.niem.gov/niem/proxy/xsd/3.0/"),
		s("s", "http://release.niem.gov/niem/structures/3.0/"),
		xsi("xsi", "http://www.w3.org/2001/XMLSchema-instance"),
		xsd("xsd", "http://www.w3.org/2001/XMLSchema"),
		nibrscodes("nibrscodes", "http://fbi.gov/cjis/nibrs/nibrs-codes/4.0"),
		lexs31("lexs31", "http://usdoj.gov/leisp/lexs/3.1"),
		lexspd31("lexspd31", "http://usdoj.gov/leisp/lexs/publishdiscover/3.1"),
		lexsdigest31("lexsdigest31", "http://usdoj.gov/leisp/lexs/digest/3.1"),
		structures20("structures20", "http://niem.gov/niem/structures/2.0")
		;
		
		public String prefix;
		public String uri;
		Namespace(String prefix, String uri) {
			this.prefix = prefix;
			this.uri = uri;
		}
	}
	
	private MapNamespaceContext delegate;
	
	public NibrsNamespaceContext() {
		delegate = new MapNamespaceContext();
		for(Namespace ns : Namespace.values()) {
			delegate.add(ns.prefix, ns.uri);
		}
	}

	public String getNamespaceURI(String prefix) {
		return delegate.getNamespaceURI(prefix);
	}

	public String getPrefix(String namespaceURI) {
		return delegate.getPrefix(namespaceURI);
	}

	public Iterator<String> getPrefixes(String namespaceURI) {
		return delegate.getPrefixes(namespaceURI);
	}
	
	/**
	 * This method collects all of the namespaces that are actually used, in this element and all of its descendants, and declares the proper prefixes
	 * on this element.
	 * 
	 * @param e the element on which to declare the namespace prefixes for itself and its descendants
	 */
	public void populateRootNamespaceDeclarations(Element e) {
		Set<String> namespaceURIs = collectNamespaceURIs(e);
		for (String uri : namespaceURIs) {
			String prefix = getPrefix(uri);
			e.setAttributeNS("http://www.w3.org/2000/xmlns/", "xmlns:" + prefix, uri);
		}
	}

	private Set<String> collectNamespaceURIs(Node e) {
		Set<String> ret = new HashSet<String>();
		String uri = e.getNamespaceURI();
		if (uri != null) {
			ret.add(uri);
		}
		NodeList children = e.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if (child instanceof Element) {
				ret.addAll(collectNamespaceURIs(child));
			}
		}
		NamedNodeMap attributeMap = e.getAttributes();
		for (int i = 0; i < attributeMap.getLength(); i++) {
			Node attr = attributeMap.item(i);
			uri = attr.getNamespaceURI();
			if (uri != null) {
				ret.add(uri);
			}
		}
		return ret;
	}

}
