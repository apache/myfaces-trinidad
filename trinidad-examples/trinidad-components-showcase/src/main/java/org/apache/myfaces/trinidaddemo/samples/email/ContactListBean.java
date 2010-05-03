/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.trinidaddemo.samples.email;

import org.apache.myfaces.trinidaddemo.samples.email.TableBean;

import java.util.List;
import java.util.ArrayList;

/**
 *
 */
public class ContactListBean {
    private List<Contact> list = new ArrayList<Contact>();
    private Contact contact;
    private TableBean tableBean = new TableBean();

    public ContactListBean() {
        list.add(new Contact("Tom", "Smith", "tom@yahoo.com", "1977.12.12"));
        list.add(new Contact("Mark", "Smith", "marc@yahoo.com", "1977.11.11"));
        list.add(new Contact("Jack", "Smith", "jack@yahoo.com", "1977.10.10"));
    }

    public ContactListBean(List<Contact> list) {
        this.list = list;
    }

    public Contact getContact() {
        return contact;
    }

    public void setContact(Contact contact) {
        this.contact = contact;
    }

    public TableBean getTableBean() {
        return tableBean;
    }

    public void setTableBean(TableBean tableBean) {
        this.tableBean = tableBean;
    }

    public void newContact() {
        contact = new Contact();
    }

    public void editContact() {
        tableBean.performReport();
        List<Object> reportItems = tableBean.getReportItems();
        if (reportItems.size() > 0) {
            contact = (Contact) reportItems.get(0);
        } else {
            contact = null;
        }
    }

    public void deleteContact() {
        tableBean.performReport();
        List<Object> reportItems = tableBean.getReportItems();
        for (Object reportItem : reportItems) {
            if (list.contains((Contact)reportItem)) {
                list.remove(reportItem);
            }
        }
    }

    public String save() {
        if (!list.contains(contact)) {
            list.add(contact);
        }
        contact = null;
        return null;
    }

    public String cancel() {
        contact = null;
        return null;
    }


    public List<Contact> getList() {
        return list;
    }

    public void setList(List<Contact> list) {
        this.list = list;
    }

    public static class Contact {
        private String firstName;
        private String eMailAddress;
        private String birthDay;
        private String lastName;
        private String city;
        private String zip;
        private String street;
        private String country;

        public Contact() {
        }

        public Contact(String firstName, String lastName, String eMailAddress, String birthDay) {
            this.firstName = firstName;
            this.lastName = lastName;
            this.eMailAddress = eMailAddress;
            this.birthDay = birthDay;
        }

        public Contact(String firstName, String country, String street, String zip,
                       String city, String lastName, String birthDay, String eMailAddress) {
            this.firstName = firstName;
            this.country = country;
            this.street = street;
            this.zip = zip;
            this.city = city;
            this.lastName = lastName;
            this.birthDay = birthDay;
            this.eMailAddress = eMailAddress;
        }

        public String getCity() {
            return city;
        }

        public void setCity(String city) {
            this.city = city;
        }

        public String getZip() {
            return zip;
        }

        public void setZip(String zip) {
            this.zip = zip;
        }

        public String getStreet() {
            return street;
        }

        public void setStreet(String street) {
            this.street = street;
        }

        public String getCountry() {
            return country;
        }

        public void setCountry(String country) {
            this.country = country;
        }

        public String getFirstName() {
            return firstName;
        }

        public void setFirstName(String firstName) {
            this.firstName = firstName;
        }

        public String getLastName() {
            return lastName;
        }

        public void setLastName(String lastName) {
            this.lastName = lastName;
        }

        public String getEMailAddress() {
            return eMailAddress;
        }

        public void setEMailAddress(String eMailAddress) {
            this.eMailAddress = eMailAddress;
        }

        public String getBirthDay() {
            return birthDay;
        }

        public void setBirthDay(String birthDay) {
            this.birthDay = birthDay;
        }
    }
}
