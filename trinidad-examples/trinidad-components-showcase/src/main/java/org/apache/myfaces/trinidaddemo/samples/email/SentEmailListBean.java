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

import org.apache.myfaces.trinidaddemo.samples.email.Email;
import org.apache.myfaces.trinidaddemo.samples.email.TableBean;

import java.util.ArrayList;
import java.util.List;
import java.util.Calendar;
import java.util.Locale;
import java.text.SimpleDateFormat;

/**
 *
 */
public class SentEmailListBean {
    private List<SentEmail> list = new ArrayList<SentEmail>();
    private TableBean tableBean = new TableBean();
    private SentEmail sentEmail;


    public SentEmailListBean(List list) {
        this.list = list;
    }

    public SentEmailListBean() {
        list.add(new SentEmail("jack@yahoo.com", "subject1", "2009.09.25 at 04:34:32", "This is the content of the mail (Sent) whitch you selected"));
        list.add(new SentEmail("john@yahoo.com", "subject2", "2009.09.25 at 04:55:32", "content2"));
        list.add(new SentEmail("john@yahoo.com", "subject2", "2009.09.25 at 04:10:32", "content3"));
    }

    public TableBean getTableBean() {
        return tableBean;
    }

    public void setTableBean(TableBean tableBean) {
        this.tableBean = tableBean;
    }

    public SentEmail getSentEmail() {
        return sentEmail;
    }

    public void setSentEmail(SentEmail sentEmail) {
        this.sentEmail = sentEmail;
    }

    public List<SentEmail> getList() {
        return list;
    }

    public void setList(List<SentEmail> list) {
        this.list = list;
    }

    public void openMail() {
        tableBean.performReport();
        List<Object> reportItems = tableBean.getReportItems();
        if (reportItems.size() > 0) {
            sentEmail = (SentEmail) reportItems.get(0);
        } else {
            sentEmail = null;
        }
    }

    public void deleteSentEmail() {
        tableBean.performReport();
        List<Object> reportItems = tableBean.getReportItems();
        for (Object reportItem : reportItems) {
            if (list.contains((SentEmail) reportItem)) {
                list.remove(reportItem);
            }
        }
        tableBean = new TableBean();
    }

    public String newEmail() {
        sentEmail = new SentEmail();
        sentEmail.setFrom("Yourself");
        Calendar now = Calendar.getInstance();
        SimpleDateFormat formatter = new SimpleDateFormat("yyyy.MM.dd 'at' hh:mm:ss", new Locale("de", "CH"));
        sentEmail.setDate(formatter.format(now.getTime()));
        return "goToNewMail";
    }

    public String save() {
        if (!list.contains(sentEmail)) {
            list.add(sentEmail);
        }
        sentEmail = null;
        return "goToSent";
    }

    public String cancel() {
        sentEmail = null;
        return "goToSent";
    }


    public static class SentEmail extends Email {
        public SentEmail(String to, String subject, String date, String content) {
            super("Yourself", to, subject, date, content);
        }

        public SentEmail() {
            super();
        }
    }
}
