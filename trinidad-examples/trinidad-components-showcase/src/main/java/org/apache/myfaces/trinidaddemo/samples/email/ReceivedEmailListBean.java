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

import java.util.List;
import java.util.ArrayList;

/**
 *
 */
public class ReceivedEmailListBean {
    private List<ReceivedEmail> list = new ArrayList<ReceivedEmail>();
    private ReceivedEmail receivedEmail;
    private TableBean tableBean = new TableBean();


    public ReceivedEmailListBean(List<ReceivedEmail> list) {
        this.list = list;
    }

    public ReceivedEmailListBean() {
        list.add(new ReceivedEmail("jack@yahoo.com","subject1","2009.09.25 at 04:34:32","This is the content of the mail (Inbox) whitch you selected"));
        list.add(new ReceivedEmail("john@yahoo.com","subject2","2009.09.25 at 04:34:32","content2"));
        list.add(new ReceivedEmail("john@yahoo.com","subject2","2009.09.25 at 04:34:32","content3"));
    }

    public ReceivedEmail getReceivedEmail() {
        return receivedEmail;
    }

    public void setReceivedEmail(ReceivedEmail receivedEmail) {
        this.receivedEmail = receivedEmail;
    }

    public TableBean getTableBean() {
        return tableBean;
    }

    public void setTableBean(TableBean tableBean) {
        this.tableBean = tableBean;
    }

    public List<ReceivedEmail> getList() {
        return list;
    }

    public void setList(List<ReceivedEmail> list) {
        this.list = list;
    }

    public void openMail() {
        tableBean.performReport();
        List<Object> reportItems = tableBean.getReportItems();
        if (reportItems.size() > 0) {
            receivedEmail = (ReceivedEmail) reportItems.get(0);
        } else {
            receivedEmail = null;
        }
    }

    public void deleteInboxEmail() {
        tableBean.performReport();
        List<Object> reportItems = tableBean.getReportItems();
        for (Object reportItem : reportItems) {
            if (list.contains((ReceivedEmail)reportItem)) {
                list.remove(reportItem);
            }
        }
        tableBean = new TableBean();
    }

    public static class ReceivedEmail extends Email {
        public ReceivedEmail(String from, String subject, String date, String content) {
            super(from, "Yourself", subject, date, content);
        }
    }
}
