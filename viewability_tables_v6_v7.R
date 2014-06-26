rm(list=ls())
require(gplots)
#library(xtable)
#require(RColorBrewer)
library(plyr)
library(ggplot2)
library(scales)
library(bigmemory)
library(randomForest)
library(RODBC)
library(doParallel)
library(itertools)
#gp <- odbcConnect("greenplum")
source(file="~/R/database_connection2.R")
options(scipen = 6, digits = 6)
#options(digits=TRUE)
gc()
setwd("/dotomi/tmp/viewability/")

start_timestamp <- "2014-06-10 19:00:00"
end_timestamp <- "2014-06-23 00:00:00"
start_date <- "20140610" 
end_date <- "20140623"
sqlQuery(gp, "drop table if exists bid_aux")
sqlQuery(gp, paste("
                   create temp table bid_aux with (appendonly=true, compresstype=quicklz) as
                   select company_id, transaction_nbr, rpt_analytics.usp_get_domain_name(content_url) content_url
                   from raw_rtb_bid_log 
                   where bid_date+ bid_time  between '",start_timestamp,"'::timestamp and '",end_timestamp,"'::timestamp and company_id =2495 and operation_id in (3,12,61) distributed by (transaction_nbr)",sep=""))
sqlQuery(gp, "analyze bid_aux")


sqlQuery(gp, "drop table if exists view_notify_transaction_nbr")
sqlQuery(gp, paste("create temp table view_notify_transaction_nbr with (appendonly=TRUE, compresstype=quicklz) as 
                   select transaction_nbr, 1 view_notify, count(*) view_notify_nrows
                   from raw_dmm_view_notif_log where dmm_date + dmm_time  between '",start_timestamp,"'::timestamp and '",end_timestamp,"'::timestamp  and company_id =2495  and media_size_id not in (-10,35,37,38,40, 41, 42, 50, 51, 52, 53, 54 ) group by 1 distributed by (transaction_nbr)"))
sqlQuery(gp, "analyze view_notify_transaction_nbr")


sqlQuery(gp, "drop table if exists dmm_transaction_nbr;")
sqlQuery(gp, paste("create temp table dmm_transaction_nbr with (appendonly=TRUE, compresstype= quicklz) as
                   select transaction_nbr, a.content_url dmm_url, b.content_url bid_url, coalesce(b.content_url, rpt_analytics.usp_get_domain_name(a.content_url)) content_url, a.company_id company_id
                   , min(dmm_date) dmm_date
                   , max(dmm_date) dmm_date_check
                   , min(company_id) company_id_dmm
                   , max(company_id) company_id_check
                   , min(status_id) status_id
                   , max(status_id) status_id_check
                   , min(media_size_id) media_size_id
                   , max(media_size_id) media_size_id_check
                   , min(browser_desc) dmm_browser
                   , max(browser_desc) browser_check
                   , min(browser_major_version_attr) dmm_browser_major_version
                   , min(browser_minor_version_attr) dmm_browser_minor_version
                   , min(operating_system_desc) dmm_os
                   , min(device_desc) dmm_device_desc
                   , min(device_type_attr) dmm_device_type_attr
                   , min(publisher_id) publisher_id
                   , min(message_campaign_id) message_campaign_id
                   , min(text(user_ip)) user_ip
                   , count(*) nrows
                   , max(view_notify) view_notify
                   , max(server_id) server_id
                   from raw_dmm_log a
                   left join bid_aux  b using(transaction_nbr, company_id)
                   left join view_notify_transaction_nbr c using(transaction_nbr)
                   where dmm_date + dmm_time  between '",start_timestamp,"'::timestamp and '",end_timestamp,"'::timestamp  and company_id =2495 and status_id in (3,12,61) and media_size_id not in (-10,35,37,38,40, 41, 42, 50, 51, 52, 53, 54 ) and message_campaign_id not in (43909, 43916, 43923) group by 1,2,3,4,5
                   distributed by (transaction_nbr);", sep=""))
sqlQuery(gp, "analyze dmm_transaction_nbr")

sqlQuery(gp, "select nrows, count(*) trans, count(*)::float/sum(count(*)) over() prop_trans,  1-(count(*)::float/sum(count(*)) over())   from dmm_transaction_nbr group by 1 order by 1")

sqlQuery(gp, "drop table if exists anly_work_tbls.nh_multiple_dmm_rows_per_transaction")
sqlQuery(gp, "create table anly_work_tbls.nh_multiple_dmm_rows_per_transaction with(appendonly=TRUE, compresstype=quicklz) as 
         select * from dmm_transaction_nbr where nrows>1 distributed by (transaction_nbr)")
sqlQuery(gp, "analyze anly_work_tbls.nh_multiple_dmm_rows_per_transaction")

sqlQuery(gp, "select count(*) from anly_work_tbls.nh_multiple_dmm_rows_per_transaction")

sqlQuery(gp, "drop table if exists viewability_parsed_event_detail_aux1")
sqlQuery(gp, paste("create temp table viewability_parsed_event_detail_aux1 with (appendonly=true, compresstype=quicklz) as
                   select * from 
                   (select transaction_nbr, event_detail_desc, browser_desc, browser_major_version_attr, browser_minor_version_attr
                   , operating_system_desc, device_desc, device_type_attr, device_manufacturer_attr, row_number() over(partition by transaction_nbr order by logrec_sk) r_n
                   from raw_dmm_view_event_log where event_date + event_time between '2014-05-19 21:00:00'::timestamp and '2014-05-21 00:00:00'::timestamp and event_id  in (10, 90) and script_version_id in(6,7))a
                   where r_n=1; ", sep=""))
sqlQuery(gp, "analyze viewability_parsed_event_detail_aux1")

sqlQuery(gp, "drop table if exists viewability_parsed_event_detail; ")
sqlQuery(gp, "create temp table viewability_parsed_event_detail with(appendonly=true, compresstype=quicklz) as
         select transaction_nbr
         , substring(event_detail_desc from 1 for 1) version_nbr
         , substring(event_detail_desc from 3 for 1) fontface
         , substring(event_detail_desc from 4 for 1) canvas
         , substring(event_detail_desc from 5 for 1) svg
         , substring(event_detail_desc from 6 for 1) vidh264
         , substring(event_detail_desc from 7 for 1) vidogg
         , substring(event_detail_desc from 8 for 1) vidwebm
         , substring(event_detail_desc from 9 for 1) webgl
         , substring(event_detail_desc from 10 for 1) rtc
         , substring(event_detail_desc from 11 for 1) rtc_data
         , substring(event_detail_desc from 12 for 1) performance
         , substring(event_detail_desc from 13 for 1) raf
         , substring(event_detail_desc from 14 for 1) json
         , substring(event_detail_desc from 15 for 1) haschange
         , substring(event_detail_desc from 16 for 1) postmessage
         , substring(event_detail_desc from 17 for 1) page_vis
         , substring(event_detail_desc from 18 for 1) security_pol
         , substring(event_detail_desc from 19 for 1) cookies
         , substring(event_detail_desc from 20 for 1) cors
         , substring(event_detail_desc from 21 for 1) orientation
         , substring(event_detail_desc from 22 for 1) motion
         , substring(event_detail_desc from 23 for 1) touch
         , substring(event_detail_desc from 24 for 1) vibration
         , substring(event_detail_desc from 25 for 1) fullscreen
         , substring(event_detail_desc from 26 for 1) sandboxing
         , substring(event_detail_desc from 27 for 1) seamless
         , substring(event_detail_desc from 28 for 1) parent_top
         , substring(event_detail_desc from 29 for 1) framed
         , substring(event_detail_desc from 30 for 1) sandboxed
         , substring(event_detail_desc from 31 for 1) moz_viz
         , substring(event_detail_desc from 32 for 1) vml
         , substring(event_detail_desc from 33 for 1) subpixelfont
         , substring(event_detail_desc from 34 for 1) ie8_as_ie7
         , substring(event_detail_desc from 35 for 1) active_x
         , split_part(event_detail_desc, ';', 3) total_iframes
         , split_part(event_detail_desc, ';', 4) iframe_order
         , split_part(event_detail_desc, ';', 5) frame_depth
         , split_part(event_detail_desc, ';', 6) zoom
         , split_part(event_detail_desc, ';', 7) pixel_dens
         , split_part(event_detail_desc, ';', 8) screen_size
         , split_part(event_detail_desc, ';', 9) available_size
         , split_part(event_detail_desc, ';', 10) viewport
         , split_part(event_detail_desc, ';', 11) flash_version
         , split_part(event_detail_desc, ';', 12) silverlight_version
         , split_part(event_detail_desc, ';', 13) unity_version
         , split_part(event_detail_desc, ';', 14) do_not_track
         , split_part(event_detail_desc, ';', 15) ie_doc
         , split_part(event_detail_desc, ';', 16) browser_version
         , split_part(event_detail_desc, ';', 17) os
         , split_part(event_detail_desc, ';', 18) browser_render_engine
         , split_part(event_detail_desc, ';', 19) vendor
         --, browser_desc
         --, browser_major_version_attr
         --, browser_minor_version_attr
         --, operating_system_desc
         --, device_desc
         --, device_type_attr
         --, device_manufacturer_attr
         from viewability_parsed_event_detail_aux1
         distributed by (transaction_nbr);")
sqlQuery(gp, "analyze viewability_parsed_event_detail; ")


sqlQuery(gp, "drop table if exists viewability_summary_tmp")
sqlQuery(gp, paste("create temp table viewability_summary_tmp with(appendonly=TRUE, compresstype=quicklz) as
                   select *, end_events -start_events  events_duration from (
                   select transaction_nbr
                   , min(script_version_id) script_version_id
                   , max(script_version_id) script_version_id_check             
                   , min(dtm_id) dtm_id
                   , max(dtm_id) dtm_id_check
                   , min(cookie_dtm_id) cookie_dtm_id
                   , max(cookie_dtm_id) cookie_dtm_id_check
                   , min(company_id) company_id
                   , min(event_date + event_time) start_events
                   , max(event_date + event_time) end_events
                   , min(server_id) server_id, max(server_id) server_id_check
                   , min(publisher_id) publisher_id, max(publisher_id) publisher_id_check
                   , min(message_campaign_id) message_campaign_id
                   , max(message_campaign_id) message_campaign_id_check
                   , min(template_id) template_id
                   , max(template_id) template_id_check
                   , min(browser_desc) browser_desc
                   , max(browser_desc) browser_desc_check
                   , min(browser_major_version_attr) browser_major_version_attr
                   , max(browser_major_version_attr) browser_major_version_attr_check
                   , min(browser_minor_version_attr) browser_minor_version_attr
                   , max(browser_minor_version_attr) browser_minor_version_attr_check
                   , min(operating_system_desc) operating_system_desc
                   , max(operating_system_desc) operating_system_desc_check
                   , min(device_desc) device_desc
                   , max(device_desc) device_desc_check
                   , min(device_type_attr) device_type_attr
                   , max(device_type_attr) device_type_attr_check
                   , min(device_manufacturer_attr) device_manufacturer_attr
                   , max(device_manufacturer_attr) device_manufacturer_attr_check
                   , sum(case when event_id = 10 then 1 else 0 end) event_10s
                   , max(case when event_id = 10 then elapsed_time_ms_meas else 0 end) event_10_time
                   , sum(case when event_id = 11 then 1 else 0 end) event_11s
                   , max(case when event_id = 11 then elapsed_time_ms_meas else 0 end) event_11_time
                   , sum(case when event_id = 20 then 1 else 0 end) event_20s
                   , max(case when event_id = 20 then elapsed_time_ms_meas else 0 end) event_20_time
                   , sum(case when event_id = 30 then 1 else 0 end) event_30s
                   , max(case when event_id = 30 then elapsed_time_ms_meas else 0 end) event_30_time
                   , sum(case when event_id = 40 then 1 else 0 end) event_40s
                   , max(case when event_id = 40 then elapsed_time_ms_meas else 0 end) event_40_time
                   , sum(case when event_id = 50 then 1 else 0 end) event_50s
                   , max(case when event_id = 50 then elapsed_time_ms_meas else 0 end) event_50_time
                   , sum(case when event_id = 60 then 1 else 0 end) event_60s
                   , max(case when event_id = 60 then elapsed_time_ms_meas else 0 end) event_60_time
                   , sum(case when event_id = 70 then 1 else 0 end) event_70s
                   , max(case when event_id = 70 then elapsed_time_ms_meas else 0 end) event_70_time
                   , sum(case when event_id = 80 then 1 else 0 end) event_80s
                   , max(case when event_id = 80 then elapsed_time_ms_meas else 0 end) event_80_time
                   , sum(case when event_id = 90 then 1 else 0 end) event_90s
                   , max(case when event_id = 90 then elapsed_time_ms_meas else 0 end) event_90_time
                   , min(case when event_id = 90 then event_detail_desc else null end) event_90_detail
                   , max(case when event_id = 90 then event_detail_desc else null end) event_90_detail_check
                   from raw_dmm_view_event_log 
                   where event_date >= '",start_date,"'::date - interval'1 day' 
                   and event_date <= '",end_date,"'::date + interval'1 day'
                   and company_id =2495
                   and script_version_id in (6,7)
                   group by 1) a
                   left join viewability_parsed_event_detail
                   using(transaction_nbr)
                   where date_trunc('day',start_events) >= '",start_date,"'::date - interval'1 day'  and 
                     date_trunc('day',start_events) <= '",end_date,"'::date + interval'1 day'
                     and script_version_id in (6,7)
                   distributed by (transaction_nbr)", sep=""))
sqlQuery(gp, "analyze viewability_summary_tmp")


sqlQuery(gp, "drop table if exists viewability_summary_2")
sqlQuery(gp, paste("create temp table viewability_summary_2 with(appendonly=TRUE, compresstype=quicklz) as
                   select *, end_events -start_events  events_duration from 
                   (select transaction_nbr, content_url, company_id, dmm_date, status_id, media_size_id, dmm_browser, dmm_browser_major_version, dmm_os, dmm_device_desc, dmm_device_type_attr, publisher_id dmm_publisher_id, message_campaign_id dmm_message_campaign_id, view_notify, user_ip, server_id dmm_server_id from dmm_transaction_nbr) b
                   left join (
                   select transaction_nbr
                   , min(script_version_id) script_version_id
                   , max(script_version_id) script_version_id_check
                   , min(dtm_id) dtm_id
                   , max(dtm_id) dtm_id_check
                   , min(cookie_dtm_id) cookie_dtm_id
                   , min(event_date + event_time) start_events
                   , max(event_date + event_time) end_events
                   , min(server_id) server_id
                   , min(publisher_id) publisher_id
                   , min(message_campaign_id) message_campaign_id
                   , max(message_campaign_id) message_campaign_id_check
                   , min(template_id) template_id
                   , min(browser_desc) browser_desc
                   , max(browser_desc) browser_desc_check
                   , min(browser_major_version_attr) browser_major_version_attr
                   , max(browser_major_version_attr) browser_major_version_attr_check
                   , min(browser_minor_version_attr) browser_minor_version_attr
                   , min(operating_system_desc) operating_system_desc
                   , max(operating_system_desc) operating_system_desc_check
                   , min(device_desc) device_desc
                   , max(device_desc) device_desc_check
                   , min(device_type_attr) device_type_attr
                   , min(device_manufacturer_attr) device_manufacturer_attr
                   , sum(case when event_id = 10 then 1 else 0 end) event_10s
                   , max(case when event_id = 10 then elapsed_time_ms_meas else 0 end) event_10_time
                   , sum(case when event_id = 11 then 1 else 0 end) event_11s
                   , max(case when event_id = 11 then elapsed_time_ms_meas else 0 end) event_11_time
                   , sum(case when event_id = 20 then 1 else 0 end) event_20s
                   , max(case when event_id = 20 then elapsed_time_ms_meas else 0 end) event_20_time
                   , sum(case when event_id = 30 then 1 else 0 end) event_30s
                   , max(case when event_id = 30 then elapsed_time_ms_meas else 0 end) event_30_time
                   , sum(case when event_id = 40 then 1 else 0 end) event_40s
                   , max(case when event_id = 40 then elapsed_time_ms_meas else 0 end) event_40_time
                   , sum(case when event_id = 50 then 1 else 0 end) event_50s
                   , max(case when event_id = 50 then elapsed_time_ms_meas else 0 end) event_50_time
                   , sum(case when event_id = 60 then 1 else 0 end) event_60s
                   , max(case when event_id = 60 then elapsed_time_ms_meas else 0 end) event_60_time
                   , sum(case when event_id = 70 then 1 else 0 end) event_70s
                   , max(case when event_id = 70 then elapsed_time_ms_meas else 0 end) event_70_time
                   , sum(case when event_id = 80 then 1 else 0 end) event_80s
                   , max(case when event_id = 80 then elapsed_time_ms_meas else 0 end) event_80_time
                   , sum(case when event_id = 90 then 1 else 0 end) event_90s
                   , max(case when event_id = 90 then elapsed_time_ms_meas else 0 end) event_90_time
                   , min(case when event_id = 90 then event_detail_desc else null end) event_90_detail
                   , max(case when event_id = 90 then event_detail_desc else null end) event_90_detail_check
                   from raw_dmm_view_event_log 
                   where event_date >= '",start_date,"'::date - interval'1 day'
                   and event_date <= '",end_date,"'::date + interval'1 day'
                   and company_id =2495
                   and script_version_id in (6,7)
                   group by 1) a
                   using(transaction_nbr)
                   left join viewability_parsed_event_detail c
                   using(transaction_nbr)
                   distributed by (transaction_nbr)", sep=""))
sqlQuery(gp, "analyze viewability_summary_2")


transactions_with_issues <- sqlQuery(gp, "select 
         case when event_10s >1 then 1 else 0 end multiple_10s,
         case when event_90s >1 then 1 else 0 end mulitple_90s,
         case when event_10s >0 and event_90s>0 then 1 else 0 end both_10_and_90,
         case when script_version_id <> script_version_id_check then 1 else 0 end multiple_versions,
         case when dtm_id <> dtm_id_check then 1 else 0 end multiple_cookies,
         case when browser_desc <> browser_desc_check then 1 else 0 end multiple_browser,
         case when browser_major_version_attr <>
                   browser_major_version_attr_check then 1 else 0 end multiple_browser_versions,
         case when operating_system_desc <> operating_system_desc_check then 1 else 0 end multiple_os,
         count(*), count(*)::float/sum(count(*)) over()*100
         from viewability_summary_2 group by 1,2,3,4,5,6,7,8 order by 10")


sqlQuery(gp, "drop table if exists anly_work_tbls.nh_view_transactions_w_issues")
sqlQuery(gp, "create table anly_work_tbls.nh_view_transactions_w_issues with(appendonly=true, compresstype=quicklz) as
         select * from viewability_summary_2 
          where event_10s>1 
             or event_90s>1 
             or (event_10s > 0 and event_90s> 0) 
             or script_version_id <> script_version_id_check
             or dtm_id <> dtm_id_check
             or browser_desc <> browser_desc_check
             or browser_major_version_attr <> browser_major_version_attr_check
             or operating_system_desc <> operating_system_desc_check
             or dmm_browser <> browser_desc
             or dmm_browser <> browser_desc_check
             or dmm_browser_major_version <> browser_major_version_attr
             or dmm_browser_major_version <> browser_major_version_attr_check
             or dmm_os <> operating_system_desc
             or dmm_os <> operating_system_desc_check
             or dmm_device_desc <> device_desc
         distributed by (transaction_nbr)")
sqlQuery(gp, "analyze anly_work_tbls.nh_view_transactions_w_issues")

sqlQuery(gp, "drop table if exists anly_work_tbls.nh_viewability")
sqlQuery(gp, "create table anly_work_tbls.nh_viewability with(appendonly=true, compresstype=quicklz) as
    select transaction_nbr, content_url, company_id, dmm_date, status_id, media_size_id, dmm_browser, dmm_browser_major_version,
           browser_minor_version_attr, dmm_os,
           dmm_device_desc, dmm_device_type_attr, dmm_publisher_id, dmm_message_campaign_id, view_notify, script_version_id, dtm_id, 
           start_events, end_events, template_id, event_10s, event_10_time, event_11s, event_11_time, event_20s, event_20_time, 
           event_30s, event_30_time, event_40s, event_40_time, event_50s, event_50_time, event_60s, event_60_time,
           event_70s, event_70_time, event_80s, event_80_time, event_90s, event_90_time, version_nbr, fontface, canvas, svg, vidh264, vidogg, vidwebm 
, webgl, rtc, rtc_data
, performance, raf, json
         , haschange, postmessage, page_vis
         , security_pol, cookies, cors
         , orientation, motion, touch 
         , vibration, fullscreen, sandboxing
         , seamless, parent_top, framed
         , sandboxed, moz_viz, vml 
         , subpixelfont, ie8_as_ie7, active_x
         , total_iframes, iframe_order, frame_depth 
         , zoom, pixel_dens, screen_size 
         , available_size, viewport, flash_version 
         , silverlight_version, unity_version, do_not_track
         , ie_doc, browser_version, os
         , browser_render_engine, vendor, events_duration 
        from (
         select * from viewability_summary_2
           left join (select transaction_nbr, 1 to_blacklist from anly_work_tbls.nh_view_transactions_w_issues)a
           using(transaction_nbr)) b
         where to_blacklist is null
         distributed by (transaction_nbr)")
sqlQuery(gp, "analyze anly_work_tbls.nh_viewability")


sqlQuery(gp, "drop table if exists anly_work_tbls.nh_viewability_summary")
sqlQuery(gp, "create table anly_work_tbls.nh_viewability_summary with(appendonly=true, compresstype=quicklz) as
select company_id, dmm_browser, dmm_browser_major_version, dmm_os, count(*) impressions
     , sum(case when script_version_id = 6 then 1 else 0 end) v6
         , sum(case when script_version_id = 7 then 1 else 0 end) v7
         , sum(case when script_version_id is null then 1 else 0 end) v_null
         , sum(case when script_version_id =6 and event_10s>0 then 1 else 0 end) v6_event10
         , sum(case when script_version_id =7 and event_10s>0 then 1 else 0 end) v7_event10
         , sum(case when script_version_id =6 and event_90s>0 then 1 else 0 end) v6_event90
         , sum(case when script_version_id =7 and event_90s>0 then 1 else 0 end) v7_event90
         , sum(case when script_version_id =6 and event_30s>0 then 1 else 0 end) v6_event30
         , sum(case when script_version_id =7 and event_30s>0 then 1 else 0 end) v7_event30
         , sum(case when script_version_id =6 and event_80s>0 then 1 else 0 end) v6_event80
         , sum(case when script_version_id =7 and event_80s>0 then 1 else 0 end) v7_event80
         , sum(case when event_10s >0 then 1 else 0 end) total10
         , sum(case when event_90s >0 then 1 else 0 end) total90
         , sum(case when event_30s >0 then 1 else 0 end) total30
         , sum(case when event_80s >0 then 1 else 0 end) total80
         , sum(case when script_version_id =6 and view_notify >0 then 1 else 0 end) v6_view_notify
         , sum(case when script_version_id =7 and view_notify >0 then 1 else 0 end) v7_view_notify
         , sum(case when script_version_id is null and view_notify >0 then 1 else 0 end) v_null_view_notify
         from anly_work_tbls.nh_viewability
         group by 1,2,3,4 distributed randomly ")
sqlQuery(gp, "analyze anly_work_tbls.nh_viewability_summary")


sqlQuery(gp, "drop table if exists anly_work_tbls.nh_viewability_detail_table")
sqlQuery(gp, "create table anly_work_tbls.nh_viewability_detail_table 
          with (appendonly=true, compresstype=quicklz) as
         select * from viewability_summary_2 distributed by (transaction_nbr)")
sqlQuery(gp, "analyze anly_work_tbls.nh_viewability_detail_table")


sqlQuery(gp, "grant select on anly_work_tbls.nh_multiple_dmm_rows_per_transaction to jgilbert")
sqlQuery(gp, "grant select on anly_work_tbls.nh_view_transactions_w_issues to jgilbert")
sqlQuery(gp, "grant select on anly_work_tbls.nh_viewability_summary to jgilbert")
sqlQuery(gp, "grant select on anly_work_tbls.nh_viewability_detail_table  to jgilbert")


# sqlQuery(gp, "drop view if exists anly_work_tbls.nh_viewability_summary_view_2")
# sqlQuery(gp, "drop table if exists anly_work_tbls.nh_viewability_summary_2")
# sqlQuery(gp, "create table anly_work_tbls.nh_viewability_summary_2 with(appendonly=true, compresstype=quicklz) as
#          select * from viewability_summary_2 distributed by (transaction_nbr)")
# sqlQuery(gp, "analyze anly_work_tbls.nh_viewability_summary_2")
# 
# sqlQuery(gp, "drop view if exists anly_work_tbls.nh_viewability_summary_view_2")
# sqlQuery(gp, "create view anly_work_tbls.nh_viewability_summary_view_2 as select * from anly_work_tbls.nh_viewability_summary_2 where company_id = 2495")
# 
# 
