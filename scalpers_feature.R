#use data.table fread to import data much faster
library(data.table)
library(dplyr)

#**load data**
#low and slow
#purchase = read.csv('data_fest/Data/approved_data_purchase-v5.csv')
#faster
purchase = fread('data_fest/Data/approved_data_purchase-v5.csv', na.strings = c('','NA',NA,'NULL',NULL))
purchase = data.frame(purchase)

#**inspect data **
#percent NA by column
colMeans(is.na(purchase))
#get a feel for the dat
head(purchase)
summary(purchase)


#**clean data**
#clean future data out
purchase$event_dt = as.Date(purchase$event_dt)
purchase$print_dt = as.Date(purchase$print_dt)
purchase$sales_ord_tran_dt = as.Date(purchase$sales_ord_tran_dt)
purchase = filter(purchase, event_dt < as.Date('2038-01-01'))
purchase = filter(purchase, trans_face_val_amt >= 0)


#**feature creation**
dfx = data.frame(table(purchase$purch_party_lkup_id))
names(dfx)[1] = 'purch_party_lkup_id'
names(dfx)[2] = 'number_purchase'

#do using dplyr
#summarise(group_by(purchase,purch_party_lkup_id),
#          num_purchase = n())

purchase = data.frame(purchase)

#collect return users
group_user = group_by(purchase, purch_party_lkup_id)

#build user-table:
#create primary features
sum_purch = summarise(group_user,
                      num_purchase = n(),
                      num_event = n_distinct(event_id),
                      total_tickets = sum(tickets_purchased_qty),
                      avg_num_tickets = mean(tickets_purchased_qty),
                      total_paid = sum(trans_face_val_amt),
                        avg_price_paid = mean(trans_face_val_amt/tickets_purchased_qty),
                      num_days_purch = n_distinct(sales_ord_tran_dt),
                      num_cities = n_distinct(venue_city),
                      num_state = n_distinct(venue_state)
                      )

#old way create num_purchase
#feature: number purchase by id from shyam
#sum_purch = merge(sum_purch, dfx, by = 'purch_party_lkup_id', all.x = TRUE)

#features: number regular, number junk, ratio regular to junk
junk = summarise(group_user,
                 num_reg = sum(la_valid_tkt_event_flg == 'Y '),
                 num_junk = sum(la_valid_tkt_event_flg == 'N '),
                 diff_reg = sum(la_valid_tkt_event_flg == 'Y ') - sum(la_valid_tkt_event_flg == 'N ')
                )

sum_purch = merge(sum_purch, junk, by = 'purch_party_lkup_id', all.x = TRUE)


##features: locations/day, tickets/day,
per_day = summarise(group_user,
                    purch_per_day = n()/n_distinct(sales_ord_tran_dt),
                    ticket_per_day = sum(tickets_purchased_qty)/n_distinct(sales_ord_tran_dt),
                    tick_plus_purch_day = (sum(tickets_purchased_qty)+n())/n_distinct(sales_ord_tran_dt))

sum_purch = merge(sum_purch, per_day, by = 'purch_party_lkup_id', all.x = TRUE)



#*save features*
#only look at people who have returned
buys = filter(sum_purch, num_purchase > 1)

write.csv(buys,'data_fest/return_users.csv')




#look at purchase per day per event
prd = summarise(group_by(purchase,event_id,sales_ord_tran_dt), n_sale = n())


### debug:groups

samp = group_user[1:4000,]

gru_samp = group_by(samp, purch_party_lkup_id)
head(gru_samp)

summ = summarise(group_user,
                 n_purch = n())



