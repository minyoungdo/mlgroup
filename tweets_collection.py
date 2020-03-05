import GetOldTweets3 as got
import pandas as pd
import time
import datetime


"""Set parameters for function"""
# search_term = 'candidate name'
# num_tweets = 1000
# since_date = '2020-01-01'
# until_date = '2020-02-01'
# example of format "2015-05-01"


"""This function pulls tweets"""
def gets_tweets(search_term, num_tweets, since_date, until_date):
    #Examples of parameters: Set parameters for function
    #search_term = 'obama'
    #num_tweets = 1000
    #since_date = '2020-01-01'
    #until_date = '2020-02-01'
    #example of format "2015-05-01"

    tweetCriteria = got.manager.TweetCriteria().setQuerySearch(search_term)\
                                                .setSince(since_date)\
                                               .setUntil(until_date)\
                                               .setMaxTweets(num_tweets)
    list_of_tweets = got.manager.TweetManager.getTweets(tweetCriteria)
    
    """Creates df of tweets"""

    username_list= []
    user_id_list = []
    text_list = []
    created_when_list = []
    
    for tweet in list_of_tweets:
                
        username_list.append(tweet.username)
        user_id_list.append(tweet.author_id) 
        text_list.append(tweet.text)
        created_when_list.append(tweet.date)
    
    
    dictionary_of_tweets = {'username': username_list,
                                        'user_id': user_id_list,
                                        'text': text_list,
                                        'created': created_when_list}
    df_tweets = pd.DataFrame(dictionary_of_tweets)
    return df_tweets
    


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
"""Run this block of code first"""
"""Create list of dates, specify the number of days you want"""
numdays = 31



base = datetime.datetime.today()
date_list = [base - datetime.timedelta(days=x) for x in range(numdays + 1)]


"""Create list of dates without time of dayime portion"""
truncated_date_list = []
for i in date_list:
    year = i.year
    month= i.month
    day = i.day
    
    date_string = '%d-%d-%d' %(year, month, day)
    
    truncated_date_list.append(date_string)
    
truncated_date_list.reverse()
#------------------------------------------------------------------------------    
#------------------------------------------------------------------------------    




#------------------------------------------------------------------------------    
#------------------------------------------------------------------------------    
"""Run this code by selecting it, don't run the entire script"""
#Measures run time
start_time = time.time()

"""Faster way to run the code. Creates list of dfs, each df has n number of tweets, each df is a one day interval"""
list_of_tweets_dfs =[gets_tweets('bernie sanders', 1000, truncated_date_list[i], truncated_date_list[i+1])
              for i in range(len(truncated_date_list)-1)] 


"""Concatenates the list of dfs into one df"""
all_df = pd.concat(list_of_tweets_dfs)

#stops measuring run time
end_time = time.time() - start_time
#------------------------------------------------------------------------------    
#------------------------------------------------------------------------------    




#------------------------------------------------------------------------------    
#------------------------------------------------------------------------------    
"""Use this code to avoid the HTTP request limit, runs loop every 7.5 minutes"""
#------------------------------------------------------------------------------    
"""This pulls 10k tweets every 15 minutes, two days every 15 minutes, run time is numdays / 2 * 15mins,
will pull 40k tweets in 1 hour, 30 days will take 2 hours 5 minutes"""
day_counter = 0
list_of_tweets_dfs = []
for i in range(len(truncated_date_list)-1):
    
    
    search_1 = gets_tweets('Bernie Sanders', 3000, truncated_date_list[i], truncated_date_list[i+1])

    list_of_tweets_dfs.append(search_1)
    
    time.sleep(450)
    
    day_counter = day_counter + 1
    
    print('days completed:', day_counter)

"""Concatenates the list of dfs into one df"""
all_df = pd.concat(list_of_tweets_dfs)

"""Exports the result as a csv"""
all_df.to_csv("~/Desktop/TwitterData/bernie_tweets.csv")

#------------------------------------------------------------------------------    
#------------------------------------------------------------------------------    






#------------------------------------------------------------------------------    
"""Tests sleep function"""
start_time = time.time()
print("start")

#argument is in seconds
time.sleep(5)

print("stop")
print('elapsed time:', time.time() - start_time)
#------------------------------------------------------------------------------    


