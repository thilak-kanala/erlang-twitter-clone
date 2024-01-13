-export([query_subscribed_tweets/3]).
-export([query_hashtag_tweets/3]).
-export([query_mention_tweets/3]).
-define(NTWEETS, 1).
-define(NHASHTAGS, 10).
-define(NMENTIONS, 10).
-define(ZIPHCONSTANT, 0.1).
-define(NTOTALTWEETS, 1000).


query_subscribed_tweets([], _Tweets, SubscribedTweets) ->
    SubscribedTweets;
query_subscribed_tweets([Author | Subscribed], Tweets, SubscribedTweets) ->
    % io:format("~p~n", [{'utility::query_subscribed_tweets', {subscribed, Subscribed}, {tweets, Tweets}}]),
    % io:format("~p~n", [{'utility::query_subscribed_tweets', {'\"author\":' Author}}]),

    KeyTakeResult = lists:keytake({'\"author\":', Author}, 1, Tweets),

    if
    KeyTakeResult == false ->
        query_subscribed_tweets(Subscribed, Tweets, SubscribedTweets);
    true ->
        {value, MatchedTweet, NewTweets} = KeyTakeResult,
        NewSubscribedTweets = SubscribedTweets ++ [MatchedTweet],
        query_subscribed_tweets([Author | Subscribed], NewTweets, NewSubscribedTweets)
    end.

query_hashtag_tweets_each_tweet(_Hashtag, [], SubscribedTweets) ->
    SubscribedTweets;
query_hashtag_tweets_each_tweet(Hashtag, [Tweet | Tweets], SubscribedTweets) ->
    % Tweet = {'\"author\":' content, type, '\"hashtags\":' mentions}
    {_Author, _Content, _Type, {'\"hashtags\":', Hashtags}, _Mentions} = Tweet,

    TweetValid = lists:member(Hashtag, Hashtags),

    if
    TweetValid ->
        NewSubscribedTweets = SubscribedTweets ++ [Tweet],
        % io:format("~p~n", [{{'utility::query_hashtag_tweets_each_tweet::valid_tweet'}, {newSubscribedTweets, NewSubscribedTweets}}]),
        query_hashtag_tweets_each_tweet(Hashtag, Tweets, NewSubscribedTweets);
    true ->
        query_hashtag_tweets_each_tweet(Hashtag, Tweets, SubscribedTweets)
    end.

query_hashtag_tweets([], _Tweets, SubscribedTweets) ->
    SubscribedTweets;
query_hashtag_tweets([Hashtag | Hashtags], Tweets, SubscribedTweets) ->
    % io:format("~p~n", [{'utility::query_subscribed_tweets', {subscribed, Subscribed}, {tweets, Tweets}}]),
    % io:format("~p~n", [{'utility::query_subscribed_tweets', {'\"author\":' Author}}]),

    ReturnTweets = query_hashtag_tweets_each_tweet(Hashtag, Tweets, []),
    NewSubscribedTweets = SubscribedTweets ++ ReturnTweets,
    % io:format("~p~n", [{{'utility::query_subscribed_tweets'}, {newSubscribedTweets, NewSubscribedTweets}}]),
    query_hashtag_tweets(Hashtags, Tweets, NewSubscribedTweets).


query_mention_tweets(_Mention, [], SubscribedTweets) ->
    SubscribedTweets;
query_mention_tweets(Mention, [Tweet | Tweets], SubscribedTweets) ->
    % Tweet = {'\"author\":' content, type, '\"hashtags\":' mentions}
    {_Author, _Content, _Type, _Hashtags, {'\"mentions\":', Mentions}} = Tweet,

    TweetValid = lists:member(Mention, Mentions),

    if
    TweetValid ->
        NewSubscribedTweets = SubscribedTweets ++ [Tweet],
        % io:format("~p~n", [{{'utility::query_hashtag_tweets_each_tweet::valid_tweet'}, {newSubscribedTweets, NewSubscribedTweets}}]),
        query_mention_tweets(Mention, Tweets, NewSubscribedTweets);
    true ->
        query_mention_tweets(Mention, Tweets, SubscribedTweets)
    end.
    

    
