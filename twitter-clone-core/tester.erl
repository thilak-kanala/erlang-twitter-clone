-module(tester).
-export([start/1, test/0]).
-export([simulate_tweets/3]).
-export([get_random_tweet/2, get_n_hashtags/4, get_n_mentions/4]).
-export([get_random_subscribed_list/3]).
-export([register_all_users/3]).
-export([simulate_zipf_distribution/1]).
-export([simulate_zipf_distribution_each_user/6]).
-export([get_empty_user_subscriber_count_structure/2]).
-export([evaluate_user_subscriber_count/2]).
-export([update_user_subscriber_count_rank/3]).
-export([send_loopdata_for_websocket/1]).
-include("utility.hrl").

start(NNodes) ->
    engine:start(),

    _AllUsers = register_all_users(NNodes, NNodes, []),

    Tweet1 = get_random_tweet(1, NNodes),
    engine:send_tweet_from_Nth_Node(1, Tweet1),
    EngineLoopData = engine:show_loopdata(),
    io:format("~p~n", [EngineLoopData]).

send_loopdata_for_websocket(LoopData) ->
    io:format("~p~n", [{{tester, LoopData}}]).

simulate_zipf_distribution_each_user([], _NNodes, UpdatedUserSubscriberCount, NRanks, NSubs, NTweets) ->
    {UpdatedUserSubscriberCount, NRanks, NSubs, NTweets};
simulate_zipf_distribution_each_user([UserNSubCount | UserSubscriberCount], NNodes, UpdatedUserSubscriberCount, NRanks, NSubs, NTweets) ->
    {UserN, {count, Count}, {rank, Rank}, _NTweets} = UserNSubCount,

    _UserNTweetsZipfNumber = round((?NTOTALTWEETS * ?ZIPHCONSTANT) / Rank),
    UserNTweetsZipfNumber = {nTweets, _UserNTweetsZipfNumber},

    NewUpdatedUserSubscriberCount = UpdatedUserSubscriberCount ++ [{UserN, {count, Count}, {rank, Rank}, UserNTweetsZipfNumber}],

    NewNRanks = NRanks ++ [Rank],
    NewNSubs = NSubs ++ [Count],
    NewNTweets = NTweets ++ [_UserNTweetsZipfNumber],

    {user, _UserN} = UserN,
    simulate_tweets_each_tweet(_UserN, NNodes, _UserNTweetsZipfNumber),
    simulate_zipf_distribution_each_user(UserSubscriberCount, NNodes, NewUpdatedUserSubscriberCount, NewNRanks, NewNSubs, NewNTweets).

simulate_zipf_distribution(NNodes) ->
    engine:start(),
    timer:sleep(500),

    AllUsers = register_all_users(NNodes, NNodes, []),
    timer:sleep(500),

    io:format("~p~n~n", [{"Simulating Zipf Distribution with: ", {'Total Users: ', NNodes}, {'Total Tweets: ', ?NTOTALTWEETS}, {'Zipf Constant: ', ?ZIPHCONSTANT}}]),

    UserSubscriberCount0 = get_empty_user_subscriber_count_structure(NNodes, []),
    UserSubscriberCount1 = evaluate_user_subscriber_count(AllUsers, UserSubscriberCount0),
    UserSubscriberCount2 = lists:keysort(2, UserSubscriberCount1),
    UserSubscriberCount3 = update_user_subscriber_count_rank(lists:reverse(UserSubscriberCount2), 1, []),

    {_UserSubscriberCount, _NRanks, _NSubs, _NTweets} = simulate_zipf_distribution_each_user(UserSubscriberCount3, NNodes, [], [], [], []),

    io:format("~p~n~n", [{_UserSubscriberCount}]).
    % file:write_file("./zipf_data.txt", io_lib:fwrite("~w~n~n~w~n~n~w~n~n", [NRanks, NSubs, NTweets])).

update_user_subscriber_count_rank([], _Rank, UpdatedUserSubscriberCount) ->
    UpdatedUserSubscriberCount;
update_user_subscriber_count_rank([UserNSubCount | UserSubscriberCount], Rank, UpdatedUserSubscriberCount) ->
    {UserN, Count, {rank, _OldRank}, NTweets} = UserNSubCount,
    NewUserNSubCount = {UserN, Count, {rank, Rank}, NTweets},
    NewUpdatedUserSubscriberCount = UpdatedUserSubscriberCount ++ [NewUserNSubCount],
    update_user_subscriber_count_rank(UserSubscriberCount, Rank + 1, NewUpdatedUserSubscriberCount).

evaluate_user_subscriber_count_each_sub([], UserSubscriberCount) ->
    UserSubscriberCount;
evaluate_user_subscriber_count_each_sub([SubN | Subscribed], UserSubscriberCount) ->

    % MatchThis = {{user, SubN}, {count, 0}, {rank, 0}, {nTweets, 0}},
    {value, UserNSubCount} = lists:keysearch({user, SubN}, 1, UserSubscriberCount),

    {UserN, {count, Count}, Rank, NTweets} = UserNSubCount,

    NewUserNSubCount = {UserN, {count, Count + 1}, Rank, NTweets},
    NewUserSubscriberCount = lists:keyreplace({user, SubN}, 1, UserSubscriberCount, NewUserNSubCount),
    evaluate_user_subscriber_count_each_sub(Subscribed, NewUserSubscriberCount).


evaluate_user_subscriber_count([], UserSubscriberCount) ->
    UserSubscriberCount;
evaluate_user_subscriber_count([UserN | AllUsers], UserSubscriberCount) ->
    {_Name, _NodePID, {subscribed, Subscribed}, _Tweets} = UserN,
    NewUserSubscriberCount = evaluate_user_subscriber_count_each_sub(Subscribed, UserSubscriberCount),
    evaluate_user_subscriber_count(AllUsers, NewUserSubscriberCount).


get_empty_user_subscriber_count_structure(0, UserSubscriberCount) ->
    UserSubscriberCount;
get_empty_user_subscriber_count_structure(UserN, UserSubscriberCount) ->
    UserNData = {{user, UserN}, {count, 0}, {rank, 0}, {nTweets, 0}},
    NewUserSubscriberCount = UserSubscriberCount ++ [UserNData],
    get_empty_user_subscriber_count_structure(UserN - 1, NewUserSubscriberCount).

register_all_users(0, _NNodes, AllUsers) ->
    engine:update_all_nodes(_NNodes),
    AllUsers;
register_all_users(UserN, NNodes, AllUsers) ->
    UserData = get_random_user(UserN, NNodes),
    NewAllUsers  = AllUsers ++ [UserData],
    node:register_user(UserData),
    register_all_users(UserN-1, NNodes, NewAllUsers).

get_random_subscribed_list(0, Subscribed, _NNodes) ->
    Subscribed;
get_random_subscribed_list(NSubscribed, Subscribed, NNodes) ->
    User = rand:uniform(NNodes),
    NewSubscribed = lists:umerge(Subscribed, [User]),
    get_random_subscribed_list(NSubscribed-1, NewSubscribed, NNodes).

get_random_user(UserN, NNodes) ->

    NSubscribed = rand:uniform(NNodes),
    Subscribed = get_random_subscribed_list(NSubscribed, [], NNodes),

    User = 
        {
            {name, UserN}, {nodePID, 0}, {subscribed, Subscribed},
            {tweets, {received, []}}
        }, 
    
    % io:format("~p~n~n", [{user, User}]),
    User.

simulate_tweets_each_tweet(_UserN, _NNodes, 0) ->
    _null = null;
simulate_tweets_each_tweet(UserN, NNodes, NTweets) ->
    _RandomTweet = get_random_tweet(UserN, NNodes),
    engine:send_tweet_from_Nth_Node(UserN, _RandomTweet),
    simulate_tweets_each_tweet(UserN, NNodes, NTweets-1).

simulate_tweets(0, _NTweets, _NNodes) ->
    % io:format("~p~n~n", ['Simulating tweets...']);
    _null = null;
simulate_tweets(UserN, _NTweets, NNodes) ->
    simulate_tweets_each_tweet(UserN, NNodes, _NTweets),
    simulate_tweets(UserN-1, _NTweets, NNodes).

get_n_hashtags(0, _NNodes, Hashtags, HashtagsString) ->
    {Hashtags, HashtagsString};
get_n_hashtags(NHashtags, NNodes, Hashtags, HashtagsString) ->
    _Hashtag = rand:uniform(NNodes),
    Hashtag = "#hashtag" ++ integer_to_list(_Hashtag),
    NewHashtags = lists:umerge(Hashtags, [Hashtag]),
    if
    NewHashtags /= Hashtags ->
        NewHashtagsString = HashtagsString ++ " " ++ Hashtag,
        get_n_hashtags(NHashtags-1, NNodes, NewHashtags, NewHashtagsString);
    true ->
        get_n_hashtags(NHashtags-1, NNodes, NewHashtags, HashtagsString)
    end.

get_n_mentions(0, _NNodes, Mentions, MentionsString) ->
    {Mentions, MentionsString};
get_n_mentions(NMentions, NNodes, Mentions, MentionsString) ->
    _Mention = rand:uniform(NNodes),
    Mention = "@" ++ integer_to_list(_Mention),
    NewMentions = lists:umerge(Mentions, [Mention]),

    if
    NewMentions /= Mentions ->
        NewMentionsString = MentionsString ++ " " ++ Mention,
        get_n_mentions(NMentions-1, NNodes, NewMentions, NewMentionsString);
    true ->
        get_n_mentions(NMentions-1, NNodes, NewMentions, MentionsString)
    end.


% return a random tweet by userN
get_random_tweet(_UserN, NNodes) ->

    NHashtags = rand:uniform(round(?NHASHTAGS)),
    NMentions = rand:uniform(round(?NMENTIONS)),

    {Hashtags, HashtagsString} = get_n_hashtags(NHashtags, NNodes, [], ""),
    {Mentions, MentionsString} = get_n_mentions(NMentions, NNodes, [], ""),

    Content = "Hello! this is a tweet from User " ++ integer_to_list(_UserN) ++ "!" ++ HashtagsString ++ MentionsString,

    Tweet = 
        {
            {author, _UserN},
            {content, Content},
            {type, "ORIGINAL"},
            {hashtags, Hashtags},
            {mentions, Mentions}
        },
    
    % io:format("~p~n~n", [{{tweet, Tweet}}]),
    Tweet.


test() ->
    Data = {user, [tweets, [tweet1, tweet2]]},
    








