-module(engine).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([start/0, show_loopdata/0, stop/0]).
-export([show_loopdata_all_nodes/1]).
-export([send_tweet_from_Nth_Node/2]).
-export([send_random_retweet_from_Nth_Node/1]).
-export([update_all_nodes/1]).
-export([register_live_user/1]).
-import(node, [start_all_nodes/0]).
-include("utility.hrl").

start() ->
    io:format("~p~n~n", ['Starting twitter engine...']),
    gen_server:start_link({local, 'Engine'}, ?MODULE, [], []).
    % node:start_all_nodes(NNodes),
    % timer:sleep(500),
    % update_all_nodes(NNodes).

register_live_user(LiveUser) ->
    gen_server:call('Engine', {'register_live_user', LiveUser}).

show_loopdata_all_nodes(0) ->
    ok;
show_loopdata_all_nodes(NNodes) ->
    gen_server:call('Engine', {'loopdata_from_Nth_node', NNodes}),
    show_loopdata_all_nodes(NNodes-1).

show_loopdata() ->
    gen_server:call('Engine', {show_loopdata}).

update_all_nodes(0) ->
    ok;
update_all_nodes(NNodes) ->
    gen_server:call('Engine', {'update_node', NNodes}),
    update_all_nodes(NNodes-1).

send_tweet_from_Nth_Node(N, _Tweet)->
    gen_server:call('Engine', {'send_tweet_from_Nth_Node', N, _Tweet}).

% picks a random tweet in user's feed and retweets it, if no tweets in feed does nothing
send_random_retweet_from_Nth_Node(N) ->
    gen_server:call('Engine', {'send_random_retweet_from_Nth_Node', N}).    

stop() ->
    gen_server:cast('Engine', {stop}).

%% Callback Functions

init([]) -> 
    {ok, {{users, []}, {tweets, []}, {live_user, {valid, false}, {user, 0}, {hashtags, []}}}}.

handle_call({show_loopdata}, _From, _LoopData) ->
    io:format("~p~n~n", [{'Engine::show_loopdata', _LoopData}]),
    file:write_file("./scratch.txt", io_lib:fwrite("~p", [_LoopData])),
    {reply, normalReply, _LoopData};

% handle_call({send_loopdata_for_websocket}, _From, _LoopData) ->
%     spawn(node, update_self, [NodePID, NthData]),
%     {reply, normalReply, _LoopData};

handle_call({register_live_user, NewLiveUser}, _From, _LoopData) ->
    {Users, Tweets, _LiveUser} = _LoopData,
    NewLoopData = {Users, Tweets, NewLiveUser},
    io:format("~p~n~n", [{'live_user_registered', NewLiveUser}]),
    {reply, normalReply, NewLoopData};

handle_call({register_user, NewData}, _From, _LoopData) ->
    {{users, Data}, Tweets, LiveUser} = _LoopData,
    _NewData = Data ++ [NewData],
    NewLoopData = {{users, _NewData}, Tweets, LiveUser},
    {reply, normalReply, NewLoopData};

handle_call({update_node, N}, _From, _LoopData) ->
    {{_, Data}, _, _} = _LoopData,
    NthData = lists:nth(length(Data) - N + 1, Data),
    {_, {_, NodePID}, _, _} = NthData,

    spawn(node, update_self, [NodePID, NthData]),

    {reply, normalReply, _LoopData};

handle_call({send_tweet_from_Nth_Node, N, Tweet}, _From, _LoopData) ->

    {{_, Data}, _, _} = _LoopData,
    NthData = lists:nth(length(Data) - N + 1, Data),
    {_Name, {_, NodePID}, _, _} = NthData,

    % io:format("~p~n", [{'engine::send_tweet_from_Nth_Node', {name, _Name}}]),

    spawn(node, send_tweet, [NodePID, Tweet]),

    {reply, normalReply, _LoopData};

handle_call({send_random_retweet_from_Nth_Node, N}, _From, _LoopData) ->

    % io:format("~p~n", [{'Engine::send_random_retweet_from_Nth_Node'}]),
    {{_, Data}, _, _} = _LoopData,
    NthData = lists:nth(length(Data) - N + 1, Data),

    {_Name, _NodePID, _Subscribed, {tweets, {received, UserFeedTweets}}} = NthData,

    Retweet = lists:nth(rand:uniform(length(UserFeedTweets)), UserFeedTweets),

    spawn(node, send_retweet, [NthData, Retweet]),

    {reply, normalReply, _LoopData};

handle_call({query_subscribed_tweets_from_Nth_Node, N}, _From, _LoopData) ->

    {{_, Data}, GlobalTweets, LiveUser} = _LoopData,

    NthData = lists:nth(length(Data) - N + 1, Data),

    {_Name, _NodePID, {subscribed, Subscribed}, {tweets, {received, ReceivedTweets}}} = NthData,

    {tweets, GlobalTweetsList} = GlobalTweets,
    ValidTweets = query_subscribed_tweets(Subscribed, GlobalTweetsList, []),

    NewNthDataReceivedTweets = lists:umerge(ReceivedTweets, ValidTweets),
    NewNthData = {_Name, _NodePID, {subscribed, Subscribed}, {tweets, {received, NewNthDataReceivedTweets}}},
    _NewGlobalUsersData = lists:keystore(_Name, 1, Data, NewNthData),
    NewGlobalUsersData = {users, _NewGlobalUsersData},

    NewLoopData = {NewGlobalUsersData, GlobalTweets, LiveUser},

    % io:format("~p~n", [{{'Engine::query_subscribed_tweets_from_Nth_Node'}, {'subscribedTweets', SubscribedTweets}}]),

    {reply, normalReply, NewLoopData};

handle_call({query_hashtag_tweets_from_Nth_Node, N, Hashtags}, _From, _LoopData) ->

    {{_, Data}, GlobalTweets, LiveUser} = _LoopData,

    NthData = lists:nth(length(Data) - N + 1, Data),

    {_Name, _NodePID, Subscribed, {tweets, {received, ReceivedTweets}}} = NthData,

    {tweets, GlobalTweetsList} = GlobalTweets,
 
    ValidTweets = query_hashtag_tweets(Hashtags, GlobalTweetsList, []),

    % io:format("~p~n", [{{'engine::query_hashtag_tweets_from_Nth_Node'}, {subscribedTweets, SubscribedTweets}}]),

    NewNthDataReceivedTweets = lists:umerge(ReceivedTweets, ValidTweets),
    NewNthData = {_Name, _NodePID, Subscribed, {tweets, {received, NewNthDataReceivedTweets}}},
    _NewGlobalUsersData = lists:keystore(_Name, 1, Data, NewNthData),
    NewGlobalUsersData = {users, _NewGlobalUsersData},

    NewLoopData = {NewGlobalUsersData, GlobalTweets, LiveUser},

    % NewLoopData = _LoopData,

    {reply, normalReply, NewLoopData};


handle_call({query_mention_tweets_from_Nth_Node, N}, _From, _LoopData) ->

    {{_, Data}, GlobalTweets, LiveUser} = _LoopData,

    NthData = lists:nth(length(Data) - N + 1, Data),

    {_Name, _NodePID, Subscribed, {tweets, {received, ReceivedTweets}}} = NthData,

    {tweets, GlobalTweetsList} = GlobalTweets,
    
    MentionString = "@" ++ integer_to_list(N),
    ValidTweets = query_mention_tweets(MentionString, GlobalTweetsList, []),

    % io:format("~p~n", [{{'engine::query_hashtag_tweets_from_Nth_Node'}, {subscribedTweets, SubscribedTweets}}]),

    NewNthDataReceivedTweets = lists:umerge(ReceivedTweets, ValidTweets),
    NewNthData = {_Name, _NodePID, Subscribed, {tweets, {received, NewNthDataReceivedTweets}}},
    _NewGlobalUsersData = lists:keystore(_Name, 1, Data, NewNthData),
    NewGlobalUsersData = {users, _NewGlobalUsersData},

    NewLoopData = {NewGlobalUsersData, GlobalTweets, LiveUser},

    % NewLoopData = _LoopData,

    {reply, normalReply, NewLoopData};

handle_call({receive_tweet, NewTweet}, _From, _LoopData) ->
    {Users, {tweets, Tweets}, LiveUser} = _LoopData,

    NewTweets = Tweets ++ [NewTweet],

    NewLoopData = {Users, {tweets, NewTweets}, LiveUser},

    {live_user, {valid, LiveUserValid}, {user, LiveUserN}, {hashtags, Hashtags}} = LiveUser,

    if
    LiveUserValid ->
        {users, Data} = Users,
        NthData = lists:nth(length(Data) - LiveUserN + 1, Data),

        % {{name,3},
        % {nodePID,<0.95.0>},
        % {subscribed,[]},
        % {tweets,{sent_original,[]},
        %         {sent_retweet,[]},
        %         {received,[]}}}

        % io:format("~p~n~n", [{'engine::receive_tweet', {nthdata, NthData}}]),

        {_Name, _NodePID, {subscribed, Subscribed}, {tweets, {received, _ReceivedTweets}}} = NthData,
        MentionString = "@" ++ integer_to_list(LiveUserN),

        SubscribedTweet = query_subscribed_tweets(Subscribed, [NewTweet], []),
        HashtagTweet = query_hashtag_tweets(Hashtags, [NewTweet], []),
        MentionTweet = query_mention_tweets(MentionString, [NewTweet], []),

        NewNthDataReceivedTweets0 = lists:umerge(_ReceivedTweets, SubscribedTweet),
        NewNthDataReceivedTweets1 = lists:umerge(NewNthDataReceivedTweets0, HashtagTweet),
        NewNthDataReceivedTweets2 = lists:umerge(NewNthDataReceivedTweets1, MentionTweet),

        NewNthData = {_Name, _NodePID, {subscribed, Subscribed}, {tweets, {received, NewNthDataReceivedTweets2}}},
        _NewGlobalUsersData = lists:keystore(_Name, 1, Data, NewNthData),
        NewGlobalUsersData = {users, _NewGlobalUsersData},
    
        NewLoopDataWithLiveUserUpdate = {NewGlobalUsersData, {tweets, NewTweets}, LiveUser},

        % io:format("~p~n", [{'engine::receive_tweet', {liveUser, LiveUser}, {tweet, NewTweet}, {subscribedTweet, SubscribedTweet}, {hashtagTweet, HashtagTweet}, {mentionTweet, MentionTweet}, {mentionaString, MentionString}}]),

        if 
        length(SubscribedTweet) == 1 ->
            [TweetS | _RestS] = SubscribedTweet,
            {AuthorS, ContentS, TypeS, _HashtagsS, _MentionsS} = TweetS,

            if 
            AuthorS /= {author, LiveUserN} ->
                io:format("~p~n~n", [{'livefeed::subscribed_tweet', {AuthorS, ContentS, TypeS}}]);
            true ->
                _nullSS = null
            end;

        true ->
            _nullS = null
        end,

        if 
        length(HashtagTweet) == 1 ->
            [TweetH | _RestH] = HashtagTweet,
            {AuthorH, ContentH, TypeH, _HashtagsH, _MentionsH} = TweetH,

            if 
            AuthorH /= {author, LiveUserN} ->
                io:format("~p~n~n", [{'livefeed::hashtag_matched_tweet', {AuthorH, ContentH, TypeH}}]);
            true ->
                _nullHH = null
            end;

        true ->
            _nullH = null
        end,

        if 
        length(MentionTweet) == 1 ->
            [TweetM | _RestM] = MentionTweet,
            {AuthorM, ContentM, TypeM, _HashtagsM, _MentionsM} = TweetM,

            if 
            AuthorM /= {author, LiveUserN} ->
                io:format("~p~n~n", [{'livefeed::mentioned_tweet', {AuthorM, ContentM, TypeM}}]);
            true ->
                _nullMM = null
            end;

        true ->
            _nullM = null
        end,
        {reply, normalReply, NewLoopDataWithLiveUserUpdate};

    true ->
        {reply, normalReply, NewLoopData}
    end;

handle_call({loopdata_from_Nth_node, N}, _From, _LoopData) ->

    {{users, Data}, _, _} = _LoopData,
    NthData = lists:nth(N, Data),
    {_, {_, NodePID}, _, _} = NthData,

    node:show_loopdata(NodePID),
    {reply, normalReply, _LoopData}.

handle_cast({stop}, LoopData) -> 
    {stop, normal, LoopData}.

terminate(_Reason, _LoopData) ->
    io:format("~p -- ~p~n~n", [terminate, {loopdata, _LoopData}]).
    