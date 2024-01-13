-module(node).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([start_all_nodes/1]).
-export([register_user/1]).
-export([send_tweet/2]).
-export([send_retweet/2]).
-export([update_self/2]).
-export([query_subscribed_tweets/1]).
-export([query_hashtag_tweets/2]).
-export([query_mention_tweets/1]).
-export([show_loopdata/1]).

start_all_nodes(0) ->
    io:format("~p~n~n", ['Registered All Users.']);

start_all_nodes(NNodes) ->
    {_, NodePID} = gen_server:start_link(?MODULE, [], []),

    Data = 
        {
            {name, NNodes}, {nodePID, NodePID}, {subscribed, []},
            {tweets, {received, []}}
        },

    gen_server:call('Engine', {register_user, Data}),
    start_all_nodes(NNodes - 1).

register_user(UserData) ->
    {_, NodePID} = gen_server:start_link(?MODULE, [], []),

    {Name, _NodePID, Subscribed, Tweets} = UserData,
    NewUserData = {Name, {nodePID, NodePID}, Subscribed, Tweets},

    gen_server:call('Engine', {register_user, NewUserData}).

%% ---
show_loopdata(NodePID) ->
    % {_, NodePID} = Node, 
    gen_server:call(NodePID, {show_loopdata}).

send_retweet(NodeData, Retweet) ->
    {_Author, Content, _Type, Hashtags, Mentions} = Retweet,
    {{name, NodeName}, {nodePID, NodePID}, _, _} = NodeData,
    NewAuthor = {author, NodeName},
    NewType = {type, "RETWEET"},
    NewTweet = {NewAuthor, Content, NewType, Hashtags, Mentions},
    gen_server:call(NodePID, {send_tweet, NewTweet}).

send_tweet(NodePID, Tweet) ->
    % Tweet = {author, content, type, hashtags, mentions}
    gen_server:call(NodePID, {send_tweet, Tweet}).

update_self(NodePID, Data) ->
    gen_server:call(NodePID, {update_self, Data}).

query_subscribed_tweets(NthNode) ->
    gen_server:call('Engine', {query_subscribed_tweets_from_Nth_Node, NthNode}).

query_hashtag_tweets(NthNode, Hashtags) ->
    gen_server:call('Engine', {query_hashtag_tweets_from_Nth_Node, NthNode, Hashtags}).

query_mention_tweets(NthNode) ->
    gen_server:call('Engine', {query_mention_tweets_from_Nth_Node, NthNode}).
%% ---

%% Callback Functions

init(Message) -> 
    {ok, {data, Message}}.

handle_call({show_loopdata}, _From, _LoopData) ->

    io:format("~p~n~n", [{'node::show_loopdata', {'LoopData', _LoopData}}]),

    {reply, normalReply, _LoopData};

handle_call({update_self, Data}, _From, _LoopData) ->

    NewLoopData = {data, Data},

    {reply, normalReply, NewLoopData};

handle_call({send_tweet, Tweet}, _From, _LoopData) ->

        % io:format("~p~n~n", [{'node::send_tweet', {'tweet', Tweet}}]),
        gen_server:call('Engine', {receive_tweet, Tweet}),
        {reply, normalReply, _LoopData}.

handle_cast(stop, LoopData) -> 
    {stop, normal, LoopData}.

terminate(_Reason, _LoopData) ->
    io:format("~p -- ~p~n~n", [terminate, {loopdata, _LoopData}]).