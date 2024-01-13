#!/usr/bin/env escript
%% vim: ts=4 sw=4 et ft=erlang

main([]) ->
    crypto:start(),
    code:ensure_loaded(uri_string),

    Filename = "include/compat.hrl",
    io:format("Generating compatibility for simple_bridge...\n"),
    Hash = hash(),
    MapsFilter = maps_filter(),
    RandUniform = rand_uniform(),
    ParseQS = parse_qs(),

    io:format("...?HASH/1 => ~p~n",[Hash]),
    io:format("...?MAPS_FILTER/2 => ~p~n",[MapsFilter]),
    io:format("...?RAND_UNIFORM/1 => ~p~n",[RandUniform]),
    io:format("...?PARSE_QS/1 => ~p~n",[ParseQS]),

    Contents = [
        "-define(HASH(Data), ",Hash,").\n"
        "-define(MAPS_FILTER(Pred, Map), ",MapsFilter,").\n"
        "-define(RAND_UNIFORM(Max), ",RandUniform,").\n",
        "-define(PARSE_QS(String), ",ParseQS, ").\n"
    ],

    ContentsBin = iolist_to_binary(Contents),
    case file:read_file(Filename) of
        {ok, ContentsBin} -> 
            io:format("...no changes needed to ~p. Skipping writing new file\n",[Filename]);
        _ -> 
            io:format("...writing ~p\n",[Filename]),
            file:write_file(Filename, Contents)
    end.



hash() ->
    case erlang:function_exported(crypto, hash, 2) of
        true ->
            "crypto:hash(sha, Data)";
        false ->
            "crypto:sha(Data)"
    end.

maps_filter() ->
    case erlang:function_exported(maps, filter, 2) of
        true ->
            "maps:filter(Pred, Map)";
        false ->
            "maps:from_list(lists:filter(fun({K,V}) -> Pred(K,V) end, maps:to_list(Map)))"
    end.
                

rand_uniform() ->
    case erlang:function_exported(rand, uniform, 1) of
        true ->
            "rand:uniform(Max)";
        false ->
            "crypto:rand_uniform(1, Max)"
    end.

parse_qs() ->
    case erlang:function_exported(uri_string, dissect_query, 1) of
        true ->
            "uri_string:dissect_query(String)";
        false ->
            "httpd:parse_query(String)"
    end.
