% vim: ts=4 sw=4 et
% Simple Bridge
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (simple_bridge_multipart).
-include("simple_bridge.hrl").
-export ([parse/1]).

% Alas, so many Erlang HTTP Servers, and so little parsing of Multipart forms.
% This file contains multipart form parsing logic that is shared by all 
% request bridges.
% Large portions of this file are from mochiweb_multipart.erl
% Copyright 2007 Mochi Media, Inc., written by Bob Ippolito <bob@mochimedia.com>.

-define(CHUNKSIZE, 8 * 1024).
-define(IDLE_TIMEOUT, 30000).

% Override with simple_bridge configuration {max_post_size, SizeInMB}
-define (MAX_POST_SIZE, 100).

-record (state, {
    req,          % The simplebridge request object.
    boundary,     % The multipart boundary
    length,       % The length of the post.
    bytes_read=0, % How many bytes we have read so far.
    parts,        % The finished parts we have accumulated.
    eof=false
}).

-record (part, {
    name,         % The name of the form element that created this part
    value=[],     % The value of the part, as a string, or {'file', FileUploadHandler} if it's a file
    filename,     % The name of the posted file
    mime_type,    % The mime type of the file
    size=0,       % The size of the part's value
    needs_rn      % True if we should add a \r\n before the next write 
}).

-define (NEWLINE, "\r\n").

parse(Req) ->
    case is_multipart_request(Req) of
        true ->  parse_multipart(Req);
        false -> {ok, not_multipart}
    end.

is_multipart_request(Req) ->
    try sbw:header_lower(content_type, Req) of
        "multipart/form-data" ++ _  -> true;
        _                           -> false
    catch _:_                       -> false
    end.

parse_multipart(Req) ->
    try
        Boundary = get_multipart_boundary(Req),
        Length = get_content_length(Req),
        ok = crash_if_too_big(Length, #state{parts = [], req=Req}),
        Data = get_opening_body(Req), 
        State = init_state(Req, Boundary, Length, Data),
        State1 = read_boundary(Data, State),
        {Params, Files} = process_parts(State1#state.parts),
        {ok, Params, Files}
    catch
        throw : {Reason, ErrState} ->
            lists:foreach(fun(Part) ->
                file:delete(Part#sb_uploaded_file.temp_file)
            end, convert_parts_to_files(ErrState#state.parts)),
            {error, Reason}
    end.

get_content_length(Req) ->
    to_integer(sbw:header(content_length, Req)).

get_opening_body(Req) ->
    simple_bridge_util:to_binary(sbw:request_body(Req)).


get_multipart_boundary(Req) ->
    {_K, _V, Props} = parse_header(sbw:header(content_type, Req)),
    Boundary = proplists:get_value("boundary", Props),
    simple_bridge_util:to_binary(Boundary).


init_state(Req, Boundary, Length, Data) ->
    #state{
        req = Req,
        boundary = Boundary,
        length=Length,
        bytes_read = size(Data),
        parts = []
    }.

crash_if_too_big(Length, State) ->
    case Length > get_max_post_size() of
        true  ->
            %flush_socket(State),
            throw({post_too_big, State});
        false -> ok
    end.

%% THis is just here for experiments. Not really used.
%flush_socket(_State = #state{req=Req}) ->
%    error_logger:info_msg("Flushing Socket: ~p",[Req]),
%    try flush_worker(Req)
%    catch Error:Reason -> error_logger:info_msg("Flush ended in Error: ~p:~p.~nsbw: ~p~nStacktrace: ~p", [Error, Reason, Req, erlang:get_stacktrace()]), ok
%    end.
%
%
%flush_worker(Req) ->
%    case sbw:recv_from_socket(?CHUNKSIZE, 10, Req) of
%        <<>> -> 
%            error_logger:info_msg("All Data Flushed"),
%            ok;
%        _Data ->
%            error_logger:info_msg("Flushed ~p bytes",[byte_size(_Data)]),
%            flush_worker(Req)
%    end.
    


process_parts(Parts) ->
    Params = convert_parts_to_params(Parts),
    Files = convert_parts_to_files(Parts),
    {Params, Files}.

convert_parts_to_params(Parts) ->
    [
        {simple_bridge_util:to_binary(Name),
         simple_bridge_util:to_binary(Value)
        } || #part { name=Name, value=Value, filename=undefined } <- Parts
    ].

convert_parts_to_files(Parts) ->
    [#sb_uploaded_file {
        original_name=Filename,
        temp_file=sb_file_upload_handler:get_tempfile(FileUploadHandler),
        data=sb_file_upload_handler:get_data(FileUploadHandler),
        size=Size,
        field_name=Name
    } || #part { filename=Filename, value={file, FileUploadHandler}, size=Size, name=Name } <- Parts].

% Not yet in a part. Read the POST headers to get content boundary and length.
read_boundary(Data, State = #state { boundary=Boundary }) ->
    {Line, Data1, undefined, State1} = get_next_line(Data, undefined, State),
    case interpret_line(Line, Boundary, State1) of
        start_next_part -> read_part_header(Data1, #part {}, State1);
        Other -> throw({unexpected, Other, Line})
    end.

%%% PART HEADERS %%%

% We are in a part. Read headers.
read_part_header(Data, Part, State = #state { boundary=Boundary }) ->
    {Line, Data1, Part1, State1} = get_next_line(Data, Part, State),
    case interpret_line(Line, Boundary, State1) of
        start_next_part -> throw({value_expected, Line});
        start_value -> read_part_value(Data1, Part1, State1);
        continue ->
            % Parse the header, add it to Part, then loop.
            Part2 = update_part_with_header(parse_header(Line), Part1),
            read_part_header(Data1, Part2, State1);
        eof -> State1
    end.

update_part_with_header({"content-disposition", "form-data", Params}, Part) ->
    Part1 = case proplists:get_value("name", Params) of
                undefined -> Part;
                Name -> Part#part { name=Name }
            end,
    case proplists:get_value("filename", Params) of
        undefined -> Part1;
        Filename ->
            Part1#part {
                filename=Filename,
                value = {file, sb_file_upload_handler:new_file(Filename)}
            }
    end;
update_part_with_header(_, Part) -> Part.

%%% PART VALUES %%%

%% If we're already at eof, there's nothing to read, just exit from the part parsing
read_part_value(_Data, Part, State = #state { eof=true }) ->
    update_state_with_part(Part, State);
% We are in a part's value. Read the value until we see a boundary.
read_part_value(Data, Part, State = #state { boundary=Boundary }) ->
    {Line, Data1, Part1, State1} = get_next_line(Data, Part, State),
    case interpret_line(Line, Boundary, State1) of
        start_next_part ->
            % Finalize the write, then start the next part.
            State2 = update_state_with_part(Part1, State1),
            read_part_header(Data1, #part {}, State2);
        A when A == start_value orelse A == continue ->
            % Write the line, then continue...  
            Part2 =
            try
                update_part_with_value(Line, true, Part1)
            catch
                throw : Reason -> throw({Reason, State})
            end,
            read_part_value(Data1, Part2, State1);
        eof ->
            update_state_with_part(Part1, State1)
    end.

update_part_with_value(Data, IsLine, Part = #part { value={file, FileUploadHandler}, size=Size, needs_rn=NeedsRN }) ->
    {Prefix, NewSize} = get_prefix_and_newsize(NeedsRN, Size, Data),

    NewFileState = sb_file_upload_handler:receive_data(FileUploadHandler, <<(list_to_binary(Prefix))/binary, Data/binary>>),

    Part#part { size=NewSize, needs_rn=IsLine, value={file, NewFileState} };

update_part_with_value(Data, IsLine, Part = #part { value=Value, size=Size, needs_rn=NeedsRN }) ->
    {Prefix, NewSize} = get_prefix_and_newsize(NeedsRN, Size, Data),
    NewValue = Value ++ Prefix ++ binary_to_list(Data),
    Part#part { value=NewValue, size=NewSize, needs_rn=IsLine }.

update_state_with_part(Part = #part { value={file, FileUploadHandler}}, State = #state { parts=Parts }) ->
    CompletedFileUploadHandler = sb_file_upload_handler:complete_file(FileUploadHandler),
    State#state { parts=[Part#part{ value={file, CompletedFileUploadHandler}}|Parts] };
update_state_with_part(Part, State = #state { parts=Parts }) ->
    State#state { parts=[Part|Parts] }.

% Returns {Prefix, NewSize}.
get_prefix_and_newsize(NeedsRN, Size, Data) ->
    case NeedsRN of
        true -> {?NEWLINE, Size + length(?NEWLINE) + size(Data)};
        _    -> {"", Size + size(Data)}
    end.


% Return the next line of input from the post, reading
% more data if necessary.
% get_next_line(Data, Part, State) -> {Line, RemainingData, Part, NewState}.
get_next_line(Data, Part, State)    -> get_next_line(Data, <<>>, Part, State).

get_next_line(<<?NEWLINE, Data/binary>>, Acc, Part, State) -> {<<Acc/binary>>, Data, Part, State};
get_next_line(<<C, Data/binary>>, Acc, Part, State) -> get_next_line(Data, <<Acc/binary, C>>, Part, State);
get_next_line(Data, Acc, Part, State = #state{length=Length, bytes_read=BytesRead, eof=EOF}) when BytesRead >= Length ->
    EOF=false,  %% Crash if we're trying to read more data but we've already reached EOF
    State1 = State#state{eof=true},
    {Data, Acc, Part, State1};
get_next_line(Data, Acc, Part, State) when Data == undefined orelse Data == <<>> ->
    {Data1, State1, Acc1, Part1} =
    try
        {TmpData1, TmpState1} = read_chunk(State),
        % We don't want Acc to grow too big, so if we have more than ?CHUNKSIZE
        % data already read, then flush it to the current part.
        {TmpAcc1, TmpPart1} = case Part /= undefined andalso size(Acc) > ?CHUNKSIZE of
                                  true -> {<<>>, update_part_with_value(Acc, false, Part)};
                                  false -> {Acc, Part}
                              end,
        {TmpData1, TmpState1, TmpAcc1, TmpPart1}
    catch
        throw : Reason -> throw({Reason, State})
    end,
    % it into a part.
    get_next_line(Data1, Acc1, Part1, State1).

read_chunk(State = #state { req=Req, length=Length, bytes_read=BytesRead }) ->
    BytesToRead = lists:min([Length - BytesRead, ?CHUNKSIZE]),
    Data = sbw:recv_from_socket(BytesToRead, ?IDLE_TIMEOUT, Req),
    NewBytesRead = BytesRead + size(Data),
    ok=crash_if_too_big(NewBytesRead, State),
    {Data, State#state { bytes_read=NewBytesRead }}.

interpret_line(Line, Boundary, State) ->
    if
        Line == <<"--", Boundary/binary, "--">>         -> eof;
        Line == <<"--", Boundary/binary>>               -> start_next_part;
        Line == <<>>                                    -> start_value;
        State#state.eof                                 -> eof;
        true                                            -> continue
    end.


parse_header(B) when is_binary(B) ->
    parse_header(unicode:characters_to_list(B));
parse_header(String) ->
    [First|Rest] = [string:strip(S) || S <- string:tokens(String, ";")],
    {Name, Value} = parse_keyvalue($:, First),
    Params = [parse_keyvalue($=,X) || X <- Rest],
    Params1 = [{K,V} || {K,V} <- Params, K /= "", V /= ""],
    {Name, Value, Params1}.

parse_keyvalue(Char, S) ->
    % If Char not found, then use an empty value...
    {Key, Value} = case string:chr(S, Char) of
                       0   -> {S, ""};
                       Pos -> {string:substr(S, 1, Pos - 1), string:substr(S, Pos + 1)}
                   end,
    {string:to_lower(string:strip(Key)),
        unquote_header(string:strip(Value))}.

get_max_post_size() ->
    simple_bridge_util:get_max_post_size(?MAX_POST_SIZE).


% unquote_header borrowed from Mochiweb.
unquote_header("\"" ++ Rest) -> unquote_header(Rest, []);
unquote_header(S) -> S.
unquote_header("", Acc) -> lists:reverse(Acc);
unquote_header("\"", Acc) -> lists:reverse(Acc);
unquote_header([$\\, C | Rest], Acc) -> unquote_header(Rest, [C | Acc]);
unquote_header([C | Rest], Acc) -> unquote_header(Rest, [C | Acc]).

to_integer(L) when is_list(L) -> list_to_integer(L);
to_integer(B) when is_binary(B) -> to_integer(binary_to_list(B));
to_integer(I) when is_integer(I) -> I.
