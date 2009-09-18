%%%-------------------------------------------------------------------
%%% File    : openid_utils.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created : 18 Sep 2009 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(openid_utils).

-export([get_tags/2, get_tags/4]).

-include("openid.hrl").

get_tags(Content, Tag) ->
    find_tags(Content, {[], Tag, none, none}).

get_tags(Content, Tag, AttrName, AttrVal) ->
    find_tags(Content, {[], Tag, string:to_lower(AttrName), string:to_lower(AttrVal)}).

find_tags("</head>" ++ _Rest, {Buffer,_,_,_}) -> lists:reverse(Buffer);
find_tags("", {Buffer,_,_,_}) -> lists:reverse(Buffer);
find_tags("<" ++ Rest, {_,Tag,_,_}=State) -> read_tag(Rest, Tag, State);
find_tags([_|Rest], State) -> find_tags(Rest, State).

read_tag([$\s|Rest], Tag, State)-> read_tag(Rest, Tag, State);
read_tag([$\r|Rest], Tag, State)-> read_tag(Rest, Tag, State);
read_tag([$\n|Rest], Tag, State)-> read_tag(Rest, Tag, State);
read_tag([$\t|Rest], Tag, State)-> read_tag(Rest, Tag, State);
read_tag([], _, State) -> find_tags("", State);
read_tag(Rest, [], State) -> get_tag_content(Rest, State);
read_tag([C1|Rest], [C2|TagRest]=Tag, State) -> 
    case string:to_lower(C1) == string:to_lower(C2) of
        true -> read_tag(Rest, TagRest, State);
        false-> read_tag(Rest, Tag, State)
    end;
read_tag(Rest, _, State) -> skip_tag(Rest, State).

skip_tag([$>|Rest], State) -> find_tags(Rest, State);
skip_tag("", State) -> find_tags("", State);
skip_tag([_|Rest], State) -> skip_tag(Rest, State).


get_tag_content(Rest, State) ->
    {Content, Tail} = get_raw_content(Rest, []),
    case re:run(string:to_lower(Content),
                "([a-z0-9-]+)\s*=\s*[\"'](.*?)[\"']", % "
                [{capture, all_but_first, list}, global]) of
        {match, Bits} -> check_attrs([{string:to_lower(K),V} || [K,V] <- Bits], Tail, State);
        _ -> find_tags(Tail, State)
    end.

get_raw_content(">" ++ Tail, Content) -> {lists:reverse(Content), Tail};
get_raw_content([Char|Rest], Bits) -> get_raw_content(Rest, [Char|Bits]).

check_attrs(PropList, Tail, {Buffer,Tag,none,none}) ->
    find_tags(Tail, {[PropList|Buffer],Tag,none,none});
check_attrs(PropList, Tail, {_,_,Key,Val}=State) ->
    case ?GVD(Key, PropList, none) of
        none -> find_tags(Tail, State);
        IVal -> check_val(string:to_lower(IVal), Val, PropList, Tail, State)
    end.

check_val(V, V, PropList, Tail, {Buffer,Tag,Key,Val})->
    find_tags(Tail, {[PropList|Buffer],Tag,Key,Val});
check_val(_, _, _, Tail, State) ->
    find_tags(Tail, State).
