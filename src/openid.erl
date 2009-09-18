%%%-------------------------------------------------------------------
%%% File    : openid.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created : 18 Sep 2009 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(openid).

-export([discover/1, test/0]).

-include("openid.hrl").

-define(GV(E, P), proplists:get_value(E, P)).
-define(GVD(E, P, D), proplists:get_value(E, P, D)).
-define(DBG(Term), io:format("~p: ~p~n", [self(), Term])).


discover(Identifier) ->
    case yadis:retrieve(Identifier) of
        {none, Body} -> html_discovery(Body);
        XRDS -> extract_identifier(XRDS)
    end.
    
        
extract_identifier(XRDS) ->
    case extract_op_id(XRDS) of
        none -> extract_claimed_id(XRDS);
        OpID -> OpID
    end.

extract_op_id(XRDS) -> 
    case find_service_type(XRDS#xrds.services, "http://specs.openid.net/auth/2.0/server") of
        none -> none;
        URL -> #authReq{opURL=URL, version={2,0}}
    end.
            
extract_claimed_id(XRDS) -> 
    extract_claimed_id(XRDS, [{"http://specs.openid.net/auth/2.0/signon", {2,0}},
                              {"http://openid.net/signon/1.1", {1,1}},
                              {"http://openid.net/signon/1.0", {1,0}}]).

extract_claimed_id(_, []) ->
    none;
extract_claimed_id(XRDS, [{Type,Version}|Rest]) ->
    case find_service_type(XRDS#xrds.services, Type) of
        none -> extract_claimed_id(XRDS, Rest);
        URL -> #authReq{opURL=URL, version=Version}
    end.
            
    
find_service_type([], _) -> none;
find_service_type([{Types, []}|Rest], Type) -> find_service_type(Rest, Type);
find_service_type([{Types, [URL|_]}|Rest], Type) ->
    case lists:any(fun(X) -> X == Type end, Types) of
        true -> URL;
        false -> find_service_type(Rest, Type)
    end.

html_discovery(Body) ->
    html_discovery(Body, [{"openid2.provider", "openid2.local_id", {2,0}},
                          {"openid.server", "openid.delegate", {1,1}}]).

html_discovery(_, []) ->
    none;
html_discovery(Body, [{ProviderRel, LocalIDRel, Version}|Rest]) ->
    case openid_utils:get_tags(Body, "link", "rel", ProviderRel) of
        [Tag|_] ->
            case ?GVD("href", Tag, none) of
                none -> html_discovery(Body, Rest);
                URL ->
                    LocalID = html_local_id(Body, LocalIDRel),
                    #authReq{opURL=URL, version=Version, localID=LocalID}
            end;
        _ -> html_discovery(Body, Rest)
    end.
    
html_local_id(Body, RelName) ->
    case openid_utils:get_tags(Body, "link", "rel", RelName) of
        [Tag|_] -> ?GVD("href", Tag, none);
        _ -> none
    end.
            

%% ------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------

test() ->

    ?DBG({"Google:", discover("https://www.google.com/accounts/o8/id")}),
    ?DBG({"AOL:", discover("http://openid.aol.com/brend")}), 
    ?DBG({"Flickr:", discover("http://flickr.com/exbrend")}),
    ?DBG({"Myspace:", discover("www.myspace.com")}),
    ?DBG({"LiveJournal:", discover("http://exbrend.livejournal.com")}),  
    ?DBG({"XRI Brend:", discover("=brendonh")}),                         

    application:stop(inets). % Avoid error spam from held-open connections
