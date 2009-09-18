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
        XRDS -> extract_authreq(XRDS)
    end.
    
        
extract_authreq(XRDS) ->
    case authreq_by_opid(XRDS) of
        none -> authreq_by_claimed_id(XRDS);
        Req -> Req
    end.

authreq_by_opid(XRDS) -> 
    case find_service(XRDS#xrds.services, "http://specs.openid.net/auth/2.0/server") of
        none -> none;
        Service -> build_authReq(XRDS, Service, {2,0})
    end.
            
    
find_service([], _) -> none;
find_service([#xrdService{uris=[]}|Rest], Type) -> find_service(Rest, Type);
find_service([#xrdService{types=Types}=Service|Rest], Type) ->
    case lists:any(fun(X) -> X == Type end, Types) of
        true -> Service;
        false -> find_service(Rest, Type)
    end.


authreq_by_claimed_id(XRDS) -> 
    authreq_by_claimed_id(XRDS, [{"http://specs.openid.net/auth/2.0/signon", {2,0}},
                                 {"http://openid.net/signon/1.1", {1,1}},
                                 {"http://openid.net/signon/1.0", {1,0}}]).

authreq_by_claimed_id(_, []) ->
    none;
authreq_by_claimed_id(XRDS, [{Type,Version}|Rest]) ->
    case find_service(XRDS#xrds.services, Type) of
        none -> authreq_by_claimed_id(XRDS, Rest);
        Service -> build_authReq(XRDS, Service, Version)
    end.


build_authReq(XRDS, Service, Version) ->
    [URL|_] = Service#xrdService.uris,
    #authReq{opURL=URL, version={2,0}, 
             claimedID=XRDS#xrds.claimedID,
             localID=Service#xrdService.localID}.


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

    ?DBG({"Someone:", discover("blog.paulbonser.com")}),
    ?DBG({"Google:", discover("https://www.google.com/accounts/o8/id")}),
    ?DBG({"AOL:", discover("http://openid.aol.com/brend")}), 
    ?DBG({"Flickr:", discover("http://flickr.com/exbrend")}),
    ?DBG({"Myspace:", discover("www.myspace.com")}),
    ?DBG({"LiveJournal:", discover("http://exbrend.livejournal.com")}),  
    ?DBG({"XRI Brend:", discover("=brendonh")}),

    application:stop(inets). % Avoid error spam from held-open connections
