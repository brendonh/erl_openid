%%%-------------------------------------------------------------------
%%% File    : openid.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created : 18 Sep 2009 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(openid).

-export([discover/1, associate/1, test/0]).

-include("openid.hrl").

-define(GV(E, P), proplists:get_value(E, P)).
-define(GVD(E, P, D), proplists:get_value(E, P, D)).
-define(DBG(Term), io:format("~p: ~p~n", [self(), Term])).



%% ------------------------------------------------------------
%% Discovery
%% ------------------------------------------------------------

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
    authreq_by_opid(XRDS, ["http://specs.openid.net/auth/2.0/server",
                           "http://openid.net/server/1.1",
                           "http://openid.net/server/1.0"]).

authreq_by_opid(_, []) -> none;
authreq_by_opid(XRDS, [Type|Rest]) -> 
    case find_service(XRDS#xrds.services, Type) of
        none -> authreq_by_opid(XRDS, Rest);
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
    #authReq{opURL=URL, version=Version, 
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
%% Association
%% ------------------------------------------------------------

% Defaults from spec
-define(P, 1500073708273015748628013388693328252000303842391466352869527958572384115195772928792417592549921617769856041063651334172856114323013748155551037713908795501949688353681514443698908035718685336822727455568028009921661496375944512427).
-define(G, 2).

-define(CONTENT_TYPE, "application/x-www-form-urlencoded; charset=UTF-8").

associate(AuthReq) ->
    application:start(crypto),

    MP = crypto:mpint(?P),
    MG = crypto:mpint(?G),

    XA = crypto:rand_uniform(crypto:mpint(1), MP),
    Public = crypto:erlint(crypto:mod_exp(MG, XA, MP)),

    Params = [{"openid.ns", "http://specs.openid.net/auth/2.0"},
              {"openid.mode", "associate"},
              {"openid.assoc_type", "HMAC-SHA1"},
              {"openid.session_type", "DH-SHA1"},
              {"openid.dh_modulus", base64:encode(roll(?P))},
              {"openid.dh_gen", base64:encode(roll(?G))},
              {"openid.dh_consumer_public", base64:encode(roll(Public))}],
    
    ReqBody = mochiweb_util:urlencode(Params),
    
    Request = {AuthReq#authReq.opURL, 
               [], ?CONTENT_TYPE, ReqBody},
    
    {ok, {_,_,Body}} = http:request(post, Request, [], []),
    
    Response = parse_keyvalue(Body),

    ExpiresIn = ?GV("expires_in", Response),
    ServPublic = ?GV("dh_server_public", Response),
    MAC = ?GV("enc_mac_key", Response),
    
    {ExpiresIn, ServPublic, MAC}.                 



%%% btwoc() in Spec
roll(N) when is_integer(N) ->
    <<_Size:32, Bin/binary>> = crypto:mpint(N),
    Bin.
 
%%% The inverse to btwoc()
unroll(Bin) when is_binary(Bin) ->
    Size = size(Bin),
    crypto:erlint(<<Size:32, Bin/binary>>).


parse_keyvalue(Body) ->
    lists:reverse(lists:foldl(
      fun(E, A) -> [split_kv(E, [])|A] end,
      [], string:tokens(Body, "\n"))).

split_kv([$:|Rest], Buff) -> {lists:reverse(Buff), Rest};
split_kv([C|Rest], Buff) -> split_kv(Rest, [C|Buff]).
    
%% ------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------

test() ->

    Test = fun(ID) ->
                   ?DBG({identifier, ID}),
                   Req = discover(ID),
                   ?DBG({request, Req}),
                   ?DBG({assoc, associate(Req)})
           end,

    % 2.0
    Test("https://www.google.com/accounts/o8/id"),
    Test("http://flickr.com/exbrend"),
    Test("=brendonh"),

    % 1.0 / 1.1
    %?DBG({"AOL:", discover("http://openid.aol.com/brend")}), 
    %?DBG({"Myspace:", discover("www.myspace.com")}),
    %?DBG({"LiveJournal:", discover("http://exbrend.livejournal.com")}),  
    %?DBG({"PaulBonser:", discover("blog.paulbonser.com")}),

    application:stop(inets). % Avoid error spam from held-open connections




%% $ make test
%% erlc -o ebin -Wall -v +debug_info src/openid.erl
%% src/openid.erl:155: Warning: function unroll/1 is unused
%% erl +W w -pa ebin -noshell -pa ../mochiweb/ebin -s openid test -s init stop
%% <0.1.0>: {identifier,"https://www.google.com/accounts/o8/id"}
%% <0.1.0>: {request,{authReq,"https://www.google.com/accounts/o8/ud",
%%                            {2,0},
%%                            none,none}}
%% <0.1.0>: {assoc,{"46800",
%%                  "AO6d/PJErS+mEYxZNEsr3L/Tz6SvipjoQQW4TN8XxzHXTW8n4POIjk9kUBfl1yQvLF2rEmL4R3OqAKgDVsTIb9WzFF75+QmJXtXq5DqyQ4HRgBqgZk2RmijOHSKsVsZbsA==",
%%                  "CbImJ5wv7y1jgGCS3RWq5cCvByE="}}
%% <0.1.0>: {identifier,"http://flickr.com/exbrend"}
%% <0.1.0>: {request,{authReq,"https://open.login.yahooapis.com/openid/op/auth",
%%                            {2,0},
%%                            none,none}}
%% <0.1.0>: {assoc,{"14400",
%%                  "EBXqqs44y4MkuvOIVGF+TSUot+/FGqBtMZJm8KyQXwabUc09iB2AesfVb4J2iM2JaPdvk0VgfYur7ywJY9zCZA5bvSNKEOicFP5SAVBsfdCNyCUEjMRt0tvcDAnygWzo",
%%                  "wBens28gyUh8NzNKVS3IafvWeYE="}}
%% <0.1.0>: {identifier,"=brendonh"}
%% <0.1.0>: {request,{authReq,"https://authn.fullxri.com/authentication/",
%%                            {2,0},
%%                            "=brendonh",none}}
%% <0.1.0>: {assoc,{"1800",
%%                  "AKngKTyiIQ0JcX3/vXrnavfyLWCj6hsiOTYypoKPS25DAaprDRKkq5gXL4q0Foc+YAUqrLlTuT63W6PeVSpZEornRfNHs3Trfoxggp3N4uE8BFvlHvyf1XySXNANPbLLFQ==",
%%                  "KIW/+jlgDASt3Xx8T7vHfb1F0vU="}}
