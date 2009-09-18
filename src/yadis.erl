%%%-------------------------------------------------------------------
%%% File    : yadis.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Yadis resource discovery
%%%
%%% Created : 17 Sep 2009 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(yadis).

-export([normalize/1, retrieve/1, test/0]).

-include("openid.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(GVD(E, P, D), proplists:get_value(E, P, D)).

-define(HTTP_HEADERS, [{"Accept", "application/xrds+xml"},
                       {"Connection", "close"},
                       {"User-Agent", "Erlang/openid"}]).

-define(HTTP_OPTIONS, [{relaxed, true}]).
-define(REQ_OPTIONS, []).

%% ------------------------------------------------------------
%% API
%% ------------------------------------------------------------

normalize("xri://" ++ Identifier) -> {Identifier, true};
normalize([$=|_]=Identifier) -> {Identifier, true};
normalize([$@|_]=Identifier) -> {Identifier, true};
normalize([$+|_]=Identifier) -> {Identifier, true};
normalize([$$|_]=Identifier) -> {Identifier, true};
normalize([$!|_]=Identifier) -> {Identifier, true};
normalize([$(|_]=Identifier) -> {Identifier, true};
normalize("http://" ++ Tail) -> {strip_fragment("http://" ++ Tail), false};
normalize("https://" ++ Tail) -> {strip_fragment("https://" ++ Tail), false};
normalize(PartialURL) -> {strip_fragment("http://" ++ PartialURL), false}.
     

retrieve(Identifier) ->
    application:start(inets),
    application:start(ssl),
    
    {Normalized, IsXRI} = normalize(Identifier),
    
    URL = case IsXRI of
              true -> resolve(Normalized);
              false -> Normalized
          end,
    
    case http:request(get, {URL, ?HTTP_HEADERS}, ?HTTP_OPTIONS, ?REQ_OPTIONS) of
        {ok, {_Status, Headers, Body}} ->
            DescriptorURL = get_descriptor_url(Headers, Body),
            XRDS = handle_response(DescriptorURL, Headers, Body),
            case XRDS of 
                none -> 
                    {none, Body};
                #xrds{} -> 

                    % XXX Todo -- Normalize DescriptorURL as claimedID 
                    % (2.0 spec #7.2.4)

                    ClaimedID = case IsXRI of
                                    true -> Normalized;
                                    false -> DescriptorURL
                                end,
                    XRDS#xrds{origID=Identifier, 
                              isXRI=IsXRI,
                              claimedID=ClaimedID}
            end;
        Other ->
            {error, {http_error, {Normalized, Other}}}
    end.


%% ------------------------------------------------------------
%% Retrieval details
%% ------------------------------------------------------------

resolve(Identifier) -> "http://xri.net/" ++ Identifier ++ "?_xrd_r=application/xrds+xml".

strip_fragment(URL) -> strip_fragment(URL, []).

strip_fragment([$#|_], SoFar) -> lists:reverse(SoFar);
strip_fragment([], SoFar) -> lists:reverse(SoFar);
strip_fragment([H|T], SoFar) -> strip_fragment(T, [H|SoFar]).



handle_response(none, Headers, Body) ->
    get_xrds(?GVD("content-type", Headers, none), Body);
handle_response(URL, _Headers, _Body) ->
    try_descriptor_url(URL).


get_xrds("application/xrds" ++ _Rest, Body) -> munge_xrds(Body);
get_xrds("text/xml" ++ _Rest, Body) -> munge_xrds(Body); % Against the spec, but LiveJournal does it.
get_xrds(Other, _Body) -> none.


try_descriptor_url(none) -> {error, no_descriptor_url};
try_descriptor_url(URL) -> retrieve_step_two(URL).


retrieve_step_two(YadisURL) ->
    case http:request(get, {YadisURL, ?HTTP_HEADERS}, ?HTTP_OPTIONS, ?REQ_OPTIONS) of
        {ok, {_Status, Headers, Body}} ->
            get_xrds(?GVD("content-type", Headers, none), Body);
        Other ->
            {error, {http_error, {step_two, YadisURL, Other}}}
    end.



get_descriptor_url(Headers, Body) when is_list(Headers) ->
    case ?GVD("x-xrds-location", Headers, none) of
        none ->
            case ?GVD("content-type", Headers, none) of
                "application/xrds+xml" ++ _Rest -> none;
                none -> none;
                _MaybeHTML -> get_descriptor_url(Body)
            end;
        URL -> URL
    end.

get_descriptor_url(Body) ->
    case openid_utils:get_tags(Body, "meta", "http-equiv", "x-xrds-location") of
        [] -> none;
        [Tag|_] -> ?GVD("content", Tag, none)
    end.


%% ------------------------------------------------------------
%% XRDS
%% ------------------------------------------------------------

munge_xrds(String) ->
    {Doc, _} = xmerl_scan:string(String),
    CanonicalID = get_canonical_id(Doc),
    Services = [S || {_P, S} <- lists:sort(
      fun({P1,_},{P2,_}) -> P1 < P2 end,
      [munge_service(S) || S <- xmerl_xpath:string("XRD/Service", Doc)])],
    #xrds{canonicalID=CanonicalID, services=Services}.

munge_service(Service) ->
    Priority = get_priority(Service#xmlElement.attributes),
    Types = [get_text(T) || T <- xmerl_xpath:string("Type", Service)],
    LocalID = get_local_id(Service),
    URIs = [U || {_P, U} <- lists:sort(
                              fun({P1,_},{P2,_}) -> P1 < P2 end,
                              [{get_priority(U#xmlElement.attributes), get_text(U)}
                               || U <- xmerl_xpath:string("URI", Service)])],
    {Priority, #xrdService{types=Types, uris=URIs, localID=LocalID}}.

get_text(#xmlElement{content=[]}) -> "";
get_text(#xmlElement{content=[Value|_]}) -> Value#xmlText.value.

get_priority([#xmlAttribute{name=priority, value=Value}|_]) -> list_to_integer(Value);
get_priority([_|Rest]) -> get_priority(Rest);
get_priority([]) -> none.

get_canonical_id(Doc) ->
    case xmerl_xpath:string("XRD/CanonicalID", Doc) of
        [] -> none;
        [#xmlElement{content=[Value|_]}|_] -> Value#xmlText.value
    end.
        

get_local_id(Service) ->     
    get_local_id(Service, ["LocalID", "Delegate"]).

get_local_id(_, []) ->
    none;
get_local_id(Service, [Tag|Rest]) ->
    case xmerl_xpath:string(Tag, Service) of
        [] -> get_local_id(Service, Rest);
        [#xmlElement{content=[Value|_]}|_] -> Value#xmlText.value
    end.
            

%% ------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------

-define(P(S), io:format("~p~n", [S])).

test() ->

    ?P({"Google:", retrieve("https://www.google.com/accounts/o8/id")}),         % Direct XRDS response
    %?P({"AOL:", retrieve("https://api.screenname.aol.com/auth/openidServer")}), % x-xrds-location header
    ?P({"Flickr:", retrieve("http://flickr.com/exbrend")}),
    ?P({"LiveJournal:", retrieve("http://exbrend.livejournal.com")}),           % x-xrds-location meta tag
    ?P({"XRI Brend:", retrieve("=brendonh")}),                                  % Direct XRDS via xri.net

    application:stop(inets). % Avoid error spam from held-open connections


%% $ make test
%% erlc -o ebin -Wall -v +debug_info src/yadis.erl
%% src/yadis.erl:146: Warning: variable 'Rest' is unused
%% erl +W w -pa ebin -noshell -s yadis test -s init stop
%% {"Google:",
%%  [{["http://specs.openid.net/auth/2.0/server","http://openid.net/srv/ax/1.0",
%%     "http://specs.openid.net/extensions/ui/1.0/mode/popup",
%%     "http://specs.openid.net/extensions/ui/1.0/icon",
%%     "http://specs.openid.net/extensions/pape/1.0"],
%%    ["https://www.google.com/accounts/o8/ud"]}]}
%% {"AOL:",
%%  [{["http://specs.openid.net/auth/2.0/return_to"],
%%    ["https://api.screenname.aol.com/auth/oidRet"]}]}
%% {"LiveJournal:",
%%  [{["http://openid.net/signon/1.0"],
%%    ["http://www.livejournal.com/openid/server.bml"]}]}
%% {"XRI Drummond:",
%%  [{[],["skype:drummondreed?chat"]},
%%   {[],["skype:drummondreed?call"]},
%%   {["xri://+i-service*(+forwarding)*($v*1.0)",[]],["http://1id.com/"]},
%%   {["http://openid.net/signon/1.0"],
%%    ["http://1id.com/sso/","https://1id.com/sso/"]},
%%   {["xri://+i-service*(+contact)*($v*1.0)",[]],["http://1id.com/contact/"]}]}

