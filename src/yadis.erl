%%%-------------------------------------------------------------------
%%% File    : yadis.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Yadis resource discovery
%%%
%%% Created : 17 Sep 2009 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(yadis).

-export([retrieve/1, test/0]).

-include_lib("xmerl/include/xmerl.hrl").

-define(GVD(E, P, D), proplists:get_value(E, P, D)).

%% ------------------------------------------------------------
%% API
%% ------------------------------------------------------------

retrieve(YadisURL) ->
    application:start(inets),
    application:start(ssl),

    case http:request(get, {YadisURL, [{"accept", "application/xrds+xml"}]}, [], []) of
        {ok, {_Status, Headers, Body}} ->
            DescriptorURL = get_descriptor_url(Headers, Body),
            handle_response(DescriptorURL, Headers, Body);
        Other ->
            {error, {http_error, {YadisURL, Other}}}
    end.


%% ------------------------------------------------------------
%% Retrieval details
%% ------------------------------------------------------------


handle_response(none, Headers, Body) ->
    get_xrds(?GVD("content-type", Headers, none), Body);
handle_response(URL, _Headers, _Body) ->
    try_descriptor_url(URL).


get_xrds("application/xrds" ++ _Rest, Body) -> munge_xrds(Body);
get_xrds("text/xml" ++ _Rest, Body) -> munge_xrds(Body); % Against the spec, but LiveJournal does it.
get_xrds(Other, _Body) -> {error, {not_xrds, Other}}.


try_descriptor_url(none) -> {error, no_descriptor_url};
try_descriptor_url(URL) -> retrieve_step_two(URL).


retrieve_step_two(YadisURL) ->
    case http:request(get, {YadisURL, [{"accept", "application/xrds+xml"},
                                       {"connection", "Close"}]},
                      [], []) of
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


get_descriptor_url("<meta" ++ Rest) -> get_meta(Rest);
get_descriptor_url("</head>" ++ _Rest) -> none;
get_descriptor_url("") -> none;
get_descriptor_url([_|Rest]) ->
    get_descriptor_url(Rest).


get_meta(Rest) ->
    Content = get_meta_content(Rest, []),
    case re:run(string:to_lower(Content),
                "([a-z0-9-]+)\s*=\s*[\"'](.*?)[\"']",
                [{capture, all_but_first, list}, global]) of
        {match, Bits} -> check_meta([{K,V} || [K,V] <- Bits], Rest);
        _ -> get_descriptor_url(Rest)
    end.

check_meta(PropList, Rest) ->
    case ?GVD("http-equiv", PropList, none) of
        "x-xrds-location" -> ?GVD("content", PropList, none);
        _ -> get_descriptor_url(Rest)
    end.


get_meta_content(">" ++ _Rest, Content) -> lists:reverse(Content);
get_meta_content([Char|Rest], Bits) -> get_meta_content(Rest, [Char|Bits]).


%% ------------------------------------------------------------
%% XRDS
%% ------------------------------------------------------------

munge_xrds(String) ->
    {Doc, _} = xmerl_scan:string(String),
    [{Ts, Us} || {_P, Ts, Us} <- lists:sort(
      fun({P1,_,_},{P2,_,_}) -> P1 < P2 end,
      [munge_service(S) || S <- xmerl_xpath:string("XRD/Service", Doc)])].

munge_service(Service) ->
    Priority = get_priority(Service#xmlElement.attributes),
    Types = [get_text(T) || T <- xmerl_xpath:string("Type", Service)],
    URIs = [U || {_P, U} <- lists:sort(
                              fun({P1,_},{P2,_}) -> P1 < P2 end,
                              [{get_priority(U#xmlElement.attributes), get_text(U)}
                               || U <- xmerl_xpath:string("URI", Service)])],
    {Priority, Types, URIs}.

get_text(Element) ->
    [Value] = Element#xmlElement.content,
    Value#xmlText.value.

get_priority([#xmlAttribute{name=priority, value=Value}|_]) -> list_to_integer(Value);
get_priority([_|Rest]) -> get_priority(Rest);
get_priority([]) -> none.


%% ------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------

-define(P(S), io:format("~p~n", [S])).

test() ->
    ?P({"Google:", retrieve("https://www.google.com/accounts/o8/id")}),         % Direct XRDS response
    ?P({"AOL:", retrieve("https://api.screenname.aol.com/auth/openidServer")}), % x-xrds-location header
    ?P({"LiveJournal:", retrieve("http://exbrend.livejournal.com")}),           % x-xrds-location meta tag

    application:stop(inets). % Avoid error spam from held-open connections


%% $ make test
%% erlc -o ebin -Wall -v +debug_info src/yadis.erl
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
