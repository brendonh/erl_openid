-module(openid_test).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-include("openid.hrl").

setup() ->
    [ application:start(X) || X <- [ crypto, ssl, sasl, inets, ibrowse ] ].

discover2_test_() ->
    Cases = [{"http://steamcommunity.com/openid",
	      #openid_authreq{opURLs = ["https://steamcommunity.com/openid/login"],
			      version = {2,0},
			      claimedID = "http://specs.openid.net/auth/2.0/identifier_select",
			      localID = "http://specs.openid.net/auth/2.0/identifier_select",
			      assoc = none}}
	    ],
    {setup, fun setup/0, [?_assertEqual(Result, openid:discover(URL))
			  || {URL, Result} <- Cases ]}.

discover1_test_() ->
    Cases = [],
    {setup, fun setup/0, [?_assertEqual(Result, openid:discover(URL))
			  || {URL, Result} <- Cases ]}.

