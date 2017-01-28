-module(openid_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, {{one_for_one, 10, 60}, [
        {openid_srv,
            {openid_srv, start_link, []},
            permanent, 1000, worker, [openid_srv]}
    ]}}.
