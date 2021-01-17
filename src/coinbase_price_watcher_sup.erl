-module(coinbase_price_watcher_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

-spec init(term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 30
    },
    ChildSpecs = [
        #{
            id => cpw_storage_srv,
            start => {cpw_storage_srv, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [cpw_storage_srv]
        },
        #{
            id => cpw_subscription_srv,
            start => {cpw_subscription_srv, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [cpw_subscription_srv]
        },
        #{
            id => cpw_listener_srv,
            start => {cpw_listener_srv, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [cpw_listener_srv]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
