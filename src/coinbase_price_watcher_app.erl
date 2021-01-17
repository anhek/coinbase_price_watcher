-module(coinbase_price_watcher_app).

-behaviour(application).

-export([start/2, stop/1]).

%%%===================================================================

-spec start(StartType :: term(), StartArgs :: any()) -> {ok, pid()} | {ok, pid(), State :: any()} | {error, any()}.
start(_StartType, _StartArgs) ->
    coinbase_price_watcher_sup:start_link().

%%--------------------------------------------------------------------

-spec stop(State :: term()) -> any().
stop(_State) ->
    ok.
