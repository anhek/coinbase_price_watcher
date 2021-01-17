-module(cpw_subscription_srv).

-behaviour(gen_server).

%% API
-export([
    notify_price_changed/3,
    subscribe/1,
    unsubscribe/1
]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    products_subs_pids = #{} :: map(),
    monitors_subs_products = #{} :: map()
}).

-include("coinbase_price_watcher.hrl").
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec notify_price_changed(ProductId :: binary(), Time :: binary(), Price :: binary()) -> ok.
notify_price_changed(ProductId, Time, Price) ->
    gen_server:cast(?SERVER, {notify_price_changed, ProductId, Time, Price}).

%%--------------------------------------------------------------------

-spec subscribe(ProductId :: binary()) -> {ok, any()}.
subscribe(ProductId) ->
    gen_server:call(?SERVER, {subscribe, self(), ProductId}).

%%--------------------------------------------------------------------

-spec unsubscribe(ProductId :: binary()) -> ok.
unsubscribe(ProductId) ->
    gen_server:call(?SERVER, {unsubscribe, self(), ProductId}).

%%--------------------------------------------------------------------

-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args :: term()) -> {ok, State :: #state{}}.
init([]) ->
    erlang:process_flag(trap_exit, true),
    {ok, #state{}}.

%%--------------------------------------------------------------------

-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {noreply, NewState :: #state{}}.
handle_call({subscribe, SubscriberPid, ProductId}, _From, State = #state{
    products_subs_pids = ProductsSubsPids,
    monitors_subs_products = MonitorsSubsProducts
}) ->
    StoredSubs = maps:get(ProductId, ProductsSubsPids, []),
    StoredProducts = maps:get(ProductId, MonitorsSubsProducts, []),
    case cpw_storage_srv:get_prices(ProductId) of
        {ok, _Prices} = Response->
            ?LOG_INFO("New subscription: ~p for ~p", [SubscriberPid, ProductId]),
            _ = erlang:monitor(process, SubscriberPid),
            {reply, Response, State#state{
                products_subs_pids = ProductsSubsPids#{
                    ProductId => [SubscriberPid | StoredSubs]
                },
                monitors_subs_products = MonitorsSubsProducts#{
                    SubscriberPid => [ProductId | StoredProducts]
                }
            }};
        {error, product_not_found} = Response ->
            {reply, Response, State}
    end;
handle_call({unsubscribe, SubscriberPid, ProductId}, _From, State = #state{
    products_subs_pids = ProductsSubsPids,
    monitors_subs_products = MonitorsSubsProducts
}) ->
    ?LOG_INFO("Unsubscription: ~p for ~p", [SubscriberPid, ProductId]),
    StoredSubs = maps:get(ProductId, ProductsSubsPids, []),
    ProductsSubsPids2 = case lists:delete(SubscriberPid, StoredSubs) of
        [] -> maps:remove(ProductId, ProductsSubsPids);
        StoredSubs2 -> ProductsSubsPids#{ProductId => StoredSubs2}
    end,
    StoredProducts = maps:get(SubscriberPid, MonitorsSubsProducts, []),
    MonitorsSubsProducts2 = case lists:delete(ProductId, StoredProducts) of
        [] -> maps:remove(SubscriberPid, MonitorsSubsProducts);
        StoredProducts2 -> MonitorsSubsProducts#{SubscriberPid => StoredProducts2}
    end,
    {reply, ok, State#state{
        products_subs_pids = ProductsSubsPids2,
        monitors_subs_products = MonitorsSubsProducts2
    }};
handle_call(Request, _From, State = #state{}) ->
    ?LOG_WARNING("Unhandled ~p", [Request]),
    {noreply, State}.

%%--------------------------------------------------------------------

-spec handle_cast(Request :: term(), State :: #state{}) -> {noreply, NewState :: #state{}}.
handle_cast({notify_price_changed, ProductId, Time, Price}, State = #state{
    products_subs_pids = ProductsSubsPids
}) ->
%%    ?LOG_INFO("~p notifies ~p", [ProductId, length(maps:get(ProductId, ProductsSubsPids, []))]),
    lists:foreach(fun(Pid) ->
        erlang:send(Pid, {?CPW_WS_RECEIVED, #{
            <<"type">> => <<"price_changed">>,
            <<"product_id">> => ProductId,
            <<"time">> => Time,
            <<"price">> => Price
        }})
    end, maps:get(ProductId, ProductsSubsPids, [])),
    {noreply, State};
handle_cast(Request, State = #state{}) ->
    ?LOG_WARNING("Unhandled ~p", [Request]),
    {noreply, State}.

%%--------------------------------------------------------------------

-spec handle_info(Info :: timeout() | term(), State :: #state{}) -> {noreply, NewState :: #state{}}.
handle_info({'DOWN', _SubscriberMonRef, process, SubscriberPid, _Reason}, State = #state{
    products_subs_pids = ProductsSubsPids,
    monitors_subs_products = MonitorsSubsProducts
}) ->
    ?LOG_INFO("Subscriber ~p down", [SubscriberPid]),
    SubscriberProducts = maps:get(SubscriberPid, MonitorsSubsProducts, []),
    ProductsSubsPids2 = lists:foldl(fun(ProductId, Acc) ->
        NewPids = lists:delete(SubscriberPid, maps:get(ProductId, Acc, [])),
        case NewPids of
            [] -> maps:remove(ProductId, Acc);
            _ -> Acc#{ProductId => NewPids}
        end
    end, ProductsSubsPids, SubscriberProducts),
    {noreply, State#state{
        products_subs_pids = ProductsSubsPids2,
        monitors_subs_products = maps:remove(SubscriberPid, MonitorsSubsProducts)
    }};
handle_info(Info, State = #state{}) ->
    ?LOG_WARNING("Unhandled ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------

-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #state{}) -> term().
terminate(_Reason, _State = #state{products_subs_pids = PSP}) ->
    Pids = lists:usort(lists:flatten(maps:values(PSP))),
    [erlang:send(Pid, ?CPW_SHUTDOWN) || Pid <- Pids],
    ok.

%%--------------------------------------------------------------------

-spec code_change(OldVsn :: term() | {down, term()}, State :: #state{}, Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
