-module(cpw_storage_srv).

-behaviour(gen_server).

%% API
-export([
    get_products/0,
    new_product/1,
    add_price/3,
    get_prices/1
]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(CPW_PRODUCTS_TABLE, cpw_products_ets).

-record(state, {}).

-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec get_products() -> [binary()].
get_products() ->
    lists:map(fun({ProductId, _}) ->
        ProductId
    end, ets:tab2list(?CPW_PRODUCTS_TABLE)).

%%--------------------------------------------------------------------

-spec new_product(ProductId :: binary()) -> ok | {error, already_exists}.
new_product(ProductId) ->
    gen_server:call(?MODULE, {new_product, ProductId}).

%%--------------------------------------------------------------------

-spec add_price(ProductId :: binary(), Time :: binary(), Price :: binary()) -> ok | {error, product_not_found}.
add_price(ProductId, Time, Price) ->
    case ets:lookup(?CPW_PRODUCTS_TABLE, ProductId) of
        [{ProductId, Table}] ->
            ets:insert(Table, {Time, Price}),
            ok;
        [] ->
            {error, product_not_found}
    end.

%%--------------------------------------------------------------------

-spec get_prices(ProductId :: binary()) -> {ok, [map()]} | {error, product_not_found}.
get_prices(ProductId) ->
    case ets:lookup(?CPW_PRODUCTS_TABLE, ProductId) of
        [{ProductId, Table}] ->
            Response = lists:map(fun({Time, Price}) ->
                #{
                    <<"time">> => Time,
                    <<"price">> => Price
                }
            end, ets:tab2list(Table)),
            {ok, Response};
        [] ->
            {error, product_not_found}
    end.

%%--------------------------------------------------------------------

-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args :: term()) -> {ok, State :: #state{}}.
init([]) ->
    ?CPW_PRODUCTS_TABLE = ets:new(?CPW_PRODUCTS_TABLE, [public, named_table, ordered_set]),
    {ok, #state{}}.

%%--------------------------------------------------------------------

-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {noreply, NewState :: #state{}}.
handle_call({new_product, ProductId}, _From, State = #state{}) ->
    Response = case ets:lookup(?CPW_PRODUCTS_TABLE, ProductId) of
        [_] -> {error, already_exists};
        [] ->
            Table = ets:new(binary_to_atom(ProductId, utf8), [public, ordered_set]),
            true = ets:insert(?CPW_PRODUCTS_TABLE, {ProductId, Table}),
            ok
    end,
    {reply, Response, State};
handle_call(Request, _From, State = #state{}) ->
    ?LOG_WARNING("Unhandled ~p", [Request]),
    {noreply, State}.

%%--------------------------------------------------------------------

-spec handle_cast(Request :: term(), State :: #state{}) -> {noreply, NewState :: #state{}}.
handle_cast(Request, State = #state{}) ->
    ?LOG_WARNING("Unhandled ~p", [Request]),
    {noreply, State}.

%%--------------------------------------------------------------------

-spec handle_info(Info :: timeout() | term(), State :: #state{}) -> {noreply, NewState :: #state{}}.
handle_info(Info, State = #state{}) ->
    ?LOG_WARNING("Unhandled ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------

-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #state{}) -> term().
terminate(_Reason, _State = #state{}) ->
    ok.

%%--------------------------------------------------------------------

-spec code_change(OldVsn :: term() | {down, term()}, State :: #state{}, Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
