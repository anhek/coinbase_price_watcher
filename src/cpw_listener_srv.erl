-module(cpw_listener_srv).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").
-include_lib("cdt_erlang_client/include/cdt_erlang_client_types.hrl").

-record(state, {
    tabs = [cdt_erlang_client:tab()]
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args :: term()) -> {ok, State :: #state{}}.
init([]) ->
    Tabs = ensure_tabs(),
    lists:foreach(fun({_, Pid} = Tab) ->
        _ = erlang:monitor(process, Pid),
        ok = cdt_erlang_client:listen_ws(Tab)
    end, Tabs),
    {ok, #state{tabs = Tabs}}.

%%--------------------------------------------------------------------

-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
    {noreply, NewState :: #state{}}.
handle_call(Request, _From, State = #state{}) ->
    ?LOG_WARNING("Unhandled ~p", [Request]),
    {noreply, State}.

%%--------------------------------------------------------------------

-spec handle_cast(Request :: term(), State :: #state{}) -> {noreply, NewState :: #state{}}.
handle_cast(Request, State = #state{}) ->
    ?LOG_WARNING("Unhandled ~p", [Request]),
    {noreply, State}.

%%--------------------------------------------------------------------

-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}}.
handle_info({?CDTEC_WS_FRAME_RECEIVED, Payload}, State = #state{}) ->
    PayloadDecoded = jsx:decode(Payload),
    case PayloadDecoded of
        #{
            <<"type">> := <<"ticker">>,
            <<"product_id">> := ProductId,
            <<"price">> := Price,
            <<"time">> := Time
        } ->
            case cpw_storage_srv:add_price(ProductId, Time, Price) of
                ok ->
                    ok = cpw_subscription_srv:notify_price_changed(ProductId, Time, Price),
                    ok;
                {error, product_not_found} ->
                    ok = cpw_storage_srv:new_product(ProductId),
                    ok = cpw_storage_srv:add_price(ProductId, Time, Price),
                    ok = cpw_subscription_srv:notify_price_changed(ProductId, Time, Price)
            end;
        _ -> ok
    end,
    {noreply, State};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State = #state{tabs = Tabs}) ->
    case lists:keymember(Pid, 2, Tabs) of
        true ->
            Tabs2 = lists:keydelete(Pid, 2, Tabs),
            {_, NewPid} = NewTab = cdt_erlang_client:new_tab("https://pro.coinbase.com/"),
            _ = erlang:monitor(process, NewPid),
            ok = cdt_erlang_client:listen_ws(NewTab),
            {noreply, State#state{tabs = [NewTab | Tabs2]}};
        false ->
            {noreply, State}
    end;
handle_info(Info, State = #state{}) ->
    ?LOG_WARNING("Unhandled ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------

-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #state{}) -> term().
terminate(_Reason, _State = #state{}) ->
    ok.

%%--------------------------------------------------------------------

-spec code_change(OldVsn :: term() | {down, term()}, State :: #state{}, Extra :: term()) -> {ok, NewState :: #state{}}.
code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec ensure_tabs() -> [cdt_erlang_client:tab()].
ensure_tabs() ->
    case cdt_erlang_client:get_tab("https://pro.coinbase.com/") of
        [] ->
            [cdt_erlang_client:new_tab("https://pro.coinbase.com/")];
        Tabs -> Tabs
    end.
