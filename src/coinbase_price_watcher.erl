-module(coinbase_price_watcher).

%% API
-export([
    get_products/0,
    get_prices/1,
    subscribe/1,
    unsubscribe/1
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec get_products() -> [binary()].
get_products() ->
    cpw_storage_srv:get_products().

%%--------------------------------------------------------------------

-spec get_prices(ProductId :: binary()) -> {ok, [map()]} | {error, product_not_found}.
get_prices(ProductId) ->
    cpw_storage_srv:get_prices(ProductId).
%%--------------------------------------------------------------------

-spec subscribe(ProductId :: binary()) -> {ok, any()}.
subscribe(ProductId) ->
    cpw_subscription_srv:subscribe(ProductId).

%%--------------------------------------------------------------------

-spec unsubscribe(ProductId :: binary()) -> ok.
unsubscribe(ProductId) ->
    cpw_subscription_srv:unsubscribe(ProductId).
