Coinbase Price Watcher
=====

Build
-----

    $ rebar3 compile

Run
-----
    $ chromium-browser --remote-debugging-port=9222
    $ ERL_FLAGS="-args_file env/local.vmargs" rebar3 shell
    
Usage
-----
    > coinbase_price_watcher:get_products().
    
    [<<"AAVE-EUR">>,<<"AAVE-USD">>,<<"ALGO-USD">>,
     <<"ATOM-USD">>,<<"BAND-GBP">>,<<"BAT-ETH">>,<<"BCH-EUR">>,
     <<"BCH-USD">>,<<"BNT-USD">>,<<"BTC-USD">>,<<"CGLD-USD">>,
     <<"CVC-USDC">>,<<"DAI-USD">>,<<"DNT-USDC">>,<<"ETH-BTC">>,
     <<"ETH-EUR">>,<<"ETH-USD">>,<<"ETH-USDC">>,<<"FIL-USD">>,
     <<"GRT-USD">>,<<"LINK-BTC">>,<<"LINK-ETH">>,<<"LINK-EUR">>,
     <<"LINK-GBP">>,<<"LINK-USD">>,<<"LOOM-USDC">>,<<"LTC-USD">>,
     <<"MANA"...>>,<<...>>|...]

        
    > coinbase_price_watcher:get_prices(<<"BTC-USD">>).
      
    {ok,[#{<<"price">> => <<"35810.84">>,
            <<"time">> => <<"2021-01-17T19:40:53.580857Z">>},
        #{<<"price">> => <<"35809.66">>,
            <<"time">> => <<"2021-01-17T19:40:55.396850Z">>},
        ...
        ]}

    > coinbase_price_watcher:subscribe(<<"BTC-USD">>).

    {ok,[#{<<"price">> => <<"35810.84">>,
            <<"time">> => <<"2021-01-17T19:40:53.580857Z">>},
        #{<<"price">> => <<"35809.66">>,
            <<"time">> => <<"2021-01-17T19:40:55.396850Z">>},
        ...
        ]}
        
    > flush().
    Shell got {ws_received,#{<<"price">> => <<"35809.76">>,
                             <<"type">> => <<"price_changed">>,
                             <<"product_id">> => <<"BTC-USD">>,
                             <<"time">> => <<"2021-01-17T19:42:50.457866Z">>}}
    Shell got {ws_received,#{<<"price">> => <<"35809.76">>,
                             <<"type">> => <<"price_changed">>,
                             <<"product_id">> => <<"BTC-USD">>,
                             <<"time">> => <<"2021-01-17T19:42:50.457866Z">>}}

    > coinbase_price_watcher:unsubscribe(<<"BTC-USD">>).
    ok
