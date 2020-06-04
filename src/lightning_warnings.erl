-module(lightning_warnings).

-export([check_for_strikes/0]).

check_for_strikes() ->
    %%{ok, _Greet} = lightning_strike_filter:strike_filter([{"stuff", "things"}]),
    {ok, FileContent} = file:read_file("truncated_example_data.json"),
    %%io:format("~p\n", [FileContent]),
    %%io:format("Before is_json\n"),
    %%true = jsx:is_json(<<FileContent>>),
    io:format("Before decode_json\n"),
    %%FileContentDecoded = jsx:decode(<<"{\"library\": \"jsx\", \"awesome\": true}">>),
    FileContentDecoded = jsx:decode(FileContent, [return_maps]),
    LightningStrikes = maps:get(<<"values">>, FileContentDecoded),
    {ok, FilteredStrikes} = lightning_strike_filter:strike_filter(LightningStrikes),
    io:format("After decode_json\n"),
    %%io:format("~p\n~p\n", [LightningStrikes, FilteredStrikes]),
    io:format("Out of ~p lightning strikes, ~p were within the requested area\n",
        [length(LightningStrikes), length(FilteredStrikes)]).
%% lightning_warnings:check_for_strikes().