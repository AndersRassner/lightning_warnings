-module(lightning_warnings).

-export([check_for_strikes/0]).

check_for_strikes() ->
    {ok, FileContent} = file:read_file("truncated_example_data.json"),
    FileContentDecoded = jsx:decode(FileContent, [return_maps]),
    LightningStrikes = maps:get(<<"values">>, FileContentDecoded),
    {ok, FilteredStrikes} = lightning_strike_filter:strike_filter(LightningStrikes),
    io:format("Out of ~p lightning strikes, ~p were within the requested area\n",
        [length(LightningStrikes), length(FilteredStrikes)]).