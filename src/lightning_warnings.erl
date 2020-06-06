-module(lightning_warnings).

-export([check_for_strikes/0]).

check_for_strikes() ->
    Values = get_lightning_strikes:get_strikes(smhi),
    FileContentDecoded = jsx:decode(Values, [return_maps]),
    LightningStrikes = maps:get(<<"values">>, FileContentDecoded),
    {ok, FilteredStrikes} = lightning_strike_filter:strike_filter(LightningStrikes),
    io:format("Out of ~p lightning strikes, ~p were within the requested area\n",
        [length(LightningStrikes), length(FilteredStrikes)]).
