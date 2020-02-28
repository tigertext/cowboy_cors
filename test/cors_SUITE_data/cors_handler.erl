-module(cors_handler).

-export([init/2]).

init(Req, _) ->
    ct:pal("req in handler is ~p ", [Req]),
    Cors = maps:get(cowboy_cors, Req, undefined),
    ct:pal("cors in handler is  ~p", [Cors]),
    case Cors of
        {stop, Code} ->
            cowboy_req:reply(Code, Req),
            {ok, Req, undefined_state};
        _ ->
            Headers = #{<<"X-Exposed">> => <<"exposed">>, <<"X-Hidden">> => <<"hidden">>},
            ct:pal("cors_handler, req is ~p ", [Req]),
            Req1 = cowboy_req:reply(204, Headers, [], Req),
            {ok, Req1, undefined_state}
    end.

