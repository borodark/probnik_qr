-module(probnik_qr).

%% API
-export([show/0, payload/0, show_net/0, payload_net/0, show_pair/0, payload_pair/0]).

-define(PAIR_TAG, probnik_pair).

%% Local copy of the record used by komone/qrcode.
-record(qrcode, {version, ecc, dimension, data}).

%% Print an ANSI ASCII QR for pairing with Probnik.
show() ->
    show_net().

show_net() ->
    case qr_encode(payload_net()) of
        {ok, QRCode} ->
            io:put_chars(render_ansi(QRCode)),
            {HostLabel, Port} = resolve_host_port(),
            io:format("Host: ~s~n", [HostLabel]),
            io:format("Port: ~p~n", [Port]),
            ok;
        {error, Reason} ->
            io:format("Failed to render QR (~p).~n", [Reason]),
            {error, Reason}
    end.

%% Returns the pairing payload as a binary.
payload() ->
    payload_net().

payload_net() ->
    {Host, Port} = resolve_host_port(),
    iolist_to_binary(format_probnikoff_net(Host, Port)).

show_pair() ->
    Node = node(),
    case Node of
        'nonode@nohost' ->
            io:format("Error: Node not started with distribution enabled.~n"),
            io:format("Start with: iex --sname myapp@localhost --cookie secret -S mix~n"),
            {error, no_distribution};
        _ ->
            case qr_encode(payload_pair()) of
                {ok, QRCode} ->
                    io:put_chars(render_ansi(QRCode)),
                    io:format("Node: ~p~n", [Node]),
                    io:format("Mode: ~p~n", [detect_name_mode(Node)]),
                    ok;
                {error, Reason} ->
                    io:format("Failed to render QR (~p).~n", [Reason]),
                    {error, Reason}
            end
    end.

payload_pair() ->
    Node = node(),
    Cookie = erlang:get_cookie(),
    Mode = detect_name_mode(Node),
    Term = {?PAIR_TAG, Node, Cookie, [{mode, Mode}]},
    iolist_to_binary(io_lib:format("~p", [Term])).

%% Internal
qr_encode(PayloadBin) ->
    case code:which(qrcode) of
        non_existing ->
            {error, no_qrcode_module};
        _ ->
            try
                {ok, qrcode:encode(PayloadBin)}
            catch
                Class:Reason ->
                    {error, {Class, Reason}}
            end
    end.

render_ansi(#qrcode{dimension = Dim, data = Data}) ->
    render_rows(Data, Dim, []).

render_rows(<<>>, _Dim, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
render_rows(Bits, Dim, Acc) ->
    <<Row:Dim/bits, Rest/bits>> = Bits,
    RowStr = render_row(Row, []),
    render_rows(Rest, Dim, [RowStr, "\n" | Acc]).

render_row(<<>>, Acc) ->
    lists:reverse(Acc);
render_row(<<1:1, Rest/bits>>, Acc) ->
    render_row(Rest, ["\e[40m  \e[0m" | Acc]);
render_row(<<0:1, Rest/bits>>, Acc) ->
    render_row(Rest, ["\e[0;107m  \e[0m" | Acc]).

resolve_host_port() ->
    Port = resolve_port(),
    case os:getenv("PROBNIKOFF_NET_HOST") of
        false ->
            Ip = get_local_ip(),
            {format_ip(Ip), Port};
        HostStr ->
            {HostStr, Port}
    end.

resolve_port() ->
    case os:getenv("PROBNIKOFF_NET_PORT") of
        false -> 4040;
        Str ->
            case string:to_integer(Str) of
                {Int, _} when Int > 0 -> Int;
                _ -> 4040
            end
    end.

format_probnikoff_net(HostStr, Port) when is_list(HostStr) ->
    case parse_ip_string(HostStr) of
        {ok, {A, B, C, D}} ->
            io_lib:format("{probnikoff_net, {{~p, ~p, ~p, ~p}, ~p}}", [A, B, C, D, Port]);
        error ->
            io_lib:format("{probnikoff_net, {'~s', ~p}}", [HostStr, Port])
    end.

get_local_ip() ->
    case inet:getifaddrs() of
        {ok, IfAddrs} ->
            case pick_ipv4(IfAddrs) of
                {ok, Ip} -> Ip;
                error -> {127, 0, 0, 1}
            end;
        _ ->
            {127, 0, 0, 1}
    end.

pick_ipv4(IfAddrs) ->
    Addrs = lists:foldl(fun({_Name, Opts}, Acc) ->
        lists:foldl(fun
            ({addr, {A, _, _, _} = Addr}, Acc2) when A =/= 127 ->
                [Addr | Acc2];
            (_, Acc2) ->
                Acc2
        end, Acc, Opts)
    end, [], IfAddrs),
    case Addrs of
        [Ip | _] -> {ok, Ip};
        [] -> error
    end.

format_ip({A, B, C, D}) ->
    lists:flatten(io_lib:format("~p.~p.~p.~p", [A, B, C, D]));
format_ip(_) ->
    "127.0.0.1".

parse_ip_string(Str) when is_list(Str) ->
    case inet:parse_address(Str) of
        {ok, Addr} -> {ok, Addr};
        _ -> error
    end.

detect_name_mode(Node) when is_atom(Node) ->
    detect_name_mode(atom_to_list(Node));
detect_name_mode(Node) when is_list(Node) ->
    case string:split(Node, "@", all) of
        [_, Host] ->
            case string:find(Host, ".") of
                nomatch -> shortnames;
                _ -> longnames
            end;
        _ ->
            shortnames
    end.
