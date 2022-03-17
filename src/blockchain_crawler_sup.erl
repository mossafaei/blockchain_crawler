-module(blockchain_crawler_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, random_val_predicate/1]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(SUP(I, Args, Number), #{
    id => {I, Number},
    start => {I, start_link, Args},
    restart => permanent,
    shutdown => 5000,
    type => supervisor,
    modules => [I]
}).

-define(WORKER(I, Args), ?WORKER(I, I, Args)).
-define(WORKER(I, M, Args), #{
    id => I,
    start => {M, start_link, Args},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [M]
}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    
    erlang:system_flag(fullsweep_after, 0),

    SupFlags = #{strategy => one_for_all, intensity => 0, period => 1},

    CrawlerNum = application:get_env(blockchain_crawler, number_of_crawlers, 1),
    %{ok, NodePort} = application:get_env(blockchain_node, jsonrpc_port),
    {ok,
        {SupFlags, blockchain_sup_generator(CrawlerNum) }}.

create_blockchain_sup_opts(Number) ->
    ok = libp2p_crypto:set_network(application:get_env(blockchain_crawler, network, mainnet)),

    SeedNodes =
        case application:get_env(blockchain_crawler, seed_nodes) of
            {ok, ""} -> [];
            {ok, Seeds} -> string:split(Seeds, ",", all);
            _ -> []
        end,

    %%BaseDir = application:get_env(blockchain_crawler, base_dir, "data"),
    BaseDir = "data" ++ Number,
    SwarmKey = filename:join([BaseDir, "blockchain_crawler", "swarm_key"]),
    ok = filelib:ensure_dir(SwarmKey),
    {PublicKey, ECDHFun, SigFun} =
        case libp2p_crypto:load_keys(SwarmKey) of
            {ok, #{secret := PrivKey0, public := PubKey}} ->
                {PubKey, libp2p_crypto:mk_ecdh_fun(PrivKey0), libp2p_crypto:mk_sig_fun(PrivKey0)};
            {error, enoent} ->
                KeyMap =
                    #{secret := PrivKey0, public := PubKey} = libp2p_crypto:generate_keys(
                        ecc_compact
                    ),
                ok = libp2p_crypto:save_keys(KeyMap, SwarmKey),
                {PubKey, libp2p_crypto:mk_ecdh_fun(PrivKey0), libp2p_crypto:mk_sig_fun(PrivKey0)}
        end,
    SeedNodeDNS = application:get_env(blockchain_crawler, seed_node_dns, []),
    SeedAddresses = string:tokens(
        lists:flatten([
            string:prefix(X, "blockchain-seed-nodes=")
         || [X] <- inet_res:lookup(SeedNodeDNS, in, txt),
            string:prefix(X, "blockchain-seed-nodes=") /= nomatch
        ]),
        ","
    ),

    Port = application:get_env(blockchain_crawler, port, 0),
    MaxInboundConnections = application:get_env(blockchain_crawler, max_inbound_connections, 10),
    [
        {crawler_num, Number},
        {key, {PublicKey, SigFun, ECDHFun}},
        {seed_nodes, SeedNodes ++ SeedAddresses},
        {max_inbound_connections, MaxInboundConnections},
        {port, Port},
        {update_dir, "update"},
        {base_dir, BaseDir}
    ].


blockchain_sup_generator(Number) -> 
    [
        ?SUP(blockchain_sup, [create_blockchain_sup_opts(integer_to_list(X))], X)
    || 
        X <- lists:seq(1, Number)].

random_val_predicate(Peer) ->
    not libp2p_peer:is_stale(Peer, timer:minutes(360)) andalso
        maps:get(<<"release_version">>, libp2p_peer:signed_metadata(Peer), undefined) /= undefined.