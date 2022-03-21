%%
%% Peer Crawler Worker
%% 
-module(peer_crawler_worker).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%%
%% API export functions.
%% 
-export([
    start_link/1
]).


%%
%% gen_server export functions.
%% 
-export([
    init/1,
    handle_info/2
]).


%%
%% API functions
%% 

start_link(Args) ->
    Id = proplists:get_value(id, Args),
    gen_server:start_link({local, list_to_atom(atom_to_list(?SERVER) ++ integer_to_list(Id))}, ?SERVER, Args, []).



%%
%% gen_server callback functions
%% 

init(Args) ->
    timer:sleep(110000),
    SwarmTID = proplists:get_value(swarm_table, Args),
    MarkTID = proplists:get_value(mark_table, Args),
    {_, IpFile} = proplists:get_value(ip_file, Args),

    PeerBook = libp2p_swarm:peerbook(SwarmTID),
    {ok, Peer} = libp2p_peerbook:get(PeerBook, libp2p_swarm:pubkey_bin(SwarmTID)),
    Bin = libp2p_peer:pubkey_bin(Peer),
    P2PAdress = libp2p_crypto:pubkey_bin_to_p2p(Bin),

    Id = proplists:get_value(id, Args),
    {ok, Que} = esq:new("data" ++ integer_to_list(Id) ++ "/queue", [{capacity, 10}]),

    ok = esq:enq(P2PAdress, Que),

    self() ! {start_crawler, SwarmTID, MarkTID, IpFile, Que},
    
    {ok, state}.

handle_info({start_crawler, SwarmTID, MarkTID, IpFile, Que}, State) ->
    timer:sleep(15000),
    %peer_crawler_logic:dfs_on_peers(SwarmTID, MarkTID, IpFile, P2PAdress, 3).
    peer_crawler_logic:while(SwarmTID, MarkTID, IpFile, Que).


%%
%% Internal functions
%% 