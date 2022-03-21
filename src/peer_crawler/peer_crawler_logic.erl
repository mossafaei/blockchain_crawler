%%
%% Peer Crawler Logic 
%%

-module(peer_crawler_logic).

-export([
    dfs_on_peers/5,
    refresh_peerbook/2,
    get_connected_peers/2,
    find_peer/2,
    check_dfs_condition/4,
    for/5,
    while/5
]).

- spec refresh_peerbook(ets:tab(), string()) -> ok.
refresh_peerbook(SwarmTID, P2PAddress) ->
    PeerBook = libp2p_swarm:peerbook(SwarmTID),
    libp2p_peerbook:refresh(PeerBook, libp2p_crypto:p2p_to_pubkey_bin(P2PAddress)),
    ok.

get_connected_peers(SwarmTID, P2PAddress) ->
    case find_peer(SwarmTID, P2PAddress) of
        {ok, Peer} -> 
                Connections = [libp2p_crypto:pubkey_bin_to_p2p(P)
                                    || P <- libp2p_peer:connected_peers(Peer)],
                Connections;
        _ -> ok
    end.

find_peer(SwarmTID, P2PAddress) -> 
    PeerBook = libp2p_swarm:peerbook(SwarmTID),
    libp2p_peerbook:get(PeerBook, libp2p_crypto:p2p_to_pubkey_bin(P2PAddress)).


%peer_connect(SwarmTID, P2PAddress) ->
%    libp2p_swarm:connect(SwarmTID, P2PAddress),
%    ok.

check_dfs_condition(SwarmTID, MarkTID, IpFile, P2PAddress) ->
    case ets:lookup(MarkTID, P2PAddress) of
        %% If the address is not available in Mark table
        [] -> dfs_on_peers(SwarmTID, MarkTID, IpFile, P2PAddress, 3);
        
        %% If the address is available in Mark table
         _ -> ok
    end,
    ok.

for([], _, _, _, _) -> ok;
for(LST, SwarmTID, MarkTID, IpFile, P2PAddress) ->
    [Item| Other] = LST,
    check_dfs_condition(SwarmTID, MarkTID, IpFile, Item),
    for(Other, SwarmTID, MarkTID, IpFile, P2PAddress).

- spec dfs_on_peers(ets:tab(), ets:tab(), _, string(), integer()) -> ok.
dfs_on_peers(_, _, _, _, 0) -> ok;
dfs_on_peers(SwarmTID, MarkTID, IpFile, P2PAddress, MaxTry) ->
    %io:format("Start DFS on the node: " ++ P2PAddress ++ "\n"),
    lager:info("~p Start DFS on the node: ~p", [SwarmTID ,P2PAddress]),
    timer:sleep(3000),
    refresh_peerbook(SwarmTID, P2PAddress),
    timer:sleep(3000),
    case find_peer(SwarmTID, P2PAddress) of
        {ok, Peer} -> 
                    [BestAddress| _] = libp2p_transport:sort_addrs(SwarmTID, libp2p_peer:listen_addrs(Peer)),
                    %ets:insert(IpFile, {P2PAddress, BestAddress}),
                    io:format(IpFile, "~s~n", [P2PAddress ++ ": " ++ BestAddress]),
                    ets:insert(MarkTID, {P2PAddress, true}),
                    PeerList = get_connected_peers(SwarmTID, P2PAddress),
                    for(PeerList, SwarmTID, MarkTID, IpFile, P2PAddress);
        _ -> dfs_on_peers(SwarmTID, MarkTID, IpFile, P2PAddress, MaxTry - 1)
    end,
    ok.


bfs_apply_lookup(SearchPeer, MarkTID, Que) ->
    case ets:lookup(MarkTID, SearchPeer) of
        %% If the address is not available in Mark table
        [] -> esq:enq(SearchPeer, Que);
        
        %% If the address is available in Mark table
         _ -> ok
    end.

try_in_bfs(1, _, _, _, _, _) -> ok;
try_in_bfs(MaxTry, P2PAddress, MarkTID, SwarmTID, IpFile, Que) ->
    
    case find_peer(SwarmTID, P2PAddress) of
        {ok, Peer} -> 
                    [BestAddress| _] = libp2p_transport:sort_addrs(SwarmTID, libp2p_peer:listen_addrs(Peer)),
                    %ets:insert(IpFile, {P2PAddress, BestAddress}),
                    io:format(IpFile, "~s~n", [P2PAddress ++ ": " ++ BestAddress]),
                    ets:insert(MarkTID, {P2PAddress, true}),
                    PeerList = get_connected_peers(SwarmTID, P2PAddress),
                    lists:foreach(
                        fun(SearchPeer) ->
                            bfs_apply_lookup(SearchPeer, MarkTID, Que)
                        end
                    , PeerList);
        _ ->
            case ets:lookup(MarkTID, P2PAddress) of
                %% If the address is not available in Mark table
                [] ->  
                    lager:info("~p Start BFS on the node: ~p", [SwarmTID ,P2PAddress]),
                    timer:sleep(3000),
                    refresh_peerbook(SwarmTID, P2PAddress),
                    timer:sleep(3000),
                    try_in_bfs(MaxTry - 1, P2PAddress, MarkTID, SwarmTID, IpFile, Que);
                
                %% If the address is available in Mark table
                 _ -> ok
            end
    end.

while(SwarmTID, MarkTID, IpFile, Que, MaxTry) ->
    case esq:deq(Que) of
        [#{payload := P2PAddress}] ->
            case ets:lookup(MarkTID, P2PAddress) of
                %% If the address is not available in Mark table
                [] ->  
                    lager:info("~p Start BFS on the node: ~p", [SwarmTID ,P2PAddress]),
                    timer:sleep(3000),
                    refresh_peerbook(SwarmTID, P2PAddress),
                    timer:sleep(3000),
        
                    try_in_bfs(MaxTry, P2PAddress, MarkTID, SwarmTID, IpFile, Que);
                
                %% If the address is available in Mark table
                 _ -> ok
            end;
        _ -> ok
    end,

    while(SwarmTID, MarkTID, IpFile, Que, MaxTry).
