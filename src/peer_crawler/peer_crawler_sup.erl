%%
%% Peer Crawler Supervisor
%% 
-module(peer_crawler_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(WORKER(I, Args, Number), #{
    id => {I, Number},
    start => {I, start_link, Args},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [I]
}).

%%
%% API functions
%% 
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%%
%% Supervisor callbacks
%% 
init(Args) ->
    CrawlerNum = application:get_env(blockchain_crawler, number_of_crawlers, 1),
    MarkTID = ets:new(mark_table, [public, named_table]),

    os:cmd("rm -rf data*/ip.txt"),

    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = create_child_specs(CrawlerNum, MarkTID),
    {ok, {SupFlags, ChildSpecs}}.

create_child_specs(Number, MarkTID) ->
    [
        ?WORKER(peer_crawler_worker, [[
            {id, X},
            {mark_table, MarkTID},
            {ip_file, file:open("data" ++ integer_to_list(X) ++ "/ip.txt", [append])},
            {swarm_table, list_to_atom( atom_to_list(blockchain_swarm) ++ integer_to_list(X) )}
        ]], X)

        ||
            X <- lists:seq(1, Number)
    ].