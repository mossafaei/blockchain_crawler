-module(blockchain_cli_registry).

-define(CLI_MODULES, [
                      blockchain_cli_peer
                     ]).

-export([register_cli/0]).

register_cli() ->
    clique:register(?CLI_MODULES).
