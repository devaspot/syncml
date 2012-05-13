-module(simple_sync).
-export([package/1]).

package(Package)->
    sync_agent:package(Package).