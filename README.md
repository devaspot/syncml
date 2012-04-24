Simple sync - The future of sync-server.


generate node with id 'node':
    cd rel
    ../rebar generate-node nodeid=node
ignore errors about some files already exist

(or generate the node with any id you want and correct the 
reltool.config, vm.arg and sys.config)

    cd ..
    rebar compile
    rebar generate
    sh /rel/node/bin/node console

go to http://localhost:8881 and check the weclome page
try mod_esi module on /erl/simple_sync:service and /erl/simple_sync:test


