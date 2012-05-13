rebar clean compile
cd rels
rebar -f generate
cd node 
sh bin/nodem console