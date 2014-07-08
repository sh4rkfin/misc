%% Copyright
-author("dave").

-record(state, {name, state=nil, allServers, leader=nil,
                successor, successors, extension, timer, created, messageCount=0}).
-record(message, {type, from, fromName, args}).


