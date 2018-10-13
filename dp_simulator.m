function [cycle,cyclestd,failure,cycletable] = dp_simulator(nodecount,participants,loopcount,connections,interval,delay,limit,parnum)

% ddoublespent_proof_simulator, by @im_uname 2018
%
% License: MIT
%
% Simple simulator that demonstrates the probability that the
% merchant is going to receive a double-spend proof from a double-spend
% elsewhere in the network, how fast it will be (in cycles; each cycle is
% 50ms per current BU rule). If it is never reached at the end of limit
% cycles, a timeout is reached and the cycletime is assumed
% to be 0.
%
% outputs: cycle is the average number of cycles it takes for merchant node to
% receive a proof (among successes), cyclestd is the standard deviation,
% failure is the number of loops that failed to relay within
% timeout,cycletable has the statistics of each loop.
%
% nodecount is the total number of nodes on the network. I assume all nodes
% follow the "default" configuration; variable configurations TODO
% 
% participants is the number of all nodes who participate in the
% double-spending relay, which will be seeded at random. Non-partipating
% nodes adhere to first-seen, where the second tx is simply ignored. 
%
% loopcount is the number of times the simulation runs to arrive at
% cycle. Loop more for more robust statistics.
%
% connections is the number of connections each node has outgoing. Nodes
% will randomly find other nodes and connect to them, resulting in a
% variable graph with each node having minimum # connections set by this
% number.
%
% interval is a crude approximation of how long an attacker delays between
% sending tx1 and tx2, in # cycles. If 0, both tx are sent out at the exact
% same time. 
% 
% delay is a boolean of whether the network relays using the "25%" logic
% where 25% of the unsent inventory (picked at random) is sent out every cycle. (false).
% Trickling is per connection.
%
% If delay is set to true, then nodes will only relay everything on
% average 5 seconds (100 cycles) on a poisson distribution.
%
% limit is the timeout in cycles. Set to 600 for 30 seconds.
% 
% txflow is background tx that does not matter, but put in there to aid
% trickle simulation. Recommended to use 1 each cycle, which is fairly
% heavy load. Each tx is sent to a random node on the network.
% 
%
% assume all tx becomes "inflight" on the round they're sent out, and
% arrives in inv next round. Variable latencies TODO

% first generate structures and fill it with node inventories per
% connection

% constants, can be variable later

txflow = 1;

inventory = 100; % number << 1000, simulate initial inventory state

% null outputs

cycle = 1;

cyclestd = 0;

failure = 0;

cycletable = zeros(loopcount,1);

p = parpool(parnum);
parfor h = 1:loopcount
    disp(['loop' num2str(h)])
    coinnet = struct;
    
    % pick participants at random
    parti = randperm(nodecount,participants);
    
    % merchant must participate! 
    parti = unique([parti 3]);
    
    % start a connections table for this loop
    conntable = [];
    
    % fill inventories
    
    for i = 1:nodecount

        coinnet(i).mempool = [];
        coinnet(i).inflight = [];
        % create connections
        peerpool = [1:i-1,i+1:nodecount];
        peerids = randperm(size(peerpool,2),connections);
        outgoing = peerpool(peerids);
        % add to the conntable
        conntemp = [repmat(i,1,connections);outgoing]';
        conntable = [conntable ; conntemp];
        for j = 1:connections
            coinnet(i).conn(j).id = outgoing(j);
            coinnet(i).conn(j).inv = randperm(1000,inventory);
            coinnet(i).conn(j).invsent = [];
            coinnet(i).mempool = unique([coinnet(i).mempool coinnet(i).conn(j).inv],'stable');
            % add delay flag
            if delay
                coinnet(i).conn(j).pssn = false;
            end
        end
    end
    
    % match outgoing connections to their destination, fill those inv as
    % well
    
    for i = 1:size(conntable,1)
        % check if a reverse does not already exist
        if ismember([conntable(i,2) conntable(i,1)],conntable,'rows')
            continue
        else
            coinnet(conntable(i,2)).conn(end+1).id = conntable(i,1);
            coinnet(conntable(i,2)).conn(end).inv = randperm(1000,inventory);
            coinnet(conntable(i,2)).conn(end).invsent = [];
            coinnet(conntable(i,2)).mempool =...
                unique([coinnet(conntable(i,2)).mempool coinnet(conntable(i,2)).conn(end).inv],'stable');
            conntable = [conntable;[conntable(i,2) conntable(i,1)]];
            % add delay flag
            if delay
                coinnet(i).conn(j).pssn = false;
            end
        end
    end
        
    % start the loop proper! tx1 is added to inv of node 1 at
    % beginning of each cycle. tx2 is added to inv of node 2 after
    % interval.
    
    count = 1;
    randtx = 1004;
    while true
        % add tx1 to node1 inv
        if count == 1
            for j = 1:size(coinnet(1).conn,2)
                coinnet(1).conn(j).inv = [coinnet(1).conn(j).inv,1001];
            end
        end
        
        % add tx2 to node2 inv if time is up
        if count == interval + 1
            coinnet(2).mempool = [coinnet(2).mempool,1002];
            
            [coinnet(2).mempool,gen] = ridsecond(coinnet(2).mempool);
            genflag = gen & ismember(2,parti);
            if genflag
                coinnet(2).mempool = [coinnet(2).mempool,1003];
            end
            
            for j = 1:size(coinnet(2).conn,2)
                coinnet(2).conn(j).inv = [coinnet(2).conn(j).inv,1002];
                [coinnet(2).conn(j).inv,~] = ridsecond(coinnet(2).conn(j).inv);
                if genflag
                    coinnet(2).conn(j).inv = [coinnet(2).conn(j).inv,1003];
                end
            end
            % clean the flag!
            gen = false;
        end
        
        % add a random tx, count of which determined by txflow, to a random
        % node
        for g = 1:txflow
            randomnode = randperm(nodecount,1);
            coinnet(randomnode).mempool = [coinnet(randomnode).mempool,randtx];
            for j = 1:size(coinnet(randomnode).conn,2)
                coinnet(randomnode).conn(j).inv = [coinnet(randomnode).conn(j).inv,randtx];
            end
            % increment randtx "id"
            randtx = randtx + 1;
        end
        
        % add tx in flight from previous round to mempool and inv of its destination
        for i = 1:nodecount
            coinnet(i).mempool = unique([coinnet(i).mempool coinnet(i).inflight],'stable');
            [coinnet(i).mempool,gen] = ridsecond(coinnet(i).mempool);
            genflag = gen && ismember(i,parti);
            
            % "presenceflag" indicates the presence of either tx1 or tx2
            presenceflag = ismember(1001,coinnet(i).mempool) || ismember(1002,coinnet(i).mempool);
            
            if ~ismember(i,parti) || ~presenceflag % if node is not participant or has no parent, get rid of 1003 ds proof
                coinnet(i).mempool(coinnet(i).mempool == 1003) = [];
            elseif genflag % if the tx1 and tx2 interact on a node, the node generates proof
                coinnet(i).mempool = unique([coinnet(i).mempool 1003],'stable');
            end
            
            % snuff out from inflight if counterpart present in mempool
            if ismember(1001,coinnet(i).mempool)
                coinnet(i).inflight(coinnet(i).inflight == 1002) = [];
            elseif ismember(1002,coinnet(i).mempool)
                coinnet(i).inflight(coinnet(i).inflight == 1001) = [];
            end
            
            for j = 1:size(coinnet(i).conn,2)
                
                deducted = coinnet(i).inflight(~ismember(coinnet(i).inflight,coinnet(i).conn(j).invsent));
                coinnet(i).conn(j).inv = unique([coinnet(i).conn(j).inv deducted],'stable');
                
                [coinnet(i).conn(j).inv,~] = ridsecond(coinnet(i).conn(j).inv);
                
                if ~ismember(i,parti) || ~presenceflag
                    coinnet(i).conn(j).inv(coinnet(i).conn(j).inv == 1003) = [];
                elseif genflag && ~ismember(1003,coinnet(i).conn(j).invsent)
                    coinnet(i).conn(j).inv = unique([coinnet(i).conn(j).inv 1003],'stable');
                end
            end
            coinnet(i).inflight = []; % cleanout, decrease memory use
            gen = false;
            
        end
        
        
        
        % start relaying! put inv content into destination inflights
        for i = 1:nodecount
            for j = 1:size(coinnet(i).conn,2)
                if ~delay
                    % trickle relay; mimic "25% chosen at random to relay"
                    % rule
                    if isempty(coinnet(i).conn(j).inv)
                        continue
                    end
                    flag = rand(size(coinnet(i).conn(j).inv));
                    flag = flag < 0.25;
                    if sum(flag) == 0
                        continue
                    end
                    invflight = coinnet(i).conn(j).inv(flag);
                    coinnet(i).conn(j).inv = coinnet(i).conn(j).inv(~flag);
                    coinnet(coinnet(i).conn(j).id).inflight =...
                        unique([coinnet(coinnet(i).conn(j).id).inflight,...
                        invflight],'stable');
                    coinnet(i).conn(j).invsent =...
                        unique([coinnet(i).conn(j).invsent,...
                        invflight],'stable');
                else
                    % check relay cycle flag
                    if coinnet(i).conn(j).pssn == false
                        % make a new poisson counter
                        coinnet(i).conn(j).pssncount = poissrnd(100); % 5 second
                        % set flag to true
                        coinnet(i).conn(j).pssn = true;
                        
                    elseif coinnet(i).conn(j).pssncount == 0
                        
                        % empty all inv if pssn countdown ends
                        
                        coinnet(coinnet(i).conn(j).id).inflight =...
                            unique([coinnet(coinnet(i).conn(j).id).inflight,...
                            coinnet(i).conn(j).inv],'stable');
                        coinnet(i).conn(j).invsent =...
                            unique([coinnet(i).conn(j).invsent,...
                            coinnet(i).conn(j).inv],'stable');
                        coinnet(i).conn(j).inv = [];
                        
                        % reset cycle flag
                        coinnet(i).conn(j).pssn = false;
                        
                    else
                        % timer not there yet, just reduce count by 1
                        coinnet(i).conn(j).pssncount = coinnet(i).conn(j).pssncount - 1;
                        
                    end
                    
                end
                
                
            end
        end
        
        count = count + 1;
        % escape if node 1 has 1003 or limit is reached
        
        if ismember(1003,coinnet(3).mempool)
            break
        elseif count >= limit
            break
        end
    end
    
    
    %     end
    if count >= limit
        cycletable(h) = 0;
    else
        cycletable(h) = count;
    end
    disp(num2str(cycletable(h)))
end

delete(p)
cycletable_pruned = cycletable(cycletable ~= 0);
cycle = mean(cycletable_pruned);
cyclestd = std(cycletable_pruned);
failure = sum(cycletable == 0);

end


% subfunction to eliminate the second-seen from tx1 and tx2 if relay ==
% false
% "gen" flag signifies whether a ds is detected
function [output,gen] = ridsecond(input)
    if ~ismember(1001,input) || ~ismember(1002,input)
        output = input;
        gen = false;
    else
        gen = true;
        pos1 = find(input == 1001);
        pos2 = find(input == 1002);
        
        if pos1 < pos2
            output = input;
            output(pos2) = [];
        else 
            output = input;
            output(pos1) = [];
        end
    end
    
end




    


