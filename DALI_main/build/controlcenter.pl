%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ControlCenter Agent Type
% Role: Coordinator
% Responsibilities: 
%   - Receive bin_full alerts from SmartBins
%   - Dispatch GarbageTrucks to collect waste
%   - Log all actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(dali)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EVENTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% External event: A SmartBin reports it is full
bin_full(Location) ::> (
    handle_bin_full(Location)
).

% External event: A GarbageTruck reports collection completed
collection_done(Location) ::> (
    send_message(logger1, log_event(collection_done(Location))),
    write('[ControlCenter] Collection confirmed at '), write(Location), nl
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ACTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Handle a full bin by dispatching a GarbageTruck
handle_bin_full(Location) :> (
    % Decide which truck to send (for now: truck1)
    send_message(truck1, collect(Location)),
    send_message(logger1, log_event(dispatch(truck1, Location))),
    write('[ControlCenter] Dispatched truck1 to '), write(Location), nl
).
