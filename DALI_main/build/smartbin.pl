%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SmartBin Agent Type
% Role: Sensor Agent
% Responsibilities: Detect when bin is full, notify ControlCenter and Logger
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(dali)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EVENTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% External event: bin becomes full
detect_full(Location) ::>
    notify_bin_full(Location).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ACTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Action: Notify ControlCenter and Logger
notify_bin_full(Location) :> (
    send_message(controlcenter1, bin_full(Location)),
    send_message(logger1, log_event(bin_full(Location))),
    write('[SmartBin] Bin at '), write(Location), write(' is full!'), nl
).
