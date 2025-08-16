%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GarbageTruck Agent Type
% Role: Collector
% Responsibilities:
%   - Receive collection orders from ControlCenter
%   - Collect waste from the specified bin location
%   - Report completion back to ControlCenter
%   - Log all activities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(dali)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EVENTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% External event: ControlCenter requests waste collection
collect(Location) ::> (
    collect_waste(Location)
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ACTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Action: Perform waste collection and notify ControlCenter + Logger
collect_waste(Location) :> (
    write('[GarbageTruck] Collecting waste at '), write(Location), nl,
    send_message(controlcenter1, collection_done(Location)),
    send_message(logger1, log_event(collection_done(Location)))
).
