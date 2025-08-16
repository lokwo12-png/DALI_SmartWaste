#!/bin/bash
#########################################################
# Script: startagent.sh
# Purpose: Start a single DALI agent
# Usage: ./startagent.sh agentname
# Example: ./startagent.sh smartbin1
#########################################################

if [ -z "$1" ]; then
  echo "Usage: $0 agentname"
  exit 1
fi

AGENT_NAME=$1

echo ">>> Starting agent: $AGENT_NAME"

# Start the agent in SICStus Prolog with DALI
sicstus -l ../mas/instances/$AGENT_NAME.pl --goal "dali_start."
