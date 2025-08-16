#!/bin/bash
#########################################################
# Script: makeconf.sh
# Purpose: Prepare communication configuration
# Usage: ./makeconf.sh
#########################################################

CONF_FILE="communication.con"

echo ">>> Setting up MAS communication configuration..."

# Check if configuration file exists
if [ -f "$CONF_FILE" ]; then
  echo "Configuration file $CONF_FILE already exists."
else
  echo "Creating default $CONF_FILE ..."
  cat > $CONF_FILE <<EOL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auto-generated Communication Configuration
% Smart Waste Management MAS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

linda_address('localhost', 44000).

agent(smartbin1, 'localhost', 44000).
agent(smartbin2, 'localhost', 44000).
agent(smartbin3, 'localhost', 44000).

agent(controlcenter1, 'localhost', 44000).

agent(truck1, 'localhost', 44000).
agent(truck2, 'localhost', 44000).

agent(logger1, 'localhost', 44000).
EOL
  echo "Default communication configuration created."
fi

echo ">>> makeconf.sh completed."
