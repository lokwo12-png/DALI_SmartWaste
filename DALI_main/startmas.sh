#!/bin/bash
#########################################################
# Script: startmas.sh
# Purpose: Launch the entire Smart Waste Management MAS
# Usage: ./startmas.sh
#########################################################

echo "==============================================="
echo "  Starting Smart Waste Management MAS System"
echo "==============================================="

# Ensure communication configuration is ready
cd conf
./makeconf.sh
cd ..

# Start the Linda server (message broker)
echo ">>> Starting Linda server..."
xterm -hold -e "sicstus -l server/linda_server.pl --goal 'go.'" &

sleep 2  # give server time to initialize

# Start Logger first (to capture all logs)
echo ">>> Starting Logger..."
xterm -hold -e "./conf/startagent.sh logger1" &

sleep 1

# Start ControlCenter
echo ">>> Starting ControlCenter..."
xterm -hold -e "./conf/startagent.sh controlcenter1" &

sleep 1

# Start GarbageTrucks
echo ">>> Starting GarbageTrucks..."
xterm -hold -e "./conf/startagent.sh truck1" &
xterm -hold -e "./conf/startagent.sh truck2" &

sleep 1

# Start SmartBins
echo ">>> Starting SmartBins..."
xterm -hold -e "./conf/startagent.sh smartbin1" &
xterm -hold -e "./conf/startagent.sh smartbin2" &
xterm -hold -e "./conf/startagent.sh smartbin3" &

echo "==============================================="
echo "  All agents launched successfully!"
echo "  Logs are visible in each terminal window."
echo "==============================================="
