#!/bin/bash
# Demo service management script
# Description: Manage system services
# Tags: system, services, admin

echo "⚙️  SERVICE MANAGER DEMO"
echo "======================="
echo

# Parse arguments
SERVICES=()
ACTION=""
FORCE=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --services)
            SERVICES+=("$2")
            shift 2
            ;;
        --action)
            ACTION="$2"
            shift 2
            ;;
        --force)
            FORCE=true
            shift
            ;;
        *)
            echo "Unknown argument: $1"
            shift
            ;;
    esac
done

echo "📋 Service Operation:"
echo "   Services: ${SERVICES[*]}"
echo "   Action: $ACTION"
echo "   Force: $FORCE"
echo

echo "🔄 Simulating service $ACTION operation..."

for service in "${SERVICES[@]}"; do
    echo "   ⚙️  Processing service: $service"
    
    case $ACTION in
        "start")
            echo "      - Starting $service..."
            sleep 1
            echo "      ✅ $service started successfully"
            ;;
        "stop")
            if [ "$FORCE" = true ]; then
                echo "      - Force stopping $service..."
            else
                echo "      - Gracefully stopping $service..."
            fi
            sleep 1
            echo "      ✅ $service stopped successfully"
            ;;
        "restart")
            echo "      - Restarting $service..."
            sleep 1.5
            echo "      ✅ $service restarted successfully"
            ;;
        "status")
            echo "      - Checking $service status..."
            sleep 0.5
            echo "      ✅ $service is running (PID: $((RANDOM % 9000 + 1000)))"
            ;;
        "enable")
            echo "      - Enabling $service for auto-start..."
            sleep 0.5
            echo "      ✅ $service enabled"
            ;;
        "disable")
            echo "      - Disabling $service auto-start..."
            sleep 0.5
            echo "      ✅ $service disabled"
            ;;
    esac
done

echo
echo "🎉 Service operation completed successfully!"
echo "📊 Processed ${#SERVICES[@]} services"