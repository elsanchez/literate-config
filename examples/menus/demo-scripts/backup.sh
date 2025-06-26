#!/bin/bash
# Demo database backup script
# Description: Database backup with options
# Tags: database, backup, demo

echo "💾 DATABASE BACKUP DEMO"
echo "======================="
echo

# Parse arguments
DATABASES=()
COMPRESSION=""
VERIFY_BACKUP=false
RETENTION_DAYS=30

while [[ $# -gt 0 ]]; do
    case $1 in
        --databases)
            DATABASES+=("$2")
            shift 2
            ;;
        --compression)
            COMPRESSION="$2"
            shift 2
            ;;
        --verify_backup)
            VERIFY_BACKUP=true
            shift
            ;;
        --retention_days)
            RETENTION_DAYS="$2"
            shift 2
            ;;
        *)
            echo "Unknown argument: $1"
            shift
            ;;
    esac
done

echo "📋 Backup Configuration:"
echo "   Databases: ${DATABASES[*]}"
echo "   Compression: $COMPRESSION"
echo "   Verify Backup: $VERIFY_BACKUP"
echo "   Retention: $RETENTION_DAYS days"
echo

echo "🔄 Simulating backup process..."

for db in "${DATABASES[@]}"; do
    echo "   ✅ Backing up database: $db"
    echo "      - Dumping data..."
    sleep 1
    
    if [ "$COMPRESSION" != "none" ]; then
        echo "      - Compressing with $COMPRESSION..."
        sleep 0.5
    fi
    
    if [ "$VERIFY_BACKUP" = true ]; then
        echo "      - Verifying backup integrity..."
        sleep 0.5
    fi
    
    echo "      - Backup saved: ${db}_backup_$(date +%Y%m%d).sql${COMPRESSION:+.${COMPRESSION}}"
done

echo
echo "🧹 Cleaning up old backups (older than $RETENTION_DAYS days)..."
sleep 1

echo "   ✅ Removed 3 old backup files"

echo
echo "🎉 Backup process completed successfully!"
echo "📊 Total backed up: ${#DATABASES[@]} databases"
echo "💾 Storage used: $(( ${#DATABASES[@]} * 256 ))MB"