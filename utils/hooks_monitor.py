#!/usr/bin/env python
"""
Hook Event Monitor for ESM-Tools Testing

This script registers hooks to monitor all events during an ESM-Tools run.
It logs events to both console and a JSON file for later analysis.

Usage:
    1. Import this module at the start of your runscript or in esm_tools/__init__.py:

       import test_hooks_monitor
       test_hooks_monitor.start_monitoring()

    2. Or run it as a standalone script that monkey-patches the hooks system:

       python test_hooks_monitor.py

    3. Then run your normal ESM-Tools command:

       esm_runscripts fesom2.6-albedo-hooks-test.yaml -e hooks_test_001

Events will be logged to:
    - Console (STDOUT)
    - hooks_events.json (JSON file for later analysis)
"""

import json
import sys
from datetime import datetime
from pathlib import Path

# Event storage
events_log = []
log_file = Path("hooks_events.json")

def log_event(event_name, **kwargs):
    """Log hook events with timestamp and details."""
    event = {
        "timestamp": datetime.now().isoformat(),
        "event": event_name,
        "details": {k: str(v)[:200] for k, v in kwargs.items() if k != 'config'}  # Truncate long values
    }

    events_log.append(event)

    # Pretty print to console
    print(f"\n{'='*80}")
    print(f"🎯 HOOK EVENT: {event_name}")
    print(f"   Time: {event['timestamp']}")
    if event['details']:
        print(f"   Details:")
        for key, value in event['details'].items():
            print(f"      {key}: {value}")
    print(f"{'='*80}\n")
    sys.stdout.flush()

    # Save to JSON file
    with open(log_file, 'w') as f:
        json.dump(events_log, f, indent=2)


def start_monitoring():
    """Register hooks to monitor all ESM-Tools events."""
    try:
        from esm_tools.hooks import add_hook

        # Register wildcard patterns for common event types
        event_patterns = [
            "esm_master:*",
            "esm_runscripts:recipe:*",
        ]

        print("\n" + "="*80)
        print("🔍 HOOK MONITORING STARTED")
        print(f"   Log file: {log_file.absolute()}")
        print(f"   Monitoring patterns: {', '.join(event_patterns)}")
        print("="*80 + "\n")
        sys.stdout.flush()

        # Since we can't use wildcards in the current implementation,
        # we'll create a general hook that captures everything
        # This requires modifying the trigger_hook function or registering specific hooks

        # For now, let's register the most common hooks we expect:
        common_hooks = [
            # esm_master hooks
            "esm_master:get-fesom-2.6:start",
            "esm_master:get-fesom-2.6:complete",
            "esm_master:get-fesom-2.6:error",
            "esm_master:comp-fesom-2.6:start",
            "esm_master:comp-fesom-2.6:complete",
            "esm_master:comp-fesom-2.6:error",

            # Recipe-level hooks
            "esm_runscripts:recipe:prepexp:start",
            "esm_runscripts:recipe:prepexp:complete",
            "esm_runscripts:recipe:compute:start",
            "esm_runscripts:recipe:compute:complete",
            "esm_runscripts:recipe:tidy_and_resubmit:start",
            "esm_runscripts:recipe:tidy_and_resubmit:complete",

            # Common recipe step hooks (you'll see more when you run it)
            "esm_runscripts:recipe:prepexp:initialize_experiment_logfile:start",
            "esm_runscripts:recipe:prepexp:initialize_experiment_logfile:complete",
            "esm_runscripts:recipe:prepexp:add_batch_hostfile:start",
            "esm_runscripts:recipe:prepexp:add_batch_hostfile:complete",
            "esm_runscripts:recipe:compute:namelist_changes:start",
            "esm_runscripts:recipe:compute:namelist_changes:complete",
            "esm_runscripts:recipe:compute:copy_files_to_work:start",
            "esm_runscripts:recipe:compute:copy_files_to_work:complete",
            "esm_runscripts:recipe:compute:create_new_files:start",
            "esm_runscripts:recipe:compute:create_new_files:complete",
            "esm_runscripts:recipe:compute:prepare_coupler_files:start",
            "esm_runscripts:recipe:compute:prepare_coupler_files:complete",
            "esm_runscripts:recipe:compute:add_batch_hostfile:start",
            "esm_runscripts:recipe:compute:add_batch_hostfile:complete",
            "esm_runscripts:recipe:compute:write_simple_runscript:start",
            "esm_runscripts:recipe:compute:write_simple_runscript:complete",
            "esm_runscripts:recipe:compute:report_missing_files:start",
            "esm_runscripts:recipe:compute:report_missing_files:complete",
            "esm_runscripts:recipe:compute:wait_for_first_job:start",
            "esm_runscripts:recipe:compute:wait_for_first_job:complete",
            "esm_runscripts:recipe:compute:run_job:start",
            "esm_runscripts:recipe:compute:run_job:complete",
            "esm_runscripts:recipe:compute:log_finish_date:start",
            "esm_runscripts:recipe:compute:log_finish_date:complete",
        ]

        for hook in common_hooks:
            add_hook(hook, log_event)

        print(f"✅ Registered {len(common_hooks)} hook listeners")
        print("   Run your ESM-Tools command now to see events!\n")
        sys.stdout.flush()

    except ImportError as e:
        print(f"❌ ERROR: Could not import hooks system: {e}")
        print("   Make sure you're running this from the esm_tools_edd environment")
        sys.exit(1)


def print_summary():
    """Print summary of captured events."""
    if not events_log:
        print("\n⚠️  No events captured")
        return

    print("\n" + "="*80)
    print(f"📊 EVENT SUMMARY")
    print(f"   Total events: {len(events_log)}")
    print(f"   Time range: {events_log[0]['timestamp']} to {events_log[-1]['timestamp']}")
    print("="*80)

    # Count events by type
    event_counts = {}
    for event in events_log:
        event_type = event['event'].split(':')[0] + ':' + event['event'].split(':')[1]
        event_counts[event_type] = event_counts.get(event_type, 0) + 1

    print("\n📈 Events by type:")
    for event_type, count in sorted(event_counts.items()):
        print(f"   {event_type}: {count}")

    print(f"\n💾 Full log saved to: {log_file.absolute()}\n")


if __name__ == "__main__":
    # If run as a script, start monitoring and wait
    start_monitoring()

    print("\n💡 TIP: You can also import this module in your Python code:")
    print("   >>> import test_hooks_monitor")
    print("   >>> test_hooks_monitor.start_monitoring()")
    print("\n   Then run your ESM-Tools commands in the same Python session")
    print("\n   Or add it to esm_tools/__init__.py for automatic monitoring\n")

    # Keep running to maintain hooks
    try:
        import time
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        print_summary()
        sys.exit(0)
