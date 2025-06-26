#!/usr/bin/env python3
"""
Textual Script Runner - Demo Version
Working with available widgets only
"""

import os
import sys
import json
import yaml
import subprocess
import asyncio
from pathlib import Path
from dataclasses import dataclass, field
from typing import List, Dict, Optional, Any, Union
import argparse

from textual.app import App, ComposeResult
from textual.containers import Container, Horizontal, Vertical
from textual.widgets import (
    Header, Footer, Button, Input, Label, Tree, Static, 
    Checkbox, Select, DataTable, TabbedContent, TabPane,
    Switch, Log
)
from textual.screen import Screen
from textual.binding import Binding
from textual import on
from rich.text import Text
from rich.panel import Panel
from rich.syntax import Syntax

@dataclass
class ScriptArg:
    name: str
    description: str
    type: str = "text"
    required: bool = True
    default: Optional[Union[str, List[str], bool, int, float]] = None
    choices: Optional[List[str]] = None
    min_value: Optional[Union[int, float]] = None
    max_value: Optional[Union[int, float]] = None
    step: Optional[Union[int, float]] = None
    help_text: str = ""

@dataclass
class Script:
    name: str
    path: str
    description: str = ""
    args: List[ScriptArg] = field(default_factory=list)
    tags: List[str] = field(default_factory=list)
    help_text: str = ""
    async_exec: bool = True
    preview_mode: bool = False
    category: str = "General"

@dataclass
class Menu:
    name: str
    description: str
    scripts: List[Script] = field(default_factory=list)
    submenus: List['Menu'] = field(default_factory=list)

class ScriptConfigScreen(Screen):
    """Screen for configuring script arguments"""
    
    BINDINGS = [
        Binding("escape", "app.pop_screen", "Back"),
    ]
    
    def __init__(self, script, **kwargs):
        super().__init__(**kwargs)
        self.script = script
        self.collected_values = {}

    def compose(self) -> ComposeResult:
        yield Header()
        
        with Container():
            yield Label(f"🚀 Configure: {self.script.name}", classes="title")
            yield Label(self.script.description, classes="subtitle")
            
            if self.script.help_text:
                yield Label(self.script.help_text, classes="help")
            
            # Build argument inputs
            for arg in self.script.args:
                yield Label(f"{arg.name}: {arg.description}")
                
                if arg.type == "text":
                    yield Input(
                        value=str(arg.default or ""),
                        placeholder=arg.description,
                        id=f"arg_{arg.name}"
                    )
                elif arg.type == "select":
                    options = [(choice, choice) for choice in (arg.choices or [])]
                    yield Select(
                        options,
                        value=arg.default,
                        id=f"arg_{arg.name}"
                    )
                elif arg.type == "checkbox":
                    yield Switch(
                        value=bool(arg.default),
                        id=f"arg_{arg.name}"
                    )
                elif arg.type == "number":
                    yield Input(
                        value=str(arg.default or 0),
                        placeholder=f"Number ({arg.min_value or 0} - {arg.max_value or 100})",
                        id=f"arg_{arg.name}"
                    )
                # Note: Slider not available in this Textual version
            
            # Buttons
            with Horizontal():
                yield Button("Cancel", variant="error", id="cancel")
                yield Button("Preview", variant="default", id="preview")
                yield Button("Execute", variant="success", id="execute")
        
        yield Footer()

    @on(Button.Pressed, "#cancel")
    def cancel_config(self):
        self.app.pop_screen()

    @on(Button.Pressed, "#preview")
    def preview_command(self):
        command = self.build_command()
        command_text = " ".join(command)
        
        # Show in log
        main_screen = self.app.screen_stack[0]
        log = main_screen.query_one("#main-log", Log)
        log.write_line(f"🔍 Preview: {command_text}")

    @on(Button.Pressed, "#execute")
    def execute_script(self):
        command = self.build_command()
        self.execute_command(command)
        self.app.pop_screen()

    def build_command(self) -> List[str]:
        """Build command from collected values"""
        cmd = [self.script.path]
        
        for arg in self.script.args:
            widget_id = f"arg_{arg.name}"
            
            try:
                if arg.type == "checkbox":
                    switch = self.query_one(f"#{widget_id}", Switch)
                    if switch.value:
                        cmd.append(f"--{arg.name}")
                else:
                    widget = self.query_one(f"#{widget_id}")
                    if hasattr(widget, 'value') and widget.value:
                        cmd.extend([f"--{arg.name}", str(widget.value)])
            except:
                pass  # Widget not found or no value
        
        return cmd

    def execute_command(self, command: List[str]):
        """Execute the command"""
        main_screen = self.app.screen_stack[0]
        log = main_screen.query_one("#main-log", Log)
        
        command_text = " ".join(command)
        log.write_line(f"🚀 Executing: {command_text}")
        
        try:
            if self.script.preview_mode:
                log.write_line("⚠️  Preview mode - simulating execution")
                log.write_line("✅ Script would execute successfully")
            else:
                result = subprocess.run(command, capture_output=True, text=True, timeout=30)
                
                if result.returncode == 0:
                    log.write_line("✅ Script completed successfully")
                    if result.stdout:
                        for line in result.stdout.strip().split('\n'):
                            log.write_line(f"   {line}")
                else:
                    log.write_line(f"❌ Script failed (exit code: {result.returncode})")
                    if result.stderr:
                        for line in result.stderr.strip().split('\n'):
                            log.write_line(f"   ERROR: {line}")
        except subprocess.TimeoutExpired:
            log.write_line("⏰ Script execution timed out")
        except Exception as e:
            log.write_line(f"❌ Error executing script: {e}")

class TextualScriptRunner(App):
    """Main Textual application"""
    
    CSS = """
    .title {
        text-style: bold;
        color: $accent;
        margin: 1;
    }
    
    .subtitle {
        color: $text-muted;
        margin-bottom: 1;
    }
    
    .help {
        color: $text-muted;
        text-style: italic;
        margin: 1;
    }
    
    .main-container {
        padding: 1;
    }
    
    .menu-tree {
        width: 30%;
        background: $surface;
        border-right: solid $primary;
    }
    
    .content-area {
        width: 70%;
        padding: 1;
    }
    
    .stats-container {
        background: $surface;
        padding: 1;
        margin: 1;
        border: solid $accent;
    }
    """
    
    BINDINGS = [
        Binding("ctrl+r", "reload_config", "Reload"),
        Binding("ctrl+q", "quit", "Quit"),
        Binding("f1", "help", "Help"),
    ]
    
    TITLE = "🚀 Textual Script Runner"
    SUB_TITLE = "Demo TUI Interface"
    
    def __init__(self, config_path: str = "~/.config/textual-script-runner.yaml"):
        super().__init__()
        self.config_path = Path(config_path).expanduser()
        self.menus: List[Menu] = []

    def compose(self) -> ComposeResult:
        yield Header()
        
        with Horizontal(classes="main-container"):
            # Left side - Menu tree
            with Container(classes="menu-tree"):
                yield Label("📁 Menus", classes="title")
                yield Tree("Scripts", id="menu-tree")
            
            # Right side - Content area
            with Container(classes="content-area"):
                with TabbedContent(initial="scripts"):
                    with TabPane("Scripts", id="scripts"):
                        yield DataTable(id="scripts-table")
                    
                    with TabPane("Statistics", id="stats"):
                        yield Container(
                            Label("📊 Statistics", classes="title"),
                            Label("", id="stats-content"),
                            classes="stats-container"
                        )
                    
                    with TabPane("Logs", id="logs"):
                        yield Log(id="main-log")
        
        yield Footer()

    async def on_mount(self):
        """Initialize the application"""
        await self.load_config()
        self.build_menu_tree()
        self.update_scripts_table()
        self.update_statistics()
        
        # Welcome message
        log = self.query_one("#main-log", Log)
        log.write_line("🚀 Textual Script Runner - Demo Version")
        log.write_line("======================================")
        log.write_line("✅ Working UI components:")
        log.write_line("   📝 Text Input, 🔘 Select, ☑️ Checkbox, 🔢 Number Input")
        log.write_line("")
        log.write_line("💡 Click on scripts to configure and execute")
        log.write_line("⌨️  Keyboard shortcuts: Ctrl+R (reload), Ctrl+Q (quit), F1 (help)")

    async def load_config(self):
        """Load configuration from YAML file"""
        if not self.config_path.exists():
            await self.create_demo_config()
            
        with open(self.config_path) as f:
            config = yaml.safe_load(f)
            
        self.menus = self._parse_config(config)

    async def create_demo_config(self):
        """Create demo configuration"""
        demo_config = {
            'menus': [
                {
                    'name': 'UI Demo Scripts',
                    'description': 'Demonstration scripts with UI components',
                    'scripts': [
                        {
                            'name': 'deploy-with-options',
                            'path': './demo-scripts/deploy.sh',
                            'description': 'Deployment with various options',
                            'category': 'Deployment',
                            'args': [
                                {
                                    'name': 'environment',
                                    'type': 'select',
                                    'description': 'Target environment',
                                    'choices': ['development', 'staging', 'production'],
                                    'default': 'development',
                                    'help_text': 'Choose deployment target'
                                },
                                {
                                    'name': 'backup_enabled',
                                    'type': 'checkbox',
                                    'description': 'Create backup before deployment',
                                    'default': True,
                                    'help_text': 'Recommended for production'
                                },
                                {
                                    'name': 'parallel_jobs',
                                    'type': 'number',
                                    'description': 'Number of parallel jobs',
                                    'default': 4,
                                    'help_text': 'Higher = faster but more resources'
                                },
                                {
                                    'name': 'timeout',
                                    'type': 'number',
                                    'description': 'Timeout in seconds',
                                    'default': 300,
                                    'help_text': 'Maximum time to wait'
                                },
                                {
                                    'name': 'server_name',
                                    'type': 'text',
                                    'description': 'Target server hostname',
                                    'default': 'localhost',
                                    'help_text': 'Server to deploy to'
                                }
                            ],
                            'tags': ['deployment', 'demo'],
                            'help_text': 'Demo deployment script showcasing UI components',
                            'preview_mode': True
                        },
                        {
                            'name': 'backup-with-compression',
                            'path': './demo-scripts/backup.sh',
                            'description': 'Backup with compression options',
                            'category': 'Database',
                            'args': [
                                {
                                    'name': 'compression',
                                    'type': 'select',
                                    'description': 'Compression method',
                                    'choices': ['gzip', 'bzip2', 'xz', 'none'],
                                    'default': 'gzip'
                                },
                                {
                                    'name': 'verify',
                                    'type': 'checkbox',
                                    'description': 'Verify backup integrity',
                                    'default': True
                                },
                                {
                                    'name': 'retention_days',
                                    'type': 'number',
                                    'description': 'Retention period (days)',
                                    'default': 30
                                },
                                {
                                    'name': 'max_size_gb',
                                    'type': 'number',
                                    'description': 'Maximum backup size (GB)',
                                    'default': 10
                                }
                            ],
                            'tags': ['backup', 'demo'],
                            'help_text': 'Demo backup script with configuration options',
                            'preview_mode': True
                        },
                        {
                            'name': 'simple-script',
                            'path': './demo-scripts/service.sh',
                            'description': 'Simple script without arguments',
                            'category': 'System',
                            'args': [],
                            'tags': ['simple', 'demo'],
                            'help_text': 'Simple script that runs without configuration',
                            'preview_mode': True
                        }
                    ]
                }
            ]
        }
        
        self.config_path.parent.mkdir(parents=True, exist_ok=True)
        with open(self.config_path, 'w') as f:
            yaml.dump(demo_config, f, indent=2, default_flow_style=False)

    def _parse_config(self, config: Dict) -> List[Menu]:
        """Parse configuration into Menu objects"""
        menus = []
        for menu_config in config.get('menus', []):
            menu = self._parse_menu(menu_config)
            menus.append(menu)
        return menus

    def _parse_menu(self, menu_config: Dict) -> Menu:
        """Parse single menu configuration"""
        scripts = []
        for script_config in menu_config.get('scripts', []):
            script = self._parse_script(script_config)
            scripts.append(script)
            
        return Menu(
            name=menu_config['name'],
            description=menu_config.get('description', ''),
            scripts=scripts
        )

    def _parse_script(self, script_config: Dict) -> Script:
        """Parse single script configuration"""
        args = []
        for arg_config in script_config.get('args', []):
            arg = ScriptArg(
                name=arg_config['name'],
                description=arg_config['description'],
                type=arg_config.get('type', 'text'),
                required=arg_config.get('required', True),
                default=arg_config.get('default'),
                choices=arg_config.get('choices'),
                min_value=arg_config.get('min_value'),
                max_value=arg_config.get('max_value'),
                step=arg_config.get('step'),
                help_text=arg_config.get('help_text', '')
            )
            args.append(arg)
            
        return Script(
            name=script_config['name'],
            path=script_config['path'],
            description=script_config.get('description', ''),
            args=args,
            tags=script_config.get('tags', []),
            help_text=script_config.get('help_text', ''),
            async_exec=script_config.get('async_exec', True),
            preview_mode=script_config.get('preview_mode', False),
            category=script_config.get('category', 'General')
        )

    def build_menu_tree(self):
        """Build the menu tree widget"""
        tree = self.query_one("#menu-tree", Tree)
        tree.clear()
        
        root = tree.root
        root.expand()
        
        for menu in self.menus:
            menu_node = root.add(f"📁 {menu.name}", data=menu)
            menu_node.expand()
            
            # Add scripts
            if menu.scripts:
                for script in menu.scripts:
                    script_node = menu_node.add(f"🚀 {script.name}", data=script)

    def update_scripts_table(self):
        """Update the scripts data table"""
        table = self.query_one("#scripts-table", DataTable)
        table.clear(columns=True)
        
        # Add columns
        table.add_columns("Name", "Category", "Description", "Args", "Tags")
        
        # Add all scripts
        for menu in self.menus:
            for script in menu.scripts:
                tags_str = ", ".join(script.tags) if script.tags else ""
                args_count = len(script.args)
                
                table.add_row(
                    script.name,
                    script.category,
                    script.description,
                    f"{args_count} args",
                    tags_str,
                    key=script.name
                )

    def update_statistics(self):
        """Update statistics display"""
        total_scripts = sum(len(menu.scripts) for menu in self.menus)
        total_menus = len(self.menus)
        
        # Count by category
        categories = {}
        for menu in self.menus:
            for script in menu.scripts:
                cat = script.category
                categories[cat] = categories.get(cat, 0) + 1
        
        stats_text = f"""📊 Total Scripts: {total_scripts}
📁 Total Menus: {total_menus}

📋 By Category:
""" + "\n".join(f"  {cat}: {count}" for cat, count in categories.items())
        
        stats_label = self.query_one("#stats-content", Label)
        stats_label.update(stats_text)

    @on(Tree.NodeSelected)
    def on_tree_node_selected(self, event: Tree.NodeSelected):
        """Handle tree node selection"""
        if hasattr(event.node.data, 'name'):
            data = event.node.data
            log = self.query_one("#main-log", Log)
            
            if isinstance(data, Script):
                log.write_line(f"📋 Selected script: {data.name}")
                # Auto-run script
                if data.args:
                    log.write_line(f"   Has {len(data.args)} arguments - opening config screen")
                    self.push_screen(ScriptConfigScreen(data))
                else:
                    log.write_line("   No arguments - executing directly")
                    self.execute_simple_script(data)
            elif isinstance(data, Menu):
                log.write_line(f"📁 Selected menu: {data.name}")

    @on(DataTable.RowSelected)
    def on_script_selected(self, event: DataTable.RowSelected):
        """Handle script selection from table"""
        table = event.data_table
        script_name = str(table.get_row_at(event.cursor_row)[0])
        
        # Find the script
        script = self.find_script_by_name(script_name)
        if script:
            log = self.query_one("#main-log", Log)
            log.write_line(f"🎯 Selected from table: {script.name}")
            
            if script.args:
                log.write_line(f"   Opening configuration screen with {len(script.args)} arguments")
                self.push_screen(ScriptConfigScreen(script))
            else:
                log.write_line("   Executing directly (no arguments)")
                self.execute_simple_script(script)

    def find_script_by_name(self, name: str) -> Optional[Script]:
        """Find script by name across all menus"""
        for menu in self.menus:
            for script in menu.scripts:
                if script.name == name:
                    return script
        return None

    def execute_simple_script(self, script: Script):
        """Execute script without arguments"""
        log = self.query_one("#main-log", Log)
        log.write_line(f"🚀 Executing: {script.name}")
        
        try:
            if script.preview_mode:
                log.write_line("⚠️  Preview mode - simulating execution")
                log.write_line("✅ Script would execute successfully")
                log.write_line("📋 Command would be: " + script.path)
            else:
                result = subprocess.run([script.path], capture_output=True, text=True, timeout=30)
                
                if result.returncode == 0:
                    log.write_line("✅ Script completed successfully")
                    if result.stdout:
                        for line in result.stdout.strip().split('\n'):
                            log.write_line(f"   {line}")
                else:
                    log.write_line(f"❌ Script failed (exit code: {result.returncode})")
                    if result.stderr:
                        for line in result.stderr.strip().split('\n'):
                            log.write_line(f"   ERROR: {line}")
        except Exception as e:
            log.write_line(f"❌ Error executing script: {e}")

    def action_reload_config(self):
        """Reload configuration"""
        async def reload():
            await self.load_config()
            self.build_menu_tree()
            self.update_scripts_table()
            self.update_statistics()
            
            log = self.query_one("#main-log", Log)
            log.write_line("🔄 Configuration reloaded successfully")
        
        self.run_worker(reload(), exclusive=True)

    def action_help(self):
        """Show help information"""
        log = self.query_one("#main-log", Log)
        log.write_line("")
        log.write_line("🚀 Textual Script Runner - Help")
        log.write_line("=================================")
        log.write_line("")
        log.write_line("⌨️  Keyboard Shortcuts:")
        log.write_line("   Ctrl+R - Reload Configuration")
        log.write_line("   Ctrl+Q - Quit Application")
        log.write_line("   F1 - Show this help")
        log.write_line("   Escape - Go back (in config screens)")
        log.write_line("")
        log.write_line("🎯 Navigation:")
        log.write_line("   • Click on scripts in table or tree to execute")
        log.write_line("   • Use tabs to switch between Scripts/Statistics/Logs")
        log.write_line("   • Scripts with arguments open configuration screen")
        log.write_line("   • Scripts without arguments execute immediately")
        log.write_line("")
        log.write_line("🧩 UI Components Available:")
        log.write_line("   📝 Text Input - Free form text")
        log.write_line("   🔘 Select - Dropdown choices")
        log.write_line("   ☑️ Checkbox - On/off toggles")
        log.write_line("   🔢 Number Input - Numeric values")
        log.write_line("")
        log.write_line("🎮 Try the demo scripts to see all components in action!")

def main():
    parser = argparse.ArgumentParser(description="Textual Script Runner - Demo Version")
    parser.add_argument("--config", "-c", default="~/.config/textual-script-runner.yaml",
                       help="Configuration file path")
    parser.add_argument("--demo", action="store_true",
                       help="Create demo configuration and exit")
    
    args = parser.parse_args()
    
    app = TextualScriptRunner(args.config)
    
    if args.demo:
        import asyncio
        asyncio.run(app.create_demo_config())
        print(f"✅ Demo configuration created at: {app.config_path}")
        print("🚀 Run without --demo to start the application:")
        print(f"   python3 {sys.argv[0]}")
    else:
        app.run()

if __name__ == "__main__":
    main()