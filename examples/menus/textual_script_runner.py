#!/usr/bin/env python3
"""
Textual Script Runner - Advanced TUI with Command Palette
Modern interface inspired by VS Code command palette
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

try:
    from textual.app import App, ComposeResult
    from textual.containers import Container, Horizontal, Vertical, ScrollableContainer
    from textual.widgets import (
        Header, Footer, Button, Input, Label, Tree, Static, 
        Checkbox, RadioSet, RadioButton, Select, ProgressBar,
        DataTable, Tabs, Tab, TabbedContent, TabPane,
        OptionList, ListView, ListItem, Switch, Slider,
        DirectoryTree, Log, RichLog, Markdown
    )
    from textual.screen import Screen, ModalScreen
    from textual.binding import Binding
    from textual.reactive import reactive
    from textual.message import Message
    from textual.command import Hit, Hits, Provider
    from textual import on, work
    from textual.validation import Function, ValidationResult, Validator
    from rich.text import Text
    from rich.console import RenderableType
    from rich.panel import Panel
    from rich.syntax import Syntax
    HAS_TEXTUAL = True
except ImportError:
    HAS_TEXTUAL = False
    print("Install textual for advanced TUI: pip install textual")

@dataclass
class ScriptArg:
    name: str
    description: str
    type: str = "text"  # text, select, multi_select, checkbox, radio, number, file, directory, slider
    required: bool = True
    default: Optional[Union[str, List[str], bool, int, float]] = None
    choices: Optional[List[str]] = None
    min_value: Optional[Union[int, float]] = None
    max_value: Optional[Union[int, float]] = None
    step: Optional[Union[int, float]] = None
    file_extensions: Optional[List[str]] = None
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

class ScriptCommandProvider(Provider):
    """Command provider for script execution"""
    
    def __init__(self, app: "TextualScriptRunner"):
        super().__init__()
        self.app = app

    async def startup(self) -> None:
        """Called when the command palette is opened"""
        pass

    async def search(self, query: str) -> Hits:
        """Search through available scripts and commands"""
        matcher = self.matcher(query)
        
        # System commands
        system_commands = [
            ("🏠 Go to main menu", "go_main"),
            ("📁 Open file explorer", "file_explorer"),
            ("⚙️ Settings", "settings"),
            ("🔄 Reload config", "reload"),
            ("❌ Quit application", "quit"),
        ]
        
        for text, command in system_commands:
            if matcher.match(text):
                yield Hit(
                    score=matcher.score,
                    match_display=matcher.highlight(text),
                    command=command,
                    help=f"Execute {text.split(' ', 1)[1]}"
                )

        # Script commands
        for menu in self.app.menus:
            for script in menu.scripts:
                search_text = f"🚀 Run {script.name} - {script.description}"
                if matcher.match(search_text):
                    yield Hit(
                        score=matcher.score,
                        match_display=matcher.highlight(search_text),
                        command=f"run_script:{script.name}",
                        help=script.help_text or script.description
                    )
                
                # Script tags
                for tag in script.tags:
                    tag_text = f"🏷️ Tag: {tag}"
                    if matcher.match(tag_text):
                        yield Hit(
                            score=matcher.score * 0.8,  # Lower score for tag matches
                            match_display=matcher.highlight(tag_text),
                            command=f"filter_tag:{tag}",
                            help=f"Show all scripts with tag '{tag}'"
                        )

class ArgumentCollectorScreen(ModalScreen):
    """Modal screen for collecting script arguments"""
    
    BINDINGS = [
        Binding("escape", "dismiss", "Cancel"),
        Binding("ctrl+s", "submit", "Execute Script"),
    ]
    
    def __init__(self, script: Script, **kwargs):
        super().__init__(**kwargs)
        self.script = script
        self.collected_values = {}
        self.widgets_map = {}

    def compose(self) -> ComposeResult:
        yield Container(
            Container(
                Label(f"🚀 Configure: {self.script.name}", classes="title"),
                Label(self.script.description, classes="subtitle"),
                classes="header"
            ),
            ScrollableContainer(
                self.build_argument_widgets(),
                classes="args-container"
            ),
            Horizontal(
                Button("Cancel", variant="error", id="cancel"),
                Button("Preview Command", variant="default", id="preview"),
                Button("Execute", variant="success", id="execute"),
                classes="buttons"
            ),
            classes="dialog"
        )

    def build_argument_widgets(self) -> ComposeResult:
        """Build widgets for each script argument"""
        for arg in self.script.args:
            # Argument container
            with Container(classes="arg-container"):
                # Label with help
                yield Label(f"{arg.name}", classes="arg-label")
                if arg.help_text:
                    yield Label(arg.help_text, classes="arg-help")
                
                # Widget based on type
                widget = self.create_argument_widget(arg)
                if widget:
                    self.widgets_map[arg.name] = widget
                    yield widget

    def create_argument_widget(self, arg: ScriptArg):
        """Create appropriate widget for argument type"""
        widget_id = f"arg_{arg.name}"
        
        if arg.type == "text":
            widget = Input(
                value=str(arg.default or ""),
                placeholder=arg.description,
                id=widget_id
            )
            
        elif arg.type == "number":
            widget = Input(
                value=str(arg.default or 0),
                placeholder=f"Number ({arg.min_value or 0} - {arg.max_value or 100})",
                id=widget_id
            )
            
        elif arg.type == "slider":
            widget = Slider(
                value=float(arg.default or arg.min_value or 0),
                min=float(arg.min_value or 0),
                max=float(arg.max_value or 100),
                step=float(arg.step or 1),
                id=widget_id
            )
            
        elif arg.type == "checkbox":
            widget = Switch(
                value=bool(arg.default),
                id=widget_id
            )
            
        elif arg.type == "select":
            options = [(choice, choice) for choice in (arg.choices or [])]
            widget = Select(
                options,
                value=arg.default,
                id=widget_id
            )
            
        elif arg.type == "radio":
            with RadioSet(id=widget_id):
                for choice in (arg.choices or []):
                    yield RadioButton(
                        choice,
                        value=(choice == arg.default),
                        id=f"radio_{arg.name}_{choice}"
                    )
            return None  # RadioSet yields its own children
            
        elif arg.type == "multi_select":
            options = [choice for choice in (arg.choices or [])]
            default_selected = set(arg.default or [])
            widget = OptionList(
                *[(choice, choice, choice in default_selected) for choice in options],
                id=widget_id
            )
            
        elif arg.type == "file":
            widget = Input(
                placeholder="File path...",
                id=widget_id
            )
            
        else:
            widget = Input(
                value=str(arg.default or ""),
                placeholder=arg.description,
                id=widget_id
            )
        
        return widget

    @on(Button.Pressed, "#cancel")
    def cancel_dialog(self):
        self.dismiss(None)

    @on(Button.Pressed, "#preview")
    def preview_command(self):
        self.collect_values()
        command = self.build_command()
        self.app.show_command_preview(command)

    @on(Button.Pressed, "#execute")
    def execute_script(self):
        self.collect_values()
        command = self.build_command()
        self.dismiss(command)

    def collect_values(self):
        """Collect values from all argument widgets"""
        self.collected_values = {}
        
        for arg in self.script.args:
            widget_id = f"arg_{arg.name}"
            
            if arg.type == "radio":
                # Find selected radio button
                radio_set = self.query_one(f"#{widget_id}", RadioSet)
                if radio_set.pressed_button:
                    self.collected_values[arg.name] = radio_set.pressed_button.label.plain
                    
            elif arg.type == "multi_select":
                option_list = self.query_one(f"#{widget_id}", OptionList)
                selected = [option.prompt for option in option_list.highlighted]
                self.collected_values[arg.name] = selected
                
            elif arg.type == "checkbox":
                switch = self.query_one(f"#{widget_id}", Switch)
                self.collected_values[arg.name] = switch.value
                
            elif arg.type == "slider":
                slider = self.query_one(f"#{widget_id}", Slider)
                self.collected_values[arg.name] = slider.value
                
            else:
                try:
                    widget = self.query_one(f"#{widget_id}")
                    if hasattr(widget, 'value'):
                        self.collected_values[arg.name] = widget.value
                except:
                    pass

    def build_command(self) -> List[str]:
        """Build command from collected values"""
        cmd = [self.script.path]
        
        for arg in self.script.args:
            value = self.collected_values.get(arg.name)
            if value is None:
                continue
                
            if arg.type == "checkbox":
                if value:
                    cmd.append(f"--{arg.name}")
            elif arg.type == "multi_select":
                for v in value:
                    cmd.extend([f"--{arg.name}", str(v)])
            else:
                cmd.extend([f"--{arg.name}", str(value)])
        
        return cmd

class CommandPreviewScreen(ModalScreen):
    """Modal screen for command preview"""
    
    BINDINGS = [
        Binding("escape", "dismiss", "Close"),
        Binding("enter", "execute", "Execute"),
    ]
    
    def __init__(self, command: List[str], **kwargs):
        super().__init__(**kwargs)
        self.command = command

    def compose(self) -> ComposeResult:
        command_text = " ".join(self.command)
        yield Container(
            Label("🔍 Command Preview", classes="title"),
            Container(
                Static(
                    Syntax(command_text, "bash", theme="monokai", line_numbers=False),
                    classes="command-preview"
                ),
                classes="preview-container"
            ),
            Horizontal(
                Button("Close", variant="default", id="close"),
                Button("Execute", variant="success", id="execute"),
                classes="buttons"
            ),
            classes="dialog"
        )

    @on(Button.Pressed, "#close")
    def close_preview(self):
        self.dismiss(None)

    @on(Button.Pressed, "#execute")
    def execute_command(self):
        self.dismiss(self.command)

class ScriptExecutionScreen(ModalScreen):
    """Modal screen for script execution with real-time output"""
    
    BINDINGS = [
        Binding("escape", "close", "Close"),
    ]
    
    def __init__(self, command: List[str], **kwargs):
        super().__init__(**kwargs)
        self.command = command

    def compose(self) -> ComposeResult:
        yield Container(
            Label(f"🚀 Executing: {' '.join(self.command)}", classes="title"),
            RichLog(classes="output-log"),
            Button("Close", variant="default", id="close"),
            classes="dialog execution-dialog"
        )

    async def on_mount(self):
        """Start script execution when screen mounts"""
        log = self.query_one(RichLog)
        
        try:
            # Execute command
            process = await asyncio.create_subprocess_exec(
                *self.command,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.STDOUT,
                text=True
            )
            
            log.write(f"🚀 Starting: {' '.join(self.command)}")
            
            # Read output line by line
            while True:
                line = await process.stdout.readline()
                if not line:
                    break
                log.write(line.rstrip())
            
            # Wait for completion
            await process.wait()
            
            if process.returncode == 0:
                log.write("✅ Script completed successfully!")
            else:
                log.write(f"❌ Script failed with exit code: {process.returncode}")
                
        except Exception as e:
            log.write(f"❌ Error executing script: {e}")

    @on(Button.Pressed, "#close")
    def close_execution(self):
        self.dismiss()

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
    
    .header {
        background: $surface;
        padding: 1;
        margin-bottom: 1;
    }
    
    .dialog {
        align: center middle;
        width: 80%;
        height: 80%;
        background: $surface;
        border: thick $primary;
        padding: 1;
    }
    
    .execution-dialog {
        width: 90%;
        height: 70%;
    }
    
    .args-container {
        height: 1fr;
        margin: 1;
    }
    
    .arg-container {
        border: solid $primary-lighten-2;
        margin: 1;
        padding: 1;
    }
    
    .arg-label {
        text-style: bold;
        color: $warning;
    }
    
    .arg-help {
        color: $text-muted;
        text-style: italic;
        margin-bottom: 1;
    }
    
    .buttons {
        height: auto;
        margin-top: 1;
    }
    
    .buttons Button {
        margin: 0 1;
    }
    
    .command-preview {
        padding: 1;
        margin: 1;
        border: solid $accent;
    }
    
    .preview-container {
        height: 1fr;
        margin: 1;
    }
    
    .output-log {
        height: 1fr;
        margin: 1;
        border: solid $success;
    }
    
    .script-table {
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
        Binding("ctrl+p", "command_palette", "Command Palette"),
        Binding("ctrl+r", "reload_config", "Reload Config"),
        Binding("ctrl+q", "quit", "Quit"),
        Binding("f1", "help", "Help"),
    ]
    
    TITLE = "🚀 Textual Script Runner"
    SUB_TITLE = "Advanced TUI with Command Palette"
    
    def __init__(self, config_path: str = "~/.config/textual-script-runner.yaml"):
        super().__init__()
        self.config_path = Path(config_path).expanduser()
        self.menus: List[Menu] = []
        self.current_menu = None
        self.command_provider = ScriptCommandProvider(self)

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
                        yield DataTable(id="scripts-table", classes="script-table")
                    
                    with TabPane("Statistics", id="stats"):
                        yield Container(
                            Label("📊 Statistics", classes="title"),
                            Label("", id="stats-content"),
                            classes="stats-container"
                        )
                    
                    with TabPane("Logs", id="logs"):
                        yield RichLog(id="main-log")
        
        yield Footer()

    async def on_mount(self):
        """Initialize the application"""
        await self.load_config()
        self.build_menu_tree()
        self.update_scripts_table()
        self.update_statistics()
        
        # Register command provider
        self.command_palette.register_provider(self.command_provider)

    async def load_config(self):
        """Load configuration from YAML file"""
        if not self.config_path.exists():
            await self.create_demo_config()
            
        with open(self.config_path) as f:
            config = yaml.safe_load(f)
            
        self.menus = self._parse_config(config)

    async def create_demo_config(self):
        """Create comprehensive demo configuration"""
        demo_config = {
            'menus': [
                {
                    'name': 'Development Tools',
                    'description': 'Development and deployment scripts',
                    'scripts': [
                        {
                            'name': 'advanced-deploy',
                            'path': './demo-scripts/deploy.sh',
                            'description': 'Advanced deployment with all UI components',
                            'category': 'Deployment',
                            'args': [
                                {
                                    'name': 'environment',
                                    'type': 'radio',
                                    'description': 'Target environment',
                                    'choices': ['development', 'staging', 'production'],
                                    'default': 'development',
                                    'help_text': 'Choose deployment target'
                                },
                                {
                                    'name': 'features',
                                    'type': 'multi_select',
                                    'description': 'Features to enable',
                                    'choices': ['auth', 'analytics', 'caching', 'cdn', 'monitoring', 'backup'],
                                    'default': ['auth', 'monitoring'],
                                    'help_text': 'Select multiple features'
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
                                    'type': 'slider',
                                    'description': 'Parallel deployment jobs',
                                    'min_value': 1,
                                    'max_value': 16,
                                    'step': 1,
                                    'default': 4,
                                    'help_text': 'Higher = faster but more resources'
                                },
                                {
                                    'name': 'timeout',
                                    'type': 'number',
                                    'description': 'Timeout in seconds',
                                    'min_value': 30,
                                    'max_value': 3600,
                                    'default': 300,
                                    'help_text': 'Maximum time to wait'
                                },
                                {
                                    'name': 'notification_method',
                                    'type': 'select',
                                    'description': 'Notification method',
                                    'choices': ['email', 'slack', 'discord', 'webhook', 'none'],
                                    'default': 'email',
                                    'help_text': 'How to receive notifications'
                                }
                            ],
                            'tags': ['deployment', 'webapp', 'production'],
                            'help_text': 'Comprehensive deployment script with all available UI components',
                            'preview_mode': True
                        },
                        {
                            'name': 'database-operations',
                            'path': './demo-scripts/backup.sh',
                            'description': 'Database backup and maintenance',
                            'category': 'Database',
                            'args': [
                                {
                                    'name': 'operation',
                                    'type': 'radio',
                                    'description': 'Database operation',
                                    'choices': ['backup', 'restore', 'optimize', 'migrate'],
                                    'default': 'backup'
                                },
                                {
                                    'name': 'databases',
                                    'type': 'multi_select',
                                    'description': 'Target databases',
                                    'choices': ['users', 'products', 'orders', 'analytics', 'logs', 'cache'],
                                    'default': ['users', 'products']
                                },
                                {
                                    'name': 'compression_level',
                                    'type': 'slider',
                                    'description': 'Compression level',
                                    'min_value': 0,
                                    'max_value': 9,
                                    'step': 1,
                                    'default': 6
                                }
                            ],
                            'tags': ['database', 'backup', 'maintenance'],
                            'preview_mode': True
                        }
                    ],
                    'submenus': [
                        {
                            'name': 'System Administration',
                            'description': 'System management tools',
                            'scripts': [
                                {
                                    'name': 'service-control',
                                    'path': './demo-scripts/service.sh',
                                    'description': 'Advanced service management',
                                    'category': 'System',
                                    'args': [
                                        {
                                            'name': 'services',
                                            'type': 'multi_select',
                                            'description': 'Services to manage',
                                            'choices': ['nginx', 'apache2', 'mysql', 'postgresql', 'redis', 'mongodb', 'elasticsearch']
                                        },
                                        {
                                            'name': 'action',
                                            'type': 'radio',
                                            'description': 'Service action',
                                            'choices': ['start', 'stop', 'restart', 'reload', 'status', 'enable', 'disable'],
                                            'default': 'status'
                                        },
                                        {
                                            'name': 'force_operation',
                                            'type': 'checkbox',
                                            'description': 'Force operation',
                                            'default': False,
                                            'help_text': 'Use with caution!'
                                        },
                                        {
                                            'name': 'wait_time',
                                            'type': 'slider',
                                            'description': 'Wait time between operations (seconds)',
                                            'min_value': 0,
                                            'max_value': 60,
                                            'step': 5,
                                            'default': 10
                                        }
                                    ],
                                    'tags': ['system', 'services', 'admin'],
                                    'preview_mode': True
                                }
                            ]
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
            
        submenus = []
        for submenu_config in menu_config.get('submenus', []):
            submenu = self._parse_menu(submenu_config)
            submenus.append(submenu)
            
        return Menu(
            name=menu_config['name'],
            description=menu_config.get('description', ''),
            scripts=scripts,
            submenus=submenus
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
                file_extensions=arg_config.get('file_extensions'),
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
            
            # Add submenus
            if menu.submenus:
                self._add_submenu_nodes(menu_node, menu.submenus)

    def _add_submenu_nodes(self, parent_node, submenus):
        """Recursively add submenu nodes"""
        for submenu in submenus:
            submenu_node = parent_node.add(f"📁 {submenu.name}", data=submenu)
            
            for script in submenu.scripts:
                script_node = submenu_node.add(f"🚀 {script.name}", data=script)
            
            if submenu.submenus:
                self._add_submenu_nodes(submenu_node, submenu.submenus)

    def update_scripts_table(self):
        """Update the scripts data table"""
        table = self.query_one("#scripts-table", DataTable)
        table.clear(columns=True)
        
        # Add columns
        table.add_columns("Name", "Category", "Description", "Args", "Tags")
        
        # Add all scripts
        for menu in self.menus:
            self._add_scripts_to_table(table, menu.scripts)
            for submenu in menu.submenus:
                self._add_scripts_to_table(table, submenu.scripts)

    def _add_scripts_to_table(self, table, scripts):
        """Add scripts to the data table"""
        for script in scripts:
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
        
        stats_text = f"""
📊 Total Scripts: {total_scripts}
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
            log = self.query_one("#main-log", RichLog)
            
            if isinstance(data, Script):
                log.write(f"Selected script: {data.name}")
                # Could auto-show script details here
            elif isinstance(data, Menu):
                log.write(f"Selected menu: {data.name}")

    @on(DataTable.RowSelected)
    def on_script_selected(self, event: DataTable.RowSelected):
        """Handle script selection from table"""
        table = event.data_table
        script_name = str(table.get_row_at(event.cursor_row)[0])
        
        # Find the script
        script = self.find_script_by_name(script_name)
        if script:
            self.run_script(script)

    def find_script_by_name(self, name: str) -> Optional[Script]:
        """Find script by name across all menus"""
        for menu in self.menus:
            for script in menu.scripts:
                if script.name == name:
                    return script
            for submenu in menu.submenus:
                for script in submenu.scripts:
                    if script.name == name:
                        return script
        return None

    async def run_script(self, script: Script):
        """Run a script with argument collection"""
        if script.args:
            # Show argument collector
            def on_result(command):
                if command:
                    self.execute_command(command)
            
            self.push_screen(ArgumentCollectorScreen(script), on_result)
        else:
            # Execute directly
            command = [script.path]
            self.execute_command(command)

    def show_command_preview(self, command: List[str]):
        """Show command preview screen"""
        def on_result(result_command):
            if result_command:
                self.execute_command(result_command)
        
        self.push_screen(CommandPreviewScreen(command), on_result)

    def execute_command(self, command: List[str]):
        """Execute command with output display"""
        self.push_screen(ScriptExecutionScreen(command))

    def action_command_palette(self):
        """Open command palette"""
        self.action_command_palette()

    def action_reload_config(self):
        """Reload configuration"""
        async def reload():
            await self.load_config()
            self.build_menu_tree()
            self.update_scripts_table()
            self.update_statistics()
            
            log = self.query_one("#main-log", RichLog)
            log.write("✅ Configuration reloaded")
        
        self.run_worker(reload(), exclusive=True)

    def action_help(self):
        """Show help information"""
        help_text = """
# 🚀 Textual Script Runner Help

## Keyboard Shortcuts
- **Ctrl+P**: Open Command Palette
- **Ctrl+R**: Reload Configuration  
- **Ctrl+Q**: Quit Application
- **F1**: Show this help

## Command Palette Features
- Type to search scripts and commands
- Execute scripts directly
- Filter by tags
- Quick navigation

## UI Components Demo
- **Radio Buttons**: Single selection
- **Checkboxes**: On/off toggles  
- **Multi-Select**: Multiple choices
- **Sliders**: Numeric ranges
- **Text Input**: Free form text
- **File Selection**: Path inputs

## Navigation
- Use mouse or keyboard to navigate
- Click on scripts to execute
- Use tabs to switch views
"""
        
        self.push_screen(
            ModalScreen(
                Container(
                    Markdown(help_text),
                    Button("Close", id="close"),
                    classes="dialog"
                )
            )
        )

def main():
    parser = argparse.ArgumentParser(description="Textual Script Runner with Command Palette")
    parser.add_argument("--config", "-c", default="~/.config/textual-script-runner.yaml",
                       help="Configuration file path")
    parser.add_argument("--demo", action="store_true",
                       help="Create demo configuration and exit")
    
    args = parser.parse_args()
    
    if not HAS_TEXTUAL:
        print("Textual not available. Install with: pip install textual")
        sys.exit(1)
    
    app = TextualScriptRunner(args.config)
    
    if args.demo:
        import asyncio
        asyncio.run(app.create_demo_config())
        print(f"Demo configuration created at: {app.config_path}")
        print("Run without --demo to start the application")
    else:
        app.run()

if __name__ == "__main__":
    main()