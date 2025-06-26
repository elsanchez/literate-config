#!/usr/bin/env python3
"""
Textual Script Runner - Interactive Version
With embedded terminal, clickable links, and live interaction
"""

import os
import sys
import json
import yaml
import subprocess
import asyncio
import pty
import select
import termios
import tty
from pathlib import Path
from dataclasses import dataclass, field
from typing import List, Dict, Optional, Any, Union
import argparse
import webbrowser

from textual.app import App, ComposeResult
from textual.containers import Container, Horizontal, Vertical, ScrollableContainer
from textual.widgets import (
    Header, Footer, Button, Input, Label, Tree, Static, 
    Checkbox, Select, DataTable, TabbedContent, TabPane,
    Switch, Log, TextArea, Markdown
)
from textual.screen import Screen, ModalScreen
from textual.binding import Binding
from textual import on, work
from rich.text import Text
from rich.panel import Panel
from rich.syntax import Syntax
from rich.console import Console

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
    interactive: bool = False  # New: support interactive scripts

class InteractiveTerminalScreen(ModalScreen):
    """Modal screen with embedded terminal for interactive script execution"""
    
    BINDINGS = [
        Binding("escape", "dismiss", "Close Terminal"),
        Binding("ctrl+c", "interrupt", "Interrupt"),
        Binding("ctrl+d", "end_input", "End Input"),
    ]
    
    def __init__(self, command: List[str], **kwargs):
        super().__init__(**kwargs)
        self.command = command
        self.process = None
        self.master_fd = None

    def compose(self) -> ComposeResult:
        yield Container(
            Container(
                Label(f"🖥️ Interactive Terminal: {' '.join(self.command)}", classes="title"),
                classes="header"
            ),
            Container(
                Log(id="terminal-output", classes="terminal-log"),
                Input(id="terminal-input", placeholder="Type commands here...", classes="terminal-input"),
                classes="terminal-container"
            ),
            Container(
                Button("Send Input", variant="default", id="send"),
                Button("Interrupt (Ctrl+C)", variant="error", id="interrupt"),
                Button("Close Terminal", variant="default", id="close"),
                classes="terminal-controls"
            ),
            classes="interactive-dialog"
        )

    async def on_mount(self):
        """Start the interactive process when screen mounts"""
        log = self.query_one("#terminal-output", Log)
        log.write_line(f"🚀 Starting interactive terminal: {' '.join(self.command)}")
        
        try:
            # Create pseudo-terminal
            self.master_fd, slave_fd = pty.openpty()
            
            # Start process with pseudo-terminal
            self.process = subprocess.Popen(
                self.command,
                stdin=slave_fd,
                stdout=slave_fd,
                stderr=slave_fd,
                preexec_fn=os.setsid
            )
            
            # Close slave fd in parent
            os.close(slave_fd)
            
            # Start reading output
            self.start_reading_output()
            
        except Exception as e:
            log.write_line(f"❌ Error starting terminal: {e}")

    def start_reading_output(self):
        """Start a worker to read terminal output"""
        @work(exclusive=False)
        async def read_output():
            log = self.query_one("#terminal-output", Log)
            
            while self.process and self.process.poll() is None:
                try:
                    # Check if data is available
                    ready, _, _ = select.select([self.master_fd], [], [], 0.1)
                    
                    if ready:
                        data = os.read(self.master_fd, 1024).decode('utf-8', errors='ignore')
                        if data:
                            # Process output line by line
                            for line in data.split('\n'):
                                if line.strip():
                                    log.write_line(line.rstrip())
                
                except OSError:
                    break
                except Exception as e:
                    log.write_line(f"Error reading output: {e}")
                    break
            
            # Process finished
            if self.process:
                log.write_line(f"🏁 Process finished with exit code: {self.process.returncode}")
        
        self.run_worker(read_output())

    @on(Button.Pressed, "#send")
    def send_input(self):
        """Send input to the terminal"""
        input_widget = self.query_one("#terminal-input", Input)
        text = input_widget.value
        
        if text and self.master_fd and self.process and self.process.poll() is None:
            try:
                # Send input to process
                os.write(self.master_fd, (text + '\n').encode())
                
                # Show what we sent
                log = self.query_one("#terminal-output", Log)
                log.write_line(f">>> {text}")
                
                # Clear input
                input_widget.value = ""
                
            except Exception as e:
                log = self.query_one("#terminal-output", Log)
                log.write_line(f"❌ Error sending input: {e}")

    @on(Input.Submitted, "#terminal-input")
    def on_input_submitted(self):
        """Handle Enter key in input"""
        self.send_input()

    @on(Button.Pressed, "#interrupt")
    def interrupt_process(self):
        """Send interrupt signal to process"""
        if self.process and self.process.poll() is None:
            try:
                # Send Ctrl+C
                os.write(self.master_fd, b'\x03')
                log = self.query_one("#terminal-output", Log)
                log.write_line("🛑 Sent interrupt signal (Ctrl+C)")
            except Exception as e:
                log = self.query_one("#terminal-output", Log)
                log.write_line(f"❌ Error sending interrupt: {e}")

    @on(Button.Pressed, "#close")
    def close_terminal(self):
        """Close the terminal"""
        self.cleanup_and_dismiss()

    def action_interrupt(self):
        """Handle Ctrl+C binding"""
        self.interrupt_process()

    def action_dismiss(self):
        """Handle escape key"""
        self.cleanup_and_dismiss()

    def cleanup_and_dismiss(self):
        """Clean up resources and dismiss"""
        if self.process and self.process.poll() is None:
            try:
                self.process.terminate()
            except:
                pass
        
        if self.master_fd:
            try:
                os.close(self.master_fd)
            except:
                pass
        
        self.dismiss()

class HyperlinkScreen(ModalScreen):
    """Modal screen for displaying content with clickable hyperlinks"""
    
    BINDINGS = [
        Binding("escape", "dismiss", "Close"),
    ]
    
    def __init__(self, title: str, content: str, **kwargs):
        super().__init__(**kwargs)
        self.title = title
        self.content = content

    def compose(self) -> ComposeResult:
        # Create markdown with hyperlinks
        markdown_content = f"""# {self.title}

{self.content}

## Quick Links

- [📁 Open Script Directory](file:///home/elsanchez/org/literate-config/examples/menus/demo-scripts)
- [📖 Textual Documentation](https://textual.textualize.io/)
- [🐍 Rich Documentation](https://rich.readthedocs.io/)
- [⚙️ Configuration File](file:///home/elsanchez/.config/textual-script-runner.yaml)
- [🔍 Project Repository](https://github.com/textualize/textual)

## Actions

Click any link above to open in your default application!
"""
        
        yield Container(
            Markdown(markdown_content, classes="hyperlink-content"),
            Button("Close", variant="default", id="close"),
            classes="hyperlink-dialog"
        )

    @on(Button.Pressed, "#close")
    def close_screen(self):
        self.dismiss()

class ScriptConfigScreen(Screen):
    """Screen for configuring script arguments with enhanced features"""
    
    BINDINGS = [
        Binding("escape", "app.pop_screen", "Back"),
    ]
    
    def __init__(self, script, **kwargs):
        super().__init__(**kwargs)
        self.script = script
        self.collected_values = {}

    def compose(self) -> ComposeResult:
        yield Header()
        
        with ScrollableContainer():
            yield Label(f"🚀 Configure: {self.script.name}", classes="title")
            yield Label(self.script.description, classes="subtitle")
            
            if self.script.help_text:
                yield Label(self.script.help_text, classes="help")
            
            # Show clickable script path
            script_path_text = Text()
            script_path_text.append("📂 Script: ")
            script_path_text.append(str(self.script.path), style="link")
            yield Static(script_path_text, id="script-path")
            
            # Build argument inputs
            for arg in self.script.args:
                with Container(classes="arg-container"):
                    yield Label(f"{arg.name}: {arg.description}")
                    if arg.help_text:
                        yield Label(f"💡 {arg.help_text}", classes="arg-help")
                    
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
            
            # Execution mode selection
            with Container(classes="execution-container"):
                yield Label("🎯 Execution Mode:", classes="section-title")
                yield Select(
                    [
                        ("preview", "🔍 Preview Only"),
                        ("background", "🏃 Run in Background"),
                        ("interactive", "🖥️ Interactive Terminal"),
                        ("output", "📄 Capture Output")
                    ],
                    value="preview",
                    id="execution-mode"
                )
            
            # Buttons
            with Horizontal(classes="button-container"):
                yield Button("Cancel", variant="error", id="cancel")
                yield Button("Preview Command", variant="default", id="preview")
                yield Button("Execute", variant="success", id="execute")
                yield Button("Help & Links", variant="default", id="help")
        
        yield Footer()

    @on(Static.Click, "#script-path")
    def open_script_file(self):
        """Handle click on script path - open file"""
        try:
            # Try to open file in default editor
            subprocess.run(["xdg-open", str(self.script.path)], check=False)
            
            log = self.app.query_one("#main-log", Log)
            log.write_line(f"🔗 Opened script file: {self.script.path}")
        except Exception as e:
            log = self.app.query_one("#main-log", Log)
            log.write_line(f"❌ Could not open file: {e}")

    @on(Button.Pressed, "#cancel")
    def cancel_config(self):
        self.app.pop_screen()

    @on(Button.Pressed, "#preview")
    def preview_command(self):
        command = self.build_command()
        command_text = " ".join(command)
        
        # Show in log with clickable elements
        main_screen = self.app.screen_stack[0]
        log = main_screen.query_one("#main-log", Log)
        
        # Create rich text with clickable command
        preview_text = Text()
        preview_text.append("🔍 Preview: ")
        preview_text.append(command_text, style="bold cyan")
        
        log.write(preview_text)

    @on(Button.Pressed, "#execute")
    def execute_script(self):
        command = self.build_command()
        execution_mode = self.query_one("#execution-mode", Select).value
        
        if execution_mode == "interactive":
            # Open interactive terminal
            self.app.push_screen(InteractiveTerminalScreen(command))
        else:
            # Regular execution
            self.execute_command(command, execution_mode)
        
        self.app.pop_screen()

    @on(Button.Pressed, "#help")
    def show_help(self):
        """Show help screen with hyperlinks"""
        help_content = f"""
## Script Information

**Name:** {self.script.name}
**Path:** `{self.script.path}`
**Category:** {self.script.category}
**Tags:** {', '.join(self.script.tags)}

## Description

{self.script.description}

{self.script.help_text}

## Arguments

"""
        
        for arg in self.script.args:
            help_content += f"- **{arg.name}** ({arg.type}): {arg.description}\n"
            if arg.help_text:
                help_content += f"  💡 {arg.help_text}\n"
        
        self.app.push_screen(HyperlinkScreen("Script Help", help_content))

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

    def execute_command(self, command: List[str], mode: str):
        """Execute the command with specified mode"""
        main_screen = self.app.screen_stack[0]
        log = main_screen.query_one("#main-log", Log)
        
        command_text = " ".join(command)
        log.write_line(f"🚀 Executing ({mode}): {command_text}")
        
        try:
            if mode == "preview" or self.script.preview_mode:
                log.write_line("⚠️  Preview mode - simulating execution")
                log.write_line("✅ Script would execute successfully")
                
            elif mode == "background":
                # Run in background
                process = subprocess.Popen(command)
                log.write_line(f"🏃 Started in background (PID: {process.pid})")
                
            elif mode == "output":
                # Capture output
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

class TextualInteractiveRunner(App):
    """Main Textual application with interactive features"""
    
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
    
    .section-title {
        text-style: bold;
        color: $warning;
        margin-top: 1;
    }
    
    .arg-container {
        border: solid $primary-lighten-2;
        margin: 1;
        padding: 1;
    }
    
    .arg-help {
        color: $text-muted;
        text-style: italic;
        margin-bottom: 1;
    }
    
    .execution-container {
        margin: 1;
        padding: 1;
        border: solid $accent;
    }
    
    .button-container {
        margin: 1;
    }
    
    .interactive-dialog {
        align: center middle;
        width: 90%;
        height: 80%;
        background: $surface;
        border: thick $primary;
        padding: 1;
    }
    
    .hyperlink-dialog {
        align: center middle;
        width: 80%;
        height: 70%;
        background: $surface;
        border: thick $accent;
        padding: 1;
    }
    
    .terminal-container {
        height: 1fr;
        margin: 1;
    }
    
    .terminal-log {
        height: 1fr;
        border: solid $success;
        margin-bottom: 1;
    }
    
    .terminal-input {
        margin-bottom: 1;
    }
    
    .terminal-controls {
        height: auto;
    }
    
    .hyperlink-content {
        height: 1fr;
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
        Binding("ctrl+t", "open_terminal", "Terminal"),
        Binding("ctrl+l", "show_links", "Links"),
        Binding("f1", "help", "Help"),
    ]
    
    TITLE = "🚀 Textual Interactive Script Runner"
    SUB_TITLE = "With Terminal, Hyperlinks & Live Interaction"
    
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
                    
                    with TabPane("Interactive", id="interactive"):
                        yield Container(
                            Label("🖥️ Interactive Features", classes="title"),
                            Button("🖥️ Open System Terminal", id="sys-terminal"),
                            Button("🔗 Show Hyperlinks Demo", id="links-demo"),
                            Button("📂 Open Script Directory", id="open-dir"),
                            Button("⚙️ Open Config File", id="open-config"),
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
        
        # Welcome message with clickable elements
        log = self.query_one("#main-log", Log)
        
        welcome_text = Text()
        welcome_text.append("🚀 Textual Interactive Script Runner\n", style="bold blue")
        welcome_text.append("=====================================\n\n")
        welcome_text.append("✨ New Interactive Features:\n", style="bold")
        welcome_text.append("   🖥️ Embedded Terminal - Run scripts interactively\n")
        welcome_text.append("   🔗 Clickable Hyperlinks - Open files and URLs\n")
        welcome_text.append("   📂 File System Integration - Click paths to open\n")
        welcome_text.append("   ⌨️ Live Input/Output - Real-time script interaction\n\n")
        welcome_text.append("🎯 Try:\n", style="bold")
        welcome_text.append("   • Click script paths to open in editor\n")
        welcome_text.append("   • Use 'Interactive Terminal' execution mode\n")
        welcome_text.append("   • Check 'Interactive' tab for demos\n")
        welcome_text.append("   • Ctrl+T for system terminal\n")
        welcome_text.append("   • Ctrl+L for hyperlinks demo\n\n")
        
        log.write(welcome_text)

    async def load_config(self):
        """Load configuration from YAML file"""
        if not self.config_path.exists():
            await self.create_demo_config()
            
        with open(self.config_path) as f:
            config = yaml.safe_load(f)
            
        self.menus = self._parse_config(config)

    async def create_demo_config(self):
        """Create demo configuration with interactive scripts"""
        demo_config = {
            'menus': [
                {
                    'name': 'Interactive Demo Scripts',
                    'description': 'Scripts demonstrating interactive features',
                    'scripts': [
                        {
                            'name': 'interactive-deploy',
                            'path': './demo-scripts/deploy.sh',
                            'description': 'Interactive deployment with terminal',
                            'category': 'Deployment',
                            'interactive': True,
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
                                    'name': 'interactive_mode',
                                    'type': 'checkbox',
                                    'description': 'Enable interactive mode',
                                    'default': True,
                                    'help_text': 'Run with embedded terminal'
                                },
                                {
                                    'name': 'server_name',
                                    'type': 'text',
                                    'description': 'Target server hostname',
                                    'default': 'localhost',
                                    'help_text': 'Click script path above to edit'
                                }
                            ],
                            'tags': ['deployment', 'interactive'],
                            'help_text': 'Try the Interactive Terminal execution mode!',
                            'preview_mode': True
                        },
                        {
                            'name': 'file-browser',
                            'path': './demo-scripts/backup.sh',
                            'description': 'File browser with clickable paths',
                            'category': 'Files',
                            'args': [
                                {
                                    'name': 'directory',
                                    'type': 'text',
                                    'description': 'Directory to browse',
                                    'default': '/home/elsanchez/org/literate-config',
                                    'help_text': 'Path will be clickable in output'
                                }
                            ],
                            'tags': ['files', 'browser'],
                            'preview_mode': True
                        },
                        {
                            'name': 'quick-command',
                            'path': './demo-scripts/service.sh',
                            'description': 'Quick command (no args)',
                            'category': 'System',
                            'args': [],
                            'tags': ['simple', 'quick'],
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
            category=script_config.get('category', 'General'),
            interactive=script_config.get('interactive', False)
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
                    icon = "🖥️" if script.interactive else "🚀"
                    script_node = menu_node.add(f"{icon} {script.name}", data=script)

    def update_scripts_table(self):
        """Update the scripts data table"""
        table = self.query_one("#scripts-table", DataTable)
        table.clear(columns=True)
        
        # Add columns
        table.add_columns("Name", "Category", "Description", "Mode", "Tags")
        
        # Add all scripts
        for menu in self.menus:
            for script in menu.scripts:
                tags_str = ", ".join(script.tags) if script.tags else ""
                mode = "Interactive" if script.interactive else "Standard"
                
                table.add_row(
                    script.name,
                    script.category,
                    script.description,
                    mode,
                    tags_str,
                    key=script.name
                )

    def update_statistics(self):
        """Update statistics display"""
        total_scripts = sum(len(menu.scripts) for menu in self.menus)
        total_menus = len(self.menus)
        interactive_scripts = sum(1 for menu in self.menus for script in menu.scripts if script.interactive)
        
        # Count by category
        categories = {}
        for menu in self.menus:
            for script in menu.scripts:
                cat = script.category
                categories[cat] = categories.get(cat, 0) + 1
        
        stats_text = f"""📊 Total Scripts: {total_scripts}
📁 Total Menus: {total_menus}
🖥️ Interactive Scripts: {interactive_scripts}

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
                if data.args:
                    log.write_line(f"   Opening config screen with {len(data.args)} arguments")
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
                self.push_screen(ScriptConfigScreen(script))
            else:
                self.execute_simple_script(script)

    @on(Button.Pressed, "#sys-terminal")
    def open_system_terminal(self):
        """Open system terminal"""
        try:
            subprocess.Popen(['gnome-terminal'], start_new_session=True)
            log = self.query_one("#main-log", Log)
            log.write_line("🖥️ Opened system terminal")
        except Exception as e:
            log = self.query_one("#main-log", Log)
            log.write_line(f"❌ Could not open terminal: {e}")

    @on(Button.Pressed, "#links-demo")
    def show_links_demo(self):
        """Show hyperlinks demonstration"""
        demo_content = """
This is a demonstration of clickable hyperlinks and interactive features.

## File System Links

These paths are clickable in some terminal emulators:

- Configuration: `/home/elsanchez/.config/textual-script-runner.yaml`
- Script Directory: `/home/elsanchez/org/literate-config/examples/menus/demo-scripts`
- Project Root: `/home/elsanchez/org/literate-config`

## Web Links

- Textual Documentation: https://textual.textualize.io/
- Rich Documentation: https://rich.readthedocs.io/
- Python Official: https://python.org

## Interactive Features

Try these features:
1. Use 'Interactive Terminal' execution mode
2. Click on script paths in configuration screens
3. Use Ctrl+T to open system terminal
4. Check the logs for clickable elements
"""
        self.push_screen(HyperlinkScreen("Hyperlinks Demo", demo_content))

    @on(Button.Pressed, "#open-dir")
    def open_script_directory(self):
        """Open script directory in file manager"""
        try:
            script_dir = Path("./demo-scripts").resolve()
            subprocess.run(['xdg-open', str(script_dir)], check=False)
            log = self.query_one("#main-log", Log)
            log.write_line(f"📂 Opened directory: {script_dir}")
        except Exception as e:
            log = self.query_one("#main-log", Log)
            log.write_line(f"❌ Could not open directory: {e}")

    @on(Button.Pressed, "#open-config")
    def open_config_file(self):
        """Open configuration file in editor"""
        try:
            subprocess.run(['xdg-open', str(self.config_path)], check=False)
            log = self.query_one("#main-log", Log)
            log.write_line(f"⚙️ Opened config: {self.config_path}")
        except Exception as e:
            log = self.query_one("#main-log", Log)
            log.write_line(f"❌ Could not open config: {e}")

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
        
        if script.interactive:
            # Open interactive terminal for interactive scripts
            self.push_screen(InteractiveTerminalScreen([script.path]))
        else:
            # Regular execution
            try:
                if script.preview_mode:
                    log.write_line("⚠️  Preview mode - simulating execution")
                    log.write_line("✅ Script would execute successfully")
                    
                    # Show clickable path
                    path_text = Text()
                    path_text.append("📂 Script path: ")
                    path_text.append(str(script.path), style="link")
                    log.write(path_text)
                    
                else:
                    result = subprocess.run([script.path], capture_output=True, text=True, timeout=30)
                    
                    if result.returncode == 0:
                        log.write_line("✅ Script completed successfully")
                        if result.stdout:
                            for line in result.stdout.strip().split('\n'):
                                log.write_line(f"   {line}")
                    else:
                        log.write_line(f"❌ Script failed (exit code: {result.returncode})")
                        
            except Exception as e:
                log.write_line(f"❌ Error executing script: {e}")

    def action_open_terminal(self):
        """Open terminal via keybinding"""
        self.open_system_terminal()

    def action_show_links(self):
        """Show links demo via keybinding"""
        self.show_links_demo()

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
        help_content = """
## Interactive Features

### 🖥️ Embedded Terminal
- Select 'Interactive Terminal' execution mode
- Real-time input/output with running scripts
- Send commands while script is running
- Interrupt with Ctrl+C

### 🔗 Clickable Elements
- Script paths in configuration screens
- File paths in output
- Hyperlinks in help screens
- Directory links

### ⌨️ Keyboard Shortcuts
- **Ctrl+T** - Open System Terminal
- **Ctrl+L** - Show Hyperlinks Demo
- **Ctrl+R** - Reload Configuration
- **Ctrl+Q** - Quit Application
- **F1** - Show this help

### 🎯 Interactive Scripts
Scripts marked as 'Interactive' automatically open in embedded terminal.
Regular scripts can still use interactive mode by selecting it.

### 📂 File Integration
Click on file paths to open in default editor.
Use the Interactive tab for quick file operations.
"""
        self.push_screen(HyperlinkScreen("Interactive Help", help_content))

def main():
    parser = argparse.ArgumentParser(description="Textual Interactive Script Runner")
    parser.add_argument("--config", "-c", default="~/.config/textual-script-runner.yaml",
                       help="Configuration file path")
    parser.add_argument("--demo", action="store_true",
                       help="Create demo configuration and exit")
    
    args = parser.parse_args()
    
    app = TextualInteractiveRunner(args.config)
    
    if args.demo:
        import asyncio
        asyncio.run(app.create_demo_config())
        print(f"✅ Interactive demo configuration created at: {app.config_path}")
        print("🚀 Run without --demo to start the application:")
        print(f"   python3 {sys.argv[0]}")
        print("")
        print("🎯 New Interactive Features:")
        print("   🖥️ Embedded Terminal - Real-time script interaction")
        print("   🔗 Clickable Hyperlinks - Open files and URLs")
        print("   📂 File System Integration - Click paths to open")
        print("   ⌨️ Live Input/Output - Send commands to running scripts")
    else:
        app.run()

if __name__ == "__main__":
    main()