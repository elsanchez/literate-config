#!/usr/bin/env python3
"""
Interactive Script Runner - Enhanced TUI with Rich Components
Supports checkboxes, radio buttons, select lists, and more
"""

import os
import sys
import json
import yaml
import subprocess
from pathlib import Path
from dataclasses import dataclass, field
from typing import List, Dict, Optional, Any, Union
import argparse

try:
    import rich
    from rich.console import Console
    from rich.table import Table
    from rich.panel import Panel
    from rich.tree import Tree
    from rich.prompt import Prompt, Confirm, IntPrompt
    from rich.syntax import Syntax
    from rich.layout import Layout
    from rich.columns import Columns
    from rich.text import Text
    from rich.align import Align
    from rich.progress import Progress, SpinnerColumn, TextColumn
    from rich.rule import Rule
    from rich.markup import escape
    HAS_RICH = True
except ImportError:
    HAS_RICH = False
    print("Install rich for better UI: pip install rich")

@dataclass
class ScriptArg:
    name: str
    description: str
    type: str = "text"  # text, select, multi_select, checkbox, radio, number, file, directory
    required: bool = True
    default: Optional[Union[str, List[str], bool, int]] = None
    choices: Optional[List[str]] = None
    min_value: Optional[int] = None
    max_value: Optional[int] = None
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
    preview_mode: bool = False  # Show what would be executed

@dataclass
class Menu:
    name: str
    description: str
    scripts: List[Script] = field(default_factory=list)
    submenus: List['Menu'] = field(default_factory=list)
    
class InteractiveScriptRunner:
    def __init__(self, config_path: str = "~/.config/interactive-script-runner.yaml"):
        self.config_path = Path(config_path).expanduser()
        self.console = Console() if HAS_RICH else None
        self.menus: List[Menu] = []
        self.current_menu = None
        self.menu_stack = []
        self.collected_args = {}
        
    def load_config(self):
        """Load configuration from YAML file"""
        if not self.config_path.exists():
            self.create_demo_config()
            
        with open(self.config_path) as f:
            config = yaml.safe_load(f)
            
        self.menus = self._parse_config(config)
        
    def create_demo_config(self):
        """Create demo configuration with all interactive components"""
        demo_config = {
            'menus': [
                {
                    'name': 'Interactive Demo',
                    'description': 'Demonstration of all interactive components',
                    'scripts': [
                        {
                            'name': 'webapp-deploy',
                            'path': './demo-scripts/deploy.sh',
                            'description': 'Deploy web application with comprehensive options',
                            'args': [
                                {
                                    'name': 'environment',
                                    'type': 'radio',
                                    'description': 'Target deployment environment',
                                    'choices': ['development', 'staging', 'production'],
                                    'default': 'development',
                                    'help_text': 'Choose the target environment for deployment'
                                },
                                {
                                    'name': 'features',
                                    'type': 'multi_select',
                                    'description': 'Features to enable',
                                    'choices': ['auth', 'analytics', 'caching', 'cdn', 'monitoring'],
                                    'default': ['auth', 'monitoring'],
                                    'help_text': 'Select multiple features to enable'
                                },
                                {
                                    'name': 'backup',
                                    'type': 'checkbox',
                                    'description': 'Create backup before deployment',
                                    'default': True,
                                    'help_text': 'Recommended for production deployments'
                                },
                                {
                                    'name': 'parallel_jobs',
                                    'type': 'number',
                                    'description': 'Number of parallel deployment jobs',
                                    'default': 4,
                                    'min_value': 1,
                                    'max_value': 16,
                                    'help_text': 'Higher values = faster deployment but more resource usage'
                                },
                                {
                                    'name': 'config_file',
                                    'type': 'file',
                                    'description': 'Configuration file path',
                                    'file_extensions': ['.yaml', '.yml', '.json'],
                                    'required': False,
                                    'help_text': 'Optional custom configuration file'
                                },
                                {
                                    'name': 'notification_channels',
                                    'type': 'select',
                                    'description': 'Notification method',
                                    'choices': ['email', 'slack', 'discord', 'webhook', 'none'],
                                    'default': 'email',
                                    'help_text': 'How to receive deployment notifications'
                                }
                            ],
                            'tags': ['deployment', 'webapp', 'demo'],
                            'help_text': 'This is a comprehensive deployment script that demonstrates all interactive components',
                            'preview_mode': True
                        },
                        {
                            'name': 'database-backup',
                            'path': './demo-scripts/backup.sh',
                            'description': 'Database backup with options',
                            'args': [
                                {
                                    'name': 'databases',
                                    'type': 'multi_select',
                                    'description': 'Databases to backup',
                                    'choices': ['users', 'products', 'orders', 'analytics', 'logs'],
                                    'default': ['users', 'products', 'orders'],
                                    'help_text': 'Select which databases to include in backup'
                                },
                                {
                                    'name': 'compression',
                                    'type': 'radio',
                                    'description': 'Compression method',
                                    'choices': ['gzip', 'bzip2', 'xz', 'none'],
                                    'default': 'gzip',
                                    'help_text': 'gzip = fast, xz = best compression'
                                },
                                {
                                    'name': 'verify_backup',
                                    'type': 'checkbox',
                                    'description': 'Verify backup integrity',
                                    'default': True
                                },
                                {
                                    'name': 'retention_days',
                                    'type': 'number',
                                    'description': 'Backup retention (days)',
                                    'default': 30,
                                    'min_value': 1,
                                    'max_value': 365
                                }
                            ],
                            'tags': ['database', 'backup', 'demo'],
                            'preview_mode': True
                        }
                    ],
                    'submenus': [
                        {
                            'name': 'System Tools',
                            'description': 'System administration tools',
                            'scripts': [
                                {
                                    'name': 'service-manager',
                                    'path': './demo-scripts/service.sh',
                                    'description': 'Manage system services',
                                    'args': [
                                        {
                                            'name': 'services',
                                            'type': 'multi_select',
                                            'description': 'Services to manage',
                                            'choices': ['nginx', 'apache2', 'mysql', 'postgresql', 'redis', 'mongodb'],
                                            'help_text': 'Select services to operate on'
                                        },
                                        {
                                            'name': 'action',
                                            'type': 'radio',
                                            'description': 'Action to perform',
                                            'choices': ['start', 'stop', 'restart', 'status', 'enable', 'disable'],
                                            'default': 'status'
                                        },
                                        {
                                            'name': 'force',
                                            'type': 'checkbox',
                                            'description': 'Force operation (use with caution)',
                                            'default': False
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
            preview_mode=script_config.get('preview_mode', False)
        )
    
    def show_main_menu(self):
        """Display main menu with enhanced UI"""
        if not HAS_RICH:
            self._show_simple_menu()
            return
            
        while True:
            self.console.clear()
            
            # Create header
            self.console.print(Panel(
                Align.center(
                    Text("🚀 Interactive Script Runner", style="bold blue") + 
                    Text("\nAdvanced TUI with Rich Components", style="dim")
                ),
                style="bold white on blue",
                padding=(1, 2)
            ))
            
            # Show current menu or main menu
            if self.current_menu:
                self._show_current_menu()
            else:
                self._show_main_menu_content()
            
            # Show commands
            self._show_commands()
            
            # Get user input
            cmd = self._get_user_command()
            
            if cmd == 'q':
                self.console.print("👋 Goodbye!", style="bold green")
                break
            elif cmd == 'r':
                self._run_script_interactive()
            elif cmd == 'b' and self.current_menu:
                self._go_back()
            elif cmd.isdigit():
                self._select_menu_item(int(cmd))
    
    def _show_current_menu(self):
        """Show current menu contents"""
        menu = self.current_menu
        
        # Menu header
        self.console.print(f"\n📁 [bold]{menu.name}[/bold]")
        if menu.description:
            self.console.print(f"   [dim]{menu.description}[/dim]")
        
        # Breadcrumb
        breadcrumb = " > ".join([m.name for m in self.menu_stack] + [menu.name])
        self.console.print(f"   [dim]Path: {breadcrumb}[/dim]\n")
        
        # Scripts table
        if menu.scripts:
            scripts_table = Table(show_header=True, header_style="bold magenta")
            scripts_table.add_column("#", style="cyan", width=3)
            scripts_table.add_column("Script", style="bold")
            scripts_table.add_column("Description", style="dim")
            scripts_table.add_column("Tags", style="blue")
            scripts_table.add_column("Args", style="yellow")
            
            for i, script in enumerate(menu.scripts):
                tags_str = ", ".join(script.tags) if script.tags else ""
                args_str = f"{len(script.args)} args" if script.args else "No args"
                scripts_table.add_row(
                    str(i + 1),
                    script.name,
                    script.description,
                    tags_str,
                    args_str
                )
            
            self.console.print(Panel(scripts_table, title="📄 Scripts"))
        
        # Submenus
        if menu.submenus:
            start_idx = len(menu.scripts) + 1
            submenus_table = Table(show_header=True, header_style="bold green")
            submenus_table.add_column("#", style="cyan", width=3)
            submenus_table.add_column("Submenu", style="bold")
            submenus_table.add_column("Description", style="dim")
            
            for i, submenu in enumerate(menu.submenus):
                submenus_table.add_row(
                    str(start_idx + i),
                    submenu.name,
                    submenu.description
                )
            
            self.console.print(Panel(submenus_table, title="📁 Submenus"))
    
    def _show_main_menu_content(self):
        """Show main menu selection"""
        table = Table(show_header=True, header_style="bold magenta")
        table.add_column("#", style="cyan", width=3)
        table.add_column("Menu", style="bold")
        table.add_column("Description", style="dim")
        table.add_column("Scripts", style="yellow")
        
        for i, menu in enumerate(self.menus):
            script_count = self._count_all_scripts(menu)
            table.add_row(
                str(i + 1),
                menu.name,
                menu.description,
                f"{script_count} scripts"
            )
        
        self.console.print(Panel(table, title="📁 Available Menus"))
    
    def _count_all_scripts(self, menu: Menu) -> int:
        """Count all scripts in menu and submenus"""
        count = len(menu.scripts)
        for submenu in menu.submenus:
            count += self._count_all_scripts(submenu)
        return count
    
    def _show_commands(self):
        """Show available commands"""
        commands = []
        if self.current_menu:
            commands.extend(["[cyan]1-9[/cyan] Select item", "[cyan]r[/cyan]un script", "[cyan]b[/cyan]ack"])
        else:
            commands.extend(["[cyan]1-9[/cyan] Select menu"])
        
        commands.append("[cyan]q[/cyan]uit")
        
        self.console.print(Panel(
            " • ".join(commands),
            title="Commands"
        ))
    
    def _get_user_command(self) -> str:
        """Get user command with validation"""
        valid_commands = ['r', 'q']
        if self.current_menu:
            valid_commands.append('b')
            total_items = len(self.current_menu.scripts) + len(self.current_menu.submenus)
            valid_commands.extend([str(i) for i in range(1, total_items + 1)])
        else:
            valid_commands.extend([str(i) for i in range(1, len(self.menus) + 1)])
        
        while True:
            cmd = Prompt.ask("\n🎯 Choose", choices=valid_commands + ['?'])
            if cmd == '?':
                self.console.print("Available commands: " + ", ".join(valid_commands))
                continue
            return cmd
    
    def _select_menu_item(self, choice: int):
        """Select menu item by number"""
        if self.current_menu:
            if choice <= len(self.current_menu.scripts):
                # Selected a script
                script = self.current_menu.scripts[choice - 1]
                self._execute_script(script)
            else:
                # Selected a submenu
                submenu_idx = choice - len(self.current_menu.scripts) - 1
                if submenu_idx < len(self.current_menu.submenus):
                    self.menu_stack.append(self.current_menu)
                    self.current_menu = self.current_menu.submenus[submenu_idx]
        else:
            # Selected a main menu
            if choice <= len(self.menus):
                self.current_menu = self.menus[choice - 1]
    
    def _execute_script(self, script: Script):
        """Execute script with interactive argument collection"""
        self.console.clear()
        self.console.print(Panel(
            f"[bold blue]{script.name}[/bold blue]\n{script.description}",
            title="🚀 Script Execution"
        ))
        
        if script.help_text:
            self.console.print(Panel(script.help_text, title="ℹ️  Help"))
        
        # Collect arguments interactively
        args = []
        self.collected_args = {}
        
        for arg in script.args:
            value = self._collect_argument(arg)
            if value is not None:
                self.collected_args[arg.name] = value
                args.extend(self._format_argument(arg, value))
        
        # Build command
        cmd = [script.path] + args
        cmd_str = " ".join(str(x) for x in cmd)
        
        # Show command preview
        self.console.print(Panel(
            Syntax(cmd_str, "bash", theme="monokai", line_numbers=False),
            title="🔍 Command Preview"
        ))
        
        # Show collected arguments summary
        if self.collected_args:
            args_table = Table(show_header=True, header_style="bold cyan")
            args_table.add_column("Argument", style="bold")
            args_table.add_column("Value", style="green")
            
            for arg_name, arg_value in self.collected_args.items():
                args_table.add_row(arg_name, str(arg_value))
            
            self.console.print(Panel(args_table, title="📋 Collected Arguments"))
        
        # Execute or preview
        if script.preview_mode:
            self.console.print("[yellow]⚠️  Preview mode - script will not be executed[/yellow]")
        else:
            if Confirm.ask("🎯 Execute this command?"):
                self._run_command(cmd, script.async_exec)
        
        Prompt.ask("\n⏎ Press Enter to continue")
    
    def _collect_argument(self, arg: ScriptArg) -> Any:
        """Collect argument value based on type"""
        self.console.print(f"\n📝 [bold]{arg.name}[/bold]: {arg.description}")
        if arg.help_text:
            self.console.print(f"   [dim]{arg.help_text}[/dim]")
        
        if arg.type == 'text':
            return Prompt.ask(
                f"   Value",
                default=arg.default if arg.default else ("" if not arg.required else None)
            )
        
        elif arg.type == 'select':
            return Prompt.ask(
                f"   Choice",
                choices=arg.choices,
                default=arg.default
            )
        
        elif arg.type == 'radio':
            return self._radio_select(arg.choices, arg.default)
        
        elif arg.type == 'multi_select':
            return self._multi_select(arg.choices, arg.default or [])
        
        elif arg.type == 'checkbox':
            return Confirm.ask(f"   Enable?", default=arg.default or False)
        
        elif arg.type == 'number':
            while True:
                try:
                    value = IntPrompt.ask(f"   Number", default=arg.default or 0)
                    if arg.min_value is not None and value < arg.min_value:
                        self.console.print(f"   [red]Value must be >= {arg.min_value}[/red]")
                        continue
                    if arg.max_value is not None and value > arg.max_value:
                        self.console.print(f"   [red]Value must be <= {arg.max_value}[/red]")
                        continue
                    return value
                except Exception:
                    self.console.print("   [red]Please enter a valid number[/red]")
        
        elif arg.type == 'file':
            return self._file_select(arg.file_extensions, arg.required)
        
        elif arg.type == 'directory':
            return self._directory_select(arg.required)
        
        return None
    
    def _radio_select(self, choices: List[str], default: str = None) -> str:
        """Radio button selection"""
        self.console.print("   [bold]Options:[/bold]")
        for i, choice in enumerate(choices):
            marker = "●" if choice == default else "○"
            self.console.print(f"     {i+1}. {marker} {choice}")
        
        while True:
            try:
                choice_idx = IntPrompt.ask("   Select number", default=choices.index(default) + 1 if default else 1)
                if 1 <= choice_idx <= len(choices):
                    return choices[choice_idx - 1]
                self.console.print(f"   [red]Please enter a number between 1 and {len(choices)}[/red]")
            except ValueError:
                self.console.print("   [red]Please enter a valid number[/red]")
    
    def _multi_select(self, choices: List[str], default: List[str] = None) -> List[str]:
        """Multi-select with checkboxes"""
        default = default or []
        selected = set(default)
        
        while True:
            self.console.print("   [bold]Options (select multiple):[/bold]")
            for i, choice in enumerate(choices):
                marker = "☑" if choice in selected else "☐"
                self.console.print(f"     {i+1}. {marker} {choice}")
            
            self.console.print("   [dim]Commands: number to toggle, 'done' to finish, 'clear' to clear all[/dim]")
            
            cmd = Prompt.ask("   Action").strip().lower()
            
            if cmd == 'done':
                return list(selected)
            elif cmd == 'clear':
                selected.clear()
            elif cmd.isdigit():
                idx = int(cmd) - 1
                if 0 <= idx < len(choices):
                    choice = choices[idx]
                    if choice in selected:
                        selected.remove(choice)
                    else:
                        selected.add(choice)
                else:
                    self.console.print(f"   [red]Please enter a number between 1 and {len(choices)}[/red]")
            else:
                self.console.print("   [red]Invalid command[/red]")
    
    def _file_select(self, extensions: List[str] = None, required: bool = True) -> Optional[str]:
        """File selection with validation"""
        while True:
            path = Prompt.ask("   File path", default="" if not required else None)
            if not path and not required:
                return None
            
            if not path:
                continue
                
            file_path = Path(path).expanduser()
            if not file_path.exists():
                self.console.print(f"   [red]File does not exist: {file_path}[/red]")
                continue
            
            if not file_path.is_file():
                self.console.print(f"   [red]Not a file: {file_path}[/red]")
                continue
            
            if extensions:
                if not any(str(file_path).endswith(ext) for ext in extensions):
                    self.console.print(f"   [red]File must have one of these extensions: {', '.join(extensions)}[/red]")
                    continue
            
            return str(file_path)
    
    def _directory_select(self, required: bool = True) -> Optional[str]:
        """Directory selection with validation"""
        while True:
            path = Prompt.ask("   Directory path", default="" if not required else None)
            if not path and not required:
                return None
            
            if not path:
                continue
                
            dir_path = Path(path).expanduser()
            if not dir_path.exists():
                self.console.print(f"   [red]Directory does not exist: {dir_path}[/red]")
                continue
            
            if not dir_path.is_dir():
                self.console.print(f"   [red]Not a directory: {dir_path}[/red]")
                continue
            
            return str(dir_path)
    
    def _format_argument(self, arg: ScriptArg, value: Any) -> List[str]:
        """Format argument for command line"""
        if value is None:
            return []
        
        if arg.type == 'checkbox':
            return [f"--{arg.name}"] if value else []
        elif arg.type == 'multi_select':
            if isinstance(value, list):
                result = []
                for v in value:
                    result.extend([f"--{arg.name}", str(v)])
                return result
            return []
        else:
            return [f"--{arg.name}", str(value)]
    
    def _run_command(self, cmd: List[str], async_exec: bool = True):
        """Execute the command"""
        try:
            if async_exec:
                subprocess.Popen(cmd)
                self.console.print("[green]✅ Script started asynchronously[/green]")
            else:
                with Progress(
                    SpinnerColumn(),
                    TextColumn("[progress.description]{task.description}"),
                    console=self.console
                ) as progress:
                    task = progress.add_task("Executing script...", total=None)
                    result = subprocess.run(cmd, capture_output=True, text=True)
                    progress.remove_task(task)
                
                if result.returncode == 0:
                    self.console.print("[green]✅ Script completed successfully[/green]")
                    if result.stdout:
                        self.console.print(Panel(result.stdout, title="📤 Output"))
                else:
                    self.console.print(f"[red]❌ Script failed (exit code: {result.returncode})[/red]")
                    if result.stderr:
                        self.console.print(Panel(result.stderr, title="❌ Error"))
        except Exception as e:
            self.console.print(f"[red]❌ Error executing script: {e}[/red]")
    
    def _run_script_interactive(self):
        """Run script with interactive selection"""
        if not self.current_menu or not self.current_menu.scripts:
            self.console.print("[yellow]No scripts available in current menu[/yellow]")
            return
        
        if len(self.current_menu.scripts) == 1:
            self._execute_script(self.current_menu.scripts[0])
        else:
            self.console.print("\n[bold]Select script to run:[/bold]")
            for i, script in enumerate(self.current_menu.scripts):
                self.console.print(f"  {i+1}. {script.name} - {script.description}")
            
            choice = IntPrompt.ask("Script number", default=1)
            if 1 <= choice <= len(self.current_menu.scripts):
                self._execute_script(self.current_menu.scripts[choice - 1])
    
    def _go_back(self):
        """Go back to previous menu"""
        if self.menu_stack:
            self.current_menu = self.menu_stack.pop()
        else:
            self.current_menu = None
    
    def _show_simple_menu(self):
        """Fallback simple menu when rich is not available"""
        print("Rich library not available. Install with: pip install rich")
        print("Using simple menu instead...")
        # Simple implementation here
    
    def run(self):
        """Main entry point"""
        try:
            self.load_config()
            self.show_main_menu()
        except KeyboardInterrupt:
            if self.console:
                self.console.print("\n👋 Goodbye!", style="bold green")
            else:
                print("\nGoodbye!")
        except Exception as e:
            if self.console:
                self.console.print(f"[red]Error: {e}[/red]")
            else:
                print(f"Error: {e}")

def main():
    parser = argparse.ArgumentParser(description="Interactive Script Runner with Rich Components")
    parser.add_argument("--config", "-c", default="~/.config/interactive-script-runner.yaml",
                       help="Configuration file path")
    parser.add_argument("--demo", action="store_true",
                       help="Create demo configuration and exit")
    
    args = parser.parse_args()
    
    runner = InteractiveScriptRunner(args.config)
    
    if args.demo:
        runner.create_demo_config()
        print(f"Demo configuration created at: {runner.config_path}")
        print("Run without --demo to start the interactive menu")
    else:
        runner.run()

if __name__ == "__main__":
    main()