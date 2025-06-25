#!/usr/bin/env python3
"""
Dynamic Script Runner - TUI interface for script execution
Reads configuration from YAML templates with visual exploration
"""

import os
import sys
import json
import yaml
import subprocess
from pathlib import Path
from dataclasses import dataclass, field
from typing import List, Dict, Optional, Any
import argparse

try:
    import rich
    from rich.console import Console
    from rich.table import Table
    from rich.panel import Panel
    from rich.tree import Tree
    from rich.prompt import Prompt, Confirm
    from rich.syntax import Syntax
    from rich.layout import Layout
    HAS_RICH = True
except ImportError:
    HAS_RICH = False
    print("Install rich for better UI: pip install rich")

@dataclass
class ScriptArg:
    name: str
    description: str
    required: bool = True
    default: Optional[str] = None
    choices: Optional[List[str]] = None

@dataclass
class Script:
    name: str
    path: str
    description: str = ""
    args: List[ScriptArg] = field(default_factory=list)
    tags: List[str] = field(default_factory=list)
    help_text: str = ""
    async_exec: bool = True

@dataclass
class Menu:
    name: str
    description: str
    scripts: List[Script] = field(default_factory=list)
    submenus: List['Menu'] = field(default_factory=list)
    
class ScriptRunner:
    def __init__(self, config_path: str = "~/.config/script-runner.yaml"):
        self.config_path = Path(config_path).expanduser()
        self.console = Console() if HAS_RICH else None
        self.menus: List[Menu] = []
        self.current_menu = None
        self.menu_stack = []
        
    def load_config(self):
        """Load configuration from YAML file"""
        if not self.config_path.exists():
            self.create_default_config()
            
        with open(self.config_path) as f:
            config = yaml.safe_load(f)
            
        self.menus = self._parse_config(config)
        
    def create_default_config(self):
        """Create default configuration file"""
        default_config = {
            'menus': [
                {
                    'name': 'Development',
                    'description': 'Development scripts',
                    'scripts': [
                        {
                            'name': 'deploy',
                            'path': '~/scripts/deploy.sh',
                            'description': 'Deploy application',
                            'args': [
                                {'name': 'environment', 'description': 'Target environment', 
                                 'choices': ['dev', 'staging', 'prod']},
                                {'name': 'version', 'description': 'Version to deploy', 'required': False}
                            ],
                            'tags': ['deployment', 'ci-cd']
                        }
                    ],
                    'submenus': [
                        {
                            'name': 'Git Operations',
                            'description': 'Git-related scripts',
                            'scripts': [
                                {
                                    'name': 'git-cleanup',
                                    'path': '~/scripts/git-cleanup.sh',
                                    'description': 'Clean up git branches',
                                    'tags': ['git', 'cleanup']
                                }
                            ]
                        }
                    ]
                }
            ]
        }
        
        self.config_path.parent.mkdir(parents=True, exist_ok=True)
        with open(self.config_path, 'w') as f:
            yaml.dump(default_config, f, indent=2)
            
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
                required=arg_config.get('required', True),
                default=arg_config.get('default'),
                choices=arg_config.get('choices')
            )
            args.append(arg)
            
        return Script(
            name=script_config['name'],
            path=Path(script_config['path']).expanduser(),
            description=script_config.get('description', ''),
            args=args,
            tags=script_config.get('tags', []),
            help_text=script_config.get('help_text', ''),
            async_exec=script_config.get('async_exec', True)
        )
    
    def discover_scripts(self, directory: str = "~/scripts") -> List[Script]:
        """Auto-discover scripts in directory"""
        scripts_dir = Path(directory).expanduser()
        discovered = []
        
        if not scripts_dir.exists():
            return discovered
            
        for script_file in scripts_dir.rglob("*"):
            if script_file.is_file() and os.access(script_file, os.X_OK):
                script = self._analyze_script(script_file)
                if script:
                    discovered.append(script)
                    
        return discovered
    
    def _analyze_script(self, script_path: Path) -> Optional[Script]:
        """Analyze script file for metadata"""
        try:
            with open(script_path, 'r') as f:
                content = f.read(2000)  # Read first 2KB
                
            # Extract metadata from comments
            description = self._extract_field(content, 'Description')
            help_text = self._extract_field(content, 'Help')
            tags = self._extract_list_field(content, 'Tags')
            
            # Extract arguments
            args = []
            import re
            arg_pattern = r'#\s*@arg\s+(\w+):\s*(.+)'
            for match in re.finditer(arg_pattern, content):
                args.append(ScriptArg(
                    name=match.group(1),
                    description=match.group(2)
                ))
            
            return Script(
                name=script_path.stem,
                path=str(script_path),
                description=description or f"Script: {script_path.name}",
                args=args,
                tags=tags,
                help_text=help_text or ""
            )
            
        except Exception as e:
            print(f"Error analyzing {script_path}: {e}")
            return None
    
    def _extract_field(self, content: str, field: str) -> Optional[str]:
        """Extract field from script comments"""
        import re
        pattern = rf'#\s*{field}:\s*(.+)'
        match = re.search(pattern, content, re.IGNORECASE)
        return match.group(1).strip() if match else None
    
    def _extract_list_field(self, content: str, field: str) -> List[str]:
        """Extract list field from script comments"""
        value = self._extract_field(content, field)
        if value:
            return [tag.strip() for tag in value.split(',')]
        return []
    
    def show_main_menu(self):
        """Display main menu"""
        if not HAS_RICH:
            self._show_simple_menu()
            return
            
        while True:
            self.console.clear()
            
            # Create layout
            layout = Layout()
            layout.split_column(
                Layout(name="header", size=3),
                Layout(name="body"),
                Layout(name="footer", size=3)
            )
            
            # Header
            layout["header"].update(Panel(
                "[bold blue]Script Runner[/bold blue] - Dynamic Menu System",
                style="bold white on blue"
            ))
            
            # Body - Menu tree
            if self.current_menu:
                menu_content = self._build_menu_display(self.current_menu)
            else:
                menu_content = self._build_main_display()
                
            layout["body"].update(Panel(menu_content, title="Menus"))
            
            # Footer
            commands = "[bold]Commands:[/bold] [cyan]r[/cyan]un [cyan]e[/cyan]dit [cyan]n[/cyan]ew [cyan]d[/cyan]iscover [cyan]c[/cyan]onfig [cyan]q[/cyan]uit"
            if self.current_menu:
                commands += " [cyan]b[/cyan]ack"
            layout["footer"].update(Panel(commands))
            
            self.console.print(layout)
            
            # Get command
            cmd = Prompt.ask("Command", choices=self._get_valid_commands())
            
            if cmd == 'q':
                break
            elif cmd == 'r':
                self._run_script_interactive()
            elif cmd == 'e':
                self._edit_script()
            elif cmd == 'n':
                self._new_script()
            elif cmd == 'd':
                self._discover_and_add()
            elif cmd == 'c':
                self._edit_config()
            elif cmd == 'b' and self.current_menu:
                self._go_back()
            elif cmd.isdigit():
                self._select_menu_item(int(cmd))
    
    def _build_main_display(self) -> Tree:
        """Build main menu display"""
        tree = Tree("📁 Available Menus")
        
        for i, menu in enumerate(self.menus):
            menu_node = tree.add(f"[bold]{i+1}.[/bold] {menu.name}")
            menu_node.add(f"[dim]{menu.description}[/dim]")
            
            if menu.scripts:
                scripts_node = menu_node.add("📄 Scripts")
                for script in menu.scripts[:3]:  # Show first 3
                    scripts_node.add(f"• {script.name}")
                if len(menu.scripts) > 3:
                    scripts_node.add(f"... and {len(menu.scripts) - 3} more")
                    
            if menu.submenus:
                submenus_node = menu_node.add("📁 Submenus")
                for submenu in menu.submenus:
                    submenus_node.add(f"• {submenu.name}")
                    
        return tree
    
    def _build_menu_display(self, menu: Menu) -> Tree:
        """Build specific menu display"""
        tree = Tree(f"📁 {menu.name}")
        tree.add(f"[dim]{menu.description}[/dim]")
        
        if menu.scripts:
            scripts_node = tree.add("📄 Scripts")
            for i, script in enumerate(menu.scripts):
                script_node = scripts_node.add(f"[bold]{i+1}.[/bold] {script.name}")
                script_node.add(f"[dim]{script.description}[/dim]")
                if script.tags:
                    script_node.add(f"Tags: {', '.join(script.tags)}")
                    
        if menu.submenus:
            start_idx = len(menu.scripts) + 1
            submenus_node = tree.add("📁 Submenus")
            for i, submenu in enumerate(menu.submenus):
                idx = start_idx + i
                submenu_node = submenus_node.add(f"[bold]{idx}.[/bold] {submenu.name}")
                submenu_node.add(f"[dim]{submenu.description}[/dim]")
                
        return tree
    
    def _get_valid_commands(self) -> List[str]:
        """Get valid commands for current state"""
        commands = ['r', 'e', 'n', 'd', 'c', 'q']
        if self.current_menu:
            commands.append('b')
            # Add numeric choices
            total_items = len(self.current_menu.scripts) + len(self.current_menu.submenus)
            commands.extend([str(i) for i in range(1, total_items + 1)])
        else:
            commands.extend([str(i) for i in range(1, len(self.menus) + 1)])
        return commands
    
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
        """Execute a script with argument collection"""
        if not HAS_RICH:
            self._execute_script_simple(script)
            return
            
        self.console.print(f"\n[bold]Executing: {script.name}[/bold]")
        self.console.print(f"Description: {script.description}")
        
        if script.help_text:
            self.console.print(Panel(script.help_text, title="Help"))
        
        # Collect arguments
        args = []
        for arg in script.args:
            if arg.choices:
                value = Prompt.ask(
                    f"{arg.name} ({arg.description})",
                    choices=arg.choices,
                    default=arg.default
                )
            else:
                value = Prompt.ask(
                    f"{arg.name} ({arg.description})",
                    default=arg.default or ("" if not arg.required else None)
                )
            
            if value:
                args.extend([f"--{arg.name}", value])
        
        # Confirm execution
        cmd = [str(script.path)] + args
        cmd_str = " ".join(cmd)
        
        self.console.print(f"\n[yellow]Command:[/yellow] {cmd_str}")
        
        if Confirm.ask("Execute this command?"):
            try:
                if script.async_exec:
                    subprocess.Popen(cmd)
                    self.console.print("[green]✓ Script started asynchronously[/green]")
                else:
                    result = subprocess.run(cmd, capture_output=True, text=True)
                    if result.returncode == 0:
                        self.console.print("[green]✓ Script completed successfully[/green]")
                        if result.stdout:
                            self.console.print(Panel(result.stdout, title="Output"))
                    else:
                        self.console.print(f"[red]✗ Script failed (exit code: {result.returncode})[/red]")
                        if result.stderr:
                            self.console.print(Panel(result.stderr, title="Error"))
                            
            except Exception as e:
                self.console.print(f"[red]Error executing script: {e}[/red]")
                
        Prompt.ask("Press Enter to continue")
    
    def _show_simple_menu(self):
        """Simple menu for when rich is not available"""
        while True:
            print("\n" + "="*50)
            print("SCRIPT RUNNER")
            print("="*50)
            
            if self.current_menu:
                print(f"\nCurrent Menu: {self.current_menu.name}")
                print(f"Description: {self.current_menu.description}")
                
                print("\nScripts:")
                for i, script in enumerate(self.current_menu.scripts):
                    print(f"  {i+1}. {script.name} - {script.description}")
                
                if self.current_menu.submenus:
                    start_idx = len(self.current_menu.scripts) + 1
                    print("\nSubmenus:")
                    for i, submenu in enumerate(self.current_menu.submenus):
                        print(f"  {start_idx + i}. {submenu.name} - {submenu.description}")
            else:
                print("\nAvailable Menus:")
                for i, menu in enumerate(self.menus):
                    print(f"  {i+1}. {menu.name} - {menu.description}")
            
            print("\nCommands: (r)un, (e)dit, (n)ew, (d)iscover, (c)onfig, (q)uit")
            if self.current_menu:
                print("          (b)ack, or number to select item")
            
            cmd = input("\nChoice: ").strip().lower()
            
            if cmd == 'q':
                break
            elif cmd.isdigit():
                self._select_menu_item(int(cmd))
            # ... handle other commands
    
    def run(self):
        """Main entry point"""
        try:
            self.load_config()
            self.show_main_menu()
        except KeyboardInterrupt:
            print("\nGoodbye!")
        except Exception as e:
            print(f"Error: {e}")

def main():
    parser = argparse.ArgumentParser(description="Dynamic Script Runner")
    parser.add_argument("--config", "-c", default="~/.config/script-runner.yaml",
                       help="Configuration file path")
    parser.add_argument("--discover", "-d", action="store_true",
                       help="Auto-discover scripts and create config")
    
    args = parser.parse_args()
    
    runner = ScriptRunner(args.config)
    
    if args.discover:
        scripts = runner.discover_scripts()
        print(f"Discovered {len(scripts)} scripts")
        for script in scripts:
            print(f"  - {script.name}: {script.description}")
    else:
        runner.run()

if __name__ == "__main__":
    main()