#!/usr/bin/env python3

print("Testing Textual imports...")

try:
    from textual.app import App, ComposeResult
    print("✅ App, ComposeResult")
except Exception as e:
    print(f"❌ App, ComposeResult: {e}")

try:
    from textual.containers import Container, Horizontal, Vertical
    print("✅ Container, Horizontal, Vertical")
except Exception as e:
    print(f"❌ Container, Horizontal, Vertical: {e}")

try:
    from textual.widgets import (
        Header, Footer, Button, Input, Label, Tree, Static, 
        Checkbox, Select, DataTable, TabbedContent, TabPane,
        Switch, Slider, Log
    )
    print("✅ All widgets")
except Exception as e:
    print(f"❌ Widgets: {e}")

try:
    from textual.screen import Screen
    print("✅ Screen")
except Exception as e:
    print(f"❌ Screen: {e}")

try:
    from textual.binding import Binding
    print("✅ Binding")
except Exception as e:
    print(f"❌ Binding: {e}")

try:
    from textual import on
    print("✅ on decorator")
except Exception as e:
    print(f"❌ on decorator: {e}")